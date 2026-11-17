#!/usr/bin/env python3
"""
Coverage of the validation cases: which functions of the generated model
(machine.v) the runs call. See validation/docs/validate.md, "Coverage".

Usage:
    validation/scripts/coverage.py [CASE...] [--jobs N] [--no-cache] [--cache-dir DIR]

CASE: a name in validation/cases (default: all). Needs, built beforehand (gmake
validate-coverage does it): validation/build/model/*.vo, validation/rocq/Coverage.vo.

Each case runs as in validate.py (reset, then up to its step limit), with the
counting interpreter (InterpCov.v, Coverage.v). Per case, in
validation/build/coverage/<case>/: prog.s, prog.elf, Cov.v, cov.out (one line
"call|foreign <name> <calls>" per function called, then "end <why> <steps>").

Counted: the µSail functions (Fun constructors) reachable in machine.v's static
call graph (stm_call) from step and init_model; the foreign functions (FunX)
called from those. Report: validation/build/coverage/report.md.

Each run must end as the validation run of the same program did
(validation/build/validate/<case>/rocq.out): column `check`.
"""

import argparse
import concurrent.futures
import hashlib
import importlib.util
import os
import re
import subprocess
import sys
import time
from collections import defaultdict
from pathlib import Path

VAL = Path(__file__).resolve().parent.parent
ROCQ = VAL / "rocq"
MODEL = VAL / "build" / "model"
MACHINE = MODEL / "machine.v"
OUT = VAL / "build" / "coverage"
VALIDATE_OUT = VAL / "build" / "validate"
COV_SOURCES = [ROCQ / f for f in ("Interp.v", "InterpCov.v", "Harness.v", "Names.v", "Coverage.v")] \
    + [MODEL / "base.v", MODEL / "machine.v"]
ROOTS = ("step", "init_model")


def load_validate():
    spec = importlib.util.spec_from_file_location("validate", VAL / "scripts" / "validate.py")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


validate = load_validate()

# Reachable in the call graph, but not on this configuration (RV32, M-mode only,
# capability mode, no compressed): (re.fullmatch on the name, why). Counted
# apart: coverage is also given without them.
DEAD = [
    (r"execute_(ADDIW|SHIFTIWOP|RTYPEW|MULW|DIVW|REMW)", "RV64 only: encdec decodes them only if xlen == 64"),
    (r"execute_RISCV_JALR?|get_arch_pc",
     "JAL, JALR, AUIPC decode as CJAL, CJALR, AUIPCC: ext_decode tries encdec_capmode first"),
    (r"execute_(C_ILLEGAL|NOT_C_CAPMODE)", "only from the compressed decoder; step executes it only if haveRVC()"),
    (r"execute_NOT_CAPMODE|internal_error|report_invalid_width", "unreachable by design: internal_error (inlined in report_invalid_width)"),
    (r"(_get|_update)_[SU](interrupts|status)_\w+|Mk_[SU](interrupts|status|edeleg)|(lift|lower|legalize)_[su](ie|ip|status)"
     r"|(get|set)_[su]tvec|legalize_s(edeleg|counteren|atp|atp32)|set_sstatus_UXL|lower_m(ie|ip|status)",
     "S-mode and N-extension CSRs and their helpers: is_CSR_defined needs haveSupMode() or haveNExt(), "
     "and misa.S, misa.N are 0 after init_sys and legalize_misa never sets them"),
]

# Areas of the model, for the list of uncovered functions: the first pattern
# (re.search on the name) that matches. Order matters.
AREAS = [
    ("instructions (execute_*)", r"^execute_"),
    ("MMIO and HTIF", r"clint|uart|htif|mmio|within_"),
    ("interrupts", r"interrupt|Interrupt|^getPendingSet|^processPending|_mie$|_mip$|^platform_wfi"),
    ("PCC and control-flow checks", r"^ext_control_check|^ext_handle_control|^ext_handle_fetch|^ext_fetch_check|pcc"),
    ("traps, exceptions, xRET", r"trap|exception|Exception|xret|^handle_|^prepare_|privLevel_of_bits|_MPP$|_MPIE$"
                                r"|[Cc]apEx|^report_invalid|^not_implemented"),
    ("CSRs", r"CSR|csr|Counteren|Counterin|Mstatus|Misa|Medeleg|Envcfg|ccsr|^legalize_|^Mk_|mtvec|^lower_mstatus"
             r"|seed|mshwm|pc_alignment_mask|cur_Architecture|haveZkr|fiom"),
    ("capabilities", r"[Cc]ap|[Ss]eal|[Pp]erm|[Bb]ound|[Rr]epresentable|[Ss]entry|clearTag"),
    ("memory", r"mem|Mem|ram|phys|translat|align|[Tt]ag|fence|Satp|architecture"),
    ("other", r"."),
]


def model_graph(text):
    """(funs, foreign, calls, foreign_calls): the Fun and FunX constructors in
    declaration order, and per Fun the Funs and FunXs its body calls."""
    fun_decl = text[text.index("Inductive Fun :"):text.index("Inductive FunX :")]
    funx_decl = text[text.index("Inductive FunX :"):text.index("End FunDeclKit.")]
    ctor = re.compile(r"^\s*\|\s*@?(\w+)", re.M)
    funs = ctor.findall(fun_decl)
    funx = ctor.findall(funx_decl)
    start = text.index("Definition FunDef")
    fundef = text[start:text.index("end.", start)]
    body_of = dict(re.findall(r"^\s*\|\s*@?(\w+)[^\n]*=>\s*@?(\w+)", fundef, re.M))
    bodies = {}
    region = text[text.index("(*FunDefKit*)"):start]
    for m in re.finditer(r"^  Definition (\w+)(.*?)(?=^  Definition |\Z)", region, re.M | re.S):
        bodies[m.group(1)] = m.group(2)
    fun_set, funx_set = set(funs), set(funx)
    calls, foreign_calls = {}, {}
    for f in funs:
        body = bodies[body_of[f]]
        calls[f] = set(re.findall(r"stm_call\s+\(?@?(\w+)", body)) & fun_set
        foreign_calls[f] = set(re.findall(r"stm_foreign\s+\(?@?(\w+)", body)) & funx_set
    return funs, funx, calls, foreign_calls


def reachable(calls, roots):
    seen, todo = set(), list(roots)
    while todo:
        f = todo.pop()
        if f not in seen:
            seen.add(f)
            todo.extend(calls[f])
    return seen


def cov_v(words, limit):
    ws = "; ".join(f"0x{w:08x}" for w in words)
    return f"""(* Generated by validation/scripts/coverage.py. *)
From Stdlib Require Import Lists.List ZArith.ZArith.
From Validation Require Import Names Coverage.
Import ListNotations.
Definition prog : list Z := [{ws}]%Z.
Definition limit : nat := {limit}.
Goal True.
  Redirect "cov" ltac2:(print_lines constr:(cov_lines limit prog)).
  exact I.
Qed.
"""


def cache_key(fingerprint, source):
    digest = hashlib.sha256(f"coverage\n{fingerprint}".encode())
    for path in COV_SOURCES:
        data = path.read_bytes()
        digest.update(f"\n{path.name} {len(data)}\n".encode())
        digest.update(data)
    digest.update(f"\nCov.v {len(source)}\n{source}".encode())
    return digest.hexdigest()


def run_rocq(out, source, cache):
    """out/cov.out, from the cache or coqc; (cached, seconds)."""
    start = time.time()
    (out / "Cov.v").write_text(source)
    target = out / "cov.out"
    if cache and cache.is_file():
        target.write_bytes(cache.read_bytes())
        return True, time.time() - start
    target.unlink(missing_ok=True)
    p = subprocess.run(["coqc", "-Q", str(ROCQ), "Validation", "-R", str(MODEL), "CheriotModel",
                        "-output-directory", str(out), "Cov.v"],
                       cwd=out, capture_output=True, text=True, timeout=validate.COQC_TIMEOUT)
    (out / "coqc.log").write_text(p.stdout + p.stderr)
    if p.returncode != 0 or not target.is_file():
        raise RuntimeError(f"coqc exited {p.returncode}: see {out / 'coqc.log'}")
    if cache:
        cache.parent.mkdir(parents=True, exist_ok=True)
        tmp = cache.with_name(f"{cache.name}.{os.getpid()}.tmp")
        tmp.write_bytes(target.read_bytes())
        os.replace(tmp, cache)
    return False, time.time() - start


def parse_cov(path):
    """({fun: calls}, {foreign: calls}, why, steps)."""
    calls, foreign, why, steps = {}, {}, "?", -1
    for line in path.read_text().splitlines():
        kind, *rest = line.split()
        if kind == "call":
            calls[rest[0]] = int(rest[1])
        elif kind == "foreign":
            foreign[rest[0]] = int(rest[1])
        elif kind == "end":
            why, steps = rest[0], int(rest[1])
    return calls, foreign, why, steps


def check(name, words, limit, why, steps):
    """'ok' if the validation run of the same program ended the same way; '-' if there is none."""
    run_v = VALIDATE_OUT / name / "Run.v"
    rocq_out = VALIDATE_OUT / name / "rocq.out"
    if not (run_v.is_file() and rocq_out.is_file()) or run_v.read_text() != validate.run_v(words, limit):
        return "-"
    lines = rocq_out.read_text().splitlines()
    v_why = lines[-1].removeprefix("end ") if lines and lines[-1].startswith("end ") else "?"
    v_steps = max((int(l.split()[0]) for l in lines[:-1]), default=-1)
    if (v_why, v_steps) == (why, steps):
        return "ok"
    return f"MISMATCH: validation {v_why} after {v_steps}, coverage {why} after {steps}"


def area(name):
    return next(a for a, pattern in AREAS if re.search(pattern, name))


def dead_reason(name):
    return next((why for pattern, why in DEAD if re.fullmatch(pattern, name)), None)


def pct(n, d):
    return f"{100 * n / d:.0f}%" if d else "-"


def table(header, rows):
    widths = [max(len(str(r[i])) for r in [header] + rows) for i in range(len(header))]
    line = lambda r: "| " + " | ".join(str(c).ljust(w) for c, w in zip(r, widths)) + " |\n"
    return line(header) + "|" + "|".join("-" * (w + 2) for w in widths) + "|\n" + "".join(map(line, rows))


def report(names, results, funs, funx, calls, foreign_calls):
    """(report.md, one-line summary)."""
    step_reach = reachable(calls, ["step"])
    reach = step_reach | reachable(calls, ["init_model"])
    dead = {f for f in reach if dead_reason(f)}
    live = reach - dead
    foreign_reach = set().union(*(foreign_calls[f] for f in reach))
    covered = set().union(*(set(r[0]) for r in results.values()))
    foreign_covered = set().union(*(set(r[1]) for r in results.values()))
    unknown = (covered - reach) | (foreign_covered - foreign_reach)
    if unknown:
        raise RuntimeError(f"called but not statically reachable: {', '.join(sorted(unknown))}")
    if covered & dead:
        raise RuntimeError(f"called but listed as dead (coverage.py DEAD): {', '.join(sorted(covered & dead))}")
    insts = {f for f in live if f.startswith("execute_")}
    by_case = {n: set(results[n][0]) for n in names}

    def row(label, total, called):
        return (label, len(total), len(called & total), pct(len(called & total), len(total)))

    text = "# Validation coverage\n\n"
    text += (f"Cases: {len(names)}. machine.v: {len(funs)} functions, {len(funx)} foreign functions. "
             "Reachable: in the static call graph (stm_call) from step or init_model. "
             "Dead: reachable, but not on this configuration ([below](#dead-on-this-configuration)).\n\n")
    text += table(("functions", "total", "called", "coverage"), [
        row("reachable", reach, covered),
        row("live (reachable, not dead)", live, covered),
        row("  live, from step", live & step_reach, covered),
        row("  live, from init_model only", live - step_reach, covered),
        row("  live instructions (execute_*)", insts, covered),
        row("dead", dead, covered),
        row("foreign, called from reachable", foreign_reach, foreign_covered),
    ]) + "\n"
    if set(funs) - reach:
        text += f"Not reachable (not counted): {', '.join(f'`{f}`' for f in sorted(set(funs) - reach))}.\n\n"

    text += "## Per case\n\n"
    rows = []
    for n in names:
        others = set().union(*(by_case[m] for m in names if m != n))
        only = sorted(by_case[n] - others)
        rows.append((n, results[n][2], results[n][3], len(by_case[n]), len(only),
                     ", ".join(f"`{f}`" for f in only)))
    text += table(("case", "stop", "steps", "functions", "only here", "functions only this case calls"), rows) + "\n"

    callers = defaultdict(set)
    for f in reach:
        for g in calls[f]:
            callers[g].add(f)
    uncovered = live - covered
    text += f"## Live, not called ({len(uncovered)})\n\n"
    text += "Per area. `←`: the called functions that call it (a branch no case takes there).\n\n"
    groups = defaultdict(list)
    for f in sorted(uncovered, key=str.lower):
        groups[area(f)].append(f)
    for a, _ in AREAS:
        if groups[a]:
            text += f"### {a} ({len(groups[a])})\n\n"
            for f in groups[a]:
                via = sorted(callers[f] & covered)
                text += f"- `{f}`" + (f" ← {', '.join(f'`{g}`' for g in via)}" if via else "") + "\n"
            text += "\n"
    if foreign_reach - foreign_covered:
        text += (f"### foreign functions ({len(foreign_reach - foreign_covered)})\n\n"
                 + "".join(f"- `{f}`\n" for f in sorted(foreign_reach - foreign_covered)) + "\n")

    text += f"## Dead on this configuration ({len(dead)})\n\n"
    for pattern, why in DEAD:
        fs = sorted((f for f in dead if re.fullmatch(pattern, f)), key=str.lower)
        if fs:
            text += f"- {why} ({len(fs)}): {', '.join(f'`{f}`' for f in fs)}\n"
    summary = (f"coverage: {len(covered & live)}/{len(live)} live functions ({pct(len(covered & live), len(live))}; "
               f"{len(reach)} reachable, {len(dead)} dead), {len(covered & insts)}/{len(insts)} instructions, "
               f"{len(foreign_covered)}/{len(foreign_reach)} foreign")
    return text, summary


def main():
    p = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument("cases", nargs="*")
    p.add_argument("--jobs", "-j", type=int, default=4)
    p.add_argument("--no-cache", action="store_true")
    p.add_argument("--cache-dir", type=Path, default=validate.DEFAULT_CACHE)
    a = p.parse_args()

    names = a.cases or sorted(c.stem for c in validate.CASES.glob("*.s"))
    for prerequisite in (MODEL / "machine.vo", ROCQ / "Coverage.vo"):
        if not prerequisite.exists():
            sys.exit(f"missing {prerequisite}: run gmake validate-coverage")
    funs, funx, calls, foreign_calls = model_graph(MACHINE.read_text())
    fingerprint = None if a.no_cache else validate.load_run_tests().coq_fingerprint("coqc")

    start = time.time()
    results, rows, jobs, errors = {}, [], {}, []
    with concurrent.futures.ThreadPoolExecutor(a.jobs) as pool:
        for name in names:
            case = validate.CASES / f"{name}.s"
            out = OUT / name
            out.mkdir(parents=True, exist_ok=True)
            try:
                limit = int(validate.case_header(case, "limit", validate.DEFAULT_LIMIT))
                words = validate.assemble(case, out)
                source = cov_v(words, limit)
                cache = None if fingerprint is None else a.cache_dir / f"cov-{cache_key(fingerprint, source)}.out"
                jobs[name] = (pool.submit(run_rocq, out, source, cache), out, words, limit)
            except (RuntimeError, OSError) as e:
                errors.append(f"{name}: {e}")
        for name, (future, out, words, limit) in jobs.items():
            try:
                cached, seconds = future.result()
                c, f, why, steps = parse_cov(out / "cov.out")
                results[name] = (c, f, why, steps)
                verdict = check(name, words, limit, why, steps)
                if verdict.startswith("MISMATCH"):
                    errors.append(f"{name}: {verdict}")
                rows.append((name, why, str(steps), str(len(c)), str(len(f)), verdict,
                             "cached" if cached else f"{seconds:.0f} s"))
            except (RuntimeError, OSError, subprocess.TimeoutExpired) as e:
                errors.append(f"{name}: {e}")

    sys.stdout.write(table(("case", "stop", "steps", "functions", "foreign", "check", "Rocq"), rows))
    done = [n for n in names if n in results]
    text, summary = report(done, results, funs, funx, calls, foreign_calls)
    (OUT / "report.md").write_text(text)
    print(f"{summary} ({time.time() - start:.0f} s); report: {OUT / 'report.md'}")
    for e in errors:
        print(f"ERROR {e}")
    return 1 if errors else 0


if __name__ == "__main__":
    sys.exit(main())
