#!/usr/bin/env python3
"""
Run the validation cases in µSail (Rocq) and Sail's C emulator, compare the
state after reset and after every step. See validation/docs/validate.md.

Usage:
    validation/scripts/validate.py [CASE...] [--jobs N] [--no-cache] [--cache-dir DIR] [--full-states]

CASE: a name in validation/cases (default: all). Needs, built beforehand
(gmake validate does it): validation/build/emulator/cheriot_sim_dump,
validation/build/model/*.vos, validation/rocq/Validate.vos (coqc -vos: Qed proofs
not checked, never unfolded by the interpreter; validation/rocq/Makefile).

Per case, in validation/build/validate/<case>/:
    prog.s, prog.elf    prologue + case + epilogue, assembled
    emu.run, emu.log    emulator run file (--dump-state) and output
    Run.v, rocq.out     generated Rocq runner and its output (a delta run file)
    rocq.run            Rocq's run file, every state in full (rebuilt from rocq.out)
    diff.txt            diff -u of the first differing step (if any)
    divergence.txt      recorded vs actual divergence, if it does not match

Expected divergences: validation/cases/divergences.toml (validate.md, "Known
divergences"); such a case reports `expected divergence at step N`, or `both
stop at step N` (Rocq stm_fail, the emulator stops on a Sail assertion or
uncaught exception), only if the run matches the table exactly.

Rocq results are cached by input hash (generated Run.v, the model, the
runner's .v files, the Rocq installation), shared by all checkouts:
$SAIL_BACKEND_VALIDATE_CACHE or ~/.cache/sail-backend/validate-cache.
Uncached cases run in at most --jobs coqc processes, several cases each, so
that each process loads the model once.
Exit status 1 if any case neither passes nor matches divergences.toml.
"""

import argparse
import concurrent.futures
import hashlib
import importlib.util
import os
import re
import shutil
import struct
import subprocess
import sys
import time
import tomllib
from pathlib import Path

VAL = Path(__file__).resolve().parent.parent
ROOT = VAL.parent
CASES = VAL / "cases"
BUILD = VAL / "build" / "validate"
ROCQ = VAL / "rocq"
MODEL = VAL / "build" / "model"
EMULATOR = VAL / "build" / "emulator" / "cheriot_sim_dump"
EMU_ARGS = ["--disable-compressed", "-F", "-W", "--insns-per-tick", "1000000000"]
RUNNER_SOURCES = [ROCQ / f for f in ("Interp.v", "Harness.v", "Names.v", "Dump.v", "Validate.v")] \
    + [MODEL / "base.v", MODEL / "machine.v"]
DEFAULT_LIMIT = 200
DEFAULT_CACHE = Path(
    os.environ.get("SAIL_BACKEND_VALIDATE_CACHE")
    or Path(os.environ.get("XDG_CACHE_HOME") or Path.home() / ".cache") / "sail-backend" / "validate-cache")
COQC_TIMEOUT = 1800
DIVERGENCES = CASES / "divergences.toml"

sys.path.insert(0, str(VAL / "compare"))
import statefile  # noqa: E402


def load_run_tests():
    """tests/coq-tests/run-tests.py, for its coq_fingerprint (as e2e caching)."""
    return load_module("run_tests", ROOT / "tests" / "coq-tests" / "run-tests.py")


def case_header(path, key, default):
    """`# key: value` in the case file."""
    m = re.search(rf"^#\s*{key}:\s*(\S+)", path.read_text(), re.M)
    return m.group(1) if m else default


def load_module(name, path):
    spec = importlib.util.spec_from_file_location(name, path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


ASM = load_module("mk_test_elf", VAL / "scripts" / "mk-test-elf.py")
ASM_BASE, ASM_TOHOST = 0x80000000, 0x80001000      # mk-test-elf.py's defaults


def assemble(case, out):
    """prologue + case + epilogue → out/prog.s, prog.elf (mk-test-elf.py, in this process); the words."""
    text = (CASES / "lib" / "prologue.s").read_text() \
        + f"# --- {case.name}\n" + case.read_text() + "# ---\n" + (CASES / "lib" / "epilogue.s").read_text()
    (out / "prog.s").write_text(text)
    try:
        words = ASM.assemble(str(out / "prog.s"), ASM_BASE)
    except SystemExit as e:
        raise RuntimeError(f"assembler: {e.code}")
    code = b"".join(struct.pack("<I", w) for w in words)
    (out / "prog.elf").write_bytes(ASM.elf32(code, ASM_BASE, ASM_TOHOST))
    return words


STOPS = {"assertion": r"Assertion failed: (.*)", "exception": r"Exiting due to uncaught exception: (.*)"}


def emulator_stop(text):
    """(kind, message) if the emulator's output says it stopped on a Sail assertion or an
    uncaught exception (the Sail runtime's and riscv_sim.c's messages), else None."""
    for kind, pattern in STOPS.items():
        m = re.search(pattern, text)
        if m:
            return kind, m.group(1).strip()
    return None


def run_emulator(out, limit):
    """emu.run; the htif exit code (None if none) and emulator_stop of its output."""
    with open(out / "emu.log", "w") as log:
        p = subprocess.run([str(EMULATOR), *EMU_ARGS, "--inst-limit", str(limit),
                            "--dump-state", str(out / "emu.run"), str(out / "prog.elf")],
                           stdout=log, stderr=subprocess.STDOUT, timeout=600)
    text = (out / "emu.log").read_text()
    if "SUCCESS" in text:
        return 0, None
    m = re.search(r"FAILURE: (\d+)", text)
    return (int(m.group(1)) if m else None), emulator_stop(text)


RUN_HEADER = """(* Generated by validation/scripts/validate.py. *)
From Stdlib Require Import Lists.List ZArith.ZArith.
From Validation Require Import Names Validate.
Import ListNotations.
"""


def run_body(words, limit, full, target):
    """The definitions and the printing of one program; Rocq's output goes to target.out."""
    ws = "; ".join(f"0x{w:08x}" for w in words)
    return f"""Definition prog : list Z := [{ws}]%Z.
Definition limit : nat := {limit}.
Lemma print_run : True.
  Time Redirect "{target}" ltac2:(print_plines constr:(case_output {str(full).lower()} fuel limit prog)).
  exact I.
Defined.
"""


def run_v(words, limit, full):
    """A case's own Run.v (coverage.py checks it against its runs)."""
    return RUN_HEADER + run_body(words, limit, full, "rocq")


def sources_digest():
    """Digest of the runner's .v files and the model (the cache key's common part)."""
    digest = hashlib.sha256()
    for path in RUNNER_SOURCES:
        data = path.read_bytes()
        digest.update(f"\n{path.name} {len(data)}\n".encode())
        digest.update(data)
    return digest.hexdigest()


def cache_key(fingerprint, sources, source):
    digest = hashlib.sha256(fingerprint.encode())
    digest.update(f"\n{sources}".encode())
    digest.update(f"\nRun.v {len(source)}\n{source}".encode())
    return digest.hexdigest()


class Case:
    """One case on its way through the stages; error is set if a stage failed."""

    def __init__(self, name, full):
        self.name, self.full = name, full
        self.out = BUILD / name
        self.error = None
        self.exit_code = None
        self.stop = None
        self.cache = None
        self.cached = False
        self.seconds = None


def prepare(case, fingerprint, sources, cache_dir):
    """Assemble, run the emulator, write Run.v; take Rocq's output from the cache if there."""
    try:
        source_path = CASES / f"{case.name}.s"
        case.out.mkdir(parents=True, exist_ok=True)
        case.limit = int(case_header(source_path, "limit", DEFAULT_LIMIT))
        case.expect_exit = 1 if case.name.startswith("trap-") else 0
        case.words = assemble(source_path, case.out)
        case.exit_code, case.stop = run_emulator(case.out, case.limit)
        source = run_v(case.words, case.limit, case.full)
        (case.out / "Run.v").write_text(source)
        target = case.out / "rocq.out"
        target.unlink(missing_ok=True)
        if fingerprint is not None:
            case.cache = cache_dir / f"{cache_key(fingerprint, sources, source)}.out"
            if case.cache.is_file():
                target.write_bytes(case.cache.read_bytes())
                case.cached = True
    except (RuntimeError, OSError, subprocess.TimeoutExpired) as e:
        case.error = str(e)


TIME = re.compile(r"Finished transaction in ([0-9.]+) secs")


def coqc(directory, filename):
    """coqc in directory, output (with each case's Time line) in directory/coqc.log."""
    p = subprocess.run(["coqc", "-vos", "-Q", str(ROCQ), "Validation", "-R", str(MODEL), "CheriotModel",
                        "-output-directory", str(directory), filename],
                       cwd=directory, capture_output=True, text=True, timeout=COQC_TIMEOUT)
    (directory / "coqc.log").write_text(p.stdout + p.stderr)
    return p.returncode, [float(t) for t in TIME.findall(p.stdout)]


def store(case):
    if case.cache:
        case.cache.parent.mkdir(parents=True, exist_ok=True)
        tmp = case.cache.with_name(f"{case.cache.name}.{os.getpid()}.{case.name}.tmp")
        tmp.write_bytes((case.out / "rocq.out").read_bytes())
        os.replace(tmp, case.cache)


def run_single(case):
    """Rocq on the case's own Run.v."""
    try:
        code, times = coqc(case.out, "Run.v")
        if code != 0 or not (case.out / "rocq.out").is_file():
            case.error = f"coqc exited {code}: see {case.out / 'coqc.log'}"
            return
        case.seconds = times[0] if times else None
        store(case)
    except (OSError, subprocess.TimeoutExpired) as e:
        case.error = str(e)


def run_batch(index, cases):
    """Rocq on several cases in one coqc, which loads the model once (about 1.5 s
    a case saved); each case in its own module, its output to its own directory.
    If the batch fails, each case runs alone, so that the error is the case's."""
    if len(cases) == 1:
        return run_single(cases[0])
    directory = BUILD / ".batch" / str(index)
    directory.mkdir(parents=True, exist_ok=True)
    (directory / "Batch.v").write_text(RUN_HEADER + "".join(
        f"Module Case{i}.\n{run_body(c.words, c.limit, c.full, str(c.out / 'rocq'))}End Case{i}.\n" for i, c in enumerate(cases)))
    try:
        code, times = coqc(directory, "Batch.v")
    except (OSError, subprocess.TimeoutExpired):
        code, times = None, []
    if code == 0 and len(times) == len(cases) and all((c.out / "rocq.out").is_file() for c in cases):
        for c, t in zip(cases, times):
            c.seconds = t
            store(c)
    else:
        for c in cases:
            (c.out / "rocq.out").unlink(missing_ok=True)
            run_single(c)


def batches(cases, n):
    """At most n batches of about equal work (longest programs first, each to the lightest batch)."""
    groups = [[] for _ in range(min(n, len(cases)))]
    load = [0] * len(groups)
    for c in sorted(cases, key=lambda c: -len(c.words)):
        i = load.index(min(load))
        groups[i].append(c)
        load[i] += len(c.words)
    return [g for g in groups if g]


def both_stop(d):
    """Whether the entry d records both sides stopping (emulator.stop) rather than a divergence."""
    return "stop" in d["emulator"]


def load_divergences():
    """{case: entry} of divergences.toml, checked against the case files."""
    table = tomllib.loads(DIVERGENCES.read_text())
    for name, d in table.items():
        if not (CASES / f"{name}.s").is_file():
            sys.exit(f"{DIVERGENCES}: [{name}]: no case {name}.s")
        try:
            emu = d["emulator"]
            ok = (isinstance(d["step"], int) and d["step"] >= 1 and isinstance(d["rocq"]["stop"], str)
                  and all(isinstance(d[k], str) for k in ("what", "reason", "doc"))
                  and all(isinstance(l, str) for l in d["rocq"]["delta"]))
            if both_stop(d):
                ok = (ok and set(emu) == {"stop", "message"} and emu["stop"] in STOPS
                      and isinstance(emu["message"], str) and emu["message"] != "")
            else:
                ok = (ok and set(emu) == {"exit", "delta"} and isinstance(emu["exit"], int)
                      and all(isinstance(l, str) for l in emu["delta"]))
        except (KeyError, TypeError):
            ok = False
        if not ok:
            sys.exit(f"{DIVERGENCES}: [{name}]: needs what, reason, doc, step >= 1, rocq.stop, rocq.delta, "
                     f"and either emulator.exit (int) and emulator.delta, or emulator.stop "
                     f"({' or '.join(STOPS)}) and emulator.message")
    return table


def step_delta(run, step):
    """Lines of run's state at step that differ from step - 1."""
    return [f"{k} {v}" for k, v in statefile.delta(run.get(step - 1, []), run.get(step, []))]


def show_stop(stop):
    return "no assertion or uncaught exception" if stop is None else f"{stop[0]} \"{stop[1]}\""


def check_divergence(d, rocq, emu, why, exit_code, stop, step):
    """None if the run diverges, or both sides stop, exactly as recorded (d), else what differs."""
    if step is None:
        return f"no divergence, expected one at step {d['step']}"
    if step != d["step"]:
        return f"diverges at step {step}, expected at step {d['step']}"
    problems = []
    if why != d["rocq"]["stop"] or len(rocq) - 1 != step:
        problems.append(f"Rocq stops {why} after step {len(rocq) - 1}, expected {d['rocq']['stop']} after step {step}")
    if step_delta(rocq, step) != d["rocq"]["delta"]:
        problems.append(f"rocq delta at step {step}")
    if both_stop(d):
        if len(emu) != step:
            problems.append(f"emulator stops after step {len(emu) - 1}, expected after step {step - 1}")
        if exit_code is not None:
            problems.append(f"emulator exit {exit_code}, expected none")
        if stop is None or stop[0] != d["emulator"]["stop"] or d["emulator"]["message"] not in stop[1]:
            problems.append(f"emulator stops on {show_stop(stop)}, expected {d['emulator']['stop']} "
                            f"containing \"{d['emulator']['message']}\"")
        return "; ".join(problems) or None
    if step_delta(emu, step) != d["emulator"]["delta"]:
        problems.append(f"emulator delta at step {step}")
    if exit_code != d["emulator"]["exit"]:
        problems.append(f"emulator exit {'-' if exit_code is None else exit_code}, expected {d['emulator']['exit']}")
    return "; ".join(problems) or None


def divergence_report(d, rocq, emu, stop, step):
    """Recorded vs actual deltas at the recorded step (and the actual one), diff -u; for a
    both-stop entry, the emulator's recorded and actual stop."""
    import difflib
    text = ""
    sides = ("rocq",) if both_stop(d) else ("rocq", "emulator")
    for s in sorted({d["step"], step} - {None}):
        for side, run in ((side, {"rocq": rocq, "emulator": emu}[side]) for side in sides):
            recorded = [f"{l}\n" for l in d[side]["delta"]] if s == d["step"] else []
            actual = [f"{l}\n" for l in step_delta(run, s)] if s in run else []
            text += "".join(difflib.unified_diff(recorded, actual, f"recorded {side} step {s}",
                                                 f"actual {side} step {s}")) or f"= {side} step {s}\n"
    if both_stop(d):
        text += (f"recorded emulator: stops after step {d['step'] - 1} on {d['emulator']['stop']} "
                 f"containing \"{d['emulator']['message']}\"\n"
                 f"actual emulator: stops after step {len(emu) - 1} on {show_stop(stop)}\n")
    return text


def compare(case, out, exit_code, stop, expect_exit, divergence):
    """(steps, stop, result) of a case whose two runs are in out."""
    lines = statefile.read_lines(out / "rocq.out")
    why = lines[-1].removeprefix("end ") if lines and lines[-1].startswith("end ") else "?"
    rocq = statefile.parse_any_run(lines[:-1], str(out / "rocq.out"))
    (out / "rocq.run").write_text(statefile.render_run(rocq))
    emu = statefile.parse_run(statefile.read_lines(out / "emu.run"), str(out / "emu.run"))
    steps = len(rocq) - 1
    step = statefile.first_difference(rocq, emu)
    (out / "diff.txt").unlink(missing_ok=True)
    (out / "divergence.txt").unlink(missing_ok=True)
    if step is not None:
        (out / "diff.txt").write_text(statefile.state_diff(rocq, emu, step, "rocq", "emulator"))
    if divergence is not None:
        problem = check_divergence(divergence, rocq, emu, why, exit_code, stop, step)
        if problem is None:
            return steps, why, f"{'both stop' if both_stop(divergence) else 'expected divergence'} at step {step}"
        (out / "divergence.txt").write_text(divergence_report(divergence, rocq, emu, stop, step))
        return steps, why, f"FAIL: {problem} (divergences.toml)"
    if step is not None:
        note = ""
        if step in rocq and dict(rocq[step]).get("step_result", "").startswith('"'):
            note = f" (Rocq stm_fail {dict(rocq[step])['step_result']}" \
                + ("" if stop is None else f"; emulator {show_stop(stop)}") + ")"
        elif step not in rocq and why in ("OutOfFuel", "NoAnswer"):
            note = f" (Rocq {why})"
        return steps, why, f"FAIL at step {step}{note}"
    if why in ("OutOfFuel", "NoAnswer"):
        return steps, why, f"Rocq {why}"
    if exit_code != expect_exit:
        return steps, why, f"BAD CASE: exit {'-' if exit_code is None else exit_code}, expected {expect_exit}"
    return steps, why, "pass"


def divergences_md(table):
    """Markdown table of divergences.toml for validate.md ("Known divergences"); doc links
    relative to validation/docs."""
    rows = ["| case | what | step | µSail | emulator | reason | doc |", "|---|---|---|---|---|---|---|"]
    for name, d in table.items():
        rocq = d["rocq"]["stop"] + ": " + ", ".join(f"`{l}`" for l in d["rocq"]["delta"])
        if both_stop(d):
            emu = f"stops on {d['emulator']['stop']}: `{d['emulator']['message']}`"
        else:
            emu = f"exit {d['emulator']['exit']}: " + ", ".join(f"`{l}`" for l in d["emulator"]["delta"])
        doc = f"[{d['doc'].rsplit('/', 1)[-1]}](../../{d['doc']})"
        rows.append(f"| `{name}` | {d['what']} | {d['step']} | {rocq} | {emu} | {d['reason']} | {doc} |")
    return "\n".join(rows) + "\n"


def main():
    p = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument("cases", nargs="*")
    p.add_argument("--jobs", "-j", type=int, default=4)
    p.add_argument("--no-cache", action="store_true")
    p.add_argument("--cache-dir", type=Path, default=DEFAULT_CACHE)
    p.add_argument("--full-states", action="store_true",
                   help="Rocq prints every state in full, not deltas (slower; checks the delta encoding)")
    p.add_argument("--list-divergences", action="store_true",
                   help="print divergences.toml as a Markdown table and exit")
    a = p.parse_args()

    divergences = load_divergences()
    if a.list_divergences:
        sys.stdout.write(divergences_md(divergences))
        return 0

    names = a.cases or sorted(c.stem for c in CASES.glob("*.s"))
    for prerequisite in (EMULATOR, MODEL / "machine.vos", ROCQ / "Validate.vos"):
        if not prerequisite.exists():
            sys.exit(f"missing {prerequisite}: run gmake validate")
    fingerprint = None if a.no_cache else load_run_tests().coq_fingerprint("coqc")

    start = time.time()
    sources = None if fingerprint is None else sources_digest()
    cases = [Case(name, a.full_states) for name in names]
    with concurrent.futures.ThreadPoolExecutor(a.jobs) as pool:
        list(pool.map(lambda c: prepare(c, fingerprint, sources, a.cache_dir), cases))
        todo = [c for c in cases if c.error is None and not c.cached]
        shutil.rmtree(BUILD / ".batch", ignore_errors=True)
        list(pool.map(lambda ib: run_batch(*ib), enumerate(batches(todo, a.jobs))))
    rows = {}
    for c in cases:
        if c.error is not None:
            rows[c.name] = ("-", "-", "-" if c.exit_code is None else str(c.exit_code), f"ERROR: {c.error}", "")
            continue
        try:
            steps, why, result = compare(c.name, c.out, c.exit_code, c.stop, c.expect_exit, divergences.get(c.name))
            rows[c.name] = (str(steps), why, "-" if c.exit_code is None else str(c.exit_code), result,
                            "cached" if c.cached else "-" if c.seconds is None else f"{c.seconds:.1f} s")
        except (OSError, statefile.FormatError) as e:
            rows[c.name] = ("-", "-", str(c.exit_code), f"ERROR: {e}", "")

    header = ("case", "steps", "stop", "exit", "result", "Rocq")
    table = [header] + [(n, *rows[n]) for n in names]
    widths = [max(len(r[i]) for r in table) for i in range(len(header))]
    text = "".join("| " + " | ".join(c.ljust(w) for c, w in zip(r, widths)) + " |\n" for r in table)
    text = text.replace("\n", "\n|" + "|".join("-" * (w + 2) for w in widths) + "|\n", 1)
    (BUILD / "results.md").write_text(text)
    sys.stdout.write(text)
    expected = [n for n in names if rows[n][3].startswith("expected divergence")]
    stopped = [n for n in names if rows[n][3].startswith("both stop")]
    failed = [n for n in names if rows[n][3] != "pass" and n not in expected + stopped]
    diffs = [n for n in failed if (BUILD / n / "diff.txt").is_file()]
    divs = [n for n in failed if (BUILD / n / "divergence.txt").is_file()]
    print(f"{len(names) - len(failed) - len(expected) - len(stopped)}/{len(names)} pass,"
          f" {len(expected)} expected divergence, {len(stopped)} both stop, {len(failed)} fail ({time.time() - start:.0f} s)"
          + (f"; diffs: {BUILD}/<case>/diff.txt ({', '.join(diffs)})" if diffs else "")
          + (f"; {BUILD}/<case>/divergence.txt ({', '.join(divs)})" if divs else ""))
    return 1 if failed else 0


if __name__ == "__main__":
    sys.exit(main())
