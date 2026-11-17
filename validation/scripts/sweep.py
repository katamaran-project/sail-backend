#!/usr/bin/env python3
"""
Every 16-bit parcel, one step from one fixed state, in µSail (Rocq) and Sail's
C emulator; compare the changes. See validation/docs/validate.md, "All 16-bit
parcels".

Usage:
    validation/scripts/sweep.py [--range LO:HI] [-j N] [--batches B] [--no-cache] [--cache-dir DIR]

Needs, built beforehand (gmake validate-sweep does it): the emulator, the
model's .vos, validation/rocq/Sweep.vos.

The fixed state: validation/cases/lib/prologue.s + sweep.s (+ epilogue.s),
run until PC reaches the label `slot`. Each parcel P in [LO, HI) (default all)
with P[1:0] != 0b11 is written at `slot` and runs one step:
- emulator: --sweep-parcels LO:HI, one child process (fork) per parcel from
  the fixed state; one process per range, all CPUs;
- Rocq: Sweep.v, the setup once per batch, then one step per parcel;
  B batches (default 16) in at most N coqc processes (default 2), cached by
  input hash as gmake validate's runs.
Per parcel: its state's lines that differ from the fixed state, compared as
sets. A difference is explained if it is of a known class (CLASSES); others
fail. Report: validation/build/sweep/report.md. Exit status 1 if the fixed
states differ or any parcel's difference is unexplained.
"""

import argparse
import concurrent.futures
import hashlib
import os
import re
import struct
import subprocess
import sys
import time
from collections import Counter, defaultdict
from pathlib import Path

VAL = Path(__file__).resolve().parent.parent
V = None  # validate.py, loaded in main


def load_validate():
    import importlib.util
    spec = importlib.util.spec_from_file_location("validate", VAL / "scripts" / "validate.py")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


BUILD = VAL / "build" / "sweep"
LIB = VAL / "cases" / "lib"


def assemble():
    """prologue + sweep.s + epilogue → prog.s, prog.elf; (words, address of `slot`)."""
    text = "".join((LIB / f).read_text() for f in ("prologue.s", "sweep.s", "epilogue.s"))
    (BUILD / "prog.s").write_text(text)
    words = V.ASM.assemble(str(BUILD / "prog.s"), V.ASM_BASE)
    pc, slot = V.ASM_BASE, None
    for _, label, mnem, args in V.ASM.parse(str(BUILD / "prog.s")):
        if label == "slot":
            slot = pc
        if mnem == ".align":
            pc += V.ASM.align_pad(pc, args)
        elif mnem:
            pc += V.ASM.size(mnem)
    code = b"".join(struct.pack("<I", w) for w in words)
    (BUILD / "prog.elf").write_bytes(V.ASM.elf32(code, V.ASM_BASE, V.ASM_TOHOST))
    return words, slot


def setup_steps(slot):
    """Steps from reset until PC = slot (an emulator run)."""
    run = BUILD / "setup.run"
    subprocess.run([str(V.EMULATOR), *V.EMU_ARGS, "--inst-limit", "500", "--dump-state", str(run),
                    str(BUILD / "prog.elf")], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL, timeout=600)
    want = f"PC 0x{slot:08x}"
    for line in run.read_text().splitlines():
        step, rest = line.split(" ", 1)
        if rest == want and step != "0":
            return int(step)
    sys.exit(f"sweep: the setup never reaches slot 0x{slot:08x} (see {run})")


def parcels_in(lo, hi):
    return [p for p in range(lo, hi) if p & 3 != 3]


def split(lo, hi, n):
    """n contiguous ranges covering [lo, hi)."""
    step = -(-(hi - lo) // n)
    return [(a, min(a + step, hi)) for a in range(lo, hi, step)]


def parse(path):
    """(fixed lines, {parcel: (lines, end)}): end is 'end' or 'stop <why>'."""
    fixed, out = [], defaultdict(lambda: [[], None])
    for line in path.read_text().splitlines():
        key, _, rest = line.partition(" ")
        if key == "fixed":
            fixed.append(rest)
        elif key.startswith("0x"):
            p = int(key, 16)
            if rest == "end" or rest.startswith("stop "):
                out[p][1] = rest
            else:
                out[p][0].append(rest)
        elif key == "error":
            sys.exit(f"sweep: {path}: {line}")
    return fixed, {p: (sorted(ls), end) for p, (ls, end) in out.items()}


MESSAGE = re.compile(r"^(?:Assertion failed|Exiting due to uncaught exception): (.*)$")


def run_emulator(i, lo, hi, n):
    """Emulator sweep of [lo, hi); (parsed, {parcel: message of a stop})."""
    out, log = BUILD / f"emu{i}.sweep", BUILD / f"emu{i}.log"
    with open(log, "w") as f:
        subprocess.run([str(V.EMULATOR), *V.EMU_ARGS, "--inst-limit", str(n), "--dump-state", str(out),
                        "--sweep-parcels", f"{lo}:{hi}", str(BUILD / "prog.elf")],
                       stdout=f, stderr=subprocess.STDOUT, timeout=3600)
    messages, current = {}, None
    for line in log.read_text().splitlines():
        if line.startswith("sweep 0x"):
            current = int(line[6:], 16)
        else:
            m = MESSAGE.match(line.strip())
            if m and current is not None:
                messages[current] = m.group(1).strip()
    return parse(out), messages


RUN_HEADER = """(* Generated by validation/scripts/sweep.py. *)
From Stdlib Require Import Lists.List ZArith.ZArith.
From Validation Require Import Names Sweep.
Import ListNotations.
"""


def batch_v(words, n, parcels):
    ws = "; ".join(f"0x{w:08x}" for w in words)
    ps = "; ".join(f"0x{p:04x}" for p in parcels)
    return RUN_HEADER + f"""Definition prog : list Z := [{ws}]%Z.
Definition parcels : list Z := [{ps}]%Z.
Lemma print_sweep : True.
  Time Redirect "rocq" ltac2:(print_plines constr:(sweep_output {n} prog parcels)).
  exact I.
Defined.
"""


def run_rocq(i, source, cache):
    """Batch i in its own directory; rocq.out (from the cache if there)."""
    d = BUILD / "rocq" / str(i)
    d.mkdir(parents=True, exist_ok=True)
    target = d / "rocq.out"
    if cache and cache.is_file():
        target.write_bytes(cache.read_bytes())
        return target, None
    (d / "Batch.v").write_text(source)
    target.unlink(missing_ok=True)
    code, times = V.coqc(d, "Batch.v")
    if code != 0 or not target.is_file():
        sys.exit(f"sweep: coqc exited {code}: see {d / 'coqc.log'}")
    if cache:
        cache.parent.mkdir(parents=True, exist_ok=True)
        tmp = cache.with_name(f"{cache.name}.{os.getpid()}.tmp")
        tmp.write_bytes(target.read_bytes())
        os.replace(tmp, cache)
    return target, times[0] if times else None


def classify(p, rocq, emu, emu_message):
    """Class name of a parcel's difference, or None if unexplained."""
    (r_lines, r_end), (e_lines, e_end) = rocq, emu
    r_fail = next((l[len("step_result "):] for l in r_lines if l.startswith('step_result "')), None)
    # Both stop on the same Sail assertion or exception.
    if r_fail is not None and e_end is not None and e_end.startswith("stop") and emu_message \
            and (emu_message in r_fail or "message computed at run time" in r_fail):
        return "both stop"
    return None


def main():
    global V
    V = load_validate()
    p = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument("--range", default="0:0x10000")
    p.add_argument("-j", "--jobs", type=int, default=2)
    p.add_argument("--batches", type=int, default=16)
    p.add_argument("--no-cache", action="store_true")
    p.add_argument("--cache-dir", type=Path, default=V.DEFAULT_CACHE)
    a = p.parse_args()
    lo, hi = (int(x, 0) for x in a.range.split(":"))
    start = time.time()
    BUILD.mkdir(parents=True, exist_ok=True)
    words, slot = assemble()
    n = setup_steps(slot)
    parcels = parcels_in(lo, hi)

    # Emulator: one process per CPU.
    ranges = split(lo, hi, os.cpu_count() or 4)
    with concurrent.futures.ThreadPoolExecutor(len(ranges)) as pool:
        emu_parts = list(pool.map(lambda x: run_emulator(x[0], *x[1], n), enumerate(ranges)))
    emu_fixed = emu_parts[0][0][0]
    emu, emu_messages = {}, {}
    for (fixed, res), messages in emu_parts:
        emu.update(res)
        emu_messages.update(messages)

    # Rocq: batches, at most --jobs at a time.
    fingerprint = V.load_run_tests().coq_fingerprint("coqc")
    digest = hashlib.sha256(V.sources_digest().encode())
    digest.update((VAL / "rocq" / "Sweep.v").read_bytes())
    sources = digest.hexdigest()
    size = -(-len(parcels) // a.batches)
    groups = [parcels[i:i + size] for i in range(0, len(parcels), size)]
    jobs = []
    for i, g in enumerate(groups):
        source = batch_v(words, n, g)
        cache = None if a.no_cache else a.cache_dir / f"sweep-{V.cache_key(fingerprint, sources, source)}.out"
        jobs.append((i, source, cache))
    with concurrent.futures.ThreadPoolExecutor(a.jobs) as pool:
        outs = list(pool.map(lambda j: run_rocq(*j), jobs))
    rocq, rocq_fixed = {}, None
    seconds = [t for _, t in outs if t is not None]
    for path, _ in outs:
        fixed, res = parse(path)
        rocq_fixed = rocq_fixed or fixed
        rocq.update(res)

    # Compare.
    report = [f"# Parcel sweep\n\nParcels {len(parcels)} in [0x{lo:04x}, 0x{hi:04x}), "
              f"from slot 0x{slot:08x} after {n} setup steps.\n"]
    bad = []
    if sorted(rocq_fixed) != sorted(emu_fixed):
        diff = sorted(set(rocq_fixed) ^ set(emu_fixed))
        bad.append("fixed")
        report.append("\n## Fixed states differ\n\n```\n" + "\n".join(
            ("rocq " if l in rocq_fixed else "emu  ") + l for l in diff) + "\n```\n")
    classes = Counter()
    unexplained = []
    by_class = defaultdict(list)
    for q in parcels:
        r, e = rocq.get(q), emu.get(q)
        if r is None or e is None:
            unexplained.append((q, "missing on " + ("Rocq" if r is None else "the emulator")))
            continue
        if r == e:
            classes["equal"] += 1
            continue
        c = classify(q, r, e, emu_messages.get(q))
        if c:
            classes[c] += 1
            by_class[c].append(q)
        else:
            unexplained.append((q, None))
    classes["unexplained"] = len(unexplained)
    report.append("\n## Results\n\n| class | parcels |\n|---|---|\n" + "".join(f"| {k} | {v} |\n" for k, v in classes.items()))
    for c, qs in by_class.items():
        report.append(f"\n### {c}\n\n" + " ".join(f"0x{q:04x}" for q in qs) + "\n")
    if unexplained:
        report.append("\n## Unexplained (first 50)\n")
        for q, why in unexplained[:50]:
            if why:
                report.append(f"\n- 0x{q:04x}: {why}\n")
                continue
            (rl, rend), (el, eend) = rocq[q], emu[q]
            rs, es = set(rl), set(el)
            report.append(f"\n### 0x{q:04x}\n\n```\nrocq {rend}, emulator {eend}"
                          + (f" ({emu_messages[q]})" if q in emu_messages else "") + "\n"
                          + "".join(f"rocq only: {l}\n" for l in rl if l not in es)
                          + "".join(f"emu only:  {l}\n" for l in el if l not in rs) + "```\n")
    (BUILD / "report.md").write_text("".join(report))
    summary = ", ".join(f"{v} {k}" for k, v in classes.items())
    rocq_time = f"Rocq {sum(seconds):.0f} s in {len(seconds)} uncached batches" if seconds else "Rocq cached"
    print(f"sweep: {len(parcels)} parcels: {summary}; fixed states "
          f"{'differ' if 'fixed' in bad else 'equal'} ({rocq_time}; {time.time() - start:.0f} s); "
          f"report: {BUILD / 'report.md'}")
    sys.exit(1 if bad or unexplained else 0)


if __name__ == "__main__":
    main()
