#!/usr/bin/env python3
"""Canonical state files (validation/docs/state-format.md).

  statefile.py check [--run] FILE...     grammar and order; exit 1 on the first error
  statefile.py delta BEFORE AFTER        lines of AFTER whose key is new or whose value changed
  statefile.py split RUN DIR             DIR/reset.state (step 0), DIR/step<i>.state
  statefile.py keys A B                  register keys of A and B are equal; else list the
                                         missing (-) and extra (+) keys of B, exit 1
  statefile.py compare RUN_A RUN_B       first step whose states differ, with diff -u of the
                                         two states; exit 1 if any (validate.md)
"""

import os

import re
import sys

NAME = r"[A-Za-z0-9_]+(?:\.[A-Za-z0-9_]+)*"
VALUE = r'0x[0-9a-f]+|-?[0-9]+|true|false|\(\)|"[^"\n]*"|[A-Za-z_][A-Za-z0-9_\']*|<depth>'
REG_LINE = re.compile(rf"({NAME}) ({VALUE})")
MEM_LINE = re.compile(r"(mem 0x[0-9a-f]{8,}) (0x[0-9a-f]{2})")
TAG_LINE = re.compile(r"(tag 0x[0-9a-f]{8,}) (true|false)")
SECTIONS = [REG_LINE, MEM_LINE, TAG_LINE]


class FormatError(Exception):
    pass


def addr_key(key):
    """Sort key within a section: registers bytewise, mem/tag by numeric address."""
    if key.startswith(("mem ", "tag ")):
        return (len(key), key)  # fixed-width lowercase hex: longer = larger
    return key.encode()


def parse_state(lines, where):
    """[(key, value)] of one state; checks the grammar, section order and key order."""
    items = []
    section = 0
    prev = None
    for n, line in enumerate(lines, 1):
        for s in range(section, len(SECTIONS)):
            m = SECTIONS[s].fullmatch(line)
            if m:
                break
        else:
            raise FormatError(f"{where}:{n}: bad line or section order: {line!r}")
        if s != section:
            section, prev = s, None
        key, value = m.group(1), m.group(2)
        if key.split(" ")[0] in ("mem", "tag") and s == 0:
            raise FormatError(f"{where}:{n}: register named mem/tag: {line!r}")
        if prev is not None and addr_key(key) <= addr_key(prev):
            raise FormatError(f"{where}:{n}: key out of order or repeated: {key!r} after {prev!r}")
        prev = key
        items.append((key, value))
    return items


def read_lines(path):
    with open(path) as f:
        text = f.read()
    if text and not text.endswith("\n"):
        raise FormatError(f"{path}: no newline at end of file")
    return text.splitlines()


def parse_run(lines, where):
    """{step: [(key, value)]}; steps 0, 1, ... in order, each a state."""
    steps = {}
    order = []
    for n, line in enumerate(lines, 1):
        m = re.fullmatch(r"(0|[1-9][0-9]*) (.*)", line)
        if not m:
            raise FormatError(f"{where}:{n}: no step prefix: {line!r}")
        step = int(m.group(1))
        if not order or order[-1] != step:
            if step != len(order):
                raise FormatError(f"{where}:{n}: step {step} where {len(order)} expected")
            order.append(step)
            steps[step] = []
        steps[step].append(m.group(2))
    return {s: parse_state(ls, f"{where} step {s}") for s, ls in steps.items()}


def delta(before, after):
    old = dict(before)
    return [(k, v) for k, v in after if old.get(k) != v]


def render(items):
    return "".join(f"{k} {v}\n" for k, v in items)


def split(run_path, out_dir):
    steps = parse_run(read_lines(run_path), run_path)
    os.makedirs(out_dir, exist_ok=True)
    for step, items in steps.items():
        name = "reset.state" if step == 0 else f"step{step}.state"
        with open(os.path.join(out_dir, name), "w") as f:
            f.write(render(items))


def first_difference(a, b):
    """First step whose states differ in the runs a, b ({step: items}), or None.
    A step present in only one run differs (its state against an empty one)."""
    for step in range(max(len(a), len(b))):
        if a.get(step) != b.get(step):
            return step
    return None


def state_diff(a, b, step, label_a, label_b):
    """diff -u of step's states in runs a and b (missing: empty)."""
    import difflib
    return "".join(difflib.unified_diff(
        render(a.get(step, [])).splitlines(keepends=True),
        render(b.get(step, [])).splitlines(keepends=True),
        f"{label_a} step {step}", f"{label_b} step {step}"))


def reg_keys(path):
    return {k for k, _ in parse_state(read_lines(path), path) if not k.startswith(("mem ", "tag "))}


def main(argv):
    if len(argv) >= 2 and argv[0] == "check":
        run = argv[1] == "--run"
        try:
            for path in argv[2:] if run else argv[1:]:
                lines = read_lines(path)
                parse_run(lines, path) if run else parse_state(lines, path)
        except FormatError as e:
            print(e, file=sys.stderr)
            return 1
        return 0
    if len(argv) == 3 and argv[0] == "delta":
        try:
            before = parse_state(read_lines(argv[1]), argv[1])
            after = parse_state(read_lines(argv[2]), argv[2])
        except FormatError as e:
            print(e, file=sys.stderr)
            return 1
        for k, v in delta(before, after):
            print(k, v)
        return 0
    if len(argv) == 3 and argv[0] == "split":
        try:
            split(argv[1], argv[2])
        except FormatError as e:
            print(e, file=sys.stderr)
            return 1
        return 0
    if len(argv) == 3 and argv[0] == "keys":
        try:
            a, b = reg_keys(argv[1]), reg_keys(argv[2])
        except FormatError as e:
            print(e, file=sys.stderr)
            return 1
        for k in sorted(a - b, key=str.encode):
            print("-", k)
        for k in sorted(b - a, key=str.encode):
            print("+", k)
        return 0 if a == b else 1
    if len(argv) == 3 and argv[0] == "compare":
        try:
            a = parse_run(read_lines(argv[1]), argv[1])
            b = parse_run(read_lines(argv[2]), argv[2])
        except FormatError as e:
            print(e, file=sys.stderr)
            return 1
        step = first_difference(a, b)
        if step is None:
            print(f"equal: {len(a)} states")
            return 0
        print(f"first difference at step {step}")
        sys.stdout.write(state_diff(a, b, step, argv[1], argv[2]))
        return 1
    print(__doc__, file=sys.stderr)
    return 2


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
