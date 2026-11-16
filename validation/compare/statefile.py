#!/usr/bin/env python3
"""Canonical state files (validation/docs/state-format.md).

  statefile.py check [--run] FILE...     grammar and order; exit 1 on the first error
  statefile.py delta BEFORE AFTER        lines of AFTER whose key is new or whose value changed
"""

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
    print(__doc__, file=sys.stderr)
    return 2


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
