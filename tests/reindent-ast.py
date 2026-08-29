#!/usr/bin/env python3
"""Extract the Def:Function ASTs from test.sh output and reindent them.

Everything that is not a Def:Function AST is discarded: the shell chatter
before the ASTs, the Def:TopLevelTypeConstraint blocks, the warnings, and
everything from "Done with translation from Sail to nanoSail" onwards.

The retained blocks are reindented purely by square bracket depth.  Every
opening bracket is followed by a newline and adds two spaces of indentation;
every closing bracket goes on a line of its own.  The one exception is a leaf
group -- an opening bracket whose matching closing bracket comes before any
further opening bracket -- which is kept on a single line, so that
Identifier["foo"] and Type:Int[8] stay compact.

Usage:
    ./reindent-ast.py file...      # process these files
    ./test.sh | ./reindent-ast.py  # or read from stdin
"""

import argparse
import re
import sys

# The blocks we keep, and the marker after which nothing is of interest.
# The block may be indented, as it is when the AST is pasted on its own.
BLOCK_START_RE = re.compile(r'^\s*Def:Function\[')
STOP_RE = re.compile(r'^\s*Done with translation from Sail to nanoSail\s*$')

ANSI_RE = re.compile(r'\x1b\[[0-9;]*m')


def bracket_balance(line):
    """Net [ minus ] on a line, ignoring brackets inside strings."""
    depth = 0
    in_string = False
    i = 0
    while i < len(line):
        c = line[i]
        if in_string:
            if c == '\\':
                i += 2
                continue
            if c == '"':
                in_string = False
        elif c == '"':
            in_string = True
        elif c == '[':
            depth += 1
        elif c == ']':
            depth -= 1
        i += 1
    return depth


def leaf_span(text, start):
    """If the group opening at text[start] closes before any further [ opens,
    return the index just past its ]; otherwise None."""
    i = start + 1
    in_string = False
    while i < len(text):
        c = text[i]
        if in_string:
            if c == '\\':
                i += 2
                continue
            if c == '"':
                in_string = False
        elif c == '"':
            in_string = True
        elif c == '[':
            return None
        elif c == ']':
            return i + 1
        i += 1
    return None


def reindent(text, indent):
    """Reindent one AST block by bracket depth."""
    out = []
    depth = 0
    in_string = False
    fresh = True  # at the start of a line, nothing written on it yet

    def newline():
        nonlocal fresh
        out.append('\n' + ' ' * (indent * depth))
        fresh = True

    i = 0
    while i < len(text):
        c = text[i]
        i += 1

        if in_string:
            out.append(c)
            if c == '\\' and i < len(text):
                out.append(text[i])
                i += 1
            elif c == '"':
                in_string = False
            continue

        if c == '"':
            in_string = True
            out.append(c)
            fresh = False
        elif c == '[':
            end = leaf_span(text, i - 1)
            if end is not None:
                # Leaf group: emit it as one line, minus the printer's layout.
                out.append(re.sub(r'\s*\n\s*', ' ', text[i - 1:end]))
                i = end
                fresh = False
                continue
            out.append(c)
            depth += 1
            newline()
        elif c == ']':
            depth -= 1
            if not fresh:
                newline()
            else:
                # Reuse the line we just opened rather than leaving it blank.
                out[-1] = '\n' + ' ' * (indent * depth)
            out.append(c)
            fresh = False
        elif c == ',':
            out.append(c)
            newline()
        elif c.isspace():
            # Drop the printer's own layout; we supply our own.
            if not fresh:
                out.append(' ')
        else:
            out.append(c)
            fresh = False

    lines = [line.rstrip() for line in ''.join(out).split('\n')]
    return '\n'.join(line for line in lines if line.strip())


def process(lines, indent, out):
    """Print the reindented Def:Function blocks and nothing else."""
    i = 0
    while i < len(lines):
        line = ANSI_RE.sub('', lines[i])
        if STOP_RE.match(line):
            return
        if not BLOCK_START_RE.match(line):
            i += 1
            continue
        # Collect lines until the block's brackets balance out again.
        block = []
        depth = 0
        while i < len(lines):
            block.append(ANSI_RE.sub('', lines[i]))
            depth += bracket_balance(lines[i])
            i += 1
            if depth <= 0:
                break
        out.write(reindent('\n'.join(block), indent) + '\n')


def main():
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument('files', nargs='*', help='files to read (default: stdin)')
    ap.add_argument('--indent', type=int, default=2,
                    help='spaces per bracket level (default: 2)')
    args = ap.parse_args()

    if not args.files:
        process(sys.stdin.read().splitlines(), args.indent, sys.stdout)
        return
    for path in args.files:
        with open(path, encoding='utf-8') as f:
            process(f.read().splitlines(), args.indent, sys.stdout)


if __name__ == '__main__':
    main()
