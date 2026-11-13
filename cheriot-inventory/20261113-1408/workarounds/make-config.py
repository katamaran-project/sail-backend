#!/usr/bin/env python3
"""Make configuration.lisp for the inventory from the case study's configuration.lisp:
add the function names in hard-failure-ignores.txt (one per line) to the
ignore-function-definition-and-top-level-type-constraints-predicate list.
Usage: make-config.py <original configuration.lisp> <ignores.txt> <output>"""
import sys
src, ignores, out = sys.argv[1:4]
text = open(src).read()
names = [l.strip() for l in open(ignores) if l.strip() and not l.startswith('#')]
anchor = '                 "slice_mask"\n'
assert anchor in text, 'anchor not found'
extra = ''.join(f'                 "{n}" ; inventory: hard generation failure\n' for n in names)
open(out, 'w').write(text.replace(anchor, anchor + extra, 1))
