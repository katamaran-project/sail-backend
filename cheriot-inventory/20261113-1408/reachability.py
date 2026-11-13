#!/usr/bin/env python3
"""
Mark which items of items.csv belong to functions reachable from the model's entry points.

Builds an approximate call graph from the Sail dump after the last rewrite
(bisect/rewrite-dump/rw_rewrite_49_toplevel_let_patterns.sail): an edge f -> g when g's
name occurs as an identifier in f's body. Then adds a column `reachable` to items.csv
(written as items-reachable.csv): `step` when the definition is reachable from `step`
(fetch/decode/execute loop), `execute` when reachable from `execute` only, else empty.

Usage: reachability.py [items.csv] [dump.sail]
"""
import csv
import re
import sys
from collections import deque
from pathlib import Path

here = Path(__file__).resolve().parent
items_path = Path(sys.argv[1]) if len(sys.argv) > 1 else here / 'items.csv'
dump_path = Path(sys.argv[2]) if len(sys.argv) > 2 else here / 'bisect/rewrite-dump/rw_rewrite_49_toplevel_let_patterns.sail'

text = dump_path.read_text(errors='replace')
# top-level definitions are separated by blank lines (bodies contain lines at column 0)
chunks = re.split(r'\n\s*\n', text)
bodies = {}
for chunk in chunks:
    match = re.match(r'(?:\$\[[^\]]*\]\s*)*function\s+(?:clause\s+)?([A-Za-z0-9_#]+)', chunk)
    if match:
        bodies.setdefault(match.group(1), []).append(chunk)
names = set(bodies)
identifier = re.compile(r'[A-Za-z_][A-Za-z0-9_#]*')
graph = {name: {i for chunk in chunks_ for i in identifier.findall(chunk) if i in names and i != name}
         for name, chunks_ in bodies.items()}


def reachable(roots):
    seen = set()
    queue = deque(r for r in roots if r in graph)
    while queue:
        node = queue.popleft()
        if node in seen:
            continue
        seen.add(node)
        queue.extend(graph.get(node, ()))
    return seen


from_step = reachable(['step', 'loop', 'main', 'init_model', 'init_sys', 'ext_init', 'ext_init_regs', 'init_platform'])
from_execute = reachable(['execute', 'ext_decode', 'ext_decode_compressed', 'encdec_backwards',
                          'encdec_compressed_backwards', 'fetch'])

# a type (or other non-function definition) counts as reachable when a reachable function mentions it
def mentioned(functions):
    return {i for f in functions for chunk in bodies[f] for i in identifier.findall(chunk)}
step_identifiers, execute_identifiers = mentioned(from_step), mentioned(from_execute)

rows = list(csv.DictReader(open(items_path)))
for row in rows:
    name = row['name']
    if name in names:
        row['reachable'] = 'step' if name in from_step else 'execute' if name in from_execute else ''
    else:
        row['reachable'] = 'step' if name in step_identifiers else 'execute' if name in execute_identifiers else ''
fields = list(rows[0].keys())
out = items_path.with_name(items_path.stem + '-reachable.csv')
with open(out, 'w', newline='') as handle:
    writer = csv.DictWriter(handle, fieldnames=fields)
    writer.writeheader()
    writer.writerows(rows)
print(f'{len(names)} functions; reachable from step: {len(from_step)}; from execute/decode/fetch: {len(from_execute)}',
      file=sys.stderr)
print(f'wrote {out}', file=sys.stderr)
