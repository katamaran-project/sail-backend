#!/usr/bin/env python3
"""Classify the functions that machine.v calls (stm_call) but never defines.
Usage: dangling-calls.py <run-dir> [items-reachable.csv] [rewrite dump]
Writes <run-dir>/dangling-calls.csv: callee, calls, class, binding."""
import csv, re, sys, collections
from pathlib import Path
run = Path(sys.argv[1])
items = Path(sys.argv[2]) if len(sys.argv) > 2 else Path(__file__).parent / 'items-reachable.csv'
dump = Path(sys.argv[3]) if len(sys.argv) > 3 else Path(__file__).parent / 'bisect/rewrite-dump/rw_rewrite_49_toplevel_let_patterns.sail'
m = (run / 'machine.v').read_text()
defined = set(re.findall(r'Definition fun_([A-Za-z0-9_#]+) : Stm', m))
calls = collections.Counter(re.findall(r'stm_call\s+([A-Za-z0-9_#]+)', m))
d = dump.read_text(errors='replace')
externs = {}
for match in re.finditer(r'^val\s+([A-Za-z0-9_#]+)\s*=\s*(?:monadic|pure)?\s*\{([^}]*)\}', d, re.M):
    externs[match.group(1)] = re.sub(r'\s+', ' ', match.group(2)).strip()
has_body = set(re.findall(r'^(?:\$\[[^\]]*\]\s*)*function\s+(?:clause\s+)?([A-Za-z0-9_#]+)', d, re.M))
status = {}
for r in csv.DictReader(open(items)):
    if r['run'] == run.name and r['stage'] in ('skipped', 'untranslated', 'hard-failure') and r['kind'] in ('function', 'val'):
        status.setdefault(r['name'], r['stage'])
rows = []
for callee, n in calls.items():
    if callee in defined:
        continue
    if callee in externs and callee not in has_body:
        cls = 'extern'
    elif status.get(callee) == 'skipped':
        cls = 'skipped by configuration'
    elif callee in status:
        cls = status[callee]
    else:
        cls = 'unknown'
    rows.append(dict(callee=callee, calls=n, cls=cls, binding=externs.get(callee, '')))
rows.sort(key=lambda r: (r['cls'], -r['calls']))
with open(run / 'dangling-calls.csv', 'w', newline='') as h:
    w = csv.DictWriter(h, fieldnames=['callee', 'calls', 'cls', 'binding']); w.writeheader(); w.writerows(rows)
summary = collections.defaultdict(lambda: [0, 0])
for r in rows: summary[r['cls']][0] += 1; summary[r['cls']][1] += r['calls']
for cls, (f, c) in sorted(summary.items(), key=lambda kv: -kv[1][1]):
    print(f'{cls:26} {f:4} functions {c:5} calls  e.g. {[r["callee"] for r in rows if r["cls"] == cls][:14]}')
