#!/usr/bin/env python3
"""
Analyse one translation of the CHERIoT model: the inventory's analysis as one command.

Usage:
    tests/cheriot/inventory.py RUN-DIR [--dump FILE] [--source-root DIR] [--out-dir DIR]

RUN-DIR is a directory in which `sail --katamaran` translated the model (see
lib/extract.py). This writes, to --out-dir (default: RUN-DIR):

    items.csv            every item, with the columns `reachable` and `pile`
    dangling-calls.csv   calls in machine.v to functions it does not define
    piles.txt            the pile table

and prints a summary. Reachability and the classification of missing calls need
a Sail AST dump after the rewrites: --dump, or else the last dump in RUN-DIR/dump/
(as written by `sail --ddump-rewrite-ast RUN-DIR/dump/rw`). Without one, those
parts are skipped.

--source-root is the sail-backend checkout whose plugin made the run (default:
this checkout). Item constructs are read from its nanosail source by line number.
"""

from __future__ import annotations

import argparse
import csv
import sys
from collections import Counter
from pathlib import Path
from typing import Optional

sys.path.insert(0, str(Path(__file__).resolve().parent))

from lib import callgraph, dangling, extract, piles          # noqa: E402
from lib.items import write_items                             # noqa: E402


def find_dump(run_dir: Path, dump: Optional[Path]) -> Optional[Path]:
    if dump is not None:
        return dump
    try:
        return callgraph.last_dump(run_dir / 'dump')
    except FileNotFoundError:
        return None


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument('run_dir', type=Path)
    parser.add_argument('--dump', type=Path, help='Sail AST dump after the rewrites')
    parser.add_argument('--source-root', type=Path, default=extract.THIS_CHECKOUT,
                        help='sail-backend checkout that built the plugin (default: this one)')
    parser.add_argument('--out-dir', type=Path, help='where to write the results (default: RUN-DIR)')
    arguments = parser.parse_args()

    run_dir: Path = arguments.run_dir
    out_dir: Path = arguments.out_dir or run_dir
    out_dir.mkdir(parents=True, exist_ok=True)

    items = extract.run_items(run_dir, extract.NanosailSource(arguments.source_root))
    stages = Counter(record['stage'] for record in items)
    print(f'run {run_dir.name}: ' + ', '.join(f'{stage} {count}' for stage, count in sorted(stages.items())))

    dump = find_dump(run_dir, arguments.dump)
    if dump is None:
        print('no Sail AST dump: reachability and missing calls skipped', file=sys.stderr)
        for record in items:
            record['reachable'] = ''
    else:
        graph = callgraph.CallGraph.from_dump(dump)
        callgraph.mark_reachable(items, graph)
        print(f'call graph from {dump.name}: {len(graph.functions)} functions, '
              f'{len(graph.reachable(callgraph.STEP_ROOTS))} reachable from step, '
              f'{len(graph.reachable(callgraph.EXECUTE_ROOTS))} from execute/decode/fetch')
        machine_v = run_dir / 'machine.v'
        if machine_v.exists():
            calls = dangling.dangling_calls(machine_v.read_text(errors='replace'),
                                            dump.read_text(errors='replace'), items, run_dir.name)
            with open(out_dir / 'dangling-calls.csv', 'w', newline='') as handle:
                writer = csv.DictWriter(handle, fieldnames=dangling.DanglingCall.FIELDS)
                writer.writeheader()
                writer.writerows(d.as_row() for d in calls)
            for cls, (callees, count) in sorted(dangling.summary(calls).items(), key=lambda kv: -kv[1][1]):
                print(f'missing calls, {cls:26} {callees:4} functions {count:5} calls')

    piles.assign_piles(items)
    table = piles.format_pile_table(*piles.pile_table(items))
    (out_dir / 'piles.txt').write_text(table + '\n')
    write_items(out_dir / 'items.csv', items)
    print(table)
    print(f'\nwrote items.csv, piles.txt{", dangling-calls.csv" if dump else ""} to {out_dir}')
    return 0


if __name__ == '__main__':
    sys.exit(main())
