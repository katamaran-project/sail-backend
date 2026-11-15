"""
The item: one translation-failure signal, such as an untranslated definition or a NYI hole.

An item is a dict with these fields, all strings:

    run          the run directory's name
    stage        untranslated | nyi | warning | error | skipped | hard-failure | coqc-error
    kind         the kind of the Sail definition (function, val, type, ...), when known
    name         the Sail definition (or µSail function) it belongs to, when known
    sail_loc     Sail source location, when known
    ocaml_loc    nanosail source location that raised it ("file line N")
    message      the message
    construct    the Sail construct involved, when it can be determined (heuristic)
    where        where it was found (file:line of the .v file or the log)
    snippet      first line(s) of the Sail code or µSail code involved

Later steps add columns, such as `reachable` (callgraph) and `pile` (piles).
"""

from __future__ import annotations

import csv
from collections import Counter
from pathlib import Path

FIELDS = ['run', 'stage', 'kind', 'name', 'sail_loc', 'ocaml_loc', 'message', 'construct', 'where', 'snippet']

Item = dict[str, str]


def read_items(path: Path) -> list[Item]:
    with open(path, newline='') as handle:
        return list(csv.DictReader(handle))


def write_items(path: Path, items: list[Item]) -> None:
    """Write items as CSV: the standard fields first, then any extra columns in the order they appear."""
    fields = list(FIELDS)
    for record in items:
        for key in record:
            if key not in fields:
                fields.append(key)
    with open(path, 'w', newline='') as handle:
        writer = csv.DictWriter(handle, fieldnames=fields)
        writer.writeheader()
        writer.writerows(items)


def count_by_stage(items: list[Item]) -> Counter[tuple[str, str]]:
    """The number of items per (run, stage)."""
    return Counter((record['run'], record['stage']) for record in items)
