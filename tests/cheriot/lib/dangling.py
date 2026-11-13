"""
Calls in machine.v (`stm_call f`) to functions f that machine.v never defines.

Each such callee is classified:

    extern                     declared in Sail with an external binding (`val f = {...}`) and no body
    skipped by configuration   ignored by the case study's configuration
    untranslated, hard-failure the Sail function failed to translate
    unknown                    none of these
"""

from __future__ import annotations

import re
from collections import Counter
from dataclasses import dataclass

from .items import Item

DEFINED = re.compile(r'Definition fun_([A-Za-z0-9_#]+) : Stm')
CALL = re.compile(r'stm_call\s+([A-Za-z0-9_#]+)')
EXTERN = re.compile(r'^val\s+([A-Za-z0-9_#]+)\s*=\s*(?:monadic|pure)?\s*\{([^}]*)\}', re.M)
HAS_BODY = re.compile(r'^(?:\$\[[^\]]*\]\s*)*function\s+(?:clause\s+)?([A-Za-z0-9_#]+)', re.M)


@dataclass
class DanglingCall:
    callee: str
    calls: int        # number of call sites
    cls: str          # see the module docstring
    binding: str      # the extern binding, if any

    FIELDS = ['callee', 'calls', 'cls', 'binding']

    def as_row(self) -> dict[str, object]:
        return dict(callee=self.callee, calls=self.calls, cls=self.cls, binding=self.binding)


def dangling_calls(machine_v: str, dump: str, items: list[Item], run: str) -> list[DanglingCall]:
    """
    The callees of machine.v that it does not define, sorted by class and then by number of calls.

    `machine_v` and `dump` are the text of machine.v and of a Sail AST dump after the rewrites;
    `items` are the run's items, of which those of run `run` are used.
    """
    defined = set(DEFINED.findall(machine_v))
    calls = Counter(CALL.findall(machine_v))
    externs = {match.group(1): re.sub(r'\s+', ' ', match.group(2)).strip() for match in EXTERN.finditer(dump)}
    has_body = set(HAS_BODY.findall(dump))
    status: dict[str, str] = {}
    for record in items:
        if (record['run'] == run and record['stage'] in ('skipped', 'untranslated', 'hard-failure')
                and record['kind'] in ('function', 'val')):
            status.setdefault(record['name'], record['stage'])
    result = []
    for callee, count in calls.items():
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
        result.append(DanglingCall(callee, count, cls, externs.get(callee, '')))
    result.sort(key=lambda d: (d.cls, -d.calls))
    return result


def summary(calls: list[DanglingCall]) -> dict[str, tuple[int, int]]:
    """Per class: (number of callees, number of calls)."""
    result: dict[str, tuple[int, int]] = {}
    for d in calls:
        callees, count = result.get(d.cls, (0, 0))
        result[d.cls] = (callees + 1, count + d.calls)
    return result
