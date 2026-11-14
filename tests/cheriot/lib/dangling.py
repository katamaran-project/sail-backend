"""
Calls in machine.v (`stm_call f`) to functions f that machine.v never defines.

Each such callee is classified:

    extern                     declared in Sail with an external binding (`val f = {...}`) and no body
    outside the scope          ignored because it is outside the loop's scope (see scope.py)
    skipped by configuration   ignored by the case study's configuration
    untranslated, hard-failure the Sail function failed to translate
    unknown                    none of these
"""

from __future__ import annotations

import re
from collections import Counter
from dataclasses import dataclass, field

from .items import Item

# The definition of a generated function, fun_<name>; functions with size or type parameters
# have implicit arguments before their type: Definition fun_f {kid__n : nat} : Stm ...
DEFINED = re.compile(r'Definition fun_([A-Za-z0-9_#]+)(?:\s*\{[^{}]*\})*\s*: Stm')
CALL = re.compile(r'stm_call\s+([A-Za-z0-9_#]+)')
EXTERN = re.compile(r'^val\s+([A-Za-z0-9_#]+)\s*=\s*(?:monadic|pure)?\s*\{([^}]*)\}', re.M)
HAS_BODY = re.compile(r'^(?:\$\[[^\]]*\]\s*)*function\s+(?:clause\s+)?([A-Za-z0-9_#]+)', re.M)


@dataclass
class DanglingCall:
    callee: str
    calls: int        # number of call sites
    cls: str          # see the module docstring
    binding: str      # the extern binding, if any
    callers: list[str] = field(default_factory=list)    # the generated functions that call it

    FIELDS = ['callee', 'calls', 'cls', 'binding', 'callers']

    def as_row(self) -> dict[str, object]:
        return dict(callee=self.callee, calls=self.calls, cls=self.cls, binding=self.binding,
                    callers=' '.join(self.callers))


def callers_of(machine_v: str) -> dict[str, set[str]]:
    """For each function that machine.v calls, the generated functions (fun_<name>) whose definitions call it."""
    result: dict[str, set[str]] = {}
    starts = list(DEFINED.finditer(machine_v))
    for index, start in enumerate(starts):
        end = starts[index + 1].start() if index + 1 < len(starts) else len(machine_v)
        for callee in CALL.findall(machine_v, start.end(), end):
            result.setdefault(callee, set()).add(start.group(1))
    return result


def dangling_calls(machine_v: str, dump: str, items: list[Item], run: str,
                   excluded: frozenset[str] | set[str] = frozenset()) -> list[DanglingCall]:
    """
    The callees of machine.v that it does not define, sorted by class and then by number of calls.

    `machine_v` and `dump` are the text of machine.v and of a Sail AST dump after the rewrites;
    `items` are the run's items, of which those of run `run` are used; `excluded` holds the
    definitions outside the scope.
    """
    defined = set(DEFINED.findall(machine_v))
    calls = Counter(CALL.findall(machine_v))
    callers = callers_of(machine_v)
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
        elif callee in excluded:
            cls = 'outside the scope'
        elif status.get(callee) == 'skipped':
            cls = 'skipped by configuration'
        elif callee in status:
            cls = status[callee]
        else:
            cls = 'unknown'
        result.append(DanglingCall(callee, count, cls, externs.get(callee, ''), sorted(callers.get(callee, ()))))
    result.sort(key=lambda d: (d.cls, -d.calls))
    return result


def summary(calls: list[DanglingCall]) -> dict[str, tuple[int, int]]:
    """Per class: (number of callees, number of calls)."""
    result: dict[str, tuple[int, int]] = {}
    for d in calls:
        callees, count = result.get(d.cls, (0, 0))
        result[d.cls] = (callees + 1, count + d.calls)
    return result
