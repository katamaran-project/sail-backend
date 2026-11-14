"""
The numbers of one run (metrics.json), and the report that run.py prints.

With a scope (lib/scope.py), every count is restricted to it: items count when
their definition is in scope (or belongs to no definition, like coqc errors),
and the totals of functions and instructions are those in scope.
"""

from __future__ import annotations

import re
from collections import Counter
from typing import Any, Optional

from . import dangling, piles, rocq
from .scope import Scope
from .callgraph import CallGraph
from .items import Item

FAILURE_STAGES = ('untranslated', 'nyi', 'hard-failure', 'error')
GENERATED_FUNCTION = re.compile(r'Definition fun_([A-Za-z0-9_#]+) : Stm')
INSTRUCTION_PREFIX = 'execute_'

Metrics = dict[str, Any]


def generated_functions(machine_v: str) -> set[str]:
    """The Sail functions that machine.v defines (as fun_<name>)."""
    return set(GENERATED_FUNCTION.findall(machine_v))


def in_scope(items: list[Item], scope: Optional[Scope]) -> list[Item]:
    """The items whose definition is in scope (all items without a scope)."""
    if scope is None:
        return items
    return [record for record in items if not record['name'] or scope.includes(record['name'])]


def compute(items: list[Item], machine_v: str, graph: CallGraph, calls: list[dangling.DanglingCall],
            compilations: Optional[dict[str, Any]] = None, scope: Optional[Scope] = None) -> Metrics:
    """
    The metrics of a run. `items` need the columns `reachable` and `pile`;
    `machine_v` is the text of machine.v ('' if there is none); `compilations` holds the
    plain and probe compilations (run.py's Rocq stage), or None if Rocq was skipped;
    `scope` restricts the counts, if given.
    """
    items = in_scope(items, scope)
    functions = graph.functions & scope.nodes if scope else graph.functions
    definitions: dict[str, set[tuple[str, str]]] = {}
    for record in items:
        definitions.setdefault(record['stage'], set()).add((record['kind'], record['name']))
    generated = generated_functions(machine_v)
    instructions = {f for f in functions if f.startswith(INSTRUCTION_PREFIX)}
    failing_reachable = {(r['kind'], r['name']) for r in items
                         if r['stage'] in FAILURE_STAGES and r.get('reachable')}
    table, unassigned = piles.pile_table(items)
    return {
        'items': dict(sorted(Counter(r['stage'] for r in items).items())),
        'definitions': {stage: len(names) for stage, names in sorted(definitions.items())},
        'untranslated_by_kind': dict(sorted(Counter(kind for kind, _ in definitions.get('untranslated', ())).items())),
        'nyi': {'holes': sum(1 for r in items if r['stage'] == 'nyi'),
                'definitions': len({r['name'] for r in items if r['stage'] == 'nyi'})},
        'hard_failures': sorted({r['name'] for r in items if r['stage'] == 'hard-failure'}),
        'functions': {'total': len(functions), 'generated': len(functions & generated)},
        'instructions': {'total': len(instructions), 'generated': len(instructions & generated)},
        'failing_reachable_definitions': len(failing_reachable),
        'calls_missing': {cls: {'callees': callees, 'calls': count}
                          for cls, (callees, count) in sorted(dangling.summary(calls).items())},
        'piles': {name: {'items': pile.items, 'definitions': len(pile.definitions), 'reachable': len(pile.reachable)}
                  for name, pile in sorted(table.items())},
        'unassigned_failures': sum(unassigned.values()),
        'rocq': compilations,
        'scope': None if scope is None else {
            'instructions': len(scope.instructions),
            'functions': len(functions),
            'excluded_functions': len(scope.excluded_functions),
            'excluded_values': len(scope.excluded_values),
        },
    }


def format_metrics(metrics: Metrics) -> list[str]:
    """The headline numbers, one per line."""
    instructions, functions, nyi = metrics['instructions'], metrics['functions'], metrics['nyi']
    calls = metrics['calls_missing']
    total_calls = sum(c['calls'] for c in calls.values())
    total_callees = sum(c['callees'] for c in calls.values())
    by_class = ', '.join(f'{cls} {c["calls"]}' for cls, c in sorted(calls.items(), key=lambda kv: -kv[1]['calls']))
    hard = metrics['hard_failures']
    lines = []
    if metrics.get('scope'):
        scope = metrics['scope']
        lines.append(f'scope: the CHERIoT core, {scope["instructions"]} instructions and {scope["functions"]} functions '
                     f'({scope["excluded_functions"]} functions and {scope["excluded_values"]} values excluded)')
    else:
        lines.append('scope: the whole model')
    lines += [
        f'instructions generated   {instructions["generated"]:5} of {instructions["total"]}',
        f'functions generated      {functions["generated"]:5} of {functions["total"]}',
        f'untranslated definitions {metrics["definitions"].get("untranslated", 0):5}',
        f'NYI holes                {nyi["holes"]:5} in {nyi["definitions"]} definitions',
        f'hard failures            {len(hard):5}' + (f'  ({", ".join(hard)})' if hard else ''),
        f'failing, reachable       {metrics["failing_reachable_definitions"]:5} definitions',
        f'missing calls            {total_calls:5} calls to {total_callees} functions ({by_class})',
    ]
    if metrics.get('rocq'):
        for kind in ('plain', 'probe'):
            lines.append(f'rocq {kind:19} {rocq.describe(metrics["rocq"][kind])}')
    return lines

