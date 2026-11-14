"""
The numbers of one run (metrics.json), and the report that run.py prints.

With a scope (lib/scope.py), every count is restricted to it: items count when
their definition is in scope (or belongs to no definition, like coqc errors),
and the totals of functions and instructions are those in scope. Policy-excluded
definitions are outside the scope: they are not failures, and the report counts
them on their own, per policy.
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
GENERATED_FUNCTION = dangling.DEFINED
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
            'policy_excluded': scope.policy_counts(),
        },
    }


def headline(metrics: Metrics) -> dict[str, Optional[int]]:
    """The numbers that the report compares over time, and that history.csv and baseline.json keep."""
    calls = metrics['calls_missing']
    return {
        'instructions clean': metrics['instructions'].get('clean'),
        'instructions generated': metrics['instructions']['generated'],
        'functions generated': metrics['functions']['generated'],
        'untranslated definitions': metrics['definitions'].get('untranslated', 0),
        'NYI holes': metrics['nyi']['holes'],
        'hard failures': len(metrics['hard_failures']),
        'missing calls': sum(c['calls'] for c in calls.values()),
    }


def format_metrics(metrics: Metrics, previous: Optional[dict[str, Any]] = None,
                   baseline: Optional[dict[str, Any]] = None) -> list[str]:
    """
    The report's numbers: the scope, the headline numbers (with those of the previous run and of
    the baseline, when given), the missing calls by class, how far Rocq gets, and the top blockers.
    """
    instructions, functions = metrics['instructions'], metrics['functions']
    calls = metrics['calls_missing']
    lines = []
    if metrics.get('scope'):
        scope = metrics['scope']
        lines.append(f'scope: the CHERIoT core, {scope["instructions"]} instructions and {scope["functions"]} functions '
                     f'({scope["excluded_functions"]} functions and {scope["excluded_values"]} values excluded)')
        policy = scope.get('policy_excluded') or {}
        if any(policy.values()):
            counts = ', '.join(f'{name} {count}' for name, count in policy.items() if count)
            lines.append(f'policy exclusions (removed with their uses, not failures): {sum(policy.values())}: {counts}')
    else:
        lines.append('scope: the whole model')
    totals = {'instructions clean': instructions['total'], 'instructions generated': instructions['total'],
              'functions generated': functions['total']}

    def cell(row: Optional[dict[str, Any]], key: str) -> str:
        value = None if row is None else row.get(key)
        return '-' if value is None or value == '' else str(value)

    lines.append(f'{"":26}{"now":>12}{"previous":>10}{"baseline":>10}')
    for key, value in headline(metrics).items():
        if value is None:
            continue
        now = f'{value} of {totals[key]}' if key in totals else str(value)
        lines.append(f'{key:26}{now:>12}{cell(previous, key):>10}{cell(baseline, key):>10}')
    if metrics['hard_failures']:
        lines.append(f'hard failures: {", ".join(metrics["hard_failures"])}')
    by_class = ', '.join(f'{cls} {c["calls"]}' for cls, c in sorted(calls.items(), key=lambda kv: -kv[1]['calls']))
    lines.append(f'missing calls by class: {by_class}')
    if metrics.get('rocq'):
        for kind in ('plain', 'probe'):
            lines.append(f'rocq {kind:6} {rocq.describe(metrics["rocq"][kind])}')
        for kind in ('plain', 'probe'):
            found = rocq.slowest(metrics['rocq'][kind])
            if found:
                lines.append(f'slowest Rocq commands ({kind}):')
                lines.extend(f'  {t["seconds"]:7.1f} s  {t["file"]}:{t["line"]}'
                             f'{" (" + t["definition"] + ")" if t["definition"] else ""}  {t["command"][:60]}'
                             for t in found)
    if metrics.get('blockers'):
        lines.append('top blockers (instructions each keeps from being clean):')
        lines.extend(f'  {count:3}  {blocker}' for blocker, count in metrics['blockers'][:8])
    return lines
