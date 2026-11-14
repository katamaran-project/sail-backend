"""
Per-definition progress, clean instructions, and the ratchet (docs/cheriot-loop.md §5).

Each in-scope function and top-level value has a status:

    ok        translated, without NYI holes
    holes     translated, with NYI holes (their number is kept too)
    missing   not translated: untranslated, skipped by the configuration, or failed

An instruction is clean when every function and value in its call cone (what its
execute_<C> reaches) is ok, and none of them calls a function that machine.v does not
define. A clean instruction is ready for Rocq, and later for a contract; the number
of clean instructions is the loop's headline number.

A snapshot holds these statuses and how far Rocq gets. The baseline is the snapshot
that was last accepted (tests/cheriot/baseline.json). compare() lists what got better
and what got worse since the baseline; anything worse is a regression, even when the
totals improved.
"""

from __future__ import annotations

import re
from collections import Counter
from dataclasses import dataclass, field
from typing import Any, Optional

from .callgraph import CallGraph
from .dangling import DanglingCall
from .items import Item
from .scope import Scope

RANK = {'missing': 0, 'holes': 1, 'ok': 2}
GENERATED_FUNCTION = re.compile(r'Definition fun_([A-Za-z0-9_#]+) : Stm')

Snapshot = dict[str, Any]


def definition_statuses(graph: CallGraph, scope: Scope, machine_v: str,
                        items: list[Item]) -> tuple[dict[str, str], dict[str, int]]:
    """The status of each in-scope function and value, and the number of NYI holes of those with holes."""
    generated = set(GENERATED_FUNCTION.findall(machine_v))
    holes = Counter(r['name'] for r in items if r['stage'] == 'nyi')
    untranslated_values = {r['name'] for r in items if r['stage'] == 'untranslated' and r['kind'] == 'let'}
    statuses: dict[str, str] = {}
    for name in sorted(scope.nodes & (graph.functions | graph.values)):
        if name in graph.values:
            statuses[name] = 'missing' if name in untranslated_values else 'ok'
        elif name not in generated:
            statuses[name] = 'missing'
        else:
            statuses[name] = 'holes' if holes[name] else 'ok'
    return statuses, {name: holes[name] for name, status in statuses.items() if status == 'holes'}


def instruction_blockers(graph: CallGraph, scope: Scope, statuses: dict[str, str],
                         calls: list[DanglingCall]) -> dict[str, list[str]]:
    """
    For each in-scope instruction (its execute_<C> function), what keeps it from being clean:
    the definitions in its call cone that are not ok ("name (status)"), and the missing
    functions that the cone calls ("name (class of the missing call)"). Clean instructions
    have no blockers.
    """
    missing_called_by: dict[str, set[str]] = {}
    for call in calls:
        for caller in call.callers:
            missing_called_by.setdefault(caller, set()).add(f'{call.callee} ({call.cls})')
    result = {}
    for function in sorted(scope.dispatched):
        cone = graph.reachable([function]) & set(statuses)
        blockers = {f'{name} ({statuses[name]})' for name in cone if statuses[name] != 'ok'}
        for name in cone:
            blockers |= missing_called_by.get(name, set())
        result[function] = sorted(blockers)
    return result


def blocker_ranking(blockers: dict[str, list[str]]) -> list[tuple[str, int]]:
    """Each blocker, with the number of instructions it blocks; most first."""
    counts = Counter(blocker for found in blockers.values() for blocker in found)
    return sorted(counts.items(), key=lambda kv: (-kv[1], kv[0]))


def rocq_position(compilations: Optional[list[dict[str, Any]]]) -> Optional[list[int]]:
    """
    How far a compilation got, as a comparable [files compiled, definitions before the first error];
    [2, 0] when both files compile. None when Rocq did not run.
    """
    if compilations is None:
        return None
    compiled = sum(1 for c in compilations if c['ok'])
    failed = next((c for c in compilations if not c['ok']), None)
    return [compiled, failed['error']['before'] if failed and failed['error'] else 0]


def snapshot(statuses: dict[str, str], holes: dict[str, int], blockers: dict[str, list[str]],
             compilations: Optional[dict[str, Any]], identity: dict[str, Any]) -> Snapshot:
    return {
        **identity,
        'definitions': statuses,
        'holes': holes,
        'clean_instructions': sorted(f for f, found in blockers.items() if not found),
        'instructions': sorted(blockers),
        'rocq': None if compilations is None else {kind: rocq_position(compilations[kind]) for kind in ('plain', 'probe')},
    }


@dataclass
class Comparison:
    regressions: list[str] = field(default_factory=list)
    improvements: list[str] = field(default_factory=list)
    notes: list[str] = field(default_factory=list)

    @property
    def result(self) -> str:
        if self.regressions:
            return 'REGRESSION'
        return 'PROGRESS' if self.improvements else 'UNCHANGED'


def compare(baseline: Snapshot, current: Snapshot) -> Comparison:
    """What got better and what got worse in `current` since `baseline`."""
    comparison = Comparison()
    before, after = baseline['definitions'], current['definitions']
    for name in sorted(before.keys() & after.keys()):
        old, new = before[name], after[name]
        if RANK[new] < RANK[old]:
            comparison.regressions.append(f'{name}: {old} -> {new}')
        elif RANK[new] > RANK[old]:
            comparison.improvements.append(f'{name}: {old} -> {new}')
        elif new == 'holes':
            old_holes, new_holes = baseline['holes'].get(name, 0), current['holes'].get(name, 0)
            if new_holes > old_holes:
                comparison.regressions.append(f'{name}: {old_holes} -> {new_holes} NYI holes')
            elif new_holes < old_holes:
                comparison.improvements.append(f'{name}: {old_holes} -> {new_holes} NYI holes')
    entered, left = sorted(after.keys() - before.keys()), sorted(before.keys() - after.keys())
    if entered:
        comparison.notes.append(f'{len(entered)} definitions entered the scope: {", ".join(entered[:10])}')
    if left:
        comparison.notes.append(f'{len(left)} definitions left the scope: {", ".join(left[:10])}')
    clean_before, clean_after = set(baseline['clean_instructions']), set(current['clean_instructions'])
    common = set(baseline['instructions']) & set(current['instructions'])
    comparison.regressions += [f'{f}: no longer clean' for f in sorted((clean_before - clean_after) & common)]
    comparison.improvements += [f'{f}: now clean' for f in sorted((clean_after - clean_before) & common)]
    if baseline.get('rocq') and current.get('rocq'):
        for kind in ('plain', 'probe'):
            old, new = baseline['rocq'][kind], current['rocq'][kind]
            if old and new and new < old:
                comparison.regressions.append(f'Rocq {kind}: stops earlier ({old} -> {new}: files compiled, definitions before the error)')
            elif old and new and new > old:
                comparison.improvements.append(f'Rocq {kind}: gets further ({old} -> {new})')
    elif baseline.get('rocq') and not current.get('rocq'):
        comparison.notes.append('Rocq did not run, so it was not compared')
    for key in ('cheriot-sail', 'scope'):
        if baseline.get(key) != current.get(key):
            comparison.notes.append(f'{key} differs from the baseline\'s ({baseline.get(key)} -> {current.get(key)})')
    return comparison
