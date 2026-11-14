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
from .dangling import DEFINED, DanglingCall
from .items import Item
from .scope import Scope

RANK = {'missing': 0, 'holes': 1, 'ok': 2}
GENERATED_FUNCTION = DEFINED   # see dangling.DEFINED

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


def rocq_position(compilations: Optional[list[dict[str, Any]]]) -> Optional[dict[str, Any]]:
    """
    How far a compilation got: the number of files that compiled, and in the file that failed,
    the names of the definitions Rocq accepted before the first error and of all its definitions.
    None when Rocq did not run.

    Names, not counts, are compared (see compare_rocq): ignoring definitions before the error
    lowers the count without the compilation getting any less far.
    """
    if compilations is None:
        return None
    compiled = sum(1 for c in compilations if c['ok'])
    failed = next((c for c in compilations if not c['ok']), None)
    error = failed['error'] if failed and failed['error'] else {}
    position = {'compiled': compiled, 'file': failed['file'] if failed else None,
                'accepted': error.get('accepted', []), 'definitions': error.get('definitions', [])}
    if failed and failed.get('stuck'):
        position['stuck'] = failed['stuck']
    return position


def compare_rocq(old: dict[str, Any], new: dict[str, Any]) -> Optional[str]:
    """
    'better' or 'worse' if the compilation got further or less far, None if neither.
    More files compiled is better. In the same failing file, it is worse if a definition that
    Rocq accepted before still exists but is no longer accepted, and better if Rocq now accepts
    a definition it did not accept before (and nothing is worse).
    'stuck' if either compilation was stopped by the watchdog (rocq.watch): where it stopped
    says nothing about how far Rocq would get.
    """
    if new.get('stuck') or old.get('stuck'):
        return 'stuck'
    if new['compiled'] != old['compiled']:
        return 'better' if new['compiled'] > old['compiled'] else 'worse'
    if new['file'] is None or new['file'] != old['file']:
        return None
    still_defined = set(new['definitions'])
    accepted_now = set(new['accepted'])
    if any(name in still_defined and name not in accepted_now for name in old['accepted']):
        return 'worse'
    return 'better' if accepted_now - set(old['accepted']) else None


def snapshot(statuses: dict[str, str], holes: dict[str, int], blockers: dict[str, list[str]],
             compilations: Optional[dict[str, Any]], identity: dict[str, Any],
             policy: Optional[dict[str, str]] = None) -> Snapshot:
    return {
        **identity,
        'definitions': statuses,
        'policy_excluded': dict(sorted((policy or {}).items())),
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
    stuck: list[str] = field(default_factory=list)      # Rocq compilations that were not compared

    @property
    def result(self) -> str:
        if self.regressions:
            return 'REGRESSION'
        if self.stuck:
            return 'STUCK'
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
    policy_before, policy_after = baseline.get('policy_excluded', {}), current.get('policy_excluded', {})
    newly_excluded = sorted(policy_after.keys() - policy_before.keys())
    no_longer_excluded = sorted(policy_before.keys() - policy_after.keys())
    if newly_excluded:
        comparison.notes.append(f'{len(newly_excluded)} definitions newly policy-excluded: '
                                + ', '.join(f'{name} ({policy_after[name]})' for name in newly_excluded[:10]))
    if no_longer_excluded:
        comparison.notes.append(f'{len(no_longer_excluded)} definitions no longer policy-excluded: {", ".join(no_longer_excluded[:10])}')
    if entered:
        comparison.notes.append(f'{len(entered)} definitions entered the scope: {", ".join(entered[:10])}')
    left_by_policy = [name for name in left if name in policy_after]
    left_otherwise = [name for name in left if name not in policy_after]
    if left_by_policy:
        comparison.notes.append(f'{len(left_by_policy)} definitions left the scope because they are policy-excluded: '
                                f'{", ".join(left_by_policy[:10])}')
    if left_otherwise:
        comparison.notes.append(f'{len(left_otherwise)} definitions left the scope: {", ".join(left_otherwise[:10])}')
    clean_before, clean_after = set(baseline['clean_instructions']), set(current['clean_instructions'])
    common = set(baseline['instructions']) & set(current['instructions'])
    comparison.regressions += [f'{f}: no longer clean' for f in sorted((clean_before - clean_after) & common)]
    comparison.improvements += [f'{f}: now clean' for f in sorted((clean_after - clean_before) & common)]
    if baseline.get('rocq') and current.get('rocq'):
        for kind in ('plain', 'probe'):
            old, new = baseline['rocq'][kind], current['rocq'][kind]
            if not isinstance(old, dict) or not isinstance(new, dict):
                comparison.notes.append(f'Rocq {kind}: the baseline records it in an older form, so it was not compared')
                continue
            verdict = compare_rocq(old, new)
            where = (f'{old["compiled"]} -> {new["compiled"]} files compiled; '
                     f'{len(old["accepted"])} -> {len(new["accepted"])} definitions accepted in {new["file"] or "-"}')
            if verdict == 'stuck':
                for which, position in (('now', new), ('in the baseline', old)):
                    if position.get('stuck'):
                        stuck = position['stuck']
                        comparison.stuck.append(
                            f'Rocq {kind} {which}: coqc stopped at {position["file"]}:{stuck["line"]}'
                            f'{" in " + stuck["definition"] if stuck.get("definition") else ""} after {stuck["seconds"]:.0f} s '
                            f'on one command (limit {stuck["limit"]:.0f} s): {stuck["command"][:60]}; not compared')
            elif verdict == 'worse':
                lost = [n for n in old['accepted'] if n in set(new['definitions']) and n not in set(new['accepted'])]
                comparison.regressions.append(f'Rocq {kind}: stops earlier ({where}; no longer accepted: {", ".join(lost[:5])})')
            elif verdict == 'better':
                comparison.improvements.append(f'Rocq {kind}: gets further ({where})')
    elif baseline.get('rocq') and not current.get('rocq'):
        comparison.notes.append('Rocq did not run, so it was not compared')
    for key in ('cheriot-sail', 'scope'):
        if baseline.get(key) != current.get(key):
            comparison.notes.append(f'{key} differs from the baseline\'s ({baseline.get(key)} -> {current.get(key)})')
    return comparison
