"""
Sort items into piles: root causes or missing features, as in the CHERIoT inventory
(cheriot-inventory/20261113-1408/INVENTORY.md, which describes each pile).

The rules are ordered; the first match wins. They key on the item's construct
(the nanosail match arm that raised it, see extract.NanosailSource) and on its message.
"""

from __future__ import annotations

from collections import defaultdict
from dataclasses import dataclass, field

from .items import Item

SIZE_POLYMORPHIC_WARNING = 'Encountered polymorphic function'

RULES = [
    # (pile id, predicate on row)
    ('P01 compile-time-known integer arguments',
     lambda r: r['stage'] == 'hard-failure'
     or 'known at compile time' in r['message']
     or 'expected constant in bitvector type' in r['message']
     or 'failed to determine bitvector size' in r['message']),
    ('P02 polymorphic unions (type parameters)',
     lambda r: r['construct'] in ('Kind.ml: Type', 'Type.ml: TypeVariable')
     or ('expected pattern AP_' in r['message'] and ('Type:Application' in r['message'] or 'Alias["TODO"]' in r['message']))),
    ('P03 nested patterns inside sum constructors',
     lambda r: 'only binder patterns supported in sum constructors' in r['message']),
    ('P04 vector literal with bits(1) elements',
     lambda r: 'Elements of bit vector should all be bits' in r['message']),
    ('P05 loops, try, early return',
     lambda r: r['construct'] in ('Function.ml: AE_for', 'Function.ml: AE_loop', 'Function.ml: AE_try', 'Function.ml: AE_return')),
    ('P06 mutable local variables (var)',
     lambda r: 'assignment to local variable (nonregister)' in r['message']),
    ('P07 register references (register(T), ref, r->F() = v)',
     lambda r: 'unknown type register' in r['message'] or r['construct'] == 'Function.ml: AV_ref'),
    ('P08 top-level let with a computed value',
     lambda r: r['construct'].startswith('ValueDefinition.ml')),
    ('P09 mappings to/from strings and bitvectors (assembly, printing)',
     lambda r: r['construct'] in ('Match.ml: String', 'Match.ml: Bitvector')),
    ('P10 vectors of non-bits (register arrays)',
     lambda r: r['construct'] == 'Type.ml: Vector'),
    ('P11 numeric types: ranges, if in nexp, parametric structs',
     lambda r: r['construct'] in ('Recursive.ml: Nexp_if', 'Type.ml: Range', 'Match.ml: Range', 'Function.ml: Typ_app')
     or r['message'] == 'Unexpected type'),
    ('P12 non-translated definition kinds: overload, pragma',
     lambda r: r['construct'] in ('Translation.ml: DEF_overload', 'Translation.ml: DEF_pragma')),
    ('P13 size-polymorphic functions (int(n), bits(n) in signatures)',
     lambda r: r['stage'] == 'warning' and SIZE_POLYMORPHIC_WARNING in r['message']),
    ('P14 skipped by configuration (hidden gaps)',
     lambda r: r['stage'] == 'skipped'),
    ('P15 casts ignored', lambda r: r['message'] == 'Ignored cast'),
]


def pile_of(row: Item) -> str:
    for pile, predicate in RULES:
        if predicate(row):
            return pile
    return ''


def assign_piles(items: list[Item]) -> None:
    """Set the column `pile` of each item (in place); empty when no rule matches."""
    for record in items:
        record['pile'] = pile_of(record)


# Stages that count in the pile table, and those of them that are failures in
# their own right (an unassigned warning is not worth listing).
COUNTED_STAGES = {'untranslated', 'nyi', 'hard-failure', 'error', 'skipped', 'warning'}
FAILURE_STAGES = {'untranslated', 'nyi', 'hard-failure', 'error'}


@dataclass
class Pile:
    items: int = 0
    definitions: set[str] = field(default_factory=set)
    reachable: set[str] = field(default_factory=set)     # definitions reachable from step or execute
    stages: dict[str, int] = field(default_factory=lambda: defaultdict(int))


def pile_table(items: list[Item]) -> tuple[dict[str, Pile], dict[tuple[str, str, str], int]]:
    """
    Per pile, the items, distinct definitions and reachable definitions.
    Also the failures that no rule matched, counted per (stage, construct, start of message).
    Items need the columns `pile` (assign_piles) and `reachable` (callgraph.mark_reachable).
    """
    piles: dict[str, Pile] = defaultdict(Pile)
    unassigned: dict[tuple[str, str, str], int] = defaultdict(int)
    for record in items:
        if record['stage'] not in COUNTED_STAGES:
            continue
        if not record['pile']:
            if record['stage'] in FAILURE_STAGES:
                unassigned[(record['stage'], record['construct'], record['message'][:60])] += 1
            continue
        pile = piles[record['pile']]
        pile.items += 1
        pile.definitions.add(record['name'])
        if record['reachable']:
            pile.reachable.add(record['name'])
        pile.stages[record['stage']] += 1
    return dict(piles), dict(unassigned)


def format_pile_table(piles: dict[str, Pile], unassigned: dict[tuple[str, str, str], int]) -> str:
    lines = [f"{'pile':66} {'items':>6} {'defs':>5} {'reach':>5}  stages"]
    for name in sorted(piles):
        pile = piles[name]
        lines.append(f'{name:66} {pile.items:6} {len(pile.definitions):5} {len(pile.reachable):5}  {dict(pile.stages)}')
    if unassigned:
        lines.append('')
        lines.append('not assigned to a pile:')
        for key, count in sorted(unassigned.items(), key=lambda kv: -kv[1]):
            lines.append(f'  {count:5}  {key}')
    return '\n'.join(lines)
