"""Tests for lib.callgraph, lib.dangling, lib.piles and lib.items, on the fixture run."""

import tempfile
import unittest
from pathlib import Path

from lib import callgraph, dangling, extract, piles
from lib.items import FIELDS, read_items, write_items

FIXTURES = Path(__file__).resolve().parent / 'fixtures'
RUN = FIXTURES / 'run'
SOURCE = extract.NanosailSource(FIXTURES / 'source')
DUMP = RUN / 'dump' / 'rw_rewrite_12_toplevel_let_patterns.sail'


def fixture_items():
    return extract.run_items(RUN, SOURCE)


class CallGraphTests(unittest.TestCase):
    graph = callgraph.CallGraph.from_dump(DUMP)

    def test_last_dump_is_chosen_by_rewrite_number(self):
        # rewrite 12 sorts before rewrite 3 as text
        self.assertEqual(callgraph.last_dump(RUN / 'dump'), DUMP)

    def test_last_dump_without_dumps(self):
        with tempfile.TemporaryDirectory() as directory, self.assertRaises(FileNotFoundError):
            callgraph.last_dump(Path(directory))

    def test_functions_include_clauses_and_exclude_vals_and_types(self):
        self.assertEqual(self.graph.functions,
                         {'step', 'fetch', 'helper', 'bool_to_bits', 'execute_ADDI', 'execute', 'unused'})

    def test_reachable(self):
        self.assertEqual(self.graph.reachable(['step']), {'step', 'fetch', 'helper'})
        self.assertEqual(self.graph.reachable(['execute', 'not_a_function']), {'execute', 'execute_ADDI', 'bool_to_bits'})

    def test_mark_reachable(self):
        items = [dict(name=name) for name in ('helper', 'bool_to_bits', 'unused', 'regidx', 'truncate')]
        callgraph.mark_reachable(items, self.graph)
        self.assertEqual({r['name']: r['reachable'] for r in items},
                         # regidx is a type that fetch mentions; truncate is an extern nobody calls
                         {'helper': 'step', 'bool_to_bits': 'execute', 'unused': '', 'regidx': 'step', 'truncate': ''})


class DanglingCallsTests(unittest.TestCase):
    def test_classification(self):
        calls = dangling.dangling_calls((RUN / 'machine.v').read_text(), DUMP.read_text(), fixture_items(), 'run')
        self.assertEqual([(d.callee, d.calls, d.cls) for d in calls],
                         [('truncate', 1, 'extern'),
                          ('undefined_bool', 1, 'skipped by configuration'),
                          ('mystery', 1, 'unknown'),
                          ('bool_to_bits', 2, 'untranslated')])
        self.assertEqual(calls[0].binding, 'coq: "truncate", _: "truncate"')
        self.assertEqual(dangling.summary(calls)['untranslated'], (1, 2))

    def test_an_extern_is_an_extern_even_when_the_configuration_skips_it(self):
        dump = DUMP.read_text() + '\nval undefined_bool = monadic {_: "undefined_bool"}: unit -> bool\n'
        calls = dangling.dangling_calls((RUN / 'machine.v').read_text(), dump, fixture_items(), 'run')
        self.assertEqual({d.callee: d.cls for d in calls}['undefined_bool'], 'extern')

    def test_items_of_other_runs_are_ignored(self):
        calls = dangling.dangling_calls((RUN / 'machine.v').read_text(), DUMP.read_text(), fixture_items(), 'other')
        self.assertEqual({d.callee: d.cls for d in calls}['bool_to_bits'], 'unknown')


class PileTests(unittest.TestCase):
    def setUp(self):
        self.items = fixture_items()
        callgraph.mark_reachable(self.items, callgraph.CallGraph.from_dump(DUMP))
        piles.assign_piles(self.items)

    def pile(self, **fields):
        [record] = [r for r in self.items if all(r[k] == v for k, v in fields.items())]
        return record['pile'][:3]

    def test_rules(self):
        self.assertEqual(self.pile(stage='untranslated', name='bool_to_bits'), 'P04')   # by message
        self.assertEqual(self.pile(stage='untranslated', name='count_down'), 'P05')     # by construct
        self.assertEqual(self.pile(stage='untranslated', name='file_start'), 'P12')
        self.assertEqual(self.pile(stage='nyi', name='sail_mask'), 'P01')
        self.assertEqual(self.pile(stage='skipped'), 'P14')
        self.assertEqual(self.pile(stage='warning', name='sail_mask'), 'P15')
        self.assertEqual(self.pile(stage='nyi', message='first hole'), '')

    def test_first_matching_rule_wins(self):
        # a hard failure is P01, whatever its message
        self.assertEqual(piles.pile_of(dict(stage='hard-failure', message='Ignored cast', construct='')), piles.RULES[0][0])

    def test_table(self):
        table, unassigned = piles.pile_table(self.items)
        p04 = table[piles.RULES[3][0]]
        self.assertEqual((p04.items, p04.definitions, p04.reachable), (2, {'bool_to_bits'}, {'bool_to_bits'}))
        self.assertEqual(dict(p04.stages), {'untranslated': 1, 'error': 1})
        # the two NYIs in two_holes match no rule; coqc errors are not counted
        self.assertEqual(unassigned, {('nyi', 'statement', 'first hole'): 1, ('nyi', 'statement', 'second hole'): 1})
        text = piles.format_pile_table(table, unassigned)
        self.assertIn('not assigned to a pile:', text)


class ItemsFileTests(unittest.TestCase):
    def test_round_trip_keeps_extra_columns_after_the_standard_ones(self):
        items = fixture_items()
        for record in items:
            record['reachable'] = 'step'
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / 'items.csv'
            write_items(path, items)
            self.assertEqual(path.read_text().splitlines()[0], ','.join(FIELDS + ['reachable']))
            self.assertEqual(read_items(path), items)


if __name__ == '__main__':
    unittest.main()
