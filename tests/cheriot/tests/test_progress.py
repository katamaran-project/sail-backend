"""Tests for lib.progress: statuses, clean instructions, blockers, snapshots and the ratchet."""

import copy
import unittest

from lib import callgraph, progress, scope
from lib.dangling import DanglingCall

DUMP = '''\
function step() = execute(x)

function clause execute ADDI(x) = execute_ADDI(x)

function clause execute SUB(x) = execute_SUB(x)

function clause execute OUT(x) = execute_OUT(x)

function execute_ADDI x = add_helper(x)

function execute_SUB x = sub_helper(x) + const_v

function execute_OUT x = ()

function add_helper x = x

function sub_helper x = x

let const_v = 3
'''

MACHINE_V = '\n'.join(f'Definition fun_{name} : Stm [] ty.unit := stm_exp tt.'
                      for name in ('step', 'execute', 'execute_ADDI', 'execute_SUB', 'add_helper', 'sub_helper'))

GRAPH = callgraph.CallGraph.from_dump_text(DUMP)
SCOPE = scope.compute(GRAPH, scope.ScopeFile(['insts.sail'], ['step'], {'execute': 'execute_'}),
                      {'insts.sail': 'union clause ast = ADDI : unit\nunion clause ast = SUB : unit\n'})
ITEMS = [dict(run='r', stage='nyi', kind='µSail Definition', name='sub_helper') for _ in range(2)]
CALLS = [DanglingCall('execute_OUT', 1, 'outside the scope', '', ['execute']),
         DanglingCall('extern_f', 1, 'extern', '', ['add_helper'])]


def make_snapshot(statuses=None, holes=None, blockers=None, compilations=None, **identity):
    return progress.snapshot(statuses if statuses is not None else {'f': 'ok'}, holes or {},
                             blockers if blockers is not None else {'execute_A': []}, compilations,
                             {'commit': 'c', 'date': 'd', 'cheriot-sail': 's', 'scope': 'x', **identity})


class StatusTests(unittest.TestCase):
    def test_statuses(self):
        statuses, holes = progress.definition_statuses(GRAPH, SCOPE, MACHINE_V, ITEMS)
        self.assertEqual(statuses, {'step': 'ok', 'execute': 'ok', 'execute_ADDI': 'ok', 'execute_SUB': 'ok',
                                    'add_helper': 'ok', 'sub_helper': 'holes', 'const_v': 'ok'})
        self.assertEqual(holes, {'sub_helper': 2})

    def test_missing_function_and_untranslated_value(self):
        items = ITEMS + [dict(run='r', stage='untranslated', kind='let', name='const_v')]
        statuses, _ = progress.definition_statuses(GRAPH, SCOPE, MACHINE_V.replace('fun_add_helper', 'fun_other'), items)
        self.assertEqual((statuses['add_helper'], statuses['const_v']), ('missing', 'missing'))

    def test_out_of_scope_definitions_have_no_status(self):
        statuses, _ = progress.definition_statuses(GRAPH, SCOPE, MACHINE_V, ITEMS)
        self.assertNotIn('execute_OUT', statuses)


class CleanTests(unittest.TestCase):
    statuses, _ = progress.definition_statuses(GRAPH, SCOPE, MACHINE_V, ITEMS)

    def test_blockers(self):
        # execute_ADDI's cone calls an extern; execute_SUB's has holes. execute's call to the
        # out-of-scope execute_OUT is not in either cone.
        self.assertEqual(progress.instruction_blockers(GRAPH, SCOPE, self.statuses, CALLS),
                         {'execute_ADDI': ['extern_f (extern)'], 'execute_SUB': ['sub_helper (holes)']})

    def test_clean(self):
        blockers = progress.instruction_blockers(GRAPH, SCOPE, self.statuses, CALLS[:1])
        self.assertEqual(blockers['execute_ADDI'], [])

    def test_ranking(self):
        ranking = progress.blocker_ranking({'a': ['x', 'y'], 'b': ['y'], 'c': []})
        self.assertEqual(ranking, [('y', 2), ('x', 1)])


class RocqPositionTests(unittest.TestCase):
    def compilation(self, file, ok, before=0):
        return {'file': file, 'ok': ok, 'error': None if ok else {'line': 9, 'before': before}}

    def test_positions_compare_by_files_then_definitions(self):
        base_fails_early = progress.rocq_position([self.compilation('base.v', False, 24)])
        base_fails_later = progress.rocq_position([self.compilation('base.v', False, 300)])
        machine_fails = progress.rocq_position([self.compilation('base.v', True), self.compilation('machine.v', False, 5)])
        both = progress.rocq_position([self.compilation('base.v', True), self.compilation('machine.v', True)])
        self.assertEqual(base_fails_early, [0, 24])
        self.assertTrue(base_fails_early < base_fails_later < machine_fails < both)
        self.assertIsNone(progress.rocq_position(None))


class CompareTests(unittest.TestCase):
    def test_unchanged(self):
        comparison = progress.compare(make_snapshot(), make_snapshot())
        self.assertEqual((comparison.result, comparison.regressions, comparison.improvements), ('UNCHANGED', [], []))

    def test_a_worse_definition_is_a_regression_even_if_others_improve(self):
        before = make_snapshot({'f': 'ok', 'g': 'missing', 'h': 'holes'}, {'h': 1})
        after = make_snapshot({'f': 'holes', 'g': 'ok', 'h': 'holes'}, {'f': 1, 'h': 3})
        comparison = progress.compare(before, after)
        self.assertEqual(comparison.result, 'REGRESSION')
        self.assertEqual(comparison.regressions, ['f: ok -> holes', 'h: 1 -> 3 NYI holes'])
        self.assertEqual(comparison.improvements, ['g: missing -> ok'])

    def test_fewer_holes_is_progress(self):
        comparison = progress.compare(make_snapshot({'h': 'holes'}, {'h': 3}), make_snapshot({'h': 'holes'}, {'h': 1}))
        self.assertEqual((comparison.result, comparison.improvements), ('PROGRESS', ['h: 3 -> 1 NYI holes']))

    def test_clean_instructions(self):
        before = make_snapshot(blockers={'execute_A': [], 'execute_B': ['x']})
        after = make_snapshot(blockers={'execute_A': ['y'], 'execute_B': []})
        comparison = progress.compare(before, after)
        self.assertEqual(comparison.regressions, ['execute_A: no longer clean'])
        self.assertEqual(comparison.improvements, ['execute_B: now clean'])

    def test_rocq(self):
        failing = {'plain': [{'file': 'base.v', 'ok': False, 'error': {'line': 1, 'before': 24}}],
                   'probe': [{'file': 'base.v', 'ok': False, 'error': {'line': 1, 'before': 24}}]}
        further = copy.deepcopy(failing)
        further['probe'][0]['error']['before'] = 100
        comparison = progress.compare(make_snapshot(compilations=failing), make_snapshot(compilations=further))
        self.assertEqual(comparison.improvements, ['Rocq probe: gets further ([0, 24] -> [0, 100])'])
        comparison = progress.compare(make_snapshot(compilations=further), make_snapshot(compilations=failing))
        self.assertEqual(comparison.result, 'REGRESSION')
        comparison = progress.compare(make_snapshot(compilations=failing), make_snapshot())
        self.assertEqual(comparison.notes, ['Rocq did not run, so it was not compared'])

    def test_scope_changes_are_notes_not_regressions(self):
        before = make_snapshot({'f': 'ok', 'gone': 'ok'})
        after = make_snapshot({'f': 'ok', 'new': 'missing'}, scope='y')
        comparison = progress.compare(before, after)
        self.assertEqual(comparison.result, 'UNCHANGED')
        self.assertEqual(comparison.notes, ['1 definitions entered the scope: new', '1 definitions left the scope: gone',
                                            "scope differs from the baseline's (x -> y)"])


if __name__ == '__main__':
    unittest.main()
