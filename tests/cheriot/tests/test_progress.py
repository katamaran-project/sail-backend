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


def compilation(file, ok, accepted=(), definitions=()):
    return {'file': file, 'ok': ok,
            'error': None if ok else {'line': 9, 'before': len(accepted), 'accepted': list(accepted),
                                      'definitions': list(definitions)}}


class RocqPositionTests(unittest.TestCase):
    def position(self, *compilations):
        return progress.rocq_position(list(compilations))

    def test_position(self):
        self.assertEqual(self.position(compilation('base.v', False, ['a'], ['a', 'b', 'c'])),
                         {'compiled': 0, 'file': 'base.v', 'accepted': ['a'], 'definitions': ['a', 'b', 'c']})
        self.assertEqual(self.position(compilation('base.v', True), compilation('machine.v', True)),
                         {'compiled': 2, 'file': None, 'accepted': [], 'definitions': []})
        self.assertIsNone(progress.rocq_position(None))

    def test_more_files_compiled_is_better(self):
        old = self.position(compilation('base.v', False, ['a'] * 300, ['a']))
        new = self.position(compilation('base.v', True), compilation('machine.v', False))
        self.assertEqual(progress.compare_rocq(old, new), 'better')
        self.assertEqual(progress.compare_rocq(new, old), 'worse')

    def test_accepting_more_is_better(self):
        old = self.position(compilation('base.v', False, ['a'], ['a', 'b', 'c']))
        new = self.position(compilation('base.v', False, ['a', 'b'], ['a', 'b', 'c']))
        self.assertEqual(progress.compare_rocq(old, new), 'better')
        self.assertEqual(progress.compare_rocq(new, old), 'worse')

    def test_ignoring_definitions_before_the_error_is_not_worse(self):
        # the configuration now ignores a and b, which Rocq accepted before; c still fails
        old = self.position(compilation('base.v', False, ['a', 'b'], ['a', 'b', 'c']))
        new = self.position(compilation('base.v', False, [], ['c']))
        self.assertIsNone(progress.compare_rocq(old, new))

    def test_same_place(self):
        old = self.position(compilation('base.v', False, ['a'], ['a', 'b']))
        self.assertIsNone(progress.compare_rocq(old, old))


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
        failing = {'plain': [compilation('base.v', False, ['a'], ['a', 'b', 'c'])],
                   'probe': [compilation('base.v', False, ['a'], ['a', 'b', 'c'])]}
        further = copy.deepcopy(failing)
        further['probe'] = [compilation('base.v', False, ['a', 'b'], ['a', 'b', 'c'])]
        comparison = progress.compare(make_snapshot(compilations=failing), make_snapshot(compilations=further))
        self.assertEqual(comparison.improvements,
                         ['Rocq probe: gets further (0 -> 0 files compiled; 1 -> 2 definitions accepted in base.v)'])
        comparison = progress.compare(make_snapshot(compilations=further), make_snapshot(compilations=failing))
        self.assertEqual(comparison.result, 'REGRESSION')
        self.assertIn('no longer accepted: b', comparison.regressions[0])
        comparison = progress.compare(make_snapshot(compilations=failing), make_snapshot())
        self.assertEqual(comparison.notes, ['Rocq did not run, so it was not compared'])

    def test_a_stuck_compilation_is_not_compared(self):
        failing = {'plain': [compilation('base.v', False, ['a', 'b'], ['a', 'b', 'c'])],
                   'probe': [compilation('base.v', False, ['a', 'b'], ['a', 'b', 'c'])]}
        stuck = copy.deepcopy(failing)
        stuck['probe'] = [{**compilation('base.v', False, ['a'], ['a', 'b', 'c']),
                           'stuck': {'line': 7, 'command': 'Derive EqDec for b.', 'seconds': 45.2, 'limit': 45,
                                     'definition': 'b'}}]
        comparison = progress.compare(make_snapshot(compilations=failing), make_snapshot(compilations=stuck))
        self.assertEqual((comparison.result, comparison.regressions), ('STUCK', []))
        self.assertEqual(comparison.stuck, ['Rocq probe now: coqc stopped at base.v:7 in b after 45 s on one command '
                                            '(limit 45 s): Derive EqDec for b.; not compared'])
        comparison = progress.compare(make_snapshot(compilations=stuck), make_snapshot(compilations=failing))
        self.assertEqual(comparison.result, 'STUCK')
        self.assertIn('Rocq probe in the baseline', comparison.stuck[0])

    def test_rocq_in_an_old_baseline_is_not_compared(self):
        failing = {'plain': [compilation('base.v', False, ['a'], ['a'])], 'probe': [compilation('base.v', False, ['a'], ['a'])]}
        old = make_snapshot(compilations=failing)
        old['rocq'] = {'plain': [0, 39], 'probe': [0, 39]}
        comparison = progress.compare(old, make_snapshot(compilations=failing))
        self.assertEqual(comparison.result, 'UNCHANGED')
        self.assertEqual(len(comparison.notes), 2)

    def test_scope_changes_are_notes_not_regressions(self):
        before = make_snapshot({'f': 'ok', 'gone': 'ok'})
        after = make_snapshot({'f': 'ok', 'new': 'missing'}, scope='y')
        comparison = progress.compare(before, after)
        self.assertEqual(comparison.result, 'UNCHANGED')
        self.assertEqual(comparison.notes, ['1 definitions entered the scope: new', '1 definitions left the scope: gone',
                                            "scope differs from the baseline's (x -> y)"])

    def test_policy_exclusions_are_notes_not_regressions(self):
        # print_helper (missing in the baseline) is now excluded as a no-op: it leaves the scope,
        # which is a note, and it is not a regression
        before = make_snapshot({'f': 'ok', 'print_helper': 'missing', 'gone': 'ok'})
        after = progress.snapshot({'f': 'ok'}, {}, {'execute_A': []}, None,
                                  {'commit': 'c', 'date': 'd', 'cheriot-sail': 's', 'scope': 'y'},
                                  {'print_helper': 'no-op'})
        comparison = progress.compare(before, after)
        self.assertEqual(comparison.result, 'UNCHANGED')
        self.assertEqual(comparison.notes, ['1 definitions newly policy-excluded: print_helper (no-op)',
                                            '1 definitions left the scope because they are policy-excluded: print_helper',
                                            '1 definitions left the scope: gone',
                                            "scope differs from the baseline's (x -> y)"])

    def test_a_definition_no_longer_excluded_is_a_note(self):
        before = progress.snapshot({'f': 'ok'}, {}, {'execute_A': []}, None,
                                   {'commit': 'c', 'date': 'd', 'cheriot-sail': 's', 'scope': 'x'}, {'g': 'failure'})
        comparison = progress.compare(before, make_snapshot({'f': 'ok', 'g': 'missing'}))
        self.assertEqual(comparison.result, 'UNCHANGED')
        self.assertEqual(comparison.notes, ['1 definitions no longer policy-excluded: g',
                                            '1 definitions entered the scope: g'])



if __name__ == '__main__':
    unittest.main()
