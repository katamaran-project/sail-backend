"""Tests for lib.scope, on the fixture dump and the fixture scope file."""

import unittest
from pathlib import Path

from lib import callgraph, dangling, extract, metrics, piles, scope

FIXTURES = Path(__file__).resolve().parent / 'fixtures'
RUN = FIXTURES / 'run'
DUMP = RUN / 'dump' / 'rw_rewrite_12_toplevel_let_patterns.sail'
CHERIOT_SAIL = FIXTURES / 'cheriot-sail'


def fixture_scope():
    scope_file = scope.ScopeFile.load(FIXTURES / 'scope.toml')
    sources = {name: (CHERIOT_SAIL / name).read_text() for name in scope_file.instruction_files}
    return scope.compute(callgraph.CallGraph.from_dump(DUMP), scope_file, sources)


class ScopeFileTests(unittest.TestCase):
    def test_load(self):
        scope_file = scope.ScopeFile.load(FIXTURES / 'scope.toml')
        self.assertEqual(scope_file.instruction_files, ['model/insts_base.sail'])
        self.assertEqual(scope_file.dispatchers, {'execute': 'execute_'})

    def test_the_real_scope_file_loads(self):
        scope_file = scope.ScopeFile.load(Path(__file__).resolve().parents[1] / 'scope.toml')
        self.assertIn('src/cheri_insts.sail', scope_file.instruction_files)
        self.assertIn('step', scope_file.roots)

    def test_constructors(self):
        source = 'union clause ast = ADDI : bits(12)\n  union clause ast=CSR\n// union clause other = X\n'
        self.assertEqual(scope.instruction_constructors(source), ['ADDI', 'CSR'])


class ComputeTests(unittest.TestCase):
    scope = fixture_scope()

    def test_instructions(self):
        self.assertEqual(self.scope.instructions, ['ADDI'])
        self.assertEqual(self.scope.dispatched, {'execute_ADDI'})

    def test_in_scope(self):
        # step and what it reaches (also through a value), the register and what its initialiser calls,
        # and the in-scope instruction and what it reaches
        self.assertEqual(self.scope.nodes,
                         {'step', 'fetch', 'helper', 'xlen_value', 'vlenb', 'vlen_init', 'execute_ADDI', 'bool_to_bits'})

    def test_the_dispatcher_is_in_scope_only_through_its_callers(self):
        # nothing in scope calls execute in the fixture, so it is excluded; its calls are not followed
        self.assertIn('execute', self.scope.excluded_functions)

    def test_excluded(self):
        self.assertEqual(self.scope.excluded_functions, {'execute', 'execute_FOO', 'vector_helper', 'unused',
                                                         'documented', 'undocumented_callee', 'after_split_comment'})
        self.assertEqual(self.scope.excluded_values, {'vlen_default'})

    def test_includes(self):
        self.assertTrue(self.scope.includes('regidx'))          # a type that fetch mentions
        self.assertTrue(self.scope.includes('helper'))
        self.assertFalse(self.scope.includes('vector_helper'))
        self.assertFalse(self.scope.includes('truncate'))       # an extern nothing in scope calls

    def test_warnings(self):
        self.assertEqual(self.scope.warnings, ['roots that are not definitions of the model: not_a_definition'])

    def test_warning_for_an_instruction_without_its_function(self):
        scope_file = scope.ScopeFile(['model/insts_vector.sail', 'model/insts_base.sail'], ['step'], {'execute': 'run_'})
        sources = {name: (CHERIOT_SAIL / name).read_text() for name in scope_file.instruction_files}
        result = scope.compute(callgraph.CallGraph.from_dump(DUMP), scope_file, sources)
        self.assertIn('no function run_FOO for instruction FOO', result.warnings)


class DispatcherInScopeTests(unittest.TestCase):
    """With execute in scope (a root calls it), only its calls for in-scope instructions are followed."""

    scope = scope.compute(callgraph.CallGraph.from_dump(DUMP),
                          scope.ScopeFile(['model/insts_base.sail'], ['step', 'execute'], {'execute': 'execute_'}),
                          {'model/insts_base.sail': (CHERIOT_SAIL / 'model/insts_base.sail').read_text()})

    def test_out_of_scope_instruction_stays_excluded(self):
        self.assertIn('execute', self.scope.nodes)
        self.assertEqual(self.scope.excluded_functions, {'execute_FOO', 'vector_helper', 'unused',
                                                         'documented', 'undocumented_callee', 'after_split_comment'})

    def test_mentioned_but_excluded_is_not_included(self):
        # execute mentions execute_FOO, but it is outside the scope
        self.assertIn('execute_FOO', self.scope.mentioned)
        self.assertFalse(self.scope.includes('execute_FOO'))

    def test_unexpected_calls(self):
        calls = [dangling.DanglingCall('execute_FOO', 1, 'outside the scope', '', ['execute']),
                 dangling.DanglingCall('vector_helper', 2, 'outside the scope', '', ['helper', 'step']),
                 dangling.DanglingCall('truncate', 1, 'extern', '', ['helper'])]
        self.assertEqual(scope.unexpected_calls(calls, self.scope),
                         ['helper, step call vector_helper, which is outside the scope'])


class ConfigurationTests(unittest.TestCase):
    def test_lists_the_excluded_names(self):
        text = scope.configuration(fixture_scope())
        self.assertIn(";; the 7 functions and 1 values outside the scope.", text)
        self.assertIn("(ignore-functions\n  '(\n   \"after_split_comment\"\n   \"documented\"\n   \"execute\"\n   \"execute_FOO\"\n   \"undocumented_callee\"\n   \"unused\"\n   \"vector_helper\"\n  ))", text)
        self.assertIn("(ignore-values\n  '(\n   \"vlen_default\"\n  ))", text)


class ScopedMetricsTests(unittest.TestCase):
    def test_counts_are_restricted_to_the_scope(self):
        items = extract.run_items(RUN, extract.NanosailSource(FIXTURES / 'source'))
        graph = callgraph.CallGraph.from_dump(DUMP)
        callgraph.mark_reachable(items, graph)
        piles.assign_piles(items)
        in_scope = fixture_scope()
        machine_v = (RUN / 'machine.v').read_text()
        calls = dangling.dangling_calls(machine_v, DUMP.read_text(), items, 'run', in_scope.excluded)
        result = metrics.compute(items, machine_v, graph, calls, None, in_scope)
        self.assertEqual(result['functions']['total'], 6)
        self.assertEqual(result['instructions'], {'total': 1, 'generated': 0})
        # bool_to_bits is in scope; count_down, file_start and the µSail definitions of machine.v are not
        self.assertEqual(result['definitions']['untranslated'], 1)
        self.assertEqual(result['items']['coqc-error'], 1)      # items without a definition stay
        self.assertEqual(result['scope'], {'instructions': 1, 'functions': 6, 'excluded_functions': 7, 'excluded_values': 1,
                                           'policy_excluded': {'no-op': 0, 'failure': 0, 'unconstrained': 0, 'registers': 0}})
        self.assertTrue(metrics.format_metrics(result)[0].startswith('scope: the CHERIoT core, 1 instructions and 6 functions'))



POLICY_SCOPE_FILE = """\
instruction-files = ["model/insts_base.sail"]
roots = ["step"]

[dispatchers]
execute = "execute_"

[policy-excluded]
no-op = ["helper"]
unconstrained = ["truncate"]
registers = ["vlenb"]
"""


def policy_scope(text=POLICY_SCOPE_FILE):
    scope_file = scope.ScopeFile.from_text(text)
    sources = {name: (CHERIOT_SAIL / name).read_text() for name in scope_file.instruction_files}
    return scope.compute(callgraph.CallGraph.from_dump(DUMP), scope_file, sources,
                         scope.declarations(DUMP.read_text()))


class PolicyExclusionTests(unittest.TestCase):
    """helper (a function step calls), truncate (an extern) and vlenb (a register) are policy-excluded."""

    scope = policy_scope()

    def test_load(self):
        self.assertEqual(scope.ScopeFile.from_text(POLICY_SCOPE_FILE).policy,
                         {'helper': 'no-op', 'truncate': 'unconstrained', 'vlenb': 'registers'})

    def test_no_table_means_no_exclusions(self):
        self.assertEqual(scope.ScopeFile.load(FIXTURES / 'scope.toml').policy, {})

    def test_unknown_key(self):
        with self.assertRaisesRegex(ValueError, 'unknown keys noop'):
            scope.ScopeFile.from_text(POLICY_SCOPE_FILE + 'noop = ["x"]\n')

    def test_two_policies_for_one_definition(self):
        with self.assertRaisesRegex(ValueError, 'helper under both no-op and failure'):
            scope.ScopeFile.from_text(POLICY_SCOPE_FILE + 'failure = ["helper"]\n')

    def test_edges_into_excluded_definitions_are_not_followed(self):
        # helper and the register vlenb are excluded; vlen_init, which only vlenb's initialiser calls, leaves the scope
        self.assertEqual(self.scope.nodes, {'step', 'fetch', 'xlen_value', 'execute_ADDI', 'bool_to_bits'})
        self.assertIn('vlen_init', self.scope.excluded_functions)

    def test_policy_exclusions_are_not_ignored(self):
        # they get their own configuration functions, not ignore-functions
        self.assertNotIn('helper', self.scope.excluded_functions)
        self.assertEqual(self.scope.policy, {'helper': 'no-op', 'truncate': 'unconstrained', 'vlenb': 'registers'})
        self.assertEqual(self.scope.policy_counts(), {'no-op': 1, 'failure': 0, 'unconstrained': 1, 'registers': 1})

    def test_excluded_definitions_are_not_in_scope(self):
        self.assertFalse(self.scope.includes('helper'))     # step mentions it
        self.assertFalse(self.scope.includes('truncate'))
        self.assertTrue(self.scope.includes('regidx'))

    def test_no_warnings(self):
        self.assertEqual(self.scope.warnings, [])

    def test_warnings(self):
        result = policy_scope(POLICY_SCOPE_FILE.replace('no-op = ["helper"]', 'no-op = ["helper", "not_there", "vlenb"]')
                              .replace('registers = ["vlenb"]', 'registers = ["fetch"]').replace('"step"]', '"step", "helper"]'))
        self.assertEqual(result.warnings, [
            'policy-excluded definitions that the model does not define: not_there',
            'policy-excluded definitions under the wrong key (registers go under registers, '
            'functions under the other keys): fetch, vlenb',
            'roots that are policy-excluded, and so left out: helper'])

    def test_configuration(self):
        text = scope.configuration(self.scope)
        self.assertIn("(exclude-functions-as-no-op\n  '(\n   \"helper\"\n  ))\n", text)
        self.assertIn("(exclude-functions-as-unconstrained\n  '(\n   \"truncate\"\n  ))\n", text)
        self.assertIn("(exclude-registers\n  '(\n   \"vlenb\"\n  ))\n", text)
        self.assertNotIn('exclude-functions-as-failure', text)
        self.assertNotIn('"helper"', text.split(';; The policy exclusions')[0])

    def test_a_remaining_call_to_an_excluded_function_is_a_problem(self):
        calls = [dangling.DanglingCall('helper', 2, 'outside the scope', '', ['step'])]
        self.assertEqual(scope.unexpected_calls(calls, self.scope),
                         ['step still calls helper, which is policy-excluded: the translation should have removed the calls'])

    def test_report(self):
        items = extract.run_items(RUN, extract.NanosailSource(FIXTURES / 'source'))
        graph = callgraph.CallGraph.from_dump(DUMP)
        callgraph.mark_reachable(items, graph)
        piles.assign_piles(items)
        machine_v = (RUN / 'machine.v').read_text()
        result = metrics.compute(items, machine_v, graph, [], None, self.scope)
        self.assertIn('policy exclusions (removed with their uses, not failures): 3: no-op 1, unconstrained 1, registers 1',
                      metrics.format_metrics(result))


if __name__ == '__main__':
    unittest.main()
