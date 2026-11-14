"""Tests for lib.metrics and for the helpers of run.py that do not need Sail."""

import tempfile
import unittest
from pathlib import Path

import run
from lib import callgraph, dangling, extract, metrics, piles

FIXTURES = Path(__file__).resolve().parent / 'fixtures'
RUN = FIXTURES / 'run'
DUMP = RUN / 'dump' / 'rw_rewrite_12_toplevel_let_patterns.sail'


class MetricsTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        items = extract.run_items(RUN, extract.NanosailSource(FIXTURES / 'source'))
        graph = callgraph.CallGraph.from_dump(DUMP)
        callgraph.mark_reachable(items, graph)
        piles.assign_piles(items)
        machine_v = (RUN / 'machine.v').read_text()
        calls = dangling.dangling_calls(machine_v, DUMP.read_text(), items, 'run')
        cls.metrics = metrics.compute(items, machine_v, graph, calls)

    def test_counts(self):
        m = self.metrics
        self.assertEqual(m['items']['untranslated'], 3)
        self.assertEqual(m['definitions']['untranslated'], 3)
        self.assertEqual(m['untranslated_by_kind'], {'function': 2, 'pragma': 1})
        self.assertEqual(m['nyi'], {'holes': 3, 'definitions': 2})
        self.assertEqual(m['hard_failures'], ['getCapBoundsBits'])

    def test_functions_and_instructions(self):
        # machine.v generates neq_bits, sail_mask and two_holes, which are not functions of the fixture model;
        # its instructions are ADDI and FOO
        self.assertEqual(self.metrics['functions'], {'total': 13, 'generated': 0})
        self.assertEqual(self.metrics['instructions'], {'total': 2, 'generated': 0})

    def test_failing_reachable_counts_definitions_not_items(self):
        # bool_to_bits is both untranslated and an error, and reachable from execute
        self.assertEqual(self.metrics['failing_reachable_definitions'], 1)

    def test_missing_calls_and_piles(self):
        self.assertEqual(self.metrics['calls_missing']['untranslated'], {'callees': 1, 'calls': 2})
        p04 = next(v for k, v in self.metrics['piles'].items() if k.startswith('P04'))
        self.assertEqual(p04, {'items': 2, 'definitions': 1, 'reachable': 1})
        self.assertEqual(self.metrics['unassigned_failures'], 2)

    def test_format(self):
        lines = metrics.format_metrics(self.metrics)
        self.assertEqual(lines[0], 'scope: the whole model')
        self.assertEqual(lines[1].split(), ['now', 'previous', 'baseline'])
        self.assertEqual(lines[2].split(), ['instructions', 'generated', '0', 'of', '2', '-', '-'])
        text = '\n'.join(lines)
        self.assertIn('hard failures: getCapBoundsBits', text)
        self.assertIn('missing calls by class: untranslated 2, extern 1, skipped by configuration 1, unknown 1', text)
        # without a scope there is no clean-instruction count, so no row for it
        self.assertNotIn('instructions clean', text)

    def test_format_with_previous_baseline_and_blockers(self):
        result = {**self.metrics, 'instructions': {**self.metrics['instructions'], 'clean': 1},
                  'blockers': [('zeros_implicit (missing)', 58), ('null_cap (missing)', 51)]}
        previous = {'instructions clean': '0', 'NYI holes': '5'}
        baseline = {'instructions clean': 0, 'NYI holes': 7}
        lines = metrics.format_metrics(result, previous, baseline)
        self.assertEqual(lines[2].split(), ['instructions', 'clean', '1', 'of', '2', '0', '0'])
        self.assertIn(['NYI', 'holes', '3', '5', '7'], [line.split() for line in lines])
        self.assertEqual(lines[-2:], ['   58  zeros_implicit (missing)', '   51  null_cap (missing)'])

    def test_headline(self):
        self.assertEqual(metrics.headline(self.metrics),
                         {'instructions clean': None, 'instructions generated': 0, 'functions generated': 0,
                          'untranslated definitions': 3, 'NYI holes': 3, 'hard failures': 1, 'missing calls': 5})

    def test_rocq_lines(self):
        error = {'line': 593, 'message': 'The reference NYI was not found', 'definition': 'Access_kind',
                 'kind': 'Inductive', 'before': 24, 'total': 555}
        compilations = {'plain': [{'file': 'base.v', 'ok': False, 'exit': 1, 'seconds': 0.5, 'error': error}],
                        'probe': [{'file': 'base.v', 'ok': True, 'exit': 0, 'seconds': 1.0, 'error': None},
                                  {'file': 'machine.v', 'ok': True, 'exit': 0, 'seconds': 9.0, 'error': None}]}
        lines = metrics.format_metrics({**self.metrics, 'rocq': compilations})
        self.assertEqual(lines[-2], 'rocq plain  base.v:593 in Inductive Access_kind '
                                    '(24 of 555 definitions before it): The reference NYI was not found')
        self.assertEqual(lines[-1], 'rocq probe  base.v, machine.v compile')
        self.assertFalse(any(l.startswith('rocq') for l in metrics.format_metrics(self.metrics)))

class ClassifyTranslationTests(unittest.TestCase):
    def test_success(self):
        self.assertEqual(run.classify_translation(0, 'Done with translation\n', True), (True, ''))

    def test_hard_failure_names_the_function(self):
        log = (RUN / 'log.txt').read_text()
        ok, summary = run.classify_translation(1, log, False)
        self.assertFalse(ok)
        self.assertTrue(summary.startswith('hard failure in getCapBoundsBits: only calls to add_bits_int'))

    def test_sail_crash(self):
        log = 'Internal error: Unreachable code (in ../type_error.ml:577)\n'
        self.assertEqual(run.classify_translation(1, log, False), (False, 'Sail crashed (exit 1)'))

    def test_uncaught_exception(self):
        log = ('Fatal error: exception Nanosail__Slang__Exception.SlangError("unbound identifier foo")\n'
               'Raised at Nanosail__Slang__Evaluation.evaluate in file "nanosail/Slang/Evaluation.ml", line 51\n')
        self.assertEqual(run.classify_translation(2, log, False),
                         (False, 'uncaught exception: Nanosail__Slang__Exception.SlangError("unbound identifier foo")'))
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / 'log.txt'
            path.write_text('[INFO] x.ml:1\n   info\n' + log)
            self.assertEqual(run.first_error(path)[1], log.splitlines()[0])

    def test_no_output(self):
        self.assertEqual(run.classify_translation(0, '', False), (False, 'Sail succeeded but wrote no base.v or machine.v'))

    def test_first_error(self):
        with tempfile.TemporaryDirectory() as directory:
            log = Path(directory) / 'log.txt'
            log.write_text('warming up\n\x1b[91mType error\x1b[0m:\nmodel.sail:3.4-5:\nmismatch\n')
            self.assertEqual(run.first_error(log)[:3], ['warming up', 'Type error:', 'model.sail:3.4-5:'])


class PinTests(unittest.TestCase):
    def warnings(self, pins, commit):
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / 'inputs.toml'
            path.write_text(pins)
            return run.pin_warnings({'cheriot-sail': {'commit': commit}}, path)

    def test_matching_commit_of_another_length(self):
        self.assertEqual(self.warnings('[cheriot-sail]\ncommit = "518813e"\n', '518813ea'), [])

    def test_other_commit(self):
        self.assertEqual(self.warnings('[cheriot-sail]\ncommit = "518813e"\n', '1234567'),
                         ['cheriot-sail is at 1234567, but inputs.toml pins 518813e'])

    def test_no_pins_file(self):
        self.assertEqual(run.pin_warnings({}, FIXTURES / 'missing.toml'), [])


class SailSourcesTests(unittest.TestCase):
    def setUp(self):
        try:
            self.make = run.gnu_make()
        except run.InputError:
            self.skipTest('GNU make 4 is not available')

    def test_sources_come_from_the_cheriot_sail_makefile(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            cheriot_sail, case_study = root / 'cheriot-sail', root / 'case-study'
            (cheriot_sail / 'src').mkdir(parents=True)
            (cheriot_sail / 'sail-riscv' / 'model').mkdir(parents=True)
            case_study.mkdir()
            (case_study / 'printvar.mak').write_text("print-%:\n\t@echo '$($*)'\n")
            (cheriot_sail / 'Makefile').write_text('SAIL_SRCS = src/a.sail src/b.sail\nall:\n\t@echo all\n')
            for name in ('src/a.sail', 'src/b.sail', 'sail-riscv/model/main.sail'):
                (cheriot_sail / name).write_text('')
            self.assertEqual(run.sail_sources(cheriot_sail, case_study, self.make),
                             [cheriot_sail / 'src/a.sail', cheriot_sail / 'src/b.sail',
                              cheriot_sail / 'sail-riscv/model/main.sail'])
            (cheriot_sail / 'src/b.sail').unlink()
            with self.assertRaises(run.InputError):
                run.sail_sources(cheriot_sail, case_study, self.make)


if __name__ == '__main__':
    unittest.main()
