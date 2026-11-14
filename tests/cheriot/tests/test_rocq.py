"""Tests for lib.rocq. A fake coqc stands in for Rocq, so these tests need no Rocq installation."""

import shutil
import stat
import tempfile
import unittest
from pathlib import Path

from lib import rocq

FIXTURES = Path(__file__).resolve().parent / 'fixtures'
RUN = FIXTURES / 'run'

COQ_OUTPUT = '''File "./base.v", line {line}, characters 38-41:
Error: The reference NYI was not found in the current environment.
Did you mean NwI?

'''


def fake_coqc(directory: Path, script: str) -> str:
    """An executable that behaves like coqc as far as `script` (a shell script body) says."""
    path = directory / 'fake-coqc'
    path.write_text('#!/bin/sh\n' + script)
    path.chmod(path.stat().st_mode | stat.S_IXUSR)
    return str(path)


class ProbeSourceTests(unittest.TestCase):
    def test_prelude_goes_after_the_imports(self):
        text = (RUN / 'base.v').read_text()
        copy, at, inserted = rocq.probe_source(text)
        lines = copy.split('\n')
        self.assertEqual(lines[0], 'From Katamaran Require Import Base.')
        self.assertEqual(lines[at - 1:at - 1 + inserted], rocq.PROBE_PRELUDE)
        self.assertEqual(lines[at - 1 + inserted], '(*')
        self.assertEqual(len(lines), len(text.split('\n')) + inserted)

    def test_multi_line_imports(self):
        text = ('Require Export base.\n\nFrom Stdlib Require Import Lists.List\n'
                '                           ZArith.BinInt.\n\nImport ListNotations.\n\nDefinition x := 1.\n')
        lines = text.split('\n')
        self.assertEqual(rocq.import_end(lines), 7)
        copy, at, _ = rocq.probe_source(text)
        self.assertEqual(at, 8)
        self.assertTrue(copy.split('\n')[-2].startswith('Definition x'))

    def test_the_untranslated_definitions_marker_and_what_follows_are_left_out(self):
        text = ('From Katamaran Require Import Base.\n\nDefinition a := 1.\n\nTheorem UNTRANSLATED_DEFINITIONS : False. Qed.\n'
                '\n(*\n  val f = "unbalanced\n*)\n')
        copy, _, _ = rocq.probe_source(text)
        self.assertNotIn('UNTRANSLATED_DEFINITIONS', copy)
        self.assertNotIn('unbalanced', copy)
        self.assertIn('Definition a := 1.', copy)
        self.assertEqual(len(copy.split('\n')), len(text.split('\n')) + len(rocq.PROBE_PRELUDE))
        self.assertTrue(rocq.needs_probe(text))
        self.assertFalse(rocq.needs_probe('Definition a := 1.\n'))

    def test_holes_are_one_argument_with_a_list_of_numbers(self):
        # NYI[1] as an argument would be two arguments, and [1] may be read as a bitvector literal
        text = 'From Katamaran Require Import Base.\n\nDefinition a := f NYI[1] NYI[23] (g NYI[4]).\n'
        copy, _, _ = rocq.probe_source(text)
        self.assertIn('Definition a := f (NYI (cons 1%nat nil)) (NYI (cons 23%nat nil)) (g (NYI (cons 4%nat nil))).', copy)
        self.assertEqual(len(copy.split('\n')), len(text.split('\n')) + len(rocq.PROBE_PRELUDE))

    def test_function_bodies_with_holes_become_the_axiom(self):
        text = '\n'.join([
            'From Katamaran Require Import Base.',
            '',
            '    Definition fun_holes {kid__m : nat} : Stm [',
            '                                "bv"  ∷  ty.bvec (kid__m)',
            '                              ] (ty.bvec (kid__m)) :=',
            '      stm_let "x" (ty.bvec (NYI[0]))',
            '              (stm_exp (exp_var "bv")).',
            '',
            '    Definition fun_clean : Stm [] (ty.int) :=',
            '      stm_exp (exp_int 1).',
            '',
            '    Definition fun_calls_missing : Stm [] (ty.int) :=',
            '      stm_call missing env.nil.',
            '',
            '    Definition fun_calls_clean : Stm [] (ty.int) :=',
            '      stm_call clean env.nil.',
            '',
        ])
        copy, at, inserted = rocq.probe_source(text)
        lines = copy.split('\n')[at - 1 + inserted:]
        self.assertEqual(lines[3], '      NYI (cons 0%nat nil). (* tests/cheriot: the body has holes or calls a missing function *)')
        self.assertEqual(lines[4], '')
        self.assertEqual(lines[7], '      stm_exp (exp_int 1).')
        self.assertEqual(lines[10], '      NYI (cons 0%nat nil). (* tests/cheriot: the body has holes or calls a missing function *)')
        self.assertEqual(lines[13], '      stm_call clean env.nil.')
        self.assertEqual(len(copy.split('\n')), len(text.split('\n')) + inserted)

    def test_command_definition(self):
        starts = rocq.definition_starts('Definition a := 1.\nDerive EqDec for T.\n')
        self.assertEqual(rocq.command_definition(starts, 2, 'Derive EqDec for T.'), 'T')
        self.assertEqual(rocq.command_definition(starts, 2, '#[export,program] Instance T_finite : Finite T :='), 'T_finite')
        self.assertEqual(rocq.command_definition(starts, 2, 'Program Instance 𝑹𝑬𝑮_finite : Finite (sigT Reg) :='), '𝑹𝑬𝑮_finite')
        self.assertEqual(rocq.command_definition(starts, 2, 'Proof.'), 'a')

    def test_original_line(self):
        # prelude of 5 lines inserted at line 3: probe lines 3..7 are the prelude, 8 is the old line 3
        self.assertEqual(rocq.original_line(2, 3, 5), 2)
        self.assertEqual(rocq.original_line(8, 3, 5), 3)
        self.assertEqual(rocq.original_line(5, 3, 5), 3)


class ErrorTests(unittest.TestCase):
    def test_first_error(self):
        self.assertEqual(rocq.first_error('noise\n' + COQ_OUTPUT.format(line=12)),
                         ('base.v', 12, 'The reference NYI was not found in the current environment. Did you mean NwI?'))
        self.assertIsNone(rocq.first_error('Warning: something\n'))

    def test_locate_finds_the_containing_definition(self):
        text = (RUN / 'machine.v').read_text()
        line = next(n for n, l in enumerate(text.splitlines(), start=1) if 'NYI[0]' in l)
        error = rocq.locate(text, line, 'm')
        self.assertEqual((error.kind, error.definition, error.before, error.total), ('Definition', 'sail_mask', 1, 3))

    def test_locate_before_any_definition(self):
        error = rocq.locate((RUN / 'machine.v').read_text(), 1, 'm')
        self.assertEqual((error.definition, error.before, error.total), ('', 0, 3))


def timing_line(start: int, end: int, seconds: float) -> str:
    return f'Chars {start} - {end} [command] {seconds} secs (0.u,0.s)'


class WatchTests(unittest.TestCase):
    def setUp(self):
        self.directory = Path(tempfile.mkdtemp()).resolve()
        self.addCleanup(shutil.rmtree, self.directory)

    def test_command_at_counts_bytes(self):
        source = 'Definition 𝑹 := 1.\nDefinition b := 2.\n'.encode()
        offset = source.index(b'Definition b')
        self.assertEqual(rocq.command_at(source, offset), (2, 'Definition b := 2.'))
        self.assertEqual(rocq.command_at(source, len('Definition 𝑹 := 1.'.encode())), (2, 'Definition b := 2.'))

    def test_a_command_that_runs_too_long_stops_coqc(self):
        source = 'Definition a := 1.\nDefinition b := 2.\n'
        (self.directory / 'base.v').write_text(source)
        coqc = fake_coqc(self.directory, f"echo '{timing_line(0, 18, 0.5)}'\nsleep 10\n")
        seen = []
        watched = rocq.watch(coqc, self.directory, 'base.v', 0.5, lambda *s: seen.append(s), every=0.2)
        self.assertEqual(watched.exit, -1)
        offset, seconds = watched.stuck
        self.assertEqual(rocq.command_at(source.encode(), offset), (2, 'Definition b := 2.'))
        self.assertGreaterEqual(seconds, 0.5)
        self.assertEqual(watched.timings, [(0.5, 0)])
        self.assertIn((1, 18), [s[:2] for s in seen])

    def test_timing_lines_are_not_in_the_log(self):
        self.assertEqual(rocq.without_timings(timing_line(0, 3, 0.1) + '\nWarning: x\n'), 'Warning: x\n')


class CompileTests(unittest.TestCase):
    def setUp(self):
        self.directory = Path(tempfile.mkdtemp()).resolve()
        self.addCleanup(shutil.rmtree, self.directory)
        shutil.copy(RUN / 'base.v', self.directory / 'base.v')
        shutil.copy(RUN / 'machine.v', self.directory / 'machine.v')
        self.calls = self.directory / 'calls.txt'

    def coqc(self, script: str = '') -> str:
        """A fake coqc called as `coqc -time FILE` that logs its calls, runs `script`, and makes FILE's .vo when it exits 0."""
        return fake_coqc(self.directory, f'echo "$PWD $2" >> {self.calls}\n{script}\n'
                                         f'touch "$(basename "$2" .v).vo"\n')

    def called(self) -> list[str]:
        return self.calls.read_text().splitlines() if self.calls.exists() else []

    def test_success_compiles_both_files_and_a_file_without_nyi_once(self):
        results = rocq.compile_both(self.coqc(), self.directory, self.directory / 'probe')
        self.assertEqual([(r.file, r.ok, r.reused) for r in results['plain']], [('base.v', True, False), ('machine.v', True, False)])
        self.assertEqual([(r.file, r.ok, r.reused) for r in results['probe']], [('base.v', True, True), ('machine.v', True, False)])
        # base.v has no NYI: compiled once; machine.v has NYI: plain and probe
        self.assertEqual(sorted(self.called()), sorted([f'{self.directory} base.v', f'{self.directory} machine.v',
                                                        f'{self.directory / "probe"} machine.v']))
        self.assertTrue((self.directory / 'probe' / 'base.vo').exists())
        self.assertEqual(rocq.describe([r.as_json() for r in results['plain']]), 'base.v, machine.v compile')

    def test_failure_stops_before_machine_and_is_located(self):
        coqc = self.coqc(f"cat <<'EOF'\n{COQ_OUTPUT.format(line=17)}EOF\nexit 1\n")
        results = rocq.compile_both(coqc, self.directory, self.directory / 'probe')
        self.assertEqual(len(results['plain']), 1)
        error = results['plain'][0].error
        self.assertEqual((error.line, error.kind, error.definition), (17, '', ''))
        self.assertTrue((self.directory / 'coqc-base.log').read_text().startswith('File "./base.v", line 17'))
        self.assertEqual(len(results['probe']), 1)
        self.assertTrue(results['probe'][0].reused)

    def test_probe_errors_are_mapped_to_lines_of_the_original(self):
        original = (self.directory / 'base.v').read_text() + '\nDefinition hole : nat := NYI [0].\n'
        (self.directory / 'base.v').write_text(original)
        _, at, inserted = rocq.probe_source(original)
        count_down = next(n for n, l in enumerate(original.splitlines(), start=1) if 'count_down' in l)
        # the fake coqc reports the probe copy's line of count_down, in both copies
        coqc = self.coqc(f"cat <<'EOF'\n{COQ_OUTPUT.format(line=count_down + inserted)}EOF\nexit 1\n")
        results = rocq.compile_both(coqc, self.directory, self.directory / 'probe')
        [result] = results['probe']
        self.assertFalse(result.reused)
        self.assertEqual(result.error.line, count_down)
        self.assertIn('Axiom NYI', (self.directory / 'probe' / 'base.v').read_text())
        self.assertEqual(len(self.called()), 2)

    def test_stuck(self):
        coqc = self.coqc(f"echo '{timing_line(0, 30, 0.25)}'\nsleep 10\n")
        [result] = rocq.compile_both(coqc, self.directory, self.directory / 'probe', stall=0.5)['plain']
        self.assertEqual(result.exit, -1)
        self.assertFalse(result.ok)
        self.assertIsNotNone(result.stuck)
        self.assertEqual(result.stuck.line, result.error.line)
        self.assertEqual(result.slowest[0].seconds, 0.25)
        self.assertIn('STUCK', rocq.describe([result.as_json()]))
        self.assertTrue(rocq.is_stuck([result.as_json()]))

    def test_slowest_commands(self):
        (self.directory / 'base.v').write_text('From Katamaran Require Import Base.\n\nDefinition a := 1.\n')
        source = (self.directory / 'base.v').read_bytes()
        definition = source.index(b'Definition')
        line = source[:definition].count(b'\n') + 1
        coqc = self.coqc(f"echo '{timing_line(0, 10, 0.1)}'\necho '{timing_line(definition, definition + 5, 2.5)}'\n")
        [base, _] = rocq.compile_both(coqc, self.directory, self.directory / 'probe')['plain']
        self.assertEqual([(t.seconds, t.line) for t in base.slowest], [(2.5, line), (0.1, 1)])
        self.assertEqual((base.slowest[0].command, base.slowest[0].definition), ('Definition a := 1.', 'a'))
        self.assertEqual(rocq.slowest([base.as_json()], 1)[0]['seconds'], 2.5)

    def test_cache(self):
        cache = rocq.Cache(self.directory / 'cache', 'fingerprint')
        rocq.compile_both(self.coqc(), self.directory, self.directory / 'probe', cache=cache)
        self.assertEqual(len(self.called()), 3)
        (self.directory / 'base.vo').unlink()
        results = rocq.compile_both(self.coqc(), self.directory, self.directory / 'probe', cache=cache)
        self.assertEqual(len(self.called()), 3)                       # nothing compiled again
        self.assertTrue(all(r.cached for r in results['plain'] + results['probe']))
        self.assertTrue((self.directory / 'base.vo').exists())        # restored from the cache
        # another fingerprint, or a changed file, misses
        (self.directory / 'machine.v').write_text((self.directory / 'machine.v').read_text() + '\n(* changed *)\n')
        rocq.compile_both(self.coqc(), self.directory, self.directory / 'probe', cache=cache)
        self.assertEqual(len(self.called()), 5)                       # machine.v, plain and probe

    def test_stuck_compilations_are_not_cached(self):
        cache = rocq.Cache(self.directory / 'cache', 'fingerprint')
        rocq.compile_both(self.coqc('sleep 10'), self.directory, self.directory / 'probe', stall=0.3, cache=cache)
        self.assertEqual(list((self.directory / 'cache').glob('*.json')) if (self.directory / 'cache').exists() else [], [])

    def test_failure_without_a_location(self):
        coqc = self.coqc('echo "Error: Unable to locate library Katamaran.Base." >&2\nexit 1\n')
        [result] = rocq.compile_both(coqc, self.directory, self.directory / 'probe')['plain']
        self.assertEqual(result.error.message, 'Error: Unable to locate library Katamaran.Base.')
        self.assertEqual(result.error.line, 0)

    def test_json_round_trip(self):
        compilation = rocq.Compilation('base.v', -1, 3.0, rocq.CoqError(4, 'm'), rocq.Stuck(4, 'c', 1.0, 0.5, 'd'),
                                       [rocq.Timing(1.0, 4, 'c', 'd')])
        self.assertEqual(rocq.Compilation.from_json(compilation.as_json()), compilation)


if __name__ == '__main__':
    unittest.main()
