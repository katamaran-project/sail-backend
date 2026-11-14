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
    """An executable that behaves like coqc as far as `script` (a shell script body) says; $1 is the file."""
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


class CompileTests(unittest.TestCase):
    def setUp(self):
        self.directory = Path(tempfile.mkdtemp())
        self.addCleanup(shutil.rmtree, self.directory)
        shutil.copy(RUN / 'base.v', self.directory / 'base.v')
        shutil.copy(RUN / 'machine.v', self.directory / 'machine.v')

    def test_success_compiles_both_files(self):
        coqc = fake_coqc(self.directory, 'echo compiled "$1" >> calls.txt\n')
        results = rocq.compile_all(coqc, self.directory, 10)
        self.assertEqual([(r.file, r.ok) for r in results], [('base.v', True), ('machine.v', True)])
        self.assertEqual(rocq.describe([r.as_json() for r in results]), 'base.v, machine.v compile')

    def test_failure_stops_before_machine_and_is_located(self):
        coqc = fake_coqc(self.directory, f"cat <<'EOF'\n{COQ_OUTPUT.format(line=17)}EOF\nexit 1\n")
        results = rocq.compile_all(coqc, self.directory, 10)
        self.assertEqual(len(results), 1)
        error = results[0].error
        self.assertEqual((error.line, error.kind, error.definition), (17, '', ''))
        self.assertTrue((self.directory / 'coqc-base.log').read_text().startswith('File "./base.v", line 17'))

    def test_probe_errors_are_mapped_to_lines_of_the_original(self):
        original = (self.directory / 'base.v').read_text()
        _, at, inserted = rocq.probe_source(original)
        count_down = next(n for n, l in enumerate(original.splitlines(), start=1) if 'count_down' in l)
        # the fake coqc reports the probe copy's line of count_down
        coqc = fake_coqc(self.directory, f"cat <<'EOF'\n{COQ_OUTPUT.format(line=count_down + inserted)}EOF\nexit 1\n")
        [result] = rocq.probe_all(coqc, self.directory, self.directory / 'probe', 10)
        self.assertEqual(result.error.line, count_down)
        self.assertIn('Axiom NYI', (self.directory / 'probe' / 'base.v').read_text())

    def test_timeout(self):
        coqc = fake_coqc(self.directory, 'sleep 5\n')
        [result] = rocq.compile_all(coqc, self.directory, 0.2)
        self.assertEqual(result.exit, -1)
        self.assertFalse(result.ok)
        self.assertEqual(rocq.describe([result.as_json()]), 'base.v: timed out after 0 s')

    def test_failure_without_a_location(self):
        coqc = fake_coqc(self.directory, 'echo "Error: Unable to locate library Katamaran.Base." >&2\nexit 1\n')
        [result] = rocq.compile_all(coqc, self.directory, 10)
        self.assertEqual(result.error.message, 'Error: Unable to locate library Katamaran.Base.')
        self.assertEqual(result.error.line, 0)


if __name__ == '__main__':
    unittest.main()
