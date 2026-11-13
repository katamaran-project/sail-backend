"""Tests for lib.extract, on the small run in fixtures/run (excerpts in the format of a real CHERIoT run)."""

import tempfile
import unittest
from collections import Counter
from pathlib import Path

from lib import extract

FIXTURES = Path(__file__).resolve().parent / 'fixtures'
RUN = FIXTURES / 'run'
# A stand-in for the nanosail source: constructs are read from it by line number.
SOURCE = extract.NanosailSource(FIXTURES / 'source')


def only(items, **fields):
    """The one item whose fields have the given values."""
    matching = [r for r in items if all(r[k] == v for k, v in fields.items())]
    assert len(matching) == 1, f'{len(matching)} items match {fields}: {matching}'
    return matching[0]


class NanosailSourceTests(unittest.TestCase):
    def test_construct_is_the_match_arm_at_the_location(self):
        self.assertEqual(SOURCE.construct_at('nanosail/SailToNanosail/Translate/Function.ml line 3'),
                         'Function.ml: AE_for')

    def test_no_construct_for_a_line_that_is_not_a_match_arm(self):
        self.assertEqual(SOURCE.construct_at('nanosail/SailToNanosail/Translate/Function.ml line 1'), '')

    def test_no_construct_for_a_line_beyond_the_file_or_a_missing_file(self):
        self.assertEqual(SOURCE.construct_at('nanosail/SailToNanosail/Translate/Function.ml line 999'), '')
        self.assertEqual(SOURCE.construct_at('nanosail/Missing.ml line 1'), '')

    def test_a_root_without_nanosail_is_rejected(self):
        # A wrong root used to leave every construct empty, and so most items outside their pile.
        with self.assertRaises(ValueError):
            extract.NanosailSource(FIXTURES / 'run')

    def test_the_default_root_is_this_checkout(self):
        self.assertTrue((extract.NanosailSource().root / 'nanosail' / 'SailToNanosail').is_dir())


class SailKindAndNameTests(unittest.TestCase):
    def test_attributes_and_comments_are_skipped(self):
        self.assertEqual(extract.sail_kind_and_name(['$[complete]', '// comment', 'function f x = x']), ('function', 'f'))
        self.assertEqual(extract.sail_kind_and_name(['/* note */ val g : int -> int']), ('val', 'g'))

    def test_attribute_followed_by_definition_on_the_same_line(self):
        self.assertEqual(extract.sail_kind_and_name(['$[sv_module] function h() = ()']), ('function', 'h'))

    def test_kinds(self):
        cases = {
            'function clause execute ADDI(imm) = ()': ('function-clause', 'execute'),
            'scattered union ast': ('scattered-union', 'ast'),
            'union clause ast = ADDI : bits(12)': ('union-clause', 'ast'),
            'register PC : bits(32)': ('register', 'PC'),
            'let (null_cap) = 0': ('let', 'null_cap'),
            '$file_start model.sail': ('pragma', 'file_start'),
            'constraint 1 < 2': ('constraint', ''),
        }
        for line, expected in cases.items():
            with self.subTest(line=line):
                self.assertEqual(extract.sail_kind_and_name([line]), expected)


class UntranslatedTests(unittest.TestCase):
    items = extract.extract_untranslated(SOURCE, 'run', RUN / 'base.v')

    def test_one_item_per_block(self):
        self.assertEqual([r['name'] for r in self.items], ['bool_to_bits', 'count_down', 'file_start'])
        self.assertEqual({r['stage'] for r in self.items}, {'untranslated'})

    def test_fields(self):
        record = only(self.items, name='count_down')
        self.assertEqual(record['kind'], 'function')
        self.assertEqual(record['ocaml_loc'], 'nanosail/SailToNanosail/Translate/Function.ml line 3')
        self.assertEqual(record['sail_loc'], 'File "model.sail" line 20 chars 1-60')
        self.assertEqual(record['message'], 'No message')
        self.assertEqual(record['where'], 'base.v:16')
        self.assertTrue(record['snippet'].startswith('function count_down n = {'))

    def test_construct_from_the_source(self):
        self.assertEqual(only(self.items, name='count_down')['construct'], 'Function.ml: AE_for')
        self.assertEqual(only(self.items, name='file_start')['construct'], 'Translation.ml: DEF_pragma')

    def test_construct_guessed_from_the_message_when_the_source_has_no_arm(self):
        self.assertEqual(only(self.items, name='bool_to_bits')['construct'], 'vector literal of bitvectors')


class NyiTests(unittest.TestCase):
    items = extract.extract_nyi(SOURCE, 'run', RUN / 'machine.v')

    def test_one_item_per_hole(self):
        self.assertEqual([(r['name'], r['where']) for r in self.items],
                         [('sail_mask', 'machine.v:15'), ('two_holes', 'machine.v:22'), ('two_holes', 'machine.v:26')])
        self.assertEqual({r['kind'] for r in self.items}, {'µSail Definition'})

    def test_wrapped_message_is_joined_and_closing_parenthesis_dropped(self):
        message = only(self.items, name='sail_mask')['message']
        self.assertTrue(message.startswith('only calls to sail_zero_extend supported'))
        self.assertTrue(message.endswith('Type:Int[NumExpr:Var[Identifier["kid__len"]]]] instead'))

    def test_holes_take_the_annotations_of_their_frame_in_order(self):
        # The code numbers holes globally (NYI[1], NYI[2]); the comment numbers them per frame ([0], [1]).
        holes = [r for r in self.items if r['name'] == 'two_holes']
        self.assertEqual([(r['ocaml_loc'], r['message']) for r in holes],
                         [('nanosail/NanosailToMicrosail/Statements.ml line 7', 'first hole'),
                          ('nanosail/NanosailToMicrosail/Statements.ml line 9', 'second hole')])


class LogTests(unittest.TestCase):
    items = extract.extract_log(SOURCE, 'run', RUN / 'log.txt')

    def test_stages(self):
        self.assertEqual(Counter(r['stage'] for r in self.items), Counter(error=1, skipped=1, warning=2))

    def test_a_recovered_failure_belongs_to_the_definition_being_processed(self):
        record = only(self.items, stage='error')
        self.assertEqual((record['kind'], record['name']), ('function', 'bool_to_bits'))
        self.assertEqual(record['ocaml_loc'], 'nanosail/SailToNanosail/Translate/Function.ml line 394')
        self.assertEqual(record['message'], 'Elements of bit vector should all be bits; they were Type:Bitvector[1]')

    def test_skipped_definition(self):
        record = only(self.items, stage='skipped')
        self.assertEqual((record['kind'], record['name']), ('val', 'undefined_bool'))
        self.assertEqual(record['construct'], 'ignored by configuration')

    def test_warnings_during_translation_belong_to_the_sail_definition(self):
        record = only(self.items, stage='warning', name='__id')
        self.assertEqual(record['kind'], 'val')
        self.assertIn('Encountered polymorphic top level type constraint', record['message'])

    def test_warnings_during_generation_belong_to_the_generated_function(self):
        record = only(self.items, stage='warning', message='Ignored cast')
        self.assertEqual((record['kind'], record['name']), ('µSail function', 'sail_mask'))

    def test_ansi_colours_are_ignored(self):
        coloured = (RUN / 'log.txt').read_text()
        for level, colour in (('DEBUG', '0'), ('INFO', '32'), ('WARNING', '31'), ('ERROR', '41;97')):
            coloured = coloured.replace(f'[{level}]', f'[\x1b[{colour}m{level}\x1b[0m]')
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / 'log.txt'
            path.write_text(coloured)
            items = extract.extract_log(SOURCE, 'run', path)
        self.assertEqual([{k: v for k, v in r.items() if k != 'where'} for r in items],
                         [{k: v for k, v in r.items() if k != 'where'} for r in self.items])


class HardFailureTests(unittest.TestCase):
    def test_from_the_log(self):
        [record] = extract.extract_hard_failures_from_log(SOURCE, 'run', RUN / 'log.txt')
        self.assertEqual((record['stage'], record['name']), ('hard-failure', 'getCapBoundsBits'))
        self.assertEqual(record['ocaml_loc'], 'nanosail/NanosailToMicrosail/FunctionCalls.ml line 358')
        self.assertEqual(record['sail_loc'], 'model.sail:418')
        self.assertTrue(record['message'].startswith('only calls to add_bits_int supported'))
        self.assertTrue(record['message'].endswith('[in call to add_bits_int | in body of function getCapBoundsBits]'))
        self.assertEqual(record['where'], 'log.txt:28')

    def test_from_the_inventory_hard_failures_log(self):
        items = extract.extract_hard_failures(SOURCE, 'run', FIXTURES / 'hard-failures.log')
        self.assertEqual([r['name'] for r in items], ['getCapBoundsBits', '__WriteRAM_Meta'])
        self.assertEqual(items[1]['sail_loc'], 'model.sail:87')


class CoqcTests(unittest.TestCase):
    def test_first_error(self):
        [record] = extract.extract_coqc(SOURCE, 'run', RUN / 'coqc-base.log')
        self.assertEqual(record['where'], 'base.v:598')
        self.assertTrue(record['message'].startswith('In environment Access_kind : NYI [0] -> Set'))


class RunItemsTests(unittest.TestCase):
    def test_the_whole_run(self):
        items = extract.run_items(RUN, SOURCE)
        self.assertEqual({r['run'] for r in items}, {'run'})
        self.assertEqual(Counter(r['stage'] for r in items),
                         Counter(untranslated=3, nyi=3, error=1, skipped=1, warning=2, **{'hard-failure': 1, 'coqc-error': 1}))


if __name__ == '__main__':
    unittest.main()
