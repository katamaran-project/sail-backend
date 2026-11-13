#!/usr/bin/env python3
"""
Extract every translation-failure signal from one sail-backend run on CHERIoT.

Usage:
    extract.py <run-dir> [<run-dir> ...] --out items.csv [--json items.json]

A <run-dir> is a directory in which `sail --katamaran` ran: it holds base.v,
machine.v, and the log. The log is found as <run-dir>/log.txt or <run-dir>.log
(the layout of `make -C <katamaran>/case_study/Cheriot ... OUT_DIR=<run-dir> > <run-dir>.log`).
Optional extras in <run-dir>:
    coqc-base.log, coqc-machine.log   coqc output (first error is recorded)
    hard-failures.log                 output of workarounds/hard-failure-loop.sh

Each extracted item has these fields:
    run          the run directory's name
    stage        untranslated | nyi | warning | error | skipped | hard-failure | coqc-error
    kind         the kind of the Sail definition (function, val, type, ...), when known
    name         the Sail definition (or µSail function) it belongs to, when known
    sail_loc     Sail source location, when known
    ocaml_loc    nanosail source location that raised it ("file line N")
    message      the message
    construct    the Sail construct involved, when it can be determined (heuristic)
    where        where it was found (file:line of the .v file or the log)
    snippet      first line(s) of the Sail code or µSail code involved

Only the standard library is used, so the inventory can be regenerated with any python3.
"""

import argparse
import csv
import json
import re
import sys
from pathlib import Path

ANSI = re.compile(r'\x1b\[[0-9;]*m')

# The kind and name of a Sail definition, from its pretty-printed source.
SAIL_DEFINITION_HEADS = [
    (re.compile(r'^\s*function\s+clause\s+([A-Za-z0-9_#]+)'),      'function-clause'),
    (re.compile(r'^\s*function\s+([A-Za-z0-9_#]+)'),                'function'),
    (re.compile(r'^\s*val\s+([A-Za-z0-9_#]+)'),                     'val'),
    (re.compile(r'^\s*overload\s+([A-Za-z0-9_#]+)'),                'overload'),
    (re.compile(r'^\s*mapping\s+clause\s+([A-Za-z0-9_#]+)'),        'mapping-clause'),
    (re.compile(r'^\s*mapping\s+([A-Za-z0-9_#]+)'),                 'mapping'),
    (re.compile(r'^\s*scattered\s+(\w+)\s+([A-Za-z0-9_#]+)'),       'scattered'),
    (re.compile(r'^\s*end\s+([A-Za-z0-9_#]+)'),                     'scattered-end'),
    (re.compile(r'^\s*union\s+clause\s+([A-Za-z0-9_#]+)'),          'union-clause'),
    (re.compile(r'^\s*union\s+([A-Za-z0-9_#]+)'),                   'union'),
    (re.compile(r'^\s*struct\s+([A-Za-z0-9_#]+)'),                  'struct'),
    (re.compile(r'^\s*enum\s+clause\s+([A-Za-z0-9_#]+)'),           'enum-clause'),
    (re.compile(r'^\s*enum\s+([A-Za-z0-9_#]+)'),                    'enum'),
    (re.compile(r'^\s*bitfield\s+([A-Za-z0-9_#]+)'),                'bitfield'),
    (re.compile(r'^\s*type\s+([A-Za-z0-9_#]+)'),                    'type'),
    (re.compile(r'^\s*register\s+([A-Za-z0-9_#]+)'),                'register'),
    (re.compile(r'^\s*let\s+\(?([A-Za-z0-9_#]+)'),                  'let'),
    (re.compile(r'^\s*outcome\s+([A-Za-z0-9_#]+)'),                 'outcome'),
    (re.compile(r'^\s*instantiation\s+([A-Za-z0-9_#]+)'),           'instantiation'),
    (re.compile(r'^\s*termination_measure\s+([A-Za-z0-9_#]+)'),     'termination-measure'),
    (re.compile(r'^\s*default\s+(\w+)'),                            'default'),
    (re.compile(r'^\s*\$(\w+)'),                                    'pragma'),
    (re.compile(r'^\s*constraint\b'),                               'constraint'),
]


def sail_kind_and_name(code_lines):
    """The kind and name of the first real definition line, skipping attributes and comments."""
    code = re.sub(r'/\*.*?\*/', ' ', '\n'.join(code_lines), flags=re.DOTALL)
    for line in code.splitlines():
        stripped = line.strip()
        if not stripped or stripped.startswith('///') or stripped.startswith('//'):
            continue
        if stripped.startswith('$[') or stripped.startswith('/*'):
            # attribute like $[complete] or $[sv_module ...]; may be followed by the definition on the same line
            rest = re.sub(r'^\$\[[^\]]*\]\s*', '', stripped)
            if rest == stripped or not rest:
                continue
            stripped = rest
        for regex, kind in SAIL_DEFINITION_HEADS:
            match = regex.match(stripped)
            if match:
                if kind == 'scattered':
                    return f'scattered-{match.group(1)}', match.group(2)
                if kind == 'constraint':
                    return kind, ''
                return kind, match.group(1)
        return 'unknown', stripped[:60]
    return 'unknown', ''


# Heuristic: which Sail construct is involved, from the OCaml location and the message.
# The OCaml location (which match arm) is the primary key; the message refines it.
def guess_construct(ocaml_loc, message, snippet=''):
    loc = ocaml_loc or ''
    msg = (message or '').lower()
    text = f'{loc} {msg}'
    rules = [
        (r'add_bits_int|sub_bits_int|compile time',                       'bitvector size/amount not known at compile time'),
        (r'sail_ones|sail_zeros|zeros|ones',                              'bitvector size not known at compile time'),
        (r'polymorphic',                                                  'polymorphic function'),
        (r'monomorph',                                                    'monomorphisation'),
        (r'elements of bit vector should all be bits',                    'vector literal of bitvectors'),
        (r'ignored cast',                                                 'cast (E_typ)'),
        (r'expected pattern|pattern',                                     'pattern'),
        (r'Match\.ml',                                                    'pattern'),
        (r'tuple',                                                        'tuple'),
        (r'Type\.ml',                                                     'type'),
        (r'Numeric\.ml|numeric|nexp',                                     'numeric expression/constraint'),
        (r'TypeDefinition|type definition|Recursive\.ml|Union|Enum|Record','type definition'),
        (r'ValueDefinition|top.?level.?let',                              'top-level let'),
        (r'TopLevelTypeConstraint',                                       'top-level type constraint (val)'),
        (r'Translation\.ml',                                              'definition kind'),
        (r'FunctionCalls\.ml',                                            'function call'),
        (r'Statements\.ml',                                               'statement'),
        (r'Function\.ml',                                                 'expression/statement'),
    ]
    for pattern, construct in rules:
        if re.search(pattern, text, re.IGNORECASE):
            return construct
    return ''


SOURCE_ROOT = Path(__file__).resolve().parents[2]      # the sail-backend checkout
_source_cache = {}
ARM = re.compile(r'^\s*\|\s*([A-Za-z_][A-Za-z0-9_.]*)')


def construct_from_source(ocaml_loc):
    """The match arm (constructor) at the OCaml location, e.g. 'Function.ml: AE_for', if there is one."""
    match = re.match(r'(\S+) line (\d+)', ocaml_loc or '')
    if not match:
        return ''
    path = SOURCE_ROOT / match.group(1)
    if path not in _source_cache:
        _source_cache[path] = path.read_text(errors='replace').splitlines() if path.exists() else []
    lines = _source_cache[path]
    number = int(match.group(2))
    if not 0 < number <= len(lines):
        return ''
    arm = ARM.match(lines[number - 1])
    return f'{path.name}: {arm.group(1)}' if arm else ''


def item(run, stage, **fields):
    record = dict(run=run, stage=stage, kind='', name='', sail_loc='', ocaml_loc='',
                  message='', construct='', where='', snippet='')
    record.update({key: (value if value is not None else '') for key, value in fields.items()})
    if not record['construct']:
        record['construct'] = construct_from_source(record['ocaml_loc'])
    if not record['construct']:
        record['construct'] = guess_construct(record['ocaml_loc'], record['message'], record['snippet'])
    return record


# ---------------------------------------------------------------------------
# Untranslated definitions: the ---- blocks in the generated .v files
# ---------------------------------------------------------------------------
def extract_untranslated(run, vfile):
    lines = vfile.read_text(errors='replace').splitlines()
    items = []
    i = 0
    while i < len(lines):
        if lines[i].strip() == '----':
            # a block runs to the next '----'; it ends with OCaml location / Sail location / message
            j = i + 1
            while j < len(lines) and lines[j].strip() != '----':
                j += 1
            block = lines[i + 1:j]
            ocaml = next((l for l in block if l.startswith('OCaml location: ')), None)
            if ocaml is not None:
                k = block.index(ocaml)
                code = block[:k]
                sail_loc = next((l[len('Sail location: '):] for l in block if l.startswith('Sail location: ')), '')
                message = next((l[len('Message: '):] for l in block if l.startswith('Message: ')), '')
                kind, name = sail_kind_and_name(code)
                snippet = ' '.join(l.strip() for l in code if l.strip())[:300]
                items.append(item(run, 'untranslated', kind=kind, name=name, sail_loc=sail_loc,
                                  ocaml_loc=ocaml[len('OCaml location: '):], message=message,
                                  where=f'{vfile.name}:{i + 1}', snippet=snippet))
            i = j + 1
        else:
            i += 1
    return items


# ---------------------------------------------------------------------------
# NYI holes: "[k] : Not yet implemented; see F line N (msg)" annotations in the
# comment that precedes a generated definition, and NYI[k] in that definition.
# ---------------------------------------------------------------------------
NYI_ANNOTATION = re.compile(r'^\s*(?:\(\*)?\[(\d+)\]\s*:\s*Not yet implemented; see (\S+) line (\d+)(?: \((.*?))?\s*(?:\*\))?\s*$')
COQ_DEFINITION = re.compile(r'^\s*(?:#\[[^\]]*\]\s*)?(?:Local\s+|Global\s+)?(Definition|Inductive|Fixpoint|Record|Notation|Instance|Lemma|Equations|Variant|Class|Let|Axiom|Parameter)\s+([^\s:(]+)')
FUN_DEFINITION = re.compile(r'^fun_([A-Za-z0-9_#]+)$')
NYI_USE = re.compile(r'NYI\[(\d+)\]')


def extract_nyi(run, vfile):
    """
    One item per NYI[id] hole in the code.

    The comment above a generated block lists that frame's annotations as [0], [1], ...
    (the index within the frame), while the code says NYI[id] with a global counter, so the
    numbers do not correspond. Frames nest, and a frame's annotations are added in the order
    of its holes. So each hole (in file order) takes the next unused annotation of the
    nearest preceding annotation block that still has unused annotations.
    """
    lines = vfile.read_text(errors='replace').splitlines()
    blocks = []          # list of lists of [ocaml_file, ocaml_line, message]; consumed front to back
    current = None       # annotation being read (its message may wrap over several lines)
    items = []
    last_block_line = 0
    for number, line in enumerate(lines, start=1):
        match = NYI_ANNOTATION.match(line)
        if match:
            index, ocaml_file, ocaml_line, message = match.groups()
            if not blocks or number - last_block_line > 3 or index == '0':
                blocks.append([])
            current = [ocaml_file, ocaml_line, (message or '').rstrip()]
            blocks[-1].append(current)
            last_block_line = number
            continue
        if current is not None:
            stripped = line.strip()
            if stripped and stripped != '*)' and not stripped.startswith('[') and current[2] and not current[2].endswith(')'):
                current[2] += ' ' + stripped        # wrapped message
                last_block_line = number
                continue
            if stripped == '' :
                continue
            current = None
        for use in NYI_USE.finditer(line):
            annotation = None
            for block in reversed(blocks):
                if block:
                    annotation = block.pop(0)
                    break
            name, coq_kind = '', '?'
            for back in range(number, max(0, number - 3000), -1):
                found = COQ_DEFINITION.match(lines[back - 1])
                if found:
                    coq_kind, name = found.group(1), found.group(2)
                    break
            fun = FUN_DEFINITION.match(name)
            if fun:
                name = fun.group(1)
            ocaml_loc = f'{annotation[0]} line {annotation[1]}' if annotation else ''
            message = annotation[2] if annotation else 'no annotation found'
            if message.endswith(')'):
                message = message[:-1]
            items.append(item(run, 'nyi', kind=f'µSail {coq_kind}', name=name, ocaml_loc=ocaml_loc,
                              message=message, where=f'{vfile.name}:{number}', snippet=line.strip()[:200]))
    return items


# ---------------------------------------------------------------------------
# Log: warnings, errors, skipped definitions, with the definition being processed
# ---------------------------------------------------------------------------
LOG_ENTRY = re.compile(r'^\[(DEBUG|INFO|WARNING|ERROR)\] (\S+):(\d+)\s*$')


def log_entries(logfile):
    """Yield (level, ocaml_file, ocaml_line, message_lines, log_line_number)."""
    current = None
    with open(logfile, errors='replace') as handle:
        for number, raw in enumerate(handle, start=1):
            line = ANSI.sub('', raw.rstrip('\n'))
            match = LOG_ENTRY.match(line)
            if match:
                if current:
                    yield current
                current = (match.group(1), match.group(2), int(match.group(3)), [], number)
            elif current is not None:
                if line.startswith(' '):
                    current[3].append(line)
                else:
                    yield current
                    current = None
    if current:
        yield current


def extract_log(run, logfile):
    items = []
    sail_definition = ('', '', '')         # kind, name, first code line, of the definition being translated
    generated = ''                         # µSail function being generated
    for level, ocaml_file, ocaml_line, body, number in log_entries(logfile):
        text = [l.strip() for l in body]
        first = text[0] if text else ''
        if level == 'DEBUG':
            if first == 'Entering Processing definition':
                code = [l.strip() for l in body[1:]]
                kind, name = sail_kind_and_name(code)
                sail_definition = (kind, name, ' '.join(c for c in code if c)[:300])
            elif first.startswith('Entering Function Definition '):
                generated = first[len('Entering Function Definition '):]
            elif first.startswith('Generating code for function '):
                generated = first[len('Generating code for function '):]
            continue
        ocaml_loc = f'{ocaml_file} line {ocaml_line}'
        in_generation = ocaml_file.startswith('nanosail/NanosailToMicrosail')
        if level == 'INFO':
            if first == 'Skipping this definition':
                code = text[1:]
                kind, name = sail_kind_and_name(code)
                items.append(item(run, 'skipped', kind=kind, name=name, ocaml_loc=ocaml_loc,
                                  message='skipped by configuration', construct='ignored by configuration',
                                  where=f'{logfile.name}:{number}', snippet=' '.join(code)[:300]))
            continue
        message = ' | '.join(text)
        kind, name, snippet = sail_definition
        if in_generation:
            kind, name, snippet = ('µSail function', generated, '')
        if level == 'ERROR' and text and text[0].startswith('Assertion error at '):
            # a Failure that was recovered: the definition became untranslated
            failing = text[0][len('Assertion error at '):]
            msg = next((t[len('Message: '):] for t in text if t.startswith('Message: ')), message)
            items.append(item(run, 'error', kind=kind, name=name, ocaml_loc=failing, message=msg,
                              where=f'{logfile.name}:{number}', snippet=snippet))
            continue
        items.append(item(run, level.lower(), kind=kind, name=name, ocaml_loc=ocaml_loc, message=message,
                          where=f'{logfile.name}:{number}', snippet=snippet))
    return items


# ---------------------------------------------------------------------------
# Hard generation failures (from workarounds/hard-failure-loop.sh)
# ---------------------------------------------------------------------------
def extract_hard_failures(run, logfile):
    items = []
    blocks = logfile.read_text(errors='replace').split('### ')[1:]
    for block in blocks:
        lines = [l.strip() for l in block.splitlines() if l.strip()]
        name = lines[0]
        error = next((l for l in lines if 'Error occurred during generation' in l), '')
        match = re.search(r'\((\S+?):(\d+)\)\s*(.*)', error)
        ocaml_loc = f'{match.group(1)} line {match.group(2)}' if match else ''
        message = match.group(3) if match else error
        sail_loc = next((l[3:] for l in lines if l.startswith('at ')), '')
        context = ' | '.join(l for l in lines[1:] if l.startswith('in '))
        items.append(item(run, 'hard-failure', kind='function', name=name, sail_loc=sail_loc,
                          ocaml_loc=ocaml_loc, message=f'{message} [{context}]',
                          where=logfile.name))
    return items


# ---------------------------------------------------------------------------
# coqc: the first error of each file
# ---------------------------------------------------------------------------
COQ_FILE = re.compile(r'^File "\./?([^"]+)", line (\d+), characters (\d+)-(\d+):')


def extract_coqc(run, logfile):
    lines = logfile.read_text(errors='replace').splitlines()
    items = []
    for i, line in enumerate(lines):
        match = COQ_FILE.match(line)
        if match and i + 1 < len(lines) and lines[i + 1].startswith('Error'):
            message = [lines[i + 1][len('Error:'):].strip()]
            for follow in lines[i + 2:i + 8]:
                if COQ_FILE.match(follow):
                    break
                message.append(follow.strip())
            items.append(item(run, 'coqc-error', ocaml_loc='', message=' '.join(m for m in message if m)[:400],
                              construct='coqc', where=f'{match.group(1)}:{match.group(2)}'))
    return items


def run_items(directory):
    directory = Path(directory)
    run = directory.name
    items = []
    for vname in ('base.v', 'machine.v'):
        vfile = directory / vname
        if vfile.exists():
            items += extract_untranslated(run, vfile)
            items += extract_nyi(run, vfile)
    for logfile in (directory / 'log.txt', directory.with_suffix('.log'), directory.parent / f'{run}.log'):
        if logfile.exists():
            items += extract_log(run, logfile)
            break
    hard = directory / 'hard-failures.log'
    if hard.exists():
        items += extract_hard_failures(run, hard)
    for coqlog in sorted(directory.glob('coqc-*.log')):
        items += extract_coqc(run, coqlog)
    return items


def main():
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument('runs', nargs='+')
    parser.add_argument('--out', required=True, help='CSV output')
    parser.add_argument('--json', help='JSON output')
    arguments = parser.parse_args()
    items = []
    for run in arguments.runs:
        items += run_items(run)
    fields = ['run', 'stage', 'kind', 'name', 'sail_loc', 'ocaml_loc', 'message', 'construct', 'where', 'snippet']
    with open(arguments.out, 'w', newline='') as handle:
        writer = csv.DictWriter(handle, fieldnames=fields)
        writer.writeheader()
        writer.writerows(items)
    if arguments.json:
        Path(arguments.json).write_text(json.dumps(items, indent=1, ensure_ascii=False))
    counts = {}
    for record in items:
        key = (record['run'], record['stage'])
        counts[key] = counts.get(key, 0) + 1
    for (run, stage), count in sorted(counts.items()):
        print(f'{run:20} {stage:14} {count}', file=sys.stderr)


if __name__ == '__main__':
    main()
