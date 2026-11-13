#!/usr/bin/env python3
"""
The outer loop: translate the CHERIoT model with this checkout's plugin, and measure the result.

Usage:
    tests/cheriot/run.py --cheriot-sail DIR --case-study DIR [--keep-dumps] [--output-dir DIR]

or `gmake cheriot CHERIOT_SAIL_DIR=... CHERIOT_CASE_STUDY=...`. See docs/cheriot-loop.md.

Inputs:
    --cheriot-sail DIR   a cheriot-sail checkout, with its sail-riscv submodule
                         (default: $CHERIOT_SAIL_DIR)
    --case-study DIR     the case study: configuration.lisp, *.template.v and
                         printvar.mak, as in katamaran's case_study/Cheriot
                         (default: $CHERIOT_CASE_STUDY)

The run has these stages; a stage that fails ends the run.

    inputs      check the inputs and record their versions (commits, Sail, Katamaran)
    typecheck   plain Sail typechecks the model, so that a failure there is not
                blamed on the plugin
    translate   the plugin translates the model, with VERBOSE=4: lower levels leave
                out the skipped definitions and the definition each warning and
                error belongs to, and save only about 10% of the time
    analyse     items, reachability, missing calls and piles (tests/cheriot/lib)

Everything goes to a fresh directory <output-dir>/<timestamp>-<commit>[-dirty]/,
with <output-dir>/latest pointing to it (default output dir: tests/cheriot/output):
sources.txt, the logs, base.v and machine.v, dump/ (only the last Sail rewrite
dump, unless --keep-dumps), items.csv, dangling-calls.csv, piles.txt,
run.json (inputs and stages) and metrics.json.

Exit status: 0 when every stage succeeded, 1 when a stage failed (the report
says which and why), 2 for invalid arguments or inputs.
"""

from __future__ import annotations

import argparse
import csv
import datetime
import json
import os
import re
import shutil
import subprocess
import sys
import time
import tomllib
from dataclasses import asdict, dataclass, field
from pathlib import Path
from typing import Any, Optional

HERE = Path(__file__).resolve().parent
CHECKOUT = HERE.parent.parent
sys.path.insert(0, str(HERE))

from lib import callgraph, dangling, extract, metrics, piles     # noqa: E402
from lib.items import write_items                                  # noqa: E402

SAIL_LOCAL = CHECKOUT / 'bin' / 'sail-local'
PLUGIN = CHECKOUT / '_build' / 'default' / 'sail_plugin' / 'sail_plugin_katamaran.cmxs'
DEFAULT_OUTPUT_DIR = HERE / 'output'
PINS = HERE / 'inputs.toml'
SAIL_VERSION = '0.20.3'
VERBOSE = '4'
CASE_STUDY_FILES = ['configuration.lisp', 'printvar.mak']

INTERNAL_ERROR = re.compile(r'^Internal error', re.M)
UNCAUGHT_EXCEPTION = re.compile(r'^Fatal error: exception (.*)$', re.M)
ERROR_START = re.compile(r'^(Type error|Internal error|Error|Syntax error|Fatal error)')
ANSI = re.compile(r'\x1b\[[0-9;]*m')


class InputError(Exception):
    """An input is missing or unusable; the run cannot start."""


@dataclass
class Stage:
    name: str
    ok: bool = False
    seconds: float = 0.0
    summary: str = ''                      # one line, for the report
    details: list[str] = field(default_factory=list)


# --------------------------------------------------------------------------
# helpers
# --------------------------------------------------------------------------

def run_command(command: list[str], cwd: Path, log: Path, environment: Optional[dict[str, str]] = None) -> int:
    """Run a command, writing its merged stdout and stderr to log; return its exit code."""
    with open(log, 'w') as handle:
        handle.write(f'$ {" ".join(command)}\n')
        handle.flush()
        try:
            return subprocess.run(command, cwd=cwd, stdout=handle, stderr=subprocess.STDOUT,
                                  env={**os.environ, **(environment or {})}).returncode
        except OSError as error:
            handle.write(f'failed to run {command[0]}: {error}\n')
            return 127


def output_of(command: list[str], cwd: Optional[Path] = None) -> Optional[str]:
    """The stripped stdout of a command, or None if it cannot run or fails."""
    try:
        completed = subprocess.run(command, cwd=cwd, capture_output=True, text=True)
    except OSError:
        return None
    return completed.stdout.strip() if completed.returncode == 0 else None


def git_state(path: Path, only_here: bool = False) -> dict[str, Any]:
    """The commit of the repository that contains path, and whether it has changes (only under path, if asked)."""
    commit = output_of(['git', '-C', str(path), 'rev-parse', '--short', 'HEAD'])
    status = output_of(['git', '-C', str(path), 'status', '--porcelain', *(['--', '.'] if only_here else [])])
    return {'commit': commit or 'unknown', 'dirty': None if status is None else bool(status)}


def describe(state: dict[str, Any]) -> str:
    return state['commit'] + ('+dirty' if state.get('dirty') else '')


def gnu_make() -> str:
    """GNU make 4 or later: the make that ships with macOS (3.81) cannot read the cheriot-sail Makefile."""
    for candidate in ('gmake', 'make'):
        version = output_of([candidate, '--version'])
        match = re.match(r'GNU Make (\d+)', version or '')
        if match and int(match.group(1)) >= 4:
            return candidate
    raise InputError('GNU make 4 or later is needed (on macOS: gmake)')


def run_identifier(state: dict[str, Any]) -> str:
    timestamp = datetime.datetime.now().strftime('%Y%m%dT%H%M%S')
    return f'{timestamp}-{state["commit"]}' + ('-dirty' if state.get('dirty') else '')


# --------------------------------------------------------------------------
# stages
# --------------------------------------------------------------------------

def sail_sources(cheriot_sail: Path, case_study: Path, make: str) -> list[Path]:
    """
    The model's source files, in order: the SAIL_SRCS of cheriot-sail's Makefile, plus
    sail-riscv's main.sail. This is what the case study's Makefile translates.
    """
    command = [make, '-sC', str(cheriot_sail), '-f', str(case_study / 'printvar.mak'), '-f', 'Makefile',
               'SAIL=sail', 'print-SAIL_SRCS']
    printed = output_of(command)
    if not printed:
        raise InputError(f'could not get SAIL_SRCS from {cheriot_sail}/Makefile ({" ".join(command)})')
    sources = [cheriot_sail / name for name in printed.split()] + [cheriot_sail / 'sail-riscv' / 'model' / 'main.sail']
    missing = [str(s) for s in sources if not s.is_file()]
    if missing:
        raise InputError(f'missing Sail sources (is the sail-riscv submodule checked out?): {", ".join(missing[:5])}')
    return sources


def pin_warnings(inputs: dict[str, Any], pins_file: Path) -> list[str]:
    """Warnings for inputs whose commit differs from the one pinned in inputs.toml."""
    if not pins_file.exists():
        return []
    pins = tomllib.loads(pins_file.read_text())
    warnings = []
    for name, pin in pins.items():
        actual = inputs.get(name, {}).get('commit', 'unknown')
        expected = pin.get('commit', '')
        if expected and not (actual.startswith(expected) or expected.startswith(actual)):
            warnings.append(f'{name} is at {actual}, but {pins_file.name} pins {expected}')
    return warnings


def check_inputs(cheriot_sail: Path, case_study: Path) -> tuple[Stage, dict[str, Any], list[Path]]:
    stage = Stage('inputs')
    start = time.monotonic()
    if not PLUGIN.is_file():
        raise InputError(f'{PLUGIN} does not exist; run dune build')
    sail_version = output_of(['sail', '--version']) or ''
    if SAIL_VERSION not in sail_version:
        raise InputError(f'Sail {SAIL_VERSION} is needed; sail --version says {sail_version or "nothing"}')
    for name in CASE_STUDY_FILES:
        if not (case_study / name).is_file():
            raise InputError(f'{case_study} has no {name}: is it the case study directory?')
    if not (cheriot_sail / 'Makefile').is_file():
        raise InputError(f'{cheriot_sail} has no Makefile: is it a cheriot-sail checkout?')
    sources = sail_sources(cheriot_sail, case_study, gnu_make())

    katamaran = 'unknown'
    coq_root = output_of(['coqc', '-where'])
    if coq_root:
        installed_from = Path(coq_root) / 'user-contrib' / 'Katamaran' / 'INSTALLED-FROM.txt'
        if installed_from.exists():
            found = re.search(r'@ ([0-9a-f]{7,})', installed_from.read_text())
            katamaran = found.group(1) if found else 'see ' + str(installed_from)
    inputs = {
        'sail-backend': git_state(CHECKOUT),
        'cheriot-sail': {**git_state(cheriot_sail), 'path': str(cheriot_sail),
                         'sail-riscv': git_state(cheriot_sail / 'sail-riscv')['commit']},
        'case-study': {**git_state(case_study, only_here=True), 'path': str(case_study)},
        'sail': sail_version,
        'katamaran': katamaran,
        'sources': len(sources),
    }
    stage.details = pin_warnings(inputs, PINS)
    stage.ok = True
    stage.seconds = time.monotonic() - start
    stage.summary = f'{len(sources)} Sail files' + (f'; {len(stage.details)} warning(s)' if stage.details else '')
    return stage, inputs, sources


def typecheck(sources: list[Path], sandbox: Path) -> Stage:
    stage = Stage('typecheck')
    start = time.monotonic()
    code = run_command(['sail', '--just-check', *map(str, sources)], sandbox, sandbox / 'typecheck.log')
    stage.seconds = time.monotonic() - start
    stage.ok = code == 0
    if not stage.ok:
        stage.summary = f'Sail rejects the model (exit {code}); the problem is in the model or the inputs, not the plugin'
        stage.details = first_error(sandbox / 'typecheck.log')
    return stage


def first_error(log: Path, lines: int = 8) -> list[str]:
    """The first error message in a Sail log, a few lines long."""
    text = ANSI.sub('', log.read_text(errors='replace')).splitlines()
    for number, line in enumerate(text):
        if ERROR_START.match(line) or 'Error occurred during generation' in line:
            return text[max(0, number - 1):number + lines]
    return text[-lines:]


def classify_translation(code: int, log_text: str, outputs_exist: bool) -> tuple[bool, str]:
    """Whether the translation succeeded, and a one-line description."""
    clean = ANSI.sub('', log_text)
    hard = re.search(r'Error occurred during generation: \(\S+\) (.*)', clean)
    function = re.search(r'in body of function (\S+)', clean)
    if hard:
        where = f' in {function.group(1)}' if function else ''
        return False, f'hard failure{where}: {hard.group(1)}'
    exception = UNCAUGHT_EXCEPTION.search(clean)
    if exception:
        return False, f'uncaught exception: {exception.group(1)[:200]}'
    if INTERNAL_ERROR.search(clean):
        return False, f'Sail crashed (exit {code})'
    if code != 0:
        return False, f'Sail failed (exit {code})'
    if not outputs_exist:
        return False, 'Sail succeeded but wrote no base.v or machine.v'
    return True, ''


def translate(sources: list[Path], case_study: Path, sandbox: Path) -> Stage:
    stage = Stage('translate')
    for template in [case_study / 'configuration.lisp', *sorted(case_study.glob('*.template.v'))]:
        shutil.copy2(template, sandbox / template.name)
    (sandbox / 'dump').mkdir()
    command = [str(SAIL_LOCAL), '--ddump-rewrite-ast', 'dump/rw',
               '--katamaran', '--katamaran-config', 'configuration.lisp', *map(str, sources)]
    start = time.monotonic()
    code = run_command(command, sandbox, sandbox / 'log.txt', {'VERBOSE': VERBOSE})
    stage.seconds = time.monotonic() - start
    outputs = (sandbox / 'base.v').is_file() and (sandbox / 'machine.v').is_file()
    stage.ok, stage.summary = classify_translation(code, (sandbox / 'log.txt').read_text(errors='replace'), outputs)
    if not stage.ok:
        stage.details = first_error(sandbox / 'log.txt')
    return stage


def last_dump(dump_directory: Path, keep_all: bool) -> Optional[Path]:
    """The last rewrite dump, if any. Unless keep_all, the other dumps are deleted: together they take about 75 MB."""
    try:
        last = callgraph.last_dump(dump_directory)
    except FileNotFoundError:
        return None
    if not keep_all:
        for path in dump_directory.glob('*_rewrite_*.sail'):
            if path != last:
                path.unlink()
    return last


def analyse(sandbox: Path, dump: Optional[Path]) -> tuple[Stage, Optional[metrics.Metrics]]:
    stage = Stage('analyse')
    start = time.monotonic()
    if dump is None:
        stage.summary = 'no Sail rewrite dump in dump/'
        return stage, None
    items = extract.run_items(sandbox, extract.NanosailSource(CHECKOUT))
    graph = callgraph.CallGraph.from_dump(dump)
    callgraph.mark_reachable(items, graph)
    piles.assign_piles(items)
    machine_v = (sandbox / 'machine.v').read_text(errors='replace')
    calls = dangling.dangling_calls(machine_v, dump.read_text(errors='replace'), items, sandbox.name)
    write_items(sandbox / 'items.csv', items)
    with open(sandbox / 'dangling-calls.csv', 'w', newline='') as handle:
        writer = csv.DictWriter(handle, fieldnames=dangling.DanglingCall.FIELDS)
        writer.writeheader()
        writer.writerows(d.as_row() for d in calls)
    (sandbox / 'piles.txt').write_text(piles.format_pile_table(*piles.pile_table(items)) + '\n')
    result = metrics.compute(items, machine_v, graph, calls)
    stage.ok = True
    stage.seconds = time.monotonic() - start
    stage.summary = f'{len(items)} items'
    return stage, result


# --------------------------------------------------------------------------
# report
# --------------------------------------------------------------------------

def report(inputs: dict[str, Any], stages: list[Stage], result: Optional[metrics.Metrics], sandbox: Path) -> str:
    lines = []
    if inputs:
        lines.append(f'CHERIoT @ {describe(inputs["sail-backend"])}   '
                     f'(cheriot-sail {describe(inputs["cheriot-sail"])}, '
                     f'case study {describe(inputs["case-study"])}, '
                     f'{inputs["sail"].split(" (")[0]}, Katamaran {inputs["katamaran"]})')
    for stage in stages:
        status = 'ok  ' if stage.ok else 'FAIL'
        lines.append(f'  {status} {stage.name:10} {stage.seconds:6.1f} s  {stage.summary}')
        lines.extend(f'         | {detail}' for detail in stage.details)
    if result is not None:
        lines.append('')
        lines.extend(metrics.format_metrics(result))
    lines.append('')
    lines.append(f'output: {sandbox}')
    return '\n'.join(lines)


def update_latest(output_dir: Path, identifier: str) -> None:
    latest = output_dir / 'latest'
    try:
        if latest.is_symlink() or latest.exists():
            latest.unlink()
        latest.symlink_to(identifier)
    except OSError as error:
        print(f'note: could not update {latest}: {error}', file=sys.stderr)


# --------------------------------------------------------------------------
# main
# --------------------------------------------------------------------------

def directory_argument(value: Optional[str], variable: str, flag: str) -> Path:
    value = value or os.environ.get(variable)
    if not value:
        raise InputError(f'pass {flag} DIR or set {variable}')
    path = Path(value).expanduser().resolve()
    if not path.is_dir():
        raise InputError(f'{flag}: {path} is not a directory')
    return path


def parse_arguments(argv: Optional[list[str]]) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument('--cheriot-sail', help='cheriot-sail checkout (default: $CHERIOT_SAIL_DIR)')
    parser.add_argument('--case-study', help='case study directory (default: $CHERIOT_CASE_STUDY)')
    parser.add_argument('--keep-dumps', action='store_true', help='keep the Sail AST dump of every rewrite')
    parser.add_argument('--output-dir', type=Path, default=DEFAULT_OUTPUT_DIR)
    return parser.parse_args(argv)


def main(argv: Optional[list[str]] = None) -> int:
    arguments = parse_arguments(argv)
    try:
        cheriot_sail = directory_argument(arguments.cheriot_sail, 'CHERIOT_SAIL_DIR', '--cheriot-sail')
        case_study = directory_argument(arguments.case_study, 'CHERIOT_CASE_STUDY', '--case-study')
    except InputError as error:
        print(f'run.py: {error}', file=sys.stderr)
        return 2

    try:
        stage, inputs, sources = check_inputs(cheriot_sail, case_study)
    except InputError as error:
        print(f'run.py: {error}', file=sys.stderr)
        return 2

    output_dir = arguments.output_dir.resolve()
    identifier = run_identifier(inputs['sail-backend'])
    sandbox = output_dir / identifier
    sandbox.mkdir(parents=True)
    update_latest(output_dir, identifier)
    (sandbox / 'sources.txt').write_text(''.join(f'{s}\n' for s in sources))

    stages = [stage, typecheck(sources, sandbox)]
    result = None
    if stages[-1].ok:
        stages.append(translate(sources, case_study, sandbox))
        dump = last_dump(sandbox / 'dump', arguments.keep_dumps)
        if stages[-1].ok:
            stage, result = analyse(sandbox, dump)
            stages.append(stage)

    (sandbox / 'run.json').write_text(json.dumps(
        {'identifier': identifier, 'inputs': inputs, 'stages': [asdict(s) for s in stages]}, indent=2) + '\n')
    if result is not None:
        (sandbox / 'metrics.json').write_text(json.dumps(result, indent=2) + '\n')
    print(report(inputs, stages, result, sandbox))
    return 0 if all(s.ok for s in stages) else 1


if __name__ == '__main__':
    sys.exit(main())
