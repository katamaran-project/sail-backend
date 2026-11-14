#!/usr/bin/env python3
"""
The outer loop: translate the CHERIoT model with this checkout's plugin, and measure the result.

Usage:
    tests/cheriot/run.py --cheriot-sail DIR --case-study DIR [--update-baseline] [--no-scope] [--no-coq]
                         [--coq-stall S] [--no-coq-cache] [--keep-dumps] [--output-dir DIR]

or `gmake cheriot CHERIOT_SAIL_DIR=...`. See docs/cheriot-loop.md.

Inputs:
    --cheriot-sail DIR   a cheriot-sail checkout, with its sail-riscv submodule
                         (default: $CHERIOT_SAIL_DIR)
    --case-study DIR     the case study: configuration.lisp, *.template.v and
                         printvar.mak (default: $CHERIOT_CASE_STUDY, or else
                         case-studies/cheriot in this checkout)

The run has these stages; a stage that fails ends the run.

    inputs      check the inputs and record their versions (commits, Sail, Katamaran)
    typecheck   plain Sail typechecks the model, so that a failure there is not
                blamed on the plugin
    scope       unless --no-scope: compute the CHERIoT core from scope.toml and the
                call graph of the rewritten model (lib/scope.py), and write the
                configuration that ignores everything else (scope-ignores.lisp).
                The call graph needs Sail's AST dump after the rewrites, which is
                cached (see DUMP_CACHE below); without it, a Sail run that only
                makes the dump takes about 20 s.
    translate   the plugin translates the model (with the scope's ignores), with VERBOSE=4: lower levels leave
                out the skipped definitions and the definition each warning and
                error belongs to, and save only about 10% of the time
    rocq        unless --no-coq: coqc base.v and machine.v, as the end-to-end tests
                do, and probe copies in which NYI is an axiom, whose first error is
                the first problem beyond the NYI holes (lib/rocq.py). An error in
                the generated Rocq is a measurement, not a failed stage.
                coqc runs under a watchdog: a command that runs longer than
                --coq-stall seconds (default 45) stops it, and the compilation is
                reported as STUCK on that command, which the ratchet does not count
                as Rocq getting less far. A file without NYI is compiled once (its
                probe is the plain compilation); plain and probe run in parallel;
                verdicts and base.vo are cached (ROCQ_CACHE below; --no-coq-cache).
                The report and metrics.json list the slowest commands.
    analyse     items, reachability, missing calls and piles (tests/cheriot/lib),
                restricted to the scope. It fails if machine.v calls an excluded
                definition other than from a dispatcher (the scope is then wrong),
                or if the translation's dump differs from the one the scope was
                computed from. With a scope, it also finds the status of each
                definition and the clean instructions (lib/progress.py).
    ratchet     with a scope: compare with tests/cheriot/baseline.json, and fail if
                anything got worse than there (a definition, a clean instruction,
                how far Rocq gets), even if the totals improved. --update-baseline
                accepts the current state as the new baseline, in the commit that
                makes the progress.

Every run that gets through the analysis appends a line to tests/cheriot/history.csv
(tracked in git, like the end-to-end tests' history).

Everything goes to a fresh directory <output-dir>/<timestamp>-<commit>[-dirty]/,
with <output-dir>/latest pointing to it (default output dir: tests/cheriot/output):
sources.txt, the logs, base.v and machine.v, dump/ (only the last Sail rewrite
dump, unless --keep-dumps), coqc-*.log, probe/, items.csv, dangling-calls.csv, piles.txt,
run.json (inputs and stages), metrics.json, and scope-ignores.lisp and scope.json.

ROCQ_CACHE: coqc verdicts and compiled .vo files, in $SAIL_BACKEND_CHERIOT_ROCQ_CACHE, or
else $XDG_CACHE_HOME/sail-backend/cheriot-rocq, keyed by the compiled file, the files it
loads and a fingerprint of the Rocq installation (coqc's version and every installed .vo).
Only the newest entries are kept; the cache can be deleted at any time.

DUMP_CACHE: the rewrite dumps for the scope are cached in $SAIL_BACKEND_CHERIOT_CACHE,
or else $XDG_CACHE_HOME/sail-backend/cheriot-dumps (~/.cache/...), keyed by the
Sail version, sail_plugin/Rewrites.ml and the model's sources. The cache can be
deleted at any time.

Exit status: 0 when every stage succeeded, 1 when a stage failed (the report
says which and why), 2 for invalid arguments or inputs.
"""

from __future__ import annotations

import argparse
import csv
import datetime
import hashlib
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

from lib import callgraph, dangling, extract, metrics, piles, progress, rocq, scope     # noqa: E402
from lib.items import write_items                                  # noqa: E402

SAIL_LOCAL = CHECKOUT / 'bin' / 'sail-local'
PLUGIN = CHECKOUT / '_build' / 'default' / 'sail_plugin' / 'sail_plugin_katamaran.cmxs'
DEFAULT_OUTPUT_DIR = HERE / 'output'
PINS = HERE / 'inputs.toml'
DEFAULT_CASE_STUDY = CHECKOUT / 'case-studies' / 'cheriot'
SAIL_VERSION = '0.20.3'
COQC = 'coqc'
VERBOSE = '4'
CASE_STUDY_FILES = ['configuration.lisp', 'printvar.mak']
SCOPE_FILE = HERE / 'scope.toml'
REWRITES = CHECKOUT / 'sail_plugin' / 'Rewrites.ml'
DUMP_CACHE = Path(
    os.environ.get('SAIL_BACKEND_CHERIOT_CACHE')
    or Path(os.environ.get('XDG_CACHE_HOME') or Path.home() / '.cache') / 'sail-backend' / 'cheriot-dumps'
)
ROCQ_CACHE = Path(
    os.environ.get('SAIL_BACKEND_CHERIOT_ROCQ_CACHE')
    or Path(os.environ.get('XDG_CACHE_HOME') or Path.home() / '.cache') / 'sail-backend' / 'cheriot-rocq'
)
SCOPE_INCLUDE = 'scope-ignores.lisp'
BASELINE = HERE / 'baseline.json'
HISTORY = HERE / 'history.csv'
HISTORY_FIELDS = ['timestamp', 'commit', 'dirty', 'scope', 'instructions total', 'instructions clean',
                  'instructions generated', 'functions total', 'functions generated', 'untranslated definitions',
                  'NYI holes', 'hard failures', 'missing calls', 'rocq plain', 'rocq probe', 'result', 'run']

# A configuration that ignores every definition, so that a Sail run only makes the rewrite dumps.
DUMP_ONLY_CONFIGURATION = """\
(ignore-function-definition-and-top-level-type-constraints-predicate (lambda (identifier) #t))
(ignore-value-definition-predicate (lambda (identifier) #t))
(ignore-type-definition-predicate (lambda (identifier) #t))
(ignore-all-overloads)
(ignore-pragmas (lambda (identifier) #t))
"""

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


def check_inputs(cheriot_sail: Path, case_study: Path, with_coq: bool) -> tuple[Stage, dict[str, Any], list[Path]]:
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
    coqc_version = output_of([COQC, '--print-version']) if with_coq else 'not used (--no-coq)'
    if coqc_version is None:
        raise InputError(f'{COQC} cannot be run (pass --no-coq to skip the Rocq stage)')

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
        'coqc': coqc_version,
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


def dump_key(sources: list[Path], sail_version: str) -> str:
    """The cache key of the rewrite dump: what the dump depends on."""
    digest = hashlib.sha256()
    digest.update(sail_version.encode())
    digest.update(REWRITES.read_bytes())
    for source in sources:
        digest.update(str(source).encode() + b'\0' + source.read_bytes() + b'\0')
    return digest.hexdigest()[:20]


def rewrite_dump(sources: list[Path], sandbox: Path, sail_version: str, cache: Path) -> tuple[Path, str]:
    """The model's AST dump after the last rewrite, from the cache or from a dump-only Sail run; and how it was got."""
    cached = cache / f'{dump_key(sources, sail_version)}.sail'
    if cached.exists():
        return cached, f'dump from the cache ({cached.name})'
    directory = sandbox / 'scope-dump'
    (directory / 'dump').mkdir(parents=True)
    (directory / 'configuration.lisp').write_text(DUMP_ONLY_CONFIGURATION)
    start = time.monotonic()
    command = [str(SAIL_LOCAL), '--ddump-rewrite-ast', 'dump/rw',
               '--katamaran', '--katamaran-config', 'configuration.lisp', *map(str, sources)]
    code = run_command(command, directory, directory / 'log.txt', {'VERBOSE': '0'})
    last = last_dump(directory / 'dump', keep_all=False)
    if code != 0 or last is None:
        raise ScopeError(f'the dump-only Sail run failed (exit {code}); see {directory / "log.txt"}')
    cache.mkdir(parents=True, exist_ok=True)
    temporary = cached.with_suffix(f'.{os.getpid()}.tmp')
    shutil.copy2(last, temporary)
    temporary.replace(cached)
    shutil.rmtree(directory)
    return cached, f'dump made in {time.monotonic() - start:.0f} s and cached'


class ScopeError(Exception):
    """The scope cannot be computed."""


def compute_scope(sources: list[Path], cheriot_sail: Path, sandbox: Path, sail_version: str,
                  cache: Path) -> tuple[Stage, Optional[scope.Scope], Optional[Path]]:
    stage = Stage('scope')
    start = time.monotonic()
    try:
        dump, how = rewrite_dump(sources, sandbox, sail_version, cache)
        scope_file = scope.ScopeFile.load(SCOPE_FILE)
        instruction_sources = {}
        for name in scope_file.instruction_files:
            path = cheriot_sail / name
            if path not in sources:
                raise ScopeError(f'{SCOPE_FILE.name} names {name}, which is not one of the model\'s sources')
            instruction_sources[name] = path.read_text(errors='replace')
    except ScopeError as error:
        stage.seconds = time.monotonic() - start
        stage.summary = str(error)
        return stage, None, None
    graph = callgraph.CallGraph.from_dump(dump)
    result = scope.compute(graph, scope_file, instruction_sources)
    (sandbox / SCOPE_INCLUDE).write_text(scope.configuration(result))
    (sandbox / 'scope.json').write_text(json.dumps({
        'instructions': result.instructions,
        'functions': sorted(result.nodes & graph.functions),
        'values': sorted(result.nodes & graph.values),
        'excluded_functions': sorted(result.excluded_functions),
        'excluded_values': sorted(result.excluded_values),
        'warnings': result.warnings,
    }, indent=1) + '\n')
    stage.ok = True
    stage.seconds = time.monotonic() - start
    stage.summary = (f'{len(result.instructions)} instructions; {len(result.nodes & graph.functions)} of '
                     f'{len(graph.functions)} functions in scope; {how}')
    stage.details = result.warnings
    return stage, result, dump


def translate(sources: list[Path], case_study: Path, sandbox: Path, with_scope: bool) -> Stage:
    stage = Stage('translate')
    for template in [case_study / 'configuration.lisp', *sorted(case_study.glob('*.template.v'))]:
        shutil.copy2(template, sandbox / template.name)
    if with_scope:
        with open(sandbox / 'configuration.lisp', 'a') as handle:
            handle.write(f'\n;; Added by tests/cheriot/run.py: ignore what is outside the scope.\n$include {SCOPE_INCLUDE}\n')
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


def compile_rocq(sandbox: Path, stall: float, use_cache: bool) -> tuple[Stage, dict[str, Any]]:
    stage = Stage('rocq')
    start = time.monotonic()
    fingerprint = rocq.coq_fingerprint(COQC) if use_cache else None
    cache = rocq.Cache(ROCQ_CACHE, fingerprint) if fingerprint else None
    compilations = rocq.compile_both(COQC, sandbox, sandbox / 'probe', stall, cache)
    stage.seconds = time.monotonic() - start
    stage.ok = True
    results = {kind: [c.as_json() for c in compilations[kind]] for kind in ('plain', 'probe')}

    def summary(kind: str) -> str:
        found = results[kind]
        verdict = ('compiles' if all(c['ok'] for c in found) else
                   'STUCK (see below)' if rocq.is_stuck(found) else 'stops (see below)')
        notes = [note for note, present in (('cached', any(c['cached'] for c in found)),
                                            ('same as plain', bool(found) and all(c['reused'] for c in found)))
                 if present]
        return f'{kind}: {verdict}' + (f' ({", ".join(notes)})' if notes else '')

    stage.summary = '; '.join(summary(kind) for kind in ('plain', 'probe'))
    return stage, results


def analyse(sandbox: Path, dump: Optional[Path], rocq_results: Optional[dict[str, Any]],
            in_scope: Optional[scope.Scope], scope_dump: Optional[Path],
            identity: dict[str, Any]) -> tuple[Stage, Optional[metrics.Metrics], Optional[progress.Snapshot]]:
    stage = Stage('analyse')
    start = time.monotonic()
    if dump is None:
        stage.summary = 'no Sail rewrite dump in dump/'
        return stage, None, None
    problems = []
    if scope_dump is not None and scope_dump.read_bytes() != dump.read_bytes():
        problems.append(f'the scope was computed from a different dump ({scope_dump}) than this translation\'s; '
                        f'the dump cache key misses an input, so delete the cache and run again')
    items = extract.run_items(sandbox, extract.NanosailSource(CHECKOUT))
    graph = callgraph.CallGraph.from_dump(dump)
    callgraph.mark_reachable(items, graph)
    piles.assign_piles(items)
    machine_v = (sandbox / 'machine.v').read_text(errors='replace')
    calls = dangling.dangling_calls(machine_v, dump.read_text(errors='replace'), items, sandbox.name,
                                    in_scope.excluded if in_scope else frozenset())
    if in_scope is not None:
        problems += scope.unexpected_calls(calls, in_scope)
    write_items(sandbox / 'items.csv', items)
    with open(sandbox / 'dangling-calls.csv', 'w', newline='') as handle:
        writer = csv.DictWriter(handle, fieldnames=dangling.DanglingCall.FIELDS)
        writer.writeheader()
        writer.writerows(d.as_row() for d in calls)
    (sandbox / 'piles.txt').write_text(piles.format_pile_table(*piles.pile_table(items)) + '\n')
    result = metrics.compute(items, machine_v, graph, calls, rocq_results, in_scope)
    snapshot = None
    if in_scope is not None:
        statuses, holes = progress.definition_statuses(graph, in_scope, machine_v, items)
        blockers = progress.instruction_blockers(graph, in_scope, statuses, calls)
        result['instructions']['clean'] = sum(1 for found in blockers.values() if not found)
        result['blockers'] = progress.blocker_ranking(blockers)
        snapshot = progress.snapshot(statuses, holes, blockers, rocq_results, identity)
        snapshot['headline'] = metrics.headline(result)
        (sandbox / 'progress.json').write_text(json.dumps({**snapshot, 'blockers': blockers}, indent=1) + '\n')
    stage.ok = not problems
    stage.seconds = time.monotonic() - start
    stage.summary = f'{len(items)} items' + (f'; {len(problems)} problem(s) with the scope' if problems else '')
    stage.details = problems
    return stage, result, snapshot


def ratchet(snapshot: progress.Snapshot, update: bool) -> tuple[Stage, Optional[progress.Comparison], Optional[dict[str, Any]]]:
    """Compare with the baseline; with update, make this run the baseline. Returns the baseline that was compared with."""
    stage = Stage('ratchet')
    start = time.monotonic()
    baseline = json.loads(BASELINE.read_text()) if BASELINE.exists() else None
    comparison = progress.compare(baseline, snapshot) if baseline else None
    if comparison is None:
        stage.ok = True
        stage.summary = 'no baseline yet' + ('; this run is now the baseline' if update else '; --update-baseline makes this run the baseline')
    else:
        stage.ok = update or not comparison.regressions
        stage.summary = (f'{comparison.result} since the baseline ({baseline["commit"]}, {baseline["date"]}): '
                         f'{len(comparison.improvements)} better, {len(comparison.regressions)} worse')
        if update:
            stage.summary += '; this run is now the baseline'
        stage.details = ([f'WORSE  {r}' for r in comparison.regressions] +
                         [f'STUCK  {s}' for s in comparison.stuck] +
                         [f'better {i}' for i in comparison.improvements[:20]] +
                         ([f'better ... and {len(comparison.improvements) - 20} more'] if len(comparison.improvements) > 20 else []) +
                         [f'note   {n}' for n in comparison.notes])
    if update:
        accepted = snapshot
        if baseline and baseline.get('rocq') and snapshot.get('rocq'):
            # a stuck compilation is no position: keep the baseline's
            accepted = {**snapshot, 'rocq': {kind: baseline['rocq'][kind] if (snapshot['rocq'][kind] or {}).get('stuck')
                                             else snapshot['rocq'][kind] for kind in ('plain', 'probe')}}
        BASELINE.write_text(json.dumps(accepted, indent=1, sort_keys=True) + '\n')
    stage.seconds = time.monotonic() - start
    return stage, comparison, baseline


def previous_history_row(scope_name: str) -> Optional[dict[str, str]]:
    """The last line of history.csv for the same scope, if any."""
    if not HISTORY.exists():
        return None
    with open(HISTORY, newline='') as handle:
        rows = [row for row in csv.DictReader(handle) if row['scope'] == scope_name]
    return rows[-1] if rows else None


def append_history(result: metrics.Metrics, inputs: dict[str, Any], scope_name: str, outcome: str, identifier: str) -> None:
    row = {
        'timestamp': datetime.datetime.now().strftime('%Y-%m-%dT%H:%M:%S'),
        'commit': inputs['sail-backend']['commit'],
        'dirty': inputs['sail-backend']['dirty'],
        'scope': scope_name,
        'instructions total': result['instructions']['total'],
        'functions total': result['functions']['total'],
        **{key: ('' if value is None else value) for key, value in metrics.headline(result).items()},
        'rocq plain': rocq_summary(result, 'plain'),
        'rocq probe': rocq_summary(result, 'probe'),
        'result': outcome,
        'run': identifier,
    }
    exists = HISTORY.exists()
    with open(HISTORY, 'a', newline='') as handle:
        writer = csv.DictWriter(handle, fieldnames=HISTORY_FIELDS)
        if not exists:
            writer.writeheader()
        writer.writerow(row)


def rocq_summary(result: metrics.Metrics, kind: str) -> str:
    """Where a compilation stops, as file:line, or ok; empty if Rocq did not run."""
    compilations = (result.get('rocq') or {}).get(kind)
    if not compilations:
        return ''
    failed = next((c for c in compilations if not c['ok']), None)
    if failed is None:
        return 'ok'
    where = f'{failed["file"]}:{failed["error"]["line"] if failed["error"] else "?"}'
    return f'stuck {where}' if failed.get('stuck') else where


# --------------------------------------------------------------------------
# report
# --------------------------------------------------------------------------

def report(inputs: dict[str, Any], stages: list[Stage], result: Optional[metrics.Metrics], sandbox: Path,
           previous: Optional[dict[str, Any]] = None, baseline: Optional[dict[str, Any]] = None) -> str:
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
        lines.extend(metrics.format_metrics(result, previous, baseline))
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

def directory_argument(value: Optional[str], variable: str, flag: str, default: Optional[Path] = None) -> Path:
    value = value or os.environ.get(variable) or (str(default) if default else None)
    if not value:
        raise InputError(f'pass {flag} DIR or set {variable}')
    path = Path(value).expanduser().resolve()
    if not path.is_dir():
        raise InputError(f'{flag}: {path} is not a directory')
    return path


def parse_arguments(argv: Optional[list[str]]) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument('--cheriot-sail', help='cheriot-sail checkout (default: $CHERIOT_SAIL_DIR)')
    parser.add_argument('--case-study', help='case study directory (default: $CHERIOT_CASE_STUDY, or else case-studies/cheriot)')
    parser.add_argument('--update-baseline', action='store_true',
                        help='accept this run as the baseline (tests/cheriot/baseline.json), even if it is worse')
    parser.add_argument('--no-scope', action='store_true', help='translate and measure the whole model')
    parser.add_argument('--no-coq', action='store_true', help='skip the Rocq stage')
    parser.add_argument('--coq-stall', type=float, default=rocq.DEFAULT_STALL,
                        help=f'seconds one Rocq command may run before coqc is stopped (default: {rocq.DEFAULT_STALL:.0f})')
    parser.add_argument('--no-coq-cache', action='store_true', help='compile without the Rocq cache (ROCQ_CACHE)')
    parser.add_argument('--keep-dumps', action='store_true', help='keep the Sail AST dump of every rewrite')
    parser.add_argument('--output-dir', type=Path, default=DEFAULT_OUTPUT_DIR)
    return parser.parse_args(argv)


def main(argv: Optional[list[str]] = None) -> int:
    arguments = parse_arguments(argv)
    try:
        cheriot_sail = directory_argument(arguments.cheriot_sail, 'CHERIOT_SAIL_DIR', '--cheriot-sail')
        case_study = directory_argument(arguments.case_study, 'CHERIOT_CASE_STUDY', '--case-study', DEFAULT_CASE_STUDY)
    except InputError as error:
        print(f'run.py: {error}', file=sys.stderr)
        return 2

    try:
        stage, inputs, sources = check_inputs(cheriot_sail, case_study, not arguments.no_coq)
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
    result = snapshot = None
    in_scope, scope_dump = None, None
    if stages[-1].ok and not arguments.no_scope:
        stage, in_scope, scope_dump = compute_scope(sources, cheriot_sail, sandbox, inputs['sail'], DUMP_CACHE)
        stages.append(stage)
    if stages[-1].ok:
        stages.append(translate(sources, case_study, sandbox, in_scope is not None))
        dump = last_dump(sandbox / 'dump', arguments.keep_dumps)
        if stages[-1].ok:
            rocq_results = None
            if not arguments.no_coq:
                stage, rocq_results = compile_rocq(sandbox, arguments.coq_stall, not arguments.no_coq_cache)
                stages.append(stage)
            identity = {'commit': describe(inputs['sail-backend']), 'date': datetime.date.today().isoformat(),
                        'run': identifier, 'cheriot-sail': inputs['cheriot-sail']['commit'],
                        'scope': hashlib.sha256(SCOPE_FILE.read_bytes()).hexdigest()[:12]}
            stage, result, snapshot = analyse(sandbox, dump, rocq_results, in_scope, scope_dump, identity)
            stages.append(stage)

    baseline = previous = None
    if result is not None:
        scope_name = 'whole model' if arguments.no_scope else 'core'
        previous = previous_history_row(scope_name)
        outcome = ''
        if snapshot is not None:
            stage, comparison, baseline_snapshot = ratchet(snapshot, arguments.update_baseline)
            stages.append(stage)
            baseline = (baseline_snapshot or {}).get('headline')
            outcome = comparison.result if comparison else 'NO BASELINE'
            if arguments.update_baseline:
                outcome += ' (baseline updated)'
        append_history(result, inputs, scope_name, outcome, identifier)
        (sandbox / 'metrics.json').write_text(json.dumps(result, indent=2) + '\n')
    (sandbox / 'run.json').write_text(json.dumps(
        {'identifier': identifier, 'inputs': inputs, 'stages': [asdict(s) for s in stages]}, indent=2) + '\n')
    print(report(inputs, stages, result, sandbox, previous, baseline))
    return 0 if all(s.ok for s in stages) else 1


if __name__ == '__main__':
    sys.exit(main())
