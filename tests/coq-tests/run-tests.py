#!/usr/bin/env python3
"""Run the coq end-to-end tests.

A test is a directory next to this script that contains a model.sail.  Each
selected test is run in its own sandbox under the output directory, so that
the source tree is never touched, and every artifact the run produces --
the generated Coq, the intermediate rewrite dumps, the diffs between them,
and the combined stdout/stderr -- is archived there.

The sandbox for a run lives in

    <output-dir>/<timestamp>-<commit>[-dirty]/<test>/

with a copy of shared-configuration.lisp one level up, because every test's
configuration.lisp does `$include ../shared-configuration.lisp`.

Usage:
    ./run-tests.py                                   # run everything
    ./run-tests.py --test bit-returning-one          # run one test
    ./run-tests.py --include 'let-shadow-'           # run everything matching
    ./run-tests.py --include-file failures.txt       # run the tests listed
    ./run-tests.py --list                            # show the selection only
    ./run-tests.py --installed-plugin                # test the plugin installed in
                                                     # the opam switch instead

By default, the tests use the katamaran plugin built by `dune build` in this
checkout, not the one installed in the opam switch, so that several checkouts
or worktrees can be tested at the same time without overwriting each other's
installed plugin.

Compiling the generated Coq is the slow part of a test.  Its verdict depends
only on the generated base.v and machine.v and on the Coq installation, so it
is cached, keyed by those files and by a fingerprint of coqc and the
libraries it loads.  A test whose generated Coq did not change since an
earlier run reuses that run's coqc exits and output instead of running coqc
again.  --no-coq-cache turns this off.

The cache does not depend on the checkout, so all checkouts and worktrees
share one: by default $XDG_CACHE_HOME/sail-backend/coq-cache (that is,
~/.cache/sail-backend/coq-cache), or $SAIL_BACKEND_COQ_CACHE if set, or
--coq-cache-dir.  A new worktree therefore starts with a warm cache.
"""

import argparse
import concurrent.futures
import csv
import datetime
import difflib
import fnmatch
import hashlib
import json
import os
import re
import shutil
import subprocess
import sys
import time
from pathlib import Path


HERE = Path(__file__).resolve().parent
REINDENT_AST = HERE.parent / 'reindent-ast.py'
SHARED_CONFIGURATION = HERE / 'shared-configuration.lisp'

# One job per CPU.  Each coqc loads Katamaran and peaks at about 650 MB, so
# running many more jobs than CPUs only adds contention and memory pressure:
# on an 8-CPU, 16 GB machine, 20 jobs took 94 s for 80 tests and 8 jobs 54 s.
DEFAULT_JOBS = os.cpu_count() or 4
DEFAULT_OUTPUT_DIR = HERE / 'test-output'
DEFAULT_COQ_CACHE_DIR = Path(
    os.environ.get('SAIL_BACKEND_COQ_CACHE')
    or Path(os.environ.get('XDG_CACHE_HOME') or Path.home() / '.cache') / 'sail-backend' / 'coq-cache'
)
DEFAULT_PLUGIN = HERE.parent.parent / '_build' / 'default' / 'sail_plugin' / 'sail_plugin_katamaran.cmxs'

# Files a previous in-place run may have left in a test directory.  These are
# never copied into a sandbox: the sandbox must start from the inputs only.
GENERATED_PATTERNS = [
    '*.v', '*.vo', '*.vok', '*.vos', '*.glob', '*.aux', '.*.aux', '*.html',
    'rewrite-*', 'delta-*', 'intermediate_rewrite_*', '*.diff',
    '*.lem', '*.thy', 'z3_problems', 'output-*',
]

# ...except templates, which are inputs despite matching *.v and *.html.
INPUT_PATTERNS = ['*.template.*']

# Removed from the sandbox after the run unless --keep-vo is given.  These are
# Coq build products: ~8 MB per test, and useless once coqc has had its say.
BULK_PATTERNS = ['*.vo', '*.vok', '*.vos', '*.glob', '*.aux', '.*.aux']

REWRITE_DUMP_RE = re.compile(r'^intermediate_rewrite_(\d+)_(\w+)\.sail$')


# --------------------------------------------------------------------------
# discovery and selection
# --------------------------------------------------------------------------

def discover():
    """Return (tests, non_tests): directory names with and without model.sail.

    A directory only counts as a would-be test -- and so only gets reported --
    if it holds something test-shaped; that keeps unrelated directories out of
    the warning.
    """
    tests, non_tests = [], []

    for entry in sorted(os.listdir(HERE)):
        directory = HERE / entry
        if not directory.is_dir() or entry.startswith('.'):
            continue

        if (directory / 'model.sail').is_file():
            tests.append(entry)
            continue

        looks_test_shaped = any(
            (directory / name).exists()
            for name in ['configuration.lisp', 'base.template.v', 'base.v']
        )
        if looks_test_shaped:
            non_tests.append(entry)

    return tests, non_tests


def read_include_file(path):
    """One test name per line; blank lines and # comments are skipped."""
    names = []

    for line in Path(path).read_text().splitlines():
        line = line.split('#', 1)[0].strip()
        if line:
            names.append(line)

    return names


def select(tests, arguments):
    """Resolve the selection options against the discovered tests.

    The three inclusion options are unioned; --exclude is applied afterwards.
    Naming a test that does not exist is an error rather than a silent
    reduction of the selection, because it is nearly always a typo.
    """
    named = list(arguments.test)
    for path in arguments.include_file:
        named.extend(read_include_file(path))

    unknown = [name for name in named if name not in tests]
    if unknown:
        sys.exit(f"error: no such test: {', '.join(sorted(set(unknown)))}")

    if not named and not arguments.include:
        selected = set(tests)
    else:
        selected = set(named)
        for pattern in arguments.include:
            regex = re.compile(pattern)
            selected.update(name for name in tests if regex.search(name))

    for pattern in arguments.exclude:
        regex = re.compile(pattern)
        selected = {name for name in selected if not regex.search(name)}

    return sorted(selected)


# --------------------------------------------------------------------------
# git information for the run id
# --------------------------------------------------------------------------

def git_output(*arguments):
    """Run a git command, or return None if git or the repository is absent."""
    try:
        completed = subprocess.run(
            ['git', *arguments],
            cwd=HERE, capture_output=True, text=True,
        )
    except OSError:
        return None

    if completed.returncode != 0:
        return None

    return completed.stdout.strip()


def git_information():
    commit = git_output('rev-parse', '--short', 'HEAD') or 'unknown'
    status = git_output('status', '--porcelain')
    dirty = 'unknown' if status is None else bool(status)
    return commit, dirty


def run_identifier(commit, dirty):
    timestamp = datetime.datetime.now().strftime('%Y%m%dT%H%M%S')
    suffix = '-dirty' if dirty is True else ''
    return f'{timestamp}-{commit}{suffix}'


# --------------------------------------------------------------------------
# running a single test
# --------------------------------------------------------------------------

def is_input(name):
    if any(fnmatch.fnmatch(name, pattern) for pattern in INPUT_PATTERNS):
        return True
    return not any(fnmatch.fnmatch(name, pattern)
                   for pattern in GENERATED_PATTERNS)


def populate_sandbox(source, sandbox):
    """Copy a test's inputs into a fresh sandbox directory."""
    sandbox.mkdir(parents=True)

    for entry in sorted(os.listdir(source)):
        if not (source / entry).is_file() or not is_input(entry):
            continue
        shutil.copy2(source / entry, sandbox / entry)


def capture(command, sandbox, log, environment=None):
    """Run a command in the sandbox, appending its output to log.

    stdout and stderr are merged so that the interleaving is preserved, which
    matters when reading a failure after the fact.  environment, if given,
    holds variables to set on top of the inherited environment.
    """
    assignments = ''.join(f'{key}={value} ' for key, value in (environment or {}).items())
    log.append(f'$ {assignments}{" ".join(command)}\n')

    try:
        completed = subprocess.run(
            command, cwd=sandbox,
            env={**os.environ, **environment} if environment else None,
            stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
            text=True, errors='replace',
        )
    except OSError as error:
        log.append(f'failed to run {command[0]}: {error}\n')
        return 127

    log.append(completed.stdout)
    log.append(f'[exit {completed.returncode}]\n\n')
    return completed.returncode


def process_rewrites(sandbox):
    """Rename sail's rewrite dumps and diff each consecutive pair.

    The index is zero padded so that lexicographic order matches numeric
    order, which is what makes the pairing below correct.
    """
    dumps = []

    for entry in os.listdir(sandbox):
        match = REWRITE_DUMP_RE.match(entry)
        if not match:
            continue
        index, description = match.group(1), match.group(2)
        renamed = f'rewrite-{index.zfill(2)}-{description}.sail'
        (sandbox / entry).rename(sandbox / renamed)
        dumps.append(renamed)

    dumps.sort()

    for index, (before, after) in enumerate(zip(dumps, dumps[1:]), start=1):
        title = re.match(r'rewrite-\d+-(.*)\.sail', after).group(1)
        diff = difflib.unified_diff(
            (sandbox / before).read_text(errors='replace').splitlines(keepends=True),
            (sandbox / after).read_text(errors='replace').splitlines(keepends=True),
            fromfile=before, tofile=after,
        )
        name = f'delta-{str(index).zfill(2)}-{title}.diff'
        (sandbox / name).write_text(''.join(diff))


def write_reindented_ast(sandbox):
    """Run the captured output through reindent-ast.py, best effort.

    A failure here is not a test failure -- the raw output.txt is still there.
    """
    target = sandbox / 'output.ast.txt'

    try:
        with open(sandbox / 'output.txt') as source:
            completed = subprocess.run(
                [sys.executable, str(REINDENT_AST)],
                stdin=source, capture_output=True, text=True, errors='replace',
            )
    except OSError as error:
        target.write_text(f'could not run reindent-ast.py: {error}\n')
        return

    if completed.returncode == 0:
        target.write_text(completed.stdout)
    else:
        target.write_text(
            f'reindent-ast.py exited {completed.returncode}:\n{completed.stderr}'
        )


# --------------------------------------------------------------------------
# caching coqc verdicts
# --------------------------------------------------------------------------

COQ_SOURCES = ['base.v', 'machine.v']


def coq_fingerprint(coqc):
    """Identify the Coq installation that coqc compiles against.

    It covers coqc's version and the path, size and modification time of
    every compiled library (.vo) under `coqc -where` and the directories in
    ROCQPATH and COQPATH, so that reinstalling Katamaran, or any other
    library, invalidates the cache.  Returns None if coqc cannot be run.
    """
    try:
        version = subprocess.run([coqc, '--version'], capture_output=True, text=True, check=True).stdout
        where = subprocess.run([coqc, '-where'], capture_output=True, text=True, check=True).stdout.strip()
    except (OSError, subprocess.CalledProcessError):
        return None

    digest = hashlib.sha256()
    digest.update(f'{shutil.which(coqc) or coqc}\n{version}\n'.encode())
    roots = [where]
    for variable in ['ROCQPATH', 'COQPATH']:
        value = os.environ.get(variable, '')
        digest.update(f'{variable}={value}\n'.encode())
        roots.extend(entry for entry in value.split(os.pathsep) if entry)
    for root in roots:
        for directory, subdirectories, files in os.walk(root):
            subdirectories.sort()
            for name in sorted(files):
                if name.endswith('.vo'):
                    path = os.path.join(directory, name)
                    status = os.stat(path)
                    digest.update(f'{path} {status.st_size} {status.st_mtime_ns}\n'.encode())
    return digest.hexdigest()


def coq_cache_path(sandbox, arguments):
    """The cache entry for this sandbox's generated Coq, or None not to cache.

    Nothing is cached when caching is off, when the .vo files are to be kept
    (a cache hit produces none), or when sail produced no base.v.
    """
    if arguments.coq_fingerprint is None or arguments.keep_vo:
        return None
    if not (sandbox / 'base.v').is_file():
        return None
    digest = hashlib.sha256(arguments.coq_fingerprint.encode())
    for source in COQ_SOURCES:
        path = sandbox / source
        contents = path.read_bytes() if path.is_file() else b''
        digest.update(f'\n{source} {len(contents)}\n'.encode())
        digest.update(contents)
    return arguments.coq_cache_dir / f'{digest.hexdigest()}.json'


def read_coq_cache(path):
    if path is None:
        return None
    try:
        entry = json.loads(path.read_text())
    except (OSError, ValueError):
        return None
    if not isinstance(entry.get('coq_exits'), dict) or not isinstance(entry.get('log'), list):
        return None
    return entry


def write_coq_cache(path, coq_exits, coq_log):
    """Write the entry atomically, since tests run in parallel threads."""
    if path is None:
        return
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_name(f'{path.name}.{os.getpid()}.{id(coq_log)}.tmp')
    temporary.write_text(json.dumps({'coq_exits': coq_exits, 'log': coq_log}) + '\n')
    os.replace(temporary, path)


def run_coq(sandbox, arguments, log):
    """Compile the generated Coq, or reuse an earlier verdict for identical files.

    Returns the coqc exit code of each file it compiled, and whether the
    verdict came from the cache.
    """
    cache_path = coq_cache_path(sandbox, arguments)
    cached = read_coq_cache(cache_path)
    if cached is not None:
        log.append(f'(coqc not run: the generated Coq is identical to an earlier run\'s; '
                   f'its output is reused from {cache_path})\n')
        log.extend(cached['log'])
        return cached['coq_exits'], True

    coq_exits = {}
    coq_log = []
    # base.vo must exist before machine.v, which does `Require Export base`.
    for source in COQ_SOURCES:
        coq_exits[source] = capture([arguments.coqc, source], sandbox, coq_log)
        if coq_exits[source] != 0:
            break
    log.extend(coq_log)
    write_coq_cache(cache_path, coq_exits, coq_log)
    return coq_exits, False


def prune(sandbox):
    for entry in os.listdir(sandbox):
        if any(fnmatch.fnmatch(entry, pattern) for pattern in BULK_PATTERNS):
            (sandbox / entry).unlink()


def run_test(name, run_directory, arguments):
    """Run one test in its own sandbox and return its result record."""
    sandbox = run_directory / name
    started = time.monotonic()
    log = []

    populate_sandbox(HERE / name, sandbox)

    sail_command = [arguments.sail]
    sail_environment = None
    if arguments.plugin is not None:
        # SAIL_NO_PLUGINS stops sail from also auto-loading the katamaran plugin
        # installed in the opam switch, whose options would shadow this one's.
        sail_command += ['--plugin', str(arguments.plugin)]
        sail_environment = {'SAIL_NO_PLUGINS': '1'}
    if arguments.dump:
        sail_command += ['--ddump-rewrite-ast', 'intermediate']
    if arguments.monomorphise:
        sail_command += ['--auto-mono', '--mono-rewrites']
    sail_command += ['--katamaran',
                     '--katamaran-config', 'configuration.lisp',
                     'model.sail']

    # A sail failure is recorded but does not stop the test: the Coq stage is
    # what decides pass or fail, as it did when this was a shell script.
    sail_exit = capture(sail_command, sandbox, log, sail_environment)

    if arguments.dump:
        process_rewrites(sandbox)

    coq_exits = {}
    coq_cached = False
    if arguments.coq:
        coq_exits, coq_cached = run_coq(sandbox, arguments, log)

        # Both must have run and succeeded; if sail died there is no base.v to
        # compile, and coqc reports that as a failure of its own.
        passed = len(coq_exits) == 2 and all(code == 0 for code in coq_exits.values())
    else:
        passed = sail_exit == 0

    duration = time.monotonic() - started

    (sandbox / 'output.txt').write_text(''.join(log))
    write_reindented_ast(sandbox)

    result = {
        'name': name,
        'passed': passed,
        'sail_exit': sail_exit,
        'coq_exits': coq_exits,
        'coq_cached': coq_cached,
        'duration_s': round(duration, 2),
        'command': ' '.join(sail_command),
    }
    (sandbox / 'result.json').write_text(json.dumps(result, indent=2) + '\n')

    if not arguments.keep_vo:
        prune(sandbox)

    return result


# --------------------------------------------------------------------------
# reporting
# --------------------------------------------------------------------------

def write_summary(run_directory, results):
    failures = sorted(r['name'] for r in results if not r['passed'])
    passes = sum(1 for r in results if r['passed'])

    lines = [
        f'{"PASS" if r["passed"] else "FAIL"}  {r["duration_s"]:7.2f}s  {r["name"]}'
        for r in sorted(results, key=lambda r: r['name'])
    ]
    lines.append('')
    lines.append(f'PASS:{passes} FAIL:{len(failures)}')

    (run_directory / 'summary.txt').write_text('\n'.join(lines) + '\n')
    (run_directory / 'failures.txt').write_text(
        ''.join(f'{name}\n' for name in failures)
    )


def update_latest_symlink(output_directory, run_identifier):
    latest = output_directory / 'latest'

    try:
        if latest.is_symlink() or latest.exists():
            latest.unlink()
        latest.symlink_to(run_identifier)
    except OSError as error:
        print(f'note: could not update {latest}: {error}', file=sys.stderr)


def append_history(output_directory, commit, dirty, results, run_identifier):
    """Append to the historical record, keeping its existing schema."""
    path = output_directory / 'test-history.csv'
    passes = sum(1 for r in results if r['passed'])
    failures = len(results) - passes
    timestamp = datetime.datetime.now().strftime('%Y-%m-%dT%H:%M:%S+')

    exists = path.exists()

    with open(path, 'a', newline='') as file:
        writer = csv.writer(file)
        if not exists:
            writer.writerow(['timestamp', 'commit', 'files changed wrt commit',
                             'pass count', 'fail count', 'run'])
        writer.writerow([timestamp, commit, dirty, passes, failures,
                         run_identifier])


# --------------------------------------------------------------------------
# command line
# --------------------------------------------------------------------------

def resolve(flag, environment_variable, default):
    """A flag wins over its environment variable, which wins over the default."""
    if flag is not None:
        return flag
    if environment_variable in os.environ:
        return True
    return default


def parse_arguments(argv):
    parser = argparse.ArgumentParser(
        description='Run the coq end-to-end tests.',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=__doc__.split('Usage:', 1)[1].rstrip() if 'Usage:' in __doc__ else None,
    )

    selection = parser.add_argument_group('selecting tests')
    selection.add_argument('--test', action='append', default=[], metavar='NAME',
                           help='run this test; repeatable')
    selection.add_argument('--include', action='append', default=[], metavar='REGEX',
                           help='run tests whose name matches; repeatable')
    selection.add_argument('--include-file', action='append', default=[], metavar='FILE',
                           help='run the tests named in this file, one per line')
    selection.add_argument('--exclude', action='append', default=[], metavar='REGEX',
                           help='skip tests whose name matches; repeatable')
    selection.add_argument('--list', action='store_true',
                           help='print the selected tests and exit')
    selection.add_argument('--list-nontests', action='store_true',
                           help='print the directories that look like tests but have no model.sail')

    behaviour = parser.add_argument_group('what to run')
    behaviour.add_argument('--dump', action=argparse.BooleanOptionalAction, default=None,
                           help='dump the intermediate rewrite ASTs (default: on)')
    behaviour.add_argument('--monomorphise', action=argparse.BooleanOptionalAction, default=None,
                           help='pass --auto-mono --mono-rewrites (default: off; env MONOMO)')
    behaviour.add_argument('--coq', action=argparse.BooleanOptionalAction, default=None,
                           help='compile the generated Coq (default: on; env NO_COQ disables)')
    behaviour.add_argument('--coq-cache', action=argparse.BooleanOptionalAction, default=True,
                           help='reuse the coqc verdict of an earlier run whose generated Coq '
                                'was identical (default: on)')
    behaviour.add_argument('--coq-cache-dir', type=Path, default=DEFAULT_COQ_CACHE_DIR, metavar='DIR',
                           help='where the coqc verdicts are cached, shared by all checkouts '
                                '(default: %(default)s; env SAIL_BACKEND_COQ_CACHE)')

    output = parser.add_argument_group('output')
    output.add_argument('-j', '--jobs', type=int, default=DEFAULT_JOBS, metavar='N',
                        help=f'tests to run in parallel (default: {DEFAULT_JOBS})')
    output.add_argument('--output-dir', type=Path, default=DEFAULT_OUTPUT_DIR, metavar='DIR',
                        help='where to archive runs (default: %(default)s)')
    output.add_argument('--keep-vo', action='store_true',
                        help='keep the Coq build products (~8 MB per test)')

    tools = parser.add_argument_group('tools')
    tools.add_argument('--sail', default='sail', help='the sail binary (default: %(default)s)')
    tools.add_argument('--coqc', default='coqc', help='the Coq compiler (default: %(default)s)')
    plugins = tools.add_mutually_exclusive_group()
    plugins.add_argument('--plugin', type=Path, default=DEFAULT_PLUGIN, metavar='CMXS',
                         help='the katamaran plugin to load (default: the one built by '
                              'dune build in this checkout)')
    plugins.add_argument('--installed-plugin', dest='plugin', action='store_const', const=None,
                         help='use the katamaran plugin installed in the opam switch')

    arguments = parser.parse_args(argv)

    if arguments.plugin is not None:
        # sail runs inside the sandbox, so a relative path would not resolve there.
        arguments.plugin = arguments.plugin.resolve()
        if not arguments.plugin.is_file():
            parser.error(f'--plugin: no such file: {arguments.plugin} (run dune build first?)')

    arguments.dump = resolve(arguments.dump, 'DUMP', True)
    arguments.monomorphise = resolve(arguments.monomorphise, 'MONOMO', False)
    # NO_COQ is a negative variable, so its presence disables the Coq stage.
    arguments.coq = arguments.coq if arguments.coq is not None \
        else 'NO_COQ' not in os.environ

    if arguments.jobs < 1:
        parser.error('--jobs must be at least 1')

    arguments.coq_cache_dir = arguments.coq_cache_dir.expanduser().resolve()
    arguments.coq_fingerprint = None
    if arguments.coq and arguments.coq_cache:
        arguments.coq_fingerprint = coq_fingerprint(arguments.coqc)

    return arguments


def main(argv=None):
    arguments = parse_arguments(argv)

    tests, non_tests = discover()

    if arguments.list_nontests:
        for name in non_tests:
            print(name)
        return 0

    if non_tests:
        shown = ', '.join(non_tests[:3])
        more = '' if len(non_tests) <= 3 else ', ...'
        print(f'note: {len(non_tests)} directories look like tests but have no '
              f'model.sail (ignored): {shown}{more} '
              f'(use --list-nontests to see all)', file=sys.stderr)

    selected = select(tests, arguments)

    if arguments.list:
        for name in selected:
            print(name)
        return 0

    if not selected:
        print('no tests selected', file=sys.stderr)
        return 1

    commit, dirty = git_information()
    identifier = run_identifier(commit, dirty)
    run_directory = arguments.output_dir.resolve() / identifier
    run_directory.mkdir(parents=True)

    # Every test's configuration.lisp does `$include ../shared-configuration.lisp`,
    # so the shared configuration must sit one level above the sandboxes.
    shutil.copy2(SHARED_CONFIGURATION, run_directory / SHARED_CONFIGURATION.name)

    print(f'running {len(selected)} test(s) with {arguments.jobs} job(s)')
    print(f'output: {run_directory}')

    started = time.monotonic()
    results = []

    with concurrent.futures.ThreadPoolExecutor(max_workers=arguments.jobs) as pool:
        futures = {
            pool.submit(run_test, name, run_directory, arguments): name
            for name in selected
        }

        for future in concurrent.futures.as_completed(futures):
            name = futures[future]
            try:
                result = future.result()
            except Exception as error:                  # noqa: BLE001
                result = {'name': name, 'passed': False, 'sail_exit': None,
                          'coq_exits': {}, 'duration_s': 0.0,
                          'command': '', 'error': repr(error)}
            results.append(result)
            print(f'[{len(results)}/{len(selected)}] '
                  f'{"PASS" if result["passed"] else "FAIL"} {name}', flush=True)

    elapsed = time.monotonic() - started
    passes = sum(1 for r in results if r['passed'])
    coq_cache_hits = sum(1 for r in results if r.get('coq_cached'))

    write_summary(run_directory, results)
    append_history(arguments.output_dir.resolve(), commit, dirty, results, identifier)
    update_latest_symlink(arguments.output_dir.resolve(), identifier)

    (run_directory / 'run.json').write_text(json.dumps({
        'run': identifier,
        'argv': sys.argv[1:] if argv is None else list(argv),
        'commit': commit,
        'dirty': dirty,
        'dump': arguments.dump,
        'monomorphise': arguments.monomorphise,
        'coq': arguments.coq,
        'coq_cache_hits': coq_cache_hits,
        'jobs': arguments.jobs,
        'selected': selected,
        'pass_count': passes,
        'fail_count': len(results) - passes,
        'duration_s': round(elapsed, 2),
    }, indent=2) + '\n')

    print(f'PASS:{passes} FAIL:{len(results) - passes} in {elapsed:.1f}s')
    if arguments.coq_fingerprint is not None:
        print(f'coqc verdicts reused from the cache: {coq_cache_hits}/{len(results)}')
    print(f'output: {run_directory}')

    return 0 if passes == len(results) else 1


if __name__ == '__main__':
    sys.exit(main())
