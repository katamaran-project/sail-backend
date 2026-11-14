"""
Compiling the generated Rocq, and where it stops.

Two compilations are useful while the translation is incomplete:

    plain   coqc base.v, then machine.v, as the end-to-end tests do. It stops at
            the first NYI hole, which Rocq does not know.
    probe   copies of base.v and machine.v in which NYI is an axiom
            (Axiom NYI : forall {A : Type}, list nat -> A), so that every hole
            type-checks. Its first error is the first *real* problem beyond the
            holes; that is the progress signal until base.v compiles.

The first error of a compilation is mapped to the generated definition that
contains it, and to how many of the file's definitions come before it.
Probe line numbers are mapped back to the lines of the original file.

coqc runs under a watchdog (watch): with -time, coqc reports each command as
soon as it finishes, so the command that is running is the one after the last
reported one. A command that runs longer than the stall limit stops coqc; the
compilation is then *stuck* (Compilation.stuck), which is not an error of the
generated Rocq but a command too slow to wait for. The per-command timings give
the slowest commands of each compilation.

Saving time (compile_both):
- a file without NYI compiles the same way in its probe copy, so the probe
  reuses the plain result (and base.vo) instead of compiling it again;
- the plain and probe compilations of a file that are both needed run in parallel;
- verdicts and compiled base.vo files are cached (Cache), keyed by the file's
  contents, the files it depends on, and a fingerprint of the Rocq installation.
"""

from __future__ import annotations

import hashlib
import json
import os
import re
import shutil
import signal
import subprocess
import threading
import time
from concurrent.futures import ThreadPoolExecutor
from dataclasses import asdict, dataclass, field
from pathlib import Path
from typing import Any, Callable, Optional

from .extract import COQ_DEFINITION

FILES = ['base.v', 'machine.v']        # machine.v does Require Export base

PROBE_PRELUDE = [
    '(* Added by tests/cheriot: every NYI hole becomes an axiom. *)',
    'From Stdlib Require Import Lists.List.',
    'Import ListNotations.',
    'Axiom NYI : forall {A : Type}, list nat -> A.',
    '',
]

DEFAULT_STALL = 45.0                   # seconds one command may run
SLOWEST = 10                           # slowest commands kept per compilation

COQ_LOCATION = re.compile(r'^File "\.?/?([^"]+)", line (\d+), characters (\d+)-(\d+):')
IMPORT_START = re.compile(r'^\s*(From\s+\S+\s+)?(Require|Import|Export)\b')
FUN_PREFIX = re.compile(r'^fun_')
TIMING = re.compile(rb'^Chars (\d+) - (\d+) \[.*\] ([\d.]+) secs')
NYI_TOKEN = re.compile(r'\bNYI\b')


@dataclass
class CoqError:
    line: int                # in the original (not the probe) file
    message: str
    definition: str = ''     # the generated definition that contains the line ('' if none)
    kind: str = ''           # Definition, Inductive, ...
    before: int = 0          # definitions of the file that come before it
    total: int = 0           # definitions in the file
    accepted: list[str] = field(default_factory=list)      # the names of the definitions before it
    definitions: list[str] = field(default_factory=list)   # the names of all definitions in the file


@dataclass
class Timing:
    """One command of a compilation and how long it took."""
    seconds: float
    line: int                # in the original file
    command: str             # its first line
    definition: str = ''     # the generated definition that contains it


@dataclass
class Stuck:
    """The command that ran longer than the stall limit, so that coqc was stopped."""
    line: int
    command: str
    seconds: float           # how long it had run
    limit: float
    definition: str = ''


@dataclass
class Compilation:
    file: str
    exit: int                # coqc's exit code; -1 when the watchdog stopped coqc
    seconds: float
    error: Optional[CoqError] = None
    stuck: Optional[Stuck] = None
    slowest: list[Timing] = field(default_factory=list)
    cached: bool = False     # the verdict came from the cache
    reused: bool = False     # a probe result that is the plain one (the file has no NYI)

    @property
    def ok(self) -> bool:
        return self.exit == 0

    def as_json(self) -> dict[str, Any]:
        return {'file': self.file, 'ok': self.ok, 'exit': self.exit, 'seconds': round(self.seconds, 1),
                'error': asdict(self.error) if self.error else None,
                'stuck': asdict(self.stuck) if self.stuck else None,
                'slowest': [asdict(t) for t in self.slowest],
                'cached': self.cached, 'reused': self.reused}

    @staticmethod
    def from_json(data: dict[str, Any]) -> Compilation:
        return Compilation(data['file'], data['exit'], data['seconds'],
                           error=CoqError(**data['error']) if data.get('error') else None,
                           stuck=Stuck(**data['stuck']) if data.get('stuck') else None,
                           slowest=[Timing(**t) for t in data.get('slowest', [])],
                           cached=data.get('cached', False), reused=data.get('reused', False))


# --------------------------------------------------------------------------
# probe copies
# --------------------------------------------------------------------------

def import_end(lines: list[str]) -> int:
    """The index of the first line after the leading import statements (and blank lines)."""
    index, inside = 0, False
    while index < len(lines):
        line = lines[index]
        if inside:
            inside = not line.rstrip().endswith('.')
        elif IMPORT_START.match(line):
            inside = not line.rstrip().endswith('.')
        elif line.strip():
            break
        index += 1
    return index


def probe_source(text: str) -> tuple[str, int, int]:
    """
    The probe copy of a generated .v file: PROBE_PRELUDE inserted after its imports.
    Returns the copy, the 1-based line where the prelude starts and the number of lines inserted.
    """
    lines = text.split('\n')
    at = import_end(lines)
    return '\n'.join(lines[:at] + PROBE_PRELUDE + lines[at:]), at + 1, len(PROBE_PRELUDE)


def original_line(line: int, inserted_at: int, inserted: int) -> int:
    """A line of a probe copy, as a line of the original file."""
    if line < inserted_at:
        return line
    return max(inserted_at, line - inserted)


def has_nyi(text: str) -> bool:
    """Whether a generated file mentions NYI (if not, its probe copy compiles as the file does)."""
    return NYI_TOKEN.search(text) is not None


# --------------------------------------------------------------------------
# errors
# --------------------------------------------------------------------------

def first_error(log: str) -> Optional[tuple[str, int, str]]:
    """The file, line and message of the first error in coqc's output, if there is one."""
    lines = log.splitlines()
    for index, line in enumerate(lines):
        match = COQ_LOCATION.match(line)
        if match and index + 1 < len(lines) and lines[index + 1].startswith('Error'):
            message = [lines[index + 1][len('Error:'):].strip()]
            for follow in lines[index + 2:index + 8]:
                if COQ_LOCATION.match(follow) or not follow.strip():
                    break
                message.append(follow.strip())
            return match.group(1), int(match.group(2)), ' '.join(m for m in message if m)[:400]
    return None


def definition_starts(text: str) -> list[tuple[int, str, str]]:
    """The line, kind and name of each generated definition in `text`."""
    return [(number, match.group(1), FUN_PREFIX.sub('', match.group(2)))
            for number, source in enumerate(text.splitlines(), start=1)
            if (match := COQ_DEFINITION.match(source))]


def containing_definition(starts: list[tuple[int, str, str]], line: int) -> str:
    """The name of the definition that contains `line`, or ''."""
    containing = [name for number, _, name in starts if number <= line]
    return containing[-1] if containing else ''


def locate(text: str, line: int, message: str) -> CoqError:
    """A CoqError at `line` of the generated file `text`, with the definition that contains it."""
    starts = definition_starts(text)
    containing = [start for start in starts if start[0] <= line]
    names = [name for _, _, name in starts]
    before = max(0, len(containing) - 1)
    error = CoqError(line=line, message=message, before=before, total=len(starts),
                     accepted=names[:before], definitions=names)
    if containing:
        _, error.kind, error.definition = containing[-1]
    return error


# --------------------------------------------------------------------------
# the watchdog
# --------------------------------------------------------------------------

def command_at(source: bytes, offset: int) -> tuple[int, str]:
    """
    The line number (1-based) and first line of the command at byte offset `offset` of `source`
    (coqc -time reports byte offsets, which differ from character offsets in files with non-ASCII names).
    """
    while offset < len(source) and source[offset:offset + 1] in b' \t\r\n':
        offset += 1
    line = source.count(b'\n', 0, offset) + 1
    end = source.find(b'\n', offset)
    text = source[offset:end if end != -1 else len(source)].decode(errors='replace').strip()
    return line, text[:100]


@dataclass
class Watched:
    """What watch saw: coqc's exit code and output, the commands' timings and where it got stuck."""
    exit: int
    output: str
    seconds: float
    timings: list[tuple[float, int]]                   # (seconds, byte offset of the command)
    stuck: Optional[tuple[int, float]] = None          # (byte offset of the command, seconds it ran)


def stop(process: subprocess.Popen) -> None:
    """Stop a process started in its own session, with everything it started."""
    for sent in (signal.SIGTERM, signal.SIGKILL):
        try:
            os.killpg(process.pid, sent)
        except ProcessLookupError:
            return
        try:
            process.wait(timeout=10)
            return
        except subprocess.TimeoutExpired:
            continue


def watch(coqc: str, directory: Path, name: str, stall: float,
          status: Optional[Callable[[int, int, float], None]] = None, every: float = 30.0) -> Watched:
    """
    Run `coqc -time name` in `directory`, stopping it when one command runs longer than `stall`
    seconds. `status`, if given, is called every `every` seconds with the number of commands done,
    the byte offset where the running command starts and how long it has been running.
    """
    state = {'done': 0, 'end': 0, 'since': time.monotonic()}
    timings: list[tuple[float, int]] = []
    chunks: list[bytes] = []
    lock = threading.Lock()
    start = time.monotonic()
    process = subprocess.Popen([coqc, '-time', name], cwd=directory, start_new_session=True,
                               stdout=subprocess.PIPE, stderr=subprocess.STDOUT)

    def read() -> None:
        assert process.stdout is not None
        for raw in process.stdout:
            match = TIMING.match(raw)
            with lock:
                chunks.append(raw)
                if match:
                    state['done'] += 1
                    state['end'] = int(match.group(2))
                    state['since'] = time.monotonic()
                    timings.append((float(match.group(3)), int(match.group(1))))

    reader = threading.Thread(target=read, daemon=True)
    reader.start()
    stuck = None
    last_status = start
    try:
        while process.poll() is None:
            time.sleep(0.1)
            now = time.monotonic()
            with lock:
                running, done, end = now - state['since'], state['done'], state['end']
            if running > stall:
                stuck = (end, running)
                stop(process)
                break
            if status is not None and now - last_status >= every:
                status(done, end, running)
                last_status = now
    except BaseException:
        stop(process)          # coqc runs in its own session, so an interrupt does not reach it
        raise
    process.wait()
    reader.join(timeout=5)
    with lock:
        output = b''.join(chunks).decode(errors='replace')
    return Watched(-1 if stuck else process.returncode, output, time.monotonic() - start, list(timings), stuck)


def without_timings(output: str) -> str:
    """coqc's output without the -time lines."""
    return ''.join(line for line in output.splitlines(keepends=True) if not line.startswith('Chars '))


# --------------------------------------------------------------------------
# the cache
# --------------------------------------------------------------------------

def coq_fingerprint(coqc: str) -> Optional[str]:
    """
    Identify the Rocq installation that coqc compiles against: coqc's version and the path, size
    and modification time of every compiled library (.vo) under `coqc -where`, ROCQPATH and COQPATH,
    so that reinstalling Katamaran, or any other library, changes it. None if coqc cannot be run.
    (The same fingerprint as tests/coq-tests/run-tests.py's.)
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


class Cache:
    """
    Compilation verdicts, and the .vo of each successful one, in `directory`:
    <key>.json and <key>.vo. Only the newest `keep` entries are kept.
    A stuck compilation is not a verdict, so it is not cached.
    """

    def __init__(self, directory: Path, fingerprint: str, keep: int = 40):
        self.directory, self.fingerprint, self.keep = directory, fingerprint, keep

    def key(self, name: str, source: bytes, dependencies: list[str]) -> str:
        digest = hashlib.sha256()
        digest.update(f'{self.fingerprint}\n{name}\n{" ".join(dependencies)}\n'.encode())
        digest.update(source)
        return digest.hexdigest()[:32]

    def get(self, key: str) -> Optional[tuple[dict[str, Any], Optional[Path]]]:
        entry = self.directory / f'{key}.json'
        try:
            data = json.loads(entry.read_text())
        except (OSError, ValueError):
            return None
        vo = self.directory / f'{key}.vo'
        if data['compilation']['exit'] == 0 and not vo.exists():
            return None
        entry.touch()
        return data, (vo if vo.exists() else None)

    def put(self, key: str, compilation: Compilation, log: str, vo: Optional[Path]) -> None:
        try:
            self.directory.mkdir(parents=True, exist_ok=True)
            if vo is not None:
                shutil.copyfile(vo, self.directory / f'{key}.vo.tmp')
                os.replace(self.directory / f'{key}.vo.tmp', self.directory / f'{key}.vo')
            (self.directory / f'{key}.json.tmp').write_text(json.dumps({'compilation': compilation.as_json(), 'log': log}))
            os.replace(self.directory / f'{key}.json.tmp', self.directory / f'{key}.json')
            self.prune()
        except OSError:
            pass

    def prune(self) -> None:
        entries = sorted(self.directory.glob('*.json'), key=lambda p: p.stat().st_mtime, reverse=True)
        for entry in entries[self.keep:]:
            for path in (entry, entry.with_suffix('.vo')):
                path.unlink(missing_ok=True)


# --------------------------------------------------------------------------
# compiling
# --------------------------------------------------------------------------

def compile_file(coqc: str, directory: Path, name: str, stall: float = DEFAULT_STALL,
                 original: Optional[str] = None, inserted_at: int = 0, inserted: int = 0,
                 cache: Optional[Cache] = None, dependencies: Optional[list[str]] = None) -> tuple[Compilation, str]:
    """
    Compile directory/name with coqc under the watchdog, writing its output to directory/coqc-<stem>.log.
    `original` is the text of the file whose lines errors refer to, for a probe copy.
    With a cache, the verdict (and the .vo) may come from it; `dependencies` are the keys of the
    compilations whose .vo files this one loads. Returns the compilation and its cache key.
    """
    log = directory / f'coqc-{Path(name).stem}.log'
    vo = directory / Path(name).with_suffix('.vo').name
    source = (directory / name).read_bytes()
    key = cache.key(name, source, dependencies or []) if cache else ''
    if cache is not None and (hit := cache.get(key)) is not None:
        data, cached_vo = hit
        log.write_text(data['log'])
        if cached_vo is not None:
            shutil.copyfile(cached_vo, vo)
        compilation = Compilation.from_json(data['compilation'])
        compilation.cached = True
        return compilation, key

    watched = watch(coqc, directory, name, stall)
    output = without_timings(watched.output)
    text = original if original is not None else source.decode(errors='replace')
    starts = definition_starts(text)

    def to_original(line: int) -> int:
        return original_line(line, inserted_at, inserted) if original is not None else line

    def timing(seconds: float, offset: int) -> Timing:
        line, command = command_at(source, offset)
        line = to_original(line)
        return Timing(round(seconds, 2), line, command, containing_definition(starts, line))

    log.write_text(output + (f'\ncoqc was stopped: one command ran longer than {stall:.0f} s\n' if watched.stuck else ''))
    compilation = Compilation(name, watched.exit, watched.seconds,
                              slowest=[timing(s, o) for s, o in sorted(watched.timings, reverse=True)[:SLOWEST]])
    if watched.stuck is not None:
        offset, seconds = watched.stuck
        line, command = command_at(source, offset)
        line = to_original(line)
        compilation.stuck = Stuck(line, command, round(seconds, 1), stall, containing_definition(starts, line))
        compilation.error = CoqError(line=line, message=f'stuck: {command} ran longer than {stall:.0f} s',
                                     definition=compilation.stuck.definition)
        return compilation, key
    if watched.exit != 0:
        found = first_error(output)
        if found is None:
            compilation.error = CoqError(line=0, message=(output.strip().splitlines() or ['no output'])[-1][:400])
        else:
            compilation.error = locate(text, to_original(found[1]), found[2])
    if cache is not None:
        cache.put(key, compilation, output, vo if compilation.ok and vo.exists() else None)
    return compilation, key


def compile_both(coqc: str, directory: Path, probe_directory: Path, stall: float = DEFAULT_STALL,
                 cache: Optional[Cache] = None) -> dict[str, list[Compilation]]:
    """
    The plain compilations (base.v, then machine.v if base.v compiled) in `directory`, and the probe
    ones in `probe_directory`, as {'plain': [...], 'probe': [...]}. A file without NYI is compiled
    once: its probe result is the plain one (Compilation.reused) as long as everything before it was
    too. Plain and probe compilations of the same file run in parallel.
    """
    probe_directory.mkdir(exist_ok=True)
    results: dict[str, list[Compilation]] = {'plain': [], 'probe': []}
    keys: dict[str, list[str]] = {'plain': [], 'probe': []}
    probe_follows_plain = True
    for name in FILES:
        original = (directory / name).read_text(errors='replace')
        copy, inserted_at, inserted = probe_source(original)
        (probe_directory / name).write_text(copy)
        alive = {kind: all(c.ok for c in results[kind]) for kind in results}
        reuse = probe_follows_plain and alive['plain'] and not has_nyi(original)
        jobs: dict[str, Callable[[], tuple[Compilation, str]]] = {}
        if alive['plain']:
            jobs['plain'] = lambda: compile_file(coqc, directory, name, stall, cache=cache,
                                                 dependencies=list(keys['plain']))
        if alive['probe'] and not reuse:
            jobs['probe'] = lambda: compile_file(coqc, probe_directory, name, stall, original, inserted_at, inserted,
                                                 cache=cache, dependencies=list(keys['probe']))
        if not jobs:
            break
        with ThreadPoolExecutor(max_workers=len(jobs)) as pool:
            futures = {kind: pool.submit(job) for kind, job in jobs.items()}
            done = {kind: future.result() for kind, future in futures.items()}
        for kind, (compilation, key) in done.items():
            results[kind].append(compilation)
            keys[kind].append(key)
        if reuse:
            compilation, key = done['plain']
            vo = directory / Path(name).with_suffix('.vo').name
            if compilation.ok and vo.exists():
                shutil.copyfile(vo, probe_directory / vo.name)
            shutil.copyfile(directory / f'coqc-{Path(name).stem}.log', probe_directory / f'coqc-{Path(name).stem}.log')
            results['probe'].append(Compilation.from_json({**compilation.as_json(), 'reused': True}))
            keys['probe'].append(key)
        else:
            probe_follows_plain = False
    return results


def describe(compilations: list[dict[str, Any]]) -> str:
    """One line for the compilations of one kind, in their JSON form (Compilation.as_json)."""
    for compilation in compilations:
        if not compilation['ok']:
            stuck = compilation.get('stuck')
            if stuck:
                where = f' in {stuck["definition"]}' if stuck.get('definition') else ''
                return (f'{compilation["file"]}:{stuck["line"]}{where}: STUCK, coqc stopped after '
                        f'{stuck["seconds"]:.0f} s on one command (limit {stuck["limit"]:.0f} s): {stuck["command"][:60]}')
            error = compilation['error'] or {'line': 0, 'message': 'failed'}
            if not error['line']:
                return f'{compilation["file"]}: {error["message"]}'
            where = f' in {error["kind"]} {error["definition"]}' if error.get('definition') else ''
            share = f' ({error["before"]} of {error["total"]} definitions before it)' if error.get('total') else ''
            return f'{compilation["file"]}:{error["line"]}{where}{share}: {error["message"][:100]}'
    return ', '.join(c['file'] for c in compilations) + ' compile'


def slowest(compilations: list[dict[str, Any]], count: int = 5) -> list[dict[str, Any]]:
    """The slowest commands of the compilations of one kind (JSON form), with their file, slowest first."""
    found = [{'file': c['file'], **t} for c in compilations if not c.get('reused') for t in c.get('slowest', [])]
    return sorted(found, key=lambda t: -t['seconds'])[:count]


def is_stuck(compilations: Optional[list[dict[str, Any]]]) -> bool:
    return any(c.get('stuck') for c in compilations or [])
