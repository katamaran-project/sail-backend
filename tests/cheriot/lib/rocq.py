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
"""

from __future__ import annotations

import re
import subprocess
import time
from dataclasses import asdict, dataclass
from pathlib import Path
from typing import Any, Optional

from .extract import COQ_DEFINITION

FILES = ['base.v', 'machine.v']        # machine.v does Require Export base

PROBE_PRELUDE = [
    '(* Added by tests/cheriot: every NYI hole becomes an axiom. *)',
    'From Stdlib Require Import Lists.List.',
    'Import ListNotations.',
    'Axiom NYI : forall {A : Type}, list nat -> A.',
    '',
]

COQ_LOCATION = re.compile(r'^File "\.?/?([^"]+)", line (\d+), characters (\d+)-(\d+):')
IMPORT_START = re.compile(r'^\s*(From\s+\S+\s+)?(Require|Import|Export)\b')
FUN_PREFIX = re.compile(r'^fun_')


@dataclass
class CoqError:
    line: int                # in the original (not the probe) file
    message: str
    definition: str = ''     # the generated definition that contains the line ('' if none)
    kind: str = ''           # Definition, Inductive, ...
    before: int = 0          # definitions of the file that come before it
    total: int = 0           # definitions in the file


@dataclass
class Compilation:
    file: str
    exit: int                # coqc's exit code; -1 for a timeout
    seconds: float
    error: Optional[CoqError] = None

    @property
    def ok(self) -> bool:
        return self.exit == 0

    def as_json(self) -> dict[str, Any]:
        return {'file': self.file, 'ok': self.ok, 'exit': self.exit, 'seconds': round(self.seconds, 1),
                'error': asdict(self.error) if self.error else None}


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


def locate(text: str, line: int, message: str) -> CoqError:
    """A CoqError at `line` of the generated file `text`, with the definition that contains it."""
    lines = text.splitlines()
    starts = [(number, match.group(1), match.group(2))
              for number, source in enumerate(lines, start=1)
              if (match := COQ_DEFINITION.match(source))]
    containing = [start for start in starts if start[0] <= line]
    error = CoqError(line=line, message=message, before=max(0, len(containing) - 1), total=len(starts))
    if containing:
        _, error.kind, name = containing[-1]
        error.definition = FUN_PREFIX.sub('', name)
    return error


# --------------------------------------------------------------------------
# compiling
# --------------------------------------------------------------------------

def compile_file(coqc: str, directory: Path, name: str, timeout: float,
                 original: Optional[str] = None, inserted_at: int = 0, inserted: int = 0) -> Compilation:
    """
    Compile directory/name with coqc, writing its output to directory/coqc-<stem>.log.
    `original` is the text of the file whose lines errors refer to, for a probe copy.
    """
    log = directory / f'coqc-{Path(name).stem}.log'
    start = time.monotonic()
    try:
        completed = subprocess.run([coqc, name], cwd=directory, capture_output=True, text=True,
                                   errors='replace', timeout=timeout)
        exit_code, output = completed.returncode, completed.stdout + completed.stderr
    except subprocess.TimeoutExpired as expired:
        exit_code = -1
        output = f'{expired.stdout or ""}{expired.stderr or ""}\ncoqc timed out after {timeout:.0f} s\n'
    seconds = time.monotonic() - start
    log.write_text(output)
    compilation = Compilation(name, exit_code, seconds)
    if exit_code == 0:
        return compilation
    if exit_code == -1:
        compilation.error = CoqError(line=0, message=f'timed out after {timeout:.0f} s')
        return compilation
    found = first_error(output)
    text = original if original is not None else (directory / name).read_text(errors='replace')
    if found is None:
        compilation.error = CoqError(line=0, message=(output.strip().splitlines() or ['no output'])[-1][:400])
    else:
        _, line, message = found
        if original is not None:
            line = original_line(line, inserted_at, inserted)
        compilation.error = locate(text, line, message)
    return compilation


def compile_all(coqc: str, directory: Path, timeout: float) -> list[Compilation]:
    """Compile base.v, then machine.v if base.v compiled."""
    results = []
    for name in FILES:
        results.append(compile_file(coqc, directory, name, timeout))
        if not results[-1].ok:
            break
    return results


def probe_all(coqc: str, directory: Path, probe_directory: Path, timeout: float) -> list[Compilation]:
    """Compile probe copies of base.v and machine.v in probe_directory (base.v first, as above)."""
    probe_directory.mkdir(exist_ok=True)
    results = []
    for name in FILES:
        original = (directory / name).read_text(errors='replace')
        copy, inserted_at, inserted = probe_source(original)
        (probe_directory / name).write_text(copy)
        results.append(compile_file(coqc, probe_directory, name, timeout, original, inserted_at, inserted))
        if not results[-1].ok:
            break
    return results


def describe(compilations: list[dict[str, Any]]) -> str:
    """One line for the compilations of one kind, in their JSON form (Compilation.as_json)."""
    for compilation in compilations:
        if not compilation['ok']:
            error = compilation['error'] or {'line': 0, 'message': 'failed'}
            if not error['line']:
                return f'{compilation["file"]}: {error["message"]}'
            where = f' in {error["kind"]} {error["definition"]}' if error.get('definition') else ''
            share = f' ({error["before"]} of {error["total"]} definitions before it)' if error.get('total') else ''
            return f'{compilation["file"]}:{error["line"]}{where}{share}: {error["message"][:100]}'
    return ', '.join(c['file'] for c in compilations) + ' compile'
