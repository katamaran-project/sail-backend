#!/usr/bin/env python3
"""
Compile one .v file with coqc under a watchdog (lib/rocq.py), command by command.

Usage:
    tests/cheriot/coq-watch.py [--stall SECONDS] [--every SECONDS] [--coqc COQC] FILE.v

- Every --every seconds (default 30), a status line says how many commands are
  done and which command is running, with its line and how long it has run.
- If one command runs longer than --stall seconds (default 45), coqc is stopped
  and the command is reported as stuck (exit status 124).
- At the end, the slowest commands are listed.

coqc runs in the file's directory, with -time; its output (without the timing
lines) goes to coqc-<stem>.log next to the file.
"""

import argparse
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))

from lib import rocq     # noqa: E402

STUCK_EXIT = 124


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument('--stall', type=float, default=rocq.DEFAULT_STALL,
                        help=f'seconds one command may run (default {rocq.DEFAULT_STALL:.0f})')
    parser.add_argument('--every', type=float, default=30, help='seconds between status lines (default 30)')
    parser.add_argument('--coqc', default='coqc')
    parser.add_argument('file', type=Path)
    arguments = parser.parse_args()
    path = arguments.file.resolve()
    source = path.read_bytes()

    def status(done: int, offset: int, running: float) -> None:
        line, command = rocq.command_at(source, offset)
        print(f'{done} commands done; running for {running:.0f} s: line {line}: {command}', flush=True)

    watched = rocq.watch(arguments.coqc, path.parent, path.name, arguments.stall, status, arguments.every)
    (path.parent / f'coqc-{path.stem}.log').write_text(rocq.without_timings(watched.output))
    timings = sorted(watched.timings, reverse=True)[:5]
    if timings:
        print('slowest commands:')
        for seconds, offset in timings:
            line, command = rocq.command_at(source, offset)
            print(f'  {seconds:8.2f} s  line {line}: {command}')
    if watched.stuck is not None:
        offset, seconds = watched.stuck
        line, command = rocq.command_at(source, offset)
        print(f'STUCK: line {line} ran for more than {arguments.stall:.0f} s ({seconds:.0f} s), so coqc was stopped: {command}')
        return STUCK_EXIT
    found = rocq.first_error(watched.output)
    if found:
        print(f'error at line {found[1]}: {found[2]}')
    print(f'coqc exited with {watched.exit} after {watched.seconds:.0f} s; output in coqc-{path.stem}.log')
    return watched.exit


if __name__ == '__main__':
    sys.exit(main())
