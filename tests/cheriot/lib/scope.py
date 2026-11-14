"""
The scope: which part of the model the loop translates and measures (docs/cheriot-loop.md §4).

tests/cheriot/scope.toml says which instructions are in scope (by the source file
that declares their constructors) and which other entry points always are. The
scope is everything the call graph reaches from those, and is computed rather
than listed by hand:

    roots       the scope file's roots, every register (registers cannot be
                ignored, so whatever their definitions use is needed), and the
                function that executes each in-scope instruction
    dispatchers functions that call one function per instruction, like execute,
                which calls execute_<C> for every constructor C of ast. From a
                dispatcher, only the calls for in-scope instructions are followed;
                otherwise every instruction would be in scope.

Functions and top-level values outside the scope are ignored with the
configuration functions ignore-functions and ignore-values. Types are not
excluded: a type that only out-of-scope code uses is still translated, which is
harmless, while a wrongly excluded one would break the translation.

A dispatcher still calls the functions of out-of-scope instructions, which are
then missing from machine.v. Those calls are expected; any other call into
excluded code means the scope is wrong (see unexpected_calls).
"""

from __future__ import annotations

import re
import tomllib
from dataclasses import dataclass, field
from pathlib import Path

from .callgraph import CallGraph
from .dangling import DanglingCall

CONSTRUCTOR = re.compile(r'^\s*union\s+clause\s+ast\s*=\s*([A-Za-z0-9_]+)', re.M)


@dataclass
class ScopeFile:
    """The contents of scope.toml."""
    instruction_files: list[str]            # relative to the cheriot-sail checkout
    roots: list[str]
    dispatchers: dict[str, str]             # dispatcher -> prefix of the function per instruction

    @classmethod
    def load(cls, path: Path) -> 'ScopeFile':
        data = tomllib.loads(Path(path).read_text())
        return cls(instruction_files=list(data['instruction-files']),
                   roots=list(data['roots']),
                   dispatchers=dict(data.get('dispatchers', {})))


def instruction_constructors(source: str) -> list[str]:
    """The constructors of ast that a Sail source file declares (union clause ast = C ...)."""
    return CONSTRUCTOR.findall(source)


@dataclass
class Scope:
    instructions: list[str]                 # in-scope constructors of ast
    nodes: set[str]                         # functions, values and registers in scope
    mentioned: set[str]                     # identifiers that in-scope definitions mention (types, constructors, ...)
    excluded_functions: set[str]
    excluded_values: set[str]
    dispatched: set[str]                    # the functions that dispatchers call for in-scope instructions
    dispatchers: dict[str, str] = field(default_factory=dict)
    warnings: list[str] = field(default_factory=list)

    def includes(self, name: str) -> bool:
        """
        Whether a definition with this name is in scope: in-scope nodes, and what they mention
        (types, constructors, ...), except excluded definitions, which a dispatcher mentions.
        """
        return name in self.nodes or (name in self.mentioned and name not in self.excluded)

    @property
    def excluded(self) -> set[str]:
        return self.excluded_functions | self.excluded_values


def compute(graph: CallGraph, scope_file: ScopeFile, instruction_sources: dict[str, str]) -> Scope:
    """
    The scope in `graph`. `instruction_sources` maps each of the scope file's
    instruction files to its text.
    """
    warnings = []
    instructions = []
    for name in scope_file.instruction_files:
        found = instruction_constructors(instruction_sources[name])
        if not found:
            warnings.append(f'{name} declares no constructors of ast')
        instructions += found
    dispatched = set()
    for dispatcher, prefix in scope_file.dispatchers.items():
        if dispatcher not in graph.functions:
            warnings.append(f'dispatcher {dispatcher} is not a function of the model')
        for constructor in instructions:
            if prefix + constructor in graph.functions:
                dispatched.add(prefix + constructor)
            else:
                warnings.append(f'no function {prefix + constructor} for instruction {constructor}')
    missing_roots = [root for root in scope_file.roots if root not in graph.nodes]
    if missing_roots:
        warnings.append(f'roots that are not definitions of the model: {", ".join(missing_roots)}')

    def skip_edge(source: str, target: str) -> bool:
        prefix = scope_file.dispatchers.get(source)
        return prefix is not None and target.startswith(prefix) and target not in dispatched

    roots = [*scope_file.roots, *sorted(graph.registers), *sorted(dispatched)]
    nodes = graph.reachable(roots, skip_edge)
    return Scope(instructions=instructions, nodes=nodes, mentioned=graph.mentioned(nodes),
                 excluded_functions=graph.functions - nodes, excluded_values=graph.values - nodes,
                 dispatched=dispatched, dispatchers=dict(scope_file.dispatchers), warnings=warnings)


def configuration(scope: Scope) -> str:
    """The Slang configuration that ignores the definitions outside the scope."""
    def names(identifiers: set[str]) -> str:
        return '\n'.join(f'   "{identifier}"' for identifier in sorted(identifiers))
    return (f';; Generated by tests/cheriot/run.py from tests/cheriot/scope.toml:\n'
            f';; the {len(scope.excluded_functions)} functions and {len(scope.excluded_values)} values outside the scope.\n'
            f'(ignore-functions\n  \'(\n{names(scope.excluded_functions)}\n  ))\n'
            f'(ignore-values\n  \'(\n{names(scope.excluded_values)}\n  ))\n')


def unexpected_calls(calls: list[DanglingCall], scope: Scope) -> list[str]:
    """
    Calls in machine.v to excluded definitions, other than those from a dispatcher to the
    function of an out-of-scope instruction. Each is a bug in the scope.
    """
    problems = []
    for call in calls:
        if call.callee not in scope.excluded:
            continue
        expected = {dispatcher for dispatcher, prefix in scope.dispatchers.items() if call.callee.startswith(prefix)}
        unexpected = sorted(set(call.callers) - expected)
        if unexpected:
            problems.append(f'{", ".join(unexpected)} call{"s" if len(unexpected) == 1 else ""} '
                            f'{call.callee}, which is outside the scope')
    return problems
