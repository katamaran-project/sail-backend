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

Policy exclusions (the scope file's [policy-excluded] table; docs/cheriot-loop.md §4.2)
are definitions that in-scope code reaches but that are not wanted, such as printing,
tracing and virtual memory. The scope does not follow edges into them, so what only
they reach leaves the scope too. They are removed together with their uses, with the
configuration functions exclude-functions-as-no-op, exclude-functions-as-failure,
exclude-functions-as-unconstrained, exclude-functions-as-value and exclude-registers
(docs/usage.md): a call to an excluded function becomes the unit value, a failure, an
unconstrained value or a given Sail expression, a read of an excluded register an
unconstrained value, and a write to it a no-op.

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
from typing import Any

from .callgraph import CallGraph
from .dangling import DanglingCall

CONSTRUCTOR = re.compile(r'^\s*union\s+clause\s+ast\s*=\s*([A-Za-z0-9_]+)', re.M)
DECLARATION = re.compile(r'^val\s+([A-Za-z0-9_#]+)', re.M)

# What the uses of a policy-excluded definition become: the keys of the scope file's
# [policy-excluded] table, and the configuration function that each one generates.
POLICIES = {
    'no-op': 'exclude-functions-as-no-op',
    'failure': 'exclude-functions-as-failure',
    'unconstrained': 'exclude-functions-as-unconstrained',
    'value': 'exclude-functions-as-value',          # a table: function -> Sail expression
    'registers': 'exclude-registers',
}


@dataclass
class ScopeFile:
    """The contents of scope.toml."""
    instruction_files: list[str]            # relative to the cheriot-sail checkout
    roots: list[str]
    dispatchers: dict[str, str]             # dispatcher -> prefix of the function per instruction
    policy: dict[str, str] = field(default_factory=dict)    # policy-excluded definition -> key of POLICIES
    values: dict[str, str] = field(default_factory=dict)    # under the key value: function -> Sail expression

    @classmethod
    def load(cls, path: Path) -> 'ScopeFile':
        return cls.from_text(Path(path).read_text())

    @classmethod
    def from_text(cls, text: str) -> 'ScopeFile':
        data = tomllib.loads(text)
        table = data.get('policy-excluded', {})
        return cls(instruction_files=list(data['instruction-files']),
                   roots=list(data['roots']),
                   dispatchers=dict(data.get('dispatchers', {})),
                   policy=policy_exclusions(table),
                   values=dict(table.get('value', {})))


def policy_exclusions(table: dict[str, Any]) -> dict[str, str]:
    """
    The scope file's [policy-excluded] table as a map from each definition to its policy.
    Each key holds a list of names, except value, which holds a table from names to Sail
    expressions. Unknown keys, a definition under two keys, a value that is not a string,
    and an expression with a double quote (Slang strings have no escapes) are errors (ValueError).
    """
    unknown = sorted(set(table) - set(POLICIES))
    if unknown:
        raise ValueError(f'[policy-excluded] has unknown keys {", ".join(unknown)}; the keys are {", ".join(POLICIES)}')
    for name, expression in table.get('value', {}).items():
        if not isinstance(expression, str) or '"' in expression:
            raise ValueError(f'[policy-excluded] value for {name} must be a Sail expression without double quotes, '
                             f'got {expression!r}')
    result: dict[str, str] = {}
    for policy in POLICIES:
        for name in table.get(policy, []):
            if result.get(name, policy) != policy:
                raise ValueError(f'[policy-excluded] lists {name} under both {result[name]} and {policy}')
            result[name] = policy
    return result


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
    policy: dict[str, str] = field(default_factory=dict)    # policy-excluded definition -> key of POLICIES
    values: dict[str, str] = field(default_factory=dict)    # under the key value: function -> Sail expression

    def includes(self, name: str) -> bool:
        """
        Whether a definition with this name is in scope: in-scope nodes, and what they mention
        (types, constructors, ...), except excluded definitions, which a dispatcher or a
        caller of a policy-excluded definition mentions.
        """
        return name in self.nodes or (name in self.mentioned and name not in self.excluded)

    @property
    def excluded(self) -> set[str]:
        """The definitions outside the scope, policy exclusions included."""
        return self.excluded_functions | self.excluded_values | set(self.policy)

    def policy_counts(self) -> dict[str, int]:
        """The number of policy-excluded definitions per policy."""
        return {policy: sum(1 for p in self.policy.values() if p == policy) for policy in POLICIES}


def compute(graph: CallGraph, scope_file: ScopeFile, instruction_sources: dict[str, str],
            declared: frozenset[str] | set[str] = frozenset()) -> Scope:
    """
    The scope in `graph`. `instruction_sources` maps each of the scope file's
    instruction files to its text. `declared` holds the functions that the model declares
    with a val specification, externs included (see declarations); a policy-excluded
    definition that is neither in the graph nor declared is a warning (a typo, or a
    definition that a new version of the model renamed).
    """
    warnings = []
    policy = scope_file.policy
    unknown = sorted(name for name in policy if name not in graph.nodes and name not in declared)
    if unknown:
        warnings.append(f'policy-excluded definitions that the model does not define: {", ".join(unknown)}')
    wrong_kind = sorted(name for name, p in policy.items()
                        if name in graph.nodes and (p == 'registers') != (name in graph.registers))
    if wrong_kind:
        warnings.append(f'policy-excluded definitions under the wrong key (registers go under registers, '
                        f'functions under the other keys): {", ".join(wrong_kind)}')
    excluded_roots = sorted(set(scope_file.roots) & set(policy))
    if excluded_roots:
        warnings.append(f'roots that are policy-excluded, and so left out: {", ".join(excluded_roots)}')
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
        if target in policy:
            return True
        prefix = scope_file.dispatchers.get(source)
        return prefix is not None and target.startswith(prefix) and target not in dispatched

    roots = [root for root in [*scope_file.roots, *sorted(graph.registers), *sorted(dispatched)] if root not in policy]
    nodes = graph.reachable(roots, skip_edge)
    return Scope(instructions=instructions, nodes=nodes, mentioned=graph.mentioned(nodes),
                 excluded_functions=graph.functions - nodes - set(policy),
                 excluded_values=graph.values - nodes - set(policy),
                 dispatched=dispatched, dispatchers=dict(scope_file.dispatchers), warnings=warnings,
                 policy=dict(policy), values=dict(scope_file.values))


def declarations(dump: str) -> set[str]:
    """The functions that a Sail AST dump declares with a val specification (externs included)."""
    return set(DECLARATION.findall(dump))


def configuration(scope: Scope) -> str:
    """
    The Slang configuration that ignores the definitions outside the scope, and excludes the
    policy-excluded definitions together with their uses.
    """
    def names(identifiers: set[str]) -> str:
        return '\n'.join(f'   "{identifier}"' for identifier in sorted(identifiers))
    text = (f';; Generated by tests/cheriot/run.py from tests/cheriot/scope.toml:\n'
            f';; the {len(scope.excluded_functions)} functions and {len(scope.excluded_values)} values outside the scope.\n'
            f'(ignore-functions\n  \'(\n{names(scope.excluded_functions)}\n  ))\n'
            f'(ignore-values\n  \'(\n{names(scope.excluded_values)}\n  ))\n')
    if scope.policy:
        text += ';; The policy exclusions ([policy-excluded]): removed together with their uses.\n'
        for policy, function in POLICIES.items():
            selected = {name for name, p in scope.policy.items() if p == policy}
            if not selected:
                continue
            if policy == 'value':
                pairs = '\n'.join(f'   ("{name}" "{scope.values[name]}")' for name in sorted(selected))
                text += f'({function}\n  \'(\n{pairs}\n  ))\n'
            else:
                text += f'({function}\n  \'(\n{names(selected)}\n  ))\n'
    return text


def unexpected_calls(calls: list[DanglingCall], scope: Scope) -> list[str]:
    """
    Calls in machine.v to excluded definitions, other than those from a dispatcher to the
    function of an out-of-scope instruction. Each is a bug in the scope.
    """
    problems = []
    for call in calls:
        if call.callee in scope.policy:
            problems.append(f'{", ".join(call.callers)} still call{"s" if len(call.callers) == 1 else ""} '
                            f'{call.callee}, which is policy-excluded: the translation should have removed '
                            f'{"the call" if call.calls == 1 else "the calls"}')
            continue
        if call.callee not in scope.excluded:
            continue
        expected = {dispatcher for dispatcher, prefix in scope.dispatchers.items() if call.callee.startswith(prefix)}
        unexpected = sorted(set(call.callers) - expected)
        if unexpected:
            problems.append(f'{", ".join(unexpected)} call{"s" if len(unexpected) == 1 else ""} '
                            f'{call.callee}, which is outside the scope')
    return problems
