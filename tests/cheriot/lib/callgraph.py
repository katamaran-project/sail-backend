"""
An approximate call graph of the model, and which definitions are reachable.

The graph is built from a Sail AST dump after the rewrites (Sail's
`--ddump-rewrite-ast PREFIX` writes PREFIX_rewrite_N_<name>.sail after rewrite N).
Top-level definitions in the dump are separated by blank lines. The nodes of the
graph are the functions, the top-level values (let) and the registers. There is
an edge d -> e when the name of node e occurs as an identifier in the definition
of d. Names of other definitions, such as types, count as reachable when a
reachable node mentions them.

`mark_reachable` adds a column `reachable` to items: `step` when the item's
definition is reachable from the fetch/decode/execute loop, `execute` when it is
reachable only from the instruction semantics and decoders, else empty.
"""

from __future__ import annotations

import re
from collections import deque
from pathlib import Path
from typing import Callable, Iterable

from .items import Item

STEP_ROOTS = ['step', 'loop', 'main', 'init_model', 'init_sys', 'ext_init', 'ext_init_regs', 'init_platform']
EXECUTE_ROOTS = ['execute', 'ext_decode', 'ext_decode_compressed', 'encdec_backwards',
                 'encdec_compressed_backwards', 'fetch']

# Comments (/* ... */, including /*! doc comments */, and // lines) and attributes ($[complete]) before a definition.
ATTRIBUTES = r'\s*(?:/\*(?s:.*?)\*/\s*|//[^\n]*\s*)*(?:\$\[[^\]]*\]\s*)*'
FUNCTION_HEAD = re.compile(ATTRIBUTES + r'function\s+(?:clause\s+)?(operator\s+[^\s(]+|[A-Za-z0-9_#]+)')
VALUE_HEAD = re.compile(ATTRIBUTES + r'let\s+\(?([A-Za-z0-9_#]+)')
REGISTER_HEAD = re.compile(ATTRIBUTES + r'register\s+([A-Za-z0-9_#]+)')
HEADS = [(FUNCTION_HEAD, 'function'), (VALUE_HEAD, 'value'), (REGISTER_HEAD, 'register')]
IDENTIFIER = re.compile(r'operator\s+[^\s(]+|[A-Za-z_][A-Za-z0-9_#]*')

# How the translator names an operator such as `operator <_s` (Sail.mangle_sail_operator_name, applied to
# Sail's internal form of the operator, in which each character of a part is set between bars: |<|_|s|).
OPERATOR_CHARACTERS = {'!': 'excl', '%': 'pct', '&': 'amp', '*': 'star', '+': 'plus', '-': 'min', '.': 'dot',
                       '/': 'slash', ':': 'colon', '<': 'lt', '>': 'gt', '=': 'eq', '@': 'at', '^': 'caret',
                       '|': 'bar', '#': 'hash'}


def operator_name(operator: str) -> str:
    """The translator's name for a Sail operator: operator_barltbar_barsbar for <_s."""
    internal = '_'.join('|' + '|'.join(part) + '|' for part in operator.split('_'))
    return 'operator_' + ''.join(OPERATOR_CHARACTERS.get(character, character) for character in internal)


def canonical(identifier: str) -> str:
    """An identifier as the translator names it: operators are mangled, other names stay."""
    if identifier.startswith('operator') and identifier[len('operator'):len('operator') + 1].isspace():
        return operator_name(identifier.split(None, 1)[1])
    return identifier
DUMP_NAME = re.compile(r'_rewrite_(\d+)_[^/]*\.sail$')


def last_dump(directory: Path) -> Path:
    """The dump written after the last rewrite, among the *_rewrite_N_*.sail files in a directory."""
    dumps = [(int(match.group(1)), path) for path in Path(directory).glob('*_rewrite_*.sail')
             if (match := DUMP_NAME.search(path.name))]
    if not dumps:
        raise FileNotFoundError(f'no *_rewrite_N_*.sail dump in {directory}')
    return max(dumps)[1]


def identifiers(text: str) -> list[str]:
    """The identifiers in a piece of Sail, operators named as the translator names them."""
    return [canonical(i) for i in IDENTIFIER.findall(text)]


class CallGraph:
    def __init__(self, bodies: dict[str, list[str]], kinds: dict[str, str]):
        """
        `bodies` maps each node to the text of its definitions (several for a scattered function);
        `kinds` maps it to function, value or register.
        """
        self.bodies = bodies
        self.kinds = kinds
        self.nodes = set(bodies)
        self.functions = {name for name, kind in kinds.items() if kind == 'function'}
        self.values = {name for name, kind in kinds.items() if kind == 'value'}
        self.registers = {name for name, kind in kinds.items() if kind == 'register'}
        self.edges = {name: {i for chunk in chunks for i in identifiers(chunk)
                             if i in self.nodes and i != name}
                      for name, chunks in bodies.items()}

    @classmethod
    def from_dump_text(cls, text: str) -> 'CallGraph':
        bodies: dict[str, list[str]] = {}
        kinds: dict[str, str] = {}
        for chunk in re.split(r'\n\s*\n', text):
            # A doc comment with blank lines in it is split over several chunks; the last one
            # starts inside the comment, and may go on with a definition after the comment.
            if '*/' in chunk and not any(head.match(chunk) for head, _ in HEADS):
                chunk = chunk[chunk.index('*/') + 2:]
            for head, kind in HEADS:
                match = head.match(chunk)
                if match:
                    name = canonical(match.group(1))
                    bodies.setdefault(name, []).append(chunk)
                    kinds.setdefault(name, kind)
                    break
        return cls(bodies, kinds)

    @classmethod
    def from_dump(cls, path: Path) -> 'CallGraph':
        return cls.from_dump_text(Path(path).read_text(errors='replace'))

    def reachable(self, roots: Iterable[str], skip_edge: Callable[[str, str], bool] = lambda source, target: False) -> set[str]:
        """
        The nodes reachable from the roots, the roots included; roots that are not nodes are ignored.
        Edges for which skip_edge(source, target) holds are not followed.
        """
        seen: set[str] = set()
        queue = deque(root for root in roots if root in self.edges)
        while queue:
            node = queue.popleft()
            if node in seen:
                continue
            seen.add(node)
            queue.extend(target for target in self.edges.get(node, ()) if not skip_edge(node, target))
        return seen

    def mentioned(self, functions: Iterable[str]) -> set[str]:
        """Every identifier in the bodies of the functions."""
        return {i for f in functions for chunk in self.bodies[f] for i in identifiers(chunk)}


def mark_reachable(items: list[Item], graph: CallGraph,
                   step_roots: Iterable[str] = STEP_ROOTS,
                   execute_roots: Iterable[str] = EXECUTE_ROOTS) -> None:
    """Set the column `reachable` of each item (in place)."""
    from_step = graph.reachable(step_roots)
    from_execute = graph.reachable(execute_roots)
    step_identifiers, execute_identifiers = graph.mentioned(from_step), graph.mentioned(from_execute)
    for record in items:
        name = record['name']
        if name in graph.nodes:
            record['reachable'] = 'step' if name in from_step else 'execute' if name in from_execute else ''
        else:
            record['reachable'] = 'step' if name in step_identifiers else 'execute' if name in execute_identifiers else ''
