#!/usr/bin/env python3
"""Generate a side-by-side HTML comparison of the Rocq/μSail files produced by
the Katamaran Sail backend and the original Sail sources they were translated
from.

The input is an output directory of the backend, containing the generated .v
files and, optionally, sources.txt (the Sail files, in order) and log.txt (the
backend's output with the nanosail AST dump, i.e. run with (dump-nanosail-ast)).
Without sources.txt the Sail files are taken from the command line recorded in
log.txt.

Rocq definitions are matched to Sail definitions as follows:

  * fun_<f>: the AST dump records the Sail source location of every statement
    of <f>; the top-level Sail item that contains most of them is the
    counterpart (plus its `val` declaration, or the `union clause` of a
    scattered function clause). Without a dump, <f> is looked up by name.
  * types, registers and constants: by name, with a few fallbacks for
    generated names (<T>Constructor, <x>_value, monomorphised <T>_...).
  * semantics_<f>: the `val <f>` declaration.
  * generated EqDec/Finite instances are grouped under the type they belong to.

Everything else is shown as Rocq-only infrastructure.

Usage: validation/scripts/gen_comparison.py [DIR] [-o comparison.html] [options]
Run with --help for the options (path remapping, GitHub links, Sail library).
"""

import argparse
import collections
import json
import os
import re
import shutil
import subprocess
import sys
import time
import urllib.parse


def warn(msg):
    print(f"warning: {msg}", file=sys.stderr)


# ---------------------------------------------------------------------------
# Paths and links


class PathMapper:
    """Rewrites source paths (as recorded by the backend) to where
    the files actually live, and resolves bare Sail library file names."""

    def __init__(self, mappings, lib_dirs):
        self.mappings = sorted(mappings, key=lambda m: -len(m[0]))
        self.lib_dirs = lib_dirs

    def __call__(self, path):
        for src, dst in self.mappings:
            if path == src or path.startswith(src.rstrip("/") + "/"):
                path = dst.rstrip("/") + path[len(src.rstrip("/")):]
                break
        if not os.path.isabs(path):
            for d in self.lib_dirs:
                candidate = os.path.join(d, path)
                if os.path.exists(candidate):
                    return os.path.abspath(candidate)
        return path


def git_web_url(remote):
    remote = remote.strip()
    m = re.match(r"^(?:ssh://)?git@([^:/]+)[:/](.+?)(?:\.git)?/?$", remote)
    if m:
        return f"https://{m.group(1)}/{m.group(2)}"
    m = re.match(r"^https?://(?:[^@/]+@)?(.+?)(?:\.git)?/?$", remote)
    if m:
        return f"https://{m.group(1)}"
    return None


class Linker:
    """Builds VS Code, file:// and GitHub links for a file and line range."""

    def __init__(self, github_overrides, use_git):
        self.overrides = sorted(github_overrides, key=lambda m: -len(m[0]))
        self.use_git = use_git
        self.cache = {}

    def _git(self, cwd, *args):
        try:
            out = subprocess.run(["git", "-C", cwd, *args], capture_output=True,
                                 text=True, timeout=10)
        except (OSError, subprocess.TimeoutExpired):
            return None
        return out.stdout.strip() if out.returncode == 0 else None

    def github_base(self, path):
        """Returns a URL prefix such that prefix + '#L1' points at line 1 of path."""
        for prefix, url in self.overrides:
            if path.startswith(prefix.rstrip("/") + "/"):
                rel = os.path.relpath(path, prefix)
                return url.rstrip("/") + "/" + urllib.parse.quote(rel)
        if not self.use_git or not os.path.exists(path):
            return None
        if path in self.cache:
            return self.cache[path]
        result = None
        d = os.path.dirname(path)
        top = self._git(d, "rev-parse", "--show-toplevel")
        if top and self._git(d, "ls-files", "--error-unmatch", path) is not None:
            sha = self._git(d, "rev-parse", "HEAD")
            remote = self._git(d, "remote", "get-url", "origin")
            web = git_web_url(remote) if remote else None
            if sha and web:
                rel = os.path.relpath(os.path.realpath(path), os.path.realpath(top))
                result = f"{web}/blob/{sha}/{urllib.parse.quote(rel)}"
        self.cache[path] = result
        return result

    def links(self, path, start, end=None):
        links = {}
        if os.path.isabs(path):
            links["vscode"] = f"vscode://file{urllib.parse.quote(path)}:{start}"
            links["file"] = "file://" + urllib.parse.quote(path)
        gh = self.github_base(path)
        if gh:
            links["github"] = f"{gh}#L{start}" + (f"-L{end}" if end and end != start else "")
        return links


# ---------------------------------------------------------------------------
# Sail sources


SAIL_START = re.compile(
    r"^(?:private\s+)?"
    r"(?P<kw>scattered\s+\w+|function\s+clause|union\s+clause|enum\s+clause|mapping\s+clause"
    r"|function|val|type|enum|union|struct|bitfield|register|let|var|mapping|overload"
    r"|default|infixl|infixr|infix|end|termination_measure|newtype|constraint|outcome"
    r"|instantiation|\$\w+)\b\s*(?P<rest>.*)$")
SAIL_IDENT = r"(?:operator\s+[^\s(]+|[A-Za-z_][\w']*)"


def scan_sail_comments(lines):
    """For each line: (inside a block comment at its start, has code outside comments)."""
    depth = 0
    info = []
    for line in lines:
        start_depth = depth
        has_code = False
        i, n = 0, len(line)
        in_string = False
        while i < n:
            c = line[i]
            if in_string:
                if c == "\\":
                    i += 1
                elif c == '"':
                    in_string = False
            elif depth > 0:
                if line.startswith("/*", i):
                    depth += 1
                    i += 1
                elif line.startswith("*/", i):
                    depth -= 1
                    i += 1
            elif line.startswith("//", i):
                break
            elif line.startswith("/*", i):
                depth += 1
                i += 1
            elif not c.isspace():
                has_code = True
                if c == '"':
                    in_string = True
            i += 1
        info.append((start_depth > 0, has_code))
    return info


def parse_sail_name(kw, rest):
    """Returns (name, constructor) of a top-level Sail item."""
    kw = re.sub(r"\s+", " ", kw)
    if kw in ("function clause", "union clause", "enum clause", "mapping clause"):
        m = re.match(rf"({SAIL_IDENT})\s*(?:=\s*)?\(?\s*([A-Za-z_][\w']*)?", rest)
        if m:
            return m.group(1), m.group(2)
        return None, None
    if kw == "val":
        m = re.match(rf'"([^"]+)"|({SAIL_IDENT})', rest)
        return (m.group(1) or m.group(2), None) if m else (None, None)
    m = re.match(SAIL_IDENT, rest)
    return (m.group(0) if m else None), None


class SailItem:
    def __init__(self, path, kind, name, ctor, start, end, doc_start, text):
        self.path, self.kind, self.name, self.ctor = path, kind, name, ctor
        self.start, self.end, self.doc_start, self.text = start, end, doc_start, text
        self.index = None  # position in the emitted snippet table


def parse_sail_file(path):
    with open(path, encoding="utf-8", errors="replace") as f:
        lines = f.read().split("\n")
    info = scan_sail_comments(lines)
    starts = []
    for i, line in enumerate(lines):
        if info[i][0] or not line or line[0].isspace():
            continue
        m = SAIL_START.match(line)
        if m:
            starts.append((i, re.sub(r"\s+", " ", m.group("kw")), m.group("rest")))
    items = []
    prev_end = -1
    for k, (i, kw, rest) in enumerate(starts):
        nxt = starts[k + 1][0] if k + 1 < len(starts) else len(lines)
        end = i
        for j in range(i, nxt):
            if info[j][1] and not (j > i and lines[j].startswith("$[")):
                end = j
        doc = i
        while (doc - 1 > prev_end and lines[doc - 1].strip()
               and (not info[doc - 1][1] or lines[doc - 1].startswith("$["))):
            doc -= 1
        name, ctor = parse_sail_name(kw, rest)
        items.append(SailItem(path, kw, name, ctor, i + 1, end + 1, doc + 1,
                              "\n".join(lines[doc:end + 1])))
        prev_end = end
    return items


class SailIndex:
    def __init__(self, mapper):
        self.mapper = mapper
        self.files = {}        # path -> [SailItem] (None if unreadable)
        self.by_name = collections.defaultdict(list)
        self.by_ctor = collections.defaultdict(list)
        self.order = []

    def load(self, path):
        path = self.mapper(path)
        if path in self.files:
            return self.files[path]
        items = None
        if os.path.isfile(path):
            try:
                items = parse_sail_file(path)
            except OSError as e:
                warn(f"cannot read {path}: {e}")
        self.files[path] = items
        for it in items or []:
            if it.name:
                self.by_name[it.name].append(it)
            if it.ctor:
                self.by_ctor[(it.kind, it.ctor)].append(it)
        return items

    def lookup(self, name, kinds=None, ctor=None):
        return [it for it in self.by_name.get(name, [])
                if (kinds is None or it.kind in kinds) and (ctor is None or it.ctor == ctor)]

    def items_of_kind(self, kind):
        """Items of a kind, the model's sources (in order) before the library."""
        paths = self.order + [p for p in self.files if p not in self.order]
        return [it for p in paths for it in (self.files.get(p) or []) if it.kind == kind]

    def item_at(self, path, line):
        for it in self.files.get(path) or []:
            if it.start <= line <= it.end:
                return it
        return None


TYPE_KINDS = {"type", "enum", "union", "struct", "bitfield", "newtype",
              "scattered union", "scattered enum"}
VALUE_KINDS = {"let", "var"}


# ---------------------------------------------------------------------------
# The nanosail AST dump


def parse_log(path, mapper):
    """Returns {function name: Counter((path, line))} from the AST dump."""
    loc_re = re.compile(r'location="(?:GeneratedLocation\()?([^":()]+?\.sail):(\d+)')
    funcs = {}
    cur = None
    with open(path, encoding="utf-8", errors="replace") as f:
        for line in f:
            if line.startswith("Def:"):
                m = re.match(r'Def:Function\[name=Identifier\["([^"]+)"\]', line)
                cur = funcs.setdefault(m.group(1), collections.Counter()) if m else None
            elif cur is not None and "location=" in line:
                for p, ln in loc_re.findall(line):
                    cur[(mapper(p), int(ln))] += 1
    return funcs


# ---------------------------------------------------------------------------
# Rocq sources


ROCQ_START = re.compile(
    r"^(?P<ind> {0,4})(?:#\[[^\]]*\]\s*)?(?:(?:Local|Global|Program|Polymorphic)\s+)*"
    r"(?P<kw>Definition|Fixpoint|CoFixpoint|Inductive|CoInductive|Record|Variant|Structure"
    r"|Lemma|Theorem|Corollary|Fact|Remark|Instance|Class|Equations|Notation|Infix"
    r"|Arguments|Section|Module|Include|Derive|Hint|Import|Export|From|Require|Ltac"
    r"|Obligation|Open|Close|Opaque|Transparent|Existing|Set|Unset|Axiom|Parameter"
    r"|Context|Variable|Variables|Coercion)\b\s*(?P<rest>.*)$")


def scan_rocq_comments(lines):
    """For each line: the comment nesting depth at its start."""
    depth = 0
    out = []
    in_string = False
    for line in lines:
        out.append(depth if not in_string else -1)
        i, n = 0, len(line)
        while i < n:
            if in_string:
                if line[i] == '"':
                    if line.startswith('""', i):
                        i += 1
                    else:
                        in_string = False
            elif line.startswith("(*", i):
                depth += 1
                i += 1
            elif depth and line.startswith("*)", i):
                depth -= 1
                i += 1
            elif not depth and line[i] == '"':
                in_string = True
            i += 1
    return out


def rocq_name(kw, rest):
    if kw == "Derive":
        m = re.match(r"\w+\s+for\s+([\w']+)", rest)
        return m.group(1) if m else None
    if kw == "Module":
        rest = re.sub(r"^(?:Import|Export)\s+", "", rest)
    if kw == "Instance":
        rest = re.sub(r"^Instance\s+", "", rest)
    if kw == "Notation":
        m = re.match(r'"([^"]*)"', rest)
        return m.group(1) if m else None
    m = re.match(r"@?([^\s:({.\[]+)", rest)
    return m.group(1) if m else None


class RocqBlock:
    def __init__(self, start, end, kw, name, text):
        self.start, self.end, self.kw, self.name, self.text = start, end, kw, name, text


def split_rocq(path):
    with open(path, encoding="utf-8", errors="replace") as f:
        lines = f.read().split("\n")
    if lines and lines[-1] == "":
        lines.pop()
    depth = scan_rocq_comments(lines)
    starts = []  # (line index, kw, rest) ; kw None for a comment
    for i, line in enumerate(lines):
        if depth[i] != 0:
            continue
        m = ROCQ_START.match(line)
        if m:
            starts.append((i, m.group("kw"), m.group("rest")))
        elif re.match(r"^ {0,4}\(\*", line):
            starts.append((i, None, ""))
    if not starts or starts[0][0] != 0:
        starts.insert(0, (0, None, ""))
    raw = []
    for k, (i, kw, rest) in enumerate(starts):
        nxt = starts[k + 1][0] if k + 1 < len(starts) else len(lines)
        end = nxt - 1
        while end > i and not lines[end].strip():
            end -= 1
        raw.append([i, end, kw, rest])
    # A comment directly above a definition belongs to that definition.
    blocks = []
    pending = None
    for i, end, kw, rest in raw:
        if kw is None and not "".join(lines[i:end + 1]).strip():
            continue
        if pending is not None and pending[1] + 1 == i:
            i = pending[0]
        elif pending is not None:
            blocks.append(RocqBlock(pending[0] + 1, pending[1] + 1, None, None,
                                    "\n".join(lines[pending[0]:pending[1] + 1])))
        pending = None
        if kw is None:
            pending = (i, end)
            continue
        blocks.append(RocqBlock(i + 1, end + 1, kw, rocq_name(kw, rest),
                                "\n".join(lines[i:end + 1])))
    if pending is not None:
        blocks.append(RocqBlock(pending[0] + 1, pending[1] + 1, None, None,
                                "\n".join(lines[pending[0]:pending[1] + 1])))
    return blocks, len(lines)


# ---------------------------------------------------------------------------
# Matching


BOILERPLATE = re.compile(r"^(?P<key>.+?)(?:_eqdec_\w+|_EqDec|_finite)$")
TYPE_DEF_KWS = {"Inductive", "Record", "Variant", "Definition"}


class Matcher:
    def __init__(self, index, log_funcs):
        self.index = index
        self.log_funcs = log_funcs

    @staticmethod
    def names_function(item, name):
        if not item.name:
            return False
        if item.ctor and name == f"{item.name}_{item.ctor}":
            return True
        # mappings give rise to <m>_forwards, <m>_backwards, <m>_forwards_matches, ...
        return name == item.name or (item.kind.startswith("mapping") and
                                     name.startswith(item.name + "_"))

    def function(self, name):
        """Sail items for the function `name` (from the AST dump), plus a note."""
        locs = self.log_funcs.get(name)
        if not locs:
            bitfield = self.bitfield_accessor(name)
            if bitfield:
                return [bitfield], [], f"generated by Sail from bitfield {bitfield.name}"
            items = self.function_by_name(name)
            return items, [], "" if items or not self.log_funcs else "no source location in the AST dump"
        per_file = collections.Counter()
        for (p, _), n in locs.items():
            per_file[p] += n
        notes = []
        items = []
        missing = []
        for p, _ in per_file.most_common():
            self.index.load(p)
            if self.index.files.get(p) is None:
                lines = sorted(ln for (q, ln) in locs if q == p)
                missing.append((p, lines[0], lines[-1]))
                continue
            hits = collections.Counter()
            by_id = {}
            for (q, ln), n in locs.items():
                if q == p:
                    it = self.index.item_at(p, ln)
                    if it is not None:
                        hits[id(it)] += n
                        by_id[id(it)] = it
            if hits:
                # Locations can also point into code that was inlined or shared
                # (e.g. mapping clauses), so an item named after the function wins.
                named = [k for k in hits if self.names_function(by_id[k], name)]
                best = max(named or hits, key=lambda k: hits[k])
                items.append(by_id[best])
                break
        if items:
            items = self.context(items[0]) + items
        elif missing and all(not os.path.isabs(p) for p, _, _ in missing):
            notes.append("built into Sail: " + ", ".join(f"{p}:{s}" for p, s, _ in missing))
            missing = []
        return items, missing, "; ".join(notes)

    def context(self, item):
        """Declarations shown above a function: its `val`, or for a clause of a
        scattered function the `union clause` of its constructor."""
        if item.kind == "function clause" and item.ctor:
            return self.index.by_ctor[("union clause", item.ctor)][:1]
        return self.index.lookup(item.name, {"val"})[:1] if item.name else []

    def function_by_name(self, name):
        """Fallback without AST dump: f, or a clause f(C ...) for a name f_C."""
        found = self.index.lookup(name, {"function"})
        if not found:
            parts = name.split("_")
            for k in range(1, len(parts)):
                found = self.index.lookup("_".join(parts[:k]), {"function clause"},
                                          "_".join(parts[k:]))
                if found:
                    break
        return self.context(found[0]) + found[:1] if found else []

    def bitfield_accessor(self, name):
        """The bitfield B behind Mk_B, _get_B_F, _set_B_F and _update_B_F, which
        Sail generates and which therefore have no source locations."""
        m = re.match(r"(?:Mk_|_get_|_set_|_update_)(\w+)$", name)
        if not m:
            return None
        rest = m.group(1)
        candidates = [it for it in self.index.items_of_kind("bitfield")
                      if rest == it.name or rest.startswith(it.name + "_")]
        return max(candidates, key=lambda it: len(it.name), default=None)

    def by_name(self, name):
        """Sail items for a type, register or constant named `name` in Rocq."""
        candidates = [name]
        if name.endswith("_value"):
            candidates.append(name[:-len("_value")])
        if name.endswith("Constructor"):
            candidates.append(name[:-len("Constructor")])
        for c in candidates:
            found = self.index.lookup(c, TYPE_KINDS | VALUE_KINDS | {"register"})
            if found:
                return found[:1], ""
        parts = name.split("_")
        for k in range(len(parts) - 1, 0, -1):
            found = self.index.lookup("_".join(parts[:k]), TYPE_KINDS)
            if found:
                return found[:1], "monomorphic instance of " + found[0].name
        return [], ""


def classify(file_key, blocks, matcher, index):
    """Turns Rocq blocks into rows: (kind, title, blocks, sail items, missing, note, ref)."""
    rows = []
    type_rows = {}
    for b in blocks:
        name = b.name or ""
        kind, items, missing, note, ref = "infra", [], [], "", None
        if b.kw == "Definition" and name.startswith("fun_"):
            kind = "function"
            items, missing, note = matcher.function(name[4:])
        elif b.kw == "Definition" and name.startswith("semantics_"):
            kind = "foreign"
            items = index.lookup(name[len("semantics_"):], {"val"})[:1]
        elif b.kw == "Inductive" and name in ("RegName", "Reg"):
            kind = "registers"
            for it in index.items_of_kind("register"):
                if re.search(rf"\b(?:RegName_)?{re.escape(it.name)}\b", b.text):
                    items.append(it)
        elif b.kw in ("Instance", "Derive", "Definition", "Inductive", "Lemma") and (
                BOILERPLATE.match(name) or b.kw == "Derive" or
                (name.endswith("Constructor") and name[:-11] in type_rows)):
            m = BOILERPLATE.match(name)
            key = m.group("key") if m else name
            if key.endswith("Constructor") and key[:-11] in type_rows:
                key = key[:-11]
            if key in type_rows:
                kind, ref = "boilerplate", key
        if kind == "infra" and b.kw in TYPE_DEF_KWS and name:
            items, note = matcher.by_name(name)
            if items:
                kind = "type" if items[0].kind in TYPE_KINDS else (
                    "register" if items[0].kind == "register" else "value")
                if items[0].kind == "scattered union":
                    items += [it for it in index.lookup(items[0].name, {"union clause"})
                              if re.search(rf"\b{re.escape(it.ctor or '')}\b", b.text)]
                type_rows[name] = True
        rows.append(dict(kind=kind, title=name or "(comment)", blocks=[b],
                         items=items, missing=missing, note=note, ref=ref))

    # Merge runs of rows that belong together.
    merged = []
    for r in rows:
        last = merged[-1] if merged else None
        if last and r["kind"] == "boilerplate" and last["kind"] == "boilerplate" \
                and last["ref"] == r["ref"]:
            last["blocks"] += r["blocks"]
        elif last and r["kind"] == "infra" and last["kind"] == "infra":
            last["blocks"] += r["blocks"]
        else:
            merged.append(r)
    for r in merged:
        if r["kind"] == "infra" and len(r["blocks"]) > 1:
            names = [b.name for b in r["blocks"] if b.name]
            r["title"] = (f"{names[0]} … {names[-1]}" if len(names) > 1 else
                          names[0] if names else "(comment)")
    return merged


# ---------------------------------------------------------------------------
# Output


KIND_LABEL = {
    "function": "function", "foreign": "foreign", "type": "type", "value": "constant",
    "register": "register", "registers": "registers", "boilerplate": "boilerplate",
    "infra": "infrastructure",
}


def build(args):
    mapper = PathMapper(args.path_map, args.sail_lib)
    linker = Linker(args.github, not args.no_git)
    index = SailIndex(mapper)

    raw_sources = read_sources(args.sources, args.log)
    if not raw_sources:
        warn("no Sail files known: pass --sources, or a log.txt that records the sail command")
    sources = [mapper(p) for p in raw_sources]
    for p in sources:
        index.load(p)
        index.order.append(p)

    log_funcs = {}
    if args.log and os.path.exists(args.log):
        print("reading AST dump …", file=sys.stderr)
        log_funcs = parse_log(args.log, lambda p: p)
    if not log_funcs:
        warn("no nanosail AST dump found (log.txt with (dump-nanosail-ast)); "
             "functions are matched by name only")
    # Other directories the dump points into hold Sail's library (vector.sail, ...).
    lib_dirs = list(args.sail_lib)
    for locs in log_funcs.values():
        for p, _ in locs:
            d = os.path.dirname(mapper(p))
            if os.path.isabs(p) and p not in raw_sources and d not in lib_dirs:
                lib_dirs.append(d)
    mapper.lib_dirs = lib_dirs
    log_funcs = {name: collections.Counter({(mapper(p), ln): n for (p, ln), n in locs.items()})
                 for name, locs in log_funcs.items()}
    # The library comes last, so that the model's own definitions win name lookups.
    for d in lib_dirs:
        if os.path.isdir(d):
            for fn in sorted(os.listdir(d)):
                if fn.endswith(".sail"):
                    index.load(os.path.join(d, fn))
    matcher = Matcher(index, log_funcs)

    snippets = []

    def snippet(it):
        if it.index is None:
            it.index = len(snippets)
            snippets.append(dict(
                path=it.path, file=os.path.basename(it.path), kind=it.kind, name=it.name,
                start=it.doc_start, def_start=it.start, end=it.end, text=it.text,
                links=linker.links(it.path, it.start, it.end)))
        return it.index

    files_out = []
    stats = {}
    used_keys = collections.Counter()
    for path in args.rocq:
        path = os.path.abspath(path)
        key = re.sub(r"[^\w.-]", "_", os.path.splitext(os.path.basename(path))[0])
        used_keys[key] += 1
        if used_keys[key] > 1:
            key += f"-{used_keys[key]}"
        blocks, nlines = split_rocq(path)
        rows = classify(key, blocks, matcher, index)
        used_ids = collections.Counter()
        out_rows = []
        for r in rows:
            first, last = r["blocks"][0], r["blocks"][-1]
            rid = re.sub(r"[^\w.-]", "_", r["title"]) or "block"
            used_ids[rid] += 1
            if used_ids[rid] > 1:
                rid += f"-{used_ids[rid]}"
            aliases = sorted({b.name for b in r["blocks"] if b.name} |
                             {it.name for it in r["items"] if it.name})
            out_rows.append(dict(
                id=rid, kind=r["kind"], label=KIND_LABEL[r["kind"]], title=r["title"],
                start=first.start, end=last.end,
                text="\n".join(b.text if i == 0 else
                               "\n" * (b.start - r["blocks"][i - 1].end - 1) + b.text
                               for i, b in enumerate(r["blocks"])),
                sail=[snippet(it) for it in r["items"]],
                missing=[dict(path=p, file=os.path.basename(p), start=s, end=e,
                              links=linker.links(p, s, e)) for p, s, e in r["missing"]],
                note=r["note"], ref=r["ref"], aliases=aliases,
                links=linker.links(path, first.start, last.end)))
        counts = collections.Counter(r["kind"] for r in rows)
        unmatched = sum(1 for r in rows if r["kind"] in ("function", "foreign")
                        and not r["items"] and not r["missing"])
        stats[key] = dict(counts=counts, unmatched=unmatched)
        files_out.append(dict(key=key, file=os.path.basename(path), path=path,
                              lines=nlines, links=linker.links(path, 1), rows=out_rows))

    missing_sources = [p for p in index.files if index.files[p] is None and os.path.isabs(p)]
    if missing_sources:
        warn(f"{len(missing_sources)} Sail file(s) not found, e.g. {missing_sources[0]}"
             " (use --path-map / --sail-lib)")
    data = dict(
        title=args.title,
        generated=time.strftime("%Y-%m-%d %H:%M"),
        files=files_out, sail=snippets,
        missing_sources=missing_sources,
    )
    return data, stats


def read_sources(sources, log):
    """The Sail files, from sources.txt or else from the command in log.txt."""
    if sources and os.path.exists(sources):
        with open(sources, encoding="utf-8") as f:
            return [l.strip() for l in f if l.strip()]
    if log and os.path.exists(log):
        with open(log, encoding="utf-8", errors="replace") as f:
            for line in f:
                if line.startswith("$ "):
                    return [a for a in line.split()[1:] if a.endswith(".sail")]
    return []


def find_rocq_files(d):
    """The generated .v files in d, each after the files it Requires."""
    files = sorted(f for f in os.listdir(d)
                   if f.endswith(".v") and not f.endswith(".template.v"))
    stems = {os.path.splitext(f)[0] for f in files}
    deps = {}
    for f in files:
        with open(os.path.join(d, f), encoding="utf-8", errors="replace") as h:
            head = h.read(4000)
        deps[os.path.splitext(f)[0]] = {
            m for m in re.findall(r"Require\s+(?:Import\s+|Export\s+)?([\w.]+)", head)
            if m in stems}
    ordered, done = [], set()

    def visit(stem, seen=()):
        if stem in done or stem in seen:
            return
        for dep in sorted(deps.get(stem, ())):
            visit(dep, seen + (stem,))
        done.add(stem)
        ordered.append(stem)
    for f in files:
        visit(os.path.splitext(f)[0])
    return [os.path.join(d, s + ".v") for s in ordered]


def default_title(d):
    name = os.path.basename(os.path.abspath(d))
    if name in ("output", "out", "build", "generated", "result"):
        name = os.path.basename(os.path.dirname(os.path.abspath(d)))
    return f"{name}: μSail ↔ Sail"


def write_html(data, out):
    payload = json.dumps(data, ensure_ascii=False, separators=(",", ":"))
    payload = payload.replace("</", "<\\/")
    title = data["title"].replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")
    with open(out, "w", encoding="utf-8") as f:
        f.write(HTML_TEMPLATE.replace("__TITLE__", title)
                .replace("/*__DATA__*/", payload, 1))


HTML_TEMPLATE = r"""<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>__TITLE__</title>
<style>
:root {
  --bg: #fbfbf9; --panel: #ffffff; --panel2: #f3f3ef; --fg: #1d1d1b; --muted: #6b6b66;
  --border: #e2e1dc; --accent: #2f5fb3; --accent-bg: #e8eefa;
  --kw: #8a3ea8; --str: #1f7a43; --com: #8b8b84; --num: #b0570e; --builtin: #2f5fb3;
  --tv: #a33a3a; --warn: #a15c00; --warn-bg: #fff4e0; --hl: #fff6c9;
  --k-function: #2f5fb3; --k-type: #1f7a43; --k-value: #6a5acd; --k-register: #b0570e;
  --k-foreign: #a33a3a; --k-infra: #8b8b84; --k-boilerplate: #8b8b84;
}
@media (prefers-color-scheme: dark) {
  :root:not([data-theme="light"]) {
    --bg: #17181a; --panel: #1e1f22; --panel2: #25272b; --fg: #e4e3de; --muted: #9a9a94;
    --border: #34363b; --accent: #8fb0ef; --accent-bg: #23314d;
    --kw: #d2a0e6; --str: #8fd3a5; --com: #7f8086; --num: #f0b072; --builtin: #8fb0ef;
    --tv: #f09a9a; --warn: #f0b072; --warn-bg: #3a2e1c; --hl: #3d3a22;
    --k-function: #8fb0ef; --k-type: #8fd3a5; --k-value: #b9aef5; --k-register: #f0b072;
    --k-foreign: #f09a9a; --k-infra: #7f8086; --k-boilerplate: #7f8086;
  }
}
:root[data-theme="dark"] {
  --bg: #17181a; --panel: #1e1f22; --panel2: #25272b; --fg: #e4e3de; --muted: #9a9a94;
  --border: #34363b; --accent: #8fb0ef; --accent-bg: #23314d;
  --kw: #d2a0e6; --str: #8fd3a5; --com: #7f8086; --num: #f0b072; --builtin: #8fb0ef;
  --tv: #f09a9a; --warn: #f0b072; --warn-bg: #3a2e1c; --hl: #3d3a22;
  --k-function: #8fb0ef; --k-type: #8fd3a5; --k-value: #b9aef5; --k-register: #f0b072;
  --k-foreign: #f09a9a; --k-infra: #7f8086; --k-boilerplate: #7f8086;
}
* { box-sizing: border-box; }
html, body { margin: 0; background: var(--bg); color: var(--fg);
  font: 14px/1.45 system-ui, -apple-system, "Segoe UI", sans-serif; }
a { color: var(--accent); text-decoration: none; }
a:hover { text-decoration: underline; }
#top { position: sticky; top: 0; z-index: 10; background: var(--panel);
  border-bottom: 1px solid var(--border); padding: 8px 16px; display: flex; flex-wrap: wrap;
  gap: 8px 16px; align-items: center; }
#top h1 { font-size: 15px; margin: 0; font-weight: 600; white-space: nowrap; }
.tabs { display: flex; gap: 4px; }
.tabs button, .ctl button { font: inherit; border: 1px solid var(--border); background: var(--panel2);
  color: var(--fg); padding: 3px 10px; border-radius: 6px; cursor: pointer; }
.tabs button[aria-selected="true"] { background: var(--accent-bg); border-color: var(--accent);
  color: var(--accent); font-weight: 600; }
.ctl { display: flex; flex-wrap: wrap; gap: 8px 12px; align-items: center; }
.ctl input[type=search] { font: inherit; padding: 3px 8px; border: 1px solid var(--border);
  border-radius: 6px; background: var(--bg); color: var(--fg); width: 240px; max-width: 60vw; }
.ctl select { font: inherit; background: var(--bg); color: var(--fg); border: 1px solid var(--border);
  border-radius: 6px; padding: 2px 4px; }
.ctl label { white-space: nowrap; color: var(--muted); }
#count { color: var(--muted); font-size: 12px; }
#fileinfo { padding: 10px 16px 0; color: var(--muted); font-size: 13px; }
#fileinfo .warn { color: var(--warn); background: var(--warn-bg); padding: 6px 10px;
  border-radius: 6px; margin: 6px 0; }
#colheads { display: grid; grid-template-columns: minmax(0,1.4fr) minmax(0,1fr); gap: 12px;
  padding: 8px 16px 0; font-size: 12px; font-weight: 600; letter-spacing: .03em;
  color: var(--muted); }
#rows { padding: 8px 16px 40vh; }
.row { background: var(--panel); border: 1px solid var(--border); border-radius: 8px;
  margin: 8px 0; scroll-margin-top: var(--hdr, 60px); }
.row:target, .row.flash { box-shadow: 0 0 0 2px var(--accent); }
.rh { display: flex; flex-wrap: wrap; align-items: baseline; gap: 4px 10px; padding: 6px 10px;
  cursor: pointer; user-select: none; }
.rh .tog { width: 1em; color: var(--muted); display: inline-block; transition: transform .1s; }
.row.open .rh .tog { transform: rotate(90deg); }
.badge { font-size: 11px; padding: 0 6px; border-radius: 4px; border: 1px solid currentColor;
  white-space: nowrap; }
.k-function .badge { color: var(--k-function); } .k-type .badge { color: var(--k-type); }
.k-value .badge { color: var(--k-value); } .k-register .badge, .k-registers .badge { color: var(--k-register); }
.k-foreign .badge { color: var(--k-foreign); } .k-infra .badge { color: var(--k-infra); }
.k-boilerplate .badge { color: var(--k-boilerplate); }
.rh .title { font-family: ui-monospace, "JetBrains Mono", Menlo, Consolas, monospace;
  font-weight: 600; word-break: break-all; }
.k-infra .rh .title, .k-boilerplate .rh .title { font-weight: 400; color: var(--muted); }
.rh .src { color: var(--muted); font-size: 12px; white-space: nowrap; }
.rh .src.nosail { color: var(--warn); }
.links { font-size: 12px; white-space: nowrap; }
.links a + a::before { content: "·"; color: var(--muted); padding: 0 5px; display: inline-block; }
.rh .links { margin-left: auto; }
.body { display: none; border-top: 1px solid var(--border); }
.row.open .body { display: grid; grid-template-columns: minmax(0,1.4fr) minmax(0,1fr); }
.col { min-width: 0; }
.col.sail { border-left: 1px solid var(--border); position: sticky; top: var(--hdr, 60px);
  align-self: start; max-height: calc(100vh - var(--hdr, 60px)); overflow: auto; }
.snip + .snip { border-top: 1px dashed var(--border); }
.sh { display: flex; flex-wrap: wrap; gap: 2px 10px; align-items: baseline; padding: 4px 10px;
  background: var(--panel2); font-size: 12px; color: var(--muted); }
.sh b { color: var(--fg); font-weight: 600; font-family: ui-monospace, Menlo, Consolas, monospace; }
.sh .links { margin-left: auto; }
.note { padding: 8px 10px; color: var(--muted); font-size: 13px; }
.note.warn { color: var(--warn); }
pre.code { margin: 0; padding: 6px 0; overflow-x: auto; font: 12.5px/1.5 ui-monospace,
  "JetBrains Mono", Menlo, Consolas, monospace; tab-size: 4; }
.line { display: block; padding-right: 12px; white-space: pre; }
.line::before { content: attr(data-n); display: inline-block; width: 6ch; margin-right: 12px;
  text-align: right; color: var(--muted); opacity: .6; user-select: none; position: sticky; left: 0;
  background: inherit; }
pre.code { background: var(--panel); }
.line:hover { background: var(--hl); }
.line.doc { opacity: .75; }
.t-kw { color: var(--kw); font-weight: 600; } .t-str { color: var(--str); }
.t-com { color: var(--com); font-style: italic; } .t-num { color: var(--num); }
.t-bi { color: var(--builtin); } .t-tv { color: var(--tv); }
.pending { padding: 10px; color: var(--muted); }
@media (max-width: 800px) {
  .row.open .body { grid-template-columns: minmax(0,1fr); }
  .col.sail { border-left: 0; border-top: 1px solid var(--border); position: static; max-height: none; }
  #colheads { display: none; }
  #top { padding: 8px 16px; }
}
</style>
</head>
<body>
<div id="top">
  <h1>__TITLE__</h1>
  <div class="tabs" role="tablist" id="tabs"></div>
  <div class="ctl">
    <input type="search" id="q" placeholder="Search definitions… (Enter = jump)" autocomplete="off">
    <select id="filter" title="Filter">
      <option value="all">All</option>
      <option value="matched">With Sail counterpart</option>
      <option value="unmatched">Without Sail counterpart</option>
      <option value="nobp">Without boilerplate/infrastructure</option>
    </select>
    <label><input type="checkbox" id="compact"> compact indentation</label>
    <button id="openall" type="button">Expand all</button>
    <button id="closeall" type="button">Collapse all</button>
    <span id="count"></span>
  </div>
</div>
<div id="fileinfo"></div>
<div id="colheads"><div>Rocq / μSail</div><div>Sail (original)</div></div>
<div id="rows"></div>
<script type="application/json" id="data">/*__DATA__*/</script>
<script>
"use strict";
const DATA = JSON.parse(document.getElementById("data").textContent);
const SAIL = DATA.sail;
const $ = (id) => document.getElementById(id);
const esc = (s) => s.replace(/[&<>"]/g, (c) => ({"&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;"}[c]));

// ---- syntax highlighting --------------------------------------------------
function mkLexer(parts, keywords) {
  const re = new RegExp(parts.map(([, r]) => "(" + r + ")").join("|"), "g");
  const kw = new Set(keywords);
  return function (text) {
    const toks = []; let last = 0, m;
    re.lastIndex = 0;
    while ((m = re.exec(text))) {
      if (m.index > last) toks.push([null, text.slice(last, m.index)]);
      let cls = null;
      for (let i = 0; i < parts.length; i++) if (m[i + 1] !== undefined) { cls = parts[i][0]; break; }
      if (cls === "id") cls = kw.has(m[0]) ? "kw" : null;
      toks.push([cls, m[0]]);
      last = re.lastIndex;
      if (m[0].length === 0) re.lastIndex++;
    }
    if (last < text.length) toks.push([null, text.slice(last)]);
    return toks;
  };
}
const ROCQ_LEX = mkLexer([
  ["com", "\\(\\*[\\s\\S]*?\\*\\)"], ["str", '"(?:[^"]|"")*"'],
  ["bi", "\\b(?:stm|exp|pat|ty|env|ctx)[._][A-Za-z_][\\w']*"],
  ["num", "\\b\\d+(?:%\\w+)?\\b"], ["id", "[A-Za-z_][\\w']*"],
], ["Definition", "Fixpoint", "Inductive", "Record", "Variant", "Lemma", "Theorem", "Proof",
    "Qed", "Defined", "Instance", "Section", "End", "Module", "Require", "Import", "Export",
    "From", "match", "with", "end", "fun", "forall", "exists", "let", "in", "return", "if",
    "then", "else", "Notation", "Arguments", "Hint", "Derive", "for", "Local", "Open", "Scope",
    "Include", "Equations", "Class", "Set", "Type", "Prop", "as", "Obligation", "Tactic",
    "Existing", "Context", "struct"]);
const SAIL_LEX = mkLexer([
  ["com", "/\\*[\\s\\S]*?\\*/|//[^\\n]*"], ["str", '"(?:\\\\.|[^"\\\\])*"'],
  ["num", "\\b0x[0-9A-Fa-f_]+\\b|\\b0b[01_]+\\b|\\b\\d+\\b"], ["tv", "'[A-Za-z_]\\w*"],
  ["id", "[A-Za-z_][\\w']*"], ["bi", "\\$\\w+"],
], ["function", "clause", "val", "type", "enum", "union", "struct", "bitfield", "register",
    "let", "var", "mapping", "overload", "scattered", "match", "if", "then", "else", "foreach",
    "from", "to", "by", "in", "return", "forall", "effect", "pure", "impure", "default",
    "order", "dec", "inc", "assert", "throw", "try", "catch", "end", "bits", "bool", "int",
    "nat", "unit", "string", "bit", "vector", "option", "Some", "None", "true", "false",
    "sizeof", "constraint", "ref", "and", "or", "repeat", "until", "while", "do",
    "backwards", "forwards", "when", "where", "range", "atom", "implicit", "Int", "Type",
    "Bool", "Order", "private", "newtype", "with", "operator"]);

const LINE_H = 18.75;  // pre.code: 12.5px font, line-height 1.5
let compact = false;
try { compact = localStorage.getItem("cmp-compact") === "1"; } catch (e) {}

function renderCode(text, firstLine, lex, docLines) {
  const toks = lex(text);
  const out = []; let line = [], n = firstLine;
  const flush = () => {
    let html = line.join("");
    if (compact) html = html.replace(/^( +)/, (s) => " ".repeat(Math.ceil(s.length / 4)));
    out.push('<span class="line' + (n - firstLine < docLines ? " doc" : "") + '" data-n="' + n + '">' + html + "</span>");
    line = []; n++;
  };
  for (const [cls, s] of toks) {
    const pieces = s.split("\n");
    pieces.forEach((p, i) => {
      if (i > 0) flush();
      if (p) line.push(cls ? '<span class="t-' + cls + '">' + esc(p) + "</span>" : esc(p));
    });
  }
  flush();
  return '<pre class="code">' + out.join("") + "</pre>";
}

function linksHtml(l) {
  if (!l) return "";
  const parts = [];
  if (l.vscode) parts.push('<a href="' + esc(l.vscode) + '" title="Open in VS Code">VS Code</a>');
  if (l.github) parts.push('<a href="' + esc(l.github) + '" target="_blank" rel="noopener" title="Open on GitHub">GitHub</a>');
  if (l.file) parts.push('<a href="' + esc(l.file) + '" target="_blank" title="Open the file (without jumping to the line)">file</a>');
  return '<span class="links" onclick="event.stopPropagation()">' + parts.join("") + "</span>";
}

// ---- rows -------------------------------------------------------------------
let current = null;
const state = {};  // file key -> {rows: [{data, el}], built}

function rowSrcSummary(r) {
  if (r.sail.length) {
    const s = SAIL[r.sail[r.sail.length - 1]];
    return '<span class="src">' + esc(s.file) + ":" + s.def_start + (r.sail.length > 1 ? " +" + (r.sail.length - 1) : "") + "</span>";
  }
  if (r.missing.length) return '<span class="src nosail">' + esc(r.missing[0].file) + ":" + r.missing[0].start + " (source missing)</span>";
  if (r.note) return '<span class="src">' + esc(r.note) + "</span>";
  if (r.kind === "function" || r.kind === "foreign") return '<span class="src nosail">no Sail found</span>';
  return "";
}

function buildFile(f) {
  const st = state[f.key] = {rows: [], file: f};
  const frag = document.createDocumentFragment();
  for (const r of f.rows) {
    const el = document.createElement("section");
    el.className = "row k-" + r.kind;
    el.id = f.key + ":" + r.id;
    const lines = r.end - r.start + 1;
    el.innerHTML = '<div class="rh"><span class="tog">▸</span><span class="badge">' + esc(r.label) +
      '</span><a class="title" href="#' + esc(el.id) + '" onclick="event.stopPropagation()">' + esc(r.title) +
      '</a><span class="src">' + esc(f.file) + ":" + r.start + (lines > 1 ? "–" + r.end : "") +
      "</span>" + rowSrcSummary(r) + linksHtml(r.links) + '</div><div class="body"></div>';
    // Reserve the rendered height up front, so that lazily rendering rows
    // does not move the rest of the page around.
    let sailH = r.note ? 40 : 0;
    for (const i of r.sail) sailH += 26 + (SAIL[i].end - SAIL[i].start + 1) * LINE_H + 12;
    sailH += r.missing.length * 70 + (!r.sail.length && !r.missing.length ? 40 : 0);
    const est = Math.max(lines * LINE_H + 12, Math.min(sailH, window.innerHeight - 60));
    el.querySelector(".body").style.minHeight = Math.round(est) + "px";
    const open = !(r.kind === "infra" || r.kind === "boilerplate");
    if (open) el.classList.add("open");
    el.querySelector(".rh").addEventListener("click", () => toggle(entry));
    const entry = {data: r, el, rendered: false, file: f};
    st.rows.push(entry);
    frag.appendChild(el);
  }
  return frag;
}

function renderBody(entry) {
  const r = entry.data, f = entry.file;
  const body = entry.el.querySelector(".body");
  let sail = "";
  for (const i of r.sail) {
    const s = SAIL[i];
    sail += '<div class="snip"><div class="sh"><span><b>' + esc(s.kind + " " + (s.name || "")) +
      "</b></span><span>" + esc(s.file) + ":" + s.def_start + "–" + s.end + "</span>" + linksHtml(s.links) +
      "</div>" + renderCode(s.text, s.start, SAIL_LEX, s.def_start - s.start) + "</div>";
  }
  for (const m of r.missing) {
    sail += '<div class="snip"><div class="sh"><span>' + esc(m.file) + ":" + m.start + "–" + m.end +
      "</span>" + linksHtml(m.links) + '</div><div class="note warn">Sail file not found at generation time: ' +
      esc(m.path) + "</div></div>";
  }
  if (r.kind === "boilerplate" && r.ref) {
    sail += '<div class="note">Generated instances for <a href="#' + esc(f.key + ":" + r.ref) + '">' +
      esc(r.ref) + "</a>; no Sail counterpart of their own.</div>";
  } else if (!r.sail.length && !r.missing.length && !r.note) {
    sail += '<div class="note' + (r.kind === "function" || r.kind === "foreign" ? " warn" : "") + '">' +
      (r.kind === "infra" ? "Rocq infrastructure of the translation; no Sail counterpart."
                          : "No Sail counterpart found.") + "</div>";
  }
  if (r.note) sail += '<div class="note">' + esc(r.note) + "</div>";
  body.innerHTML = '<div class="col rocq">' + renderCode(r.text, r.start, ROCQ_LEX, 0) +
    '</div><div class="col sail">' + sail + "</div>";
  body.style.minHeight = "";
  entry.rendered = true;
}

const observer = new IntersectionObserver((ents) => {
  for (const e of ents) {
    if (!e.isIntersecting) continue;
    const entry = e.target._entry;
    if (entry && !entry.rendered && entry.el.classList.contains("open")) renderBody(entry);
  }
}, {rootMargin: "1500px 0px"});

function toggle(entry, force) {
  const open = force === undefined ? !entry.el.classList.contains("open") : force;
  entry.el.classList.toggle("open", open);
  if (open && !entry.rendered) renderBody(entry);
}

function showFile(key) {
  current = key;
  const f = DATA.files.find((x) => x.key === key);
  for (const b of $("tabs").children) b.setAttribute("aria-selected", b.dataset.key === key);
  const rows = $("rows");
  rows.innerHTML = "";
  if (!state[key]) {
    const frag = buildFile(f);
    for (const e of state[key].rows) { e.el._entry = e; }
    state[key].frag = frag;
  }
  for (const e of state[key].rows) { rows.appendChild(e.el); observer.observe(e.el); }
  let info = '<span>' + esc(f.path) + " (" + f.lines + " lines) </span>" + linksHtml(f.links);
  if (DATA.missing_sources.length) {
    info += '<div class="warn">' + DATA.missing_sources.length + " Sail file(s) not found at generation time (e.g. " +
      esc(DATA.missing_sources[0]) + "). Rewrite paths with --path-map. Links to the Sail source still work where the location is known.</div>";
  }
  $("fileinfo").innerHTML = info;
  applyFilter();
}

function matches(e, q, filt) {
  const r = e.data;
  const hasSail = r.sail.length > 0 || r.missing.length > 0;
  if (filt === "matched" && !hasSail) return false;
  if (filt === "unmatched" && (hasSail || r.kind === "boilerplate")) return false;
  if (filt === "nobp" && (r.kind === "infra" || r.kind === "boilerplate")) return false;
  if (!q) return true;
  if (r.title.toLowerCase().includes(q)) return true;
  return r.aliases.some((a) => a.toLowerCase().includes(q));
}

function applyFilter() {
  const q = $("q").value.trim().toLowerCase();
  const filt = $("filter").value;
  let shown = 0;
  for (const e of state[current].rows) {
    const ok = matches(e, q, filt);
    e.el.style.display = ok ? "" : "none";
    if (ok) shown++;
  }
  $("count").textContent = shown + " / " + state[current].rows.length + " blocks";
}

function jumpTo(id, flash) {
  const key = id.split(":")[0];
  if (!DATA.files.some((f) => f.key === key)) return;
  if (key !== current) showFile(key);
  const el = document.getElementById(id);
  if (!el) return;
  el.style.display = "";
  toggle(el._entry, true);
  el.scrollIntoView({block: "start"});
  // rows rendered meanwhile may differ slightly from their estimated height
  requestAnimationFrame(() => requestAnimationFrame(() => el.scrollIntoView({block: "start"})));
  if (flash) { el.classList.add("flash"); setTimeout(() => el.classList.remove("flash"), 1200); }
}

// ---- setup --------------------------------------------------------------
for (const f of DATA.files) {
  const b = document.createElement("button");
  b.type = "button"; b.role = "tab"; b.dataset.key = f.key; b.textContent = f.file;
  b.addEventListener("click", () => { showFile(f.key); history.replaceState(null, "", "#" + f.key); window.scrollTo(0, 0); });
  $("tabs").appendChild(b);
}
$("q").addEventListener("input", applyFilter);
$("q").addEventListener("keydown", (ev) => {
  if (ev.key !== "Enter") return;
  const q = $("q").value.trim().toLowerCase();
  const rows = state[current].rows.filter((e) => e.el.style.display !== "none");
  const exact = rows.find((e) => e.data.title.toLowerCase() === q || e.data.aliases.some((a) => a.toLowerCase() === q));
  const hit = exact || rows[0];
  if (hit) { history.replaceState(null, "", "#" + hit.el.id); jumpTo(hit.el.id, true); }
});
$("filter").addEventListener("change", applyFilter);
$("compact").checked = compact;
$("compact").addEventListener("change", () => {
  compact = $("compact").checked;
  try { localStorage.setItem("cmp-compact", compact ? "1" : "0"); } catch (e) {}
  for (const k in state) for (const e of state[k].rows) {
    if (e.rendered) { e.rendered = false; if (e.el.classList.contains("open") && e.el.isConnected) renderBody(e); }
  }
});
$("openall").addEventListener("click", () => {
  for (const e of state[current].rows) if (e.el.style.display !== "none") e.el.classList.add("open");
  // bodies render lazily through the observer
  for (const e of state[current].rows) { observer.unobserve(e.el); observer.observe(e.el); }
});
$("closeall").addEventListener("click", () => {
  for (const e of state[current].rows) e.el.classList.remove("open");
});
function setHdr() { document.documentElement.style.setProperty("--hdr", $("top").offsetHeight + "px"); }
window.addEventListener("resize", setHdr);
window.addEventListener("hashchange", () => { const h = decodeURIComponent(location.hash.slice(1)); if (h.includes(":")) jumpTo(h, true); else if (h) showFile(h); });
document.addEventListener("click", (ev) => {
  const a = ev.target.closest('a[href^="#"]');
  if (!a) return;
  const h = decodeURIComponent(a.getAttribute("href").slice(1));
  if (h.includes(":")) { ev.preventDefault(); history.pushState(null, "", "#" + h); jumpTo(h, true); }
});
setHdr();
const h0 = decodeURIComponent(location.hash.slice(1));
showFile(h0 && DATA.files.some((f) => f.key === h0.split(":")[0]) ? h0.split(":")[0] : DATA.files[DATA.files.length - 1].key);
if (h0.includes(":")) jumpTo(h0, false);
</script>
</body>
</html>
"""


def kv(s):
    if "=" not in s:
        raise argparse.ArgumentTypeError(f"expected FROM=TO, got {s!r}")
    a, b = s.split("=", 1)
    return os.path.abspath(a) if not a.startswith(("http:", "https:")) else a, b


def main():
    ap = argparse.ArgumentParser(description=__doc__.split("\n\n")[0],
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("dir", nargs="?", default=".",
                    help="output directory of the Sail backend (default: current directory)")
    ap.add_argument("--rocq", action="append", metavar="FILE",
                    help="generated Rocq file to show; repeatable "
                         "(default: all *.v in DIR except *.template.v)")
    ap.add_argument("--sources", help="file listing the Sail files in translation order "
                                      "(default: DIR/sources.txt, else the command in the log)")
    ap.add_argument("--log", help="backend log with the nanosail AST dump (default: DIR/log.txt)")
    ap.add_argument("--out", "-o", help="output HTML file (default: DIR/comparison.html)")
    ap.add_argument("--title", help="page title (default: derived from DIR)")
    ap.add_argument("--path-map", action="append", type=kv, default=[], metavar="FROM=TO",
                    help="rewrite Sail source paths starting with FROM to start with TO, "
                         "e.g. when the sources moved since the translation; repeatable")
    ap.add_argument("--sail-lib", action="append", default=[], metavar="DIR",
                    help="directory with Sail's standard library (vector.sail, …); by default "
                         "`sail --dir`/lib and the library directories log.txt points into")
    ap.add_argument("--github", action="append", type=kv, default=[], metavar="DIR=URL",
                    help="files under DIR are linked as URL/<relative path>, e.g. "
                         "DIR=https://github.com/org/repo/blob/<commit>; repeatable. "
                         "Without it, GitHub links are derived from git (origin + HEAD).")
    ap.add_argument("--no-git", action="store_true", help="do not query git for GitHub links")
    args = ap.parse_args()
    if not os.path.isdir(args.dir):
        ap.error(f"not a directory: {args.dir}")
    args.rocq = args.rocq or find_rocq_files(args.dir)
    if not args.rocq:
        ap.error(f"no .v files in {args.dir}")
    args.sources = args.sources or os.path.join(args.dir, "sources.txt")
    args.log = args.log or os.path.join(args.dir, "log.txt")
    args.out = args.out or os.path.join(args.dir, "comparison.html")
    args.title = args.title or default_title(args.dir)

    if not args.sail_lib and shutil.which("sail"):
        try:
            d = subprocess.run(["sail", "--dir"], capture_output=True, text=True,
                               timeout=10).stdout.strip()
            if d:
                args.sail_lib.append(os.path.join(d, "lib"))
        except (OSError, subprocess.TimeoutExpired):
            pass

    data, stats = build(args)
    write_html(data, args.out)
    for key, s in stats.items():
        c = s["counts"]
        print(f"{key}: " + ", ".join(f"{n} {k}" for k, n in sorted(c.items())) +
              (f"; {s['unmatched']} functions without Sail source" if s["unmatched"] else ""),
              file=sys.stderr)
    print(f"{len(data['sail'])} Sail snippets; "
          f"{len(data['missing_sources'])} missing Sail files", file=sys.stderr)
    print(f"wrote {args.out} ({os.path.getsize(args.out) // 1024} KiB)", file=sys.stderr)


if __name__ == "__main__":
    main()
