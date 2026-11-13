#!/usr/bin/env bash
# Usage: try.sh <name> <source-list-file> [configuration.lisp]
# Runs sail-local --katamaran on the given sources in bisect/<name>/.
# Extra sail arguments (e.g. -splice FILE) can be given in $EXTRA.
# Reports OK, CRASH (Sail's type_error.ml "Unreachable code") or FAIL.
set -u
here="$(cd "$(dirname "$0")" && pwd)"
SB="$HOME/phd/PROJECTS/sail-backend"
config="${3:-$here/../full/configuration.lisp}"
out="$here/$1"; mkdir -p "$out"
cp "$config" "$out/configuration.lisp"; cp "$here/../full/"*.template.v "$out/"
cp "$2" "$out/sources.txt"; echo "${EXTRA:-}" > "$out/extra-args.txt"
cd "$out" && VERBOSE="${VERBOSE:-4}" "$SB/bin/sail-local" ${EXTRA:-} --katamaran --katamaran-config configuration.lisp $(cat sources.txt) > log.txt 2>&1
code=$?
if grep -q 'Unreachable code' log.txt; then verdict=CRASH; elif [ $code -eq 0 ]; then verdict=OK; else verdict="FAIL($code)"; fi
echo "$1: exit=$code $verdict | $(sed 's/\x1b\[[0-9;]*m//g' log.txt | grep -m1 -A4 -E '^(Type error|Internal error|Error)' | tr '\n' ' ' | cut -c1-400)"
