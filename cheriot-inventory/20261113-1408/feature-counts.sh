#!/usr/bin/env bash
# Source-level frequency of Sail constructs in the CHERIoT sources that were translated
# (bisect/full-sources.txt; the stubbed vector-register helpers are still counted).
# Counts are grep line counts: approximate, for sizing only.
cd "$(dirname "$0")"
SRCS=(); while IFS= read -r f; do SRCS+=("$f"); done < bisect/full-sources.txt
count() { printf '%-30s %6s lines in %3s files\n' "$1" "$(cat "${SRCS[@]}" | grep -c -E "$2")" "$(grep -l -E "$2" "${SRCS[@]}" | wc -l | tr -d ' ')"; }
count 'foreach'                 '\bforeach\b'
count 'while/repeat'            '\b(while|repeat)\b'
count 'return'                  '\breturn\b'
count 'try'                     '\btry\b'
count 'throw'                   '\bthrow\b'
count 'mapping (def)'           '^\s*mapping\s+[A-Za-z_]'
count 'mapping clause'          '^\s*mapping clause'
count 'scattered'               '^\s*scattered'
count 'function clause'         '^\s*function clause'
count 'overload'                '^\s*overload'
count 'union clause'            '^\s*union clause'
count 'bitfield'                '^\s*bitfield'
count 'register'                '^\s*register\b'
count 'top-level let'           '^let\b'
count 'Some/None'               '\b(Some|None)\b'
count 'Ok(/Err('                '\b(Ok|Err)\s*\('
count 'MemValue/MemException'   '\b(MemValue|MemException)\b'
count 'struct'                  '^\s*struct\b'
count 'field assign x.f = e'    '\b[a-zA-Z_]+\.[A-Za-z_]+\s*=[^=>]'
count 'bitfield assign x[F] = e' '\b[a-zA-Z_]+\[[A-Z][A-Za-z_]*\]\s*=[^=>]'
count 'var'                     '\bvar\b'
count 'zero/sign_extend'        '\b(zero_extend|sign_extend|EXTZ|EXTS)\b'
count 'match'                   '\bmatch\b'
count 'assert('                 '\bassert\('
count 'forall (polymorphic val)' '^\s*val\b.*forall'
count "type var 'a ('a : Type)" "\('[a-z]+ *: *Type\)"
