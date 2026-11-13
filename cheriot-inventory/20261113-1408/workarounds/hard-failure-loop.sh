#!/usr/bin/env bash
# Repeatedly translate the full model; whenever µSail generation aborts with a hard
# "Error occurred during generation" in the body of a function, add that function to
# hard-failure-ignores.txt (with the error) and try again.
set -u
W="$(cd "$(dirname "$0")" && pwd)"; RUN="$(dirname "$W")"; B="$RUN/bisect"
KAT_CONFIG="$HOME/phd/PROJECTS/katamaran-tex-cheriot/case_study/Cheriot/configuration.lisp"
for i in $(seq 1 ${MAX:-60}); do
  python3 "$W/make-config.py" "$KAT_CONFIG" "$W/hard-failure-ignores.txt" "$W/configuration.lisp"
  VERBOSE=2 EXTRA="-splice $W/splice.sail" "$B/try.sh" loop "$B/full-sources.txt" "$W/configuration.lisp" > /dev/null
  log="$B/loop/log.txt"
  err=$(sed 's/\x1b\[[0-9;]*m//g' "$log" | grep -A6 'Error occurred during generation')
  if [ -z "$err" ]; then
    echo "iteration $i: no hard generation failure; $(sed 's/\x1b\[[0-9;]*m//g' "$log" | grep -m1 -E '^(Type error|Internal error|Error)' || echo 'translation finished')"; exit 0
  fi
  fn=$(echo "$err" | grep -m1 -o -E 'in body of function [A-Za-z0-9_#]+' | awk '{print $NF}')
  if [ -z "$fn" ]; then echo "iteration $i: failure outside a function body:"; echo "$err"; exit 1; fi
  echo "$fn" >> "$W/hard-failure-ignores.txt"
  { echo "### $fn"; echo "$err"; echo; } >> "$W/hard-failures.log"
  echo "iteration $i: $fn | $(echo "$err" | head -1 | cut -c1-200)"
done
