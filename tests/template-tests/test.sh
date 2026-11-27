#!/usr/bin/env bash
# Translates model.sail and compares the result with expected/; run in a test's directory.
# Every file in expected/ must be generated identically, and a directory in expected/
# (such as Machine/ for preserve-file-structure) must not get other .v files, also not in subdirectories.
SAIL=${SAIL:-sail}

SAIL_SOURCES=model.sail

$SAIL --katamaran --katamaran-config configuration.lisp $SAIL_SOURCES || exit 1

failed=0
for file in $(cd expected && find . -type f | sort); do
    diff -u "expected/$file" "$file" || failed=1
done
for directory in $(cd expected && find . -mindepth 1 -maxdepth 1 -type d | sort); do
    for file in $(find "$directory" -name '*.v' | sort); do
        if [[ ! -f "expected/$file" ]]; then
            echo "unexpected file $file"
            failed=1
        fi
    done
done
exit $failed
