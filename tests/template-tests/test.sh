#!/usr/bin/env bash
# Translates model.sail and compares the result with expected/; run in a test's directory.
SAIL=${SAIL:-sail}

SAIL_SOURCES=model.sail
TESTED_FILE=microsail.v

$SAIL --katamaran --katamaran-config configuration.lisp $SAIL_SOURCES || exit 1
diff -u "expected/$TESTED_FILE" "$TESTED_FILE" || exit 1
