#!/usr/bin/env bash
SAIL=sail

SAIL_SOURCES=model.sail
TESTED_FILE=microsail.v


$SAIL -katamaran -katamaran_config configuration.lisp $SAIL_SOURCES
DIFF=$(diff $TESTED_FILE expected/$TESTED_FILE)
if [ "$DIFF" ]; then
    exit -1;
fi
