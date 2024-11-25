#!/usr/bin/env bash
SAIL=sail
COQ=coqc

SAIL_SOURCES="model.sail generated.sail"
TESTED_FILE=microsail.v

if [ -n "$1" ]
then
    OPT="-ddump_rewrite_ast intermediate"
else
    OPT=
fi

ruby generate.rb > generated.sail

$SAIL $OPT -katamaran -katamaran_config configuration.lisp $SAIL_SOURCES
