#!/usr/bin/env bash

set -e

SAIL=sail
COQ=coqc

SAIL_SOURCES=model.sail

if [[ -v DUMP_REWRITE ]]
then
    OPT="-ddump_rewrite_ast intermediate"
else
    OPT=
fi

ruby generate.rb > generated.sail

$SAIL $OPT -katamaran -katamaran_config configuration.lisp $SAIL_SOURCES

if [[ ! -v NO_COQ ]]
then
    $COQ base.v
    $COQ machine.v
fi
