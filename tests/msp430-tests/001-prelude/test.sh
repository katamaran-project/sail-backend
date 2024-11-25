#!/usr/bin/env bash

set -e

SAIL=sail
COQ=coqc

SAIL_SOURCES=model.sail

echo $SAIL_SOURCES

if [ -n "$1" ]
then
    OPT="-ddump_rewrite_ast intermediate"
else
    OPT=
fi

ruby generate.rb > generated.sail

$SAIL $OPT -katamaran -katamaran_config configuration.lisp $SAIL_SOURCES
$COQ base.v
$COQ machine.v
