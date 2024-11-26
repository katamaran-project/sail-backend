#!/usr/bin/env bash
SAIL=sail
COQ=coqc

SAIL_SOURCES=model.sail

if [ -n "$1" ]
then
    OPT="-ddump_rewrite_ast intermediate"
else
    OPT=
fi

$SAIL $OPT -katamaran -katamaran_config configuration.lisp $SAIL_SOURCES
