#!/usr/bin/env bash

set -e

SAIL=sail
COQ=coqc

SAIL_SOURCES=model.sail
TESTED_FILE=microsail.v

if [ -n "$1" ]
then
    OPT="-ddump_rewrite_ast intermediate"
else
    OPT=
fi

$SAIL $OPT -katamaran -katamaran_config configuration.lisp $SAIL_SOURCES
$COQ base.v
$COQ machine.v
