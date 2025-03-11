#!/usr/bin/env bash

set -e

SAIL=sail
COQ=coqc

SAIL_SOURCES=model.sail

git clean -fX

if [[ -v DUMP ]]
then
    OPT="--ddump-rewrite-ast intermediate"
else
    OPT=""
fi

if [[ -v MONOMO ]]
then
    OPT+=" --auto-mono --mono-rewrites"
fi


$SAIL $OPT --katamaran --katamaran-config configuration.lisp $SAIL_SOURCES


if [[ -v DUMP ]]
then
    ../process-rewrites.rb
fi


if [[ ! -v NO_COQ ]]
then
    $COQ base.v
    $COQ machine.v
fi
