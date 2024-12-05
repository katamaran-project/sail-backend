#!/usr/bin/env bash

set -e

SAIL=sail
COQ=coqc

SAIL_SOURCES=model.sail

git clean -fX


OPT="--ddump-rewrite-ast intermediate"

if [[ -v MONOMO ]]
then
    OPT+=" --auto-mono --mono-rewrites"
fi


$SAIL $OPT -katamaran -katamaran_config configuration.lisp $SAIL_SOURCES

../process-rewrites.rb
