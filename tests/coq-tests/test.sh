#!/usr/bin/env bash

set -e

SAIL=sail
COQ=coqc

SAIL_SOURCES="model.sail"

git clean -fX

OPT=""

if [[ -v DUMP ]]
then
    echo "DUMP environment variable is recognized"
    EXTRA_OPT="--ddump-rewrite-ast intermediate"
    echo "Extra flags to sail: $EXTRA_OPT"
    OPT+="$EXTRA_OPT"
fi

if [[ -v MONOMO ]]
then
    echo "MONOMO environment variable is recognized"
    EXTRA_OPT+=" --auto-mono --mono-rewrites"
    echo "Extra flags to sail: $EXTRA_OPT"
    OPT+="$EXTRA_OPT"
fi

echo "All flags to sail: $OPT"
$SAIL $OPT --katamaran --katamaran-config configuration.lisp $SAIL_SOURCES


if [[ -v DUMP ]]
then
    echo "Starting process-rewrites.rb"
    ../process-rewrites.rb
    echo "Finished process-rewrites.rb"
fi


if [[ ! -v NO_COQ ]]
then
    echo "Starting Rocq compiler on base.v"
    $COQ base.v
    echo "Finished Rocq compiler on base.v"
    echo "Starting Rocq compiler on machine.v"
    $COQ machine.v
    echo "Finished Rocq compiler on machine.v"
fi
