#!/usr/bin/env bash

set -e

SAIL=sail

SAIL_SOURCES="model.sail"

VERBOSE=0

while getopts "s:dmvnD:" opt; do
    case $opt in
        s)
            SAIL_SOURCES="$OPTARG"
            ;;
        d)
            DUMP=1
            ;;
        m)
            MONOMO=1
            ;;
        v)
            VERBOSE="$(( VERBOSE + 1 ))"
            ;;
        n)
            DRY_RUN=1
            ;;
        D)
            EXECUTION_DIRECTORY="$OPTARG"
            ;;
        *)
            echo "Wrong flag passed. Aborting" > /dev/stderr
            exit 1
            ;;
    esac
done

if [[ -v EXECUTION_DIRECTORY ]]
then
    echo "Changing working directory to $EXECUTION_DIRECTORY"
    cd "$EXECUTION_DIRECTORY"
    if [[ ! "$?" -eq 0 ]]
    then
        echo "Error changing directory. Aborting" > /dev/stderr
    fi
fi

# remove git-ignored files from the current directory
git clean -fX

OPT="--katamaran --katamaran-config configuration.lisp"

if [[ -v VERBOSE ]]
then
    echo "VERBOSE level: $VERBOSE"
fi

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

echo "Full sail call: $SAIL $OPT $SAIL_SOURCES"
if [[ -v DRY_RUN ]]
then
    echo "Dry run, not actually calling sail"
    exit 0
else
    $SAIL $OPT $SAIL_SOURCES
fi

if [[ -v DUMP ]]
then
    echo "Starting process-rewrites.rb"
    ../process-rewrites.rb
    echo "Finished process-rewrites.rb"
fi
