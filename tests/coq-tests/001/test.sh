#!/usr/bin/env bash
SAIL=sail
COQ=coqc

SAIL_SOURCES=model.sail
TESTED_FILE=microsail.v

$SAIL -ddump_rewrite_ast intermediate -katamaran -katamaran_config configuration.lisp $SAIL_SOURCES
# $SAIL -katamaran -katamaran_config configuration.lisp $SAIL_SOURCES
$COQ microsail.v
