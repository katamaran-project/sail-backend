#! /usr/bin/env bash


function build() {
    echo $1
    rm -rf output-$1
    mkdir output-$1

    echo Sail to Lem
    (cd output-$1; sail --lem $2 -o output ../model.sail)

    echo Lem to Isabelle
    (cd output-$1; lem -isa -outdir . -lib Sail=~/repos/sail/src/lem_interp/ -lib Sail=~/repos/sail/src/gen_lib ./output_types.lem ./output.lem)

    echo ---
}


build "standard" ""
build "automono" "--auto-mono --mono-rewrites"
build "mwords" "--lem-mwords"
