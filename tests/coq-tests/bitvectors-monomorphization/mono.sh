#! /usr/bin/env bash


sail --lem -o output_words -lem-mwords ./model.sail
# sail --auto-mono --lem -o output ./model.sail

rm -rf temp
mkdir temp
lem -isa -outdir ./temp/ -lib Sail=~/repos/sail/src/lem_interp/ -lib Sail=~/repos/sail/src/gen_lib ./output_words_types.lem ./output_words.lem
