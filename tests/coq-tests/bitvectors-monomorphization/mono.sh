#! /usr/bin/env bash


sail --auto-mono --mono-rewrites --lem -o output-words -lem-mwords ./model.sail
sail --auto-mono --mono-rewrites --lem -o output ./model.sail

# lem -isa -outdir ./temp/ -lib Sail=~/repos/sail/src/lem_interp/ -lib Sail=~/repos/sail/src/gen_lib ./output_types.lem ./output.lem
