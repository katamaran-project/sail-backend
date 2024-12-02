#! /usr/bin/env bash


echo Sail to Lem
sail --lem -o output ./model.sail

echo Lem to Isabelle
rm -rf isabelle
mkdir isabelle
lem -isa -outdir ./isabelle -lib Sail=~/repos/sail/src/lem_interp/ -lib Sail=~/repos/sail/src/gen_lib ./output_types.lem ./output.lem



echo Sail to Lem with auto-mono
sail --lem --auto-mono --mono-rewrites -o output_auto_mono ./model.sail

echo Lem to Isabelle
rm -rf isabelle-auto-mono
mkdir isabelle-auto-mono
lem -isa -outdir ./isabelle-auto-mono -lib Sail=~/repos/sail/src/lem_interp/ -lib Sail=~/repos/sail/src/gen_lib ./output_auto_mono_types.lem ./output_auto_mono.lem



echo Sail to Lem with -lem-mwords
sail --lem -o output_mwords -lem-mwords ./model.sail

echo Lem to Isabelle with -lem-mwords
rm -rf isabelle-mwords
mkdir isabelle-mwords
lem -isa -outdir ./isabelle-mwords -lib Sail=~/repos/sail/src/lem_interp/ -lib Sail=~/repos/sail/src/gen_lib ./output_mwords_types.lem ./output_mwords.lem
