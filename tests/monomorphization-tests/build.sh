#! /usr/bin/env bash


# echo Sail to Lem */
# sail --auto-mono --mono-rewrites --lem -o output ./model.sail */

# echo Lem to Isabelle */
# rm -rf isabelle */
# mkdir isabelle */
# lem -isa -outdir ./isabelle -lib Sail=~/repos/sail/src/lem_interp/ -lib Sail=~/repos/sail/src/gen_lib ./output_types.lem ./output.lem */

echo Sail to Lem with -lem-mwords
sail --lem -o output-mwords -lem-mwords ./model.sail

echo Lem to Isabelle with -lem-mwords
rm -rf isabelle-mwords
mkdir isabelle-mwords
lem -isa -outdir ./isabelle-mwords -lib Sail=~/repos/sail/src/lem_interp/ -lib Sail=~/repos/sail/src/gen_lib ./output-mwords_types.lem ./output-mwords.lem
