#!/bin/bash

files=(*)
last_file=

for file in ${files[@]}
do
  if [ -f $last_file ] && cmp -s "$file" "$last_file"
  then
    rm $file
  else
    last_file=$file
  fi
done