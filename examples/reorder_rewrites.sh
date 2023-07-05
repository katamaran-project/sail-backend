#!/bin/bash

base=${1}_rewrite_
base_length=`expr length $base`

for file in ${base}?_*
do
  if [ -f $file ]
  then
    new_file=${file::$base_length}0${file:$base_length}
    mv $file $new_file
  fi
done