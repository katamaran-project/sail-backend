#!/bin/env bash

indent_level=0

indent_skip=2

while IFS="" read -r p
do
	# decrease indentation level before printing an enter
	if [[ "$p" =~ " Entering " ]]
	then
		indent_level=$(( "$indent_level" + 1 ))
	fi

	# print indent_level
	printf "%05i" "$indent_level"

	# print the line with indent_level many indentations of indent_skip spaces.
	printf "%*s%s\n" $(( indent_level * indent_skip )) "" "$p"
	
	# decrease indentation level after an exit
	if [[ "$p" =~ " Exiting " ]]
	then
		indent_level=$(( "$indent_level" - 1 ))
	fi
done
