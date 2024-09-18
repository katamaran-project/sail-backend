#!/usr/bin/env bash

pass_count=0
fail_count=0

for dir in `find -mindepth 1 -maxdepth 1 -type d | sort`; do
    (
        cd $dir;
        echo "Testing $(basename `pwd`)";
        ./test.sh > /dev/null 2> /dev/null;
        if [ $? != 0 ]; then
            echo "FAILED $dir";
            fail_count=$((fail_count+1));
        else
            pass_count=$((pass_count+1));
        fi
    )
done


echo PASS $pass_count
echo FAIL $fail_count
