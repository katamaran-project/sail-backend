#!/usr/bin/env bash

for dir in `find -mindepth 1 -maxdepth 1 -type d`; do
    (
        cd $dir;
        echo "Testing $(basename `pwd`)";
        ./test.sh;
        if [ $? != 0 ]; then
            echo "FAILED $?";
            exit -1
        fi
    )
done
