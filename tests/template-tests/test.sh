#!/usr/bin/env bash

for dir in `./list-tests`; do
    (
        cd $dir;
        echo "Testing $(basename `pwd`)";
        ./test.sh;
        if [ $? != 0 ]; then
            echo "FAILED $dir";
            exit -1
        fi
    )
done
