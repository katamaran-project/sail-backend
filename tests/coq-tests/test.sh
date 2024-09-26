s#!/usr/bin/env bash

pass_count=0
fail_count=0

run_test() {
    dir=$1

    # pushd $dir > /dev/null; */
    cd $dir;
    ./test.sh > /dev/null 2> /dev/null;
    if [ $? != 0 ]; then
        echo "FAIL $dir";
        fail_count=$(($fail_count+1));
    else
        echo "PASS $dir";
        pass_count=$(($pass_count+1));
    fi
    # popd > /dev/null;
}

for dir in `find -mindepth 1 -maxdepth 1 -type d | sort`; do
    (run_test $dir)
done



echo PASS $pass_count
echo FAIL $fail_count


printf "%(%Y-%m-%d)T PASS:$pass_count FAIL:$fail_count\n" >> results.txt
