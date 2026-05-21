#!/usr/bin/env python3

# accepts (size, name) in fd 0
# can be obtained with
# find tests/coq-tests/ -iname 'model.sail' -print0 | xargs -0 -I % sh -c "wc -c %" | sed "s,/model.sail,,; s, /.*/, ,g"

# accepts (passfail, name) in fd 3
# can be obtained from tests-overview.txt

# prints (size, passfail, name) in fd 1

import os

with os.fdopen(0, "r") as zero:
    # read lines with the sizes from fd 0
    zerolines = [line.split(' ') for
                 line in zero.read().splitlines()]

    # convert size to int
    zerolines = \
        list(
            map(
                lambda tuple : (int(tuple[0]), tuple[1]),
                zerolines
            )
        )

    # sort on size
    zerolines.sort(key=lambda x : x[0])

with os.fdopen(3, "r") as three:
    # read lines with passfail from fd 3
    threelines = [line.split(' ') for
                  line in three.read().splitlines()]

    # sort on name for time linearithmic complexity
    threelines.sort(key=lambda x : x[1])

# loop over the size lines
for (size, name) in zerolines:
    # find the matching passfail from that list
    passfail = [x for x in threelines
                if x[1] == name
                ][0][0]

    # print the total to the stdout
    print("{0} {1} {2}".format(size, passfail, name))
