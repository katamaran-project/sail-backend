#!/usr/bin/env python3

import os
from pathlib import Path
import subprocess
import psutil
import time

MAXIMUM_QUEUE_SIZE = 20


def get_free_memory():
    vm = psutil.virtual_memory()
    return 100 - vm.percent


def wait_for_test(directory, process):
    result = process.wait()
    if result == 0:
        print(f"PASS {directory}")
    else:
        print(f"FAIL {directory}")


directories = sorted([Path(entry).absolute() for entry in os.listdir() if os.path.isdir(entry)])

queue = []

for directory in directories:
    if len(queue) >= MAXIMUM_QUEUE_SIZE:
        directory, process = queue.pop(0)
        wait_for_test(directory, process)

    script_path = directory / 'test.sh'
    process = subprocess.Popen(script_path, cwd=str(directory), stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    queue.append((directory, process))


for directory, process in queue:
    wait_for_test(directory, process)
