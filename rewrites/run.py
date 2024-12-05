#!/usr/bin/env python3

import os
from pathlib import Path
import subprocess
import psutil
import time
import sys
from datetime import date


MAXIMUM_QUEUE_SIZE = 20


script_path = Path('./compile.sh').absolute()

if not script_path.is_file():
    print("Did not find test.sh")
    sys.exit(-1)


def run_process(directory_name, path):
    return subprocess.Popen(script_path, cwd=str(path), stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)


def wait_for_process(directory_name, path, process):
    print(directory_name)
    global pass_count, fail_count
    
    stdout, stderr = process.communicate()
    result = process.returncode


directory_names = sorted([entry for entry in os.listdir() if os.path.isdir(entry)])        

queue = []

for directory_name in directory_names:
    if len(queue) >= MAXIMUM_QUEUE_SIZE:
        directory, path, process = queue.pop(0)
        wait_for_process(directory, path, process)

    path = Path(directory_name).absolute()
    process = run_process(directory_name, path)
    queue.append((directory_name, path, process))


for directory, path, process in queue:
    wait_for_process(directory, path, process)
