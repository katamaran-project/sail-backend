#!/usr/bin/env python3

import os
from pathlib import Path
import subprocess
import psutil
import time
import sys
from datetime import date


# Determines the number of tests that can run in parallel
# Lower this number if you encounter out-of-memory problems
MAXIMUM_QUEUE_SIZE = 20


pass_count = 0
fail_count = 0

script_path = Path('./test.sh').absolute()

if not script_path.is_file():
    print("Did not find test.sh")
    sys.exit(-1)

log = open('tests-output.txt', 'w')


def run_test(directory_name, path):
    return subprocess.Popen(script_path, cwd=str(path), stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)


def wait_for_test(directory_name, path, process):
    global pass_count, fail_count
    
    stdout, stderr = process.communicate()
    result = process.returncode

    log_message = f"""Test {directory_name}
Exit code={result}
Path={path}
    
STDOUT
#{stdout}
    
STDERR
#{stderr}

--------------------------------------------------
"""

    if result == 0:
        print(f"PASS {directory}", flush=True)
        pass_count += 1
    else:
        print(f"FAIL {directory}", flush=True)
        print(log_message, file=log)
        fail_count += 1


directory_names = sorted([entry for entry in os.listdir() if os.path.isdir(entry)])        

queue = []

for directory_name in directory_names:
    if len(queue) >= MAXIMUM_QUEUE_SIZE:
        directory, path, process = queue.pop(0)
        wait_for_test(directory, path, process)

    path = Path(directory_name).absolute()
    process = run_test(directory_name, path)
    queue.append((directory_name, path, process))


for directory, path, process in queue:
    wait_for_test(directory, path, process)

with open('tests-history.txt', 'a') as file:
    today = date.today()
    formatted_today = today.strftime('%Y-%m-%d')
    print(f"{formatted_today} PASS:{pass_count} FAIL:{fail_count}", file=file)

print(f"PASS:{pass_count} FAIL:{fail_count}")


log.close()
