#!/usr/bin/env python3

import os
from pathlib import Path
import subprocess
import psutil
import time
import sys
from datetime import datetime

# use git as an optional dependency
try:
    import git
    git_available=True
except:
    git_available=False


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

with open('tests-history.csv', 'a') as file:

    # get the current timestamp
    now = datetime.now()
    formatted_now = now.strftime('%Y-%m-%dT%H:%M:%S+%Z')

    # find the git information, if we have git available
    if git_available:
        # get the current commit hash
        try:
            repo = git.Repo(search_parent_directories=True)
        except git.exc.InvalidGitRepositoryError:
            printf(f"We appear not to be in a git repository. Panicking.")

        sha_prefix = repo.head.object.hexsha[0:7]

        # determine if any files have been changed
        changed_files = bool(repo.index.diff(None))
    else:
        sha_prefix = "unknown"
        changed_files = "unknown"

    # report the number of passes and fails
    print(f"{formatted_now},{sha_prefix},{changed_files},{pass_count},{fail_count}", file=file)

print(f"PASS:{pass_count} FAIL:{fail_count}")


log.close()
