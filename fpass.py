#!/usr/bin/python3
# Find a pass-name in password-store store using one or more search terms.
# If exactly one pass-name is found, "pass show" command will be invoked with it.
# Usage: fpass.py [options] [word1 [word2] ...]

import os
import sys
import glob

def print_err(msg):
    print(msg, file=sys.stderr)

store_path = os.path.expanduser("~/.password-store")
os.chdir(store_path)

options = []
search_terms = []
for arg in sys.argv[1:]:
    if arg.startswith("-"):
        options.append(arg)
    else:
        search_terms.append(arg)

results = []
for f in glob.glob("**/*.gpg", recursive=True):
    f = f[0:-4]
    all_match = True
    for search_term in search_terms:
        if search_term not in f:
            all_match = False
            break
    if all_match:
        results.append(f)

num_results = len(results)
if num_results == 0:
    print_err("\nNo matching pass-name found")
    exit(1)
else:
    print_err("\nFound %d pass-name(s):" % num_results)
    for f in results:
        print_err(f)
    if num_results == 1:
        cmd_array = ["pass", "show"]
        cmd_array.extend(options)
        cmd_array.extend(results)
        cmd = " ".join(cmd_array)
        print_err("\nRunning '%s' ..." % cmd)
        exit(os.system(cmd))
    else:
        print_err("\nTry adding more words to narrow down.")
        exit(1)
