"""Script for running integration tests.

Simply runs the interpreter and checks for unit test failures.
"""

from subprocess import PIPE, run
import os
import re
import subprocess
import sys
import textwrap

# Name of the interpreter:
NAME = 'typed_imp'

DIR = os.path.dirname(__file__)
LANG_PATH = os.path.join(DIR, './' + NAME)

def abs_of_base_path(fname):
    return os.path.join(DIR, 'tests', fname)

def fetch_test_names():
    tests = os.listdir('tests')
    # Remove all tests that end in ".in".
    # These are test scripts that need to be preprocessed to generate
    # actual test scripts.
    real_tests = []
    for test in tests:
        if not test.endswith('.in'):
            real_tests.append(test)
    return real_tests

def indent(s):
    return textwrap.indent(s, ' ' * 4)

def run_test(name):
    timeout = False
    try:
        test_path = abs_of_base_path(name)
        cp = run([LANG_PATH, test_path], 
                 stdout=PIPE, stderr=PIPE, 
                 timeout=10, encoding='UTF-8')
        stdout = cp.stdout
        stderr = cp.stderr
    except subprocess.TimeoutExpired:
        print(f"ERROR: {name} timed out")
        return False

    pass_regex = re.compile(r'(\d+) of (\d+) unit tests passed')

    match = pass_regex.search(stdout)

    if match is None:
        print(f'{name} failed--unit tests did not run')
        print(f'captured stdout:\n\n{indent(stdout)}\n')
        print(f'captured stderr:\n\n{indent(stderr)}\n')
        return False

    if match[1] != match[2]:
        print(f'{name} failed--not all tests passed')
        print(f'captured stdout:\n\n{indent(stdout)}\n')
        print(f'captured stderr:\n\n{indent(stderr)}\n')
        return False

    print(f'{name}: passed')
    return True

def main():
    tests = fetch_test_names()
    count = 0

    for test in tests:
        count += run_test(test)

    print(f'Tests complete.\n{count}/{len(tests)} tests passed.')

    if count != len(tests):
        sys.exit(1)

if __name__ == '__main__':
    main()

