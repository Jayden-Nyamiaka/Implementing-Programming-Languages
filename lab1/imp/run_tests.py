'''Script for running `imp` integration tests.

Simply runs the `imp` interpreter and checks for imp unit test failures.
'''

from subprocess import PIPE, run
import os
import re
import subprocess
import sys
import glob
import textwrap

test_files_to_exclude = ['extra.imp']

DIR = os.path.dirname(__file__)
IMP_PATH = os.path.join(DIR, 'imp')

def abs_of_base_path(fname):
    return os.path.join(DIR, 'tests', fname)

def fetch_test_names():
    names = glob.glob('*.imp', root_dir='tests')
    names.sort()
    for name in test_files_to_exclude:
        if name in names:
            names.remove(name)
    return names

def indent(s):
    return textwrap.indent(s, ' ' * 4)

def run_test(name):
    timeout = False
    stdin  = sys.stdin
    stdout = sys.stdout
    try:
        test_path = abs_of_base_path(name)
        cp = run([IMP_PATH, test_path], 
                 stdout=PIPE, stderr=PIPE, 
                 timeout=10, encoding='UTF-8')
        stdout = cp.stdout
        stderr = cp.stderr
    except subprocess.TimeoutExpired:
        timeout = True

    if timeout:
        print(f'{name} timed out')
        print(f'captured stdout:\n\n{indent(stdout)}\n')
        print(f'captured stderr:\n\n{indent(stderr)}\n')
        return False

    pass_regex    = re.compile(r'(\d+) of (\d+) unit tests passed')
    fail_regex    = re.compile('failed')
    unknown_regex = re.compile('Unknown name')
    fatal_regex   = re.compile('Fatal error')

    pass_match    = pass_regex.search(stdout)
    fail_match    = fail_regex.search(stderr)
    fatal_match   = fatal_regex.search(stderr)
    unknown_match = unknown_regex.search(stderr)

    if fatal_match is not None:
        print(f'{name} failed (fatal error)')
        print(f'captured stdout:\n\n{indent(stdout)}\n')
        print(f'captured stderr:\n\n{indent(stderr)}\n')
        return False

    if unknown_match is not None:
        print(f'{name} failed (unknown name)')
        print(f'captured stdout:\n\n{indent(stdout)}\n')
        print(f'captured stderr:\n\n{indent(stderr)}\n')
        return False

    if (pass_match is None) and (fail_match is None):
        print(f'{name} failed--unit tests did not run')
        print(f'captured stdout:\n\n{indent(stdout)}\n')
        print(f'captured stderr:\n\n{indent(stderr)}\n')
        return False

    if fail_match is not None:
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
