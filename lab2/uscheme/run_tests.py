'''
Script for running `uscheme` integration tests.

Simply runs the `uscheme` interpreter and checks for unit test failures.
'''

from fnmatch import fnmatch
from subprocess import PIPE, run

import os
import re
import subprocess
import sys
import textwrap


DIR = os.path.dirname(__file__)
EXEC_PATH = os.path.join(DIR, './uscheme')

def abs_of_base_path(fname):
    return os.path.join(DIR, 'tests', fname)

def fetch_test_names():
    return list(filter(lambda f: fnmatch(f, '*.scm'), os.listdir('tests')))

def indent(s):
    return textwrap.indent(s, ' ' * 4)

def run_test(name):
    timeout = False
    stdout = None
    stderr = None
    try:
        test_path = abs_of_base_path(name)
        cp = run([EXEC_PATH, test_path], 
                 stdout=PIPE, stderr=PIPE, 
                 timeout=5, encoding='UTF-8')
        stdout = cp.stdout
        stderr = cp.stderr
    except subprocess.TimeoutExpired:
        timeout = True

    if timeout:
        print(f'>>> FAILED: {name} timed out')
        if stdout:
            print(f'captured stdout:\n\n{indent(stdout)}\n')
        if stderr:
            print(f'captured stderr:\n\n{indent(stderr)}\n')
        return False

    pass_regex = re.compile(r'(\d+) of (\d+) unit tests passed')

    if stdout:
        match = pass_regex.search(stdout)

        if match is None:
            print(f'>>> FAILED: {name}: unit tests did not run')
            print(f'captured stdout:\n\n{indent(stdout)}\n')
            print(f'captured stderr:\n\n{indent(stderr)}\n')
            return False

        if match[1] != match[2]:
            print(f'>>> FAILED: {name}: not all tests passed')
            print(f'captured stdout:\n\n{indent(stdout)}\n')
            print(f'captured stderr:\n\n{indent(stderr)}\n')
            return False

    if stderr:
        print(f'captured stderr:\n\n{indent(stderr)}\n')
        return False

    print(f'{name}: passed')
    return True

def run_syntax_tests():
    print('---------- RUNNING SYNTAX TESTS ----------\n')
    os.chdir('tests/syntax')
    cp = run(['./test_syntax_errors'])
    os.chdir('../..')
    return cp.returncode == 0

def run_unit_tests():
    print('---------- RUNNING UNIT TESTS ----------\n')
    tests = fetch_test_names()
    count = 0

    for test in tests:
        count += run_test(test)

    print(f'Unit tests complete.\n{count}/{len(tests)} unit tests passed.')

    return count == len(tests)

if __name__ == '__main__':
    print()
    syntax_test_results = run_syntax_tests()
    print()
    unit_test_results = run_unit_tests()
    print()
    unit_test_results = True

    print('---------- END OF TESTS ----------\n')

    if not (syntax_test_results and unit_test_results):
        sys.exit(1)

