#!/bin/env python3
import os
import subprocess
import sys

# TODO: Harcoded
COMPILER="/home/momoyon/Programming/python/lang/lang"
TESTS_DIR="./tests/"
SUFFIX=".momo"

class Test:
    expected_stdout = ''
    expected_stderr = ''
    expected_returncode = -1

    def __init__(self, name):
        self.name = name

        def read_or_create_expected_file(name: str) -> str:
            f = f"{self.name}.{name}.expected"
            if not os.path.exists(f):
                with open(f, "w") as file:
                    print(f"[INFO] Created empty {self.name}.{name}.expected")
                return ""
            else:
                with open(f, "r") as file:
                     return file.read()

        self.expected_stdout = read_or_create_expected_file("out")
        self.expected_stderr = read_or_create_expected_file("err")
        self.expected_returncode = read_or_create_expected_file("code")
        if self.expected_returncode == '':
            self.expected_returncode = -1
        else:
            self.expected_returncode = int(self.expected_returncode)

        # if self.expected_stdout: print(f"{self.name}.out.expected: {self.expected_stdout}")
        # if self.expected_stderr: print(f"{self.name}.err.expected: {self.expected_stderr}")
    def save_expected(self):
        def write_expected(name: str, content: str):
            f = f"{self.name}.{name}.expected"
            with open(f, "w") as file:
                file.write(content)

        write_expected("out", self.expected_stdout)
        write_expected("err", self.expected_stderr)
        write_expected("code", str(self.expected_returncode))


def usage(program: str):
    print(f"Usage: {program} <subcmd> [flags]")

# NOTE: We named this hhelp because help is a builtin python function
def hhelp():
    print('''
    Subcommands:
        help    - Prints this help message.
        build   - Builds all the tests.
        run     - Runs all the tests.
        record  - Records the expected behaviour of all the tests.

    Flags:
        -h      - Same as the help subcommand.
        -v      - Verbose output.
        -x      - Stop on first error.
          ''')

def vlog(verbose_output, msg):
    if verbose_output:
        print(msg)

def main():
    program = sys.argv.pop(0)

    if len(sys.argv) <= 0:
        print("[ERROR] Please provide at least one subcommand!", file=sys.stderr)
        usage(program)
        hhelp()
        exit(1)


    flags = []

    # FLAG_VALUES
    verbose_output = False
    stop_on_error  = False

    subcmds = []

    while len(sys.argv) > 0:
        arg = sys.argv.pop(0)

        if arg.startswith('-') or arg.startswith('/'):
            flags.append(arg)
        else:
            subcmds.append(arg)

    # Parse flags
    for flag_with_prefix in flags:
        flag = flag_with_prefix[1:]
        if flag == 'h':
            hhelp()
            exit(0)
        elif flag == 'v':
            verbose_output = True
        elif flag == 'x':
            stop_on_error = True
        else:
            print(f"[ERROR] Invalid flag '{flag}'", file=sys.stderr)
            exit(1)

    if len(subcmds) <= 0:
        print("[ERROR] Please provide at least one subcommand!", file=sys.stderr)
        usage(program)
        hhelp()
        exit(1)

    os.chdir(TESTS_DIR)

    tests = {}

    for e in sorted(os.listdir(os.getcwd())):
        if not e.endswith(SUFFIX): continue
        base_name = e.removesuffix(SUFFIX)
        if not tests.get(base_name):
            tests[base_name] = Test(base_name)

    for subcmd in subcmds:
        total_tests_count = len(tests)
        current_test_id = 0
        passing_tests_count = 0

        if subcmd == "help":
            hhelp()
            exit(0)
        elif subcmd == "build":
            print(f'----- [BUILD] -----')
            for test_name in tests:
                print(f'+ Building {test_name} [{current_test_id+1}/{total_tests_count}]...')
                current_test_id += 1
                test = tests[test_name]

                cmd = [COMPILER, f"{test_name}{SUFFIX}"]
                vlog(verbose_output, f"[CMD] {cmd}")
                res = subprocess.run(cmd,
                                     capture_output = True,
                                     text = True)
                if res.returncode != 0:
                    print("[FAILED] ", end='')
                    if res.stderr:
                        print(f"{res.stderr}")
                    else:
                        print('')
                    if stop_on_error: exit(1)
                    else: continue
                else:
                    passing_tests_count += 1
                    print("[PASS] ", end='')
                    o = False
                    if res.stdout:
                        print(f"{res.stdout}")
                        o = True
                    if verbose_output and res.stderr:
                        print(f"{res.stderr}")
                        o = True
                    if not o: print('')

                print(f"Build {passing_tests_count}/{total_tests_count} tests")
        elif subcmd == "run":
            print(f'----- [RUN] -----')
            for test_name in tests:
                print(f'+ Running {test_name} [{current_test_id+1}/{total_tests_count}]...')
                current_test_id += 1
                test = tests[test_name]

                res = None
                try:
                    cmd = [f"./{test_name}"]
                    vlog(verbose_output, f"[CMD] {cmd}")
                    res = subprocess.run(cmd, capture_output = True, text = True)
                except Exception as e:
                    print(f"[ERROR] Failed to run ./{test_name}: {e}")
                    if stop_on_error: exit(1)
                    else: continue

                if test.expected_returncode == -1:
                    print(f"[WARNING] Test doesn't have any expected returncode!")
                    print(f"[WARNING] Please record the expected behaviour of the test using the 'record' subcommand!")

                if res.stdout != test.expected_stdout:
                    print('[FAILED]', file=sys.stderr)
                    print(f"Expected: >>>{test.expected_stdout}>>>")
                    print(f"But Got: >>>{res.stdout}>>>")
                    if stop_on_error: exit(1)
                    else: continue
                passing_tests_count += 1
                print('[PASS]')

            print(f"PASSED {passing_tests_count}/{total_tests_count}")
        elif subcmd == "record":
            print(f'----- [RECORD] -----')
            for test_name in tests:
                print(f"+ Recording expected behaviour for '{test_name}'...")
                test = tests[test_name]

                prompt_msg = "Record current behaviour as the expected one? [y/N]"
                ans = input(prompt_msg)

                if ans.lower() == "y":
                    res = subprocess.run([f"./{test_name}"],
                                         capture_output = True,
                                         text = True)
                    tests[test_name].expected_stdout = res.stdout
                    tests[test_name].expected_stderr = res.stderr
                    tests[test_name].expected_returncode = res.returncode
                    tests[test_name].save_expected()
                    print('[SUCCESS] Recorded expected behaviour')
                else:
                    print('[SKIP]')

        else:
            print(f"[ERROR] Invalid subcommand '{subcmd}'", file=sys.stderr)
            exit(1)

if __name__ == "__main__":
    main()
