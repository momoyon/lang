import os
import sys
import subprocess

TEST_DIR = "./tests/"
TEST_FILE_SUFFIX = ".test"

def error(msg: str):
    print(f"ERROR: {msg}", file=sys.stderr)


def cmd(args: [str], echo=False) -> subprocess.CompletedProcess:
    if echo:
        print(f"CMD: \"", end='')
        for i in range(len(args)):
            print(f"{args[i]}", end='')
            if i < len(args)-1:
                print(" ", end='')

        print("\"")
    return subprocess.run(args, capture_output=True, text=True)


def test_source_file(filename: str):
    if not os.path.exists(filename):
        error(f"File {filename} doesn't exist!")
        exit(1)

    # TODO: option to record/update test file
    testfilename: str = filename + TEST_FILE_SUFFIX
    if not os.path.exists(testfilename):
        error("Test file {testfilename} doesn't exist!")
        exit(1)

    p = cmd(["python", "./main.py", filename])

    output = p.stdout

    f = open(testfilename, 'r')
    expected_output = f.read()
    f.close()

    if output != expected_output:
        print("Failed!")
        print(f"Expected: `{expected_output}`")
        print(f"Got:      `{output}`")
    else:
        print("Success!")

def main():
    test_source_file("./tests/01-unterminated-string.momo")


if __name__ == '__main__':
    main()
