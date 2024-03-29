#!/usr/bin/env python
from ast import If
import os
import subprocess

TEST_DIR = "./test/"
TEST_ERR_DIR = "./test/err/"
EXE = "./build/bin/minic "

RED = "\033[91m"
GREEN = "\033[92m"
RESET = "\033[0m"


def get_test_file(dir):
    file_list = []
    for item in os.listdir(dir):
        if item.endswith(".c"):
            file_list.append(dir + item)
    return file_list


def get_output(cmd):
    return subprocess.getoutput(cmd)


def removeExt(name: str):
    return os.path.splitext(name)[0]


def test(list):
    s = 0
    f = 0
    for item in list:
        command = EXE + item
        print(f"=============================={item}==============================")
        exit_code = os.system(command)
        if exit_code == 0:
            s += 1
            print(f"{GREEN}TEST {item} COMPILE SUCCESS EXIT {exit_code}{RESET}")
            x = get_output(removeExt(item))
            clang_cmd = "clang {} -o {} &> /dev/null && ./{}".format(
                item, removeExt(item) + "_clang", removeExt(item) + "_clang"
            )
            y = get_output(clang_cmd)
            if x == y:
                print(f"{GREEN}TEST {item} EXECUTE SUCCESS EXIT {exit_code}{RESET}")
            else:
                print(f"{RED}EXECUTION FAILURE {item} EXIT {exit_code}{RESET}")
        else:
            f += 1
            print(f"{RED} FAILURE {item} EXIT {exit_code}{RESET}")
    return s, f


test_list = get_test_file(TEST_DIR)
test_err_list = get_test_file(TEST_ERR_DIR)

_, f = test(test_list)
s, _ = test(test_err_list)

if f != 0:
    print(f"{RED}TEST: {f} WRONG{RESET}")
else:
    print(f"{GREEN}TEST: ALL PASSED{RESET}")

if s != 0:
    print(f"{RED}TEST ERR: {s} WRONG{RESET}")
else:
    print(f"{GREEN}TEST ERR: ALL PASSED{RESET}")
