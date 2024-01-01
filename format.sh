#!/bin/bash

find . -regex '.*\.\(cpp\|hh\|hpp\|cc\|cxx\)' -exec clang-format -style=file -i {} \;