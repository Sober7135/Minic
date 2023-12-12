CXX = clang++
CXXFLAGS = -std=c++2b -Wall -Wextra
# LDFLAGS = -L /usr/lib

SOURCE = src/main.cc src/flex.cc
CLEAN	= src/token2string.hh src/flex.cc main

gen: scripts/generate.py src/lexer.l src/lexer.hh
	python scripts/generate.py
	cd src && flex lexer.l

build: $(SOURCE) gen
	$(CXX) $(SOURCE) $(CXXFLAGS) $(LDFLAGS) -o main
	
clean:  
	rm -f $(CLEAN)