CXX = clang++
CXXFLAGS = -std=c++2b -Wall -Werror -Wextra
LDFLAGS = -L /usr/lib

SOURCE = $(wildcard src/*.cc)
CLEAN	= src/token2string.hh src/lexer.cc main

gen: scripts/generate.py src/lexer.l src/lexer.hh
	python scripts/generate.py
	cd src && flex lexer.l

build: $(SOURCE) gen
	which $(CXX)
	$(CXX) $(SOURCE) $(CXXFLAGS) $(LDFLAGS) -o main
	
clean:  
	rm -f $(CLEAN)