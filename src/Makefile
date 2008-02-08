# Normally I'm all for turning on debug information always, but it makes 
# about a 30s difference in compilation times, and an order of magnitude
# difference in executable size. Likely caused by debug information for 
# the nested templates used by Spirit.
#DEBUG = -ggdb

all: cellgen

clean:
	rm -rf cellgen *.o

cellgen:	cellgen.o parse_tree.o
		$(CXX) $(DEBUG) -Wall cellgen.o parse_tree.o -o cellgen -lstdc++ -lboost_program_options -lboost_regex

cellgen.o:	cellgen.cpp c_grammar.h parse_tree.h variable.h spe_region.h streamops.h skip.h
		$(CXX) $(DEBUG) -Wall -c cellgen.cpp 

parse_tree.o:	parse_tree.cpp parse_tree.h ids.h variable.h spe_region.h
		$(CXX) $(DEBUG) -Wall -c parse_tree.cpp
