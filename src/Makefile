# Normally I'm all for turning on debug information always, but it makes 
# about a 30s difference in compilation times, and an order of magnitude
# difference in executable size. Likely caused by debug information for 
# the nested templates used by Spirit.
#DEBUG = -ggdb

all: cellgen

clean:
			rm -rf cellgen *.o

cellgen:		main.o cellgen_grammar.o parse_tree.o
			$(CXX) $(DEBUG) -Wall main.o cellgen_grammar.o parse_tree.o -o cellgen -lstdc++ -lboost_program_options -lboost_regex

main.o:			main.cpp streamops.h spe_region.h cellgen_grammar.o parse_tree.o
			$(CXX) $(DEBUG) -Wall -c main.cpp 

cellgen_grammar.o:	cellgen_grammar.cpp cellgen_grammar.h c_grammar.h parse_tree.h variable.h mult_expr.h spe_region.h skip.h
			$(CXX) $(DEBUG) -Wall -c cellgen_grammar.cpp

parse_tree.o:		parse_tree.cpp parse_tree.h ids.h variable.h mult_expr.h spe_region.h
			$(CXX) $(DEBUG) -Wall -c parse_tree.cpp
