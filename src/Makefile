all: cellgen

clean:
	rm -rf cellgen *.o

cellgen:	cellgen.o parse_tree.o
		$(CXX) -Wall -ggdb cellgen.o parse_tree.o -o cellgen -lstdc++ -lboost_program_options -lboost_regex

cellgen.o:	cellgen.cpp c_grammar.h streamops.h variable.h ids.h parse_tree.h
		$(CXX) -Wall -ggdb -c cellgen.cpp 

parse_tree.o:	parse_tree.cpp parse_tree.h ids.h variable.h
		$(CXX) -Wall -ggdb -c parse_tree.cpp
