# Normally I'm all for turning on debug information always, but it makes 
# about a 30s difference in compilation times, and an order of magnitude
# difference in executable size. Likely caused by debug information for 
# the nested templates used by Spirit.
#DEBUG = -ggdb
OPT = -O3
CXXFLAGS = -Wall -DUNIX $(DEBUG) $(OPT) 

BOOST_LIBS = -lboost_program_options -lboost_regex
LDFLAGS = -lstdc++ $(BOOST_LIBS) -DUNIX

OBJS = main.o cellgen_grammar.o parse_tree.o math_exprs.o operations.o

# All implementations need access to these declarations/defintions.
COMMON_INCLUDES = variable.h math_exprs.h spe_region.h utility.h operations.h

all: cellgen
clean:
			rm -rf cellgen *.o

cellgen:		$(OBJS)
			$(CXX) $(CXXFLAGS) $(OBJS) -o cellgen $(LDFLAGS)

main.o:			main.cpp cellgen_grammar.h streamops.h $(COMMON_INCLUDES)
			$(CXX) $(CXXFLAGS) -DTEMPLATE_DIR=\"$(PWD)/../template_code/\" -c main.cpp 

cellgen_grammar.o:	cellgen_grammar.cpp cellgen_grammar.h parse_tree.h c_grammar.h skip.h $(COMMON_INCLUDES)
parse_tree.o:		parse_tree.cpp parse_tree.h ids.h xformers.h c_grammar.h skip.h $(COMMON_INCLUDES)
math_exprs.o:		math_exprs.cpp math_exprs.h 
operations.o:		operations.cpp operations.h
