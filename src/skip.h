#ifndef SKIP_H
#define SKIP_H

#undef BOOST_SPIRIT_THREADSAFE // performance optimization
#include <boost/spirit/core.hpp>
#include <boost/spirit/utility.hpp>
using namespace boost::spirit;

struct skip_grammar: public grammar<skip_grammar>
{
	template <typename ScannerT>
	struct definition {
		definition(skip_grammar const& /*self*/)
		{
			skip =   
				space_p
			|	chlit<>('\\') >> chlit<>('\n')
			|	comment_p("//")                 // C++ comment
			|	comment_p("/*", "*/")           // C comment
			|	comment_p("#line")
			;
		}

		rule<ScannerT> skip;

		rule<ScannerT> const& start() const { return skip; }
	};
};

const skip_grammar skip;

#endif	// SKIP_H

