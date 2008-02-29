#ifndef STREAMOPS_H
#define STREAMOPS_H

#include <iostream>
#include <sstream>
using namespace std;

#include <boost/spirit/iterator/file_iterator.hpp>
using namespace boost::spirit;

class sstream_out {
	ostream& out;
public:
	sstream_out(ostream& o): out(o) {}
	void operator()(const stringstream* ss) { out << ss->str(); }
};

class stream_writer {
	ostream& out;

public:
	stream_writer(ostream& o): out(o) {}
	void operator()(file_iterator<char> first, const file_iterator<char>& last) const
	{
		while (first != last) {
			out << *first++;
		}
	}
};

#endif // STREAMOPS_H

