#ifndef VARIABLE_H
#define VARIABLE_H

#include <list>
#include <string>
#include <sstream>
using namespace std;

#include <boost/spirit/symbols.hpp>
using namespace boost::spirit;

const string buff_size = "16";

class c_variable {
	string _type;
	string _name;
	string _actual;

public:
	c_variable() {}
	c_variable(const string& t, const string& l = "", const string& a = ""):
		_type(t), _name(l), _actual(a) {}
	virtual ~c_variable() {}

	string type() const { return _type; }
	virtual string name() const { return _name; }
	string actual() const { return _actual; }
	void set_actual(string s) { _actual = s; }

	virtual string declare() const
	{
		stringstream ss;
		ss << _type << " " << _name;
		return ss.str();
	}
};

class shared_variable: public c_variable {
	string _addr;
	string _buff;
	string _index;

public:
	shared_variable(): c_variable(), _addr("_addr"), _buff("_buff"), _index("_index") {}
	shared_variable(const string& t, const string& l = "", const string& a = ""):
		c_variable(t, l, a), _addr("_addr"), _buff("_buff"), _index("_index") {}
	virtual ~shared_variable() {}

	string orig_name() const { return c_variable::name(); }
	string buff_name() const { return orig_name() + _buff; }
	string index_name() const { return orig_name() + _index; }
	virtual string name() const { return orig_name() + _addr; }

	string buff_type() const
	{
		string noptr = type();
		size_t pos = noptr.find('*');
		if (pos == string::npos) {
			cerr	<< "error: variable " << declare() 
				<< " is declared shared, but is not a pointer." << endl;
			exit(1);
		}
		noptr.replace(pos, strlen("*"), "");
		return noptr;
	}

	virtual string declare() const
	{
		return type() + " " + orig_name() + _addr;
	}

	string orig_declare() const
	{
		return type() + " " + orig_name();
	}

	string buff_declare() const
	{
		stringstream ss;
		ss	<< buff_type() << " " << orig_name() << _buff 
			<< "[2][" + buff_size + "] __attribute__((aligned(128)))";
		return ss.str();
	}

	string index_declare() const
	{
		return "int " + index_name();
	}
};

typedef list<const c_variable*>		cvarlist_t;
typedef list<const shared_variable*>	sharedlist_t;
typedef symbols<const c_variable*>	symtbl_t;
typedef symbols<const shared_variable*> sharedtbl_t;

#endif // VARIABLE_H

