#ifndef VARIABLE_H
#define VARIABLE_H

#include <list>
#include <map>
#include <set>
#include <string>
#include <sstream>
using namespace std;

#include "mult_expr.h"

const string pass_var = "pass";

class variable {
	string _type;
	string _name;
	string _alias;
	mult_expr _math;

public:
	variable() {}
	variable(const variable& c): 
		_type(c._type), _name(c._name), _alias(c._alias) 
		{}
	variable(const string& t, const string& l = "", const string& a = ""):
		_type(t), _name(l), _alias(a) 
		{}
	virtual ~variable() {}

	virtual string name() const { return _name; }
	virtual string type() const { return _type; }
	string alias() const { return _alias; }
	mult_expr math() const { return _math; }

	void alias(string s) { _alias = s; }
	void math(mult_expr m) { _math = m; }

	virtual string declare() const
	{
		return type() + " " + name();
	}

	virtual string actual() const { return pass_var + "." + name(); }
	virtual string formal() const { return declare(); }

	bool is_pointer() const
	{
		return _type.find("*") != string::npos;
	}
};

class const_variable: public variable {
public:
	const_variable(const string& t, const string& l, const string& a):
		variable("const " + t, l, a)
		{}
	virtual string declare() const
	{
		return type() + " " + name() + "=" + alias() + ";";
	}
};
/* The Cell SPU C compiler doesn't allow const variables in static expressions. Oh well.
const const_variable buff_size("int", "buff_size", "16");
*/

class pound_define: public variable {
public:
	pound_define(const string& l, const string& a):
		variable("", l, a)
		{}
	virtual string declare() const
	{
		return "#define " + name() + " " + alias() + "\n";
	}
};
const pound_define buff_size("buff_size", "80");

class shared_variable: public variable {
public:
	shared_variable() {}
	shared_variable(const string& t, const string& l, const string& a):
		variable(t, l, a) 
		{}

	virtual string name() const { return variable::name() + "_addr"; }
	virtual string actual() const { return pass_var + "." + name(); }
};

class buffer_variable: public variable {
private:
	size_t _depth; // What type of buffering? Currently we only go up to triple.

public:
	buffer_variable(const variable* cv, size_t d): variable(*cv), _depth(d)
	{
		assert(_depth > 0);	
	}

	virtual string name() const { return variable::name() + "_buff"; }

	virtual string type() const
	{
		string noptr = variable::type();
		size_t pos = noptr.find('*');
		if (pos == string::npos) {
			cerr	<< "error: variable " << variable::name() 
				<< " can't be made into a buffer because it is not a pointer." 
				<< endl;
			exit(1);
		}
		noptr.replace(pos, strlen("*"), "");
		return noptr;
	}

	virtual string declare() const
	{
		stringstream ss;
		ss << type() << " " << name();
		if (_depth > 1) {
			ss << "[" << _depth << "]";
		}
		ss << "[" << buff_size.name() << "] __attribute__((aligned(128)))"; 
		return ss.str();
	}

	string depth() const
	{
		stringstream ss;
		ss << _depth;
		return ss.str();
	}
};

class next_variable: public variable {
public:
	next_variable(const variable* cv): variable(*cv) {}
	virtual string type() const { return "int"; }
	virtual string name() const { return variable::name() + "_next"; }
};

class orig_variable: public variable {
public:
	orig_variable(const variable* cv): variable(*cv) {}
	virtual string type() const { return variable::type(); }
	virtual string name() const { return variable::name(); }
	virtual string declare() const { return variable::declare(); }
	virtual string actual() const { return variable::actual(); }
	virtual string formal() const { return variable::formal(); }
};

class reduction_variable: public variable {
public:
	reduction_variable(const string& t, const string& l, const string& a):
		variable(t, l, a)
		{}

	virtual string name() const { return variable::name() + "_reduc"; }
	virtual string actual() const { return "&" + pass_var + "." + name(); }

	virtual string formal() const
	{  
		return type() + "* " + name();
	}
};

typedef list<variable*>		varlist;
typedef map<string, variable*>	symtbl;
typedef set<string>		symset;

#endif // VARIABLE_H

