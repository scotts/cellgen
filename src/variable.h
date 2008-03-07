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

	virtual string declare() const { return type() + " " + name(); }

	virtual string actual() const { return pass_var + "." + name(); }
	virtual string formal() const { return declare(); }

	bool is_pointer() const { return _type.find("*") != string::npos; }
};

class const_variable: public variable {
public:
	const_variable(const string& t, const string& l, const string& a):
		variable("const " + t, l, a)
		{}

	virtual string declare() const { return type() + " " + name() + "=" + alias() + ";"; }
};
/* The Cell SPU C compiler doesn't allow const variables in static expressions. Oh well.
const const_variable buff_size("int", "buff_size", "16");
*/

class pound_define: public variable {
public:
	pound_define(const string& l, const string& a):
		variable("", l, a)
		{}
	virtual string declare() const { return "#define " + name() + " " + alias() + "\n"; }
};
const pound_define buff_size("buff_size", "80");

class region_variable: public variable {
	int region_num;
public:
	region_variable(const string& t, const string& l, const string& a):
		variable(t, l, a), region_num(0) {}
	region_variable(const string& t, const string& l, const string& a, int r):
		variable(t, l, a), region_num(r) {}


	virtual string name() const
	{
		stringstream ss;
		ss << variable::name();
		if (region_num > 0) {
			ss << region_num;
		}
		return ss.str();
	}
};

class private_variable: public region_variable {
public:
	private_variable(const string& t, const string& l, const string& a, int r):
		region_variable(t, l, a, r) {}

	virtual string formal() const { return type() + " " + variable::name(); }
};

class shared_variable: public region_variable {
public:
	shared_variable(const string& t, const string& l, const string& a, int r):
		region_variable(t, l, a, r)
		{}

	virtual string name() const
	{
		stringstream ss;
		ss << region_variable::name() << "_adr";
		return ss.str(); 
	}
};

class reduction_variable: public region_variable {
public:
	reduction_variable(const string& t, const string& l, const string& a, int r):
		region_variable(t, l, a, r)
		{}

	virtual string name() const { return region_variable::name() + "_reduc"; }
	virtual string actual() const { return "&" + pass_var + "." + name(); }

	virtual string formal() const {  return type() + "* " + name(); }
};

class buffer_adaptor {
private:
	const region_variable* v;
	size_t _depth; // What type of buffering? Currently we only go up to triple.

public:
	buffer_adaptor(const region_variable* _v, size_t d): v(_v), _depth(d)
	{
		assert(v);
		assert(_depth > 0);	
	}

	string name() const { return v->region_variable::name() + "_buff"; }

	string type() const
	{
		string noptr = v->type();
		size_t pos = noptr.find('*');
		if (pos == string::npos) {
			cerr	<< "error: variable " << v->name() 
				<< " can't be made into a buffer because it is not a pointer." 
				<< endl;
			exit(1);
		}
		noptr.replace(pos, strlen("*"), "");
		return noptr;
	}

	string declare() const
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

class next_adaptor {
	const region_variable* v;
public:
	next_adaptor(const region_variable* _v): v(_v) {}
	string type() const { return "int"; }
	string name() const { return v->region_variable::name() + "_nxt"; }
	string declare() const { return type() + " " + name(); }
};

class orig_adaptor {
	const variable* v;
public:
	orig_adaptor(const region_variable* _v): v(_v) {}
	string type() const { return v->variable::type(); }
	string name() const { return v->variable::name(); }
	string declare() const { return type() + " " + name(); }
};

typedef set<region_variable*>		varset;
typedef map<string, region_variable*>	symtbl;
typedef set<string>			symset;

#endif // VARIABLE_H

