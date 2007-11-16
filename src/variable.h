#ifndef VARIABLE_H
#define VARIABLE_H

#include <list>
#include <string>
#include <sstream>
using namespace std;

const string pass_var = "pass";

class c_variable {
	string _type;
	string _name;
	string _alias;

public:
	c_variable() {}
	c_variable(const c_variable& c): 
		_type(c._type), _name(c._name), _alias(c._alias) 
		{}
	c_variable(const string& t, const string& l = "", const string& a = ""):
		_type(t), _name(l), _alias(a) 
		{}
	virtual ~c_variable() {}

	virtual string name() const { return _name; }
	virtual string type() const { return _type; }
	string alias() const { return _alias; }

	void set_alias(string s) { _alias = s; }

	virtual string declare() const
	{
		stringstream ss;
		ss << type() << " " << name();
		return ss.str();
	}

	virtual string actual() const { return pass_var + "." + name(); }
	virtual string formal() const { return declare(); }
};

class const_c_variable: public c_variable {
public:
	const_c_variable(const string& t, const string& l, const string& a):
		c_variable("const " + t, l, a)
		{}
	virtual string declare() const
	{
		stringstream ss;
		ss << type() << " " << name() << " = " << alias() << ";";
		return ss.str();
	}
};

const const_c_variable buff_size("int", "buff_size", "16");

class shared_variable: public c_variable {
public:
	shared_variable() {}
	shared_variable(const string& t, const string& l, const string& a):
		c_variable(t, l + "_addr", a) 
		{}
};

class buff_variable: public c_variable {
public:
	buff_variable(const c_variable* cv):
		c_variable(cv->type(), cv->name() + "_buff", cv->alias()) {}

	virtual string type() const
	{
		string noptr = c_variable::type();
		size_t pos = noptr.find('*');
		if (pos == string::npos) {
			cerr	<< "error: variable " << c_variable::declare() 
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
		ss	<< type() << " " << name() 
			<< "[2][" + buff_size.alias() + "] __attribute__((aligned(128)))";
		return ss.str();
	}

};

class index_variable: public c_variable {
public:
	index_variable(const c_variable* cv): 
		c_variable("int", cv->name() + "_index", cv->alias()) 
		{}
};

class orig_variable: public c_variable {
public:
	orig_variable(const c_variable* cv):
		c_variable(cv->type(), cv->name(), cv->alias())
		{}

	virtual string name() const
	{
		string noaddr = c_variable::name();
		size_t pos = noaddr.find("_addr");
		if (pos == string::npos) {
			cerr	<< "error: variable " << declare() 
				<< " is not an shared variable." 
				<< endl;
			exit(1);
		}
		noaddr.replace(pos, strlen("_addr"), "");
		return noaddr;
	}
};


class reduction_variable: public c_variable {
public:
	reduction_variable(const string& t, const string& l, const string& a):
		c_variable(t, l, a)
		{}

	virtual string actual() const { return "&" + pass_var + "." + name(); }

	virtual string formal() const
	{  
		stringstream ss;
		ss << type() << "* " << name();
		return ss.str();
	}
};

typedef list<const c_variable*>			cvarlist_t;
typedef map<string, const c_variable*>		symtbl_t;

#endif // VARIABLE_H

