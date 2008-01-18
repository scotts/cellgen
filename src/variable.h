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
		return type() + " " + name();
	}

	virtual string actual() const { return pass_var + "." + name(); }
	virtual string formal() const { return declare(); }

	bool is_pointer() const
	{
		return _type.find("*") != string::npos;
	}
};

class const_c_variable: public c_variable {
public:
	const_c_variable(const string& t, const string& l, const string& a):
		c_variable("const " + t, l, a)
		{}
	virtual string declare() const
	{
		return type() + " " + name() + "=" + alias() + ";";
	}
};
/* The Cell SPU C compiler doesn't allow const variables in static expressions. Oh well.
const const_c_variable buff_size("int", "buff_size", "16");
*/

class pound_define: public c_variable {
public:
	pound_define(const string& l, const string& a):
		c_variable("", l, a)
		{}
	virtual string declare() const
	{
		return "#define " + name() + " " + alias() + "\n";
	}
};
const pound_define buff_size("buff_size", "80");

class shared_variable: public c_variable {
public:
	shared_variable() {}
	shared_variable(const string& t, const string& l, const string& a):
		c_variable(t, l, a) 
		{}

	virtual string name() const { return c_variable::name() + "_addr"; }
	virtual string actual() const { return pass_var + "." + name(); }
};

class buffer_variable: public c_variable {
public:
	buffer_variable(const c_variable* cv): c_variable(*cv) {}

	virtual string name() const { return c_variable::name() + "_buff"; }

	virtual string type() const
	{
		string noptr = c_variable::type();
		size_t pos = noptr.find('*');
		if (pos == string::npos) {
			cerr	<< "error: variable " << c_variable::name() 
				<< " can't be made into a buffer because it is not a pointer." 
				<< endl;
			exit(1);
		}
		noptr.replace(pos, strlen("*"), "");
		return noptr;
	}

};

class double_buffer: public buffer_variable {
public:
	double_buffer(const c_variable* cv): buffer_variable(cv) {}

	virtual string declare() const
	{
		return type() + " " + name() + "[2][" + buff_size.name() + "] __attribute__((aligned(128)))"; 
	}
};

class private_buffer: public buffer_variable {
public:
	private_buffer(const c_variable* cv): buffer_variable(cv) {}

	virtual string declare() const
	{
		return type() + " " + name() + "[" + buff_size.name() + "] __attribute__((aligned(128)))"; 
	}
};

class next_variable: public c_variable {
public:
	next_variable(const c_variable* cv): c_variable(*cv) {}
	virtual string type() const { return "int"; }
	virtual string name() const { return c_variable::name() + "_next"; }
};

class orig_variable: public c_variable {
public:
	orig_variable(const c_variable* cv): c_variable(*cv) {}
	virtual string type() const { return c_variable::type(); }
	virtual string name() const { return c_variable::name(); }
	virtual string declare() const { return c_variable::declare(); }
	virtual string actual() const { return c_variable::actual(); }
	virtual string formal() const { return c_variable::formal(); }
};

class reduction_variable: public c_variable {
public:
	reduction_variable(const string& t, const string& l, const string& a):
		c_variable(t, l, a)
		{}

	virtual string name() const { return c_variable::name() + "_reduc"; }
	virtual string actual() const { return "&" + pass_var + "." + name(); }

	virtual string formal() const
	{  
		return type() + "* " + name();
	}
};

typedef list<const c_variable*>		cvarlist_t;
typedef map<string, const c_variable*>	symtbl_t;

#endif // VARIABLE_H

