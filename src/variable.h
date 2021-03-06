#ifndef VARIABLE_H
#define VARIABLE_H

#include <list>
#include <map>
#include <set>
#include <string>
#include <sstream>
#include <algorithm>
#include <cmath>
using namespace std;

#include "utility.h"
#include "conditions.h"
#include "math_exprs.h"

const string pass_var = "pass";

class shared_variable_double_orientation {};

class variable {
	string _type;
	string _name;
	string _definition;

public:
	variable() {}
	variable(const variable& c): 
		_type(c._type), _name(c._name), _definition(c._definition) 
		{}
	variable(const string& t, const string& l, const string& a = ""):
		_type(t), _name(l), _definition(a) 
		{}
	virtual ~variable() {}

	virtual string name() const { return _name; }
	virtual string type() const { return _type; }
	string definition() const { return _definition; }

	void definition(string s) { _definition = s; }

	virtual string declare() const { return type() + " " + name(); }
	virtual string define() const { return type() + " " + name() + "=" + definition(); }

	virtual string actual() const { return pass_var + "." + name(); }
	virtual string formal() const { return declare(); }

	bool is_non_scalar() const { return _type.find("*") != string::npos || _type.find("[") != string::npos; }

	c_type scalar_type()
	{
		if (_type.find("int") != string::npos) {
			return INT;
		}
		else if (_type.find("long") != string::npos) {
			return LONG;
		}
		else if (_type.find("float") != string::npos) {
			return FLOAT;
		}
		else if (_type.find("double") != string::npos) {
			return DOUBLE;
		}
		else if (_type.find("char") != string::npos) {
			return CHAR;
		}

		return UNKNOWN_VAR;
	}
};

const variable prev("int", "prev", "0");
const variable clipped_range("int", "__N__");
const variable spe_start("int", "spe_start");
const variable spe_stop("int", "spe_stop");

class const_variable: public variable {
public:
	const_variable(const string& t, const string&l):
		variable("const " + t, l)
		{}
	const_variable(const string& t, const string& l, const string& a):
		variable("const " + t, l, a)
		{}
};
/* The Cell SPU C compiler doesn't allow const variables in static expressions. Oh well.
const const_variable buff_size("int", "buff_size", "16");
*/

const const_variable leftover("int", "leftover");

class pound_define: public variable {
public:
	pound_define(const string& l, const string& a):
		variable("", l, a)
		{}
	virtual string define() const { return "#define " + name() + " " + definition() + "\n"; }
};
const string default_buff_size("64");

class region_variable: public variable {
	int region_num;

public:
	region_variable(const string& t, const string& l, const string& a):
		variable(t, l, a), region_num(0)
		{}
	region_variable(const string& t, const string& l, const string& a, int r):
		variable(t, l, a), region_num(r)
		{}

	string unique_name() const { return name() + to_string(region_num); }
	string unique_declare() const { return type() + " " + unique_name(); }

	virtual string actual() const { return pass_var + "." + unique_name(); }
	virtual int stencil_spread(const string& ivar) const { return 0; }
};

class private_variable: public region_variable {
public:
	private_variable(const string& t, const string& l, const string& a):
		region_variable(t, l, a) {}
	private_variable(const string& t, const string& l, const string& a, int r):
		region_variable(t, l, a, r) {}

	virtual string formal() const { return type() + " " + variable::name(); }
};

enum orientation_t { UNINITIALIZED, ROW, COLUMN };

class unitialized_access_orientation: public exception {};

class shared_variable: public region_variable {
	add_expr _math;
	list<string> _dimensions; // Dimensions for multidimensional array.
	orientation_t _orientation;
	bool _in_generated;
	bool _out_generated;
	conditions _conds; // Closest conditions to the shared varaible.
	conditions _off; // Conditions for the off induction stencil, if any
	map<string, int> _lowest;
	map<string, int> _highest;

public:
	shared_variable(const string& t, const string& l, const string& a, int r):
		region_variable(t, l, a, r), _orientation(UNINITIALIZED), 
		_in_generated(false), _out_generated(false) {}
	shared_variable(const string& t, const string& l, const string& a, const list<string>& d, int r):
		region_variable(t, l, a, r), _dimensions(d), _orientation(UNINITIALIZED), 
		_in_generated(false), _out_generated(false) {}

	add_expr math() const { return _math; }

	bool is_row() const { return _orientation == ROW; }
	void row() { _orientation = ROW; }

	bool is_column() const { return _orientation == COLUMN; }
	void column() { _orientation = COLUMN; }

	bool is_off_induction_stencil() const { return _off != conditions(); }

	bool has_orientation() const { return _orientation != UNINITIALIZED; }
	virtual string name() const { return region_variable::name() + "_adr"; }

	const list<string>& dimensions() const { return _dimensions; }
	bool is_flat() const { return _dimensions.size() == 0; }
	bool seen() const { return _orientation != UNINITIALIZED; }

	bool in_not_generated() const { return !_in_generated; }
	bool out_not_generated() const { return !_out_generated; }
	void in_generated() { _in_generated = true; }
	void out_generated() { _out_generated = true; }

	conditions conds() const { return _conds; }
	conditions off() const { return _off; }

	int stencil_low(const string& i) const { return _lowest.find(i)->second; }
	int stencil_high(const string& i) const { return _highest.find(i)->second; }
	int stencil_spread(const string& i) const { return abs(stencil_high(i) - stencil_low(i)); }
	int stencil_row_spread() const { return stencil_spread(_conds.induction); }

	add_expr analyze_access(const list<string>& dimensions, const list<add_expr>& accesses, const condslist& above)
	{
		_math = construct_access_formula(dimensions, accesses);
		_conds = above.back();

		// Access type, columun or row.
		if (accesses.back().str().find(_conds.induction) != string::npos) {
			if (is_column()) {
				throw shared_variable_double_orientation();
			}

			row();
		}
		else {
			if (is_row()) {
				throw shared_variable_double_orientation();
			}

			column();
		}

		// Access stencil attributes, if any.
		for (condslist::const_reverse_iterator i = above.rbegin(); i != above.rend(); ++i) {
			const string& ivar = i->induction;
			if (_lowest.find(ivar) == _lowest.end()) {
				_lowest[ivar] = 0;
			}
			if (_highest.find(ivar) == _highest.end()) {
				_highest[ivar] = 0;
			}

			try {
				int offset = from_string<int>(_math.stencil_offset(ivar));
				if (offset < _lowest[ivar]) {
					_lowest[ivar] = offset;
				}
				else if (offset > _highest[ivar]) {
					_highest[ivar] = offset;
				}

				if (i != above.rbegin() && offset) {
					_off = *i;
				}
			}
			catch (ivar_not_found) {
				// Yes, do nothing.
			}
		}

		return _math;
	}
};

class reduction_variable: public region_variable {
public:
	reduction_variable(const string& t, const string& l, const string& a, int r):
		region_variable(t, l, a, r)
		{}

	virtual string name() const { return region_variable::name() + "_red"; }
	virtual string actual() const { return "&" + pass_var + "." + unique_name(); }

	virtual string formal() const {  return type() + "* " + name(); }
};

class buffer_adaptor {
	const region_variable* v;

public:
	buffer_adaptor(const region_variable* _v): v(_v) { assert(v); }
	string name() const { return v->region_variable::name() + "_buf"; }
	string declare() const { return type() + "* " + name(); }
	string size() const { return name() + "_sz"; }
	string abs() const { return name() + "_abs"; }

	string type() const
	{
		size_t star = v->type().find('*');
		size_t bracket = v->type().find('['); 

		if (bracket == string::npos && star == string::npos) {
			throw user_error(string("variable ") + v->region_variable::name() + 
					" can't be made into a buffer because it is a scalar." );
		}

		size_t pos = (star == string::npos) ? bracket : star;
		return v->type().substr(0, pos);
	}
};

class dma_list_adaptor {
	const shared_variable* v;

public:
	dma_list_adaptor(const shared_variable* _v): v(_v) { assert(v); }
	string name() const { return v->region_variable::name() + "_lst"; }
	string name(const int i) const { return v->region_variable::name() + "_lst[" + to_string(i) + "]"; }
	string name(const string s) const { return v->region_variable::name() + "_lst[" + s + "]"; }
	string type() const { return "spe_dma_list_t"; }
	string declare(const int depth) const { return type() +  " " + name() + "[" + to_string(depth) + "]"; }
};

class next_adaptor {
	const region_variable* v;
public:
	next_adaptor(const region_variable* _v): v(_v) { assert(v); }
	string type() const { return "int"; }
	string name() const { return v->region_variable::name() + "_nxt"; }
	string declare() const { return type() + " " + name(); }
};

class orig_adaptor {
	const variable* v;
public:
	orig_adaptor(const region_variable* _v): v(_v) { assert(v); }
	string type() const { return v->variable::type(); }
	string name() const { return v->variable::name(); }
	string declare() const { return type() + " " + name(); }

	string off_stencil_name(const int i)
	{
		if (i < 0) {
			return name() + "_m" + to_string(abs(i));
		}
		else if (i > 0) {
			return name() + "_p" + to_string(i);
		}
		else {
			return name();
		}
	}
};

class rem_adaptor {
	const shared_variable* v;
public:
	rem_adaptor(const shared_variable* _v): v(_v) { assert(v); }
	string name() const { return v->region_variable::name() + "_rem"; }
	string declare() const
	{
		return "unsigned int " + name();
	}

	string reset_value(const conditions& conds) const
	{
		string factor;
		if (v->is_flat()) {
			factor = v->math().ihs(conds.induction).non_ihs(conds.induction).str();
			if (factor != "") {
				factor = "/" + factor;
			}
		}
		
		return hug(conds.stop + "-" + conds.start) + "%" + hug(buffer_adaptor(v).size() + factor);
	}

	string reset(const conditions& conds) const
	{
		return name() + "=" + reset_value(conds);
	}
};

class full_adaptor {
	const shared_variable* v;
public:
	full_adaptor(const shared_variable* _v): v(_v) { assert(v); }
	string name() const { return v->region_variable::name() + "_ful"; }
	string declare() const
	{
		return "unsigned int " + name(); 
	}

	string reset(const conditions& conds) const
	{
		return name() + "= (" + conds.stop + ") -" + rem_adaptor(v).reset_value(conds);
	}
};

struct index_adapt: unary_function<const conditions&, variable> {
	variable operator()(const conditions& cond) const
	{
		return variable("unsigned int", "__" + cond.induction + "__", "0");
	}
	variable operator()(const string& i) const
	{
		return variable("unsigned int", "__" + i + "__", "0");
	}

};

typedef set<variable*> varset;
typedef set<private_variable*> privset;
typedef set<shared_variable*> sharedset;
typedef set<reduction_variable*> reduceset;
typedef map<string, variable*> var_symtbl;
typedef map<string, shared_variable*> shared_symtbl;
typedef map<string, private_variable*> priv_symtbl;
typedef map<string, reduction_variable*> reduc_symtbl;
typedef set<string> symset;
typedef list<string> symlist;
typedef map<const region_variable*, int> depths;

#endif // VARIABLE_H

