#ifndef XFORMERS_H
#define XFORMERS_H

#include <string>
#include <list>
using namespace std;

#include <boost/regex.hpp>

#include "variable.h"
#include "operations.h"

struct xformer: public unary_function<const string&, string> {
	virtual ~xformer() {}
	virtual void remainder_me() {} // Sigh. It's a hack to put this here, but it makes life so much easier.
	virtual void nest_me() {} // Double sigh, double hack.
	virtual string operator()(const string& old) = 0;
	virtual xformer* clone() const = 0;
	virtual string class_name() const = 0; // For debugging purposes only.
};

typedef list<xformer*> xformerlist;

template <class X, class V>
struct make_xformer: public unary_function<const V*, xformer*> {
	xformer* operator()(const V* v)
	{
		return new X(v);
	}
};

class depth_xformer: virtual public xformer {
protected:
	const int depth;
public:
	depth_xformer(int d): depth(d)
	{
		assert(depth > 0);	
	}
};

template <class X, class V>
struct make_depth_xformer: public unary_function<const V*, xformer*> {
	const depths& scope_depths;
	make_depth_xformer(const depths& d): scope_depths(d) {}

	xformer* operator()(const V* v)
	{
		return new X(v, scope_depths.find(v)->second);
	}
};

class conditions_xformer: virtual public xformer, public depth_xformer {
protected:
	shared_variable* v;
	condslist above;
public:
	conditions_xformer(shared_variable* _v, const condslist& a, const int d): depth_xformer(d), v(_v), above(a) {}
};

template <class X>
struct make_conditions: public unary_function<shared_variable*, xformer*> {
	const condslist& above;
	const depths& local_depths;
	make_conditions(const condslist& a, const depths& l): above(a), local_depths(l) {}
	xformer* operator()(shared_variable* v)
	{
		return new X(v, above, local_depths.find(v)->second);
	}
};

class remainder_xformer: virtual public xformer {
protected:
	bool is_remainder;
public:
	remainder_xformer(): is_remainder(false) {}
	virtual void remainder_me()
	{
		is_remainder = true;
	}
};

class nested_xformer: virtual public xformer {
protected:
	bool nested;
public:
	nested_xformer(): nested(false) {}
	virtual void nest_me()
	{
		nested = true;
	}
};

struct nop: public xformer {
	string operator()(const string&)
	{
		return "";
	}
	xformer* clone() const { return new nop; }
	string class_name() const { return "nop"; }
};

class to_buffer_space: virtual public xformer {
	const shared_variable* v;
	const add_expr math;
	const condslist above;
	const variable index;
public:
	to_buffer_space(const shared_variable* _v, const add_expr& m, const condslist& a, const variable& i): 
		v(_v), math(m), above(a), index(i) {}
	string operator()(const string& old)
	{
		string offset;
		string factor;
		if (v->is_flat() && v->math().factor(above.back().induction) != "") {
			offset = math.non_ihs(above.back().induction).str();
			if (offset != "") {
				offset = "+" + offset;
			}

			factor = math.factor(above.back().induction);
			if (factor != "") {
				factor = "*" + factor;
			}
		}
		else if (v->stencil_low(above.back().induction) != v->stencil_high(above.back().induction)) {
			offset = "+" + to_string(from_string<int>(math.stencil_offset(above.back().induction)) - v->stencil_low(above.back().induction));
		}

		return old + orig_adaptor(v).name() + "[" + index.name() + factor + offset + "]";
	}

	xformer* clone() const { return new to_buffer_space(*this); }
	string class_name() const { return "to_buffer_space"; }
};

class augment_induction: public remainder_xformer {
	const variable index;
	const shared_variable* cause;
public:
	augment_induction(const variable& x, const shared_variable* c): 
		index(x), cause(c) {}
	string operator()(const string& old)
	{
		string offset;
		if (is_remainder) {
			offset = full_adaptor(cause).name();
		}
		else {
			offset = old;
		}

		return offset + "+" + index.name();
	}
	xformer* clone() const { return new augment_induction(*this); }
	string class_name() const { return "augment_induction"; }
};

class total_timer_start: public xformer {
public:
	string operator()(const string& old)
	{
		return old + "cellgen_total_start(); \n";
	}

	xformer* clone() const { return new total_timer_start(*this); }
	string class_name() const { return "total_timer_start"; }
};

class buffer_loop_start: public remainder_xformer {
	const variable index;
	const string buffer_size;
	const string remainder_size;
	const string induction;
	const string step;
public:
	buffer_loop_start(const variable& i, const string& b, const string& l, const string& ind, const string& s): 
		index(i), buffer_size(b), remainder_size(l), induction(ind), step(s) {}
	string operator()(const string& old)
	{
		return old + "for (" + 
			index.name() + "= 0;" + 
			index.name() + "<" + (is_remainder ? remainder_size : buffer_size) + ";" + 
			regex_replace(step, regex(induction), index.name()) + ") {"; 
	}

	xformer* clone() const { return new buffer_loop_start(*this); }
	string class_name() const { return "buffer_loop_start"; }
};

class buffer_loop_stop: public xformer {
public:
	buffer_loop_stop() {}
	string operator()(const string& old)
	{
		return "}" + old;
	}

	xformer* clone() const { return new buffer_loop_stop(); }
	string class_name() const { return "buffer_loop_stop"; }
};

class loop_increment: public xformer {
	const string induction;
	const string size;
public:
	loop_increment(const string& i, const string& s): induction(i), size(s) {}
	string operator()(const string& old)
	{
		return induction + "+=" + size;
	}

	xformer* clone() const { return new loop_increment(*this); }
	string class_name() const { return "loop_increment"; }
};

class if_clause: public xformer {
	const string exp;
public:
	if_clause(const string& e): exp(e) {}
	string operator()(const string&)
	{
		return "if (" + exp + ")";
	}

	xformer* clone() const { return new if_clause(*this); }
	string class_name() const { return "if_clause"; }
};

class total_timer_stop: public xformer {
public:
	string operator()(const string& old)
	{
		return "cellgen_total_stop(fn_id);" + old;
	}

	xformer* clone() const { return new total_timer_stop(*this); }
	string class_name() const { return "total_timer_stop"; }
};

class variable_name: public xformer {
	const variable v;
public:
	variable_name(const variable _v): v(_v) {}
	string operator()(const string&)
	{
		return v.name();
	}

	xformer* clone() const { return new variable_name(*this); }
	string class_name() const { return "variable_name"; }
};

class naked_string: public xformer {
	const string str;
public:
	naked_string(const string& s): str(s) {}
	string operator()(const string&)
	{
		return str;
	}

	xformer* clone() const { return new naked_string(*this); }
	string class_name() const { return "naked_string"; }
};

struct variable_increment: public xformer {
	const string var;
	variable_increment(const string& v): var(v) {}
	string operator()(const string& old)
	{
		return old + "++" + var + ";";
	}

	xformer* clone() const { return new variable_increment(*this); }
	string class_name() const { return "variable_increment"; }
};

class compute_bounds: public xformer {
	const string least;
public:
	compute_bounds(const string& l): least(l) {}
	string operator()(const string& old)
	{
		return old + "compute_bounds(&spe_start, &spe_stop, sizeof(" + least + "));";
	}

	xformer* clone() const { return new compute_bounds(*this); }
	string class_name() const { return "compute_bounds"; }
};

struct reduction_assign: public xformer {
	const reduction_variable* v;
	reduction_assign(const reduction_variable* v): v(v) {}
	string operator()(const string& old)
	{
		return "*" + v->name() + "=" + orig_adaptor(v).name() + "; \n" + old;
	}

	xformer* clone() const { return new reduction_assign(*this); }
	string class_name() const { return "reduction_assign"; }
};

class define_clipped_range: public xformer {
	const string start;
	const string stop;
	const string max_type;
public:
	define_clipped_range(const string& b, const string& e, const string& m): start(b), stop(e), max_type(m) {}
	string operator()(const string& old)
	{
		return old + clipped_range.declare() + "=min((" + stop + "-" + start + "), 16384/sizeof(" + max_type + "));";
	}

	xformer* clone() const { return new define_clipped_range(*this); }
	string class_name() const { return "define_clipped_range"; }
};

class shared_buffer_size: public depth_xformer {
	const shared_variable* v;
	const shared_variable* max;
	const string buffer;
	const int num_shared;
	const string par_induction;
	const string max_type;

public:
	shared_buffer_size(const shared_variable* _v, const shared_variable* m, const string& b, const int n, const string& p, const int d, const string& mt): 
		depth_xformer(d), v(_v), max(m), buffer(b), num_shared(n), par_induction(p), max_type(mt) {}
	string operator()(const string& old)
	{
		string declaration;
		if (depth > 0) {
			string factor;
			try {
				const string max_factor = max->math().factor(par_induction);
				const string v_factor = v->math().factor(par_induction);
				if (from_string<int>(v_factor) < from_string<int>(max_factor)) {
					factor = "/ " + max_factor;	
				}
			} catch (ivar_not_found e) {
				// Yes, do nothing.
			}

			string start = v->conds().start;
			string stop = v->conds().stop;
			if (v->is_row() && v->is_flat()) {
				start = spe_start.name();
				stop = spe_stop.name();
			}

			string def;
			if (buffer == "") {
				const const_variable range("unsigned int", v->name() + "_rng", 
						"min(((" + stop + ")-(" + start + ")), 16384/sizeof(" + max_type + "))");
				const string depth1 = to_string(depth + 1);
				def = range.name() + "*" + depth1 + "< (" + stop + ") - (" + start + ") ?" + 
					range.name() + ":" +
					"prev16(" + range.name() + "/" + depth1 + ")";

				declaration = range.define() + ";";
			}
			else {
				def = buffer;
			}

			declaration += variable("unsigned int", buffer_adaptor(v).size(), def + factor).define() + ";" + 
				const_variable("unsigned int", buffer_adaptor(v).abs(), def + factor).define() + ";";
		}

		return old + declaration;
	}

	xformer* clone() const { return new shared_buffer_size(*this); }
	string class_name() const { return "shared_buffer_size"; }
};

struct make_shared_buffer_size: public unary_function<shared_variable*, xformer*> {
	const shared_variable* max;
	const string& buffer;
	const int num_shared;
	const string& par_induction;
	const depths& local_depths;
	const string& max_type;
	make_shared_buffer_size(const shared_variable* m, const string& b, const int n, const string& p, const depths& l, const string& mt): 
		max(m), buffer(b), num_shared(n), par_induction(p), local_depths(l), max_type(mt) {}

	xformer* operator()(shared_variable* v)
	{
		return new shared_buffer_size(v, max, buffer, num_shared, par_induction, local_depths.find(v)->second, max_type);
	}
};

class private_buffer_size: public xformer {
	const private_variable* v;

public:
	private_buffer_size(const private_variable* v): v(v) {}
	string operator()(const string& old)
	{
		string dec;
		if (v->is_non_scalar()) {
			dec = "const int " + buffer_adaptor(v).size() + "=" + default_buff_size + "; \n";
		}

		return old + dec;
	}

	xformer* clone() const { return new private_buffer_size(*this); }
	string class_name() const { return "private_buffer_size"; }
};

class private_buffer_allocation: public xformer {
	const private_variable* v;
public:
	private_buffer_allocation(const private_variable* _v): v(_v) {}
	string operator()(const string& old)
	{
		string alloc;
		if (v->is_non_scalar()) {
			buffer_adaptor buff(v);
			alloc = buff.declare() + "= (" + buff.type() + "*) _malloc_align(sizeof(" + buff.type() + ")*" + buff.size() + ",7);";
		}
		return old + alloc;
	}

	xformer* clone() const { return new private_buffer_allocation(*this); }
	string class_name() const { return "private_buffer_allocation"; }
};

class shared_buffer_allocation: public conditions_xformer {
public:
	shared_buffer_allocation(shared_variable* v, const condslist& a, const int d): conditions_xformer(v, a, d) {}
	string operator()(const string& old)
	{
		string alloc;
		if (v->is_non_scalar()) {
			buffer_adaptor buff(v);
			alloc = buff.declare() + "= (" + buff.type() + "*) _malloc_align(sizeof(" + buff.type() + ")*" + to_string(depth) + 
					"*" + hug(buff.size() + "+" + to_string(v->stencil_row_spread())) + ",7);";
		}

		return old + alloc;
	}

	xformer* clone() const { return new shared_buffer_allocation(*this); }
	string class_name() const { return "shared_buffer_allocation"; }
};

class dma_list_allocation: public conditions_xformer {
public:
	dma_list_allocation(shared_variable* v, const condslist& a, const int d): conditions_xformer(v, a, d) {}
	string operator()(const string& old)
	{
		string allocation;

		if (v->is_column()) {
			dma_list_adaptor lst(v);
			buffer_adaptor buff(v);
			allocation = lst.declare(depth) + "; \n";

			for (int i = 0; i < depth; ++i) {
				allocation += "allocate_dma_list(&" + lst.name(i) + "," + buff.size() + "+" + to_string(v->stencil_row_spread()) + ",1);\n";
			}
		}

		return old + allocation;
	}

	xformer* clone() const { return new dma_list_allocation(*this); }
	string class_name() const { return "dma_list_allocation"; }
};

class buffer_deallocation: public xformer {
	const region_variable* v;

public:
	buffer_deallocation(const region_variable* v): v(v) {}
	string operator()(const string& old)
	{
		string free;
		if (v->is_non_scalar()) {
			free = "_free_align(" + buffer_adaptor(v).name() + ");";
		}

		return free + old;
	}

	xformer* clone() const { return new buffer_deallocation(*this); }
	string class_name() const { return "buffer_deallocation"; }
};

class dma_list_deallocation: public depth_xformer {
	const shared_variable* v;

public:
	dma_list_deallocation(const shared_variable* v, const int d): depth_xformer(d), v(v) {}
	string operator()(const string& old)
	{
		string deallocation;

		if (v->is_column()) {
			dma_list_adaptor lst(v);

			for (int i = 0; i < depth; ++i) {
				deallocation += "free_dma_list(&" + lst.name(i) + "); \n";
			}
		}

		return deallocation + old;
	}

	xformer* clone() const { return new dma_list_deallocation(*this); }
	string class_name() const { return "dma_list_deallocation"; }
};

class define_next: public xformer {
	const shared_variable* v;

public:
	define_next(const shared_variable* _v): v(_v) {}
	string operator()(const string& old)
	{
		return old + next_adaptor(v).declare() + " = 0; \n";
	}

	xformer* clone() const { return new define_next(*this); }
	string class_name() const { return "define_next"; }
};

class define_buffer: public xformer {
	const shared_variable* v;

public:
	define_buffer(const shared_variable* _v): v(_v) {}
	string operator()(const string& old)
	{
		buffer_adaptor buff(v);
		orig_adaptor orig(v);

		return	old + 
			orig.declare() + "; \n" +
			orig.name() + " = " + buff.name() + "; \n";
	}

	xformer* clone() const { return new define_buffer(*this); }
	string class_name() const { return "define_buffer"; }
};

class define_rem: public xformer {
	const shared_variable* v;

public:
	define_rem(const shared_variable* _v): v(_v) {}
	string operator()(const string& old)
	{
		return	old + rem_adaptor(v).declare() + ";";
	}

	xformer* clone() const { return new define_rem(*this); }
	string class_name() const { return "define_rem"; }
};

class define_full: public xformer {
	const shared_variable* v;

public:
	define_full(const shared_variable* _v): v(_v) {}
	string operator()(const string& old)
	{
		return	old + full_adaptor(v).declare() + ";";
	}

	xformer* clone() const { return new define_full(*this); }
	string class_name() const { return "define_full"; }
};

class reset_buf_sz: public xformer {
	const shared_variable* v;
	const conditions conds;

public:
	reset_buf_sz(const shared_variable* _v, const conditions& c): v(_v), conds(c) {}
	string operator()(const string& old)
	{
		buffer_adaptor buff(v);

		return	buff.size() + "=" + buff.abs() + ";" + 
			"if (" + buff.size() + "> (" + conds.stop + ") - (" + conds.start + ") && (" + conds.stop + ") - (" + conds.start + ") > 0) {" +
				buff.size() + "= (" + conds.stop + ") - (" + conds.start + ") ;}" + 
			old;
	}

	xformer* clone() const { return new reset_buf_sz(*this); }
	string class_name() const { return "reset_buf_sz"; }
};

struct make_reset_buf_sz: public unary_function<shared_variable*, xformer*> {
	const conditions& conds;
	make_reset_buf_sz(const conditions& c): conds(c) {}

	xformer* operator()(shared_variable* v)
	{
		return new reset_buf_sz(v, conds);
	}
};

class reset_rem: public xformer {
	const shared_variable* v;
	const conditions conds;
	const string max_factor;

public:
	reset_rem(const shared_variable* _v, const conditions& c, const string& m): v(_v), conds(c), max_factor(m) {}
	string operator()(const string& old)
	{
		return old + rem_adaptor(v).reset(conds, max_factor) + ";";
	}

	xformer* clone() const { return new reset_rem(*this); }
	string class_name() const { return "reset_rem"; }
};

struct make_reset_rem: public unary_function<shared_variable*, xformer*> {
	const conditions& conds;
	const string& max_factor;
	make_reset_rem(const conditions& c, const string& m): conds(c), max_factor(m) {}

	xformer* operator()(shared_variable* v)
	{
		return new reset_rem(v, conds, max_factor);
	}
};

class reset_full: public xformer {
	const shared_variable* v;
	const string stop;

public:
	reset_full(const shared_variable* _v, const string& b): v(_v), stop(b) {}
	string operator()(const string& old)
	{
		return old + full_adaptor(v).reset(stop) + ";";
	}

	xformer* clone() const { return new reset_full(*this); }
	string class_name() const { return "reset_full"; }
};

struct make_reset_full: public unary_function<shared_variable*, xformer*> {
	const string stop;
	make_reset_full(const string& e): stop(e) {}

	xformer* operator()(shared_variable* v)
	{
		return new reset_full(v, stop);
	}
};

template <class Xrow, class Xcolumn>
struct make_choice: public unary_function<shared_variable*, xformer*>  {
	const condslist& above;
	const depths& local_depths;
	make_choice(const condslist& a, const depths& l):
		above(a), local_depths(l) {}

	xformer* operator()(shared_variable* v)
	{
		xformer* x = NULL;

		if (v->is_row()) {
			x = new Xrow(v, above, local_depths.find(v)->second);
		}
		else if (v->is_column()) {
			x = new Xcolumn(v, above, local_depths.find(v)->second);
		}
		else {
			throw unitialized_access_orientation();
		}

		return x;
	}
};

class init_private_buffer: public xformer {
	const private_variable* v;

public:
	init_private_buffer(const private_variable* v): v(v) {}
	string operator()(const string& old)
	{
		string ret;
		if (v->is_non_scalar()) {
			buffer_adaptor buff(v);
			orig_adaptor orig(v);

			ret =	"dma_get(" +
					buff.name() + "," +
					"(unsigned long)" + orig.name() + "," +
					"sizeof(" + buff.type() + ")*" + buff.size() + ","
					"3); \n" + 
				orig.name() + "=" + buff.name() + ";"
				"dma_wait(3, fn_id); \n";
		}

		return old + ret;
	}

	xformer* clone() const { return new init_private_buffer(*this); }
	string class_name() const { return "init_private_buffer"; }
};

struct define_variable: public xformer {
	const variable var;
	define_variable(const variable& v): var(v) {}

	string operator()(const string& old)
	{
		return old + var.define() + ";";
	}

	xformer* clone() const { return new define_variable(*this); }
	string class_name() const { return "define_variable(" + var.name() + ")"; }
};

struct make_define_variable: public unary_function<shared_variable*, xformer*> {
	xformer* operator()(const variable var)
	{
		return new define_variable(var);
	}
};

class define_reduction: public xformer {
	const reduction_variable* v;
public:
	define_reduction(const reduction_variable* v): v(v) {}
	string operator()(const string& old)
	{
		return old + orig_adaptor(v).declare() + "= *" + v->name() + "; \n";
	}

	xformer* clone() const { return new define_reduction(*this); }
	string class_name() const { return "define_reduction"; }
};

class base_access {
protected:
	shared_variable* v; 
	const conditions conds;

public:
	base_access(shared_variable* _v, const conditions& c): v(_v), conds(c) {}
	virtual ~base_access() {}
	
	virtual string class_name() const = 0; // debugging purposes only

	virtual string iteration() const = 0;
	virtual string next_iteration() const = 0;
	virtual string bounds_check() const = 0;
	virtual string final_iteration() const = 0;
	virtual string next_buffer(const condslist& above, const bool nested) const = 0;
	virtual string this_buffer(const condslist& above, const bool nested) const = 0;
	virtual string first_buffer(const condslist& above, const conditions& off, const string& rep, const bool nested) const = 0;
	virtual string final_buffer() const = 0;
	virtual string remainder_size() const = 0;
	virtual string dma_in(const string& address, const string& local_buffer, const string& tsize) const = 0;
	virtual string dma_out(const string& address, const int depth) const = 0;
	virtual string dma_out(const string& address, const int depth, const string& tsize, const string& next) const = 0;

	virtual string first_buffer_no_off(const condslist& above, const string& rep, const bool nested) const
	{
		return first_buffer(above, conditions(), rep, nested);
	}

	virtual string correction(const string& no_spread, const string& start, const int yes, const int no) const
	{
		if (v->stencil_spread(conds.induction)) {
			return hug(start + "+" + to_string(v->stencil_low(conds.induction)) + "< 0 ?" + to_string(yes) + ":" + to_string(no));
		}
		return no_spread;
	}
};

class row_access: public base_access {
public:
	row_access(shared_variable* v, const conditions& c): base_access(v, c) {}

	string class_name() const
	{
		return "row_access";
	}

	string iteration() const
	{
		string s;
		if (v->is_flat()) {
			s = v->math().ihs(conds.induction).str();
		}
		else {
			s = conds.induction;
		}

		return s;
	}

	string next_iteration() const
	{
		string s;
		if (v->is_flat()) {
			s = v->math().ihs(conds.induction).next_iteration(conds.induction);
		}
		else {
			s = conds.induction + "+1";
		}

		return s;
	}

	string bounds_check() const
	{
		const string& factor = v->math().factor(conds.induction);

		string buffer_size;
		if (factor != "") {
			buffer_size = buffer_adaptor(v).size() + "/" + factor;
		}
		else {
			buffer_size = buffer_adaptor(v).size();
		}

		return conds.induction + "+" + buffer_size;
	}

	string factor() const
	{
		string f;
		if (v->is_flat()) {
			f = v->math().ihs(conds.induction).non_ihs(conds.induction).str();
			if (f != "") {
				f = "*" + f;
			}
		}

		return f;
	}

	string final_iteration() const
	{
		return hug(hug(hug(conds.stop) + "-" + hug(conds.start)) + factor());
	}

	string next_buffer(const condslist& above, const bool nested) const
	{
		add_expr math;
		if (v->is_flat() && v->math().factor(conds.induction) != "") {
			math = v->math().ihs(conds.induction); 
		}
		else {
			math = v->math().remove_stencil(conds.induction).expand_all_inductions(remove_back(above), nested);
		}

		return hug(math.str() + "+" + buffer_adaptor(v).size() + "+" + to_string(v->stencil_low(conds.induction)));
	}

	string this_buffer(const condslist& above, const bool nested) const
	{
		if (v->is_flat()) {
			return v->math().ihs(conds.induction).str();
		}
		else {
			return v->math().remove_stencil(conds.induction).expand_all_inductions(remove_back(above), nested).str();
		}
	}

	string first_buffer(const condslist& above, const conditions& off, const string& rep, const bool nested) const
	{
		if (v->is_flat()) {
			string factor = v->math().factor(conds.induction);
			if (factor != "") {
				factor = "*" + factor;
			}

			return conds.start + factor;
		}
		else {
			const int low = v->stencil_low(conds.induction);
			return v->math().remove_stencil(conds.induction).
				replace_induction(off.induction, off.start + "+" + to_string(low)).
				replace_induction(conds.induction, rep).
				expand_all_inductions(remove_back(above), nested).str();
		}
	}

	string final_buffer() const
	{
		buffer_adaptor buff(v);

		string offset;
		if (!v->is_flat()) {
			offset = v->math().non_ihs(conds.induction).str() + "+";
		}

		return offset + hug(full_adaptor(v).name() + factor());
	}

	string remainder_size() const
	{
		const string& factor = v->math().factor(conds.induction);
		if (v->is_flat() && factor != "") {
			return rem_adaptor(v).name() + "*" + factor;
		}
		else {
			return rem_adaptor(v).name();
		}
	}

	string dma_in(const string& address, const string& local_buffer, const string& tsize) const
	{
		return "dma_get(" + local_buffer +  "," + address + ","
				"sizeof(" + buffer_adaptor(v).type() + ") * (" + tsize + ")," +
				next_adaptor(v).name() + ");";
	}

	string dma_out(const string& address, const int depth) const
	{
		return dma_out(address, depth, buffer_adaptor(v).size(), next_adaptor(v).name());
	}

	string dma_out(const string& address, const int depth, const string& tsize, const string& next) const
	{
		buffer_adaptor buff(v);
		orig_adaptor orig(v);

		return "dma_put(" + orig.name() + "," + address + "," + "sizeof(" + buff.type() + ")*" + tsize + "," + next + ");";
	}
};

class column_access: public base_access {
public:
	column_access(shared_variable* v, const conditions& c): base_access(v, c) {}

	string class_name() const
	{
		return "column_access";
	}

	string iteration() const
	{
		return conds.induction;
	}

	string next_iteration() const
	{
		return iteration() + "+1";
	}

	string bounds_check() const
	{
		return conds.induction + "+" + buffer_adaptor(v).size();
	}

	string final_iteration() const
	{
		return hug(hug(conds.stop) + "-" + hug(conds.start));
	}

	string next_buffer(const condslist& above, const bool nested) const
	{
		return v->math().remove_stencil(conds.induction).
			expand_all_inductions(remove_back(above), nested).
			add_iteration(conds.induction, hug(buffer_adaptor(v).size() + "+" + to_string(v->stencil_low(conds.induction))));
	}

	string this_buffer(const condslist& above, const bool nested) const
	{
		return v->math().remove_stencil(above.back().induction).expand_all_inductions(remove_back(above), nested).str();
	}

	string first_buffer(const condslist& above, const conditions& off, const string& rep, const bool nested) const
	{
		return v->math().remove_stencil(conds.induction).
			replace_induction(conds.induction, rep).
			replace_induction(off.induction, off.start + "+" + to_string(v->stencil_low(off.induction))).
			expand_all_inductions(remove_back(above), nested).str();
	}

	string final_buffer() const
	{
		return v->math().replace_induction(conds.induction, full_adaptor(v).name()).str();
	}

	string remainder_size() const
	{
		return rem_adaptor(v).name();
	}

	string stride() const
	{
                string s;
		const int total = v->dimensions().size() - v->math().index(conds.induction);
		list<string>::const_reverse_iterator d = v->dimensions().rbegin();
		for (int i = 0; i < total; ++i, ++d) {
			s += *d + "*";
		}

		return s;
	}

	string dma_in(const string& address, const string& local_buffer, const string& tsize) const
	{
		dma_list_adaptor lst(v);
		buffer_adaptor buff(v);
		next_adaptor next(v);

		return	"add_to_dma_list(&" + lst.name(next.name()) + "," + tsize + "," + address + ","
				"sizeof(" + buff.type() + "), " + 
				stride() + "sizeof(" + buff.type() + "),1);" +
			"dma_getl(" + local_buffer + "," + address + "," + "&" + lst.name(next.name()) + "," + 
				next.name() + "," "1," + "sizeof(" + buff.type() + "));";
	}

	string dma_out(const string& address, const int depth) const
	{
		return dma_out(address, depth, buffer_adaptor(v).size(), hug(next_adaptor(v).name() + "+" + to_string(depth - 1)) + "%" + to_string(depth));
	}

	string dma_out(const string& address, const int depth, const string& tsize, const string& next) const
	{
		dma_list_adaptor lst(v);
		buffer_adaptor buff(v);
		orig_adaptor orig(v);
		string make_list;

		if (depth < 3) {
			make_list = "add_to_dma_list(&" + lst.name(next) + "," + 
					tsize + "," +
					address + ","
					"sizeof(" + buff.type() + "), " + 
					stride() + "sizeof(" + buff.type() + "),"
					"1);";
		}

		return 	make_list + 
			"dma_putl(" + orig.name() + "," + address + "," + "&" + lst.name(next) + "," + next + "," "1," + "sizeof(" + buff.type() + "));";
	}
};

template <class Access>
class gen_in_first: public conditions_xformer, public nested_xformer, public Access {

	string local_buffer(const string& i)
	{
		buffer_adaptor buff(v);
		return hug(hug(buff.name() + "+" + hug(hug(buff.abs() + "+" + to_string(v->stencil_spread(Access::conds.induction))) + "*" + i)));
	}

public:
	gen_in_first(shared_variable* v, const condslist& a, const int d): conditions_xformer(v, a, d), Access(v, a.back()) {}
	string operator()(const string& old)
	{
		buffer_adaptor buff(v);
		next_adaptor next(v);

		const string tsize = buffer_adaptor(v).size() + "+" + to_string(v->stencil_spread(Access::conds.induction));
		string dma;

		if (v->is_off_induction_stencil()) {
			for (condslist::const_iterator it = above.begin(); it != above.end(); ++it) {
				if (*it == Access::conds) {
					continue;
				}
				if (v->stencil_spread(it->induction)) {
					for (int i = 0; i < v->stencil_spread(it->induction) + 1; ++i) {
						const string more = correction("0", Access::conds.induction, abs(v->stencil_low(Access::conds.induction)), 0);
						const string less = correction("0", Access::conds.induction, 0, v->stencil_low(Access::conds.induction));
						dma += dma_in("(unsigned long)" + hug(v->name() + "+" + 
							Access::first_buffer(above, *it, Access::conds.induction + "+" + less, nested)), 
							local_buffer(to_string(i)) + "+" + more, 
							tsize + "-" + more);
					}
				}
			}
		}
		else {
			const int low = v->stencil_low(Access::conds.induction);
			const string more = correction("0", Access::conds.start, low, 0);
			const string less = correction(Access::conds.start, Access::conds.start, 0, low);
			dma = dma_in("(unsigned long)" + hug(v->name() + "+" + 
					Access::first_buffer_no_off(above, less, nested)), 
					local_buffer(next.name()) + "+" + more, 
					tsize + "-" + more);
		}

		return old + "cellgen_dma_prep_start();" + dma + "cellgen_dma_prep_stop();";
	}

	xformer* clone() const { return new gen_in_first<Access>(*this); }
	string class_name() const { return "gen_in_first<" + Access::class_name() + ">"; }
};

template <class Access>
struct gen_in: public conditions_xformer, public remainder_xformer, public nested_xformer, public Access {
	gen_in(shared_variable* v, const condslist& a, const int d): conditions_xformer(v, a, d), Access(v, a.back()) {}
	string operator()(const string& old)
	{
		next_adaptor next(v);
		buffer_adaptor buff(v);
		orig_adaptor orig(v);

		const string spread = to_string(v->stencil_spread(above.back().induction));
		const string wait_next = "dma_wait(" + next.name() + ", fn_id);";
		const string wait_prev = "dma_wait(" + prev.name() + ", fn_id);";
		const string rotate_next = next.name() + "=" + hug(next.name() + "+1") + "%" + to_string(depth) + ";";

		if (is_remainder) {
			return old +
				orig.name() + "=" + buff.name() + "+" + hug(buff.abs() + "+" + spread) + "*" + next.name() + ";" +
				wait_next;
		}
		else {
			string local_buffer = buff.name() + "+ ((" + buff.abs() + "+" + to_string(v->stencil_spread(Access::conds.induction)) + ")*" + next.name() + ")";
			return old +
				"cellgen_dma_prep_start();" + 
				prev.name() + "=" + next.name() + ";" +
				rotate_next + 
				wait_next +
				dma_in("(unsigned long)" + hug(v->name() + "+" + Access::next_buffer(above, nested)), 
						local_buffer,
						hug(Access::bounds_check() + "<" + full_adaptor(v).name() + "?" + 
							buff.size() + "+" + spread + ":" + Access::remainder_size() + "+" + spread)) + 
				orig.name() + "=" + buff.name() + "+" + hug(buff.abs() + "+" + spread) + "*" + prev.name() + ";" +
				wait_prev +
				"cellgen_dma_prep_stop();";
		}
	}

	xformer* clone() const { return new gen_in<Access>(*this); }
	string class_name() const { return "gen_in<" + Access::class_name() + ">"; }
};

template <class Access>
struct gen_out: public conditions_xformer, public remainder_xformer, public nested_xformer, public Access {
	gen_out(shared_variable* v, const condslist& a, const int d): conditions_xformer(v, a, d), Access(v, a.back()) {}
	string operator()(const string& old)
	{
		buffer_adaptor buff(v);
		orig_adaptor orig(v);
		next_adaptor next(v);

		string var_switch;
		if (depth < 3) {
			var_switch = next.name() + "=" + hug(next.name() + "+1") + "%" + to_string(depth) + "; \n" +
					orig.name() + "=" + buff.name() + "+" + buff.abs() + "*" + next.name() + "; \n";
		}

		const string wait = "dma_wait(" + next.name() + ", fn_id);";

		string dma;
		if (is_remainder) {
			dma = dma_out("(unsigned long)" + hug(v->name() + "+" + Access::final_buffer()), depth, Access::remainder_size(), next.name()); 
			return dma + wait + old;
		}
		else {
			dma = dma_out("(unsigned long)" + hug(v->name() + "+" + Access::this_buffer(above, nested)), depth);
			return "cellgen_dma_prep_start();" + dma + var_switch + wait + "cellgen_dma_prep_stop();" + old;
		}
	}

	xformer* clone() const { return new gen_out<Access>(*this); }
	string class_name() const { return "gen_out<" + Access::class_name() + ">"; }
};

#endif // XFORMERS_H
