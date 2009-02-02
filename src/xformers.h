#ifndef XFORMERS_H
#define XFORMERS_H

#include <string>
#include <list>
using namespace std;

#include <boost/regex.hpp>

#include "variable.h"
#include "operations.h"

inline int count_ocurrences(const string& code, const string& find)
{
	smatch res;
	string::const_iterator start = code.begin();
	const string::const_iterator end = code.end();
	match_flag_type flags = match_default; 
	int ocurrences = 0;

	while (regex_search(start, end, res, regex(find), flags)) {
		++ocurrences;

		start = res[0].second;
		flags |= boost::match_prev_avail; 
		flags |= boost::match_not_bob;
	}

	return ocurrences;
}

struct xformer: public unary_function<const string&, string> {
	virtual ~xformer() {}
	virtual void unroll_me(int u) {}
	virtual void epilogue_me() {}
	virtual string operator()(const string& old) = 0;
	virtual xformer* clone() const = 0;
	virtual string class_name() const = 0; // For debugging purposes only.

	virtual operations cost() 
	{
		operations ops;
		const string code = operator()("");

		ops.add(INT, count_ocurrences(code, "\\+"));
		ops.sub(INT, count_ocurrences(code, "\\-"));
		ops.mul(INT, count_ocurrences(code, "\\*"));
		ops.div(INT, count_ocurrences(code, "\\\\"));
		ops.mod(INT, count_ocurrences(code, "\\%"));

		return ops;
	}
};

typedef list<xformer*> xformerlist;

template <class X, class V>
struct make_xformer: public unary_function<const V*, xformer*> {
	xformer* operator()(const V* v)
	{
		return new X(v);
	}
};

struct conditions {
	string start;
	string induction;
	string stop;

	conditions()
		{}
	conditions(const string& _start, const string& _induction, const string& _stop):
		start(_start), induction(_induction), stop(_stop)
		{}
};
typedef list<conditions> condslist;

class conditions_xformer: virtual public xformer {
public:
	// FIXME: this should not be public
	shared_variable* v;
protected:
	conditions conds;
public:
	conditions_xformer(shared_variable* _v, const conditions& c): v(_v), conds(c) {}
};

template <class X>
struct make_conditions: public unary_function<shared_variable*, xformer*> {
	const conditions& conds;
	make_conditions(const conditions& c): conds(c) {}
	xformer* operator()(shared_variable* v)
	{
		return new X(v, conds);
	}
};

const int NO_UNROLL = 0;

class unrollable_xformer: public conditions_xformer {
protected:
	int unroll;
public:
	unrollable_xformer(shared_variable* v, const conditions& c): conditions_xformer(v, c), unroll(NO_UNROLL) {}
	unrollable_xformer(shared_variable* v, const conditions& c, const int u): conditions_xformer(v, c), unroll(u) {}
	unrollable_xformer(shared_variable* v, const int u): conditions_xformer(v, conditions()), unroll(u) {}
	virtual void unroll_me(int u)
	{
		// TODO: Do I need to know induction information to determine if 
		// unrolling needs to happen? I used to think so. Now I'm not sure.
		unroll = u;
	}
};

class epilogue_xformer: virtual public xformer {
protected:
	bool epilogue;
public:
	epilogue_xformer(): epilogue(false) {}
	void epilogue_me()
	{
		epilogue = true;
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
protected:
	const shared_variable* v;

public:
	to_buffer_space(const shared_variable* _v): v(_v) {}
};

class flat_buffer_space: virtual public to_buffer_space, public epilogue_xformer {
	const add_expr math;
public:
	flat_buffer_space(const shared_variable* v, const add_expr& m): to_buffer_space(v), math(m) {}
	string operator()(const string& old)
	{
		buffer_adaptor buff(v);
		orig_adaptor orig(v);
		string buff_size;

		if (epilogue) {
			buff_size = "(" + buff.size() + "/" + "unroll_factor)";
		}
		else {
			buff_size = buff.size();
		}

		return old + orig.name() + "[(" + math.str() + ")%" + buff_size + "]";
	}

	xformer* clone() const { return new flat_buffer_space(*this); }
	string class_name() const { return "flat_buffer_space"; }
};

class multi_buffer_space: public to_buffer_space {
	const string induction;
public:
	multi_buffer_space(const shared_variable* v, const string& i): to_buffer_space(v), induction(i) {}
	string operator()(const string& old)
	{
		buffer_adaptor buff(v);
		orig_adaptor orig(v);

		return old + orig.name() + "[(" + induction + ")%" + buff.size() + "]";
	}

	xformer* clone() const { return new multi_buffer_space(*this); }
	string class_name() const { return "multi_buffer_space"; }
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

class total_timer_stop: public xformer {
public:
	string operator()(const string& old)
	{
		return old + "\n cellgen_total_stop(); \n";
	}

	xformer* clone() const { return new total_timer_stop(*this); }
	string class_name() const { return "total_timer_stop"; }
};

class variable_name: public xformer {
	const variable v;
public:
	variable_name(const variable v): v(v) {}
	string operator()(const string&)
	{
		return v.name();
	}

	xformer* clone() const { return new variable_name(*this); }
	string class_name() const { return "variable_name"; }
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
	const shared_variable* v;
public:
	compute_bounds(const shared_variable* v): v(v) {}
	string operator()(const string& old)
	{
		string size;
		if (v) { // TODO: I don't like this NULL check. I'd rather it be a default value. Not sure how.
			size = buffer_adaptor(v).size();
		}
		else {
			size = default_buff_size;
		}
		return old + "compute_bounds(&SPE_start, &SPE_stop," + size + ");";
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

class shared_buffer_size: public unrollable_xformer {
	const int buffer;

public:
	shared_buffer_size(shared_variable* v, const int b, const int u): unrollable_xformer(v, u), buffer(b) {}
	string operator()(const string& old)
	{
		string declaration;
		if (v->depth() > 0 ) {
			string def;

			// User specified buffer overrides fitting buffer to unrolling
			if (buffer) {
				def = "(" + to_string(buffer) + ")";
			}
			else if (unroll) {
				if (v->is_row()) {
					def = "(" + to_string(v->dimensions().back()) + "*" + to_string(unroll) + ")";
				}
				else if (v->is_column()) {
					def = "(" + to_string(unroll) + ")";
				}
				else {
					throw unitialized_access_orientation();
				}

				// What's the right thing to do with the factors?
				//def = "(" + to_string(unroll) + v->math().factor(induction) + ")";
			}
			else {
				def = default_buff_size;
			}
			declaration = const_variable("int", buffer_adaptor(v).size(), def).define() + ";";
		}

		return old + declaration;
	}

	xformer* clone() const { return new shared_buffer_size(*this); }
	string class_name() const { return "shared_buffer_size"; }
};

struct make_shared_buffer_size: public unary_function<shared_variable*, xformer*> {
	const int buffer;
	const int unroll;
	make_shared_buffer_size(const int b, const int u): buffer(b), unroll(u) {}

	xformer* operator()(shared_variable* v)
	{
		return new shared_buffer_size(v, buffer, unroll);
	}
};

class private_buffer_size: public xformer {
	const private_variable* v;

public:
	private_buffer_size(const private_variable* v): v(v) {}
	string operator()(const string& old)
	{
		string declaration;
		if (v->depth() > 0 ) {
			declaration = "const int " + buffer_adaptor(v).size() + "=" + default_buff_size + "; \n";
		}

		return old + declaration;
	}

	xformer* clone() const { return new private_buffer_size(*this); }
	string class_name() const { return "private_buffer_size"; }
};

class buffer_allocation: public xformer {
	const region_variable* v;

public:
	buffer_allocation(const region_variable* v): v(v) {}
	string operator()(const string& old)
	{
		string allocation;

		if (v->depth() > 0) {
			buffer_adaptor buff(v);
			allocation = buff.declare() + "= _malloc_align(sizeof(" + buff.type() + ")*" + buff.depth() + "*" + buff.size() + ",7);";
		}

		return old + allocation;
	}

	xformer* clone() const { return new buffer_allocation(*this); }
	string class_name() const { return "buffer_allocation"; }
};

class dma_list_allocation: public xformer {
	const shared_variable* v;

public:
	dma_list_allocation(const shared_variable* v): v(v) {}
	string operator()(const string& old)
	{
		string allocation;

		if (v->is_column()) {
			// FIXME: this is not the correct way to define and allocate a DMA list.
			dma_list_adaptor list(v);
			buffer_adaptor buff(v);
			allocation = list.declare() + "; \n";

			for (int i = 0; i < v->depth(); ++i) {
				allocation += "allocate_dma_list(&" + list.name(i) + "," + buff.size() + ",1);\n";
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
		string deallocation;

		if (v->depth() > 0) {
			deallocation = "_free_align(" + buffer_adaptor(v).name() + ");";
		}

		return deallocation + old;
	}

	xformer* clone() const { return new buffer_deallocation(*this); }
	string class_name() const { return "buffer_deallocation"; }
};

class dma_list_deallocation: public xformer {
	const shared_variable* v;

public:
	dma_list_deallocation(const shared_variable* v): v(v) {}
	string operator()(const string& old)
	{
		string deallocation;

		if (v->is_column()) {
			dma_list_adaptor list(v);

			for (int i = 0; i < v->depth(); ++i) {
				deallocation += "free_dma_list(&" + list.name(i) + "); \n";
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

template <class Xrow, class Xcolumn>
struct make_choice: public unary_function<shared_variable*, xformer*>  {
	const conditions& conds_row;
	const conditions& conds_column;
	make_choice(const conditions& c): conds_row(c), conds_column(c) {}
	make_choice(const conditions& c_row, const conditions& c_column): conds_row(c_row), conds_column(c_column) {}

	xformer* operator()(shared_variable* v)
	{
		xformer* x = NULL;

		if (v->is_row()) {
			x = new Xrow(v, conds_row);
		}
		else if (v->is_column()) {
			x = new Xcolumn(v, conds_column);
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

			ret =	"DMA_get(" +
					buff.name() + "," +
					"(unsigned long)" + orig.name() + "," +
					"sizeof(" + buff.type() + ")*" + buff.size() + ","
					"3); \n" + 
				orig.name() + "=" + buff.name() + ";"
				"MMGP_SPE_dma_wait(3, fn_id); \n";
		}

		return old + ret;
	}

	xformer* clone() const { return new init_private_buffer(*this); }
	string class_name() const { return "init_private_buffer"; }
};

class define_unroll_boundaries: public xformer {
	const string start;
	const string stop;
	const int unroll;
public:
	define_unroll_boundaries(const string& t, const string& p, const int u): start(t), stop(p), unroll(u) {}
	string operator()(const string& old)
	{
		return	old + 
			unrolled.declare() + "=" + start + " + ((" + stop + " - " + start + ") / unroll_factor) * unroll_factor; " + 
			epilogue.declare() + "=" + unrolled.name() + " + ((" + stop + " - " + start + ") % unroll_factor);";
	}

	xformer* clone() const { return new define_unroll_boundaries(*this); }
	string class_name() const { return "define_unroll_boundaries"; }
};

struct define_prev: public xformer {
	string operator()(const string& old)
	{
		return old + prev.define() + ";";
	}

	xformer* clone() const { return new define_prev(*this); }
	string class_name() const { return "define_prev"; }
};

struct define_const: public xformer {
	const const_variable unroll_factor;
	define_const(const const_variable uf): unroll_factor(uf) {}

	string operator()(const string& old)
	{
		return old + unroll_factor.define() + ";";
	}

	xformer* clone() const { return new define_const(*this); }
	string class_name() const { return "define_const"; }
};

class reduction_declare: public xformer {
	const reduction_variable* v;
public:
	reduction_declare(const reduction_variable* v): v(v) {}
	string operator()(const string& old)
	{
		return old + orig_adaptor(v).declare() + "= *" + v->name() + "; \n";
	}

	xformer* clone() const { return new reduction_declare(*this); }
	string class_name() const { return "reduction_declare"; }
};

class base_access {
protected:
	const conditions conds;

public:
	// FIXME: this should be protected, at least
	shared_variable* v; 

	base_access(shared_variable* _v, const conditions& c): conds(c), v(_v) {}
	virtual ~base_access() {}
	
	virtual string class_name() const = 0; // debugging purposes only

	virtual string iteration() const = 0;
	virtual string next_iteration() const = 0;
	virtual string bounds_check() const = 0;
	virtual string final_iteration() const = 0;
	virtual string access() const = 0;
	virtual string previous_access() const = 0;
	virtual string first_access() const = 0;
	virtual string final_size() const = 0;
	virtual string final_access() const = 0;
	virtual string dma_in(const string& address) const = 0;
	virtual string dma_out(const string& address) const = 0;
	virtual string dma_out(const string& address, const string& tsize) const = 0;
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
		return conds.induction + "+1";
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
		return "((" + conds.stop + "-" + conds.start + ")" + factor() + ")";
	}

	string access() const
	{
		string a;
		if (v->is_flat()) {
			a = v->math().ihs(conds.induction).str(); 
		}
		else {
			a = v->math().str();
		}

		return a + "+" + buffer_adaptor(v).size();
	}

	string previous_access() const
	{
		string a;
		if (v->is_flat()) {
			a = v->math().next_iteration(conds.induction);
		}
		else {
			a = v->math().str() + "+1";
		}

		return a + "-" + buffer_adaptor(v).size();
	}

	string first_access() const
	{
		string first;

		if (!v->is_flat()) {
			first = v->math().zero_induction(conds.induction);
		}
		else {
			string factor = v->math().ihs(conds.induction).non_ihs(conds.induction).str();
			if (factor != "") {
				factor = "*" + factor;
			}

			first = conds.start + factor;
		}

		return first;
	}

	string final_size() const
	{
		buffer_adaptor buff(v);
		return "(((" + conds.stop + "-" + conds.start + ")" + factor() + ")%" + buff.size() + ")";
	}

	string final_access() const
	{
		buffer_adaptor buff(v);

		string offset;
		if (!v->is_flat()) {
			offset = conds.induction + "*" + v->dimensions().back() + "+";
		}

		return offset + "(" + conds.stop + factor() + ")-" + final_size();
	}

	string dma_in(const string& address) const
	{
		buffer_adaptor buff(v);
		next_adaptor next(v);

		return "DMA_get(" + buff.name() + "+" + buff.size() + "*" + next.name() + "," +
				address + ","
				"sizeof(" + buff.type() + ") *" + buff.size() + "," +
				next.name() + ");";
	}

	string dma_out(const string& address) const
	{
		return dma_out(address, buffer_adaptor(v).size());
	}

	string dma_out(const string& address, const string& tsize) const
	{
		buffer_adaptor buff(v);
		orig_adaptor orig(v);
		next_adaptor next(v);

		return "DMA_put(" + orig.name() + "," + 
				address + "," +
				"sizeof(" + buff.type() + ")*" + tsize + "," +
				next.name() + ");";
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
		return "(" + conds.stop + "-" + conds.start + ")";
	}

	string access() const
	{
		return "(" + v->math().add_iteration(conds.induction, buffer_adaptor(v).size()) + ")";
	}

	string previous_access() const
	{
		return "(" + v->math().add_iteration(conds.induction, "(1-" + buffer_adaptor(v).size() + ")") + ")";
	}

	string first_access() const
	{
		return v->math().zero_induction(conds.induction);
	}

	string final_size() const
	{
		return "((" + conds.stop + "-" + conds.start + ")%" + buffer_adaptor(v).size() + ")";
	}

	string final_access() const
	{
		/*
		buffer_adaptor buff(v);
		const string dim1 = v->dimensions().front();
		const string dim2 = v->dimensions().back();

		return "((" + dim1 + "-(" + dim1 + "%" + buff.size() + "))*" + dim2 + ")+" + conds.induction;
		*/
		return "(" + v->math().replace_induction(conds.induction, "(" + conds.stop + "-" + final_size() + "))");
	}

	string dma_in(const string& address) const
	{
		dma_list_adaptor list(v);
		buffer_adaptor buff(v);
		next_adaptor next(v);

		return	"add_to_dma_list(&" + list.name(next.name()) + "," + 
				buff.size() + "," +
				address + ","
				"sizeof(" + buff.type() + "), " + 
				v->dimensions().back() + "* sizeof(" + buff.type() + "),"
				"1);" +
			"DMA_getl(" + buff.name() + "+" + buff.size() + "*" + next.name() + "," +
				address + "," +
				"&" + list.name(next.name()) + "," + 
				next.name() + ","
				"1," +
				"sizeof(" + buff.type() + "));";
	}

	string dma_out(const string& address) const
	{
		return dma_out(address, buffer_adaptor(v).size());
	}

	string dma_out(const string& address, const string& tsize) const
	{
		dma_list_adaptor list(v);
		buffer_adaptor buff(v);
		next_adaptor next(v);
		orig_adaptor orig(v);
		string make_list;
		const string depth = to_string(v->depth());

		if (v->depth() < 3) {
			make_list = "add_to_dma_list(&" + list.name("(" + next.name() + "+(" + depth + "-1))%" + depth) + "," + 
					tsize + "," +
					address + ","
					"sizeof(" + buff.type() + "), " + 
					v->dimensions().back() + "* sizeof(" + buff.type() + "),"
					"1);";
		}

		return 	make_list + 
			"DMA_putl(" + orig.name() + "," +
				address + "," +
				"&" + list.name("(" + next.name() + "+(" + depth + "-1))%" + depth) + "," + 
				"(" + next.name() + "+(" + depth + "-1))%" + depth + ","
				"1," +
				"sizeof(" + buff.type() + "));";
	}
};

template <class Access>
class gen_in_first: public unrollable_xformer, public epilogue_xformer, public Access {
public:
	gen_in_first(shared_variable* v, const conditions& c): unrollable_xformer(v, c), Access(v, c) {}
	string operator()(const string& old)
	{
		next_adaptor next(v);
		buffer_adaptor buff(v);
		orig_adaptor orig(v);

		return old + 
			"cellgen_dma_prep_start();" + 
			dma_in("(unsigned long)(" + v->name() + " + (" + Access::first_access() + "))") + 
			"cellgen_dma_prep_stop();";
	}

	xformer* clone() const { return new gen_in_first<Access>(*this); }
	string class_name() const { return "gen_in_first<" + Access::class_name() + ">"; }
};

template <class Access>
struct gen_in: public unrollable_xformer, public epilogue_xformer, public Access {
	gen_in(shared_variable* v, const conditions& c): unrollable_xformer(v, c), Access(v, c) {}
	string operator()(const string& old)
	{
		next_adaptor next(v);
		buffer_adaptor buff(v);
		orig_adaptor orig(v);

		const string wait_next = "MMGP_SPE_dma_wait(" + next.name() + ", fn_id);";
		const string wait_prev = "MMGP_SPE_dma_wait(" + prev.name() + ", fn_id);";
		const string rotate_next = next.name() + "= (" + next.name() + "+1)%" + buff.depth() + ";";
		const string outer_if = "if (!((" + Access::iteration() + ")%" + buff.size() + "))";
		const string inner_if = "if (" + Access::bounds_check() + "<" + conds.stop + ")";

		return old + 
			"cellgen_dma_prep_start();" + 
			outer_if + "{ \n" +
				prev.name() + "=" + next.name() + ";" +
				rotate_next + 
				wait_next +
				inner_if + "{" + 
					dma_in("(unsigned long)(" + v->name() + "+" + Access::access() + ")") + 
				"}" +
				orig.name() + "=" + buff.name() + "+" + buff.size() + "*" + prev.name() + ";" +
				wait_prev +
			"}"
			"cellgen_dma_prep_stop();";
	}

	xformer* clone() const { return new gen_in<Access>(*this); }
	string class_name() const { return "gen_in<" + Access::class_name() + ">"; }
};

template <class Access>
struct gen_out: public unrollable_xformer, public epilogue_xformer, public Access {
	gen_out(shared_variable* v, const conditions& c): unrollable_xformer(v, c), Access(v, c) {}
	string operator()(const string& old)
	{
		buffer_adaptor buff(v);
		orig_adaptor orig(v);
		next_adaptor next(v);
		string var_switch;
		string if_statement;

		if (!unroll) {
			if_statement = "if (!((" + Access::next_iteration() + ")%" + buff.size() + "))";
		}

		if (v->depth() < 3) {
			var_switch =	next.name() + "=(" + next.name() + "+1)%" + buff.depth() + "; \n" +
					orig.name() + "=" + buff.name() + "+" + buff.size() + "*" + next.name() + "; \n" +
					"MMGP_SPE_dma_wait(" + next.name() + ", fn_id);";
		}

		return "cellgen_dma_prep_start();" +
			if_statement + "{ \n" +
					dma_out("(unsigned long)(" + v->name() + "+" + Access::previous_access() + ")") +
					var_switch +
			"}" 
			"cellgen_dma_prep_stop();" +
			old;
	}

	xformer* clone() const { return new gen_out<Access>(*this); }
	string class_name() const { return "gen_out<" + Access::class_name() + ">"; }
};

template <class Access>
struct gen_out_final: public unrollable_xformer, public epilogue_xformer, public Access {
	gen_out_final(shared_variable* v, const conditions& c): unrollable_xformer(v, c), Access(v, c) {}
	string operator()(const string& old)
	{
		string ret;

		if (!unroll) {
			buffer_adaptor buff(v);
			orig_adaptor orig(v);
			next_adaptor next(v);
			const string if_statement = "if ((" + Access::final_iteration() + ")%" + buff.size() + ")";

			ret =	"cellgen_dma_prep_start(); " +
				if_statement + "{ " +
					prev.name() + "=" + next.name() + ";" +
					next.name() + "=(" + next.name() + "+1)%" + buff.depth() + "; " +
					dma_out("(unsigned long)(" + v->name() + "+" + Access::final_access() + ")", Access::final_size()) +
				"} \n"
				"MMGP_SPE_dma_wait(" + prev.name() + ", fn_id); " +
				"MMGP_SPE_dma_wait(" + next.name() + ", fn_id); " 
				"MMGP_SPE_dma_wait((" + next.name() + "+(" + to_string(v->depth()) + "-1))%" + to_string(v->depth()) + ", fn_id); "
				"cellgen_dma_prep_stop(); ";
		}

		return old + ret;
	}

	xformer* clone() const { return new gen_out_final<Access>(*this); }
	string class_name() const { return "gen_out_final<" + Access::class_name() + ">"; }
};

#endif // XFORMERS_H
