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
	virtual void leftover_me() {} // Sigh. It's a hack to put this here, but it makes life so much easier.
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

	bool operator==(const conditions& o) const
	{
		return start == o.start && induction == o.induction && stop == o.stop;
	}
};
typedef list<conditions> condslist;

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
public:
	// FIXME: this should not be public
	shared_variable* v;
protected:
	conditions conds;
public:
	conditions_xformer(shared_variable* _v, const conditions& c, const int d): depth_xformer(d), v(_v), conds(c) {}
};

template <class X>
struct make_conditions: public unary_function<shared_variable*, xformer*> {
	const conditions& conds;
	const depths& local_depths;
	make_conditions(const conditions& c, const depths& l): conds(c), local_depths(l) {}
	xformer* operator()(shared_variable* v)
	{
		return new X(v, conds, local_depths.find(v)->second);
	}
};

class leftover_xformer: virtual public xformer {
protected:
	bool is_leftover;
public:
	leftover_xformer(): is_leftover(false) {}
	virtual void leftover_me()
	{
		is_leftover = true;
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
	const conditions conds;
	const variable index;
public:
	to_buffer_space(const shared_variable* _v, const add_expr& m, const conditions& c, const variable& i): 
		v(_v), math(m), conds(c), index(i) {}
	string operator()(const string& old)
	{
		string offset;
		string factor;
		if (v->is_flat()) {
			string offset = math.non_ihs(conds.induction).str();
			string factor = math.ihs(conds.induction).non_ihs(conds.induction).str();
			if (offset != "") {
				offset = "+" + offset;
			}
			if (factor != "") {
				factor = "*" + factor;
			}
		}

		return old + orig_adaptor(v).name() + "[" + index.name() + factor + offset + "]";
	}

	xformer* clone() const { return new to_buffer_space(*this); }
	string class_name() const { return "to_buffer_space"; }
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

class buffer_loop_start: public leftover_xformer {
	const variable index;
	const string buffer_size;
	const string leftover_size;
public:
	buffer_loop_start(const variable& i, const string& b, const string& l): index(i), buffer_size(b), leftover_size(l) {}
	string operator()(const string& old)
	{
		return old + 
			"for (" + index.name() + "= 0;" + index.name() + "<" + 
			(is_leftover ? leftover_size : buffer_size) + 
			"; ++" + index.name() + ") {"; 
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
	const variable var;
public:
	if_clause(const variable& v): var(v) {}
	string operator()(const string& old)
	{
		return "if (" + var.name() + ")";
	}

	xformer* clone() const { return new if_clause(*this); }
	string class_name() const { return "if_clause"; }
};

class total_timer_stop: public xformer {
public:
	string operator()(const string& old)
	{
		return "cellgen_total_stop();" + old;
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
	const string buffer_size;
public:
	compute_bounds(const string& b): buffer_size(b) {}
	string operator()(const string& old)
	{
		return old + "compute_bounds(&SPE_start, &SPE_stop," + buffer_size + ");";
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

class shared_buffer_size: public depth_xformer {
	const shared_variable* v;
	const int buffer;

public:
	shared_buffer_size(const shared_variable* _v, const int b, const int d): depth_xformer(d), v(_v), buffer(b) {}
	string operator()(const string& old)
	{
		string declaration;
		if (depth > 0) {
			string def;

			if (buffer) {
				def = "(" + to_string(buffer) + ")";
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
	const depths& local_depths;
	make_shared_buffer_size(const int b, const depths& l): buffer(b), local_depths(l) {}

	xformer* operator()(shared_variable* v)
	{
		return new shared_buffer_size(v, buffer, local_depths.find(v)->second);
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

class buffer_allocation: public depth_xformer {
	const region_variable* v;

public:
	buffer_allocation(const region_variable* v, const int d): depth_xformer(d), v(v) {}
	string operator()(const string& old)
	{
		string alloc;
		if (v->is_non_scalar()) {
			buffer_adaptor buff(v);
			alloc = buff.declare() + "= _malloc_align(sizeof(" + buff.type() + ")*" + to_string(depth) + "*" + buff.size() + ",7);";
		}

		return old + alloc;
	}

	xformer* clone() const { return new buffer_allocation(*this); }
	string class_name() const { return "buffer_allocation"; }
};

class dma_list_allocation: public depth_xformer {
	const shared_variable* v;

public:
	dma_list_allocation(const shared_variable* v, const int d): depth_xformer(d), v(v) {}
	string operator()(const string& old)
	{
		string allocation;

		if (v->is_column()) {
			dma_list_adaptor lst(v);
			buffer_adaptor buff(v);
			allocation = lst.declare(depth) + "; \n";

			for (int i = 0; i < depth; ++i) {
				allocation += "allocate_dma_list(&" + lst.name(i) + "," + buff.size() + ",1);\n";
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

template <class Xrow, class Xcolumn>
struct make_choice: public unary_function<shared_variable*, xformer*>  {
	const conditions& conds_row;
	const conditions& conds_column;
	const depths& local_depths;
	make_choice(const conditions& c, const depths& l):
		conds_row(c), conds_column(c), local_depths(l) {}
	make_choice(const conditions& c_row, const conditions& c_column, const depths& l):
		conds_row(c_row), conds_column(c_column), local_depths(l) {}

	xformer* operator()(shared_variable* v)
	{
		xformer* x = NULL;

		if (v->is_row()) {
			x = new Xrow(v, conds_row, local_depths.find(v)->second);
		}
		else if (v->is_column()) {
			x = new Xcolumn(v, conds_column, local_depths.find(v)->second);
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

class define_leftover_full: public xformer {
	const string start;
	const string stop;
	const string buffer_size;
public:
	define_leftover_full(const string& t, const string& p, const string& b): start(t), stop(p), buffer_size(b) {}
	string operator()(const string& old)
	{
		return	old + 
			leftover.declare() + "= (" + stop + " - " + start + ") % " + buffer_size + ";" +
			full.declare() + "=" + stop + "-" + leftover.name() + ";";
	}

	xformer* clone() const { return new define_leftover_full(*this); }
	string class_name() const { return "define_leftover_full"; }
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
	virtual string next_buffer() const = 0;
	virtual string this_buffer() const = 0;
	virtual string first_buffer() const = 0;
	virtual string final_size() const = 0;
	virtual string final_buffer() const = 0;
	virtual string dma_in(const string& address) const = 0;
	virtual string dma_in(const string& address, const string& tsize) const = 0;
	virtual string dma_out(const string& address, const int depth) const = 0;
	virtual string dma_out(const string& address, const int depth, const string& tsize) const = 0;
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
		const string& factor = v->math().ihs(conds.induction).non_ihs(conds.induction).str();

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
		return "((" + conds.stop + "-" + conds.start + ")" + factor() + ")";
	}

	string next_buffer() const
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

	string this_buffer() const
	{
		if (v->is_flat()) {
			return v->math().ihs(conds.induction).str();
		}
		else {
			return v->math().str();
		}
	}

	string first_buffer() const
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

	string final_buffer() const
	{
		buffer_adaptor buff(v);

		string offset;
		if (!v->is_flat()) {
			//offset = conds.induction + "*" + v->dimensions().back() + "+";
			offset = v->math().non_ihs(conds.induction).str() + "+";
		}

		return offset + "(" + conds.stop + factor() + ")-" + final_size();
	}

	string dma_in(const string& address) const
	{
		return dma_in(address, buffer_adaptor(v).size());
	}

	string dma_in(const string& address, const string& tsize) const
	{
		buffer_adaptor buff(v);
		next_adaptor next(v);

		return "DMA_get(" + buff.name() + "+" + buff.size() + "*" + next.name() + "," +
				address + ","
				"sizeof(" + buff.type() + ") *" + tsize + "," +
				next.name() + ");";
	}

	string dma_out(const string& address, const int depth) const
	{
		return dma_out(address, depth, buffer_adaptor(v).size());
	}

	string dma_out(const string& address, const int depth, const string& tsize) const
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

	string next_buffer() const
	{
		return "(" + v->math().add_iteration(conds.induction, buffer_adaptor(v).size()) + ")";
	}

	string this_buffer() const
	{
		return "(" + v->math().add_iteration(conds.induction, "(1-" + buffer_adaptor(v).size() + ")") + ")";
	}

	string first_buffer() const
	{
		return v->math().zero_induction(conds.induction);
	}

	string final_size() const
	{
		return "((" + conds.stop + "-" + conds.start + ")%" + buffer_adaptor(v).size() + ")";
	}

	string final_buffer() const
	{
		return "(" + v->math().replace_induction(conds.induction, "(" + conds.stop + "-" + final_size() + "))");
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

	string dma_in(const string& address) const
	{
		return dma_in(address, stride());
	}

	string dma_in(const string& address, const string& tsize) const
	{
		dma_list_adaptor lst(v);
		buffer_adaptor buff(v);
		next_adaptor next(v);

		return	"add_to_dma_list(&" + lst.name(next.name()) + "," + 
				buff.size() + "," +
				address + ","
				"sizeof(" + buff.type() + "), " + 
				tsize + "sizeof(" + buff.type() + "),"
				"1);" +
			"DMA_getl(" + buff.name() + "+" + buff.size() + "*" + next.name() + "," +
				address + "," +
				"&" + lst.name(next.name()) + "," + 
				next.name() + ","
				"1," +
				"sizeof(" + buff.type() + "));";
	}

	string dma_out(const string& address, const int depth) const
	{
		return dma_out(address, depth, buffer_adaptor(v).size());
	}

	string dma_out(const string& address, const int depth, const string& tsize) const
	{
		dma_list_adaptor lst(v);
		buffer_adaptor buff(v);
		next_adaptor next(v);
		orig_adaptor orig(v);
		string make_list;
		const string& depth_s = to_string(depth);

		if (depth < 3) {
			make_list = "add_to_dma_list(&" + lst.name("(" + next.name() + "+(" + depth_s + "-1))%" + depth_s) + "," + 
					tsize + "," +
					address + ","
					"sizeof(" + buff.type() + "), " + 
					stride() + "sizeof(" + buff.type() + "),"
					"1);";
		}

		return 	make_list + 
			"DMA_putl(" + orig.name() + "," +
				address + "," +
				"&" + lst.name("(" + next.name() + "+(" + depth_s + "-1))%" + depth_s) + "," + 
				"(" + next.name() + "+(" + depth_s + "-1))%" + depth_s + ","
				"1," +
				"sizeof(" + buff.type() + "));";
	}
};

template <class Access>
class gen_in_first: public conditions_xformer, public Access {
public:
	gen_in_first(shared_variable* v, const conditions& c, const int d): conditions_xformer(v, c, d), Access(v, c) {}
	string operator()(const string& old)
	{
		return old + 
			"cellgen_dma_prep_start();" + 
			dma_in("(unsigned long)(" + v->name() + " + (" + Access::first_buffer() + "))") + 
			"cellgen_dma_prep_stop();";
	}

	xformer* clone() const { return new gen_in_first<Access>(*this); }
	string class_name() const { return "gen_in_first<" + Access::class_name() + ">"; }
};

template <class Access>
struct gen_in: virtual public conditions_xformer, virtual public leftover_xformer, public Access {
	gen_in(shared_variable* v, const conditions& c, const int d): conditions_xformer(v, c, d), Access(v, c) {}
	string operator()(const string& old)
	{
		next_adaptor next(v);
		buffer_adaptor buff(v);
		orig_adaptor orig(v);

		const string wait_next = "MMGP_SPE_dma_wait(" + next.name() + ", fn_id);";
		const string wait_prev = "MMGP_SPE_dma_wait(" + prev.name() + ", fn_id);";
		const string rotate_next = next.name() + "= (" + next.name() + "+1)%" + to_string(depth) + ";";

		const string& factor = v->math().ihs(conds.induction).non_ihs(conds.induction).str();
		string leftover_size;
		if (v->is_flat() && factor != "") {
			leftover_size = leftover.name() + "*" + factor;
		}
		else {
			leftover_size = leftover.name();
		}

		string in;
		if (is_leftover) {
			in = old +
				orig.name() + "=" + buff.name() + "+" + buff.size() + "*" + next.name() + ";" +
				wait_next;
		}
		else {
			in =  old +
				"cellgen_dma_prep_start();" + 
				prev.name() + "=" + next.name() + ";" +
				rotate_next + 
				wait_next +
				dma_in("(unsigned long)(" + v->name() + "+" + Access::next_buffer() + ")", 
						"(" + Access::bounds_check() + "<" + full.name() + "?" + buff.size() + ":" + leftover_size + ")" ) + 
				orig.name() + "=" + buff.name() + "+" + buff.size() + "*" + prev.name() + ";" +
				wait_prev +
				"cellgen_dma_prep_stop();";
		}

		return in;
	}

	xformer* clone() const { return new gen_in<Access>(*this); }
	string class_name() const { return "gen_in<" + Access::class_name() + ">"; }
};

template <class Access>
struct gen_out: virtual public conditions_xformer, virtual public leftover_xformer, public Access {
	gen_out(shared_variable* v, const conditions& c, const int d): conditions_xformer(v, c, d), Access(v, c) {}
	string operator()(const string& old)
	{
		buffer_adaptor buff(v);
		orig_adaptor orig(v);
		next_adaptor next(v);

		string var_switch;
		if (depth < 3) {
			var_switch = next.name() + "=(" + next.name() + "+1)%" + to_string(depth) + "; \n" +
					orig.name() + "=" + buff.name() + "+" + buff.size() + "*" + next.name() + "; \n";
		}

		const string wait = "MMGP_SPE_dma_wait(" + next.name() + ", fn_id);";

		string out;
		string dma;
		if (is_leftover) {
			dma = dma_out("(unsigned long)(" + v->name() + "+" + Access::final_buffer() + ")", depth, Access::final_size()); 
			out = dma + wait + old;
		}
		else {
			dma = dma_out("(unsigned long)(" + v->name() + "+" + Access::this_buffer() + ")", depth);
			out = "cellgen_dma_prep_start();" + dma + var_switch + wait + "cellgen_dma_prep_stop();" + old;
		}

		return out;
	}

	xformer* clone() const { return new gen_out<Access>(*this); }
	string class_name() const { return "gen_out<" + Access::class_name() + ">"; }
};

/*
 * This needs to become several functions.
 */
template <class Access>
struct gen_out_final: virtual public conditions_xformer, virtual public leftover_xformer, public Access {
	gen_out_final(shared_variable* v, const conditions& c, const int d): conditions_xformer(v, c, d), Access(v, c) {}
	string operator()(const string& old)
	{
		string ret;

		buffer_adaptor buff(v);
		orig_adaptor orig(v);
		next_adaptor next(v);
		const string if_statement = "if ((" + Access::final_iteration() + ")%" + buff.size() + ")";

		ret =	"cellgen_dma_prep_start(); " +
			if_statement + "{ " +
				prev.name() + "=" + next.name() + ";" +
				next.name() + "=(" + next.name() + "+1)%" + to_string(depth) + "; " +
				dma_out("(unsigned long)(" + v->name() + "+" + Access::final_buffer() + ")", depth, Access::final_size()) +
			"} \n"
			"MMGP_SPE_dma_wait(" + prev.name() + ", fn_id); " +
			"MMGP_SPE_dma_wait(" + next.name() + ", fn_id); " 
			"MMGP_SPE_dma_wait((" + next.name() + "+(" + to_string(depth) + "-1))%" + to_string(depth) + ", fn_id); "
			"cellgen_dma_prep_stop(); ";

		return old + ret;
	}

	xformer* clone() const { return new gen_out_final<Access>(*this); }
	string class_name() const { return "gen_out_final<" + Access::class_name() + ">"; }
};

#endif // XFORMERS_H
