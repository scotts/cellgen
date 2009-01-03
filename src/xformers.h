#ifndef XFORMERS_H
#define XFORMERS_H

#include <string>
#include <list>
using namespace std;

#include <boost/regex.hpp>

#include "variable.h"
#include "type_ops.h"

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

	virtual type_ops cost() 
	{
		type_ops ops;
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

class induction_xformer: virtual public xformer {
protected:
	string induction;
public:
	induction_xformer(const string& i): induction(i) {}
};

template <class X>
struct make_induction: public unary_function<const shared_variable*, xformer*> {
	const string& inductions;
	make_induction(const string& i): inductions(i) {}
	xformer* operator()(const shared_variable* v)
	{
		return new X(v, inductions);
	}
};

template <class X, class P>
struct append_induction_if: public unary_function<const shared_variable*, void> {
	xformerlist& lst;
	P pred;
	const string& induction;
	append_induction_if(xformerlist& l, P p, const string& i): lst(l), pred(p), induction(i) {}

	void operator()(const shared_variable* v)
	{
		if (pred(v)) {
			lst.push_back(new X(v, induction));
		}
	}
};

template <class X, class P>
append_induction_if<X, P> make_append_induction_if(xformerlist& lst, P pred, const string& induction)
{
	return append_induction_if<X, P>(lst, pred, induction);
}

const int NO_UNROLL = 0;

class unrollable_xformer: public induction_xformer {
protected:
	int unroll;
public:
	unrollable_xformer(const string& i): induction_xformer(i), unroll(NO_UNROLL) {}
	unrollable_xformer(const string& i, const int u): induction_xformer(i), unroll(u) {}
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

class row_buffer_space: virtual public xformer, public epilogue_xformer {
	const shared_variable* v;
	add_expr add;
public:
	row_buffer_space(const shared_variable* v, add_expr a): v(v), add(a) {}
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

		return old + orig.name() + "[(" + add.str() + ")%" + buff_size + "]";
	}

	xformer* clone() const { return new row_buffer_space(*this); }
	string class_name() const { return "row_buffer_space"; }
};

class column_buffer_space: public xformer {
	const shared_variable* v;
	add_expr add;
public:
	column_buffer_space(const shared_variable* v, add_expr a): v(v), add(a) {}
	string operator()(const string& old)
	{
		buffer_adaptor buff(v);
		orig_adaptor orig(v);

		return old + orig.name() + "[(" + add.lhs().lhs().str() + ")%" + buff.size() + "]";
	}

	xformer* clone() const { return new column_buffer_space(*this); }
	string class_name() const { return "column_buffer_space"; }
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
	const shared_variable* v;
	const int buffer;

public:
	shared_buffer_size(const shared_variable* v, const int b, const int u): unrollable_xformer("", u), v(v), buffer(b) {}
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

struct make_shared_buffer_size: public unary_function<const shared_variable*, xformer*> {
	const int buffer;
	const int unroll;
	make_shared_buffer_size(const int b, const int u): buffer(b), unroll(u) {}

	xformer* operator()(const shared_variable* v)
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

class gen_in_first: public unrollable_xformer, public epilogue_xformer {
protected:
	const shared_variable* v;
	string buff_size;

public:
	gen_in_first(const shared_variable* _v, const string& i): unrollable_xformer(i), v(_v) {}
	string operator()(const string& old)
	{
		next_adaptor next(v);
		buffer_adaptor buff(v);
		orig_adaptor orig(v);

		set_buff_size();

		return old + 
			"cellgen_dma_prep_start(); \n" + 
			dma_in() + 
			"cellgen_dma_prep_stop(); \n";
	}

	virtual void set_buff_size()
	{
		buff_size = buffer_adaptor(v).size();
	}

	virtual string dma_in() = 0;
};

class gen_in_first_row: public gen_in_first {
public:
	gen_in_first_row(const shared_variable* v, const string& i): gen_in_first(v, i) {}

	void set_buff_size()
	{
		buffer_adaptor buff(v);

		if (epilogue) {
			buff_size = "(" + buff.size() + "/" + "unroll_factor)";
		}
		else {
			buff_size = buff.size();
		}
	}

	string dma_in()
	{
		buffer_adaptor buff(v);
		next_adaptor next(v);
		orig_adaptor orig(v);
		string address;
		string buff_size;

		if (epilogue) {
			buff_size = "(" + buff.size() + "/" + "unroll_factor)";
		}
		else {
			buff_size = buff.size();
		}

		if (unroll) {
			address = "(unsigned long)(" + v->name() + "+ (" + unrolled.name() + v->math().factor(induction) + "))";
		}
		else {
			address = "(unsigned long)(" + v->name() + " + (SPE_start" + v->math().factor(induction) + "))";
		}

		return	prev.name() + "=" + next.name() + "; \n" +
			"DMA_get(" +
				buff.name() + "+" + buff_size + "*" + next.name() + ", " +
				address + "," 
				"sizeof(" + buff.type() + ")*" + buff_size + ", " +
				next.name() + "); \n" +
			orig.name() + "=" + buff.name() + "+" + buff_size + "*" + prev.name() + "; \n";
	}

	xformer* clone() const { return new gen_in_first_row(*this); }
	string class_name() const { return "gen_in_first_row"; }
};

class gen_in_first_column: public gen_in_first {
public:
	gen_in_first_column(const shared_variable* v, const string& i): gen_in_first(v, i) {}
	string dma_in()
	{
		dma_list_adaptor list(v);
		buffer_adaptor buff(v);
		next_adaptor next(v);
		orig_adaptor orig(v);

		return	"add_to_dma_list(&" + list.name(next.name()) + "," + 
				buff.size() + ","
				"(unsigned long)(" + v->name() + "+" + induction + "),"
				"sizeof(" + buff.type() + "), " + 
				v->dimensions().back() + "* sizeof(" + buff.type() + "),"
				"1); \n" +
			"DMA_getl(" + buff.name() + "+" + buff.size() + "*" + next.name() + ","
				"(unsigned long)(" + v->name() + "+" + induction + "),"
				"&" + list.name(next.name()) + "," + 
				next.name() + ","
				"1," +
				"sizeof(" + buff.type() + ")); \n" +
			orig.name() + "=" + buff.name() + "; \n";
	}

	xformer* clone() const { return new gen_in_first_column(*this); }
	string class_name() const { return "gen_in_first_column"; }
};

struct make_gen_in_first: public unary_function<shared_variable*, xformer*>  {
	const string& induction;
	make_gen_in_first(const string& i): induction(i) {}

	xformer* operator()(shared_variable* v)
	{
		xformer* x = NULL;

		if (v->is_row()) {
			x = new gen_in_first_row(v, induction);
		}
		else if (v->is_column()) {
			x = new gen_in_first_column(v, induction);
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

struct gen_in: public unrollable_xformer, public epilogue_xformer {
	shared_variable* v;
	string buff_size;
	gen_in(shared_variable* v, const string& i): unrollable_xformer(i), v(v) {}
	string operator()(const string& old)
	{
		next_adaptor next(v);
		buffer_adaptor buff(v);
		orig_adaptor orig(v);

		const string wait_next = "MMGP_SPE_dma_wait(" + next.name() + ", fn_id); \n";
		const string wait_prev = "MMGP_SPE_dma_wait(" + prev.name() + ", fn_id); \n";
		const string rotate_next = next.name() + "= (" + next.name() + "+1)%" + buff.depth() + "; \n";

		set_buff_size();

		return old + 
			"cellgen_dma_prep_start(); \n" + 
			outer_if() + "{ \n" +
				prev.name() + "=" + next.name() + "; \n" +
				rotate_next + 
				wait_next +
				inner_if() + "{ \n" + 
					dma_in() + 
				"} \n" +
				orig.name() + "=" + buff.name() + "+" + buff_size + "*" + prev.name() + "; \n" +
				wait_prev +
			"} \n"
			"cellgen_dma_prep_stop(); \n";
	}

	virtual void set_buff_size()
	{
		buff_size = buffer_adaptor(v).size();	
	}

	virtual string address() = 0;
	virtual string outer_if() = 0;
	virtual string inner_if() = 0;
	virtual string dma_in() = 0;
};

struct gen_in_row: public gen_in {
	gen_in_row(shared_variable* v, const string& i): gen_in(v, i) {}

	void set_buff_size()
	{
		buffer_adaptor buff(v);

		if (epilogue) {
			buff_size = "(" + buff.size() + "/ unroll_factor)";
		}
		else {
			buff_size = buff.size();
		}
	}

	string outer_if()
	{
		if (!unroll) {
			buffer_adaptor buff(v);

			return "if (!((" + v->math().lhs().str() + ")%" + buff_size + "))"; 
		}
		else {
			return string();
		}
	}

	string inner_if()
	{
		if (!unroll) {
			return "if (" + induction + "+1 < SPE_stop)";
		}
		else {
			return string();
		}
	}

	string address()
	{
		buffer_adaptor buff(v);

		return "(unsigned long)(" + v->name() + "+((" + v->math().lhs().str() + ")+" + buff_size + "))";
	}

	string dma_in()
	{
		buffer_adaptor buff(v);
		next_adaptor next(v);

		return "DMA_get(" + buff.name() + "+" + buff_size + "*" + next.name() + "," +
				address() + ","
				"sizeof(" + buff.type() + ") *" + buff_size + "," +
				next.name() + ");\n";
	}

	xformer* clone() const { return new gen_in_row(*this); }
	string class_name() const { return "gen_in_row"; }
};

struct gen_in_column: public gen_in {
	gen_in_column(shared_variable* v, const string& i): gen_in(v, i) {}

	string outer_if()
	{
		if (!unroll) {
			buffer_adaptor buff(v);

			return "if (!((" + v->math().lhs().lhs().str() + ")%" + buff.size() + "))"; 
		}
		else {
			return string();
		}
	}

	string inner_if()
	{
		if (!unroll) {
			buffer_adaptor buff(v);

			return "if (" + v->math().lhs().lhs().str() + "+" + buff.size() + "<" + v->dimensions().front() + ")";
		}
		else {
			return string();
		}
	}

	string address()
	{
		buffer_adaptor buff(v);

		return "(unsigned long)(" + v->name() + "+(" + v->math().lhs().lhs().str() + "+" + buff.size() + ")*" + v->dimensions().back() + "+" + induction + ")";
	}

	string dma_in()
	{
		dma_list_adaptor list(v);
		buffer_adaptor buff(v);
		next_adaptor next(v);

		return	"add_to_dma_list(&" + list.name(next.name()) + "," + 
				buff.size() + "," +
				address() + ","
				"sizeof(" + buff.type() + "), " + 
				v->dimensions().back() + "* sizeof(" + buff.type() + "),"
				"1); \n" +
			"DMA_getl(" + buff.name() + "+" + buff.size() + "*" + next.name() + "," +
				address() + "," +
				"&" + list.name(next.name()) + "," + 
				next.name() + ","
				"1," +
				"sizeof(" + buff.type() + ")); \n";
	}

	xformer* clone() const { return new gen_in_column(*this); }
	string class_name() const { return "gen_in_column"; }
};

struct gen_out: public unrollable_xformer, public epilogue_xformer {
	const shared_variable* v;
	string buff_size;
	gen_out(const shared_variable* v, const string& i): unrollable_xformer(i), v(v) {}
	string operator()(const string& old)
	{
		buffer_adaptor buff(v);
		orig_adaptor orig(v);
		next_adaptor next(v);
		string var_switch;

		set_buff_size();

		if (v->depth() < 3) {
			var_switch =	next.name() + "=(" + next.name() + "+1)%" + buff.depth() + "; \n" +
					orig.name() + "=" + buff.name() + "+" + buff_size + "*" + next.name() + "; \n" +
					"MMGP_SPE_dma_wait(" + next.name() + ", fn_id);";
		}

		return "cellgen_dma_prep_start(); \n" +
			if_statement() + "{ \n" +
					dma_out() +
					var_switch +
			"} \n" 
			"cellgen_dma_prep_stop(); \n" +
			old;
	}

	virtual void set_buff_size()
	{
		buff_size = buffer_adaptor(v).size();
	}

	virtual string if_statement() = 0;
	virtual string address() = 0;
	virtual string dma_out() = 0;
};

struct gen_out_row: public gen_out {
	gen_out_row(const shared_variable* v, const string& i): gen_out(v, i) {}

	void set_buff_size()
	{
		buffer_adaptor buff(v);

		if (epilogue) {
			buff_size = "(" + buff.size() + "/ unroll_factor)";
		}
		else {
			buff_size = buff.size();
		}
	}

	string if_statement()
	{
		if (!unroll) {
			buffer_adaptor buff(v);
			return "if (!(" + v->math().next_iteration(induction) + "%" + buff_size + "))";
		}
		else {
			return string();
		}
	}

	string address()
	{
		buffer_adaptor buff(v);
		return "(unsigned long)(" + v->name() + "+" + v->math().next_iteration(induction) + "-" + buff_size + ")";
	}

	string dma_out()
	{
		buffer_adaptor buff(v);
		orig_adaptor orig(v);
		next_adaptor next(v);

		return "DMA_put(" + orig.name() + "," + 
				address() + "," +
				"sizeof(" + buff.type() + ")*" + buff_size + "," +
				next.name() + "); \n";
	}

	xformer* clone() const { return new gen_out_row(*this); }
	string class_name() const { return "gen_out_row"; }
};

struct gen_out_column: public gen_out {
	gen_out_column(const shared_variable* v, const string& i): gen_out(v, i) {}

	string if_statement()
	{
		if (!unroll) {
			buffer_adaptor buff(v);
			return "if (!((" + v->math().lhs().lhs().str() + "+1)%" + buff.size() + "))";
		}
		else {
			return string();
		}
	}

	string address()
	{
		buffer_adaptor buff(v);
		return "(unsigned long)(" + v->name() + "+(" + v->math().lhs().lhs().str() + "+1-" + buff.size() + ")*" + v->dimensions().back() + "+" + induction + ")";
	}

	string dma_out()
	{
		dma_list_adaptor list(v);
		buffer_adaptor buff(v);
		next_adaptor next(v);
		orig_adaptor orig(v);
		string make_list;
		const string depth = to_string(v->depth());

		if (v->depth() < 3) {
			make_list = "add_to_dma_list(&" + list.name("(" + next.name() + "+(" + depth + "-1))%" + depth) + "," + 
					buff.size() + "," +
					address() + ","
					"sizeof(" + buff.type() + "), " + 
					v->dimensions().back() + "* sizeof(" + buff.type() + "),"
					"1); \n";
		}

		return 	make_list + 
			"DMA_putl(" + orig.name() + "," +
				address() + "," +
				"&" + list.name("(" + next.name() + "+(" + depth + "-1))%" + depth) + "," + 
				"(" + next.name() + "+(" + depth + "-1))%" + depth + ","
				"1," +
				"sizeof(" + buff.type() + ")); \n";
	}

	xformer* clone() const { return new gen_out_column(*this); }
	string class_name() const { return "gen_out_column"; }
};

struct gen_out_final: public unrollable_xformer, public epilogue_xformer {
	const shared_variable* v;
	string buff_size;
	gen_out_final(const shared_variable* v, const string& i): unrollable_xformer(i), v(v) {}
	string operator()(const string& old)
	{
		string ret;

		if (!unroll) {
			buffer_adaptor buff(v);
			orig_adaptor orig(v);
			next_adaptor next(v);

			if (epilogue) {
				buff_size = "(" + buff.size() + "/ unroll_factor)";
			}
			else {
				buff_size = buff.size();
			}

			ret =	"cellgen_dma_prep_start(); \n" +
				if_statement() + "{ \n" +
					prev.name() + "=" + next.name() + ";" +
					next.name() + "=(" + next.name() + "+1)%" + buff.depth() + "; \n" +
					dma_final_out() +
				"} \n"
				"MMGP_SPE_dma_wait(" + prev.name() + ", fn_id); \n" +
				"MMGP_SPE_dma_wait(" + next.name() + ", fn_id); \n" 
				"MMGP_SPE_dma_wait((" + next.name() + "+(" + to_string(v->depth()) + "-1))%" + to_string(v->depth()) + ", fn_id); \n"
				"cellgen_dma_prep_stop(); \n";
		}

		return old + ret;
	}

	virtual string if_statement() = 0;
	virtual string address() = 0;
	virtual string dma_final_out() = 0;
};

struct gen_out_final_row: public gen_out_final {
	gen_out_final_row(const shared_variable* v, const string& i): gen_out_final(v, i) {}

	string if_statement()
	{
		buffer_adaptor buff(v);
		return "if ((((SPE_stop - SPE_start)" + v->math().factor(induction) + ")%" + buff_size + "))";
	}

	string address()
	{
		buffer_adaptor buff(v);
		const string factor = v->math().factor(induction);
		return "(unsigned long)(" + v->name() + "+(SPE_stop" + factor + ")-(((SPE_stop-SPE_start)" + factor + ")%" + buff_size + "))";
	}

	string dma_final_out()
	{
		buffer_adaptor buff(v);
		orig_adaptor orig(v);
		next_adaptor next(v);

		return "DMA_put(" + orig.name() + "," +
				address() + ","
				"sizeof(" + buff.type() + ")*(((SPE_stop-SPE_start)" + v->math().factor(induction) + ")%" + buff_size + ")," +
				next.name() + "); \n";
	}

	xformer* clone() const { return new gen_out_final_row(*this); }
	string class_name() const { return "gen_out_final_row"; }
};

struct gen_out_final_column: public gen_out_final {
	gen_out_final_column(const shared_variable* v, const string& i): gen_out_final(v, i) {}

	string if_statement()
	{
		buffer_adaptor buff(v);
		return "if (" + v->dimensions().front() + "%" + buff.size() + ")";
	}

	string address()
	{
		buffer_adaptor buff(v);
		const string dim1 = v->dimensions().front();
		const string dim2 = v->dimensions().back();

		return "(unsigned long)(" + v->name() + "+((" + dim1 + "-(" + dim1 + "%" + buff.size() + "))*" + dim2 + ")+" + induction + ")";
	}

	string dma_final_out()
	{
		dma_list_adaptor list(v);
		buffer_adaptor buff(v);
		next_adaptor next(v);
		orig_adaptor orig(v);
		string make_list;

		return "add_to_dma_list(&" + list.name(next.name()) + ", " + 
				buff.size() + "," + 
				address() + ","
				"sizeof(" + buff.type() + "), " + 
				v->dimensions().front() + "* sizeof(" + buff.type() + "),"
				"1); \n" +
			"DMA_putl(" + orig.name() + "," +
				address() + ","
				"&" + list.name("(" + next.name() + "+(" + to_string(v->depth()) + "-1))%" + to_string(v->depth())) + "," + 
				"(" + next.name() + "+(" + to_string(v->depth()) + "-1))%" + to_string(v->depth()) + ","
				"1," +
				"sizeof(" + buff.type() + ")); \n";
	}

	xformer* clone() const { return new gen_out_final_column(*this); }
	string class_name() const { return "gen_out_final_column"; }
};

#endif // XFORMERS_H
