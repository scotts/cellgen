#ifndef XFORMERS_H
#define XFORMERS_H

#include <string>
using namespace std;

#include "variable.h"

struct xformer: public unary_function<const string&, string> {
	virtual ~xformer() {}
	virtual void unroll_me(int u) {}
	virtual string operator()(const string& old) = 0;
	virtual xformer* clone() const = 0;
	virtual string class_name() const = 0; // For debugging purposes only.
};

template <class X, class V>
struct make_xformer: public unary_function<const V*, xformer*> {
	xformer* operator()(const V* v)
	{
		return new X(v);
	}
};

class induction_xformer: public xformer {
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

const int NO_UNROLL = 0;

class unrollable_xformer: public induction_xformer {
protected:
	int unroll;
public:
	unrollable_xformer(const string& i): induction_xformer(i), unroll(NO_UNROLL) {}
	virtual void unroll_me(int u)
	{
		// TODO: Do I need to know induction information to determine if 
		// unrolling needs to happen? I used to think so. Now I'm not sure.
		unroll = u;
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

class to_row_space: public xformer {
	const shared_variable* v;
	add_expr add;
public:
	to_row_space(const shared_variable* v, add_expr a): v(v), add(a) {}
	string operator()(const string& old)
	{
		return old + 
			orig_adaptor(v).name() + 
			"[(" + add.str() + ")%" + buffer_adaptor(v).size() + "]";
	}

	xformer* clone() const { return new to_row_space(*this); }
	string class_name() const { return "to_row_space"; }
};

class to_column_space: public xformer {
	const shared_variable* v;
	add_expr add;
public:
	to_column_space(const shared_variable* v, add_expr a): v(v), add(a) {}
	string operator()(const string& old)
	{
		assert(false);
		return string();
	}

	xformer* clone() const { return new to_column_space(*this); }
	string class_name() const { return "to_column_space"; }
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
	shared_buffer_size(const shared_variable* v, const int b): unrollable_xformer(""), v(v), buffer(b) {}
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
				def = "(" + to_string(unroll) + v->math().factor(induction) + ")";
			}
			else {
				def = default_buff_size;
			}
			declaration = "const int " + buffer_adaptor(v).size() + "=" + def + "; \n";
		}

		return old + declaration;
	}

	xformer* clone() const { return new shared_buffer_size(*this); }
	string class_name() const { return "shared_buffer_size"; }
};

struct make_shared_buffer_size: public unary_function<const shared_variable*, xformer*> {
	const int buffer;
	make_shared_buffer_size(const int b): buffer(b) {}

	xformer* operator()(const shared_variable* v)
	{
		return new shared_buffer_size(v, buffer);
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

class buffer_malloc: public xformer {
	const shared_variable* v;

public:
	buffer_malloc(const shared_variable* v): v(v) {}
	string operator()(const string& old)
	{
		buffer_adaptor buff(v);
		return old + buff.declare() + "= _malloc_align(sizeof(" + buff.type() + ")*" + buff.depth() + "*" + buff.size() + ",7);";
	}

	xformer* clone() const { return new buffer_malloc(*this); }
	string class_name() const { return "buffer_malloc"; }
};

class buffer_free: public xformer {
	const shared_variable* v;

public:
	buffer_free(const shared_variable* v): v(v) {}
	string operator()(const string& old)
	{
		return "_free_align(" + buffer_adaptor(v).name() + ");" + old;
	}

	xformer* clone() const { return new buffer_free(*this); }
	string class_name() const { return "buffer_free"; }
};

class init_buffers: public xformer {
	const shared_variable* v;

public:
	init_buffers(const shared_variable* _v): v(_v) {}
	string operator()(const string& old)
	{
		buffer_adaptor buff(v);
		next_adaptor next(v);
		orig_adaptor orig(v);

		return	old + 
			orig.declare() + "; \n" +
			next.declare() + " = 0; \n" +
			orig.name() + " = " + buff.name() + "; \n";
	}

	xformer* clone() const { return new init_buffers(*this); }
	string class_name() const { return "init_buffers"; }
};
	
class in_init_buffers: public induction_xformer {
	const shared_variable* v;

public:
	in_init_buffers(const shared_variable* _v, const string& i): induction_xformer(i), v(_v) {}
	string operator()(const string& old)
	{
		buffer_adaptor buff(v);
		next_adaptor next(v);

		return	init_buffers(v)(old) +
			"mfc_get(" +
				buff.name() + "+" + buff.size() + "*" + next.name() + ", " 
				"(unsigned long)(" + v->name() + " + (SPE_start" + v->math().factor(induction) + ")),"
				"sizeof(" + buff.type() + ")*" + buff.size() + ", " +
				next.name() + ", 0, 0); \n";
	}

	xformer* clone() const { return new in_init_buffers(*this); }
	string class_name() const { return "in_init_buffers"; }
};

class init_private_buffers: public xformer {
	const private_variable* v;

public:
	init_private_buffers(const private_variable* v): v(v) {}
	string operator()(const string& old)
	{
		string ret;
		if (v->is_non_scalar()) {
			buffer_adaptor buff(v);
			orig_adaptor orig(v);

			ret =	"mfc_get(" +
					buff.name() + "," +
					"(unsigned long)" + orig.name() + "," +
					"sizeof(" + buff.type() + ")*" + buff.size() + ","
					"3, 0, 0); \n" + 
				orig.name() + "=" + buff.name() + ";"
				"MMGP_SPE_dma_wait(3, fn_id); \n";
		}

		return old + ret;
	}

	xformer* clone() const { return new init_private_buffers(*this); }
	string class_name() const { return "init_private_buffers"; }
};

class unroll_boundaries: public xformer {
	const int unroll;
public:
	unroll_boundaries(const int u): unroll(u) {}
	string operator()(const string& old)
	{
		stringstream ss;
		ss << unroll;

		return 	old + 
			const_variable("int", "unroll_factor", ss.str()).define() + ";" +
			unrolled.define() + ";" + 
			epilouge.define() + ";";
	}

	xformer* clone() const { return new unroll_boundaries(*this); }
	string class_name() const { return "unroll_boundaries"; }
};

struct declare_prev: public xformer {
	string operator()(const string& old)
	{
		return old + prev.define() + ";";
	}

	xformer* clone() const { return new declare_prev(*this); }
	string class_name() const { return "declare_prev"; }
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

struct gen_in: public unrollable_xformer {
	shared_variable* v;
	gen_in(shared_variable* v, const string& i): unrollable_xformer(i), v(v) {}
};

struct gen_in_row: public gen_in {
	gen_in_row(shared_variable* v, const string& i): gen_in(v, i) {}
	string operator()(const string& old)
	{
		next_adaptor next(v);
		buffer_adaptor buff(v);
		orig_adaptor orig(v);

		string outer_if;
		string inner_if;
		string wait;

		if (!unroll) {
			outer_if = "if (!((" + v->math().lhs().str() + ")%" + buff.size() + "))"; 
			inner_if = "if (" + induction + "+1 < SPE_stop)";
		}

		if (v->depth() == 3) {
			wait = "MMGP_SPE_dma_wait(" + next.name() + ", fn_id);";
		}

		return old + 
			"cellgen_dma_prep_start(); \n" + 
			outer_if + "{ \n" +
				prev.name() + "=" + next.name() + "; \n" +
				next.name() + "= (" + next.name() + "+1)%" + buff.depth() + "; \n" + 
				wait +
				inner_if + "{ \n"
					"mfc_get(" + 
						buff.name() + "+" + buff.size() + "*" + next.name() + "," 
						"(unsigned long)(" + v->name() + "+((" + v->math().lhs().str() + ")+" + buff.size() + "))," 
						"sizeof(" + buff.type() + ") *" + buff.size() + "," +
						next.name() + ", 0, 0);\n"
				"} \n" +
				orig.name() + "=" + buff.name() + "+" + buff.size() + "*" + prev.name() + "; \n"
				"MMGP_SPE_dma_wait(" + prev.name() + ", fn_id); \n"
			"} \n"
			"cellgen_dma_prep_stop(); \n";
	}

	xformer* clone() const { return new gen_in_row(*this); }
	string class_name() const { return "gen_in_row"; }
};

struct gen_in_column: public gen_in {
	gen_in_column(shared_variable* v, const string& i): gen_in(v, i) {}

	string operator()(const string& old)
	{
		assert(false);
		return string();
	}

	xformer* clone() const { return new gen_in_column(*this); }
	string class_name() const { return "gen_in_column"; }
};

struct gen_out: public unrollable_xformer {
	const shared_variable* v;
	gen_out(const shared_variable* v, const string& i): unrollable_xformer(i), v(v) {}
};

struct gen_out_row: public gen_out {
	gen_out_row(const shared_variable* v, const string& i): gen_out(v, i) {}
	string operator()(const string& old)
	{
		buffer_adaptor buff(v);
		orig_adaptor orig(v);
		next_adaptor next(v);
		string var_switch;
		string if_statement;

		if (v->depth() < 3) {
			var_switch =	next.name() + "=(" + next.name() + "+1)%" + buff.depth() + "; \n" +
					orig.name() + "=" + buff.name() + "+" + buff.size() + "*" + next.name() + "; \n" +
					"MMGP_SPE_dma_wait(" + next.name() + ", fn_id);";
		}

		if (!unroll) {
			if_statement = "if (!(" + v->math().next_iteration(induction) + "%" + buff.size() + "))";
		}

		return "cellgen_dma_prep_start(); \n" +
			if_statement + "{\n" +
					"mfc_put(" + orig.name() + "," + "(unsigned long)(" + v->name() + 
							"+" + v->math().next_iteration(induction) + "-" + buff.size() + "),"
						"sizeof(" + buff.type() + ")*" + buff.size() + "," +
						next.name() + ", 0, 0); \n" +
					var_switch +
			"} \n" 
			"cellgen_dma_prep_stop(); \n" +
			old;
	}

	xformer* clone() const { return new gen_out_row(*this); }
	string class_name() const { return "gen_out_row"; }
};

struct gen_out_column: public gen_out {
	gen_out_column(const shared_variable* v, const string& i): gen_out(v, i) {}
	string operator()(const string& old)
	{
		assert(false);
		return string();
	}

	xformer* clone() const { return new gen_out_column(*this); }
	string class_name() const { return "gen_out_column"; }
};

struct gen_final_out: public unrollable_xformer {
	const shared_variable* v;
	gen_final_out(const shared_variable* v, const string& i): unrollable_xformer(i), v(v) {}
};

struct gen_final_out_row: public gen_final_out {
	gen_final_out_row(const shared_variable* v, const string& i): gen_final_out(v, i) {}
	string operator()(const string& old)
	{
		string ret;

		if (!unroll) {
			buffer_adaptor buff(v);
			orig_adaptor orig(v);
			next_adaptor next(v);

			ret =	"cellgen_dma_prep_start(); \n" 
				"if ((((SPE_stop - SPE_start)" + v->math().factor(induction) + ")%" + buff.size() + ")) {" +
					prev.name() + "=" + next.name() + ";" +
					next.name() + "=(" + next.name() + "+1)%" + buff.depth() + "; \n" +
					"mfc_put(" + orig.name() + "," 
						"(unsigned long)(" + v->name() +
							"+(SPE_stop" + v->math().factor(induction) + ")-(((SPE_stop-SPE_start)" + 
							v->math().factor(induction) + ")%" + 
							buff.size() + ")),"
						"sizeof(" + buff.type() + ")*(((SPE_stop-SPE_start)" + v->math().factor(induction) +
						")%" + buff.size() + ")," +
						next.name() + ", 0, 0"
					"); \n" +
					"MMGP_SPE_dma_wait(" + prev.name() + ", fn_id); \n" +
					"MMGP_SPE_dma_wait(" + next.name() + ", fn_id); \n" 
				"} \n"
				"cellgen_dma_prep_stop(); \n";
		}

		return old + ret;
	}

	xformer* clone() const { return new gen_final_out_row(*this); }
	string class_name() const { return "gen_final_out_row"; }
};

struct gen_final_out_column: public gen_final_out {
	gen_final_out_column(const shared_variable* v, const string& i): gen_final_out(v, i) {}
	string operator()(const string& old)
	{
		assert(false);
		return string();
	}

	xformer* clone() const { return new gen_final_out_column(*this); }
	string class_name() const { return "gen_final_out_column"; }
};

#endif // XFORMERS_H
