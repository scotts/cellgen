#ifndef OPERATIONS_H
#define OPERATIONS_H

#include <iostream>
#include <map>
using namespace std;

const int dma_startup_cost = 16; // cycles
const double dma_bandwidth_s = 1; // GBytes/s
const double spe_frequency = 3.192; // GHz
const double dma_bandwidth_cycles = dma_bandwidth_s / spe_frequency;

enum c_type {CHAR, INT, LONG, FLOAT, DOUBLE, UNKNOWN_VAR};
enum op_type {ADD, SUB, MUL, DIV, MOD, LOAD, STORE, UNKNOWN_OP};

/* WTF, right? If I want to "construct" something, why not just 
 * make it a class and use named constants? Because then I 
 * couldn't use it in switch statements, which was the whole point
 * of make it a type and not just a string.
 *
 * Day later edit: I could probably allow explicit conversions to 
 * ints, but really, is that better?
 */
op_type construct_op_type(const string& op);
c_type construct_c_type(const string& op);
bool c_type_less(const c_type a, const c_type b);
bool c_type_greater(const c_type a, const c_type b);

class latency_estimator {
	map<c_type, map<op_type, int> > latency;

public:
	latency_estimator();
	int cycles(const int n, const op_type op, const c_type var) const;
};
int estimate_buffer_size(const int iteration, const int startup);

const latency_estimator estimator;

template <c_type type>
struct operation_counts {
	int add;
	int sub;
	int mul;
	int div;
	int mod;
	int load;
	int store;

	operation_counts():
		add(0), sub(0), mul(0), div(0), mod(0), load(0), store(0)
		{}
	operation_counts(const operation_counts& o):
		add(o.add), sub(o.sub), mul(o.mul), div(o.div), mod(o.mod), load(o.load), store(o.store)
		{}

	template <c_type t>
	friend operation_counts<t> operator+(const operation_counts<t>&, const operation_counts<t>&);	

	template <c_type t, class T>
	friend operation_counts<t> operator*(const T, const operation_counts<t>&);	

	template <c_type t, class T>
	friend operation_counts<t> operator*(const operation_counts<t>&, const T);	

	void operator=(const operation_counts<type>& o);
	void operator+=(const operation_counts<type>& o);

	int comp_cycles();
	int data_cycles();
};

template <c_type type, class T>
operation_counts<type> operator*(const T a, const operation_counts<type>& b)
{
	operation_counts<type> o = b;
	o.add *= a;
	o.sub *= a;
	o.mul *= a;
	o.div *= a;
	o.mod *= a;
	o.load *= a;
	o.store *= a;

	return o;
}

template <c_type type, class T>
operation_counts<type> operator*(const operation_counts<type>& a, const T b)
{
	operation_counts<type> o = a;
	o.add *= b;
	o.sub *= b;
	o.mul *= b;
	o.div *= b;
	o.mod *= b;
	o.load *= b;
	o.store *= b;

	return o;
}

template <c_type type>
void operation_counts<type>::operator=(const operation_counts<type>& o)
{
	add = o.add;
	sub = o.sub;
	mul = o.mul;
	div = o.div;
	mod = o.mod;
	load = o.load;
	store = o.store;
}

template <c_type type>
void operation_counts<type>::operator+=(const operation_counts<type>& o)
{
	add += o.add;
	sub += o.sub;
	mul += o.mul;
	div += o.div;
	mod += o.mod;
	load += o.load;
	store += o.store;
}

template <c_type type>
operation_counts<type> operator+(const operation_counts<type>& a, const operation_counts<type>& b)
{
	operation_counts<type> o;
	o.add = a.add + b.add;
	o.sub = a.sub + b.sub;
	o.mul = a.mul + b.mul;
	o.div = a.div + b.div;
	o.mod = a.mod + b.mod;
	o.load = a.load + b.load;
	o.store = a.store + b.store;
	return o;
}

template <c_type type>
int operation_counts<type>::comp_cycles()
{
	int comp = 0;
	comp += estimator.cycles(add, ADD, type);
	comp += estimator.cycles(sub, SUB, type);
	comp += estimator.cycles(mul, MUL, type);
	comp += estimator.cycles(div, DIV, type);
	comp += estimator.cycles(mod, MOD, type);

	return comp;
}

template <c_type type>
int operation_counts<type>::data_cycles()
{
	int data = 0;
	data += estimator.cycles(load, LOAD, type);
	data += estimator.cycles(store, STORE, type);

	return data;
}

class unknown_c_type {};
class unsupported_c_type {};
class unknown_op_type {};

class operations {
	operation_counts<CHAR> char_ops;
	operation_counts<INT> int_ops;
	operation_counts<FLOAT> float_ops;
	operation_counts<DOUBLE> double_ops;

public:
	operations()
		{}
	operations(const operations& o):
		char_ops(o.char_ops), int_ops(o.int_ops), float_ops(o.float_ops), double_ops(o.double_ops)
		{}

	friend operations operator+(const operations&, const operations&);

	template <class T>
	friend operations operator*(const T, const operations&);

	template <class T>
	friend operations operator*(const operations&, const T);

	friend ostream& operator<<(ostream& out, const operations& ops);
		
	void operator=(const operations& o);
	void operator+=(const operations& o);

	void add(const c_type type, const int n);
	void sub(const c_type type, const int n);
	void mul(const c_type type, const int n);
	void div(const c_type type, const int n);
	void mod(const c_type type, const int n);
	void load(const c_type type, const int n);
	void store(const c_type type, const int n);

	void inc(const op_type& op, const c_type type);

	void inc_add(const c_type type) { add(type, 1); }
	void inc_sub(const c_type type) { sub(type, 1); }
	void inc_mul(const c_type type) { mul(type, 1); }
	void inc_div(const c_type type) { div(type, 1); }
	void inc_mod(const c_type type) { mod(type, 1); }
	void inc_load(const c_type type) { load(type, 1); }
	void inc_store(const c_type type) { store(type, 1); }

	int comp_cycles()
	{
		return char_ops.comp_cycles() + int_ops.comp_cycles() + 
			float_ops.comp_cycles() + double_ops.comp_cycles();
	}

	int data_cycles()
	{
		return char_ops.data_cycles() + int_ops.data_cycles() + 
			float_ops.data_cycles() + double_ops.data_cycles();
	}

	int cycles()
	{ 
		return max(comp_cycles(), data_cycles());
	}
};

operations operator+(const operations& a, const operations& b);

template <class T>
operations operator*(const T a, const operations& b)
{
	operations o = b;
	o.char_ops = o.char_ops * a;
	o.int_ops = o.int_ops * a;
	o.float_ops = o.float_ops * a;
	o.double_ops = o.double_ops * a;
	return o;
}

template <class T>
operations operator*(const operations& a, const T b)
{
	operations o = a;
	o.char_ops = o.char_ops * b;
	o.int_ops = o.int_ops * b;
	o.float_ops = o.float_ops * b;
	o.double_ops = o.double_ops * b;
	return o;
}

template <c_type type>
ostream& operator<<(ostream& out, const operation_counts<type>& op)
{
	out	<< type << " ("	
		<< "add " << op.add << ", "
		<< "sub " << op.sub << ", "
		<< "mul " << op.mul << ", "
		<< "div " << op.div << ", "
		<< "mod " << op.mod << ", "
		<< "load " << op.load << ", "
		<< "store " << op.store
		<< ")";

	return out;
}

ostream& operator<<(ostream& out, const c_type& type);
ostream& operator<<(ostream& out, const operations& ops);

#endif // OPERATIONS_H

