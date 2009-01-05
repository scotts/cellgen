#ifndef OPERATIONS_H
#define OPERATIONS_H

#include <iostream>
#include <map>
using namespace std;

const int dma_startup_cost = 16; // cycles
const double dma_bandwidth_s = 16.8; // GBytes/s
const double spe_frequency = 2.1; // GHz
const double dma_bandwidth_cycles = dma_bandwidth_s / spe_frequency;

enum variable_type {CHAR, INT, LONG, FLOAT, DOUBLE, UNKNOWN_VAR};
enum op_type {ADD, SUB, MUL, DIV, MOD, UNKNOWN_OP};

/* WTF, right? If I want to "construct" something, why not just 
 * make it a class and use named constants? Because then I 
 * couldn't use it in switch statements, which was the whole point
 * of make it a type and not just a string.
 *
 * Day later edit: I could probably allow explicit conversions to 
 * ints, but really, is that better?
 */
op_type construct_op_type(const string& op);

template <variable_type type>
struct operation_counts {
	int add;
	int sub;
	int mul;
	int div;
	int mod;

	operation_counts():
		add(0), sub(0), mul(0), div(0), mod(0)
		{}
	operation_counts(const operation_counts& o):
		add(o.add), sub(o.sub), mul(o.mul), div(o.div), mod(o.mod)
		{}

	template <variable_type t>
	friend operation_counts<t> operator+(const operation_counts<t>&, const operation_counts<t>&);	

	template <variable_type t, class T>
	friend operation_counts<t> operator*(const T, const operation_counts<t>&);	

	template <variable_type t, class T>
	friend operation_counts<t> operator*(const operation_counts<t>&, const T);	

	void operator=(const operation_counts<type>& o);
	void operator+=(const operation_counts<type>& o);

	int cycles();
};

template <variable_type type, class T>
operation_counts<type> operator*(const T a, const operation_counts<type>& b)
{
	operation_counts<type> o = b;
	o.add *= a;
	o.sub *= a;
	o.mul *= a;
	o.div *= a;
	o.mod *= a;

	return o;
}

template <variable_type type, class T>
operation_counts<type> operator*(const operation_counts<type>& a, const T b)
{
	operation_counts<type> o = a;
	o.add *= b;
	o.sub *= b;
	o.mul *= b;
	o.div *= b;
	o.mod *= b;

	return o;
}

template <variable_type type>
void operation_counts<type>::operator=(const operation_counts<type>& o)
{
	add = o.add;
	sub = o.sub;
	mul = o.mul;
	div = o.div;
	mod = o.mod;
}

template <variable_type type>
void operation_counts<type>::operator+=(const operation_counts<type>& o)
{
	add += o.add;
	sub += o.sub;
	mul += o.mul;
	div += o.div;
	mod += o.mod;
}

template <variable_type type>
operation_counts<type> operator+(const operation_counts<type>& a, const operation_counts<type>& b)
{
	operation_counts<type> o;
	o.add = a.add + b.add;
	o.sub = a.sub + b.sub;
	o.mul = a.mul + b.mul;
	o.div = a.div + b.div;
	o.mod = a.mod + b.mod;
	return o;
}

int counts_to_cycles(const int n, const op_type op, const variable_type var);

template <variable_type type>
int operation_counts<type>::cycles()
{
	int count = 0;

	count += counts_to_cycles(add, ADD, type);
	count += counts_to_cycles(sub, SUB, type);
	count += counts_to_cycles(mul, MUL, type);
	count += counts_to_cycles(div, DIV, type);
	count += counts_to_cycles(mod, MOD, type);

	return count;
}

class unknown_variable_type {};
class unsupported_variable_type {};
class unknown_op_type {};

class operations {
	operation_counts<INT> int_ops;
	operation_counts<FLOAT> float_ops;
	operation_counts<DOUBLE> double_ops;

public:
	operations()
		{}
	operations(const operations& o):
		int_ops(o.int_ops), float_ops(o.float_ops), double_ops(o.double_ops)
		{}

	friend operations operator+(const operations&, const operations&);

	template <class T>
	friend operations operator*(const T, const operations&);

	template <class T>
	friend operations operator*(const operations&, const T);

	friend ostream& operator<<(ostream& out, const operations& ops);
		
	void operator=(const operations& o);
	void operator+=(const operations& o);

	void add(const variable_type type, const int n);
	void sub(const variable_type type, const int n);
	void mul(const variable_type type, const int n);
	void div(const variable_type type, const int n);
	void mod(const variable_type type, const int n);

	void inc(const op_type& op, const variable_type type);

	void inc_add(const variable_type type) { add(type, 1); }
	void inc_sub(const variable_type type) { sub(type, 1); }
	void inc_mul(const variable_type type) { mul(type, 1); }
	void inc_div(const variable_type type) { div(type, 1); }
	void inc_mod(const variable_type type) { mod(type, 1); }

	int cycles() { return int_ops.cycles() + float_ops.cycles() + double_ops.cycles(); }
};

operations operator+(const operations& a, const operations& b);

template <class T>
operations operator*(const T a, const operations& b)
{
	operations o = b;
	o.int_ops = o.int_ops * a;
	o.float_ops = o.float_ops * a;
	o.double_ops = o.double_ops * a;
	return o;
}

template <class T>
operations operator*(const operations& a, const T b)
{
	operations o = a;
	o.int_ops = o.int_ops * b;
	o.float_ops = o.float_ops * b;
	o.double_ops = o.double_ops * b;
	return o;
}

template <variable_type type>
ostream& operator<<(ostream& out, const operation_counts<type>& op)
{
	out	<< type << " ("	
		<< "add " << op.add << ", "
		<< "sub " << op.sub << ", "
		<< "mul " << op.mul << ", "
		<< "div " << op.div << ", "
		<< "mod " << op.mod
		<< ")";

	return out;
}

ostream& operator<<(ostream& out, const variable_type& type);
ostream& operator<<(ostream& out, const operations& ops);

#endif // OPERATIONS_H

