#ifndef OPERATIONS_H
#define OPERATIONS_H

#include <iostream>
#include <map>
using namespace std;

enum variable_type {CHAR, INT, LONG, FLOAT, DOUBLE, UNKNOWN};

extern map<variable_type, map<string, int> > cost_tbl;

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

	friend operation_counts operator+(const operation_counts&, const operation_counts&);	

	template <class T>
	friend operation_counts operator*(const T, const operation_counts&);	

	template <class T>
	friend operation_counts operator*(const operation_counts&, const T);	

	void operator=(const operation_counts& o);
	void operator+=(const operation_counts& o);
};

operation_counts operator+(const operation_counts& a, const operation_counts& b);

template <class T>
operation_counts operator*(const T a, const operation_counts& b)
{
	operation_counts o = b;
	o.add *= a;
	o.sub *= a;
	o.mul *= a;
	o.div *= a;
	o.mod *= a;

	return o;
}

template <class T>
operation_counts operator*(const operation_counts& a, const T b)
{
	operation_counts o = a;
	o.add *= b;
	o.sub *= b;
	o.mul *= b;
	o.div *= b;
	o.mod *= b;

	return o;
}

class unknown_variable_type {};
class unsupported_variable_type {};
class unknown_op_type {};

class operations {
	operation_counts int_ops;
	operation_counts float_ops;
	operation_counts double_ops;

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
	void inc_add(const variable_type type);
	void sub(const variable_type type, const int n);
	void inc_sub(const variable_type type);
	void mul(const variable_type type, const int n);
	void inc_mul(const variable_type type);
	void div(const variable_type type, const int n);
	void inc_div(const variable_type type);
	void mod(const variable_type type, const int n);
	void inc_mod(const variable_type type);

	void inc(const string& op, const variable_type type);
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

ostream& operator<<(ostream& out, const variable_type& type);
ostream& operator<<(ostream& out, const operation_counts& op);
ostream& operator<<(ostream& out, const operations& ops);

#endif // OPERATIONS_H

