#ifndef TYPE_OPS_H
#define TYPE_OPS_H

#include <iostream>
using namespace std;

enum variable_type {INT, LONG, FLOAT, DOUBLE, CHAR, UNKNOWN};

inline ostream& operator<<(ostream& out, const variable_type& type)
{
	string str;
	switch (type) {
		case INT:	str = "INT";
				break;
		case LONG:	str = "LONG";
				break;
		case FLOAT:	str = "FLOAT";
				break;
		case DOUBLE:	str = "DOUBLE";
				break;
		case CHAR:	str = "CHAR";
				break;
		case UNKNOWN:	str = "UNKNOWN";
				break;
	}

	out << str;
	return out;
}

struct opcounts {
	int add;
	int sub;
	int mul;
	int div;
	int mod;

	opcounts():
		add(0), sub(0), mul(0), div(0), mod(0)
		{}
	opcounts(const opcounts& o):
		add(o.add), sub(o.sub), mul(o.mul), div(o.div), mod(o.mod)
		{}

	friend opcounts operator+(const opcounts&, const opcounts&);	

	template <class T>
	friend opcounts operator*(const T, const opcounts&);	

	template <class T>
	friend opcounts operator*(const opcounts&, const T);	

	void operator=(const opcounts& o)
	{
		add = o.add;
		sub = o.sub;
		mul = o.mul;
		div = o.div;
		mod = o.mod;
	}

	void operator+=(const opcounts& o)
	{
		add += o.add;
		sub += o.sub;
		mul += o.mul;
		div += o.div;
		mod += o.mod;
	}
};

inline opcounts operator+(const opcounts& a, const opcounts& b)
{
	opcounts o;
	o.add = a.add + b.add;
	o.sub = a.sub + b.sub;
	o.mul = a.mul + b.mul;
	o.div = a.div + b.div;
	o.mod = a.mod + b.mod;
	return o;
}

template <class T>
inline opcounts operator*(const T a, const opcounts& b)
{
	opcounts o = b;
	o.add *= a;
	o.sub *= a;
	o.mul *= a;
	o.div *= a;
	o.mod *= a;

	return o;
}

template <class T>
inline opcounts operator*(const opcounts& a, const T b)
{
	opcounts o = a;
	o.add *= b;
	o.sub *= b;
	o.mul *= b;
	o.div *= b;
	o.mod *= b;

	return o;
}

class unknown_variable_type {};
class unsupported_variable_type {};

class type_ops {
	opcounts int_ops;
	opcounts float_ops;
	opcounts double_ops;

public:
	type_ops()
		{}
	type_ops(const type_ops& o):
		int_ops(o.int_ops), float_ops(o.float_ops), double_ops(o.double_ops)
		{}

	friend type_ops operator+(const type_ops&, const type_ops&);

	template <class T>
	friend type_ops operator*(const T, const type_ops&);

	template <class T>
	friend type_ops operator*(const type_ops&, const T);

	friend ostream& operator<<(ostream& out, const type_ops& ops);
		
	void operator=(const type_ops& o)
	{
		int_ops = o.int_ops;
		float_ops = o.float_ops;
		double_ops = o.double_ops;
	}

	void operator+=(const type_ops& o)
	{
		int_ops += o.int_ops;
		float_ops += o.float_ops;
		double_ops += o.double_ops;
	}

#define __SWITCH_TYPE_INCREMENT(op, type, n) \
({ \
	switch (type) { \
		case INT:	int_ops.op += n; \
				break; \
		case FLOAT:	float_ops.op += n; \
				break; \
		case DOUBLE:	double_ops.op += n; \
				break; \
		case UNKNOWN:	throw unknown_variable_type(); \
				break; \
		default:	throw unsupported_variable_type(); \
	} \
})

	void add(const variable_type type, const int n)
	{
		__SWITCH_TYPE_INCREMENT(add, type, n);
	}

	void inc_add(const variable_type type)
	{
		add(type, 1);
	}

	void sub(const variable_type type, const int n)
	{
		__SWITCH_TYPE_INCREMENT(sub, type, n);
	}

	void inc_sub(const variable_type type)
	{
		sub(type, 1);
	}

	void mul(const variable_type type, const int n)
	{
		__SWITCH_TYPE_INCREMENT(mul, type, n);
	}

	void inc_mul(const variable_type type)
	{
		mul(type, 1);
	}

	void div(const variable_type type, const int n)
	{
		__SWITCH_TYPE_INCREMENT(div, type, n);
	}

	void inc_div(const variable_type type)
	{
		div(type, 1);
	}

	void mod(const variable_type type, const int n)
	{
		__SWITCH_TYPE_INCREMENT(mod, type, n);
	}

	void inc_mod(const variable_type type)
	{
		mod(type, 1);
	}
};

inline type_ops operator+(const type_ops& a, const type_ops& b)
{
	type_ops o;
	o.int_ops = a.int_ops + b.int_ops;
	o.float_ops = a.float_ops + b.float_ops;
	o.double_ops = a.double_ops + b.double_ops;
	return o;
}

template <class T>
type_ops operator*(const T a, const type_ops& b)
{
	type_ops o = b;
	o.int_ops = o.int_ops * a;
	o.float_ops = o.float_ops * a;
	o.double_ops = o.double_ops * a;
	return o;
}

template <class T>
type_ops operator*(const type_ops& a, const T b)
{
	type_ops o = a;
	o.int_ops = o.int_ops * b;
	o.float_ops = o.float_ops * b;
	o.double_ops = o.double_ops * b;
	return o;
}

inline ostream& operator<<(ostream& out, const opcounts& op)
{
	out	<< "add " << op.add << ", "
		<< "sub " << op.sub << ", "
		<< "mul " << op.mul << ", "
		<< "div " << op.div << ", "
		<< "mod " << op.mod;

	return out;
}

inline ostream& operator<<(ostream& out, const type_ops& ops)
{
	out	<< "int (" << ops.int_ops << ")" << endl	
		<< "float (" << ops.float_ops << ")" << endl
		<< "double (" << ops.double_ops << ")" << endl;

	return out;
}

#endif // TYPE_OPS_H

