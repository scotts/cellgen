#include "operations.h"

op_type construct_op_type(const string& op)
{
	if (op == "+") {
		return ADD;
	}
	else if (op == "-") {
		return SUB;
	}
	else if (op == "*") {
		return MUL;
	}
	else if (op == "/") {
		return DIV;
	}
	else if (op == "%") {
		return MOD;
	}

	return UNKNOWN_OP;
}

operations operator+(const operations& a, const operations& b)
{
	operations o;
	o.int_ops = a.int_ops + b.int_ops;
	o.float_ops = a.float_ops + b.float_ops;
	o.double_ops = a.double_ops + b.double_ops;
	return o;
}

void operations::operator=(const operations& o)
{
	int_ops = o.int_ops;
	float_ops = o.float_ops;
	double_ops = o.double_ops;
}

void operations::operator+=(const operations& o)
{
	int_ops += o.int_ops;
	float_ops += o.float_ops;
	double_ops += o.double_ops;
}

#define __SWITCH_TYPE_INCREMENT(op, type, n) \
({ \
	switch (type) { \
		case INT:		int_ops.op += n; \
					break; \
		case FLOAT:		float_ops.op += n; \
					break; \
		case DOUBLE:		double_ops.op += n; \
					break; \
		case UNKNOWN_VAR:	throw unknown_variable_type(); \
					break; \
		default: throw unsupported_variable_type(); \
	} \
})

void operations::add(const variable_type type, const int n)
{
	__SWITCH_TYPE_INCREMENT(add, type, n);
}

void operations::sub(const variable_type type, const int n)
{
	__SWITCH_TYPE_INCREMENT(sub, type, n);
}

void operations::mul(const variable_type type, const int n)
{
	__SWITCH_TYPE_INCREMENT(mul, type, n);
}

void operations::div(const variable_type type, const int n)
{
	__SWITCH_TYPE_INCREMENT(div, type, n);
}

void operations::mod(const variable_type type, const int n)
{
	__SWITCH_TYPE_INCREMENT(mod, type, n);
}

void operations::inc(const op_type& op, const variable_type type)
{
	switch (op) {
		case ADD: inc_add(type);
			  break;
		case SUB: inc_sub(type);
			  break;
		case MUL: inc_mul(type);
			  break;
		case DIV: inc_div(type);
			  break;
		case MOD: inc_mod(type);
			  break;

		default: throw unknown_op_type();
	}
}

ostream& operator<<(ostream& out, const variable_type& type)
{
	string str;
	switch (type) {
		case INT:		str = "INT";
					break;
		case LONG:		str = "LONG";
					break;
		case FLOAT:		str = "FLOAT";
					break;
		case DOUBLE:		str = "DOUBLE";
					break;
		case CHAR:		str = "CHAR";
					break;
		case UNKNOWN_VAR:	str = "UNKNOWN_VAR";
					break;

		default: throw unknown_variable_type();
	}

	out << str;
	return out;
}

ostream& operator<<(ostream& out, const operations& ops)
{
	out	<< ops.int_ops << endl	
		<< ops.float_ops << endl
		<< ops.double_ops << endl;

	return out;
}

map<variable_type, map<op_type, int> > latency;

/* Until I can pass native lists to a constructor as promised 
 * in C++0x, this is the best way I can come up with to 
 * initialize the cost table.
 */
__attribute__((constructor)) void construct_latency()
{
	// Assuming all integer types are the same.

	latency[CHAR][ADD] = 2;
	latency[CHAR][SUB] = 2;
	latency[CHAR][MUL] = 7;
	latency[CHAR][DIV] = 7;
	latency[CHAR][MOD] = 7;

	latency[INT][ADD] = 2;
	latency[INT][SUB] = 2;
	latency[INT][MUL] = 7;
	latency[INT][DIV] = 7;
	latency[INT][MOD] = 7;

	latency[LONG][ADD] = 2;
	latency[LONG][SUB] = 2;
	latency[LONG][MUL] = 7;
	latency[LONG][DIV] = 7;
	latency[LONG][MOD] = 7;

	latency[FLOAT][ADD] = 6;
	latency[FLOAT][SUB] = 6;
	latency[FLOAT][MUL] = 6;
	latency[FLOAT][DIV] = 6; // assuming div is same as mul
	latency[FLOAT][MOD] = 6; // complete guess

	latency[DOUBLE][ADD] = 13;
	latency[DOUBLE][SUB] = 13;
	latency[DOUBLE][MUL] = 13;
	latency[DOUBLE][DIV] = 13; // assuming div is same cost as mul
	latency[DOUBLE][MOD] = 13; // complete guess
}

int counts_to_cycles(const int n, const op_type op, const variable_type var)
{
	return n * latency[var][op];
}

