#include "operations.h"

operation_counts operator+(const operation_counts& a, const operation_counts& b)
{
	operation_counts o;
	o.add = a.add + b.add;
	o.sub = a.sub + b.sub;
	o.mul = a.mul + b.mul;
	o.div = a.div + b.div;
	o.mod = a.mod + b.mod;
	return o;
}

operations operator+(const operations& a, const operations& b)
{
	operations o;
	o.int_ops = a.int_ops + b.int_ops;
	o.float_ops = a.float_ops + b.float_ops;
	o.double_ops = a.double_ops + b.double_ops;
	return o;
}

ostream& operator<<(ostream& out, const variable_type& type)
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

ostream& operator<<(ostream& out, const operation_counts& op)
{
	out	<< "add " << op.add << ", "
		<< "sub " << op.sub << ", "
		<< "mul " << op.mul << ", "
		<< "div " << op.div << ", "
		<< "mod " << op.mod;

	return out;
}

ostream& operator<<(ostream& out, const operations& ops)
{
	out	<< "int (" << ops.int_ops << ")" << endl	
		<< "float (" << ops.float_ops << ")" << endl
		<< "double (" << ops.double_ops << ")" << endl;

	return out;
}

/* Why strings instead of an enum like ADD, SUB, etc? I already 
 * have the op in string form from the source. I never act on it 
 * in the same way as a variable_type, so I don't see any benefit
 * in conversion.
 */
map<variable_type, map<string, int> > cost_tbl;

/* Until I can pass native lists to a constructor as promised 
 * in C++0x, this is the best way I can come up with to 
 * initialize the cost table.
 */
__attribute__((constructor)) void construct_cost_tbl()
{
	// Assuming all integer types are the same.

	cost_tbl[CHAR]["+"] = 2;
	cost_tbl[CHAR]["-"] = 2;
	cost_tbl[CHAR]["*"] = 7;
	cost_tbl[CHAR]["/"] = 7;
	cost_tbl[CHAR]["%"] = 7;

	cost_tbl[INT]["+"] = 2;
	cost_tbl[INT]["-"] = 2;
	cost_tbl[INT]["*"] = 7;
	cost_tbl[INT]["/"] = 7;
	cost_tbl[INT]["%"] = 7;

	cost_tbl[LONG]["+"] = 2;
	cost_tbl[LONG]["-"] = 2;
	cost_tbl[LONG]["*"] = 7;
	cost_tbl[LONG]["/"] = 7;
	cost_tbl[LONG]["%"] = 7;

	cost_tbl[FLOAT]["+"] = 6;
	cost_tbl[FLOAT]["-"] = 6;
	cost_tbl[FLOAT]["*"] = 6;
	cost_tbl[FLOAT]["/"] = 6; // assuming div is same as mul
	cost_tbl[FLOAT]["%"] = 6; // complete guess

	cost_tbl[DOUBLE]["+"] = 13;
	cost_tbl[DOUBLE]["-"] = 13;
	cost_tbl[DOUBLE]["*"] = 13;
	cost_tbl[DOUBLE]["/"] = 13; // assuming div is same cost as mul
	cost_tbl[DOUBLE]["%"] = 13; // complete guess
}

