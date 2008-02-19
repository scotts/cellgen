#ifndef MULT_EXPR_H
#define MULT_EXPR_H

#include <string>
using namespace std;

class mult_expr {
	string _lhs;
	string _op;
	string _rhs;
	string _ivar;

public:
	mult_expr() {}
	mult_expr(const mult_expr& o):
		_lhs(o._lhs), _op(o._op), _rhs(o._rhs), _ivar(o._ivar)
		{}

	void lhs(const string& s) { _lhs = s; }
	void op(const string& s) { _op = s; }
	void rhs(const string& s) { _rhs = s; }
	void ivar(const string& s) { _ivar = s; }

	void build_lhs(const string& s) { _lhs += s; }
	void build_rhs(const string& s) { _rhs += s; }

	string as_written()
	{
		if (_op == "") return _ivar;
		else return "(" + _lhs + _op + _rhs + ")";
	}

	string next_iteration()
	{
		string plus_one = "(" + _ivar + "+1)";

		if (_lhs == _ivar) {
			return "(" + plus_one + _op + _rhs + ")";
		}
		else {
			return "(" +_rhs + _op + plus_one + ")";
		}
	}

	string factor()
	{
		if (_lhs == _ivar) {
			return _op + _rhs;
		}
		else {
			return _op + _lhs;
		}
	}

	string report()
	{
		return "'" + _lhs + "' '" + _op + "' '" + _rhs + "' ivar: '" + _ivar + "'";
	}
};

#endif	// MULT_EXPR_H
