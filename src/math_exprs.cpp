#include "math_exprs.h"

/* class paren_expr
 */
paren_expr::paren_expr(const paren_expr& o): terminal(o.terminal)
{
	if (o.recurse) {
		recurse = new add_expr(*o.recurse);
	}
	else {
		recurse = NULL;
	}
}

add_expr paren_expr::eval() const
{
	if (recurse) {
		return *recurse;
	}
	return add_expr(mult_expr(terminal));
}

string paren_expr::next_iteration(const string& ivar) const
{
	if (recurse) {
		return recurse->next_iteration(ivar);
	}
	else {
		return terminal;
	}
}

string paren_expr::str() const
{
	if (recurse) {
		if (recurse->op() == "") {
			return recurse->str();
		}
		else {
			return "(" + recurse->str() + ")";
		}
	}
	return terminal;
}

paren_expr& paren_expr::operator=(const string& s)
{
	terminal = s;
	if (recurse) {
		delete recurse;
		recurse = NULL;
	}

	return *this;
}

paren_expr& paren_expr::operator=(add_expr* r)
{
	terminal = "";
	if (recurse) {
		delete recurse;
	}
	recurse = r;

	return *this;
}

paren_expr& paren_expr::operator=(const paren_expr& p)
{
	if (p.recurse) {
		recurse = new add_expr(*p.recurse);
	}
	else {
		recurse = NULL;
		terminal = p.terminal;
	}

	return *this;
}

paren_expr& paren_expr::operator+=(const string& s)
{
	terminal += s;
	if (recurse) {
		delete recurse;
		recurse = NULL;
	}

	return *this;
}

paren_expr::~paren_expr()
{
	delete recurse;
}

/* class mult_expr
 */
string mult_expr::str() const
{
	if (_lhs.str() == "" && _op == "" && _rhs.str() == "") {
		return "";
	}
	else {
		return _lhs.str() + _op + _rhs.str();
	}
}

string mult_expr::next_iteration(const string& ivar) const
{
	assert(ivar != "");

	const string plus_one = "(" + ivar + "+1)";
	if (_lhs.str() == ivar) {
		return "(" + plus_one + _op + _rhs.str() + ")";
	}
	else if (_rhs.str() == ivar) {
		return "(" + _lhs.str() + _op + plus_one + ")";
	}
	else {
		return "(" +	_lhs.next_iteration(ivar) + 
				_op + 
				_rhs.next_iteration(ivar) + 
			")";
	}
}

class ivar_not_found: public exception {};

string mult_expr::factor(const string& ivar) const
{
	if (_lhs.str().find(ivar) != string::npos) {
		return _op + _rhs.str();
	}
	else if (_rhs.str().find(ivar) != string::npos) {
		return _op + _lhs.str();
	}
	else {
		throw ivar_not_found();
	}
}

/* class add_expr
 */
string add_expr::str() const
{
	if (_lhs.str() == "" && _op == "" && _rhs.str() == "") {
		return "";
	}
	return _lhs.str() + _op + _rhs.str();
}

string add_expr::next_iteration(const string& ivar) const
{
	// Not sure if ONLY doing the side with ivar works 
	// in the general case.
	if (_lhs.str().find(_ivar) != string::npos) {
		return _lhs.next_iteration(ivar);
	}
	else if (_rhs.str().find(_ivar) != string::npos) {
		return _rhs.next_iteration(ivar);
	}
	else {
		throw ivar_not_found();
	}
}

string add_expr::next_iteration() const
{
	assert(_ivar != "");
	return next_iteration(_ivar);
}

string add_expr::factor() const
{
	if (_lhs.str().find(_ivar) != string::npos) {
		return _lhs.factor(_ivar);
	}
	else if (_rhs.str().find(_ivar) != string::npos) {
		return _rhs.factor(_ivar);
	}
	else {
		throw ivar_not_found();
	}
}

