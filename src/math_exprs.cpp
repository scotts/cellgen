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

string paren_expr::add_iteration(const string& ivar, const string& size) const
{
	if (recurse) {
		return recurse->add_iteration(ivar, size);
	}
	else {
		return terminal;
	}
}

string paren_expr::str() const
{
	if (recurse) {
		return recurse->str();
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

class cant_zero_string {};

string paren_expr::zero_induction(const string& ivar) const
{
	if (terminal.find(ivar) != string::npos) {
		throw cant_zero_string();
	}

	return recurse->zero_induction(ivar);
}

operations paren_expr::cost() const
{
	operations ops;
	if (recurse) {
		ops = recurse->cost();
	}

	return ops;
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

string mult_expr::add_iteration(const string& ivar, const string& size) const
{
	assert(ivar != "");

	const string plus_size = "(" + ivar + "+" + size + ")";
	if (_lhs.str() == ivar) {
		return "(" + plus_size + _op + _rhs.str() + ")";
	}
	else if (_rhs.str() == ivar) {
		return "(" + _lhs.str() + _op + plus_size + ")";
	}
	else {
		return "(" +	_lhs.add_iteration(ivar, size) + 
				_op + 
				_rhs.add_iteration(ivar, size) + 
			")";
	}
}

string mult_expr::next_iteration(const string& ivar) const
{
	return add_iteration(ivar, "1");
}

class ivar_not_found: public exception {};

paren_expr mult_expr::side(const string& ivar, const paren_expr* left_result, const paren_expr* right_result) const
{
	const paren_expr* p = NULL;
	if (_lhs.str().find(ivar) != string::npos) {
		p = left_result;
	}
	else if (_rhs.str().find(ivar) != string::npos) {
		p = right_result;
	}
	else {
		throw ivar_not_found();
	}

	return *p;
}

paren_expr mult_expr::ihs(const string& ivar) const
{
	return side(ivar, &_lhs, &_rhs);
}

paren_expr mult_expr::non_ihs(const string& ivar) const
{
	return side(ivar, &_rhs, &_lhs);
}

string mult_expr::zero_induction(const string& ivar) const
{
	string zeroed;

	if (_lhs.str() == ivar || _rhs.str() == ivar) {
		zeroed = "0";
	}
	else if (_lhs.str().find(ivar) != string::npos) {
		zeroed = "(" + _lhs.zero_induction(ivar) + _op + _rhs.str() + ")";
	}
	else if (_rhs.str().find(ivar) != string::npos) {
		zeroed = "(" + _lhs.str() + _op + _lhs.zero_induction(ivar) + ")";
	}
	else {
		throw ivar_not_found();
	}

	return zeroed;
}

operations mult_expr::cost() const
{
	operations ops;
	ops += _lhs.cost();
	ops += _rhs.cost();

	if (_op == "*") {
		ops.inc_mul(INT);
	}
	else if (_op == "/") {
		ops.inc_div(INT);
	}
	else if (_op == "%") {
		ops.inc_mod(INT);
	}

	return ops;
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

string add_expr::add_iteration(const string& ivar, const string& size) const
{
	if (_lhs.str().find(ivar) != string::npos) {
		return "(" + _lhs.add_iteration(ivar, size) + _op + _rhs.str() + ")";
	}
	else if (_rhs.str().find(ivar) != string::npos) {
		return "(" + _lhs.str() + _op + _rhs.add_iteration(ivar, size) + ")";
	}
	else {
		throw ivar_not_found();
	}
}

string add_expr::next_iteration(const string& ivar) const
{
	return add_iteration(ivar, "1");
}

mult_expr add_expr::side(const string& ivar, const mult_expr* left_result, const mult_expr* right_result) const
{
	const mult_expr* m = NULL;
	if (_lhs.str().find(ivar) != string::npos) {
		m = left_result;
	}
	else if (_rhs.str().find(ivar) != string::npos) {
		m = right_result;
	}
	else {
		throw ivar_not_found();
	}

	return *m;
}

mult_expr add_expr::ihs(const string& ivar) const
{
	return side(ivar, &_lhs, &_rhs);
}

mult_expr add_expr::non_ihs(const string& ivar) const
{
	return side(ivar, &_rhs, &_lhs);
}

string add_expr::zero_induction(const string& ivar) const
{
	string zeroed;

	if (_lhs.str() == ivar) {
		zeroed = "(" + _rhs.str() + ")";
	}
	else if (_rhs.str() == ivar) {
		zeroed = "(" + _lhs.str() + ")";
	}
	else if (_lhs.str().find(ivar) != string::npos) {
		zeroed = "(" + _lhs.zero_induction(ivar) + _op + _rhs.str() + ")";
	}
	else if (_rhs.str().find(ivar) != string::npos) {
		zeroed = "(" + _lhs.str() + _op + _rhs.zero_induction(ivar) + ")";
	}
	else {
		throw ivar_not_found();
	}

	return zeroed; 
}

operations add_expr::cost() const
{
	operations ops;
	ops += _lhs.cost();
	ops += _rhs.cost();

	if (_op == "+") {
		ops.inc_add(INT);
	}
	else if (_op == "-") {
		ops.inc_sub(INT);
	}

	return ops;
}

