#include "math_exprs.h"
#include "variable.h"

#include <boost/regex.hpp>

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

bool paren_expr::operator==(const paren_expr& o) const
{
	if (recurse && o.recurse) {
		return *recurse == *o.recurse;
	}
	else {
		return terminal == o.terminal;
	}
}

paren_expr::~paren_expr()
{
	delete recurse;
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

string paren_expr::stencil_offset(const string& ivar) const
{
	string offset;
	if (recurse) {
		offset = recurse->stencil_offset(ivar);
	}
	return offset;
}

paren_expr paren_expr::replace_induction(const string& ivar, const string& rep) const
{
	paren_expr replaced;
	if (terminal.find(ivar) != string::npos) {
		replaced = paren_expr(regex_replace(terminal, regex(ivar), rep));
	}
	else if (recurse && recurse->str().find(ivar) != string::npos) {
		replaced = paren_expr(new add_expr(recurse->replace_induction(ivar, rep)));
	}
	else {
		std::cerr << terminal << " " << ivar << std::endl;
		throw ivar_not_found();
	}

	return replaced;
}

paren_expr paren_expr::remove_stencil(const string& ivar) const
{
	if (recurse && recurse->str().find(ivar) != string::npos) {
		return paren_expr(new add_expr(recurse->remove_stencil(ivar)));
	}

	return *this;
}

paren_expr paren_expr::expand_induction(const string& i) const
{
	paren_expr exp;
	if (terminal.find(i) != string::npos) {
		exp = paren_expr(new add_expr(terminal, "+", index_adapt()(i).name()));
	}
	else if (recurse && recurse->str().find(i) != string::npos) {
		exp = paren_expr(new add_expr(recurse->expand_induction(i)));
	}
	else {
		exp = *this;
	}

	return exp;
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
	const string lhs = _lhs.str();
	const string rhs = _rhs.str();

	assert(ivar != "");

	if (lhs == ivar) {
		return hug(hug(lhs + "+" + size) + _op + rhs);
	}
	else if (rhs == ivar) {
		return hug(lhs + _op + hug(rhs + "+" + size));
	}
	else {
		return hug(_lhs.add_iteration(ivar, size) + _op + _rhs.add_iteration(ivar, size));
	}
}

string mult_expr::next_iteration(const string& ivar) const
{
	return add_iteration(ivar, "1");
}

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

string mult_expr::stencil_offset(const string& ivar) const
{
	return side(ivar, &_lhs, &_rhs).stencil_offset(ivar);
}

mult_expr mult_expr::replace_induction(const string& ivar, const string& rep) const
{
	mult_expr replaced;

	if (_lhs.str() == ivar) {
		replaced = mult_expr(rep, _op, _rhs);	
	}
	else if (_rhs.str() == ivar) {
		replaced = mult_expr(_lhs, _op, rep);
	}
	else if (_lhs.str().find(ivar) != string::npos) {
		replaced = mult_expr(_lhs.replace_induction(ivar, rep), _op, _rhs);
	}
	else if (_rhs.str().find(ivar) != string::npos) {
		replaced = mult_expr(_lhs, _op, _lhs.replace_induction(ivar, rep));
	}
	else {
		throw ivar_not_found();
	}

	return replaced;
}

mult_expr mult_expr::zero_induction(const string& ivar) const
{
	return replace_induction(ivar, "0");
}

mult_expr mult_expr::remove_stencil(const string& ivar) const
{
	if (_lhs.str().find(ivar) != string::npos) {
		return mult_expr(_lhs.remove_stencil(ivar), _op, _rhs);
	}
	else if (_rhs.str().find(ivar) != string::npos) {
		return mult_expr(_lhs, _op, _rhs.remove_stencil(ivar));
	}

	return *this;
}

mult_expr mult_expr::expand_induction(const string& i) const
{
	mult_expr exp;
	if (_lhs.str().find(i) != string::npos) {
		exp = mult_expr(_lhs.expand_induction(i), _op, _rhs);
	}
	else if (_rhs.str().find(i) != string::npos) {
		exp = mult_expr(_lhs, _op, _rhs.expand_induction(i));
	}
	else {
		exp = *this;
	}

	return exp;
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
	else if (_lhs.str() == "") {
		return _rhs.str();
	}
	else if (_rhs.str() == "") {
		return _lhs.str();
	}

	return hug(_lhs.str() + _op + _rhs.str());
}

string add_expr::add_iteration(const string& ivar, const string& size) const
{
	const string lhs = _lhs.str();
	const string rhs = _rhs.str();

	if (lhs == ivar || rhs == ivar) {
		return hug(lhs + _op + rhs + "+" + size);
	}
	else if (lhs.find(ivar) != string::npos) {
		return hug(_lhs.add_iteration(ivar, size) + _op + rhs);
	}
	else if (rhs.find(ivar) != string::npos) {
		return hug(lhs + _op + _rhs.add_iteration(ivar, size));
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

string add_expr::stencil_offset(const string& ivar) const
{
	const string& lhs = _lhs.str();
	const string& rhs = _rhs.str();
	string offset;

	if (lhs == ivar) {
		offset = _op + rhs;
	}
	else if (rhs == ivar) {
		offset = _op + lhs;
	}
	else {
		return side(ivar, &_lhs, &_rhs).stencil_offset(ivar);
	}

	return offset;
}

string add_expr::stencil_offset(const condslist& above) const
{
	int offset = 0;
	for (condslist::const_reverse_iterator i = above.rbegin(); i != above.rend(); ++i) {	
		offset = from_string<int>(stencil_offset(i->induction));
		if (offset != 0) {
			break;
		}
	}

	return to_string(offset);
}

add_expr add_expr::replace_induction(const string& ivar, const string& rep) const
{
	add_expr replaced;

	if (_lhs.str() == ivar) {
		replaced = add_expr(mult_expr(rep), _op, _rhs);
	}
	else if (_rhs.str() == ivar) {
		replaced = add_expr(_lhs, _op, mult_expr(rep));
	}
	else if (_lhs.str().find(ivar) != string::npos) {
		replaced = add_expr(_lhs.replace_induction(ivar, rep), _op, _rhs);
	}
	else if (_rhs.str().find(ivar) != string::npos) {
		replaced = add_expr(_lhs, _op, _rhs.replace_induction(ivar, rep));
	}
	else {
		throw ivar_not_found();
	}

	return replaced; 
}

add_expr add_expr::zero_induction(const string& ivar) const
{
	return replace_induction(ivar, "0");
}

add_expr add_expr::remove_stencil(const string& ivar) const
{
	const string& lhs = _lhs.str();
	const string& rhs = _rhs.str();

	// We lost all of our node meta-information when we transformed it 
	// into a data structure. So we need to resort to regexes.
	if (lhs == ivar && regex_match(rhs, regex("\\d*"))) {
		return add_expr(_lhs, _op, mult_expr());
	}
	else if (rhs == ivar && regex_match(lhs, regex("\\d*"))) {
		return add_expr(mult_expr(), _op, _rhs);
	}
	else if (lhs.find(ivar) != string::npos) {
		return add_expr(_lhs.remove_stencil(ivar), _op, _rhs);
	}
	else if (rhs.find(ivar) != string::npos) {
		return add_expr(_lhs, _op, _rhs.remove_stencil(ivar));
	}

	return *this;
}

struct call_remove_stencil {
	add_expr& add;
	call_remove_stencil(add_expr& a): add(a) {}
	void operator()(const conditions& cond) const
	{
		add = add.remove_stencil(cond.induction);
	}
};

add_expr add_expr::remove_all_stencil(const condslist& nested) const
{
	add_expr exp = *this;
	for_all(nested, call_remove_stencil(exp));
	return exp;
}

add_expr add_expr::expand_induction(const string& i) const
{
	add_expr exp;
	if (_lhs.str().find(i) != string::npos) {
		exp = add_expr(_lhs.expand_induction(i), _op, _rhs);
	}
	else if (_rhs.str().find(i) != string::npos) {
		exp = add_expr(_lhs, _op, _rhs.expand_induction(i));
	}
	else {
		exp = *this;
	}

	return exp;
}

struct call_expand_induction {
	add_expr& add;
	call_expand_induction(add_expr& a): add(a) {}
	void operator()(const conditions& cond) const
	{
		add = add.expand_induction(cond.induction);
	}
};

add_expr add_expr::expand_all_inductions(const condslist& nested) const
{
	add_expr exp = *this;
	for_all(nested, call_expand_induction(exp));
	return exp;
}

unsigned int add_expr::index(const string& ivar) const
{
	unsigned int i = 1;
	for (list<string>::const_iterator it = _indices.begin(); it != _indices.end(); ++it) {
		if (it->find(ivar) != string::npos) {
			break;
		}
		else {
			++i;
		}
	}

	if (i > _indices.size()) {
		throw ivar_not_found();
	}

	return i;
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

