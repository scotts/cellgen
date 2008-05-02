#ifndef MULT_EXPR_H
#define MULT_EXPR_H

#include <list>
#include <string>
#include <iostream>
using namespace std;

class add_expr;

class paren_expr {
	string terminal;
	add_expr* recurse;

public:
	paren_expr(): recurse(NULL) {}
	paren_expr(const string& t): terminal(t), recurse(NULL) {}
	paren_expr(add_expr* r): recurse(r) {}
	paren_expr(const paren_expr& o);
	
	~paren_expr();

	paren_expr& operator=(const string& s);
	paren_expr& operator=(add_expr* r);
	paren_expr& operator=(const paren_expr& p);
	paren_expr& operator+=(const string& s);

	string str() const;
	string next_iteration(const string& ivar) const;
	add_expr eval() const;
};

class mult_expr {
	paren_expr _lhs;
	string _op;
	paren_expr _rhs;

public:
	mult_expr() {}
	mult_expr(const string& l): _lhs(l) {}
	mult_expr(const string& l, const string& o, const string& r): _lhs(l), _op(o), _rhs(r) {}
	mult_expr(const paren_expr& l, const string& o, const string& r): _lhs(l), _op(o), _rhs(r) {}
	mult_expr(const string& l, const string& o, const paren_expr& r): _lhs(l), _op(o), _rhs(r) {}
	mult_expr(const mult_expr& o): _lhs(o._lhs), _op(o._op), _rhs(o._rhs) {}

	void lhs(const string& s)	{ _lhs = s; }
	void lhs(add_expr* a)		{ _lhs = a; }

	void op(const string& s)	{ _op = s; }

	void rhs(const string& s)	{ _rhs = s; }
	void rhs(add_expr* a)		{ _rhs = a; }

	void build_lhs(const string& s) { _lhs += s; }
	void build_rhs(const string& s) { _rhs += s; }

	string str() const;
	string next_iteration(const string& ivar) const;
	string factor(const string& ivar) const;
};

class add_expr {
	mult_expr _lhs;
	string _op;
	mult_expr _rhs;
	
public:
	add_expr() {}
	add_expr(const add_expr& o): _lhs(o._lhs), _op(o._op), _rhs(o._rhs) {}
	add_expr(const mult_expr& m): _lhs(m) {}
	add_expr(const mult_expr& l, const string& o, const mult_expr& r):
		_lhs(l), _op(o), _rhs(r) {}

	void lhs(const mult_expr& e)	{ _lhs = e; }
	void op(const string& o)	{ _op = o; }
	void rhs(const mult_expr& e)	{ _rhs = e; }

	mult_expr lhs() const	{ return _lhs; }
	string op() const	{ return _op; }
	mult_expr rhs() const	{ return _rhs; }

	string str() const;
	string next_iteration(const string& ivar) const;
	string factor(const string& ivar) const;
};

#endif	// MULT_EXPR_H
