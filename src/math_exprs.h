#ifndef MULT_EXPR_H
#define MULT_EXPR_H

#include <list>
#include <string>
#include <iostream>
#include <cassert>
using namespace std;

#include "utility.h"
#include "operations.h"
#include "conditions.h"

class ivar_not_found: public exception {};

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
	bool operator==(const paren_expr& o) const;

	string str() const;
	string add_iteration(const string& ivar, const string& size) const;
	string stencil_offset(const string& ivar) const;
	add_expr eval() const;
	paren_expr replace_induction(const string& ivar, const string& rep) const;
	paren_expr remove_stencil(const string& ivar) const;
	paren_expr expand_induction(const string& i) const;

	operations cost() const;
};

class mult_expr {
	paren_expr _lhs;
	string _op;
	paren_expr _rhs;

	paren_expr side(const string& ivar, const paren_expr* l, const paren_expr* r) const;

public:
	mult_expr() {}
	mult_expr(const string& l): _lhs(l) {}
	mult_expr(const paren_expr& p): _lhs(p) {}
	mult_expr(const string& l, const string& o, const string& r): _lhs(l), _op(o), _rhs(r) {}
	mult_expr(const paren_expr& l, const string& o, const string& r): _lhs(l), _op(o), _rhs(r) {}
	mult_expr(const string& l, const string& o, const paren_expr& r): _lhs(l), _op(o), _rhs(r) {}
	mult_expr(const paren_expr& l, const string& o, const paren_expr& r): _lhs(l), _op(o), _rhs(r) {}
	mult_expr(const mult_expr& o): _lhs(o._lhs), _op(o._op), _rhs(o._rhs) {}

	void lhs(const string& s)	{ _lhs = s; }
	void lhs(add_expr* a)		{ _lhs = a; }
	void lhs(const paren_expr& p)	{ _lhs = p; }
	paren_expr lhs()		{ return _lhs; }

	void op(const string& s)	{ _op = s; }

	void rhs(const string& s)	{ _rhs = s; }
	void rhs(add_expr* a)		{ _rhs = a; }
	void rhs(const paren_expr& p)	{ _rhs = p; }
	paren_expr rhs()		{ return _rhs; }

	void build_lhs(const string& s) { _lhs += s; }
	void build_rhs(const string& s) { _rhs += s; }

	paren_expr ihs(const string& ivar) const;
	paren_expr non_ihs(const string& ivar) const;

	string str() const;
	string add_iteration(const string& ivar, const string& size) const;
	string next_iteration(const string& ivar) const;
	string stencil_offset(const string& ivar) const;
	mult_expr replace_induction(const string& ivar, const string& rep) const;
	mult_expr zero_induction(const string& ivar) const;
	mult_expr remove_stencil(const string& ivar) const;
	mult_expr expand_induction(const string& i) const;

	operations cost() const;

	bool operator==(const mult_expr& o) const { return _lhs == o._lhs && _op == o._op && _rhs == o._rhs; }
};

class add_expr {
	mult_expr _lhs;
	string _op;
	mult_expr _rhs;
	list<string> _indices;

	mult_expr side(const string& ivar, const mult_expr* l, const mult_expr* r) const;
	
public:
	add_expr() {}
	add_expr(const add_expr& o): _lhs(o._lhs), _op(o._op), _rhs(o._rhs), _indices(o._indices) {}
	add_expr(const mult_expr& m): _lhs(m) {}
	add_expr(const mult_expr& l, const string& o, const mult_expr& r):
		_lhs(l), _op(o), _rhs(r) {}

	void lhs(const mult_expr& e)	{ _lhs = e; }
	void op(const string& o)	{ _op = o; }
	void rhs(const mult_expr& e)	{ _rhs = e; }
	void indices(const list<string>& i) { _indices = i; }

	mult_expr lhs() const	{ return _lhs; }
	string op() const	{ return _op; }
	mult_expr rhs() const	{ return _rhs; }

	mult_expr ihs(const string& ivar) const;
	mult_expr non_ihs(const string& ivar) const;

	string str() const;
	string add_iteration(const string& ivar, const string& size) const;
	string next_iteration(const string& ivar) const;
	string stencil_offset(const string& ivar) const;
	string stencil_offset(const condslist& above) const;
	add_expr replace_induction(const string& ivar, const string& rep) const;
	add_expr zero_induction(const string& ivar) const;
	add_expr remove_stencil(const string& i) const;
	add_expr remove_all_stencil(const condslist& above) const;
	add_expr expand_induction(const string& i) const;
	add_expr expand_all_inductions(const condslist& above, const bool nested) const;

	string factor(const string& ivar) const
	{
		return ihs(ivar).non_ihs(ivar).str();
	}

	unsigned int index(const string& ivar) const;

	operations cost() const;

	bool operator==(const add_expr& o) const { return _lhs == o._lhs && _op == o._op && _rhs == o._rhs; }
};

add_expr construct_access_formula(const list<string>& dimensions, const list<add_expr>& indices);

#endif	// MULT_EXPR_H
