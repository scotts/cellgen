#ifndef SPE_REGION_H
#define SPE_REGION_H

class spe_region;
typedef list<spe_region*> spelist;

#include "variable.h"
#include "parse_tree.h"

class spe_region {
	privset _priv;
	sharedset _shared;
	sharedset _in;		// _in + _out + _inout = _shared
	sharedset _out;
	sharedset _inout;
	reduceset _reductions;
	string _reduction_op;
	symtbl _symbols;
	ast_node* _ast_root;
	string _induction;
	int _unroll;

public:
	spe_region(const privset& p, const sharedset& sh, const reduceset& r, const string& o, const symtbl& sy, int u):
		_priv(p), _shared(sh), _reductions(r), _reduction_op(o), _symbols(sy), _unroll(u)
	{
		assert(_unroll >= 0);
		if (_reductions.size() > 0) {
			assert(_reduction_op != "");
		}
	}

	privset& priv()			{ return _priv; }
	sharedset& shared()		{ return _shared; }
	sharedset& in()			{ return _in; }
	sharedset& out()		{ return _out; }
	sharedset& inout()		{ return _inout; }
	reduceset& reductions()		{ return _reductions; }
	symtbl& symbols()		{ return _symbols; }

	string reduction_op() const	{ return _reduction_op; }
	int unroll() const		{ return _unroll; }
	string induction() const	{ return _induction; }

	void induction(const string& i)
	{
		_induction = i;
	}

	void ast_root(ast_node* a)
	{
		assert(a);
		_ast_root = a;
	}

	ast_node* ast_root() const
	{
		assert(_ast_root);
		return _ast_root;
	}
};

#endif	// SPE_REGION_H
