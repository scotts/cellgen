#ifndef SPE_REGION_H
#define SPE_REGION_H

class spe_region;
typedef list<spe_region*> spelist;

#include "variable.h"
#include "parse_tree.h"

class spe_region {
	varset* _priv;
	varset* _shared;
	varset _in;		// _in + _out + _inout = _shared
	varset _out;
	varset _inout;
	varset* _reductions;
	string _reduction_op;
	symtbl* _symbols;
	ast_node* _ast_root;
	int _unroll;

public:
	spe_region(varset* v, varset* d, varset* r, string o, symtbl* s, int u):
		_priv(v), _shared(d), _reductions(r), _reduction_op(o), _symbols(s), _unroll(u)
	{
		assert(v);
		assert(d);
		assert(r);
		assert(s);
		assert(_unroll >= 0);
	}

	varset*	priv()		const { return _priv; }
	varset*	shared()	const { return _shared; }
	varset&	in()		{ return _in; }
	varset&	out()		{ return _out; }
	varset&	inout()		{ return _inout; }
	varset*	reductions()	const { return _reductions; }
	string		reduction_op()	const { return _reduction_op; }
	int		unroll()	const { return _unroll; }

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

	symtbl* symbols()
	{
		assert(_symbols);
		return _symbols;
	}
};

#endif	// SPE_REGION_H
