#ifndef SPE_REGION_H
#define SPE_REGION_H

#include "variable.h"
#include "parse_tree.h"

class spe_region {
	varlist* _priv;
	varlist* _shared;
	varlist _in;		// _in + _out + _inout = _shared
	varlist _out;
	varlist _inout;
	varlist* _reductions;
	string _reduction_op;
	symtbl* _symbols;
	tree_node_t* _ast;

public:
	spe_region( varlist* v, varlist* d, varlist* r, string o, symtbl* s):
		_priv(v), _shared(d), _reductions(r), _reduction_op(o), _symbols(s)
	{
		assert(v);
		assert(d);
		assert(r);
		assert(s);
	}

	varlist*	priv()		const { return _priv; }
	varlist*	shared()	const { return _shared; }
	varlist&	in()		{ return _in; }
	varlist&	out()		{ return _out; }
	varlist&	inout()		{ return _inout; }
	varlist*	reductions()	const { return _reductions; }
	string		reduction_op()	const { return _reduction_op; }

	void ast(tree_node_t* a)
	{
		_ast = a;
	}

	tree_node_t* ast() const
	{
		assert(_ast);
		return _ast;
	}

	symtbl* symbols()
	{
		return _symbols;
	}
};

typedef list<spe_region*> spelist;

#endif	// SPE_REGION_H
