#ifndef SPE_REGION_H
#define SPE_REGION_H

#include "variable.h"
#include "parse_tree.h"

class spe_region {
	cvarlist_t* _priv;
	cvarlist_t* _shared;
	cvarlist_t* _reductions;
	string _reduction_op;
	stringstream* _text;
	tree_node_t* _ast;

public:
	spe_region(
		cvarlist_t* v, 
		cvarlist_t* d, 
		stringstream* t, 
		cvarlist_t* r,
		string o):
		_priv(v), _shared(d), _reductions(r), _reduction_op(o), _text(t)
	{
		assert(v);
		assert(d);
		assert(t);
		assert(r);
	}

	cvarlist_t*	priv()		const { return _priv; }
	cvarlist_t*	shared()	const { return _shared; }
	stringstream*	text()		const { return _text; }
	cvarlist_t*	reductions()	const { return _reductions; }
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
};

typedef list<spe_region*>	spelist_t;

#endif	// SPE_REGION_H
