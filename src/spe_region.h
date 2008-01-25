#ifndef SPE_REGION_H
#define SPE_REGION_H

#include "variable.h"
#include "parse_tree.h"

class spe_region {
	cvarlist* _priv;
	cvarlist* _shared;
	cvarlist _in;		// _in + _out + _inout = _shared
	cvarlist _out;
	cvarlist _inout;
	cvarlist* _reductions;
	string _reduction_op;
	stringstream* _text;
	tree_node_t* _ast;

public:
	spe_region(
		cvarlist* v, 
		cvarlist* d, 
		stringstream* t, 
		cvarlist* r,
		string o):
		_priv(v), _shared(d), _reductions(r), _reduction_op(o), _text(t)
	{
		assert(v);
		assert(d);
		assert(t);
		assert(r);
	}

	cvarlist*	priv()		const { return _priv; }
	cvarlist*	shared()	const { return _shared; }
	cvarlist&	in()		{ return _in; }
	cvarlist&	out()		{ return _out; }
	cvarlist&	inout()		{ return _inout; }
	stringstream*	text()		const { return _text; }
	cvarlist*	reductions()	const { return _reductions; }
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

typedef list<spe_region*>	spelist;

#endif	// SPE_REGION_H
