#ifndef SPE_REGION_H
#define SPE_REGION_H

class spe_region;
typedef list<spe_region*> spelist;

#include "variable.h"
#include "parse_tree.h"

class spe_region {
	privset _priv;
	sharedset _shared;
	reduceset _reductions;
	string _reduction_op;
	shared_symtbl _shared_symbols;
	priv_symtbl _priv_symbols;
	ast_node* _ast_root;
	string _estimate;
	int _buffer;

public:
	spe_region(const privset& p, const sharedset& sh, const reduceset& r, const string& o, const shared_symtbl& ss, const priv_symtbl& ps, int b):
		_priv(p), _shared(sh), _reductions(r), _reduction_op(o), _shared_symbols(ss), _priv_symbols(ps), _buffer(b)
	{
		assert(_buffer >= 0);
		if (_reductions.size() > 0) {
			assert(_reduction_op != "");
		}
	}

	privset& priv()			{ return _priv; }
	sharedset& shared()		{ return _shared; }
	reduceset& reductions()		{ return _reductions; }
	shared_symtbl& shared_symbols()	{ return _shared_symbols; }
	priv_symtbl& priv_symbols()	{ return _priv_symbols; }

	string reduction_op() const	{ return _reduction_op; }
	int buffer() const		{ return _buffer; }

	void estimate(const string& e) { _estimate = e; }
	string estimate() const { return _estimate; }

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
