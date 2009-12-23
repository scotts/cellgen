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
	pt_node* _pt_root;
	string _estimate;
	string _buffer;

public:
	spe_region(const privset& p, const sharedset& sh, const reduceset& r, const string& o, const shared_symtbl& ss, const priv_symtbl& ps, const string& b):
		_priv(p), _shared(sh), _reductions(r), _reduction_op(o), _shared_symbols(ss), _priv_symbols(ps), _buffer(b)
	{
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
	string buffer() const		{ return _buffer; }

	void estimate(const string& e) { _estimate = e; }
	string estimate() const { return _estimate; }

	void pt_root(pt_node* a)
	{
		assert(a);
		_pt_root = a;
	}

	pt_node* pt_root() const
	{
		assert(_pt_root);
		return _pt_root;
	}
};

#endif	// SPE_REGION_H
