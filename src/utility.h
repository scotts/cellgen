#ifndef UTILITY_H
#define UTILITY_H

#include <algorithm>
#include <numeric>
#include <list>
#include <set>
#include <string>
#include <sstream>
#include <boost/bind.hpp>
#include <boost/mem_fn.hpp>
using namespace std;
using namespace boost;

struct user_error {
	const string err;
	user_error(const string& e): err(e) {}
};

template <class I, class F>
F* for_each(I first, I last, F* f)
{
	for ( ; first != last; ++first) {
		(*f)(*first);
	}
	return f;
}

template <class C, class F>
F for_all(const C& c, F f)
{
	return for_each(c.begin(), c.end(), f);
}

template <class C, class F>
F for_all(C& c, F f)
{
	return for_each(c.begin(), c.end(), f);
}

template <class C, class P>
typename C::const_iterator find_if_all(const C& c, P p)
{
	return find_if(c.begin(), c.end(), p);
}

template <class C, class P>
typename C::iterator find_if_all(C& c, P p)
{
	return find_if(c.begin(), c.end(), p);
}

template <class C, class T, class F>
T accumulate_all(const C& c, T init, F f)
{
	return accumulate(c.begin(), c.end(), init, f);
}

template <class C, class T, class F>
T accumulate_all(C* c, T init, F f)
{
	return accumulate(c->begin(), c->end(), init, f);
}

template <class O, class T>
T inv_accumulate_all(const list<O>& ops, T init)
{
	for (typename list<O>::const_iterator i = ops.begin(); i != ops.end(); ++i) {
		init = ((*i))(init);
	}
	return init;
}

template <class O, class T>
T inv_accumulate_all(const list<O*>& ops, T init)
{
	for (typename list<O*>::const_iterator i = ops.begin(); i != ops.end(); ++i) {
		init = (*(*i))(init);
	}
	return init;
}

template <class C, class O>
O copy_all(C& c, O o)
{
	return copy(c.begin(), c.end(), o);
}

template <class F, class Sequence>
list<typename F::result_type> fmap(F f, const Sequence& in)
{
	list<typename F::result_type> out;
	for (typename Sequence::const_iterator i = in.begin(); i != in.end(); ++i) {
		out.push_back(f(*i));
	}
	return out;
}

template <class Sequence, class F>
list<typename F::value_type> fmap(F f, Sequence* in)
{
	return fmap(f, *in);
}

template <class Pred, class Sequence>
Sequence filter(Pred p, const Sequence& in)
{
	Sequence out; 
	for (typename Sequence::const_iterator i = in.begin(); i != in.end(); ++i) {
		if (p(*i)) {
			out.insert(out.end(), *i);
		}
	}
	return out;
}

template <class L1, class L2>
void append(L1& l1, const L2& l2)
{
	l1.insert(l1.end(), l2.begin(), l2.end());
}

template <class T>
void delete_ptr(T* ptr)
{
	delete ptr;
}

template <class Test, class X>
bool is_type(X* t)
{
	return dynamic_cast<Test*>(t) != NULL;
}

template <class I, class C>
I next(I i, const C& c)
{
	I nxt = i;
	++nxt;

	if (nxt != c.end()) {
		return nxt;
	}

	return i;
}

template <class I, class C>
I previous(I i, const C& c)
{
	if (i == c.begin()) {
		return i;
	}

	return --i;
}

template <class Container1, class Container2, class Out, class Comp>
Out set_intersection_all(const Container1& c1, const Container2& c2, Out o, Comp co)
{
	return set_intersection(c1.begin(), c1.end(), c2.begin(), c2.end(), o, co);
}

template <class Container1, class Container2, class Out>
Out set_intersection_all(const Container1& c1, const Container2& c2, Out o)
{
	return set_intersection(c1.begin(), c1.end(), c2.begin(), c2.end(), o);
}

template <class Container1, class Container2, class Out>
Out set_difference_all(const Container1& c1, const Container2& c2, Out o)
{
	return set_difference(c1.begin(), c1.end(), c2.begin(), c2.end(), o);
}

template <class Container1, class Container2, class Out, class Comp>
Out set_difference_all(const Container1& c1, const Container2& c2, Out o, Comp co)
{
	return set_difference(c1.begin(), c1.end(), c2.begin(), c2.end(), o, co);
}

template <class Container1, class Container2, class Out>
Out set_union_all(const Container1& c1, const Container2& c2, Out o)
{
	return set_union(c1.begin(), c1.end(), c2.begin(), c2.end(), o);
}

template <class Container>
Container set_union_all(const Container& c1, const Container& c2)
{
	Container u;
	set_union_all(c1, c2, inserter(u, u.begin()));
	return u;
}

template <class Container1, class Container2, class Container3, class Out>
Out set_union_all(const Container1& c1, const Container2& c2, const Container3& c3, Out o)
{
	typename Out::container_type temp1;
	set_union_all(c1, c2, inserter(temp1, temp1.begin()));
	return set_union_all(temp1, c3, o);
}

template <class Container>
Container set_union_all(const Container& c1, const Container& c2, const Container& c3)
{
	Container u;
	set_union_all(c1, c2, c3, inserter(u, u.begin()));
	return u;
}

template <class Key, class Val, class Set>
struct assign_set {
	Val val;
	Set& set;
	assign_set(Val v, Set& s): val(v), set(s) {}

	void operator()(const Key k)
	{
		set[k] = val;
	}
};

template <class Key, class Val, class Set>
assign_set<Key, Val, Set> make_assign_set(Val v, Set& s)
{
	return assign_set<Key, Val, Set>(v, s);
}

template <class T>
struct erase_from_set: unary_function<T, void> {
	set<T>& container;
	erase_from_set(set<T>& c): container(c) {}
	void operator()(T value)
	{
		container.erase(value);
	}
};

template <class T, class Container>
bool exists_in(const Container& c, const T& val)
{
	return find_if(c.begin(), c.end(), boost::bind(equal_to<T>(), val, _1)) != c.end();
}

template <class T, class Container, class Pred>
bool exists_in(const Container& c, const T& val, Pred p)
{
	return find_if(c.begin(), c.end(), boost::bind(p, val, _1)) != c.end();
}

template <class T>
struct fn_and: public unary_function<bool, const T*> {
	bool (T::*func1)() const;
	bool (T::*func2)() const;
	fn_and(bool (T::*f1)() const, bool (T::*f2)() const): 
		func1(f1), func2(f2)
		{}
	bool operator()(const T* v) const
	{
		return (v->*func1)() && (v->*func2)();
	}
};

template <class T>
fn_and<T> make_fn_and(bool (T::*f1)() const, bool (T::*f2)() const)
{
	return fn_and<T>(f1, f2);
}

template <class T>
struct acc_or: public binary_function<bool, bool, const T*> {
	bool (T::*func)() const;
	acc_or(bool (T::*f)() const): func(f) {}

	bool operator()(bool init, const T* v)
	{
		return init || (v->*func)();
	}
};

template <class T>
acc_or<T> make_acc_or(bool (T::*f)() const)
{
	return acc_or<T>(f);
}

template <class T>
T from_string(const string& str)
{
	stringstream ss;
	ss << str;
	T temp = 0;
	ss >> temp;
	return temp;
}

template <class T>
string to_string(T t)
{
	stringstream ss;
	ss << t;
	return ss.str();
}

#endif // UTILITY_H

