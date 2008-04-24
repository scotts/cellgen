#ifndef UTILITY_H
#define UTILITY_H

#include <algorithm>
#include <numeric>
#include <list>
#include <string>
#include <sstream>
using namespace std;

template <class I, class F>
F* for_each(I first, I last, F* f)
{
	for ( ; first != last; ++first) {
		(*f)(*first);
	}
	return f;
}

template <class C, class F>
F for_all(C& c, F f)
{
	return for_each(c.begin(), c.end(), f);
}

template <class C, class F>
F for_all(C* c, F f)
{
	return for_each(c->begin(), c->end(), f);
}

template <class C, class T, class F>
T accumulate_all(C& c, T init, F f)
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

template <class C, class F, class P>
typename C::iterator for_all_duplicate(C& c, F f, P pred)
{
	for (typename C::iterator i = c.begin(); i != c.end(); ++i) {
		f(*i);
		if (pred(*i)) {
			return c.insert(i, *i);
		}
	}

	return c.end();
}

template <class C, class F, class P>
typename C::iterator for_all_duplicate(C& c, F* f, P pred)
{
	return for_all_duplicate(c, *f, pred);
}

template <class S, class F>
list<typename F::result_type> fmap(F f, const S& seq)
{
	list<typename F::result_type> lst;
	for (typename S::const_iterator i = seq.begin(); i != seq.end(); ++i) {
		lst.push_back(f(*i));
	}
	return lst;
}

template <class S, class F>
list<typename F::result_type> fmap(F f, S* seq)
{
	return fmap(f, *seq);
}

template <class L>
void append(L& l1, const L& l2)
{
	l1.insert(l1.end(), l2.begin(), l2.end());
}

template <class T>
list<T> cons(T t, const list<T>& l)
{
	return list<T>(l).push_back(t);
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

template <class I>
I next(I i)
{
	return ++i;
}

template <class Container1, class Container2, class Out, class Comp>
Out set_intersection_all(const Container1& c1, const Container2& c2, Out o, Comp co)
{
	return set_intersection(c1.begin(), c1.end(),
				c2.begin(), c2.end(),
				o, co);
}

template <class Container1, class Container2, class Out>
Out set_intersection_all(const Container1& c1, const Container2& c2, Out o)
{
	return set_intersection(c1.begin(), c1.end(),
				c2.begin(), c2.end(),
				o);
}

template <class Container1, class Container2, class Out, class Comp>
Out set_intersection_all(Container1* c1, Container2* c2, Out o, Comp co)
{
	return set_intersection(c1->begin(), c1->end(),
				c2->begin(), c2->end(),
				o, co);
}

template <class Container1, class Container2, class Out>
Out set_difference_all(const Container1& c1, const Container2& c2, Out o)
{
	return set_difference(c1.begin(), c1.end(),
				c2.begin(), c2.end(),
				o);
}

template <class Container1, class Container2, class Out, class Comp>
Out set_difference_all(const Container1& c1, const Container2& c2, Out o, Comp co)
{
	return set_difference(c1.begin(), c1.end(),
				c2.begin(), c2.end(),
				o, co);
}

template <class Container1, class Container2, class Out>
Out set_difference_all(Container1* c1, Container2* c2, Out o)
{
	return set_difference(c1->begin(), c1->end(),
				c2->begin(), c2->end(),
				o);
}

template <class Container1, class Container2, class Out>
Out set_union_all(const Container1& c1, const Container2& c2, Out o)
{
	return set_union(c1.begin(), c1.end(),
			c2.begin(), c2.end(),
			o);
}

template <class Container1, class Container2, class Container3, class Out>
Out set_union_all(const Container1& c1, const Container2& c2, const Container3& c3, Out o)
{
	typename Out::container_type temp1;
	set_union_all(c1, c2, inserter(temp1, temp1.begin()));
	return set_union_all(temp1, c3, o);
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

