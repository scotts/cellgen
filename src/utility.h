#define UTILITY_H
#define UTILITY_H

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

template <class F, class T, class C>
T accumulate_all(C& c, T init, F f)
{
	return accumlate(c.begin(), c.end(), init, f);
}

template <class F, class T, class C>
T accumulate_all(C* c, T init, F f)
{
	return accumulate(c->begin(), c->end(), init, f);
}

template <class O, class T, class A>
T inv_accumulate_all(O ops, T init, A& atom)
{
	for (typename O::iterator it = ops.begin(); it != ops.end(); ++it) {
		init += (*(*it))(atom);
	}
	return init;
}

template <class P, class C, class F>
typename C::iterator for_all_duplicate(P pred, F f, C& c)
{
	typename C::value_type dupe;
	typename C::iterator res = c.end();
	for (typename C::iterator it = c.begin(); it != c.end(); ++it) {
		if (pred(*it)) {
			dupe = *it;
			res = it;
		}
		f(*it);
	}
	res = c.insert(res, dupe);

	return res;
}

template <class P, class C, class F>
typename C::iterator for_all_duplicate(P pred, F* f, C& c)
{
	return for_all_duplicate(pred, *f, c);
}

template <class S, class F>
list<typename F::result_type> fmap(F f, S seq)
{
	list<typename F::result_type> lst;
	for (typename S::iterator it = seq.begin(); it != seq.end(); ++it) {
		lst.splice(lst.end(), f(*it));
	}
	return lst;
}

template <class T>
list<T> cons(T t, const list<T>& l)
{
	return list<T>(l).push_back(t);
}

template <class L>
void append(const L& l1, const L& l2)
{
	l1.splice(l1.end(), l2);
}

#endif // UTILITY_H

