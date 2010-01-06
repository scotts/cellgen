#ifndef CONDITIONS_H
#define CONDITIONS_H

#include <list>
#include <string>
using namespace std;

struct conditions {
	string start;
	string induction;
	string stop;
	string step;

	conditions()
		{}
	conditions(const string& _start, const string& _induction, const string& _stop, const string& _step):
		start(_start), induction(_induction), stop(_stop), step(_step)
		{}

	// Not using the step. I should, but I need to do some semantic analysis to make sure that 
	// "++i" == "i++".
	bool operator==(const conditions& o) const
	{
		return start == o.start && induction == o.induction && stop == o.stop;
	}

	string to_string() const
	{
		return "(" + start + " " + induction + " " + stop + " " + step + ") ";
	}
};
typedef list<conditions> condslist;

#endif // CONDITIONS_H

