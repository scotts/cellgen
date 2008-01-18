#ifndef IDS_H
#define IDS_H

#include <limits>

namespace ids {
	static const int identifier = 1;
	static const int declaration_list = 2;
	static const int declaration = 3;
	static const int array_index = 4;
	static const int argument_expression_list = 5;
	static const int postfix_expression_helper = 6;
	static const int postfix_expression = 7;
	static const int statement_list = 8;
	static const int statement = 9;
	static const int relational_expression = 10;
	static const int assignment_expression = 11;
	static const int expression_helper = 12;
	static const int expression = 13;
	static const int expression_statement = 14;
	static const int for_loop = 15;
	static const int compound = 16;
	static const int cell_region = 17;
	static const int int_constant_dec = 18;
	static const int multiplicative_expression = 19;
	static const int multiplicative_expression_helper = 20;
	static const int selection_statement = 21;
	static const int any_rule = INT_MAX;
};

#endif	// IDS_H

