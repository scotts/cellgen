#ifndef IDS_H
#define IDS_H

#include <string>
#include <limits>
#include <utility>
using namespace std;

#include <boost/spirit/core/non_terminal/parser_id.hpp>
using namespace boost::spirit;

namespace ids {
	const int identifier = 1;
	const int declaration_list = 2;
	const int declaration = 3;
	const int array_index = 4;
	const int argument_expression_list = 5;
	const int postfix_expression_helper = 6;
	const int postfix_expression = 7;
	const int statement_list = 8;
	const int statement = 9;
	const int relational_expression = 10;
	const int assignment_expression = 11;
	const int expression_helper = 12;
	const int expression = 13;
	const int expression_statement = 14;
	const int for_loop = 15;
	const int compound = 16;
	const int cell_region = 17;
	const int int_constant_dec = 18;
	const int multiplicative_expression = 19;
	const int multiplicative_expression_helper = 20;
	const int selection_statement = 21;
	const int additive_expression = 22;
	const int additive_expression_helper = 23;
	const int init_declarator = 24;
	const int declarator = 25;
	const int pointer = 26;
	const int declaration_specifiers = 27;
	const int unary_expression = 28;
	const int semicolon = 29;
	const int dot = 30;
	const int ptr_op = 31;
	const int float_constant_1 = 32;
	const int float_constant_2 = 33;
	const int float_constant_3 = 34;
	const int free_compound = 35;
	const int last = 35; // should equal one above

	static const string __mappings[] = { "",
		"identifier",
		"declaration_list",
		"declaration",
		"array_index",
		"argument_expression_list",
		"postfix_expression_helper",
		"postfix_expression",
		"statement_list",
		"statement",
		"relational_expression",
		"assignment_expression",
		"expression_helper",
		"expression",
		"expression_statement",
		"for_loop",
		"compound",
		"cell_region",
		"int_constant_dec",
		"multiplicative_expression",
		"multiplicative_expression_helper",
		"selection_statement",
		"additive_expression",
		"additive_expression_helper",
		"init_declarator",
		"declarator",
		"pointer",
		"declaration_specifiers",
		"unary_expression",
		"semicolon",
		"dot",
		"ptr_op",
		"float_constant_1",
		"float_constant_2",
		"float_constant_3",
		"free_compound"
	};
	static const vector<string> mappings(__mappings, __mappings + last + 1);

	inline string rule_string(const parser_id& i)
	{
		string rule;
		if (i.to_long() < mappings.size()) {
			rule = mappings[i.to_long()];
		}
		else {
			rule = to_string(i.to_long());
		}

		return rule;
	}
};

#endif	// IDS_H

