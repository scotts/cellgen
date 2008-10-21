#ifndef IDS_H
#define IDS_H

#include <string>
#include <limits>
#include <utility>
using namespace std;

#include <boost/spirit/core/non_terminal/parser_id.hpp>
using namespace boost::spirit;

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
	static const int additive_expression = 22;
	static const int additive_expression_helper = 23;
	static const int init_declarator = 24;
	static const int declarator = 25;
	static const int pointer = 26;
	static const int declaration_specifiers = 27;
	static const int unary_expression = 28;
	static const int semicolon = 29;
	static const int last = 29;

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
		"semicolon"
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

