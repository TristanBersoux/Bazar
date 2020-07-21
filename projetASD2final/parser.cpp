#include <cctype>
#include <cstdlib>
#include <iterator>
#include <vector>
#include "parser.hpp"

typedef std::vector<std::string> T_STRING_VECTOR;

// decompose a line into a list of strings
void decompose(const std::string& line, T_STRING_VECTOR& symbols)
{
  int i = 0, n = line.length();
  std::string s = "";
  char c;

  while (i<n)
  {
    c = line[i];
    if (isspace(c))
    {
      if (s.length() > 0)
        symbols.push_back(s);

      s = "";
    }
    else
    {
      // append c at the end of s
      s.push_back(c);
    }
    i = i + 1;
  }
  if (s.length() > 0)
    symbols.push_back(s);
}

// Returns true if s contains only digits
bool is_an_integer(const std::string& s)
{
  int n = s.length();
  if (n == 0)
    return false;
  else
  {
    int i = 0;
    while ((i<n) && isdigit(s[i]))
      i = i + 1;

    return (i == n);
  }
}

// Returns true if s contains only letters
bool is_a_variable(const std::string& s)
{
  int n = s.length();
  if (n == 0)
    return false;
  else
  {
    int i = 0;
    while ((i<n) && isalpha(s[i]))
      i = i + 1;

    return (i == n);
  }
}

// Constructor
parser::parser():
  last_(TOKEN_ERROR), error_(""), var_(""), value_(0)
{}

// Parsing function
TOKEN parser::parse(const std::string& line)
{
  std::string s1, s2;

  // decompose the line into a list of symbols
  T_STRING_VECTOR symbols;
  decompose(line,symbols);
  
  int n = symbols.size();

  if (n == 0)
  {
    error_ = "empty line!";
    return last_ = TOKEN_ERROR;
  }

  // first symbol
  s1 = symbols[0];

  // open
  if (s1.compare("{") == 0)
  {
    if (n > 1)
    {
      error_ = "{ followed by another symbol!";
      return last_ = TOKEN_ERROR;
    }
    else
      return last_ = TOKEN_OPEN;
  }

  // close
  if (s1.compare("}") == 0)
  {
    if (n > 1)
    {
      error_ = "} followed by another symbol!";
      return last_ = TOKEN_ERROR;
    }
    else
      return last_ = TOKEN_CLOSE;
  }

  // print
  if (s1.compare(">") == 0)
  {
    if (n > 2)
    {
      error_ = "bad print instruction!";
      return last_ = TOKEN_ERROR;
    }
    else
    {
      s2 = symbols[1];

      if (is_a_variable(s2))
      {
	var_ = s2;
	return last_ = TOKEN_PRINT;
      }
      else
      {
	error_ = "bad variable name!";
	return last_ = TOKEN_ERROR;
      }
    }
  }

	// increment
	 if (s1.compare("+") == 0)
  {
    if (n > 2)
    {
      error_ = "bad print instruction!";
      return last_ = TOKEN_ERROR;
    }
    else
    {
      s2 = symbols[1];

      if (is_a_variable(s2))
      {
	var_ = s2;
	return last_ = TOKEN_ADD;
      }
      else
      {
	error_ = "bad variable name!";
	return last_ = TOKEN_ERROR;
      }
    }
  }

  // variable
  if (!is_a_variable(s1))
  {
    error_ = "bad instruction or variable name!";
    return last_ = TOKEN_ERROR;
  }
  else
  {
    if (n != 2)
    {
      error_ = "bad declaration of a variable!";
      return last_ = TOKEN_ERROR;
    }
    
    s2 = symbols[1];

    if (is_an_integer(s2))
    {
      var_ = s1;
      value_ = atoi(s2.c_str());
      return last_ = TOKEN_VAR;
    }
    else
    {
      error_ = "bad variable value!";
      return last_ = TOKEN_ERROR;
    }
  }
}

std::string parser::var() const
{
  if ((last_ == TOKEN_VAR) || (last_ == TOKEN_PRINT || last_ == TOKEN_ADD))
    return var_;
  else
    throw new std::logic_error("No variable name in the last line parsed!");
}

int parser::value() const
{
  if (last_ == TOKEN_VAR)
    return value_;
  else
    throw new std::logic_error("No variable value in the last line parsed!");
}

std::string parser::error() const
{
  if (last_ == TOKEN_ERROR)
    return error_;
  else
    throw new std::logic_error("No error in the last line!");
}
