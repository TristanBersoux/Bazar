#ifndef PARSER_HPP
#define PARSER_HPP

#include <exception>
#include <string>

/* Token type */
enum TOKEN { TOKEN_OPEN, TOKEN_CLOSE, TOKEN_VAR, TOKEN_PRINT, TOKEN_ERROR, TOKEN_ADD };

/* Parser of a line of the asd2 language */
class parser {
public:
  // constructor
  parser();

  /* Parsing function that returns a token given a line of a program:
     - T_OPEN for an opening brace {
     - T_CLOSE for a closing brace }
     - T_VAR for the declaration of a variable of the form 'var value'
     - T_PRINT for a printing instruction of the form '> var'
     - T_ERROR for a parsing error 
     - T_ADD for the incrementation of a variable of the form '+ var'*/
  TOKEN parse(const std::string& line);

  /* Returns the variable name from the last call to parse
     if it returned T_VAR or T_PRINT; throws an exception
     std::logic_error() otherwise */
  std::string var() const;

  /* Returns the variable value from the last call to parse
     if it returned T_VAR ; throws an exception
     std::logic_error() otherwise */
  int value() const;

  /* Returns the error message from the last call to parse
     if it returned T_ERROR ; throws an exception
     std::logic_error() otherwise */
  std::string error() const;

private:
  TOKEN last_;         // last token parsed
  std::string error_;  // error message
  std::string var_;    // variable name
  int value_;          // variable value
};

#endif
