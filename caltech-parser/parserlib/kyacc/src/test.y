%start list blah

%left '|'
%nonassoc concat_expr
%right '?'
%right '+' 
%right '*'
%right repeat_expr

A:
  ab		'a' 'b'

B:
  b		'b'

blah:
  abc		A 'c'
  abd		'a' B 'd'

list:
  empty
  cons		list stat '\n'

stat:
  macro		MACRO expr
  rule		RULE expr

expr:
  paren		'(' expr ')'
  concat	expr expr
  or		expr '|' expr
  plus		expr '+'
  otherplus	expr '+'
  star		expr '*'
  quest		expr '?'
  repeat	expr COUNT 
  ident		IDENTIFIER
  string	STRING
  charRange	CHAR_RANGE

loop:
  loop		loop

unused:
  a		PROD 'a' ASSIGN