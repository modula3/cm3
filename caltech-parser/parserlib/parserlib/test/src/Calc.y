%left '+' '-'
%left '*' '/'
%nonassoc uminus_expr

%start list

list:
  empty
  cons		list stat '\n'

stat:
  eval		expr
  assign	LETTER ASSIGN expr

expr:
  paren		'(' expr ')'
  add		expr '+' expr
  sub		expr '-' expr
  mul		expr '*' expr
  div		expr '/' expr
  uminus	'-' expr
  ident		LETTER
  num		number

number:
  digit		DIGIT
  cons		number DIGIT
