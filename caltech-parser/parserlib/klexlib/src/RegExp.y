%start expr

%left '|'
%left concat_expr
%nonassoc '?' '+' '*' repeat_expr

expr:
  paren         '(' expr ')'
  concat        expr expr
  or            expr '|' expr
  plus          expr '+'
  star          expr '*'
  quest         expr '?'
  repeat        expr COUNT 
  ident         IDENTIFIER
  string        STRING
  charRange     CHAR_RANGE
