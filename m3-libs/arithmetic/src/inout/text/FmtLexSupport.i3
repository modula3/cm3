INTERFACE FmtLexSupport;
(*Copyright (c) 1996, m3na project

Abstract: Support for FmtLex type modules

*)

(*==========================*)

TYPE
  Precedence =
    {sum     (* a-b is considered as a+b*(-1) *)
    ,product (* a/b is considered as a*b^(-1) *)
    ,power   (* this refers to parentheses of the basis,
                the exponent can always live without parentheses*)
    };

PROCEDURE Parenthesize (t : TEXT; inner, outer : Precedence) : TEXT;
(*t contains a mathematical expression, formatted in TeX;
  The main operation of the expression in t
  is of type 'inner' and
  we want to used it within a operation of type 'outer'.
  Returns t enclosed in parentheses if necessary
  and t unmodified if parentheses are not required.*)

(*==========================*)
END FmtLexSupport.
