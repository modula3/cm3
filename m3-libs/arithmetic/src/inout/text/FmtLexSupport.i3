INTERFACE FmtLexSupport;
(*Copyright (c) 1996, m3na project*)

(*Abstract: Support for FmtLex type modules*)

IMPORT Rd, Thread;
IMPORT Lex AS L;

TYPE
  Precedence = {sum              (* a-b is considered as a+b*(-1) *)
                , product        (* a/b is considered as a*b^(-1) *)
                , power          (* this refers to parentheses of the
                                    basis, the exponent can always live
                                    without parentheses*)
               };

PROCEDURE Parenthesize (t: TEXT; inner, outer: Precedence; ): TEXT;
(*t contains a mathematical expression, formatted in TeX; The main
   operation of the expression in t is of type 'inner' and we want to used
   it within a operation of type 'outer'.  Returns t enclosed in
   parentheses if necessary and t unmodified if parentheses are not
   required.*)

PROCEDURE AssertChar (rd: Rd.T; sep: CHAR; )
  RAISES {L.Error, Rd.Failure, Thread.Alerted};
(*Assert that the next non-space character is the separator 'sep' otherwise
   raise the Error.*)

PROCEDURE CheckChar (rd: Rd.T; sep: CHAR; ): BOOLEAN
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted};
(*Check that the next non-space character is the separator 'sep' and return
   TRUE in that case.*)

END FmtLexSupport.
