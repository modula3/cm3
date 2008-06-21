(* Copyright 1995-96 Critical Mass, Inc. All rights reserved.    *)

(* "RegExpr" provides regular expression matching of text strings. *)

INTERFACE RegExpr;

TYPE T <: REFANY;
(* The special expression "NIL" matches every string. *)

PROCEDURE Compile (pattern: TEXT): T  RAISES {Error};
(* Translate the string "pattern" and return its internal form. *)

PROCEDURE Match (t: T;  txt: TEXT): BOOLEAN;
(* Returns "TRUE" if "txt" matches the pattern "t". *)

PROCEDURE MatchSub (t: T;  READONLY str: ARRAY OF CHAR): BOOLEAN;
(* Returns "TRUE" if "str" matches the pattern "t". *)

PROCEDURE MatchSubstring (t: T;  txt: TEXT;  pre, post: CARDINAL): BOOLEAN;
(* Returns "TRUE" if "a" matches the pattern "t" when the first "pre"
   characters and last "post" characters are dropped. *)

PROCEDURE SimpleString (t: T): TEXT;
(* Returns the string that matches "t" if it contains no non-trivial
   regular expression operators.  Otherwise, returns NIL.  *)

EXCEPTION Error (TEXT);

END RegExpr.

(* The pattern grammar is:

|    pattern             matches
|    -------------       ------------------------------------
|    <expr1>|<expr2>     strings matching <expr1> or <expr2>
|    <expr1>&<expr2>     strings matching <expr1> and <expr2>
|    (expr)              strings matching <expr>
|    *                   any string of zero or more characters
|    @                   any single character
|    \*                  the single character '*' (asterisk)
|    \@                  the single character '@' (at sign)
|    \|                  the single character '|' (vertical bar)
|    \&                  the single character '&' (ampersand)
|    \(                  the single character '(' (left parenthesis)
|    \)                  the single character ')' (right parenthesis)
|    \\                  the single character '\' (back slash)
|    <char>              any other single character not mentioned above.

   "&" has higher precedence than "|".

*)

