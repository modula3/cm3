GENERIC MODULE FractionFmtLex(RF);
(*Copyright (c) 1996, m3na project

Abstract: Formatting and parsing fraction numbers

12/13/95  Harry George    Initial version
1/27/96   Harry George    Converted to m3na format
2/3/96    Harry George    Added trancendentals
2/17/96   Harry George    Converted from Objects to ADT's
3/16/96   Warren Smith    Improved routines, and new routines.
                          The ones with beginning caps are wds's
*)
(*FROM xUtils IMPORT Error,Err;*)

<*UNUSED*> CONST Module = "FractionFmtLex.";

(*------------------------*)
(*
PROCEDURE Lex( 
               str:TEXT):Fr.T RAISES {Error}=
BEGIN
  RAISE Error(Err.not_implemented);
END Lex;
*)
(*----------------------------*)
PROCEDURE Fmt (READONLY x : T; READONLY style := FmtStyle{}) : TEXT =
VAR
  t:TEXT;
BEGIN
  t:="FRACTION{n:=" & RF.Fmt(x.n,style.elemStyle) & ","
            & "d:=" & RF.Fmt(x.d,style.elemStyle) & "}";
	RETURN t;
END Fmt;

(*==========================*)
BEGIN
END FractionFmtLex.
