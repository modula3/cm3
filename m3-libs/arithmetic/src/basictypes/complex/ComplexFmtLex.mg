GENERIC MODULE ComplexFmtLex(C,R,RF);
(*Copyright (c) 1996, m3na project

Abstract: Formatting and parsing complex numbers

12/13/95  Harry George    Initial version
1/27/96   Harry George    Converted to m3na format
2/3/96    Harry George    Added trancendentals
2/17/96   Harry George    Converted from Objects to ADT's
3/16/96   Warren Smith    Improved routines, and new routines.
                          The ones with beginning caps are wds's
*)
(*FROM xUtils IMPORT Error,Err;*)
IMPORT Fmt AS F;

<*UNUSED*> CONST Module = "ComplexFmtLex.";

(*------------------------*)
(*
PROCEDURE Lex( 
               str:TEXT):C.T RAISES {Error}=
BEGIN
  RAISE Error(Err.not_implemented);
END Lex;
*)
(*----------------------------*)
PROCEDURE Fmt ( 
                VALUE c:C.T;
                style:F.Style:=F.Style.Fix;
                prec:CARDINAL:=3
                ):TEXT=
(*Generate in format "COMPLEX{re:=1.0,im:=2.0}"
Uses simple F.Real if c.im=0.0.
style and precision can be overridden*)
VAR
  t:TEXT;
BEGIN
  IF R.Equal(c.im,R.Zero) THEN
    t:=RF.Fmt(c.re);
    RETURN t;
  ELSE
	t:="COMPLEX{re:=" & RF.Fmt(c.re,style,prec) & "D0,"
             & "im:=" & RF.Fmt(c.im,style,prec) & "D0}";
	RETURN t;
  END;
END Fmt;

(*==========================*)
BEGIN
END ComplexFmtLex.
