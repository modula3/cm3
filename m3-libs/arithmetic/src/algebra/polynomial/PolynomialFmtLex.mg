GENERIC MODULE PolynomialFmtLex(P,Rf);
(*Copyright (c) 1995, Harry George
  
Abstract: Polynomials.

12/27/95  Harry George    Initial version
2/3/96    Harry George    Converted to m3na format.
2/17/96   Harry George    Converted from OO to ADT format.
*)

FROM P IMPORT T;
IMPORT Fmt AS F,Wr,TextWr,Thread;

<* UNUSED *>
CONST Module = "PolynomialFmtLex.";

(*--------------------*)
(*
<* UNUSED *>
PROCEDURE Lex( 
               str:TEXT):T =
BEGIN
  RAISE Error(Err.not_implemented);
END Lex;
*)

(*----------------------*)
PROCEDURE Fmt( 
               p:T;
               style:F.Style:=F.Style.Fix;
               prec:CARDINAL:=1
               ):TEXT RAISES {Thread.Alerted, Wr.Failure} =
(*Generate a text object for the polynomial poly, in form:
 T3{a0,a1,a2}
*)
VAR
  n:=NUMBER(p^); n1:=0; nn:=n-1;
  wr:=NEW(TextWr.T).init(); 
BEGIN
  Wr.PutText(wr,"T"
     & F.Int(n) & "{");
  FOR i:=n1 TO nn DO
    Wr.PutText(wr,Rf.Fmt(p[i],style,prec));
    IF i#nn THEN Wr.PutText(wr,", "); END;
  END;
  Wr.PutText(wr,"}");
  RETURN TextWr.ToText(wr);
END Fmt;

(*==========================*)
BEGIN
END PolynomialFmtLex.
