GENERIC MODULE PolynomialFmtLex(RF);
(*Copyright (c) 1995, Harry George
  
Abstract: Polynomials.

12/27/95  Harry George    Initial version
2/3/96    Harry George    Converted to m3na format.
2/17/96   Harry George    Converted from OO to ADT format.
*)

IMPORT Fmt AS F,Wr,TextWr,Thread;

<* UNUSED *>
CONST Module = "PolynomialFmtLex.";

(*--------------------*)
(*
PROCEDURE Lex( 
               str:TEXT):T =
BEGIN
  RAISE Error(Err.not_implemented);
END Lex;
*)

(*----------------------*)
PROCEDURE Fmt (x : T; READONLY style := FmtStyle{}):TEXT
               RAISES {Thread.Alerted, Wr.Failure} =
(*Generate a text object for the polynomial poly, in form:
 T3{a0,a1,a2}
*)
VAR
  n:=NUMBER(x^); nf:=FIRST(x^); nl:=LAST(x^);
  wr:=NEW(TextWr.T).init(); 
BEGIN
  Wr.PutText(wr,"Polynomial"
     & F.Int(n) & "{");
  FOR i:=nf TO nl DO
    Wr.PutText(wr,RF.Fmt(x[i],style.elemStyle));
    IF i#nl THEN Wr.PutText(wr,", "); END;
  END;
  Wr.PutText(wr,"}");
  RETURN TextWr.ToText(wr);
END Fmt;

(*==========================*)
BEGIN
END PolynomialFmtLex.
