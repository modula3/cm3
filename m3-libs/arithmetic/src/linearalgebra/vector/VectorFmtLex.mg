GENERIC MODULE VectorFmtLex(RF);
(*
Abstract:

6/6/87    hgeorge
          Initial version.

2/11/89   hgeorge
          To work with generic matrices.

11/20/94  Harry George
          Converted to Modula3 dynamic arrays.

12/18/95  Harry George
          ...and back to fully instantiated for REAL32.

1/27/96   Harry George
          Converted to OO format, and R.T

2/17/96   Harry George   Converted from OO to ADT format
*)
(*
FROM xUtils IMPORT Error,Err;
*)
IMPORT Wr,TextWr,Fmt AS F,Thread;

<*UNUSED*> CONST Module = "VectorFmt.";

(*-----------------*)
(*
PROCEDURE Lex(
               str:TEXT):T =
BEGIN
  RAISE Error(Err.not_implemented);
END Lex;
*)
(*-----------------*)
PROCEDURE Fmt (x : T; READONLY style := FmtStyle{}):TEXT
               RAISES {Thread.Alerted, Wr.Failure} =
CONST width = 12;
VAR
  wr:=TextWr.New();
BEGIN
  Wr.PutText(wr,"V" & F.Int(NUMBER(x^)) & "{");
  FOR i:=FIRST(x^) TO LAST(x^) DO
    Wr.PutText(wr,F.Pad(RF.Fmt(x[i],style.elemStyle),width));
    IF i#LAST(x^) THEN Wr.PutText(wr,", "); END;
  END;
  Wr.PutText(wr,"}\n");
  RETURN TextWr.ToText(wr);
END Fmt;


(*-----------------*)
BEGIN
END VectorFmtLex.
