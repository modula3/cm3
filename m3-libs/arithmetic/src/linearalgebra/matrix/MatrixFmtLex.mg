GENERIC MODULE MatrixFmtLex(RF);
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
          Converted to OO format and R.T

2/17/96   Harry George   ...and back to ADT format
*)

IMPORT Wr,TextWr,Fmt AS F,Thread;

<*UNUSED*>
CONST Module = "MatrixFmtLex.";

(*-----------------*)
(*
PROCEDURE Lex(
               str:TEXT):T RAISES {Error}=
BEGIN
  RAISE Error(Err.not_implemented);
END Lex;
*)

(*-----------------*)
PROCEDURE Fmt (x : T; READONLY style := FmtStyle{}):TEXT
               RAISES {Thread.Alerted, Wr.Failure} =
VAR
  m:=NUMBER(x^);    mf:=FIRST(x^);   ml:=LAST(x^);
  n:=NUMBER(x[0]);  nf:=FIRST(x[0]); nl:=LAST(x[0]);
  wr:=TextWr.New();
BEGIN
  Wr.PutText(wr,"M" & F.Int(m) & "x" & F.Int(n) & "{\n");
  FOR i:=mf TO ml DO
    Wr.PutText(wr,"V" & F.Int(n) & "{");
    FOR j:= nf TO nl DO
      Wr.PutText(wr,F.Pad(RF.Fmt(x[i,j],style.elemStyle),style.width));
      IF j#nl THEN Wr.PutText(wr,", "); END;
    END;
    Wr.PutText(wr,"}");
    IF i#ml THEN Wr.PutText(wr,","); END;
    Wr.PutText(wr,"\n");
  END;
  Wr.PutText(wr,"}\n");
  RETURN TextWr.ToText(wr);
END Fmt;

(*-----------------*)
BEGIN
END MatrixFmtLex.
