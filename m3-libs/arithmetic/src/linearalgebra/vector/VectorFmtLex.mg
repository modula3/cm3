GENERIC MODULE VectorFmtLex(RF,VR);
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
(*FROM NADefinitions IMPORT Error,Err;*)
IMPORT Wr,TextWr,Fmt AS F,Thread;
FROM FmtLexSupport IMPORT Precedence;

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
VAR
  wr:=TextWr.New();
BEGIN
  Wr.PutText(wr,"V" & F.Int(NUMBER(x^)) & "{");
  FOR i:=FIRST(x^) TO LAST(x^) DO
    Wr.PutText(wr,F.Pad(RF.Fmt(x[i],style.elemStyle),style.width));
    IF i#LAST(x^) THEN Wr.PutText(wr,", "); END;
  END;
  Wr.PutText(wr,"}\n");
  RETURN TextWr.ToText(wr);
END Fmt;

(*-----------------*)
PROCEDURE Tex (x : T; READONLY style := TexStyle{}; <*UNUSED*> within := Precedence.sum) : TEXT RAISES {Thread.Alerted, Wr.Failure} =
VAR
  wr:=TextWr.New();
  sep:TEXT;
BEGIN
  IF TexFlag.vertical IN style.flags THEN
    sep:=" \\\\\n";
    Wr.PutText(wr,"\\left(\\begin{array}{c}\n");
  ELSE
    sep:=style.sep;
    Wr.PutText(wr,"\\left(");
  END;
  FOR i:=FIRST(x^) TO LAST(x^) DO
    Wr.PutText(wr,RF.Tex(x[i],style.elemStyle,Precedence.sum));
    IF i#LAST(x^) THEN Wr.PutText(wr,sep); END;
  END;
  IF TexFlag.vertical IN style.flags THEN
    Wr.PutText(wr,"\\end{array}\\right)\n");
  ELSE
    Wr.PutText(wr,"\\right)\n");
  END;
  RETURN TextWr.ToText(wr);
END Tex;


(*-----------------*)
BEGIN
END VectorFmtLex.
