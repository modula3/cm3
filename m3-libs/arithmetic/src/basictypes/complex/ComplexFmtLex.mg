GENERIC MODULE ComplexFmtLex(R,RF);
(*Copyright (c) 1996, m3na project*)

IMPORT Rd, Wr, Thread;
IMPORT Fmt AS F;
IMPORT Lex AS L;
IMPORT FloatMode;
(*FROM NADefinitions IMPORT Error,Err;*)
FROM FmtLexSupport IMPORT Precedence, Parenthesize;

<*UNUSED*> CONST Module = "ComplexFmtLex.";

(*
PROCEDURE Lex(
               str:TEXT):C.T RAISES {Error}=
PROCEDURE Lex (rd: Rd.T; READONLY style : LexStyle; ): T RAISES {L.Error, FloatMode.Trap, Rd.Failure, Thread.Alerted}=BEGIN END Lex;

BEGIN
  RAISE Error(Err.not_implemented);
END Lex;
*)
PROCEDURE Fmt (READONLY x : T; READONLY style := FmtStyle{}) : TEXT =
(*Generate in format "COMPLEX{re:=1.0,im:=2.0}"
Uses simple F.Real if c.im=0.0.
style and precision can be overridden*)
VAR
  t:TEXT;
PROCEDURE Lex (rd: Rd.T; READONLY style : LexStyle; ): T RAISES {L.Error, FloatMode.Trap, Rd.Failure, Thread.Alerted}=BEGIN END Lex;

BEGIN
  IF R.IsZero(x.im) THEN
    t:=RF.Fmt(x.re,style.elemStyle);
  ELSE
    t:="Complex{re:=" & RF.Fmt(x.re,style.elemStyle) & ","
             & "im:=" & RF.Fmt(x.im,style.elemStyle) & "}";
  END;
  RETURN t;
END Fmt;

PROCEDURE Tex (READONLY x : T; READONLY style := TexStyle{}; within := Precedence.sum) : TEXT =
VAR
  t:TEXT;
PROCEDURE Lex (rd: Rd.T; READONLY style : LexStyle; ): T RAISES {L.Error, FloatMode.Trap, Rd.Failure, Thread.Alerted}=BEGIN END Lex;

BEGIN
  IF R.IsZero(x.im) THEN
    t:=RF.Tex(x.re,style.elemStyle,within);
  ELSIF R.IsZero(x.re) THEN
    t:=Parenthesize(RF.Tex(x.im,style.elemStyle,Precedence.product) & " i",
                    Precedence.product,within);
  ELSE
    t:=Parenthesize(RF.Tex(x.re,style.elemStyle,Precedence.sum) & " + " &
                    RF.Tex(x.im,style.elemStyle,Precedence.product) & " i",
                    Precedence.sum,within);
  END;
  RETURN t;
END Tex;

PROCEDURE Lex (rd: Rd.T; READONLY style : LexStyle; ): T RAISES {L.Error, FloatMode.Trap, Rd.Failure, Thread.Alerted}=BEGIN END Lex;

BEGIN
END ComplexFmtLex.
