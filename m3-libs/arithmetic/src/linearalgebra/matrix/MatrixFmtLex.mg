GENERIC MODULE MatrixFmtLex(RF);
(*Copyright (c) 1996, m3na project*)

IMPORT Rd, Wr, TextWr, Thread;
IMPORT Fmt AS F;
IMPORT Lex AS L;
IMPORT FloatMode;

<*UNUSED*>
CONST Module = "MatrixFmtLex.";

PROCEDURE Fmt (x : T; READONLY style := FmtStyle{}):TEXT
               RAISES {Thread.Alerted, Wr.Failure} =
VAR
  m:=NUMBER(x^);    mf:=FIRST(x^);   ml:=LAST(x^);
  n:=NUMBER(x[0]);  nf:=FIRST(x[0]); nl:=LAST(x[0]);
  wr:=TextWr.New();
PROCEDURE Lex (rd: Rd.T; READONLY style : LexStyle; ): T RAISES {L.Error, FloatMode.Trap, Rd.Failure, Thread.Alerted}=BEGIN END Lex;

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

PROCEDURE Dup (str:TEXT; n:CARDINAL):TEXT
               RAISES {Thread.Alerted, Wr.Failure} =
  VAR
    wr:=TextWr.New();
  BEGIN
    FOR j:=0 TO n-1 DO
      Wr.PutText(wr,str);
    END;
    RETURN TextWr.ToText(wr);
  END Dup;

PROCEDURE Tex (x : T; READONLY style := TexStyle{}):TEXT
               RAISES {Thread.Alerted, Wr.Failure} =
VAR
  wr:=TextWr.New();
PROCEDURE Lex (rd: Rd.T; READONLY style : LexStyle; ): T RAISES {L.Error, FloatMode.Trap, Rd.Failure, Thread.Alerted}=BEGIN END Lex;

BEGIN
  Wr.PutText(wr,"\\left(\\begin{array}{");
  Wr.PutText(wr,Dup("c",NUMBER(x[0])));
  Wr.PutText(wr,"}\n");
  FOR i:=FIRST(x^) TO LAST(x^) DO
    FOR j:=FIRST(x[i]) TO LAST(x[i]) DO
      Wr.PutText(wr,RF.Tex(x[i,j],style.elemStyle));
      IF j<LAST(x[i]) THEN Wr.PutText(wr," & "); END;
    END;
    Wr.PutText(wr," \\\\\n");
  END;
  Wr.PutText(wr,"\\end{array}\\right)\n");
  RETURN TextWr.ToText(wr);
END Tex;

PROCEDURE Lex (rd: Rd.T; READONLY style : LexStyle; ): T RAISES {L.Error, FloatMode.Trap, Rd.Failure, Thread.Alerted}=BEGIN END Lex;

BEGIN
END MatrixFmtLex.
