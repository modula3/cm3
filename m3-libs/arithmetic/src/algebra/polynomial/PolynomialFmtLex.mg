GENERIC MODULE PolynomialFmtLex(R,RF);
(*Copyright (c) 1995, Harry George

Abstract: Polynomials.

12/27/95  Harry George    Initial version
2/3/96    Harry George    Converted to m3na format.
2/17/96   Harry George    Converted from OO to ADT format.
*)

IMPORT Fmt AS F,Wr,TextWr,Thread;
FROM FmtLexSupport IMPORT Precedence;

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
  wr:=NEW(TextWr.T).init();
BEGIN
  Wr.PutText(wr,"Polynomial"
     & F.Int(NUMBER(x^)) & "{");
  FOR i:=FIRST(x^) TO LAST(x^) DO
    Wr.PutText(wr,RF.Fmt(x[i],style.elemStyle));
    IF i#LAST(x^) THEN Wr.PutText(wr,", "); END;
  END;
  Wr.PutText(wr,"}");
  RETURN TextWr.ToText(wr);
END Fmt;



PROCEDURE Tex (x : T; READONLY style := TexStyle{}; <*UNUSED*> within := Precedence.sum) : TEXT RAISES {Thread.Alerted, Wr.Failure} =

  PROCEDURE TexPower (i : CARDINAL) : TEXT =
    BEGIN
      RETURN RF.Tex(x[i],style.elemStyle,Precedence.product) & " " &
             style.var & "^{" & F.Int(i) & "}";
    END TexPower;

  PROCEDURE TexSimplePower (i : CARDINAL) : TEXT =
    BEGIN
      IF i=0 THEN
        RETURN RF.Tex(x[i],style.elemStyle,Precedence.sum);
      ELSIF i=1 THEN
        RETURN RF.Tex(x[i],style.elemStyle,Precedence.product) & " " &
               style.var;
      ELSE
        RETURN TexPower(i);
      END;
    END TexSimplePower;

  PROCEDURE TexCoefficient (i : CARDINAL) : TEXT =
    BEGIN
      RETURN RF.Tex(x[i],style.elemStyle,Precedence.sum);
    END TexCoefficient;

TYPE
  TexCoefProc = PROCEDURE (i : CARDINAL) : TEXT;

  PROCEDURE TexMonomial (texCoef:TexCoefProc; i : CARDINAL; reqSep : BOOLEAN)
      RAISES {Thread.Alerted, Wr.Failure} =
    BEGIN
      IF NOT (TexFlag.omitZero IN style.flags AND
              R.IsZero(x[i])) THEN
        IF reqSep THEN
          Wr.PutText(wr,sep);
        END;
        Wr.PutText(wr,texCoef(i));
      END;
    END TexMonomial;


  PROCEDURE LoopForward (texCoef:TexCoefProc)
      RAISES {Thread.Alerted, Wr.Failure} =
    BEGIN
      FOR i:=FIRST(x^) TO LAST(x^) DO
        TexMonomial(texCoef,i,i>FIRST(x^))
      END;
    END LoopForward;

  PROCEDURE LoopBackward (texCoef:TexCoefProc)
      RAISES {Thread.Alerted, Wr.Failure} =
    BEGIN
      FOR i:=LAST(x^) TO FIRST(x^) BY -1 DO
        TexMonomial(texCoef,i,i<LAST(x^))
      END;
    END LoopBackward;

  PROCEDURE Loop (texCoef:TexCoefProc)
      RAISES {Thread.Alerted, Wr.Failure} =
    BEGIN
      IF NUMBER(x^)=0 THEN
        IF TexFlag.powerSum IN style.flags THEN
          Wr.PutText(wr,RF.Tex(R.Zero,style.elemStyle,Precedence.sum));
          (*ELSE we output () as vector notation*)
        END;
      ELSE
        IF TexFlag.reverse IN style.flags THEN
          LoopBackward(texCoef);
        ELSE
          LoopForward(texCoef);
        END;
      END;
    END Loop;

VAR
  wr:=TextWr.New();
  sep:TEXT;
BEGIN
  <*ASSERT NOT (NUMBER(x^)>1 AND R.IsZero(x[LAST(x^)]))*>
  IF TexFlag.powerSum IN style.flags THEN
    sep:="+";
    IF TexFlag.simplePower IN style.flags THEN
      Loop(TexSimplePower);
    ELSE
      Loop(TexPower);
    END;
  ELSE
    <*ASSERT NOT TexFlag.simplePower IN style.flags *>
    sep:=",";
    Wr.PutText(wr,"\\left(");
    Loop(TexCoefficient);
  END;

  IF NOT TexFlag.powerSum IN style.flags THEN
    Wr.PutText(wr,"\\right)\n");
  END;
  RETURN TextWr.ToText(wr);
END Tex;

(*==========================*)
BEGIN
END PolynomialFmtLex.
