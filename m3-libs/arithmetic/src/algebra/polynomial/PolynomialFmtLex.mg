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

VAR
  wr:=TextWr.New();
  sep:TEXT;
  texCoef:PROCEDURE (i : CARDINAL) : TEXT;
BEGIN
  IF TexFlag.powerSum IN style.flags THEN
    IF TexFlag.simplePower IN style.flags THEN
      texCoef:=TexSimplePower;
    ELSE
      texCoef:=TexPower;
    END;
    sep:="+";
  ELSE
    <*ASSERT NOT TexFlag.simplePower IN style.flags *>
    texCoef:=TexCoefficient;
    sep:=",";
    Wr.PutText(wr,"\\left(");
  END;

  IF NUMBER(x^)=0 THEN
    IF TexFlag.powerSum IN style.flags THEN
      Wr.PutText(wr,RF.Tex(R.Zero,style.elemStyle,Precedence.sum));
      (*ELSE we output () as vector notation*)
    END;
  ELSE
    IF TexFlag.reverse IN style.flags THEN
      VAR
        i:=NUMBER(x^);
      BEGIN
        LOOP
          DEC(i);
          Wr.PutText(wr,texCoef(i));
          IF i=0 THEN EXIT END;
          Wr.PutText(wr,sep);
        END;
      END;
    ELSE
      VAR
        i:=FIRST(x^);
      BEGIN
        LOOP
          Wr.PutText(wr,texCoef(i));
          INC(i);
          IF i=NUMBER(x^) THEN EXIT END;
          Wr.PutText(wr,sep);
        END;
      END;
    END;
  END;

  IF NOT TexFlag.powerSum IN style.flags THEN
    Wr.PutText(wr,"\\right)\n");
  END;
  RETURN TextWr.ToText(wr);
END Tex;

(*==========================*)
BEGIN
END PolynomialFmtLex.
