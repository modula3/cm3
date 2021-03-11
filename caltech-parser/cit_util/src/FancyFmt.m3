(*
   Copyright (c) 2008 Generation Capital Ltd.
   All rights reserved.

   Author: Mika Nystrom <mika@alum.mit.edu>
*)

(* $Id$ *)

MODULE FancyFmt;
FROM Fmt IMPORT Style, Base;
IMPORT Fmt;
IMPORT Wr, CharSeq;
IMPORT Word, Text, TextUtils, TextWr;
IMPORT Thread;

PROCEDURE Reformat(what                        : TEXT; 
                   radixPoint, superRadixComma : CHAR;
                   superRadix                  : CARDINAL;
                   plain                       : BOOLEAN) : TEXT =  
  PROCEDURE IsDigit(c : CHAR) : BOOLEAN =
    BEGIN RETURN c >= '0' AND c <= '9' END IsDigit;

  PROCEDURE Get(p : CARDINAL) : CHAR =
    BEGIN RETURN Text.GetChar(what,(intEnd-1)-p) END Get;
    
  VAR intEnd : [-1..LAST(CARDINAL) ] := -1;
      gotJunk                        := FALSE;
  BEGIN
    IF plain THEN 
      RETURN what
    ELSE
      (* scan input *)
      FOR i := 0 TO Text.Length(what)-1 DO
        WITH c = Text.GetChar(what,i) DO
          IF     c = '.' AND intEnd = -1 THEN intEnd := i
          ELSIF NOT IsDigit(c) AND c # '.' AND c # '-' AND c # '+' THEN 
            gotJunk := TRUE
          END
        END
      END;

      IF intEnd = -1 THEN intEnd := Text.Length(what) END;

      IF gotJunk THEN
        RETURN TextUtils.ReplaceChar(what,'.',radixPoint)
      ELSE
        WITH intSeq = NEW(CharSeq.T).init() DO
          FOR power := 0 TO intEnd-1 DO
            IF power # 0 AND power MOD superRadix = 0 AND IsDigit(Get(power)) THEN
              intSeq.addlo(superRadixComma)
            END;
            intSeq.addlo(Get(power))
          END;

          VAR wr := NEW(TextWr.T).init();
              <*FATAL Wr.Failure, Thread.Alerted *>
          BEGIN
            FOR i := 0 TO intSeq.size()-1 DO
              Wr.PutChar(wr, intSeq.get(i))
            END;
            Wr.PutText(wr, Text.Sub(what, intEnd));

            RETURN TextWr.ToText(wr)
          END
        END
      END
    END
  END Reformat;

PROCEDURE Int(n: INTEGER; base: Base;
              radixPoint, superRadixComma : CHAR;
              superRadix                  : CARDINAL;
              plain                       : BOOLEAN) : TEXT =
  BEGIN
    WITH std = Fmt.Int(n,base) DO
      RETURN Reformat(std,radixPoint,superRadixComma,superRadix,plain)
    END
  END Int;

PROCEDURE Unsigned(n: Word.T; base: Base;
              radixPoint, superRadixComma : CHAR;
              superRadix                  : CARDINAL;
              plain                       : BOOLEAN) : TEXT =
  BEGIN
    WITH std = Fmt.Unsigned(n,base) DO
      RETURN Reformat(std,radixPoint,superRadixComma,superRadix,plain)
    END
  END Unsigned;

PROCEDURE Real(
    x: REAL;
    style : Style;
    prec: CARDINAL;
    literal : BOOLEAN;
    radixPoint, superRadixComma : CHAR;
              superRadix                  : CARDINAL;
              plain                       : BOOLEAN) : TEXT =
  BEGIN
    WITH std = Fmt.Real(x,style,prec,literal) DO
      RETURN Reformat(std,radixPoint,superRadixComma,superRadix,plain OR literal)
    END
  END Real;

PROCEDURE LongReal(
    x: LONGREAL;
    style : Style;
    prec: CARDINAL;
    literal : BOOLEAN;
    radixPoint, superRadixComma : CHAR;
              superRadix                  : CARDINAL;
              plain                       : BOOLEAN) : TEXT =
  BEGIN
    WITH std = Fmt.LongReal(x,style,prec,literal) DO
      RETURN Reformat(std,radixPoint,superRadixComma,superRadix,plain OR literal)
    END
  END LongReal;

PROCEDURE Extended(
    x: EXTENDED;
    style : Style;
    prec: CARDINAL;
    literal : BOOLEAN;
    radixPoint, superRadixComma : CHAR;
              superRadix                  : CARDINAL;
              plain                       : BOOLEAN) : TEXT =
  BEGIN
    WITH std = Fmt.Extended(x,style,prec,literal) DO
      RETURN Reformat(std,radixPoint,superRadixComma,superRadix,plain OR literal)
    END
  END Extended;

BEGIN END FancyFmt.

