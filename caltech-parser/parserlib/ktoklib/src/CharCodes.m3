(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: CharCodes.m3,v 1.1.1.1 2001-09-19 11:40:10 wagner Exp $ *)

MODULE CharCodes;
IMPORT Text;
IMPORT Rd, Wr, Thread;
IMPORT TextRd, TextWr;
IMPORT Fmt;
<* FATAL Wr.Failure, Rd.Failure, Thread.Alerted *>

PROCEDURE FmtChar(c: CHAR): TEXT =
  BEGIN
    CASE c OF
    | '\t' => RETURN "\\t";
    | '\n' => RETURN "\\n";
    | '\\' => RETURN "\\\\";
    ELSE
      IF ORD(c) < 32 THEN
        RETURN "\\0" & Fmt.Int(ORD(c) DIV 8) &
               Fmt.Int(ORD(c) MOD 8);
      ELSE
        RETURN Text.FromChar(c);
      END;
    END;
  END FmtChar;

PROCEDURE QC(c: CHAR): TEXT =
  BEGIN
    RETURN "'" & FmtChar(c) & "'";
  END QC;

PROCEDURE Q(t: TEXT): TEXT =
  VAR
    rd := TextRd.New(t);
    wr := TextWr.New();
  BEGIN
    Wr.PutChar(wr, '\"');
    TRY
      WHILE TRUE DO
        Wr.PutText(wr, FmtChar(Rd.GetChar(rd)));
      END;
    EXCEPT
      Rd.EndOfFile =>
    END;
    Wr.PutChar(wr, '\"');
    RETURN TextWr.ToText(wr);
  END Q;
    
PROCEDURE StripDelims(t: TEXT): TEXT =
  BEGIN
    RETURN Text.Sub(t, 1, Text.Length(t)-2);
  END StripDelims;

<*OBSOLETE*> PROCEDURE ParseChar(t: TEXT; VAR pos: INTEGER): CHAR =
  VAR
    c: CHAR;
  PROCEDURE Get() =
    BEGIN
      <* ASSERT pos < Text.Length(t) *>
      c := Text.GetChar(t, pos);
      INC(pos);
    END Get;
  BEGIN
    Get();
    IF c = '\134' THEN (*backslash*)
      Get();
      CASE c OF
      | 'n' => RETURN '\n';
      | 't' => RETURN '\t';
      | '0'..'3' =>
        VAR
          c0 := c;
          c1 : CHAR;
        BEGIN
          Get();
          c1 := c;
          Get();
          RETURN VAL((ORD(c)-ORD('0')) +
                 (ORD(c1)-ORD('0'))*8 +
                 (ORD(c0)-ORD('0'))*64, CHAR);
        END;
      ELSE
        RETURN c;
      END;
    ELSE
      RETURN c;
    END;
  END ParseChar;

PROCEDURE GetChar(rd: Rd.T): CHAR RAISES {Rd.EndOfFile} =
  VAR
    c: CHAR;
  BEGIN
    c := Rd.GetChar(rd);
    IF c = '\134' THEN (*backslash*)
      c := Rd.GetChar(rd);
      CASE c OF
      | 'n' => RETURN '\n';
      | 't' => RETURN '\t';
      | '0'..'3' => RETURN VAL((ORD(c)-ORD('0')) +
        (ORD(Rd.GetChar(rd))-ORD('0'))*8 +
        (ORD(Rd.GetChar(rd))-ORD('0'))*64, CHAR);
      ELSE
      END;
    END;
    RETURN c;
  END GetChar;
  
PROCEDURE ParseString(t: TEXT): TEXT =
  VAR
    result: TEXT := "";
    s := t;
    rd: Rd.T;
  BEGIN
    IF Text.GetChar(s, 0) = '\042' THEN
      s := StripDelims(t); (*might be enclosed in quotes*)
    END;
    rd := TextRd.New(s);
    TRY
      WHILE TRUE DO
        result := result & Text.FromChar(GetChar(rd));
      END;
    EXCEPT
      Rd.EndOfFile =>
    END;
    RETURN result;
  END ParseString;

BEGIN
END CharCodes.
