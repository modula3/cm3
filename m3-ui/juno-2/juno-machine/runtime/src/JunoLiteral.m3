(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Apr 21 15:00:09 PDT 1994 by heydon                   *)

MODULE JunoLiteral;

IMPORT JunoValue;
IMPORT Rd, Lex, Text, Thread, FloatMode;

<* FATAL Thread.Alerted *>

PROCEDURE Parse(rd: Rd.T): JunoValue.T RAISES {ParseError, Rd.Failure} =
  VAR c: CHAR; res: JunoValue.T; BEGIN
    TRY
      Lex.Skip(rd, Lex.Blanks);
      c := Rd.GetChar(rd);
      CASE c OF
	'-', '0'..'9' =>
          VAR v: REF JunoValue.Real; BEGIN
            Rd.UnGetChar(rd);
            v := NEW(REF JunoValue.Real); res := v;
            v^ := Lex.Real(rd);
          END
      | 'N' =>
          IF Rd.GetChar(rd) = 'I' AND Rd.GetChar(rd) = 'L'
            THEN res := JunoValue.Nil
            ELSE RAISE ParseError
          END
      | '\"' =>
          res := ParseText(rd)
      | '(' =>
          VAR v := NEW(REF JunoValue.Pair); BEGIN
            v^.car := Parse(rd);
            Lex.Skip(rd, Lex.Blanks);
            Lex.Match(rd, ",");
            v^.cdr := Parse(rd);
            Lex.Skip(rd, Lex.Blanks);
            Lex.Match(rd, ")");
	    res := v
          END
      | '[' =>
          VAR curr := NEW(REF JunoValue.Pair); BEGIN
            res := curr;
            LOOP
              curr^.car := Parse(rd);
              Lex.Skip(rd, Lex.Blanks);
              IF Rd.GetChar(rd) # ',' THEN EXIT END;
              VAR next := NEW(REF JunoValue.Pair); BEGIN
                curr^.cdr := next; curr := next
              END
            END;
            IF c # ']' THEN RAISE ParseError END;
            curr^.cdr := JunoValue.Nil
          END
      ELSE RAISE ParseError
      END
    EXCEPT Rd.EndOfFile, Lex.Error, FloatMode.Trap => RAISE ParseError
    END;
    RETURN res
  END Parse;

PROCEDURE ParseText(rd: Rd.T): TEXT
  RAISES {ParseError, Rd.Failure, Rd.EndOfFile} =
(* Read the Juno text literal from "rd", assuming that the double-quote
   character starting the text has already been consumed. *)
  VAR res := ""; c := Rd.GetChar(rd); BEGIN
    WHILE c # '\"' DO
      IF c = '\\' THEN
        c := Rd.GetChar(rd);
        CASE c OF
        | 'n'  => res := res & "\n"
        | 't'  => res := res & "\t"
        | 'r'  => res := res & "\r"
        | 'f'  => res := res & "\f"
        | '\\' => res := res & "\\"
        | '\"' => res := res & "\""
        | '0'..'9' => res := res & ReadOctEscape(c, rd)
        ELSE RAISE ParseError
        END
      ELSE
        res := res & Text.FromChar(c)
      END;
      c := Rd.GetChar(rd)
    END;
    RETURN res
  END ParseText;

<* INLINE *>
PROCEDURE ReadOctEscape(c: CHAR; rd: Rd.T): TEXT
  RAISES {ParseError, Rd.Failure, Rd.EndOfFile} =
(* Read a 3-digit octal value from the stream "c" concatenated with "rd", and
   return the text containing the single character with that ASCII value. *)
  CONST OctDigits = SET OF CHAR {'0'..'7'};
  VAR val := ORD(c) - ORD('0'); BEGIN
    FOR i := 1 TO 2 DO
      c := Rd.GetChar(rd);
      IF NOT c IN OctDigits THEN RAISE ParseError END;
      val := (val * 8) + ORD(c) - ORD('0')
    END;
    IF val > ORD(LAST(CHAR)) THEN RAISE ParseError END;
    RETURN Text.FromChar(VAL(val, CHAR))
  END ReadOctEscape;

BEGIN END JunoLiteral.
