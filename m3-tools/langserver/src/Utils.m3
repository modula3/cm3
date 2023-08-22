(* Copyright (C) 2023, Peter McKinna *)
(* All rights reserved. *)
(* Licensed under the MIT license. *)

MODULE Utils;

IMPORT Text, Rd, TextRd;

TYPE RefChar = REF ARRAY OF CHAR;

(* The json text of a module is escaped for special characters.
   We need updated text to match the original file so that we can match
   the line and col positions of messages like hover.*)

PROCEDURE UnEncode (s: TEXT): TEXT =
  VAR
    ch   : CHAR;
    rd   : Rd.T;
    out  : RefChar;
    index: INTEGER := 0;

  PROCEDURE Out (ch: CHAR) =
    BEGIN
      out[index] := ch;
      INC(index);
    END Out;

  BEGIN
    out := NEW(RefChar, Text.Length(s));
    rd := TextRd.New(s);
    WHILE NOT Rd.EOF(rd) DO
      ch := Rd.GetChar(rd);
      IF ch # '\\' THEN
        Out(ch);
      ELSE
        ch := Rd.GetChar(rd); (* first *)
        CASE ch OF
        | 'n'  => Out('\n');
        | 't'  => Out('\t');
        | 'f'  => Out('\f');
        | 'r'  => Out('\r');
        | '\"' => Out('\"');
        | '\\' => 
          ch := Rd.GetChar(rd); (* second *)
          CASE ch OF
          | 'n'  => Out('\\'); Out('n');
          | 't'  => Out('\\'); Out('t');
          | 'f'  => Out('\\'); Out('f');
          | 'r'  => Out('\\'); Out('r');
          | '\'' => Out('\\'); Out('\'');
          | '\\' => 
            ch := Rd.GetChar(rd); (* third *)
            CASE ch OF
            | '\"' => Out('\\'); Out('\"');
            | '\\' => Out('\\'); Out('\\');
            ELSE
              <* ASSERT FALSE *>
            END;
          ELSE
            Out('\\'); Out(ch);
          END;
        ELSE
          <* ASSERT FALSE *>
        END;
      END
    END;
    RETURN Text.FromChars(SUBARRAY(out^, 0, index - 1));
  END UnEncode;

BEGIN
END Utils.

