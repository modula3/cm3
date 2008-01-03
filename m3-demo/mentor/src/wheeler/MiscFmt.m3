(* Copyright (C) 1993, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Jan  9 12:15:20 PST 1995 by najork                   *)
(*      modified on Wed Jul 21 14:43:20 PDT 1993 by mann                     *)
(*      modified on Mon Jul 19 11:45:29 PDT 1993 by perl                     *)

MODULE MiscFmt;

IMPORT Fmt, Text, TextConv;

PROCEDURE Char(c: CHAR): TEXT =
  VAR enc: ARRAY[0..3] OF CHAR;
  BEGIN
    RETURN "'" &
           Text.FromChars(SUBARRAY(enc, 0, TextConv.EncodeChar(c, enc))) &
           "'";
  END Char;

PROCEDURE IntArray(ia: RefIntArray): TEXT =
  VAR res := "[";
  BEGIN
    FOR i := FIRST(ia^) TO LAST(ia^) DO
      res := res & Fmt.Int(ia[i]);
      IF i # LAST(ia^) THEN res := res & ", " END
    END;
    res := res & "]";
    RETURN res
  END IntArray;

PROCEDURE TextArray(ta: RefTextArray): TEXT =
  VAR res := "[";
  BEGIN
    FOR i := FIRST(ta^) TO LAST(ta^) DO
      res := res & TextConv.Encode(ta[i]);
      IF i # LAST(ta^) THEN res := res & ", " END
    END;
    res := res & "]";
    RETURN res
  END TextArray;

BEGIN
END MiscFmt.
