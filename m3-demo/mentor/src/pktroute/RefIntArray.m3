(* Copyright 1993 Digital Equipment Corporation.                             *)
(* Distributed only by permission.                                           *)
(*                                                                           *)
(* Last modified on Sat Jul 17 15:45:24 PDT 1993 by heydon                   *)

MODULE RefIntArray;

IMPORT Fmt;

PROCEDURE ToText(ia: REF IntArray): TEXT =
  VAR res := "["; BEGIN
    FOR i := 0 TO LAST(ia^) DO
      res := res & Fmt.Int(ia[i]);
      IF i < LAST(ia^) THEN res := res & ", " END
    END;
    RETURN res & "]"
  END ToText;

BEGIN
END RefIntArray.
