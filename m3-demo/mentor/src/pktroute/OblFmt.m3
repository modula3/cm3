(* Copyright 1993 Digital Equipment Corporation.                             *)
(* Distributed only by permission.                                           *)
(*                                                                           *)
(* Last modified on Mon Aug  2 00:24:48 PDT 1993 by heydon                   *)

MODULE OblFmt;

IMPORT Fmt, Text;

PROCEDURE Bool(b: BOOLEAN): TEXT =
  BEGIN
    CASE b OF
      TRUE => RETURN "true"
    | FALSE => RETURN "false"
    END
  END Bool;

PROCEDURE Real(r: REAL): TEXT =
  VAR res := ""; BEGIN
    IF r < 0.0 THEN res := "~"; r := - r END;
    res := res & Fmt.Real(r);
    IF Text.FindChar(res, '.') = -1 THEN res := res & ".0" END;
    RETURN res
  END Real;

BEGIN
END OblFmt.
