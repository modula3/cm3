(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Thu Jun 16 14:31:18 PDT 1994 by mhb                      *)
(*      modified on Sun Jun  5 17:01:37 PDT 1994 by najork                   *)
(*       Created on Sun Jun  5 16:59:40 PDT 1994 by najork                   *)


MODULE ZFmt;

IMPORT Fmt, TextConv;

PROCEDURE Int (n : INTEGER) : TEXT =
  BEGIN 
    RETURN Fmt.Int (n) 
  END Int;

PROCEDURE Bool (b : BOOLEAN) : TEXT =
  BEGIN
    IF b THEN RETURN "true" ELSE RETURN "false" END
  END Bool;

PROCEDURE Char (c : CHAR) : TEXT =
  BEGIN 
    RETURN Fmt.Char (c) 
  END Char;

PROCEDURE Real (r : REAL) : TEXT =
  BEGIN
    RETURN Fmt.Real (r, style := Fmt.Style.Fix, literal := TRUE);
  END Real;

PROCEDURE LongReal (r : LONGREAL) : TEXT =
  BEGIN
    RETURN Fmt.LongReal (r, style := Fmt.Style.Fix, literal := TRUE);
  END LongReal;

PROCEDURE Text (t: TEXT): TEXT = 
  BEGIN
    RETURN TextConv.Encode (t)
  END Text;

BEGIN
END ZFmt.
