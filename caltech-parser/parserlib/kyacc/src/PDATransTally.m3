(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: PDATransTally.m3,v 1.2 2001-09-19 15:13:58 wagner Exp $ *)

MODULE PDATransTally;
IMPORT Integer;
IMPORT PDATrans;
PROCEDURE Compare(a, b: T): [-1..1] =
  VAR
    result := Integer.Compare(a.key, b.key);
  BEGIN
    IF result # 0 THEN RETURN result; END;
    RETURN PDATrans.Compare(a.tr, b.tr);
  END Compare;
BEGIN
END PDATransTally.
