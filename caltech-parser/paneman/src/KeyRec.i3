(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id$ *)

INTERFACE KeyRec;
IMPORT VBT;
CONST
  Brand = "KeyRec";
TYPE
  T = VBT.KeyRec;
PROCEDURE LowerChar(a: T): CHAR;
PROCEDURE Equal(a,b: T): BOOLEAN; (* ignores time, modifiers, wentdown *)
END KeyRec.
