(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id$ *)

INTERFACE PDATransTally;
IMPORT PDATrans;
CONST
  Brand = "PDATransTally";
TYPE
  T = RECORD
    key: INTEGER;
    tr: PDATrans.T;
  END;
PROCEDURE Compare(a, b: T): [-1..1];
END PDATransTally.
