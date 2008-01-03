(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: PDATransTally.i3,v 1.2 2001-09-19 15:13:58 wagner Exp $ *)

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
