(* Copyright (C) 1994, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(*                                                            *)
(* Last modified on Fri Nov  4 09:20:06 PST 1994 by kalsow    *)

INTERFACE SilWr;

TYPE
  T <: Tx;
  Tx = OBJECT
    generation    : INTEGER := 0;
    next_font_id  : INTEGER := 0;
    next_macro_id : INTEGER := 0;
  METHODS
    putInt  (i: INTEGER);
    putText (t: TEXT);
    putStr  (t: TEXT);
    endLine ();
  END;

PROCEDURE Open (filename: TEXT): T;
PROCEDURE Close (t: T);

END SilWr.


