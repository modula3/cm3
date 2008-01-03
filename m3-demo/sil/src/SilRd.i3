(* Copyright (C) 1994, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(*                                                            *)
(* Last modified on Mon Nov  7 10:23:01 PST 1994 by kalsow    *)

INTERFACE SilRd;

TYPE
  T <: Tx;
  Tx = OBJECT
    n_ints   : INTEGER := 0;
    n_strs   : INTEGER := 0;
    ints     : ARRAY [0..11] OF INTEGER;
    strs     : ARRAY [0..1] OF TEXT;
    cmd      : CHAR;
    fonts    : REFANY  := NIL;
    macros   : REFANY  := NIL;
    in_macro : BOOLEAN := FALSE;
  END;

PROCEDURE Open (filename: TEXT): T;
PROCEDURE ParseNextLine (t: T): BOOLEAN;
PROCEDURE Close (t: T);

END SilRd.


