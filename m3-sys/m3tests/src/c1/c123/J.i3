(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
INTERFACE J;
IMPORT I;
PROCEDURE P(c := I.C);
TYPE R = RECORD a := I.D + 2.0d1; END;
END J.
