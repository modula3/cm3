(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: RECORD constants *)

INTERFACE Test;

TYPE 
  R = RECORD 
        a : INTEGER;
        b : BOOLEAN; 
        c : REAL := 7.3; END;

CONST
  V1 = R {5, TRUE};
  V2 = R {a := 5, b := TRUE};
  V3 = R {5, b := TRUE};
  V4 = R {b := TRUE, a := 5};
  V5 = R {b := TRUE, a := 5, c := 6.3};

END Test.
