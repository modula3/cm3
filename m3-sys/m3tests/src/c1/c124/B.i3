(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
INTERFACE B;

IMPORT A;

TYPE
  Closure <: OBJECT
    METHODS
      callback(m := A.Mode.Entry);
  END;

END B.
