(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
INTERFACE A;
TYPE
  Mode = {Entry, Exit};

  Closure <: OBJECT
    METHODS
      callback(m := Mode.Entry);
  END;

END A.
