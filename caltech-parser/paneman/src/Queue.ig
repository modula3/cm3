(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id$ *)

GENERIC INTERFACE Queue(Elem);
TYPE
  T <: Public;
  Public = OBJECT
  METHODS
    init(): T;
    get(VAR e: Elem.T): BOOLEAN;
    put(e: Elem.T);
  END;
END Queue.
