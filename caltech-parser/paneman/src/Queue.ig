(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: Queue.ig,v 1.1.1.1 2001-09-19 11:40:10 wagner Exp $ *)

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
