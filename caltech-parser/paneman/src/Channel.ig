(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id$ *)

GENERIC INTERFACE Channel(Elem);
TYPE
  T <: Public;
  Public = OBJECT
  METHODS
    init(): T;
    send(elem: Elem.T);
    recv(): Elem.T;
  END;
END Channel.
