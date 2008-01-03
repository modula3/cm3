(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: Channel.ig,v 1.2 2001-09-19 14:22:13 wagner Exp $ *)

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
