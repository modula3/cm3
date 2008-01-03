(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: TestDrawContext.i3,v 1.2 2001-09-19 15:30:31 wagner Exp $ *)

INTERFACE TestDrawContext;
IMPORT DrawContext;
IMPORT BoundDrawContext;
TYPE
  T <: Public;
  Public = BoundDrawContext.T OBJECT
  METHODS
    init(dc: DrawContext.T): T;
    (* the idea is to decide if lines and text drawn in dc would be visible. *)

    reset();
    (* start accumulating visible status *)

    visible(): BOOLEAN;
    (* TRUE iff some line drawn since reset() would be visible in dc. *)
  END;
END TestDrawContext.
