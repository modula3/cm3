(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id$ *)

INTERFACE PSTextBounder;
IMPORT TextBounder;
TYPE
  T <: Public;
  Public = TextBounder.T OBJECT METHODS
    init(): T;
  END;
END PSTextBounder.
