(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id$ *)

INTERFACE VBTTextBounder;
IMPORT TextBounder;
IMPORT VBT;
TYPE
  T <: Public;
  Public = TextBounder.T OBJECT METHODS
    init(v: VBT.T): T;
  END;
END VBTTextBounder.
