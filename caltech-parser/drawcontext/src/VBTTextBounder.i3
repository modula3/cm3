(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: VBTTextBounder.i3,v 1.2 2001-09-19 15:30:31 wagner Exp $ *)

INTERFACE VBTTextBounder;
IMPORT TextBounder;
IMPORT VBT;
TYPE
  T <: Public;
  Public = TextBounder.T OBJECT METHODS
    init(v: VBT.T): T;
  END;
END VBTTextBounder.
