(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: PSDrawContext.i3,v 1.2 2001-09-19 15:30:31 wagner Exp $ *)

INTERFACE PSDrawContext;
IMPORT DrawContext;
IMPORT Wr;
TYPE
  T <: Public;
  Public = DrawContext.T OBJECT
  METHODS
    init(captureResDPI := 720): T;
    write(wr: Wr.T; title: TEXT);
  END;
END PSDrawContext.
