(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id$ *)

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
