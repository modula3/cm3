(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(* *)
(* by Stephen Harrison and Steve Glassman *)
(* *)
(* Last modified on Fri Aug 19 16:28:05 PDT 1994 by steveg                   *)
(*      modified on Thu Jul  9 17:13:49 PDT 1992 by harrison                 *)

INTERFACE ShapeUtils;

IMPORT R2Path;

PROCEDURE RegularPolygon(sides: CARDINAL := 3; radius := 1.0): R2Path.T;
  (* Return a "R2Path.T" representing the regular polygon centered at the origin, with given "radius" and number of "sides". *)

END ShapeUtils.
