(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* by Steve Glassman and Stephen Harrison *)
(* Last modified on Wed Jul 22 01:39:07 1992 by steveg *)

<*PRAGMA LL*>

INTERFACE GridMJJ;

IMPORT Axis, MG;

TYPE Array = REF ARRAY OF ARRAY OF MG.Rectangle;

TYPE
  V <: PublicV;
  PublicV = MG.V OBJECT
              <* LL = self.mu *>
              a: Array;
              (* two dimensional array [rows x cols] of MG.Rectangles *)
              group: MG.Group;
              (* MGPublic.Pos(group) is northwest corner of grid *)
              size: ARRAY Axis.T OF REAL;
              (* (width * cols, height * rows) *)
            METHODS
              <* LL <= self.mu *>
              init (rows, cols      : CARDINAL;
                    width, height   : REAL;
                    borderW, borderH             := 0.0): V;
            END;

END GridMJJ.

