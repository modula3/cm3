(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* by Steve Glassman and Stephen Harrison *)
(* *)
(* Last modified on Fri Aug 19 16:25:37 PDT 1994 by steveg   *)
(*      modified on Sun Jul 19 12:08:15 PDT 1992 by harrison *)

(* This interface provides operations on objects of type Path.T and of type
   R2Path.T.  A Path.T is the path represented on an integer grid, a
   R2Path.T is a path represented by coordinates which are reals. *)

INTERFACE R2PathExtra;

IMPORT Point, MG, Path, R2Path, Rect, Matrix2D;

PROCEDURE TranslatePath (READONLY path: Path.T; READONLY delta: Point.T):
  Path.T;
(* Return "path" translated by "delta". *)

PROCEDURE R2PathToPath (         v       : MG.V;
                          READONLY R2Path: R2Path.T;
                          READONLY matrix := Matrix2D.Identity): Path.T;
(* Convert "R2Path" to the equivalent "Path.T" on an integer grid
   transformed by "matrix". *)

TYPE
  Element = RECORD
              pt: Point.T;
              steps: INTEGER;
            END;

  SubPath = RECORD
              start: Point.T;
              elems: REF ARRAY OF Element := NIL;
              closed: BOOLEAN;
            END;

  Segments <: PublicSegments;
  PublicSegments = OBJECT subPaths: REF ARRAY OF SubPath := NIL;  END;

PROCEDURE PathToSegments (v: MG.V; READONLY path: Path.T): Segments;
(* Convert "path" to the corresponding "Segment" structure. *)

PROCEDURE PathBounds (READONLY path: Path.T): Rect.T;
(* Return the integer bounds of "path". *)

END R2PathExtra.

