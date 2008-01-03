(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* by Steve Glassman and Stephen Harrison *)
(* *)
(* Contributed by Michel Dagenais (dagenais@vlsi.polymtl.ca), 1994. *)

(* This interface provides operations on objects of type Path.T and of type
   RealPath.T.  A Path.T is the path represented on an integer grid, a
   RealPath.T is a path represented by coordinates which are reals. *)

INTERFACE PathExtra;

IMPORT Path, RealPath, Rect, RealTransform;

PROCEDURE RealPathToPath (READONLY realPath: RealPath.T;
                          READONLY matrix := RealTransform.Identity): Path.T;
(* Convert "realPath" to the equivalent "Path.T" on an integer grid
   transformed by "matrix". *)

PROCEDURE PathBounds (READONLY path: Path.T): Rect.T;
(* Return the integer bounds of "path". *)

END PathExtra.

