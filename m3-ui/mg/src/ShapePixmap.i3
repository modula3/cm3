(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* by Stephen Harrison and Steve Glassman *)
(* *)
(* Last modified on Tue Jul 28 20:43:52 1992 by steveg   *)
(*      modified on Tue Jul 21 20:28:25 PDT 1992 by harrison *)

INTERFACE ShapePixmap;

IMPORT MGV, Pixmap, Path;

PROCEDURE New (READONLY path  : Path.T;
               READONLY border: CARDINAL := 0;
               READONLY fill             := TRUE;
               READONLY v     : MGV.V             ): Pixmap.T;
(* Return a bitmap of the "path" for "v". *)

END ShapePixmap.

