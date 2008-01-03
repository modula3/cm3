(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)

INTERFACE RectR;

IMPORT PointR;

TYPE T = RECORD west, east, north, south: REAL END;

CONST Empty = T {0.0,0.0,0.0,0.0};  (* An empty rectangle *)

CONST Full = T {FIRST(REAL), LAST(REAL), FIRST(REAL), LAST(REAL)};
                (* The biggest possible rectangle *)

PROCEDURE Join (READONLY r, s: T): T;
(* Return the smallest rectangle containing both "r" and "s". *)

PROCEDURE Overlap (READONLY r, s: T): BOOLEAN;
(* Return whether "r" and "s" have any element in common. *)

PROCEDURE Subset (READONLY r, s: T): BOOLEAN;
(* Return whether "r" is contained in "s". *)

PROCEDURE Member(READONLY p: PointR.T; READONLY r: T): BOOLEAN;
(* Return whether p is in r. *)

PROCEDURE Add(READONLY r: T; READONLY p: PointR.T): T;

END RectR.
