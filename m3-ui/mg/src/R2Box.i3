(* Copyright 1989 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Created by stolfi on Thu Mar 22 8:35:20 PST 1990 *)
(* Last modified on Tue Jul 21 16:10:38 PDT 1992 by harrison *)
(* modified on Wed Mar 13 8:12:27 PST 1991 by stolfi *)

(* A box in R2 *)

INTERFACE R2Box;

IMPORT R2, Fuzzy;

TYPE T = ARRAY R2.Axis OF Fuzzy.T;
(* An R2Box.T describes a box in R^2, i.e., an axis-aligned rectangle. *)

CONST
  Empty = T{Fuzzy.Empty, Fuzzy.Empty};
  Full = T{Fuzzy.Full, Fuzzy.Full};

(* ---- construction ---- *)

PROCEDURE FromEdges (h1, h2, v1, v2: REAL): T;
(* If "h1 >= h2" or "v1 >= v2" return "Empty", else return "T{Fuzzy.T{h1,
   h2}, Fuzzy.T{v1, v2}}". *)

PROCEDURE FromAbsEdges (h1, h2, v1, v2: REAL): T;
(* Return
| FromEdges(MIN(h1, h2), MAX(h1, h2),
|           MIN(v1, v2), MAX(v1, v2))
   *)

PROCEDURE FromCorners (READONLY p, q: R2.T): T;
(* Return "FromAbsEdges(p[0],q[0],p[1],q[1])". *)

(* ---- transformation ---- *)

PROCEDURE Meet (READONLY a, b: T): T;
(* Intersection of boxes "a" and "b". *)

PROCEDURE Join (READONLY a, b: T): T;
(* Smallest box that contains both "a" and "b". *)

PROCEDURE Inset (READONLY a: T; by: REAL): T;
(* Make "a" smaller by "by". *)

PROCEDURE Extend (READONLY a: T; p: R2.T): T;
(* Extend "a" to include the point "p". *)

PROCEDURE Translate (READONLY a: T; p: R2.T): T;
(* Translate "a" by "p". *)

(* ---- selection ---- *)

PROCEDURE NorthWest (READONLY a: T): R2.T;

PROCEDURE NorthEast (READONLY a: T): R2.T;

PROCEDURE SouthWest (READONLY a: T): R2.T;

PROCEDURE SouthEast (READONLY a: T): R2.T;

PROCEDURE Middle (READONLY a: T): R2.T;
(* Center of box "a" *)

PROCEDURE Size (READONLY a: T): R2.T;
PROCEDURE HalfSize (READONLY a: T): R2.T;
(* Size(width and height) of box "a".  HalfSize is half the size. *)

(* ---- test ---- *)

PROCEDURE IsEmpty (READONLY a: T): BOOLEAN;
(* TRUE iff "a" is empty *)

(* ---- utils ---- *)

PROCEDURE ToText (READONLY a: T): TEXT;
(* Return a text representation of "a". *)

END R2Box.

