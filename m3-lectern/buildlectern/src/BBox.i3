(* Copyright 1994 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Last modified on Wed Nov  2 11:44:13 PST 1994 by mcjones *)

(* Bounding box. *) 

INTERFACE BBox;

CONST Brand = "BBox";

TYPE T = RECORD blx, bly, trx, try: INTEGER END;
(* A bounding box "t" contains every point "(x, y)" with:

| blx <= x < try AND bly <= y < try

  The coordinate system is like that of Trestle: the origin is in
  the top left corner, with positive coordinates representing
  positions below and to the right.

*)

END BBox.
