(* Copyright 1992 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Last modified on Sun Jul 19 08:31:28 1992 by mhb  *)
(*      modified on Mon Jun 22 14:14:53 PDT 1992 by johnh*)


INTERFACE DotsView;

IMPORT PaintOp, RectsVBT, Sort, SortViewClass;

TYPE
  T <: Public;
  Public = SortViewClass.T OBJECT
           METHODS
             init (op: PaintOp.T): T;
             setPosition (rects: RectsVBT.T;
                          i    : CARDINAL;
                          val  : Sort.Key    );
           END;

(* The call "v.init(op)" initializes a newly allocated DotsView.
   Its rectangles will have color "op".

   The call "v.setPosition(r, i, val)" is called to position a
   rectangle in RectVBT "r" to represent element "i" when its
   value is "val". *)

END DotsView.
