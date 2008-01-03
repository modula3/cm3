(* Copyright 1992 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Last modified on Fri Jul 16 12:37:50 PDT 1993 by steveg *)
(*      modified on Fri Aug  7 07:17:10 PDT 1992 by mhb *)

INTERFACE PaintOpAnim;

IMPORT GraphVBT, PaintOp;

<* PRAGMA LL *>

TYPE
  RGB = RECORD r, g, b: REAL END;
  (* The red, green and blue values of a color.  "r", "g", "b" should be in
     the range [0.0 .. 1.0].  (0, 0, 0) is Black, (1, 1, 1) is White *)

  T <: PublicT;
  PublicT = OBJECT
  METHODS
    init(rgb: RGB): T;
    (* Initializes self as a "T" with a PaintOp.T whose color is "rgb" *)
    set(graph: GraphVBT.T; rgb: RGB); <* LL = graph.mu *>
    (* Set the color of the PaintOp.T associated with self to "rgb" *)
    get(): RGB;
    (* Return the color of the PaintOp.T associated with self *)
    op(): PaintOp.T;
    (* Return the PaintOp.T associated with self *)
    animate(graph: GraphVBT.T; animation: Animation); <* LL = graph.mu *>
    (* Next time "graph.animate" is called, self.set(graph, animation.rgb(time))
       will be repeatedly called to change the color of T *)
  END;

  
  Animation = OBJECT
  METHODS
    rgb(time: REAL): RGB;
    (* "rgb" is called with "time" in the range [0.0 .. 1.0]. *)
  END;

END PaintOpAnim.
