(* Copyright 1990 Digital Equipment Corporation. *)
(* Distributed only by permission. *)

(* Lectern: a user interface for viewing documents stored as images *)
(* VBT for selecting the view area of an image *)

(* Last modified on Tue May  3 13:47:40 PDT 1994 by birrell   *)

INTERFACE ViewAreaVBT;

IMPORT Images, ImageVBT, PaintOp, Rect, VBT;

TYPE T <: Public;

TYPE Public = ImageVBT.T OBJECT
    (* A VBT that displays the image with a superimposed selection rectangle.
       The user can modify the selection rectangle by dragging control points.
       In each of the methods, the selection rectangle is expressed in the
       same coordinates as the image's screen-independent domain,
       v.get.domain(NIL). *)
  METHODS
    init(pm: Images.T; bg: PaintOp.T; r: Rect.T): T;
      (* Initialize the VBT, with initial selection "r". *)
    hit(time: VBT.TimeStamp);
      (* LL = VBT.mu *)
      (* Called whenever the user changes the selection. *)
    setSelected(r: Rect.T);
      (* LL = VBT.mu *)
      (* Sets the value of the selection *)
    getSelected(): Rect.T;
      (* LL = VBT.mu *)
      (* Returns the current value of the selection. *)
  END;

END ViewAreaVBT.
