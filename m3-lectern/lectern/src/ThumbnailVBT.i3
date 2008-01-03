(* Copyright 1990 Digital Equipment Corporation. *)
(* Distributed only by permission. *)

(* Lectern: a user interface for viewing documents stored as images *)
(* VBT for viewing and selecting thumbnail images *)

(* Last modified on Thu Sep 22 14:47:33 PDT 1994 by birrell   *)

INTERFACE ThumbnailVBT;

IMPORT Images, LecternDoc, PaintOp, Point, Rd, VBT;

TYPE
  T <: Public;
  Private <: VBT.Split;
  Public = Private OBJECT
      shadow: BOOLEAN := FALSE;
        (* whether to paint drop shadow; LL = VBT.mu *)
    METHODS
      init(colors: PaintOp.ColorQuad := NIL;
           size: Point.T;  (* rectangle's SE relative to its NW *)
           gap: Point.T    (* h and v gap between rectangles *)
           ): T;
        (* LL.sup = VBT.mu *)
        (* Initialize with no cells.  May be called multiple times *)
      setContents(rd: Rd.T;
                  READONLY dir: LecternDoc.Dir;
                  gamma := 1.0);
        (* LL.sup = VBT.mu *)
        (* Provides cell contents, and marks for repaint *)
      set(n: INTEGER);
        (* LL.sup = VBT.mu *)
        (* make N be the selected rectangle *)
      get(): INTEGER;
        (* LL.sup = VBT.mu *)
        (* Returns the number of the selected rectangle *)
      scrollToShow(n: INTEGER);
        (* LL.sup = VBT.mu *)
        (* scroll the contents so that rectangle N is fully visible *)
      acquire(n: INTEGER): Images.T;
        (* LL.sup = mu.t, i.e. t's share of VBT.mu, as for repaint method *)
        (* Returns the image for rectangle N, or NIL (meaning no image) *)
      hit(n: INTEGER; time: VBT.TimeStamp);
        (* LL.sup = VBT.mu *)
        (* Called when user has modified the selection to be rectangle N *)
        (* Default method does nothing *)
    END;

END ThumbnailVBT.

