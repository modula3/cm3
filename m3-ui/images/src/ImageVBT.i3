(* Copyright 1990 Digital Equipment Corporation. *)
(* Distributed only by permission. *)

(* Lectern: a user interface for viewing documents stored as images *)
(* VBT for viewing images *)

(* Last modified on Mon Mar 27 11:47:26 PST 1995 by birrell   *)

INTERFACE ImageVBT;

IMPORT Images, PaintOp, Point, Region, Thread, VBT;

TYPE T <: Public;

TYPE Public = VBT.Leaf OBJECT
    (* This VBT class will display an Images.T.  The client can move the
       image by calling "moveTo" or "moveToSector". *)
    (* Locking: the revelation for "T" includes a mutex "imageMu".  The locking
       levels for T's put, moveTo, flash, redisplay and repaint
       methods include v.imageMu.  The locking level for T's paint method is
       LL <= v.imageMu.  In other words, don't call those methods of T from
       within a call of T's paint method.  However, get and getDelta have
       loocking level = any. *)
    (* VBT.mu < v.imageMu < v *)
  METHODS
    init(pm: Images.T; bg: PaintOp.T; shadow := FALSE; paintChunk := 0): T;
      (* Initialize the VBT. "pm" is the desired image and "bg" is used for
         for filling parts of the VBT's domain not occupied by the image.
         If "shadow" the image will be painted with a surrounding drop-shadow,
         whenever there's enough space in the VBT's domain.  "paintChunk" is
         passed to pm.paint. *)
    put(pm: Images.T; bg: PaintOp.T; shadow := FALSE; paintChunk := 0);
      (* LL = VBT.mu *)
      (* Sets the image for future use, and marks the VBT for redisplay.
         The arguments are the same as .init *)
    get(): Images.T;
      (* LL = any (internally synchronized) *)
      (* Returns the image *)
    getDelta(): Point.T;
      (* LL = any (internally synchronized) *)
      (* Returns the amount by which the center of the pixmap is offset from
         the center of the VBT. *)
    moveTo(delta: Point.T; pixelRate := 0.0; highlighting := FALSE);
      (* LL = VBT.mu *)
      (* Scrolls and repaints so that center of pixmap is offset from center of
         the VBT by delta.  Attempts to time the scrolling so that the image
         moves at an average rate of pixelRate seconds per pixel (e.g., if
         pixelRate is 0.01 and the overall distance to be moved is 100 pixels,
         the operation would ideally take 1 second to execute).  Iff
         "highlighting", the previously visible area will be highlighted during
         the move and for about half a second after it. *)
    flash();
      (* LL = VBT.mu *)
      (* Flash v's domain briefly *)
    paint(READONLY rgn: Region.T)
          RAISES { Thread.Alerted, Images.Error };
      (* LL <= v.imageMu (see comments above) *)
      (* This method is called for all painting.  The default implementation
         makes appropriate calls on image.paint.  All painting is completed
         (in the sense of being given to the underlying VBT.T) synchronously.
         A sub-class can thus over-ride this method to superimpose its own
         pixels on those produced by the image. *)
  END;

END ImageVBT.
