(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman, Mark Manasse and Greg Nelson           *)
(* Last modified on Tue Mar 10 18:59:24 1992 by steveg   *)
(*      modified on Mon Feb 24 13:57:27 PST 1992 by muller   *)
(*      modified on Wed Dec 11 18:45:20 PST 1991 by gnelson  *)

<*PRAGMA LL*>

(* The "Palette" interface allows you to implement your own
   screen-independent resources by registering a closure to
   produce an appropriate screen-dependent resource for any given 
   screentype.  *)

INTERFACE Palette;

IMPORT VBT, PaintOp, Cursor, Pixmap, Font, 
  ScrnPaintOp, ScrnCursor, ScrnPixmap, ScrnFont;

(* Translating a screen-independent resource into its screen-dependent
   form is called {\it resolving} the resource.  Here are the closure 
   types for resolving resources: *)

TYPE 
  OpClosure = OBJECT METHODS
    <* LL.sup <= VBT.mu *>
    apply(st: VBT.ScreenType): ScrnPaintOp.T;
  END;
  
  CursorClosure = OBJECT METHODS
    <* LL.sup <= VBT.mu *>
    apply(st: VBT.ScreenType): ScrnCursor.T;
  END;
  
  PixmapClosure = OBJECT METHODS
    <* LL.sup <= VBT.mu *>
    apply(st: VBT.ScreenType): ScrnPixmap.T;
  END;
  
  FontClosure = OBJECT METHODS
    <* LL.sup <= VBT.mu *>
    apply(st: VBT.ScreenType): ScrnFont.T;
  END;

(* When an "apply" method is called, "st # NIL".
  If the method returns "NIL", then some default screen-dependent
  resource will be used; for example, the built-in font or the
  transparent painting operation.

  The following procedures produce screen-independent resources
  from closures: *)
   
PROCEDURE FromOpClosure(cl: OpClosure): PaintOp.T;
<* LL.sup <= VBT.mu *>
(* Return a "PaintOp.T" that behaves like "cl.apply(st)" on "st". *)

PROCEDURE FromCursorClosure
  (cl: CursorClosure): Cursor.T; <* LL.sup <= VBT.mu *>
(* Return a "Cursor.T" that behaves like "cl.apply(st)" on "st". *)

PROCEDURE FromPixmapClosure
  (cl: PixmapClosure): Pixmap.T; <* LL.sup <= VBT.mu *>
(* Return a "Pixmap.T" that behaves like "cl.apply(st)" on "st". *)

PROCEDURE FromFontClosure(cl: FontClosure): Font.T;
<* LL.sup <= VBT.mu *>
(* Return a "Font.T" that behaves like "cl.apply(st)" on "st". *)

(* If your apply method that resolves a resource needs to resolve some
   other resource, you should use one of the following procedures to
   do so.  In all cases, "st" must be non-"NIL". *)
   
PROCEDURE ResolveOp(st: VBT.ScreenType; op: PaintOp.T)
  : ScrnPaintOp.T;
(* Resolve "op" for "st". *)

PROCEDURE ResolveCursor(st: VBT.ScreenType; 
  cursor: Cursor.T): ScrnCursor.T;
(* Resolve "cursor" for "st". *)
   
PROCEDURE ResolvePixmap(st: VBT.ScreenType; 
  pixmap: Pixmap.T): ScrnPixmap.T;
(* Resolve "pixmap" for "st". *)
   
PROCEDURE ResolveFont(st: VBT.ScreenType; font: Font.T)
  : ScrnFont.T;
(* Resolve "font" for "st". *)
   
(* If you create a cycle of screen-independent resources each of which 
   tries to resolve the next resource in the cycle, then the program 
   will deadlock. 

   To implement screen-independent resources, every screentype includes
   a {\it palette}, which is a table of screen-dependent resources
   appropriate for that screentype.  Most clients don't need to worry
   about the palette, but if you are implementing a "VBT" class that
   translates to some other window system---like X or Microsoft
   Windows---here is the procedure for building the palette in the
   screentype for a top-level window: \index{palette}*)

PROCEDURE Init(st: VBT.ScreenType);
<* LL.sup = VBT.mu.v *>
(* Initialize "st"'s palette, if it is not already initialized,
   by resolving all screen-independent resources for "st" and
   storing the results.  *)
   
END Palette.

