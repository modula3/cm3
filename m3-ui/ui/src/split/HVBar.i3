(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* HVBar.def, by CGN and MSM, Tue Sep 30 17:24:31 1986 *)
(* Last modified on Mon Feb 24 13:53:19 PST 1992 by muller  *)
(*      modified on Wed Dec 11 18:37:40 PST 1991 by gnelson *)
(*      modified on Fri Feb  2 14:00:55 PST 1990 by glassman *)
(*      modified on Fri May  5 23:54:23 1989 by stolfi *)
(*      modified on Sat Dec 13 18:04:44 1986 by msm *)
<*PRAGMA LL*>

(* An "HVBar.T" is an adjustable bar that allows a user to
   adjust the division of space between the children of an "HVSplit".

   An "HVBar" must be a child of an "HVSplit".  When the user pushes
   a mouse button over the bar, the cursor changes shape and the outline
   of the bar is highlighted.  The highlight follows the cursor as long
   as the button is down.  When the button comes up, the bar calls
   "HVSplit.Adjust" to move the bar to the currently highlighted
   position.  If the user tries to move the bar outside the range of
   positions that are consistent with the size constraints of the
   children of the parent "HVSplit", the highlighted bar will not
   follow the cursor.  If the user chords while dragging, then adjusting 
   mode is cancelled.

   The bar has methods that you can override that are called each
   time the bar is moved, or continuously during adjustment.

   In order for the bar to highlight correctly, some ancestor of the
   "HVSplit" on which it is installed must be a "HighlightVBT". 
   Since "Trestle.Install" automatically inserts a "HighlightVBT"
   over top-level windows, you usually don't have to worry about this.
   *)

INTERFACE HVBar;

IMPORT VBT, PaintOp, Pixmap, TextureVBT;

TYPE
  T <: Public; 
  Public = TextureVBT.T OBJECT METHODS
    <* LL = VBT.mu *>
    pre(READONLY cd: VBT.MouseRec); 
    post(READONLY cd: VBT.MouseRec); 
    during(n: INTEGER);
    <* LL <= VBT.mu *>
    init(size: REAL := DefaultSize;
      op: PaintOp.T := PaintOp.BgFg;
      txt: Pixmap.T := Pixmap.Gray): T
  END;

(* The call "v.init(...)" initializes "v" as an "HVBar" with
   the given properties and returns "v".  This includes
   calling  "TextureVBT.T.init(v, op, txt)".
    
   The argument "size" gives the number of millimeters that the bar
   will occupy in the parent "HVSplit".  

   An adjusting bar "b" calls "b.pre(cd)" when it begins adjusting
   in response to a mouse click "cd".  It calls "b.during(k)" each
   time the mouse moves during dragging, where "k" is the coordinate that 
   the "lo" (i.e., west or north) edge of the bar would move to if 
   dragging were stopped at that instant.  Finally, the bar calls
   "b.post(cd)" when it stops adjusting in response to an upclick
   or chord "cd".  The "HVSplit" will be adjusted (but not redisplayed)
   before "b.post(cd)" is called. 

   The default "pre" and "during" methods highlight the position the
   bar would move to if dragging were stopped.  The default "post" 
   method removes the highlighting.  *)

CONST
  DefaultSize = 2.5;

PROCEDURE New(
    size := DefaultSize;
    op := PaintOp.BgFg;
    txt := Pixmap.Gray): T; <* LL.sup <= VBT.mu *>
(* "New(...)" is equivalent to "NEW(T).init(...)". *)

END HVBar.
