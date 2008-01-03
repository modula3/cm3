(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* File: TextureVBT.i3, coded by cgn Thu Apr 30 14:28:28 1987 *)
(* Last modified on Mon Feb 24 13:54:54 PST 1992 by muller  *)
(*      modified on Sun Nov 17 16:26:41 PST 1991 by gnelson *)
(*      modified on Wed Sep 11 10:35:45 PDT 1991 by msm *)
<*PRAGMA LL*>

(* A "TextureVBT.T" is a "VBT" that displays a texture, possibly
   colored.  Its preferred and minimum sizes are zero and its maximum 
   size is very large, in each axis.  *)

INTERFACE TextureVBT;

IMPORT VBT, PaintOp, Pixmap;

TYPE
  T <: Public;
  Public = VBT.Leaf OBJECT METHODS
    <* LL.sup <= VBT.mu *>
    init(op: PaintOp.T := PaintOp.BgFg; 
      txt: Pixmap.T := Pixmap.Solid; 
      nwAlign: BOOLEAN := FALSE): T
  END;

(* The call "v.init(...)" initializes "v" as a "TextureVBT" 
   displaying "txt" with the painting operation "op".

   The domain of "v" will be painted using the painting operation "op"
   and the texture "txt+delta", where "delta" is the origin unless
   "nwAlign" is set to "TRUE", in which case "delta" will be set to
   the northwest corner of "v".  *)

PROCEDURE New(
  op: PaintOp.T := PaintOp.BgFg; 
  txt: Pixmap.T := Pixmap.Solid; 
  nwAlign: BOOLEAN := FALSE): T;  <* LL.sup <= VBT.mu *>
(* "New(...)" is equivalent to "NEW(T).init(...)". *)

PROCEDURE Set(
  v: T;   
  op: PaintOp.T := PaintOp.BgFg; 
  txt: Pixmap.T := Pixmap.Solid;
  nwAlign: BOOLEAN := FALSE);
<* LL.sup = VBT.mu *>
(* Change "v"'s texture and mark it for redisplay. *)

PROCEDURE Get(
  v: T;   
  VAR op: PaintOp.T; 
  VAR txt: Pixmap.T;
  VAR nwAlign: BOOLEAN); <* LL.sup = VBT.mu *>
(* Fetch "v"'s texture. *)

END TextureVBT.
