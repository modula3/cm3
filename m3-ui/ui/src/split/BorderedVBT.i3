(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* File: BorderedVBT.i3, by cgn, Tue Apr 21 22:00:25 1987 *)
(* Last modified on Mon Feb 24 13:52:30 PST 1992 by muller  *)
(*      modified on Wed Dec 11 18:31:39 PST 1991 by gnelson *)
(*      modified on Wed Sep 11 15:02:21 PDT 1991 by msm *)
<*PRAGMA LL*>

(* A "BorderedVBT.T" is a filter whose parent's screen consists of the
   child's screen surrounded by a border.  The parent's shape is determined 
   from the child's shape by adding the border size.  *)

INTERFACE BorderedVBT;

IMPORT VBT, Filter, PaintOp, Pixmap;

TYPE
  T <: Public;
  Public = Filter.T OBJECT METHODS 
    <* LL.sup <= VBT.mu *>
    init(ch: VBT.T; 
      size: REAL := Default;
      op: PaintOp.T := PaintOp.BgFg;
      txt: Pixmap.T := Pixmap.Solid): T
  END;

(* The call "v.init(...)" initializes  "v" as a "BorderedVBT" with 
   child "ch" and marks "v" for redisplay. 

   The border size is given in millimeters.  The border will be painted
   in the untranslated texture "txt" using the paint op "op".  *)
   
CONST Default = 0.5;

PROCEDURE New(
    ch: VBT.T;
    size: REAL := Default;
    op: PaintOp.T := PaintOp.BgFg;
    txt: Pixmap.T := Pixmap.Solid)
    : T; <* LL.sup <= VBT.mu *>
(* "New(...)" is equivalent to "NEW(T).init(...)". *)


PROCEDURE SetSize(v: T; newSize: REAL); 
<* LL.sup = VBT.mu *>
(* Change the size of the border of "v" to "newSize" millimeters and
   mark "v" for redisplay.  *)


PROCEDURE SetColor(
  v: T; 
  op: PaintOp.T; 
  txt := Pixmap.Solid);
<* LL.sup = VBT.mu *>
(* Change the "op" and "texture" of "v" and mark "v" for redisplay. *)

PROCEDURE Get(
  v: T; 
  VAR size: REAL; 
  VAR op: PaintOp.T; 
  VAR txt: Pixmap.T); <* LL.sup = VBT.mu *>
(* Fetch "v"'s parameters. *)

END BorderedVBT.
