(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Sat Feb  6 18:42:14 PST 1993 by meehan *)
(*      modified on Tue Feb  2 14:30:16 PST 1993 by mhb    *)
(*      modified on Tue Jun 16 13:08:13 PDT 1992 by muller *)
(* modified on Fri Mar 27 02:12:01 1992 by steveg*)
(* modified on Mon Jun 18 16:11:27 PDT 1990 by birrell *)
<* PRAGMA LL *>
<* PRAGMA SUBTYPE *>

(* A "SplitterVBT.T" is a parent window that partitions its screen
   into a row or column of children windows, depending on the
   {\em axis} of the split, with adjusting bars between all
   children.  The adjusting bars allow the user to adjust the
   allocation of screen real estate among the splitter's
   children, subject to the size constraints of each child.

   A "SplitterVBT" is subclass of an "HVSplit", but through the
   "MultiSpit" interface, only the ``interesting'' children of the
   "HVSplit" are exposed.  That is, adjusting bars are never
   exposed to the client: they are inserted automatically when a
   new child is added, and removed as necessary.  To access all
   children, including the adjusting bars, use the "Split"
   interface instead.  The "HVSplit" routines "Move", "Adjust",
   "FeasibleRange", "AvailSize", and "AxisOf" can be used. *)

INTERFACE SplitterVBT;

IMPORT Axis, HVSplit, PaintOp, Pixmap;

TYPE
  <* SUBTYPE T <: MultiSplit.T *>
  T <: Public;
  Public = HVSplit.T OBJECT
           METHODS
             <* LL <= VBT.mu *>
             init (hv      : Axis.T;
                   size    : REAL      := DefaultSize;
                   op      : PaintOp.T := PaintOp.BgFg;
                   txt     : Pixmap.T  := Pixmap.Gray;
                   saveBits: BOOLEAN   := FALSE;
                   parlim  : INTEGER   := -1            ): T;
           END;

(* The call "v.init(...)" initializes "v" as a "SplitterVBT" with
   no children.  See the "HVSplit" interface for an explanation
   of parameters "saveBits" and "parlim".  See the "HVBar"
   interface for an explanation of the "size", "op", and "txt"
   parameters. *)

CONST
  DefaultSize = 2.0;

END SplitterVBT.



