(* Copyright (C) 1992 Digital Equipment Corporation.               *)
(* All rights reserved.                             *)
(* See the file COPYRIGHT for a full description *)
(* ZShape.i3, coded Fri Oct 31 11:24:53 1986 by cgn *)
(* Last modified on Wed Feb 26 18:58:50 1992 by msm     *)
(*      modified on Wed Dec 11 18:29:58 PST 1991 by gnelson *)
(*      modified on Fri Feb  2 14:08:01 PST 1990 by glassman *)

<*PRAGMA LL*>

(* A "ZShape.T" is a parent window with overlapping non-rectangular
   child windows.  This generalizes ZSplit by refining the notion
   of domain to exclude bits that the child doesn't want. *)
   
INTERFACE ZShape;

IMPORT VBT, Region, ZSplit;

TYPE 
  T <: ZSplit.T;

PROCEDURE New(
    bg: VBT.T := NIL;
    saveBits := FALSE;
    parlim: INTEGER := -1)
    : T; <* LL <= VBT.mu *>
(* "New(...)" is equivalent to "NEW(T).init(...)". *)

(* \subsubsection{Getting domains} *)


PROCEDURE GetDomain(ch: VBT.T): Region.T;
<* LL.sup = VBT.mu *>
(* Return the effective domain of "ch". *)

(* \subsubsection{Restricting the domain to a region} *)

(* You can supply procedures to control what portions of a child's
   domain are excluded when the child is reshaped.  If you don't
   supply a procedure, the child will be rectangular.  If you do,
   the procedure can return a region whose bounding box is equal
   to the offered rectangle. *)

PROCEDURE SetRegionControl(
    ch: VBT.T;
    rc: RegionControl); <* LL.sup = VBT.mu *>
(* Set the region control object for the child "ch" to be "rc".  *)

(* Setting the RegionControl to NIL causes ch to be rectangular.  The
   RegionControl will be called after a child call to NewShape, a call
   to ZSplit.Move, a call to the ZSplit ReshapeControl procedure, or
   after a change in the RegionControl. *)

TYPE RegionControl = OBJECT METHODS
  apply(ch:VBT.T; READONLY dom: Rect.T)
  : Region.T <* LL.sup = VBT.mu.ch *>
END;

END ZShape.
