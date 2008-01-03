(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Jul 27 12:28:33 PDT 1995 by mhb                      *)
(*      modified on Mon Feb  8 16:27:42 PST 1993 by steveg                   *)
(*      modified on Mon Feb 24 13:55:12 PST 1992 by muller                   *)
<*PRAGMA LL*>

(* An "OffsetVBT.T" is a filter that maintains a translation
   between the coordinate systems of the child and parent such
   that the child's coordinate system has its origin at the
   northwest corner of the child domain.

   The child's domain has its northwest corner at (0, 0) and its
   width and height correspond to the child's preferred size.

   The parent's domain has its northwest corner at an arbitrary
   offset from the child's.

   The parent's domain may be a different size than the child's.
   Any portion of the child's domain that isn't visible is
   clipped and any portion of the parent's domain that doesn't
   show a portion of the child, displays in a background color.

   The child can be "NIL", in which case the "OffsetVBT" ignores
   all events. *)

INTERFACE OffsetVBT;

IMPORT VBT, Filter, PaintOp;

TYPE
  T <: TPublic;
  TPublic =
    Filter.T OBJECT
    METHODS
      init (ch: VBT.T; north, west: REAL := 0.0; bg: PaintOp.T := PaintOp.Bg): T;
      move (north, west: REAL);  <* LL = VBT.mu *>
    END;

(* The call "v.init(ch)" initializes "v" as an "OffsetVBT" with
   child "ch".  And the northwest corner of the parent is aligned
   at co-ordinate ("north", "west") mm in the child domain. 

   The call "v.move(north, west)" adjusts the co-ordinate system
   so that the "v"s northwest corner is positioned at co-ordinate
   ("north", "west") millimeters in the child domain.  This
   offset will be maintained as "v" is reshaped or rescreened. *)


END OffsetVBT.

