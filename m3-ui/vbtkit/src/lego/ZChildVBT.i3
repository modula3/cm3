(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Sep  7 16:54:31 PDT 1993 by mhb                      *)
(*      modified on Fri Jun 11 15:54:12 PDT 1993 by meehan                   *)
(*      modified on Tue Jun 16 13:07:59 PDT 1992 by muller                   *)
(*      modified on Wed Feb 26 00:21:41 PST 1992 by steveg                   *)
<* PRAGMA LL *>
<* PRAGMA SUBTYPE *>

(* An "ZChildVBT.T" is a VBT that is typically used as a subwindow.

   A "ZChildVBT" is a subclass of a "HighlightVBT" that insulates any
   highlighting done in the "ZChildVBT" from highlighting done in
   other subwindows.  Clients should use a "ZBackgroundVBT" around the
   background child in order to insulate highlighting in the
   background child from highlighting in the subwindows.

   There are two alternate ways to initialize a "ZChildVBT". Each
   allows the client to specify whether the subwindow should be
   initially visible (``mapped'') and how the subwindow should
   be reshaped when the parent "ZSplit" is reshaped.

   The method call "v.init(...)" allows the client to specify where
   the center or a corner of "v" should be placed, relative to the
   parent, either in absolute distance (in millimeters) from the
   parent's northwest corner ("CoordType.Absolute"), or as percentages
   of the parent's width and height ("CoordType.Scaled").  The default
   is to align the center of "v" with the center of the parent. The size
   of "v" is its preferred sizes in both the horizontal and vertical
   dimensions.

   The method call "v.initFromEdges(...)" allows the client to specify
   the edges of "v", either in absolute distance (in millimeters) from
   the parent's northwest corner (this is the only case in which the
   client specifies the absolute size of "v"), or as percentages of
   the parent's width and height.

   The implementation will not pop up a subwindow with its northwest
   corner north or west of the visible portion of the "ZSplit" parent;
   it will override the specified position as necessary to bring it
   into view. It is a checked runtime error to specify scaled
   coordinates (percentages) that are outside the range 0.0--1.0.
   If the specified position is overriden, or if the subwindow is 
   not entirely visible when the subwindow is first made visible, 
   the implementation will also override the reshape method so that 
   the subwindow will be repositioned using the information specified
   when it was initialized.

   Finally, in order for the reformatting to meet specifications
   above, the client must call "Inserted" after the subwindow is
   inserted as a child of a "ZSplit"; the client must call "Moved"
   after the subwindow is repositioned by the user; and the client
   must call "Grew" after the size of the subwindow is changed by the
   user. *)

INTERFACE ZChildVBT;

IMPORT HighlightVBT, VBT, ZSplit;

TYPE
  Location = {NW, NE, SW, SE, Center};
  CoordType = {Absolute, Scaled};
  T <: Public;
  Public = HighlightVBT.T OBJECT
           METHODS
             <* LL <= VBT.mu *>
             init (ch  : VBT.T;
                   h, v          := 0.5;
                   loc           := Location.Center;
                   type          := CoordType.Scaled;
                   shaper: ZSplit.ReshapeControl := NIL;
                   open: BOOLEAN                 := TRUE): T;
             initFromEdges (ch: VBT.T;
                            w, e, n, s: REAL;
                            type := CoordType.Absolute;
                            shaper: ZSplit.ReshapeControl := NIL;
                            open := TRUE): T;
           END;

PROCEDURE InitiallyMapped (v: VBT.T): BOOLEAN;
(* If "v" is a "ZChild", return the value of "open" when it was
   initialized.  Otherwise, return "TRUE". *)

PROCEDURE Pop (v: VBT.T; forcePlace := FALSE);
(* Map "v" and lift it to the top of its parent's children.  If
   "forcePlace" is set, position "v" at its initial location. *)

PROCEDURE Inserted (v: VBT.T);
(* The client must call this procedure after "v" has been inserted
   into a "ZSplit".  This procedure sets a "ReshapeControl" object on
   "v".  If "v" isn't a "ZChildVBT", the "ReshapeControl" tries to
   center "v", subject to the contraint that its northwest corner is
   just visible.  If "v" is a "ZChild", the "ReshapeControl" will
   follow "vbt"'s initial position until "v" is moved by the user
   (usually because "Moved" is called).  At that point, the northwest
   corner of "v" will remain at that position relative to its parent,
   until the user moves it again. *)

PROCEDURE Moved (v: VBT.T);
(* The client must call this procedure after "v" has been moved by a
   user.  If "v" is a "ZChildVBT", this procedure notes that "v" has
   been moved by the user, so that the next time it is reshaped, "v"
   will retain its current position relative to its parent.  If "v"
   isn't a "ZChildVBT", this procedure is a no-op. *)

PROCEDURE Grew (v: VBT.T; w, h: INTEGER);
(* The client must call this procedure after the size of "v" has been
   changed to "w"-by-"h" (in pixels) by a user.  If "v" is a
   "ZChildVBT", this procedure notes that "v" has a new shape and
   calls "VBT.NewShape" to tell the parent "ZSplit".  Subsequently,
   "v" will report its shape as "w"-by-"h".  If "v" is not a
   "ZChildVBT", this procedure is a no-op. *)


(* Finally, here are a few "ZSplit" reshape controllers
   that are sometimes useful:  *)

VAR (*CONST*) 
  Scaled: ZSplit.ReshapeControl;
  ScaledHFixed: ZSplit.ReshapeControl;
  ScaledVFixed: ZSplit.ReshapeControl;
  ScaledHVFixed: ZSplit.ReshapeControl;

(* "Scaled" reshapes the child by 
   scaling the old child domain to occupy the same relative position
   of the changing parent domain. "ScaledHFixed" does the same, and then
   insets the west and east edges so that the child's width is not
   changed. Similarly, "ScaledVFixed" scales the child's domain and then
   insets the north and south edges. "ScaledHVFixed" insets both the
   north and south edges and the west and east edges so the size of
   the child's domain stays fixed. In other words, "ScaledHVFixed" can
   be used to reposition the center point of the child without changing
   its size. *)

END ZChildVBT.
