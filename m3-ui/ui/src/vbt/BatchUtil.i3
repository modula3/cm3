(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* BatchUtil.def, by Greg Nelson and Mark Manasse. *)
(* Last modified on Mon Oct  4 11:36:04 PDT 1993 by sfreeman *)
(* modified on Mon Feb 24 13:56:41 PST 1992 by muller *)
(* modified on Thu Dec 12 0:02:43 PST 1991 by gnelson *)
<*PRAGMA LL*>

(* This interface provides operations to clip and translate a batch of
   painting commands.  It is useful to those who are implementing window
   classes with customized painting behavior.

   Don't apply these procedures to a batch whose contents are concurrently
   being read or written. *)

INTERFACE BatchUtil;

IMPORT Batch, Rect, Point, PaintPrivate;

PROCEDURE GetLength (ba: Batch.T): CARDINAL;
(* Return the number of "Word.Ts" in use in "ba". *)

PROCEDURE Copy (ba: Batch.T): Batch.T;
(* Allocate and return a new batch initialized with a copy of "ba". *)

(* Every entry in a batch has a clipping rectangle; there is also a
   clipping rectangle for the batch as a whole.  The effective clipping
   rectangle for a painting operation is the intersection of its clipping
   rectangle with its batch's clipping rectangle. *)

PROCEDURE GetClip (ba: Batch.T): Rect.T;
(* Return "ba"'s clipping rectangle. *)

TYPE ClipState = {Unclipped, Clipped, Tight};

PROCEDURE GetClipState (ba: Batch.T): ClipState;
(* Return "ba"'s clipping state. *)

(* If "GetClipState(ba)" is "Clipped" then the clipping rectangle of every
   painting operation in "ba" is a subset of "GetClip(ba)".  If
   "GetClipState(ba)" is "Tight" then "GetClip(ba)" is equal to the join of
   the clipping rectangles of the painting operations in "ba".  If
   "GetClipState(ba)" is "Unclipped", there is no particular relationship
   between "ba"'s clipping rectangle and the clipping rectangles of the
   entries in "ba". *)

PROCEDURE Meet (ba: Batch.T; READONLY clip: Rect.T);
(* Set "ba"'s clipping rectangle to "Rect.Meet(GetClip(ba), clip)". *)

(* If the assignment is non-trivial, this will change the clip state of
   "ba" to be "Unclipped". *)

PROCEDURE Clip (ba: Batch.T);
(* Apply "ba"'s clipping rectangle to each operation. *)

(* That is, if "GetClipState(ba)" is "Unclipped", then for each painting
   operation in "ba", "Clip" replaces the clipping rectangle of the
   operation with the meet of the rectangle and "GetClip(ba)", and sets the
   clipstate of "ba" to "Clipped". *)

PROCEDURE Tighten (ba: Batch.T);
(* Achieve "ba.clipped = Tight" without changing the effect of "ba". *)

(* That is, "Tighten(ba)" is equivalent to "Clip(ba)" followed by assigning
   to "ba"'s clipping rectangle the join of the resulting clipping
   rectangles of the entries in "ba". *)

PROCEDURE Translate (ba: Batch.T; READONLY delta: Point.T);
(* Translate "ba" by "delta". *)

(* That is, for each painting operation in "ba", translate the target of
   the painting operation by "delta".  This always involves translating the
   clipping rectangle of the operation by "delta".  It also adds "delta" to
   the "delta" components of all textures and to the reference point of
   "TextComs".  It adjusts the "p1", "p2", "vlo", and "vhi" fields of
   "TrapComs".  The relative displacement of a scrolling command is not
   affected; that is, both the source and target of the scroll are
   translated by "delta".  The clipping rectangle of the batch is also
   translated. *)

PROCEDURE ByteSwap (ba: Batch.T);
(* Convert all text painting operations in "ba" to have the same byteorder
   as "PaintPrivate.HostByteOrder". *)

PROCEDURE Succ (ba: Batch.T; cptr: PaintPrivate.CommandPtr):
  PaintPrivate.CommandPtr;
(* Return the pointer to the entry in "ba" that follows the one pointed to
   by "cptr". *)

(* "Succ(ba, NIL)" returns the first entry in "ba"; "Succ(ba, cptr) = NIL"
   when "cptr" is the last entry in "ba".  To visit each entry in the batch
   "ba", use a loop like this:

| cptr := BatchUtil.Succ(ba, NIL);
| WHILE cptr # NIL DO
|   CASE cptr.command OF ... END;
|   cptr := BatchUtil.Succ(ba, cptr)
| END

   The "PaintPrivate" interface explains the format of the entries. *)

PROCEDURE SetPicture (ba: Batch.T);
(* mark the batch as containing pictures *)

END BatchUtil.
