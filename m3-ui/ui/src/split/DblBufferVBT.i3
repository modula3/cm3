(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Jan 18 16:46:24 PST 1996 by heydon                   *)
<* PRAGMA LL *>

INTERFACE DblBufferVBT;

(* A "DblBufferVBT.T" is a filter that redirects the painting operations of
   its child to an off-screen buffer, and then updates its screen from the
   buffer when the child's "sync" method is invoked. This can be accomplished
   by calling the "VBT.Sync" procedure with the child or any of the child's
   descendants as argument. *)

IMPORT VBT, Filter;

TYPE T <: Filter.T;

(* The call "NEW(DblBufferVBT.T).init(ch)" returns a newly initialized
   double-buffer VBT with child "ch". *)

(* The child coordinate system of a double-buffer VBT is a translation of its
   parent's coordinate system. You can compute the translation vector between
   the parent and child by subtracting the northwest corners of their domains.

   A double-buffer VBT "v" does not forward repaint events to its child;
   instead, it repaints by copying from the off-screen buffer.

   In addition to its off-screen buffer, a "DblBufferVBT.T" maintains a
   \it{saved buffer} and provides operations for copying the off-screen buffer
   to and from the saved buffer. This is convenient for building up a
   background to be restored on each frame of an animation, for example.
   The initial content of the saved buffer is a conceptually infinite pixmap
   of background pixels.

   Here are the procedures for saving, restoring, and clearing the saved
   buffer: *)

PROCEDURE Save(v: VBT.T); <* LL.sup < v *>
(* Requires that some proper ancestor of "v" be a "T". Sets the saved buffer
   of the first such ancestor to be a copy of its off-screen buffer. *)

PROCEDURE Restore(v: VBT.T); <* LL.sup < v *>
(* Requires that some proper ancestor of "v" be a "T". Sets the off-screen
   buffer of the first such ancestor to be a copy of its saved buffer. *)

(* "Save(v)" and "Restore(v)" force all painting operations (paint
   \it{batches}, in Trestle terminology) from "v" up to the relevant
   off-screen buffer. This will work smoothly if "v" is the only leaf
   decendant of the relevant double buffer (i.e., if all splits between them
   are filters). Otherwise, you may get the wrong answer due to unforced paint
   batches on other leaf decendants. *)

PROCEDURE ClearSaved(v: VBT.T); <* LL.sup < v *>
(* Requires that some proper ancestor of "v" be a "T". Clears the saved buffer
   of the first such ancestor to contain an infinite pixmap of background
   pixels. *)

END DblBufferVBT.
