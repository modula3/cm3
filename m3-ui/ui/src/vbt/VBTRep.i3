(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* VBTRep.def, code Sun Aug 11 12:18:43 1985 by Greg Nelson *)
(* Last modified on Mon Dec 21 17:55:03 PST 1992 by msm     *)
(*      modified on Mon Feb 24 13:59:00 PST 1992 by muller  *)
(*      modified on Thu Jan 23 10:02:46 PST 1992 by gnelson *)
(*      modified on Tue Jul 24 13:15:10 PDT 1990 by steveg *)

<*PRAGMA LL*>

(* The "VBTRep" interface defines the representation of "VBTs", and provides
   operations that are useful for implementing low-level or esoteric
   split classes. *)

INTERFACE VBTRep;

IMPORT Batch, Cursor, PropertyV, Rect, Region, 
  ScreenType, ScrnPaintOp, ScrnCursor, ScrnPixmap, 
  ScrnFont, VBT, VBTClass, Word, PaintPrivate, Axis, Palette,
  PaintOp, Font, Pixmap;

CONST EmptyCage = VBT.EmptyCage;

TYPE
  MiscRef = REF MiscRec;
  MiscRec = RECORD
    cage := EmptyCage;
    badRgn := Region.Empty;
    rpseqno: Word.T;
    oldDomain := Rect.Empty;
    link: MiscRef := NIL
  END;

(* To save space in a "VBT", the "cage", "badRgn", "rpseqno", and 
   "oldDomain" fields are only stored if at least one of them has an 
   ``unusual'' value.  This is achieved by including a "MiscRef" in the 
   "VBT" object.  If the "MiscRef" is "NIL", then the "badRgn" and "oldDomain"
   are empty, the "rpseqno" is irrelevant, and the cage is determined
   from the "cagetype" field as follows: if the "cagetype" is
   "VBT.CageType.Rectangle", then the cage rectangle is assumed to be
   "Rect.Empty"; otherwise the rectangle is irrelevant.
   
   The "rpseqno" field is the {\it repainting sequence number}.  It is
   incremented whenever the "badRgn" is expanded and recorded before
   activating a "repaint" method.  Thus when the repaint method returns,
   the current value can be compared with the recorded value to
   determine whether the current "badRgn" is the one that the "repaint"
   method responded to, or whether a new bad region arrived while the
   client was responding to the old one.  *)

TYPE
  Prop =
    {EscapePending, Reshaping, RepaintPending, OnQ, 
     Covered, Combiner, ShortCircuit, CageCovered, 
     Marked, ExcessBegins, HasNewShape, BlockNewShape,
     EscapeCovered};
  Props = SET OF Prop;

CONST
  AllProps = Props{FIRST(Prop)..LAST(Prop)};
  NoProps = Props{};

(* Here is the meaning of the properties:

   "EscapePending": Set when a thread of control is forked to deliver a
   cage escape to gone. Set to "FALSE" when any position is delivered.

   "Reshaping": set when the "VBT" has a non-empty old domain.

   "RepaintPending": Set when a thread of control exists that will deliver
   the bad region.

   "OnQ": Set when the "VBT" is on the "Metermaid"'s to-be-serviced queue.

   "Covered":  Ordinarily painting into an empty batch will put the
   "VBT" on the "Metermaid"'s queue and set "onQ".  If the "covered"
   bit is set, this will not happen.  For example, "PutPosition" sets
   "covered" before passing the position to child, and clears it and
   forces the batch before it returns.  Only set during event delivery,
   so any action that could be deferred until after event delivery can
   check "covered" and clean up on method exit.

   "Combiner": Set to indicate that this "VBT" is a good place to pile up
   small paint batches, even if it is not covered. Trestle sets this bit
   in one "VBT" near the root of a client address space to avoid shipping
   many small batches across "RPC".

   "ShortCircuit":  Set on a "VBT" when painting on the VBT can be
   implemented by clipping to the "VBT"'s domain and painting on its 
   parent.

   "Marked": set by "VBT.Mark".

   "CageCovered": "VBTClass.PutPosition" sets this bit on a "VBT" before 
   calling its position method; after calling the position method, 
   the procedure relays the child's cage to the parent and clears 
   the bit.  "VBTClass.SetCage" notices the bit and omits relaying the 
   cage to the parent.  

   "ExcessBegins": Set when "excessBegins > 0".

   "HasNewShape" is set when "VBT.NewShape" is called, and cleared by
   a call to "VBTClass.HasNewShape" or "VBTClass.GetShape". 

   If "BlockNewShape" is set, "VBT.NewShape" calls will not be relayed
   to the parent of the "VBT".
 *)

REVEAL VBT.Prefix = 
  VBTClass.Prefix BRANDED OBJECT
    <* LL >= {SELF} *>
    cursor := Cursor.DontCare;
    cageType: (*BITS 16 FOR*) VBTClass.VBTCageType
      := VBTClass.VBTCageType.Gone;
    props: (*BITS 16 FOR*) Props := NoProps;
    batch: Batch.T := NIL;
    remaining: INTEGER := 0; 
    propset: PropertyV.Set := NIL;
    miscRef: MiscRef := NIL;
  OVERRIDES
    getcursor := GetcursorDefault;
    axisOrder := AxisOrderDefault;
  END;

(* The "batch" field contains the batch of uncompleted painting commands
   for the "VBT", and "remaining" contains the number of free addressable
   units remaining in the "batch".  In particular, if "remaining # 0", the
   "batch" field is not "NIL".  The "miscRef" field is always "NIL"
   if the parent is "NIL".  *)

REVEAL VBT.ScreenType <: STPub;

TYPE STPub = 
  ScreenType.Public OBJECT
    ops: REF ARRAY OF ScrnPaintOp.T;
    cursors: REF ARRAY OF ScrnCursor.T;
    pixmaps: REF ARRAY OF ScrnPixmap.T;
    fonts: REF ARRAY OF ScrnFont.T
  METHODS
    opApply(cl: Palette.OpClosure; op: PaintOp.T): ScrnPaintOp.T;
    cursorApply(cl: Palette.CursorClosure; cs: Cursor.T): ScrnCursor.T;
    pixmapApply(cl: Palette.PixmapClosure; pm: Pixmap.T): ScrnPixmap.T;
    fontApply(cl: Palette.FontClosure; ft: Font.T): ScrnFont.T
  END;

(* The tables "st.ops", "st.fonts", "st.cursors", and "st.pixmaps" are
   collectively called the screentype's {\it palette}.  They are used
   to translate between screen-independent resources and
   screen-dependent resources.  For example, recall that "Pixmap.Gray"
   is a record containing the integer field "Pixmap.Gray.pm".  The
   screen-dependent equivalent of "Pixmap.Gray" on the screentype "st"
   is simply "st.pixmaps[Pixmap.Gray.pm]".  When creating the palette,
   the above apply methods are called for all resources; if cl is NIL, the
   resource is built-in.  The default values for these return the result of
   invoking the closure or the built-in method; your procedure must not
   return NIL when invoked on a built-in. *)
   
TYPE OffscreenType = VBT.ScreenType OBJECT st: VBT.ScreenType END;

(* An "OffscreenType" "s", is a screen type that is derived from the screen
   type "s.st".  An OffscreenType will be replaced by its associated
   screentype in calls to "Trestle.InstallOffscreen".  The "st" field
   is read-only after creation. *)

PROCEDURE CheckMisc(v: VBT.T); <* LL >= {v} *>
(* Set "v.misc := NIL" if "v"'s "badRgn" and "oldDomain" are empty and its
   cage type is not "Rect". *)

PROCEDURE CreateMisc(v: VBT.T); <* LL >= {v} *>
(* If "v.misc = NIL", then create a "misc" for "v" with empty "badRgn" and
   "oldDomain", and with appropriate cage. Otherwise, do nothing. *)

PROCEDURE DestroyMisc(v: VBT.T);
<* LL >= {v, v.parent} *>
(* Set "v"'s misc to "NIL", clearing "Reshaping" from "v.props". *)

PROCEDURE NewBatch(v: VBT.T; len: INTEGER := -1); 
<* LL.sup = v *>
(* Force "v"'s batch if it is non-nil and allocate a new batch for it
   of size at least "len", or of size "VBTTuning.BatchSize" if "len=-1".  *)

PROCEDURE ForceBatch(v: VBT.T); <* LL.sup = v *>
(* Force "v"'s batch if it is non-nil, and leave it nil. *)

PROCEDURE CancelBatch(v: VBT.T); <* LL.sup = v *>
(* Free "v"'s batch and set it to "NIL". *)

PROCEDURE Enqueue(v: VBT.T); <* LL.sup = v *>
(* Place "v" on the list of "VBTs" scheduled to be serviced by the
   MeterMaid. *)

PROCEDURE GetcursorDefault(v: VBT.Prefix): ScrnCursor.T;
(* Return the result of resolving "cursor(v)" using "v"'s screentype. *)

PROCEDURE AxisOrderDefault(v: VBT.Prefix): Axis.T;
(* Return "Axis.T.Hor". *)

PROCEDURE ExpandBadRect(w: VBT.T; 
  READONLY clp: Rect.T; ba: Batch.T);
<* LL.sup = w *>
(* Expand "w"'s bad region for "ba". *)

(* In "ExpandBadRect", the rectangle "clp" is the original clipping
   rectangle for "ba", before intersection with "w.domain".  The
   expansion is caused by (a) using out-of-domain bits as source (b)
   painting into the old domain (c) scrolling an existing bad rectangle.
   *)
     
PROCEDURE ExtendBatch(v: VBT.T; VAR ba: Batch.T);
(* Extend "v"'s batch to include the painting operations in "ba", and free
   "ba". It is assumed that "v" has a non-empty batch which has room for
   the extension. *)

PROCEDURE MaxRepeat(v: VBT.T): CARDINAL;
<* LL.sup = v *>
(* Return the number of RepeatRec's that can fit in "v"'s current batch.
   *)

PROCEDURE PaintRepeat(v: VBT.T; 
  READONLY clip: ARRAY OF Rect.T);
<* LL.sup = v *>
(* Add a "RepeatRec" to "v"'s batch for each rectangle in "clip". *)

(* "PaintRepeat" is a checked run-time error if there isn't enough space
   in "v"'s batch.  Calling "PaintRepeat" does not call
   "Enqueue(v)". *)

PROCEDURE PaintSingle(v: VBT.T; READONLY clip: Rect.T; 
  com: PaintPrivate.CommandPtr); <* LL.sup = v *>
(* Add the paint operation referenced by "com" to "v"'s batch, but
   use the clipping rectangle "clip" instead of the one in "com". *)

(* "PaintSingle" forces "v"'s batch if necessary and allocates a new
   one.  It does not call "Enqueue(v)".  The command "com" must not be a
   scroll command.  *)

PROCEDURE Scroll(v: VBT.T; READONLY clip: Rect.T; 
  com: PaintPrivate.ScrollPtr); <* LL.sup = v *>
(* Like "PaintSingle", but "com" must be a scroll command. *)

PROCEDURE Mark(v: VBT.T); <* LL >= {v} *>
(* Identical to "VBT.Mark" except for the locking level. *)

PROCEDURE Redisplay(); <* LL.sup = VBT.mu *>
(* Redisplay and unmark all marked windows whose screentype is non-"NIL".  *)

(* That is, "Redisplay" is equivalent to this loop:

| LOOP
|   WITH m = `an array containing all marked windows` DO
|     IF NUMBER(m) = 0 THEN EXIT END;
|     `Sort "m" in order of non-decreasing depth`;
|     FOR i := 0 TO LAST(m) DO 
|       IF IsMarked(m[i]) AND m[i].st # NIL THEN
|          Unmark(m[i]);
|          m[i].redisplay() 
|       END
|     END
|   END
| END

    The depth of a window is the number of parent pointers that must
    be followed to reach "NIL".  Sorting by depth guarantees that
    ancestors will be redisplayed before their descendants.  The reason
    is that redisplaying an ancestor window often reshapes its
    descendants, and if a descendant is going to be reshaped it would
    be wasteful to redisplay it in its old position.
    
   Ordinarily when a window is marked, a thread is forked that will
   call "Redisplay".  This is wasteful if "Redisplay" will be called
   soon anyway.  Therefore, if you know that "Redisplay" will be called
   soon, you can call "CoverRedisplay", which increments a ``coverage
   counter''.  If the coverage counter is non-zero, marking a "VBT"
   does not fork a thread.  Of course by calling "CoverRedisplay" you
   acquire the obligation to ensure that "Redisplay" will be called
   soon.  Calling "UncoverRedisplay" decrements the counter and calls
   "Redisplay" if the result is zero.  *)
    
PROCEDURE CoverRedisplay(); <* LL.sup = VBT.mu *>
(* Increment the redisplay coverage counter. *)

PROCEDURE UncoverRedisplay(); <* LL.sup = VBT.mu *>
(* Decrement the redisplay coverage counter and call "Redisplay" if
   the result is zero.  *)

END VBTRep.
