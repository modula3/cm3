(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* Last modified on Fri Oct  8 09:51:04 PDT 1993 by sfreeman *)
(* modified on Sat Mar 21 02:53:17 1992 by msm *)
(* modified on Mon Feb 24 13:58:34 PST 1992 by muller *)
(* modified on Sat Dec 21 16:38:43 PST 1991 by gnelson *)

(* modified on Tue Aug 7 17:12:32 PDT 1990 by steveg *)
<*PRAGMA LL*>

(* The "VBTClass" interface specifies the up methods, the split methods,
   and the wrapper procedures by which a parent activates a child's down
   methods.

   In general, to implement a split or filter you override the down
   methods, up methods, and split methods of the parent.  However, usually
   you will be able to inherit the majority of the methods from existing
   classes, and only have to override a few of them.  We mention several
   groups of methods that in most cases you will want to inherit rather
   than reimplement.

   The two down methods

| VBT.Split.mouse
| VBT.Split.position

   together with the two up methods

| VBT.Split.setcage
| VBT.Split.setcursor

   conspire to implement the mouse-cage semantics described in the "VBT"
   interface for delivering mouse clicks and cursor positions and for
   setting the cursor shape.  They work for any "VBT.Split", and there is
   almost never any reason to override them.  As a far-fetched example of
   when you would override them, imagine a filter that converts shifted
   left button clicks to right button clicks.

   Although you probably won't want to override these methods, you will
   have to help them a bit.  They cache the results of the "locate" method,
   and therefore require that you call "VBTClass.LocateChanged" whenever
   the geometry of your split changes in a way that affects the locate
   method.

   The up methods

| VBT.Split.acquire
| VBT.Split.release
| VBT.Split.put
| VBT.Split.forge
| VBT.Split.readUp
| VBT.Split.writeUp

   implement the event-time semantics described in the "VBT" interface.
   They simply recurse up the tree of "VBT"s.  At the root the recursive
   calls reach a "VBT" in which these methods are overridden to make the
   appropriate X calls.  There is rarely any reason to override these
   methods.  As an example of when you might want to override them, imagine
   keeping track of which "VBT" in your application last held the keyboard
   focus.  You could do this by introducing a filter whose "acquire" method
   recorded the information before recursing on the parent.

   Keystrokes and miscellaneous codes can skip levels of the tree when they
   are delivered.  For example, associated with each top-level window is a
   filter much like the one just described, which keeps track of which of
   its decendants are selection owners.  This filter forwards keystrokes
   and lost codes directly to the appropriate owner, bypassing the
   intermediate windows in the tree.

   The up methods

| VBT.Split.paintbatch
| VBT.Split.capture
| VBT.Split.sync

   implement painting, painting synchronization, and screen capture.  The
   "sync" and "capture" methods recurse up the tree in the obvious way.
   The "paintbatch" method also recurses up the tree, but in a less obvious
   way.

   It would be too inefficient to call a method for every painting command;
   therefore the class-independent painting code groups painting commands
   into batches and hands them to the method a batch at a time.  For
   example, the "paintbatch" method of a "ZSplit" clips the batch of
   painting commands to the visible portion of the child's domain and then
   executes the clipped operations on itself.

   Painting on the vast majority of "VBT"s can be implemented simply by
   clipping to their domain and then relaying the painting to their parent.
   To speed up this common case, every "VBT" has a {\it short-circuit} bit.
   If this bit is set then Trestle doesn't call the "VBT"'s "paintbatch"
   method at all; it just clips to the "VBT"'s domain and paints on its
   parent.  Typically the only "VBT"s whose short-circuit bits are not set
   are the root "VBT" and those "ZSplit" children that are overlapped by
   other children or that extend outside the parent's domain.

   If the short-circuit bits are set on all the "VBT"s from "v" to the
   root, then the class-independent painting code will relay batches of
   painting commands from "v" to the root without activating any methods.
   The "paintbatch" method at the root translates the batch of painting
   commands into the appropriate X operations.

   The default method "VBT.Split.paintbatch" sets the short-circuit bit and
   recurses on the parent.  In the unlikely event that you want to override
   this method, the interfaces "Batch", "BatchUtil", and "PaintPrivate"
   define the representation of painting commands in batches.  You could
   for example overriding the paintbatch method to implement a class of
   "VBT" that paints into a raw pixmap in your address space.

   To speed up painting, Trestle does not rely on garbage collection for
   paintbatches: you must free them explicitly.

   You almost never need to implement the split methods "succ", "pred",
   "move", "nth", "index", and "locate"; on the other hand you must be
   careful to inherit them from the right place.  There are two main
   subtypes of "VBT.Split", filters and ``proper'' splits, and they have
   different suites of split methods.  The implementations of the split
   methods for filters are

| Filter.T.succ
| Filter.T.pred
| Filter.T.move
| Filter.T.nth
| Filter.T.index
| Filter.T.locate

   These are all quite trivial procedures, since a filter has at most one
   child.  If you declare a split as a subtype of "Filter.T", you inherit
   these methods automatically.

   Most proper splits are subtypes of "ProperSplit.T", which keeps the
   children in a doubly-linked list.  For example, "ZSplits", "HVSplits",
   "TSplits", and "PackSplits" are all subtypes of "ProperSplit.T".  The
   methods

| ProperSplit.T.succ
| ProperSplit.T.pred
| ProperSplit.T.move
| ProperSplit.T.nth
| ProperSplit.T.index
| ProperSplit.T.locate

   implement the split methods using the doubly-linked list.  If you
   declare a split as a subtype of "ProperSplit.T", you inherit these
   methods automatically.

   *)

INTERFACE VBTClass;

IMPORT VBT, Trestle, Axis, Point, Rect, Region, ScrnCursor, ScrnPixmap,
       Cursor, Batch;

(* Before we get to the up methods and the split methods, there is more to
   be revealed about "VBT"s in general: *)

REVEAL VBT.Prefix <: Prefix;

TYPE
  Prefix = MUTEX OBJECT          <* LL >= {VBT.mu, SELF} *>
             parent: VBT.Split      := NIL;
             upRef : ROOT           := NIL;
             domain: Rect.T         := Rect.Empty;
             st    : VBT.ScreenType := NIL;
           METHODS               <* LL.sup = SELF *>
             getcursor (): ScrnCursor.T;
             <* LL.sup = VBT.mu *>
             axisOrder (): Axis.T;
           END;

(* From "VBT.Prefix <: Prefix" it follows "VBT.T <: Prefix"; hence every
   "VBT" is a "MUTEX" object, and has the above fields and methods.  The
   complete revelation for the type "VBT.T" is private to Trestle.

   The fields "v.parent", "v.domain", and "v.st" record "v"'s parent,
   domain, and screentype.

   The object "v.upRef" is used by the methods of "v.parent" to store
   information specific to the child "v".  For example, if "v.parent" is a
   "ZSplit", then "v.upRef" contains a region representing the visible part
   of "v", pointers to the children before and after "v", and other
   information.  In a filter, "v.upRef" is usually "NIL", since when there
   is only one child, all the state can be stored in data fields directly
   in the parent object.

   If "v.parent" is "NIL", then so is "v.upRef".

   The locking level comment on the data fields means that in order to
   write one of the fields "v.parent", "v.upRef", "v.domain", or "v.st", a
   thread must have both "VBT.mu" and "v" locked.  Consequently, in order
   to read one of the fields, a thread must have either "VBT.mu" (or a
   share of "VBT.mu") or "v" locked.  Thus the fields can be read either by
   up methods or by down methods.

   The call "v.getcursor()" returns the cursor that should be displayed
   over "v"; that is, the cursor that was called "GetCursor(v)" in the
   "VBT" interface.  It is almost never necessary to override the
   "getcursor" method, since leaves and splits have suitable default
   methods.

   The "axisOrder" method determines whether it is preferable to fix a
   "VBT"'s height first or its width first.  For example, a horizontal
   packsplit would rather have its width fixed before its range of heights
   is queried, since its height depends on its width.  In general, if "v"'s
   size range in axis "ax" affects its size range in the other axis (and
   not vice-versa), then "v.axisOrder()" should return "ax".  The default
   is to return "Axis.T.Hor".

   Next we come to the specifications of the split methods and the up
   methods: *)

REVEAL VBT.Split <: Public;

TYPE
  Public =
    VBT.Leaf OBJECT
    METHODS

      (*
      | (* The split methods *)
      *)
      <* LL >= {VBT.mu, SELF, ch} *>
      beChild (ch: VBT.T);
      <* LL.sup = VBT.mu *>
      replace (ch, new: VBT.T);
      insert  (pred, new: VBT.T);
      move    (pred, ch: VBT.T);
      locate  (READONLY pt: Point.T; VAR (*OUT*) r: Rect.T): VBT.T;
      <* LL >= {VBT.mu} *>
      succ  (ch: VBT.T): VBT.T;
      pred  (ch: VBT.T): VBT.T;
      nth   (n: CARDINAL): VBT.T;
      index (ch: VBT.T): CARDINAL;

      (*
      | (* The up methods *)
      *)

      <* LL.sup = ch *>
      setcage    (ch: VBT.T);
      setcursor  (ch: VBT.T);
      paintbatch (ch: VBT.T; b: Batch.T);
      sync       (ch: VBT.T; wait := TRUE);
      capture (ch: VBT.T; READONLY rect: Rect.T; VAR (*out*) br: Region.T):
               ScrnPixmap.T;
      screenOf (ch: VBT.T; READONLY pt: Point.T): Trestle.ScreenOfRec;
      <* LL.sup < SELF AND LL >= {ch, VBT.mu.ch} *>
      newShape (ch: VBT.T);
      <* LL.sup = ch *>
      acquire (ch: VBT.T; w: VBT.T; s: VBT.Selection; ts: VBT.TimeStamp)
               RAISES {VBT.Error};
      release (ch: VBT.T; w: VBT.T; s: VBT.Selection);
      put (         ch    : VBT.T;
                    w     : VBT.T;
                    s     : VBT.Selection;
                    ts    : VBT.TimeStamp;
                    type  : VBT.MiscCodeType;
           READONLY detail                     := VBT.NullDetail)
           RAISES {VBT.Error};
      forge (         ch    : VBT.T;
                      w     : VBT.T;
                      type  : VBT.MiscCodeType;
             READONLY detail                     := VBT.NullDetail)
             RAISES {VBT.Error};
      <* LL.sup <= VBT.mu *>
      readUp (ch: VBT.T;
              w : VBT.T;
              s : VBT.Selection;
              ts: VBT.TimeStamp;
              tc: CARDINAL       ): VBT.Value RAISES {VBT.Error};
      writeUp (ch : VBT.T;
               w  : VBT.T;
               s  : VBT.Selection;
               ts : VBT.TimeStamp;
               val: VBT.Value;
               tc : CARDINAL       ) RAISES {VBT.Error};
    END;

(* Notice that a "VBT.Split" is a subtype of a "VBT.Leaf".  That is, every
   "VBT.Split" is also a "VBT.Leaf", and therefore the painting operations
   in the "VBT" interface can be applied to splits.  This fact is revealed
   here rather than in the "VBT" interface to prevent clients of "VBT" from
   accidentally painting on splits.  To do so is almost certainly a
   mistake---it is the responsibility of the split's implementation to
   paint on the parent as necessary to keep its screen up to date. *)

(* \subsubsection{Specifications of the split methods} *)

(* The first group of methods implement the behavior in the "Split"
   interface:

   The method call "v.beChild(ch)" initializes "ch.upRef" as appropriate
   for a child of "v".  The method can assume that "ch" is non-nil and has
   the same screentype as "v".  When the method is called, "LL >= {VBT.mu,
   v, ch}".

   When declaring a subtype "ST" of a split type "S", the "beChild" method
   for "ST" will ordinarily call "S.beChild(v, ch)", which in turn will
   call "S"'s supertype's "beChild" method, and so on.  Only one of the
   methods should allocate the "upRef", but all of them may initialize
   different parts of it.  Two rules make this work.  First, the type of
   the "upRef" for children of "ST" splits should be a subtype of the type
   of the "upRef" for children of "S" splits.  Second, if a "beChild"
   method finds "ch.upRef" is "NIL" and "NIL" is not appropriate for the
   type, the method should allocate "ch.upRef"; otherwise it should narrow
   "ch.upRef" to the appropriate type and initialize it.

   For example, "HVSplit.T" is a subtype of "ProperSplit.T".  Hidden in the
   "HVSplit" module is a type "HVSplit.Child", which represents the
   per-child information needed by an "HVSplit".  The type "HVSplit.Child"
   is a subtype of "ProperSplit.Child".  The method "HVSplit.beChild(hv,
   ch)" allocates a new "HVSplit.Child", stores it in "ch.upRef",
   initializes the part of it that is specific to "HVSplit", and then calls
   "ProperSplit.beChild(hv, ch)", which initializes the part of "ch.upRef"
   that is common to all proper splits, and then calls its supertype's
   "beChild" method, and so on.

   The chain of calls eventually ends with a call to "VBT.Split.beChild",
   which causes an error if "ch" is not detached or if "ch"'s screentype
   differs from "v", and otherwise sets "ch.parent" to "v" and marks "v"
   for redisplay.

   The method call "v.replace(ch, new)" simply implements the operation
   "Split.Replace(v, ch, new)", and the call "v.replace(ch, NIL)"
   implements "Split.Delete(v, ch)".  Before calling the method, the
   generic code in "Split" marks "v" for redisplay, checks that "ch" is a
   child of "v" and that "new" is detached, and rescreens "new" to the
   screentype of "v".

   Similarly, the method call "v.insert(pred, new)" implements the
   operation "Split.Insert(v, pred, new)".  Before calling the method, the
   generic code in "Split" marks "v" for redisplay, checks that "pred" is
   "NIL" or a child of "v" and that "new" is detached, and rescreens "new"
   to the screentype of "v".  A split that can only contain a limited
   number of children may detach and discard the previous child to
   implement "insert".

   The call "v.move(pred, ch)" implements "Split.Move(v, pred, ch)".
   Before calling the method, the generic code verifies that "pred" and
   "ch" are children of "v" (or "NIL", in the case of "pred"), and avoids
   the call if "pred = ch" or "v.succ(pred) = ch".

   When the "replace", "insert", or "move" method is called, "LL.sup =
   VBT.mu".  The default methods are equal to "NIL"; so every split class
   must arrange to override these methods, usually by inheriting them from
   "Filter" or from "ProperSplit".

   The method calls "v.succ(ch)", "v.pred(ch)", "v.nth(n)", and
   "v.index(ch)" implement the corresponding operations in the "Split"
   interface.  In all cases, "LL >= {VBT.mu}".

   The default method "VBT.Split.succ" is "NIL"; so every split class must
   arrange to override the method, usually by inheriting them from "Filter"
   or from "ProperSplit".  The default methods "VBT.Split.pred",
   "VBT.Split.nth", and "VBT.Split.index" are implemented by repeatedly
   calling the "succ" method.

   The method call "v.locate(pt, r)" returns the child of "v" that controls
   the position "pt", or "NIL" if there is no such child.  The method also
   sets "r" to a rectangle containing "pt" such that for all points "q" in
   the meet of "r" and "domain(v)", "v.locate(q, ...)" would return the
   same result as "v.locate(pt, ...)".  The split implementation is
   expected to make "r" as large as possible, so that clients can avoid
   calling "locate" unnecessarily.  When the method is called, "pt" will be
   in "domain(v)".  When the locate method is called, "LL.sup = VBT.mu".

   If "v" inherits the "mouse", "position", "setcursor", or "setcage"
   methods from "VBT.Split", then you must call "LocateChanged(v)" whenever
   any operation on the split invalidates a rectangle-child pair returned
   previously by "v.locate": *)

PROCEDURE LocateChanged (v: VBT.Split);
<* LL.sup = VBT.mu *>
(* Clear any cached results of the "locate" method. *)

(* The default method "VBT.Split.locate(v, pt, r)" enumerates "v"'s
   children in "succ" order and returns the first child "ch" whose domain
   contains "pt".  It sets "r" to a maximal rectangle that lies inside the
   domain of "ch" and outside the domains of all preceding children.  If no
   child contains "pt", it returns "NIL" and sets "r" to a maximal
   rectangle that lies inside the domain of "v" and outside the domains of
   all its children.  This is suitable if the children don't overlap or if
   whenever two children overlap, the top one appears earlier in "succ"
   order. *)

(* \subsubsection{Specifications of the up methods} *)

(* So much for the split methods; here now are the specifications of the up
   methods.  In all cases, "ch" is a child of "v".

   The method call "v.setcage(ch)" is called by the system whenever "ch"'s
   cage is changed.  It is called with "LL.sup = ch".  The default method
   implements the behavior described in the "VBT" interface.

   The method call "v.setcursor(ch)" is called by the system whenever the
   result of "ch.getcursor()" might have changed.  It is called with
   "LL.sup = ch".  The default method implements the behavior described in
   the "VBT" interface.

   The method call "v.paintbatch(ch, b)" is called to paint the batch "b"
   of painting commands on "v"'s child "ch".  The procedure can assume that
   the batch is not empty and that its clipping rectangle is a subset of
   "ch"'s domain.  It is responsible for ensuring that "b" is eventually
   freed, which can be achieved by calling passing "b" to "Batch.Free" or
   by passing "b" to another paintbatch method, which will inherit the
   obligation to free the batch.  A "paintbatch" method is allowed to
   modify the batch.  The default method clips the batch to "ch"'s domain,
   paints the batch on the parent, and sets "ch"'s shortcircuit bit.  The
   method is called with "LL.sup = ch".

   The method call "v.sync(ch, wait)" implements "VBT.Sync(ch, wait)".
   When the method is called, "ch"'s batch will have been forced.  The
   default method acquires "v", releases "ch", forces "v", calls
   "v.parent"'s sync method, releases "v", and reacquires "ch".  When the
   method is called, "ch"'s batch is "NIL" and "LL.sup = ch".

   The method call "v.capture(ch, r, br)" implements "VBT.Capture(ch, r,
   br)".  The default method recurses on the parent.  When the method is
   called, "ch"'s batch is "NIL", "r" is a subset of "ch"'s domain, and
   "LL.sup = ch".

   The method call "v.screenOf(ch, pt)" implements "Trestle.ScreenOf(ch,
   pt)".  The default method recurses on the parent.  When the method is
   called, "LL.sup = ch".

   The method call "v.newShape(ch)" signals that "ch"'s size range,
   preferred size, or axis order may have changed.  The default recurses on
   the parent.  When the method is called, "LL.sup < v AND LL >= {ch,
   VBT.mu.ch}".

   The remaining methods implement event-time operations for a descendent
   (not necessarily a direct child) of the window "v".  In all cases, "ch"
   is a child of "v" and "w" is a descendant of "ch".

   The "acquire", "release", "put", and "forge" methods implement the
   corresponding procedures from the "VBT" interface.  For example,
   "v.put(ch, w, s, ts, cd)" implements "VBT.Put(w, s, ts, cd.type,
   cd.detail)".  When these methods are called, "LL.sup = ch".

   Similarly, the "readUp" and "writeUp" methods implement the procedures
   "VBT.Read" and "VBT.Write".  When these methods are called, "LL.sup <=
   VBT.mu". *)

(* \subsubsection{Getting and setting the state of a VBT} *)

PROCEDURE Cage (v: VBT.T): VBT.Cage; <* LL >= {v} *>
(* Return v's cage. *)

TYPE VBTCageType = {Gone, Everywhere, Rectangle};

PROCEDURE CageType (v: VBT.T): VBTCageType;
<* LL >= {v} *>
(* Return "v"'s cage's type. *)

(* "CageType(v)" returns "Gone" if "Cage(v) = VBT.GoneCage", "Everywhere"
   if "Cage(v) = VBT.EverywhereCage", and "Rectangle" otherwise.  It is
   more efficient than "Cage". *)

PROCEDURE GetCursor (v: VBT.T): Cursor.T;
<* LL >= {v} *>
(* Return "cursor(v)". *)

PROCEDURE SetShortCircuit (v: VBT.T); <* LL >= {v} *>
(* Set the short-circuit property of "v". *)

PROCEDURE ClearShortCircuit (v: VBT.T); <* LL >= {v} *>
(* Clear the short-ciruit propery of "v". *)

(* If "v"'s short-circuit property is on, painting on "v" will be
   implemented by clipping to its domain and painting on its parent.

   The next three procedures are equivalent to the corresponding procedures
   in "VBT", except they have a different locking level: *)

PROCEDURE PutProp (v: VBT.T; ref: REFANY);
<* LL >= {v} *>

PROCEDURE GetProp (v: VBT.T; tc: INTEGER): REFANY;
<* LL >= {v} *>

PROCEDURE RemProp (v: VBT.T; tc: INTEGER);
<* LL >= {v} *>

(* In implementing a split it is sometimes necessary to read a child's bad
   region; in which case the following procedure is useful: *)

PROCEDURE GetBadRegion (v: VBT.T): Region.T;
<* LL >= {v} *>
(* Return v's bad region; that is, the join of "bad(v)" and
   "exposed(v)". *)

(* For the convenience of split implementors, every "VBT" has a
   ``newshape'' bit which is set by a call to "VBT.NewShape".  For example,
   the redisplay or shape method of a split can test these bits to
   determine which of its children have new shapes. *)

PROCEDURE HasNewShape (v: VBT.T): BOOLEAN;
<* LL.sup < v *>
(* Return the value of "v"'s newshape bit. *)

PROCEDURE ClearNewShape (v: VBT.T); <* LL.sup < v *>
(* Clear "v"'s "newshape" bit. *)

(* \subsubsection{Procedures for activating the down methods of a VBT} *)

PROCEDURE Reshape (v: VBT.T; READONLY new, saved: Rect.T);
<* LL.sup >= VBT.mu.v AND LL.sup <= VBT.mu *>
(* Prepare for and call "v"'s "reshape" method. *)

(* That is, "Reshape" changes "v.domain" and then schedules a call to

| v.reshape(VBT.ReshapeRec{v.domain, new, saved})

   It should always be called instead of a direct call to the method, since
   it establishes essential internal invariants before calling the method.
   The bits in the "saved" argument must remain valid until the method
   returns.  It is all right for "saved" to be larger than "v"'s old
   domain; "Reshape" will clip it to "v"'s old domain before calling the
   method.  It is illegal to reshape a detached "VBT" to have a non-empty
   domain.

   For example, the "reshape" method of "BorderedVBT" uses
   "VBTClass.Reshape" to reshape its child. *)

PROCEDURE Rescreen (v: VBT.T; st: VBT.ScreenType);
<* LL.sup >= VBT.mu.v AND LL.sup <= VBT.mu *>
(* Prepare for and call "v"'s "rescreen" method. *)

(* That is, "Rescreen" executes

| prev := v.domain;
| v.domain := Rect.Empty;
| v.st := st;
| v.rescreen(VBT.RescreenRec{prev, st}).
   *)

(* For example, to determine how large a menu "m" would be if it were
   inserted into a "ZSplit" "z", you can't simply call "GetShapes(m)",
   since in general the screentype of "m" could be different from the
   screentype of "z", and the shape can depend on the screentype.  But you
   can call "VBTClass.Rescreen(m, z.st)" followed by "GetShapes(m)". *)

PROCEDURE Repaint (v: VBT.T; READONLY badR: Region.T);
<* LL.sup >= VBT.mu.v AND LL.sup <= VBT.mu *>
(* Join "badR" into "v"'s bad region and then prepare for and call "v"'s
   repaint method. *)

PROCEDURE Position (v: VBT.T; READONLY cd: VBT.PositionRec);
<* LL.sup = VBT.mu *>
(* Prepare for and call "v"'s "position" method. *)

PROCEDURE Key (v: VBT.T; READONLY cd: VBT.KeyRec);
<* LL.sup = VBT.mu *>
(* Prepare for and call "v"'s "key" method. *)

PROCEDURE Mouse (v: VBT.T; READONLY cd: VBT.MouseRec);
<* LL.sup = VBT.mu *>
(* Prepare for and call "v"'s "mouse" method. *)

PROCEDURE Misc (v: VBT.T; READONLY cd: VBT.MiscRec);
<* LL.sup = VBT.mu *>
(* Prepare for and call "v"'s "misc" method. *)

(* The following two procedures schedule calls to the down methods without
   making the calls synchronously.  They are useful when you hold too many
   locks to call a down method directly.  For example, when a "ZSplit"
   child scrolls bits that are obscured, the locking level of the
   "paintbatch" method precludes calling the "repaint" method directly; but
   a call can be scheduled with "ForceRepaint". *)

PROCEDURE ForceEscape (v: VBT.T); <* LL.sup >= {v} *>
(* Enqueue a cage escape to "gone" for delivery to "v". *)

PROCEDURE ForceRepaint (v: VBT.T; READONLY rgn: Region.T; deliver := TRUE);
<* LL.sup >= {v} *>
(* Join "rgn" into "v"'s bad region, and possibly schedule a call to "v"'s
   repaint method. *)

(* "VBTClass.ForceRepaint" is like "VBT.ForceRepaint", except that it has a
   different locking level, and if "deliver" is "FALSE" then no thread will
   be forked to deliver the bad region---in this case the caller has the
   obligation to deliver the bad region soon, either by calling
   "ForceRepaint" with "deliver = TRUE", or by calling "Repaint". *)

PROCEDURE Redisplay (v: VBT.T);  <* LL.sup = VBT.mu *>
(* If "v" is marked for redisplay, then unmark it and prepare for and call
   "v.redisplay()". *)

PROCEDURE GetShape (v            : VBT.T;
                    ax           : Axis.T;
                    n            : CARDINAL;
                    clearNewShape             := TRUE): VBT.SizeRange;
<* LL.sup >= VBT.mu.v AND LL.sup <= VBT.mu *>
(* Prepare for and call "v"'s "shape" method. *)

(* "GetShape" causes a checked runtime error if the result of the shape
   method is invalid.  If "clearNewShape" is "TRUE", "GetShape" calls
   "ClearNewShape(v)" before it calls the method. *)

PROCEDURE GetShapes (v: VBT.T; clearNewShape := TRUE):
  ARRAY Axis.T OF VBT.SizeRange;
<* LL.sup >= VBT.mu.v AND LL.sup <= VBT.mu *>
(* Return the shapes of "v" in both axes. *)

(* "GetShapes" calls the shape method of "v" in each axis, using the order
   determined by "v.axisOrder()", and returns the array of the resulting
   size ranges.  If "clearNewShape" is "TRUE", "GetShapes" calls
   "ClearNewShape(v)" before it calls the method.

   "GetShapes" is convenient if both the height and width preferences of
   the child can be accomodated---for example, when inserting a top level
   window or "ZSplit" child. *)

PROCEDURE Detach (v: VBT.T);     <* LL.sup = VBT.mu *>
(* Set "v.parent" and "v.upRef" to "NIL"; set "v"'s domain to empty,
   enqueue a reshape to empty, and clear "v"'s shortcircuit bit. *)

(* \subsubsection{Procedures for activating the up methods of a VBT} *)

(* The following six procedures are like the corresponding procedures in
   the "VBT" interface, except that they have a different locking level: *)

PROCEDURE SetCage (v: VBT.T; READONLY cg: VBT.Cage);
<* LL.sup = v *>

PROCEDURE SetCursor (v: VBT.T; cs: Cursor.T);
<* LL.sup = v *>

PROCEDURE Acquire (v: VBT.T; s: VBT.Selection; t: VBT.TimeStamp)
  RAISES {VBT.Error};            <* LL.sup = v *>

PROCEDURE Release (v: VBT.T; s: VBT.Selection);
<* LL.sup = v *>

PROCEDURE Put (         v     : VBT.T;
                        s     : VBT.Selection;
                        t     : VBT.TimeStamp;
                        type  : VBT.MiscCodeType;
               READONLY detail                     := VBT.NullDetail)
  RAISES {VBT.Error};
<* LL.sup = v *>

PROCEDURE Forge (         v     : VBT.T;
                          type  : VBT.MiscCodeType;
                 READONLY detail                     := VBT.NullDetail)
  RAISES {VBT.Error};
<* LL.sup = v *>

(* Finally, here is a procedure for executing a batch of painting commands
   on a "VBT": *)

PROCEDURE PaintBatch (v: VBT.T; VAR b: Batch.T);
<* LL.sup < v *>
(* Execute the batch "b" of painting commands on "v", free "b", and set "b"
   to "NIL". *)

(* The interpretation of "b" is described in the "Batch" and "PaintPrivate"
   interfaces.  If "b.clipped" is erroneously set to "TRUE", then
   "PaintBatch" may execute the batched painting commands without clipping
   them to "b.clip", but it will not paint outside "v"'s domain. *)

END VBTClass.
