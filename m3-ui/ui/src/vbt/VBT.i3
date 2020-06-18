(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman, Mark Manasse and Greg Nelson           *)
(* Last modified on Thu Apr  7 09:54:12 PDT 1994 by heydon   *)
(*      modified on Thu Oct  7 17:57:18 PDT 1993 by sfreeman *)
(*      modified on Sat Oct  3 11:17:12 PDT 1992 by msm      *)
(*      modified on Mon Feb 24 13:58:30 PST 1992 by muller   *)
(*      modified on Sun Dec 22 14:33:44 PST 1991 by gnelson  *)
(*      modified on Fri Aug  3 10:13:01 PDT 1990 by steveg   *)
<*PRAGMA LL*>

(* A "VBT.T" (or simply a "VBT") is the basic window
   abstraction of the Trestle system. *)

INTERFACE VBT;

IMPORT Word, Axis, Point, Rect, Region, Trapezoid,
  Path, Pixmap, Cursor, Font, PaintOp, ScrnPixmap;

(* \subsection{The public methods} *)

(* A "VBT" is represented as an object with a private prefix and twelve
   public methods, which define the way the "VBT" responds to events.
   Here are the type declarations that reveal the public methods, while
   concealing the private prefix: *)

TYPE
  T <: Public;
  Public = Prefix OBJECT
    METHODS
      <* LL.sup = mu *>
      mouse(READONLY cd: MouseRec);
      position(READONLY cd: PositionRec);
      redisplay();
      misc(READONLY cd: MiscRec);
      key(READONLY cd: KeyRec);
      discard();
      <* LL.sup = mu.SELF *>
      reshape(READONLY cd: ReshapeRec);
      rescreen(READONLY cd: RescreenRec);
      repaint(READONLY rgn: Region.T);
      shape(ax: Axis.T; n: CARDINAL): SizeRange;
      <* LL.sup <= mu *>
      read(sel: Selection; tc: CARDINAL): Value
        RAISES {Error};
      write(sel: Selection; val: Value; tc: CARDINAL)
        RAISES {Error};
    END;
  Prefix <: ROOT;

(* For example, if the user reshapes a window, Trestle will call
   the window's reshape method; if the user exposes some part of the
   window, Trestle will call the window's repaint method.  The
   remainder of the "VBT" interface specifies the methods in detail.
   The pragmas about "LL" are explained in the section on locking level,
   below.

   You should never call a "VBT"'s methods directly.  The "VBTClass"
   interface provides wrapper procedures that call the methods
   indirectly.  *)

(* \subsection{Screens and domains} *)

(* Every "VBT" has a {\it screen} that associates a pixel value with
   each integer lattice point.  We write "v[p]" to denote the value
   of the pixel at point "p" of the screen of the "VBT" "v".  Changing
   the pixel values in a "VBT"'s screen is called {\it painting}.

   The part of a "VBT"'s screen that is visible to the user---or that would
   be visible if other windows weren't in the way---is called the
   {\it domain} of the "VBT": *)

PROCEDURE Domain(v: T): Rect.T; <* LL.sup < v *>
(*  Return the rectangular extent of the visible part of
    "v"'s screen. *)

 (* The domain is an arbitrary rectangle: it can be empty, the
    coordinate origin can be anywhere inside or outside it, and it does
    not necessarily correspond to the position of the window on the
    physical display screen.

    When "v" is reshaped, "Domain(v)" changes from one rectangle to
    another.  During this transformation Trestle tries to save the old
    screen until the new screen is fully repainted: thus in the midst
    of reshaping, "v[p]" can be useful for some points "p" outside
    "Domain(v)".  At other times, Trestle keeps track of "v[p]" only
    for points "p" inside "Domain(v)".

    The pragma "LL.sup < v" is explained in the next section.  *)

(* \subsection{Locking level} *)

(* The global mutex "mu" serializes operations that affect the tree
   of "VBTs":
   \index{LL (Locking Level)@{\protect\tt LL} (Locking Level)}*)

VAR mu: MUTEX;

(* In addition, every "VBT" includes a private mutex that serializes
   operations on the "VBT" itself.  The private mutex of a "VBT" is
   revealed in the "VBTClass" interface, not in this interface.

   The order in which a thread is allowed to acquire these locks is
   called the ``locking order''.  It is defined by these two rules:

   \medskip\bulletitem The global "mu" precedes every "VBT".

   \medskip\bulletitem Every "VBT" precedes its parent.

   \medskip\noindent The ``locking level'' of a thread, or "LL" for
   short, is the set of locks that the thread has acquired.  The
   expression "LL.sup" denotes the maximum of the locks in "LL".  (The
   locking order is partial, but "LL.sup" will be defined for any thread
   in a correct program, since threads acquire locks in ascending
   order.)

   Each procedure declaration in the Trestle system includes a pragma
   specifying the locking level at which a thread can legally call the
   procedure.  For example, the pragma "LL.sup < v" on the "Domain"
   procedure allows a thread to call "Domain" with no locks, or with
   "mu" locked, or with descendants of "v" locked, but forbids calling
   it with any other "VBT"s locked.

   Similarly, each public data field and method of an object has a
   locking level.  In both cases, a locking level pragma applies to
   all the fields or methods between it and the next pragma.  These
   pragmas may contain the special identifier "SELF", which refers
   to the object itself.

   The locking level for a method is identical to the locking level
   for a procedure: it specifies the locking level at which a thread
   can legally call the method.  For example, whenever the "mouse",
   "position", "redisplay", "misc", "key", or "discard" methods of
   a "VBT" are called, the locking level satisfies "LL.sup = mu".

   The locking level for a writable data field is of the form

| LL >= {mu1, ..., muN}.

   This specifies that in order to write the field, a thread must hold
   all of the locks "mu1" through "muN".  As a consequence, a thread
   can read the field if it holds any of the locks.

   (In a locking level pragma, the ordering symbols ">=", "<=", "<",
   and ">" are overloaded to denote either set containment or lock
   order, depending on context.  For example, "LL >= {mu, v}" indicates
   that the thread has both "mu" and "v" locked, while "LL.sup <= mu"
   indicates that all locks held by the thread precede "mu" in the
   locking order.)

   A data field may also be commented "CONST", meaning that it is
   readonly after initialization and therefore can be read with no
   locks at all.

   There is one more special notation related to locking levels: a "VBT"
   "v" can hold a ``share'' of the global lock "mu"; its share is
   denoted by "mu.v".  This is explained in the section of this
   interface that specifies the "reshape" method.

   All the procedures in the Trestle system restore the caller's locking
   level when they return.  For example, calling "Domain(v)" has no
   net effect on a thread's locking level.  *)


(* \subsection{ScreenTypes} *)

(* Pixel values are integers.   The color associated with a pixel value
   is determined in some manner that depends on the {\it screentype}
   of the "VBT".  A value "st" of type "VBT.ScreenType" represents a
   screentype: *)

TYPE
  ScreenType <: ScreenTypePublic;
  ScreenTypePublic = OBJECT (*CONST*)
    depth: INTEGER;
    color: BOOLEAN;
    res: ARRAY Axis.T OF REAL
  END;

(* The integer "st.depth" is the number of bits per pixel in screens
    of type "st". The boolean "st.color" is "TRUE" if the pixels are
    colored, "FALSE" if they are black and white or gray-scale.  The
    array "st.res" gives the horizontal and vertical resolution of the
    screen in pixels per millimeter for desk-top displays, or in
    visually equivalent units for other displays.

   The screentype of a newly-allocated "VBT" is "NIL"; it becomes
   non-"NIL" only when the "VBT" is connected to a window system.

   Here are two procedures for reading the screentype of a "VBT" and
   for converting distances to screen coordinates: *)


PROCEDURE ScreenTypeOf(v: T): ScreenType;
<* LL.sup < v *>
(* Return the screentype of "v". *)


PROCEDURE MMToPixels(v: T; mm: REAL; ax: Axis.T)
: REAL; <* LL.sup < v *>
(* Return the number of pixels that correspond to "mm" millimeters on
   "v"'s screentype in the axis "ax"; or return "0" if "v"'s screentype
   is "NIL".  *)

(* The "ScreenType" interface reveals more details, for example,
   about color maps.  *)

(* \subsection{Splits and leaves} *)

(* User interfaces are usually constructed from a tree of "VBTs" whose
   root is the ``top-level window'' known to the window manager.  "VBTs"
   are classified into two main subtypes based on their positions in
   the tree: *)

TYPE
  Split <: T;
  Leaf <: T;


PROCEDURE Parent(v: T): Split; <* LL.sup < v *>
(* Return "v"'s parent, or "NIL" if "v" has no parent.  *)

(* A "Split" (also called a parent "VBT") divides its screen up
   among its children according to some layout policy that depends on
   the class of split.  Each pixel of the parent screen represents
   a pixel of one of the child "VBTs", which is said to control that
   pixel.  For example, overlapping windows are provided by a class of
   split called a "ZSplit", for which the children are ordered bottom to
   top, and each pixel "v[p]" of the parent domain is controlled by the
   top-most child whose domain includes "p".

   See the "Split" interface for common operations on splits (e.g.,
   enumerating children).

   A "Leaf" is a "VBT" in which the twelve public methods make the
   "Leaf" ignore all events, be indifferent about its shape, and do
   nothing when discarded.  It is provided as a starting point: you
   can define a useful subtype of "Leaf" by overriding the methods that
   are relevant to the new class.

   Almost all subtypes of "VBT" are subtypes of either "Split" or
   "Leaf".  *)


(* \subsection{Timestamps, modifiers, mouse buttons, and cursor positions} *)

(* The following types are used in several of the event methods: *)

TYPE
  TimeStamp = Word.T;

  Modifier =
    {Shift, Lock, Control, Option,
     Mod0, Mod1, Mod2, Mod3,
     MouseL, MouseM, MouseR,
     Mouse0, Mouse1, Mouse2, Mouse3, Mouse4};

  Button = [Modifier.MouseL..Modifier.Mouse4];

  Modifiers = SET OF Modifier;
CONST
  ModifiersLB = Word.Shift (-1, BITSIZE (Modifiers)-1);
  ModifiersUB = Word.Not (ModifiersLB);
TYPE
  ModifiersAsInt = [ModifiersLB .. ModifiersUB];
  (* ^A signed subrange of INTEGER (Possibly all of INTEGER)
      with same BITSIZE as Modifiers. *)

TYPE
  ScreenID = INTEGER;

  CursorPosition = RECORD
    pt: Point.T;
    screen: ScreenID;
    gone, offScreen: BOOLEAN;
  END;

CONST
  Buttons = Modifiers{FIRST(Button)..LAST(Button)};

(* Trestle has an internal unsigned clock register that is incremented every
   few milliseconds.  When Trestle reports a mouse or keyboard event to a
   "VBT", it also reports the value of the clock register when the event
   occurred, which is called the {\it timestamp} of the event.
   Timestamps serve as unique identifiers for the associated events.
   Also, the absolute time interval between two events can be computed
   by subtracting their timestamps with "Word.Minus" and multiplying by
   "Trestle.TickTime()", which is the absolute interval between clock
   ticks. \index{time interval between events}

   A few keys on the keyboard are defined to be {\it modifiers}, like "Shift",
   "Control", and "Option".  When Trestle reports a mouse or keyboard event
   to a "VBT", it also reports the set of modifier keys and buttons that
   were down when the event occurred.  Thus the application can distinguish
   shifted mouse clicks from unshifted mouse clicks, for example.

   The modifier "Shift" is reported if either of the keyboard's shift
   keys is down; similarly for "Control" and "Option".  The modifier "Lock"
   is reported if the lock key is locked down.  If the keyboard has
   a key labelled "lock" but this key does not have mechanical alternate
   action, then the modifier "Lock" reflects the simulated state of
   the lock key (that is, alternate presses of the lock key turn the
   modifier on or off).  Trestle does not define whether it reports up
   and down transitions for lock keys while the modifier is set.

   Some Trestle servers interpret other keys as modifiers: the type
   definition accommodates up to four additional modifiers, "Mod0"
   through "Mod3".

   The mouse buttons are reported as modifiers.  The naming of the first
   three buttons assumes a three-button mouse; in general it is assumed
   that there are at most eight buttons.

   When Trestle reports a mouse position event to a "VBT" "v", it
   also reports a value "cp" of type "CursorPosition".  The point
   "cp.pt" is the position of the cursor; the integer "cp.screen"
   identifies the screen of the window system where the event occurred;
   and "cp.offScreen" is "TRUE" if the position is on a different screen
   than "v", and "FALSE" otherwise.  If "cp.offScreen" is "FALSE", then
   "cp.pt" is in "v"'s coordinate system, otherwise "cp.pt" is in the
   coordinate system of "cp.screen".  The boolean "cp.gone" is "TRUE"
   if "v" doesn't control the position "cp.pt", and "FALSE" if it does.
   If "cp.offScreen" is "TRUE", then so is "cp.gone".  A position is
   controlled by a "VBT" "w" if a mouse-click at that position would
   ordinarily be delivered to "w".  All positions controlled by a "VBT"
   are in its domain; every pixel in the domain of a split is controlled
   by at most one child of that split.  You should think of the positions
   controlled by a "VBT" as the visible positions in its domain. *)

(* \subsection{The mouse method} *)

(* Trestle calls a "VBT"'s mouse method to report mouse clicks.  The
   method will be called with "LL.sup = mu", and takes an argument of
   type "MouseRec". *)

TYPE MouseRec = RECORD
  whatChanged: Button;
  time: TimeStamp;
  cp: CursorPosition;
  modifiers: Modifiers;
  clickType: ClickType;
  clickCount: INTEGER;
END;

ClickType =
  {FirstDown, OtherDown, OtherUp, LastUp};

(* The method call "v.mouse(cd)" indicates that the mouse button
   "cd.whatChanged" went down or up at time "cd.time" and cursor
   position "cd.cp".

   The field "cd.clickType" is "FirstDown" if the button went down when
   no other buttons were down, "OtherDown" if it went down when some
   other button(s) were already down, "LastUp" if it went up when all
   other buttons were up, and "OtherUp" if it went up when some other
   button(s) were still down.

   The field "cd.modifiers" reflects the state of the modifiers
   (either just before or just after the button transition; it is
   not specified which).

   If "cd.clickType" is "FirstDown", then "cd.cp.gone" will be "FALSE".

   The field "cd.clickCount" is the number of preceding transitions
   of the button that were near in time and space.  For example,
   "clickCount=3" on the final up transition of a double click.
   Some Trestle implementations have auxilliary interfaces that
   allow you to set the amount of time and mouse motion allowed. *)

(* \subsection{The mouse focus rule} *)

(* A split relays mouse clicks to whichever child of the split controls the
   pixel at the position of the click---more or less.  If this rule were
   applied blindly, a child could receive a down-click and never receive
   the corresponding up-click, which would make it impossible to program
   many user interfaces that involve dragging.  Therefore the actual rule
   is more complicated.  \index{mouse~focus}

   Each split "sp" contains a variable "mouseFocus(sp)", which records
   the child of the split that has received a transition of type
   "FirstDown" but not yet received a subsequent transition of type
   "LastUp".  If there is no such child, "mouseFocus(sp)" is "NIL".
   The split "sp" relays the "MouseRec" "cd" by the ``mouse focus rule'':

| IF `some child "ch" controls "cd.cp"` THEN
|   w := ch;
|   w.mouse(cd)
| ELSE
|   w := NIL
| END;
| IF cd.clickType = ClickType.FirstDown THEN
|   mouseFocus(sp) := w
| ELSE
|   IF mouseFocus(sp) # NIL AND mouseFocus(sp) # w THEN
|     cd.cp.gone := TRUE;
|     mouseFocus(sp).mouse(cd)
|   END;
|   IF cd.clickType = ClickType.LastUp THEN
|     mouseFocus(sp) := NIL
|   END
| END

   The mouse focus is guaranteed to receive all button transitions until
   the last button comes up, no matter where it occurs.  *)

(* \subsection{The position method} *)

(* Trestle calls a "VBT"'s position method to report cursor positions.  The
   method will be called with "LL.sup = mu", and takes an argument of
   type "PositionRec". *)

TYPE PositionRec = RECORD
  cp: CursorPosition;
  time: TimeStamp;
  modifiers: Modifiers;
END;

(* The method call "v.position(cd)" indicates that at the time "cd.time"
   the cursor position was "cd.cp" and the set of modifiers keys that
   were down was "cd.modifiers".

   The next section explains how to control the delivery of cursor
   positions.  *)


(* \subsection{Tracking the cursor by setting cages} *)

(* Every "VBT" "v" contains a field "cage(v)", which represents a set
   of cursor positions.  As long as the cursor's position is inside
   "v"'s cage, Trestle won't report the position to "v".  As soon as
   the cursor's position moves outside "cage(v)", Trestle reports the
   position to "v", after first resetting "v"'s cage to contain all
   cursor positions.  Resetting the cage inhibits further reporting
   of cursor positions: to continue tracking, the position method must
   set a new cage.  \index{cursor~tracking}
   \index{cages~(for~cursor~tracking)} *)

TYPE
  Cage = RECORD
    rect: Rect.T;
    inOut: InOut;
    screen: ScreenID;
  END;
  InOut = SET OF BOOLEAN;

CONST
  AllScreens: ScreenID = -1;

(* The cage "cg" contains the cursor position "cp" if

\medskip\bulletitem "cp.pt" is in "cg.rect",

\medskip\bulletitem "cp.gone" is in "cg.inOut", and

\medskip\bulletitem either "cg.screen = AllScreens" or "cg.screen = cp.screen".

\medskip\noindent Trestle imposes the restriction on cages that if "cg.screen
   = AllScreens", then "cg.rect" must be "Rect.Full" or "Rect.Empty",
   and if "cg" contains no cursor positions, then it must be equal as a
   record to "EmptyCage" (which is declared below).  For example,
   here are some useful cages: *)

CONST
  GoneCage =
    Cage{Rect.Full, InOut{TRUE}, AllScreens};
  InsideCage =
    Cage{Rect.Full, InOut{FALSE}, AllScreens};
  EverywhereCage =
    Cage{Rect.Full, InOut{FALSE, TRUE}, AllScreens};
  EmptyCage =
    Cage{Rect.Empty, InOut{}, AllScreens};

(* "GoneCage" contains all cursor positions that are ``gone''; set it
   on a "VBT" to wait for the cursor to be over a position controlled
   by the "VBT".  The cage "InsideCage" is the complement of "GoneCage":
   it contains all positions that the "VBT" controls.  The cage
   "EverywhereCage" contains all cursor positions, and "EmptyCage"
   contains none.

   Here is the procedure for setting the cage of a "VBT": *)

PROCEDURE SetCage(v: T; READONLY cg: Cage);
<* LL.sup < v *>
(* Set "cage(v)" to the intersection of "cage(v)" with "cg".  *)

(* In the usual case, "SetCage" is called from "v"'s position method,
   at which point "v"'s cage is "EverywhereCage" and therefore the
   intersection just comes out to "cg".  In unusual cases, it will be found
   that intersecting the new cage with the old is what is required.

   The procedure "CageFromPosition" is helpful for tracking the
   cursor continuously.  By setting "CageFromPosition(cp)" in
   response to each cursor position "cp", you can track the cursor as long
   as it moves within your "VBT".  There are two additional optional
   boolean arguments: setting "trackOutside" allows you to
   track the cursor over the whole screen containing the "VBT";
   setting "trackOffScreen" allows you to track the cursor even
   onto other screens: *)

PROCEDURE CageFromPosition(
  READONLY cp: CursorPosition;
  trackOutside, trackOffScreen: BOOLEAN := FALSE)
  : Cage; <* LL arbitrary *>
(* "CageFromPosition(cp)" returns the cage that contains only the position
   "cp"; or "GoneCage" if either "cp.gone" or "cp.offScreen" is "TRUE"
   and the corresponding argument is not. *)

(* More precisely, "CageFromPosition" is equivalent to:

| IF NOT cp.gone OR
|   trackOutside AND NOT cp.offScreen OR
|   trackOffScreen
| THEN
|   RETURN `the cage containing only the position` cp
| ELSIF cp.offScreen AND trackOutside THEN
|   RETURN Cage{Rect.Full, InOut{FALSE,TRUE}, cp.screen}
| ELSE
|   RETURN GoneCage
| END

Finally, the following two procedures are occasionally useful: *)

PROCEDURE Outside(
  READONLY cp: CursorPosition; READONLY c: Cage)
  : BOOLEAN; <* LL arbitrary *>
(* Return whether the position "cp" is outside the cage "cg". *)

PROCEDURE CageFromRect(READONLY r: Rect.T;
  READONLY cp: CursorPosition): Cage; <* LL arbitrary *>
(* Return "Cage{r, InOut{cp.gone}, cp.screen}".  *)

(* The effect of "SetCage(v, CageFromRect(r, cp))" is to suspend cursor
   positions as long as the cursor stays inside the rectangle "r" and
   has the same value of "gone" as "cp" does.  This is useful when
   sweeping text selections, for example.  *)

(* Splits relay cursor positions to their children.  If several
   of the children are tracking the cursor at the same time, the order in
   which positions are relayed to the different children can be
   important.  The order is determined by the following rule, which
   specifies the way a split "sp" forwards a "PositionRec" "cd" to its
   children (the variable "current(sp)" is the child that controls the
   last cursor position seen by "sp"):

| IF `some child "ch" controls "cd.cp"` THEN
|   w := ch
| ELSE
|   w := NIL
| END;
| goneCd := cd;
| goneCd.cp.gone := TRUE;
| IF w # current(sp) THEN
|   Deliver(current(sp), goneCd)
| END;
| FOR `all "ch" other than "w" and "current(sp)"` DO
|   Deliver(ch, goneCd)
| END;
| IF w # NIL THEN Deliver(w, cd) END;
| current(sp) := w

where

| Deliver(v, cd) =
|   IF Outside(cd.cp, cage(v)) THEN
|     cage(v) := EverywhereCage;
|     v.position(cd)
|   END

   A split maintains its cage to be a subset of the intersection of
   its children's cages, so that it will receive any cursor positions
   that it owes its children. *)


(* \subsection{The key method} *)

(* Trestle calls a "VBT"'s "key" method to report keystrokes.  The
   method will be called with "LL.sup = mu", and takes an argument of
   type "KeyRec".  \index{key method} *)

TYPE
  KeyRec = RECORD
    whatChanged: KeySym;
    time: TimeStamp;
    wentDown: BOOLEAN;
    modifiers: Modifiers;
  END;

  KeySym = INTEGER;

CONST
  NoKey: KeySym = 0;

(* The method call "v.key(cd)" indicates that the key "cd.whatChanged"
   went up or down at time "cd.time".  The boolean "cd.wentDown" is
   true if the key went down; false if it went up.  The set
   "cd.modifiers" reflects the state of the modifiers (either just
   before or just after the transition; it is not specified which).

   A "KeySym" represents a symbol on a key of the keyboard.  For
   example, there are separate "KeySyms" for upper and lower case
   letters.  The interfaces "Latin1Key" and "KeyboardKey" specify the
   "KeySym" codes for many symbols that occur on standard keyboards.
   These interfaces are shipped with SRC Trestle but are not included
   in the printed version of the reference manual.  The codes are chosen
   to agree with the X Keysym codes (see X Window System, Scheifler
   et al., \cite{XSpec} Appendix E).

   If the keyboard, like most keyboards, has two symbols on some of
   the keys, then the "KeySym" for the down transition and later up
   transition might be different.  For example, if the user pushes the
   left shift key, then the "z"/"Z" key, and then releases the keys in
   the same order, Trestle would report these four transitions:

| `left shift down`, modifiers = {} or {Shift}
| Z `down`, modifiers = {Shift}
| `left shift up`, modifiers = {} or {Shift}
| z `up`, modifiers = {}

   Although the same physical "Z"/"z" key went down and up, the down
   transition is reported for the "Z" "KeySym" and the up transition
   is reported for the "z" "KeySym".

   The constant "NoKey" is simply an unused "KeySym" code.

   To get Trestle to deliver keystrokes to a "VBT", you make the "VBT"
   the owner of the keyboard focus by calling the procedure
   "VBT.Acquire".  *)


(* \subsection{The redisplay method} *)

(* A typical "VBT" has a ``display invariant'' that defines what its
   screen looks like as a function of its state.  When the state changes,
   the display invariant is reestablished by updating the screen.
   \index{redisplay~method} \index{marking~for~redisplay}

   When a series of changes are made, each of which invalidates the
   display invariant, it is undesirable to update the screen
   after every change.  For example, if the border width and the
   border texture of a "BorderedVBT" both change, it is better not
   to paint the intermediate state.

   Therefore, Trestle keeps track of a set of "VBTs" that have been
   ``marked for redisplay''.  Procedures that invalidate a "VBT"'s
   display invariant mark the "VBT" instead of updating the screen
   directly.  Trestle automatically schedules a call to the "redisplay"
   method of every marked window (unless the window's screentype is
   "NIL").  The method takes no arguments: the call "v.redisplay()"
   must reestablish "v"'s display invariant.  It will be called with
   "LL.sup = mu".

   The default redisplay method for a "Leaf" calls the reshape method
   with an empty "saved" rectangle.

   There are several procedures related to redisplay:  *)


PROCEDURE Mark(v: T); <* LL.sup < v *>
(* Mark "v" for redisplay. *)

PROCEDURE IsMarked(v: T): BOOLEAN; <* LL.sup < v *>
(* Return "TRUE" if "v" is marked for redisplay. *)

PROCEDURE Unmark(v: T); <* LL.sup < v *>
(* If "v" is marked for redisplay, unmark it. *)

(* A marked window is automatically unmarked when it is redisplayed,
   reshaped, or rescreened.  Thus the "Unmark" procedure is rarely
   needed.  *)


(* \subsection{The reshape method} *)

(* Trestle calls a "VBT"'s reshape method to report changes in its
   domain.  The method will be called with "LL.sup = mu.v" (as explained
   below), and takes an argument of type "ReshapeRec".
   \index{reshape method} *)

TYPE ReshapeRec = RECORD
  new, prev, saved: Rect.T;
  marked: BOOLEAN
END;

(* The method call "v.reshape(cd)" indicates that the domain of "v"
   has changed from "cd.prev" to "cd.new".  The rectangle "cd.saved"
   is the subset of the previous domain that Trestle has preserved for
   the client in case it is of use in painting the new domain.  This
   is the only case in which Trestle tries to save portions of a "VBT"'s
   screen outside its domain.  After the reshape method returns, Trestle
   will generally forget the old parts of the screen.  The boolean
   "cd.marked" indicates whether "v" was marked when it was reshaped;
   in any case, "v" is automatically unmarked as it is reshaped.

   If "new = Rect.Empty" then the window is no longer visible (for
   example, this happens when the window is iconized).  Any background
   threads that are painting should be stopped, since their efforts
   are useless.

   The default reshape method for a "Leaf" calls the "repaint" method
   to repaint the whole new domain.

   When the reshape method is called, "mu" is locked, and it will remain
   locked until the method returns.  However, Trestle may lock "mu"
   and then reshape, repaint, or rescreen several "VBTs" concurrently,
   so you can't assume that an activation of your reshape method
   excludes the activation of another "VBT"'s reshape, repaint, or
   rescreen method.

   This locking level will be referred to as "v"'s share of "mu",
   and written "mu.v".  Holding "mu" is logically equivalent to holding
   "mu.v" for every "v".  Consequently, "mu.v < mu" in the locking
   order.  Holding "mu.v" does not suffice to call a procedure that
   requires "mu" to be locked; on the other hand you cannot lock "mu"
   while holding "mu.v", since this would deadlock.  *)

(* \subsection{The rescreen method} *)

(* Trestle calls a "VBT"'s rescreen method to report changes to its
   screentype.  The method will be called with "LL.sup = mu.v", and
   takes an argument of type "RescreenRec". \index{rescreen method} *)

TYPE RescreenRec = RECORD
  prev: Rect.T;
  st: ScreenType;
  marked: BOOLEAN;
END;

(* The method call "v.rescreen(cd)" indicates that the screentype of
   "v" has changed to "cd.st" and that its domain has changed from
   "cd.prev" to "Rect.Empty".  (Typically the "VBT" will be reshaped
   to a non-empty domain on the new screentype.)  It is possible that
   "cd.st=NIL".  The boolean "cd.marked" indicates whether "v" was
   marked when it was rescreened; in any case, "v" is automatically
   unmarked as it is rescreened.  "VBT.Leaf.rescreen" reshapes "v" to
   empty. *)

(* \subsection{The repaint method} *)

(* Trestle calls a "VBT"'s repaint method to report that part of its
   screen has been exposed and must be repainted.  The method
   will be called with "LL.sup = mu.v", and takes an argument of type
   "Region.T".  \index{repaint method}

   There are some subtleties if you are scrolling (that is, copying
   bits from one part of the screen to another) at the same time that
   Trestle is activating your repaint method.  To explain them we will
   become more formal and precise.

   Every "VBT" "v" has a ``bad region'' "bad(v)".  For each point "p"
   that is in "Domain(v)" and not in "bad(v)", the pixel "v[p]" is
   displayed to the user; that is, if "vis[p]" denotes what is actually
   visible at pixel "p", then we have the basic invariant
   \index{bad region} \index{exposed region}

| vis[p] = v[p] `for all` p `controlled by "v" and outside` bad(v)

   Trestle can expand "bad(v)" at any time, as though
   cosmic rays had damaged the pixels.

   Whenever "bad(v)" contains pixels that are controlled by "v",
   Trestle will call "v"'s repaint method by setting "exposed(v)"
   (the ``exposed region'' of "v") to include all such pixels, and then
   executing the following code:

| < bad(v) := `the set difference` bad(v) - exposed(v);
|   FOR p `in` exposed(v) DO v[p] := vis[p] END >;
| v.repaint(exposed(v));
| exposed(v) := `the empty set`

   That is, as a pixel "p" is removed from "bad(v)" and added to
   "exposed(v)", the screen "v[p]" is changed to "vis[p]", so that the
   basic invariant is maintained.  You can imagine that the cosmic ray's
   damage has now reached "v[p]", not just "vis[p]".  The angle brackets
   indicate that the shrinking of "bad(v)" and the damaging of "v[p]"
   occur atomically, so that the basic invariant is maintained. (In
   particular, the basic invariant is true whenever you call the
   procedure "VBT.Scroll", where you can find more about the
   bad region and the exposed region.)

   Sometimes it is convenient to do all painting from the repaint
   method; in which case the following procedure is useful:  *)

PROCEDURE ForceRepaint(v: T; READONLY rgn: Region.T);
<* LL.sup < v *>
(* Set "bad(v) := Region.Join(rgn, bad(v))".  If the resulting
   "bad(v)" is non-empty, schedule an activation of "v"'s repaint
   method. *)


(* \subsection{About painting in general} *)

(* Trestle's painting procedures all follow the same pattern.
   The arguments to the procedure specify:

    \medskip\bulletitem a {\it destination}, which is a set of pixels
      in a "VBT"'s screen.  For example, the destination could be a
      rectangle, a trapezoid, a shape bounded by a curved path, or a
      region.

    \medskip\bulletitem a {\it source}, which is conceptually an
      infinite array of pixels, not necessarily of the same depth as
      those on the screen.  For example, the source could be a texture,
      a text string in some font, an explicit bitmap or image, or the
      "VBT"'s screen itself.

    \medskip\bulletitem an {\it operation}, which is a function that takes
      a destination pixel value and a source pixel value and produces a
      destination pixel value.  For example, the operation could be
      planewise "XOR".

   \medskip\noindent The effect of the painting procedure is to apply
   the operation to each pixel in the destination region.  That is,
   if "v" is the "VBT", the effect of the painting procedure is to set
   "v[p] := op(v[p], s[p])" for each point "p" in the destination, where
   "op" is the operation, "v[p]" is the pixel at point "p" of "v"'s
   screen, and "s[p]" is the source pixel at point "p".

   Two useful operations are "PaintOp.Bg" and "PaintOp.Fg",
   defined by

| PaintOp.Bg(d, s) = `the screen's background pixel`
| PaintOp.Fg(d, s) = `the screen's foreground pixel`

   These operations ignore their arguments; they set each destination
   pixel to a constant value, regardless of its previous value or the
   source value.  The actual background and foreground pixels vary from
   screentype to screentype; you can think of "Bg" as white and "Fg"
   as black (unless you prefer video-reversed screens).

   Another useful operation is "PaintOp.Copy", defined by

| PaintOp.Copy(d, s) = s

   For example, "PaintOp.Copy" can be used to paint an eight-bit pixmap
   source on an eight-bit pixmap screen.  It would be an error to use
   "PaintOp.Copy" with a one-bit source and an eight-bit screen---the
   system wouldn't crash, but anything could happen to the destination
   pixels.s

   For more painting operations, see the "PaintOp" interface. *)

(* \subsection{Scrolling (copying one part of the screen to another)} *)


PROCEDURE Scroll(
    v: Leaf;
    READONLY clip: Rect.T;
    READONLY delta: Point.T;
    op: PaintOp.T := PaintOp.Copy); <* LL.sup < v *>
(* Translate a rectangle of "v"'s screen by "delta" and use
   it as a source for the operation "op" applied to each
   destination pixel in the clipping rectangle "clip". *)

(* The "Scroll" procedure uses "v"'s screen as source.  It can
   therefore be used to copy pixels from one part of "v"'s screen
   to another.  Any operation can be used for combining the
   translated pixels with the destination pixels, but the operation
   defaults to "PaintOp.Copy".

   The source rectangle can be computed from "clip" by subtracting
   "delta".  More precisely, "Scroll(v, clip, delta, op)" is equivalent
   to:

| `for each pair of points "p", "q" such that`
|      p `is in` clip,
|      p = q + delta`, and`
|      q `is in` Domain(v)
| `simultaneously assign`
|      v[p] := op(v[p], v[q]);
|      `if "q" is in "exposed(v)" and "p" is not,`
|          `or if "q" is in "bad(v)"`
|      `then add "p" to "bad(v)"`

   By ``simultaneously'' it is meant that the pairs "p", "q" are
   enumerated in an order so that no destination pixel of an early pair
   corresponds to a source pixel of any later pair.
   \index{bad region} \index{exposed region}

   Recall the bad region and exposed region "bad(v)" and "exposed(v)"
   from the description of the repaint method.

   If you do all your painting from within the "repaint", "reshape",
   and "redisplay" methods, then you can ignore the subtleties involving
   the "bad(v)" and "exposed(v)".  But if you have any asynchronous
   threads that call "Scroll", you have to be careful.  For example,
   suppose you do all your painting from a concurrent worker thread,
   and arrange for your repaint and reshape methods to simply add entries
   to the worker thread's queue recording the painting that must be
   done.  Then you must be careful to avoid the following sequence of events:

    \medskip\bulletitem The worker thread removes from its work queue
   an item indicating that it must repaint some region "A", and
   determines that the best way to do this is to scroll some other
   region "B".

    \medskip\bulletitem The "repaint" method is activated with exposed
   region "B"; it adds "B" to the work queue and returns.  As it
   returns, the system sets the "VBT"'s bad and exposed regions to be
   empty. (See the description of the "repaint" method.)

    \medskip\bulletitem The worker thread copies the garbage from "B"
    into "A".

   \medskip Eventually the worker thread will get around to repainting
   "B", but the damage to "A" will never be repaired.

   To avoid this race condition, the repaint method should convey
   the bad region to the worker thread by a separate communication path,
   rather than simply put it the ordinary work queue.  The worker thread
   can thus avoid using bad bits as the source of scroll operations.

   Of course it is possible for the scrolling to happen after the
   "repaint" method is called but before the method has conveyed the
   bad region to the worker thread.  There is no way to prevent this
   sequence of events, but there is no need to, either: in this case
   the source of the scroll operation will be in the exposed region
   (since the "repaint" method has not yet returned), and therefore
   (by the specification above) the call to "Scroll" will expand the
   bad region.  This will eventually lead to the repaint method being
   activated a second time, repairing the damage.

   In short, in order to allow concurrent painting, we do not clear the
   exposed region until the "repaint" method returns, and we specify that
   a scroll from a "q" in "bad(v)" or "exposed(v)" to a "p" that is
   not in "bad(v)" invalidates the destination.

   Notice that a scroll from "exposed(v)" to "exposed(v)" does not
   invalidate the destination.  This allows the repaint method to paint
   a portion of "exposed(v)" and then scroll that portion to other parts
   of "exposed(v)"---unusual, but legal. *)

(* \subsection{Painting textures} *)

(* This section describes procedures for texturing rectangles, regions, and trapezoids. *)

PROCEDURE PaintTexture(
    v: Leaf;
    READONLY clip: Rect.T;
    op: PaintOp.T := PaintOp.BgFg;
    src: Pixmap.T;
    READONLY delta := Point.Origin); <* LL.sup < v *>
(* Paint the rectangle "clip" with the texture "src+delta" using
   the operation "op". *)

(* A {\it texture} is an infinite periodic pixmap.  A texture "txt"
is represented by a pixmap "src" with a finite non-empty rectangular
domain "Domain(src)"; the rule is that "txt" is the result of tiling
the plane with translates of the pixmap "src".  Using the convenient
procedure "Rect.Mod" we can state this rule as: "txt[p] = src[Rect.Mod(p,
Domain(src))]".

The texture "src+delta" is the translation of the texture "src" by
the vector "delta".

Putting this all together, "PaintTexture(v, clip, op, src, delta)"
is equivalent to:

| `for each pair of points "p", "q" such that`
|     p `is in` clip `and`
|     p = q + delta
| `assign`
|     v[p] := op(v[p], src[Rect.Mod(q, Domain(src))]).

   Note that setting "delta" to "Point.Origin" causes the texture to
   be aligned in an absolute coordinate system independent of the domain
   of the window (which helps to make textures in different windows
   match), while setting it to the northwest corner of "v"'s domain
   causes the texture to be aligned in the window's coordinate system
   (which allows a window to be reshaped by scrolling the old domain
   into the new).

   If "src"'s domain is empty, the effect is undefined but limited to
   the clipping region.

   The default paint operation for PaintTexture is "BgFg", defined by

| PaintOp.BgFg(d, 0) = `the screen's background pixel`
| PaintOp.BgFg(d, 1) = `the screen's foreground pixel`

   This paint operation is only appropriate if "src" is one-bit deep;
   the effect is to copy the source to the destination, interpreting
   0 as background and 1 as foreground.  *)


PROCEDURE PaintTint(
    v: Leaf;
    READONLY clip: Rect.T;
    op: PaintOp.T); <* LL.sup < v *>
(* Paint the rectangle "clip" with the texture "Pixmap.Solid" using
   the operation "op". *)

(* For example, "PaintTint(v, clip, PaintOp.Bg)" paints "clip" with
   the background color, and "PaintTint(v, clip, PaintOp.Fg)" paints
   "clip" with the foreground color.  *)

PROCEDURE PolyTint(
    v: Leaf;
    READONLY clip: ARRAY OF Rect.T;
    op: PaintOp.T); <* LL.sup < v *>
(* Paint each rectangle "clip[i]" in order with the texture
   "Pixmap.Solid" using the operation "op".  *)


PROCEDURE PolyTexture(
    v: Leaf;
    READONLY clip: ARRAY OF Rect.T;
    op: PaintOp.T := PaintOp.BgFg;
    src: Pixmap.T;
    READONLY delta := Point.Origin); <* LL.sup < v *>
(* Paint each rectangle "clip[i]" in order with the texture "src+delta" using
   the operation "op". *)


PROCEDURE PaintRegion(
    v: Leaf;
    READONLY rgn: Region.T;
    op: PaintOp.T := PaintOp.BgFg;
    src: Pixmap.T := Pixmap.Solid;
    READONLY delta := Point.Origin); <* LL.sup < v *>
(* Paint the region "rgn" with the texture "src+delta" using
   the operation "op". *)


PROCEDURE PaintTrapezoid(
    v: Leaf;
    READONLY clip: Rect.T;
    READONLY trap: Trapezoid.T;
    op: PaintOp.T := PaintOp.BgFg;
    src: Pixmap.T := Pixmap.Solid;
    READONLY delta := Point.Origin); <* LL.sup < v *>
(* Paint the intersection of "clip" and "trap" with the texture
   "src+delta" using the operation "op". *)


(* \subsection{Filling and stroking paths} *)

(* Trestle also supports PostScript-like graphics operations
   \cite{PostScript}: *)

TYPE
  WindingCondition = {Odd, NonZero};
  EndStyle = {Round, Butt, Square};
  JoinStyle = {Round, Bevel, Miter};


PROCEDURE Fill(
    v: Leaf;
    READONLY clip: Rect.T;
    path: Path.T;
    wind := WindingCondition.NonZero;
    op: PaintOp.T := PaintOp.BgFg;
    src: Pixmap.T := Pixmap.Solid;
    READONLY delta := Point.Origin); <* LL.sup < v *>
(* Paint the intersection of "clip" and the region entwined by "path"
   with the texture "src+delta" using the operation "op". *)

(* The point "p" is entwined by "path" if the winding number of "path"
   around "p" satisfies the winding condition "wind".  To ensure that
   the winding number is defined even for the points on the path, the
   path is regarded as translated north by $\epsilon$ and west by
   $\epsilon^2$, where $\epsilon$ is infinitesimal.  *)

PROCEDURE Stroke(
    v: Leaf;
    READONLY clip: Rect.T;
    path: Path.T;
    width: CARDINAL := 0;
    end := EndStyle.Round;
    join := JoinStyle.Round;
    op: PaintOp.T := PaintOp.BgFg;
    src: Pixmap.T := Pixmap.Solid;
    READONLY delta := Point.Origin); <* LL.sup < v *>
(* Paint the intersection of "clip" and the stroke determined by "path",
   "end", and "join" with the texture "src+delta" using the operation
   "op".  *)

(* The exact results of "Stroke" are different on different Trestle
   implementations.  The approximate specification is like PostScript:

   If "end = Round" and "join = Round", the path is drawn by a
   circular brush of diameter "width" that traverses the path.

   If "end = Butt", then the ends of unclosed trails in the path are
   stroked by a line segment of length "width" centered and
   perpendicular to the path in the neighborhood of the endpoint. If
   "end = Square", the path is extended at the endpoint by a straight
   line segment of length "width/2" tangent to the path and a butt end is
   drawn.

   If "join = Bevel", the joint between two patches is constructed
   by using "Butt" endstyles for them and then filling the triangular
   notch that remains. If "join = Miter", then instead of just
   filling the triangular notch, the outer edges of the two lines are
   extended to meet at a point, and the resulting quadrilateral is
   filled.

   If "width = 0", "join" is ignored and "end" determines whether the
   final endpoint of an open subpath should be drawn: if "end" is
   "Butt", the final endpoint is omitted, otherwise it is drawn.

   If "join = Miter", "width > 0", and the angle formed by the two
   segments meeting at some joint is small, then the tip of the miter may
   extend quite far from the joint point. Trestle implementations are
   free to bevel those joints whose angle is smaller than some
   implementation-dependent {\it miter limit}. The miter limit will be
   made available to clients in some to-be-determined interface.  On X,
   the miter limit is 11 degrees.

   Finally, there is a convenience procedure for stroking a path
   containing a single straight line segment: *)


PROCEDURE Line(
    v: Leaf;
    READONLY clip: Rect.T;
    p, q: Point.T;
    width: CARDINAL := 0;
    end := EndStyle.Round;
    op: PaintOp.T := PaintOp.BgFg;
    src: Pixmap.T := Pixmap.Solid;
    READONLY delta := Point.Origin); <* LL.sup < v *>
(* Like "Stroke" applied to the path containing the segment "(p,q)". *)


(* \subsection{Painting pixmaps} *)

(* The following procedure paints a pixmap without replicating it into an
   infinite texture: *)

PROCEDURE PaintPixmap(
    v: Leaf;
    READONLY clip: Rect.T := Rect.Full;
    op: PaintOp.T := PaintOp.BgFg;
    src: Pixmap.T;
    READONLY delta: Point.T); <* LL.sup < v *>
(* Translate the pixmap "src" by "delta" and paint it on the screen
of "v", using the operation "op" and clipping to the rectangle "clip".
*)

(* More precisely, "PaintPixmap(v, clip, op, src, delta)" is
equivalent to

| `for each pair of points "p", "q" such that`
|     p `is in` clip,
|     q `is in` Domain(src)`, and`
|     p = q + delta,
| `assign`
|     v[p] := op(v[p], src[q])

Since a "Pixmap.T" is a screen-independent resource, you can't read its domain without specifying the "VBT" it is to be used on: *)


PROCEDURE PixmapDomain(v: T; pix: Pixmap.T): Rect.T;
<* LL.sup < v *>
(* Return the domain of "pix" on the screentype of "v". *)

(* It is also possible to paint screen-dependent pixmaps: *)

PROCEDURE PaintScrnPixmap(
    v: Leaf;
    READONLY clip: Rect.T := Rect.Full;
    op: PaintOp.T := PaintOp.Copy;
    src: ScrnPixmap.T;
    READONLY delta: Point.T); <* LL.sup < v *>
(*  Like "PaintPixmap", but with a screen-dependent pixmap instead
of a screen-independent pixmap.  *)

(* If "src" does not have an appropriate screentype for "v", the effect
   of the procedure is undefined but limited to the clipping
   region.

   Because Trestle batches painting operations, the pixmap "src"
   must be regarded as still in use after "PaintScrnPixmap"
   returns.  If you wish to free the pixmap by calling "src.free()",
   you should first call "VBT.Sync(v)". *)

(* \subsection{Painting text} *)

(* The text painting procedures take an optional array of
displacements, whose entries have the following type: *)

TYPE
  DeltaH = [-512 .. 511];
  Displacement =
    RECORD index: CARDINAL; dh: DeltaH END;

(* A displacement "d" causes all characters whose index in the text
   is "d.index" or greater to be displaced "d.dh" pixels to the right.
   The first character has index "0". The "d.index" values in an array
   of displacements must be non-decreasing.  *)


PROCEDURE PaintText(
    v: Leaf;
    READONLY clip: Rect.T := Rect.Full;
    READONLY pt: Point.T;
    fnt: Font.T := Font.BuiltIn;
    t: TEXT;
    op: PaintOp.T := PaintOp.TransparentFg;
    READONLY dl := ARRAY OF Displacement{});
<* LL.sup < v *>
(* Paint the text "t" onto the screen of "v", starting
   at position "pt", using the font "fnt", the operation "op",
   and the displacement list "dl". *)

(* The arguments to "PaintText" must satisfy at least one of the
   following two conditions:

   \medskip\bulletitem the background operation is transparent; that is,
   "op(p, 0) = p" for any pixel "p", or

   \medskip\bulletitem the font is self-clearing (see below) and
   "dl" is empty.

   \medskip\noindent If neither condition is true, the effect of "PaintText" is
   implementation-dependent, but is confined to the clipping rectangle.

   The "ScrnFont" interface defines the properties of fonts.
   Here we introduce names for the properties needed to
   explain "PaintText".  If "f" is a font and "ch" is a character, then

       \medskip\bulletitem  "printWidth(ch, f)" is the printing width of "ch";
       that is, the amount to increment the reference point when "ch" is
       printed in font "f";

       \medskip\bulletitem "bits(ch, f)" is the bitmap for "ch" in "f", which
        is positioned with "ch"'s reference point at the origin;

       \medskip\bulletitem "height(ch, f)" is the height of "ch" above
       the baseline; that is, the number of rows of "bits(ch, f)" whose
       "v"-coordinate is at most zero; and "depth(ch, fnt)" is the
       number of rows of "bits(ch, f)" whose "v"-coordinate exceeds
       zero;

       \medskip\bulletitem "ascent(f)" and "descent(f)" are the logical extent
       of "f" above and below the baseline.  Some characters may extend
       higher or lower.

\medskip\noindent A font is {\it self-clearing} if

\medskip\bulletitem
each character's
height and depth equal the font's ascent and descent, and

\medskip\bulletitem
each character's
"printWidth" equals the width of its bitmap and each character's
reference point is at the west boundary of its bitmap (or each
character's "printWidth" equals the negative of the width of its bitmap
and each character's reference point is at the east boundary of its
bitmap).

\medskip\noindent The call to "PaintText" is equivalent to the following loop:

| rp := pt;
| i := 0;
| LOOP
|   IF dl # NIL THEN
|     FOR j := 0 TO HIGH(dl^) DO
|       IF dl[j].index = i THEN INC(rp.h, dl[j].dh) END
|     END
|   END;
|   IF i = Text.Length(t) THEN EXIT END;
|   PaintPixmap(v, clip, op, bits(t[i], fnt), rp);
|   rp.h := rp.h + PrintWidth(t[i], fnt);
|   i := i + 1
| END
    *)

(* The following two procedures are useful for computing the sizes
   of texts.  Since fonts are screen-independent, they take the
   "VBT" whose screentype is to be used: *)

PROCEDURE BoundingBox
  (v: Leaf; txt: TEXT; fnt: Font.T): Rect.T;
  <* LL.sup < v *>
(* Return the bounding box of the text "txt" if it were painted
   at the origin on the screen of "v". *)

(* More precisely, let "r" be the smallest rectangle that contains the
   bounding boxes of the characters of "txt" if "txt" were painted on
   "v" in the font "fnt" with "txt"'s reference point at the origin.
   Then "BoundingBox" returns a rectangle with the same horizontal extent
   as "r", but whose height and depth are the maximum height and depth
   of any character in the font.  *)

PROCEDURE TextWidth
  (v: Leaf; txt: TEXT; fnt: Font.T): INTEGER;
  <* LL.sup < v *>
(* Return the sum of the printing widths of the characters in "txt"
   in the font "fnt".  *)

(* "TextWidth" returns the displacement of the reference point
   that would occur if "t" were painted on "v" in font "fnt".  It may
   differ from the width of "BoundingBox(txt, fnt)", since the printing
   width of the last character can be different from the width of its
   bounding box, and the reference point for the first character might
   not be at the left edge of "txt"'s bounding box.

   You can paint characters out of an array instead of a "TEXT": *)


PROCEDURE PaintSub(
    v: Leaf;
    READONLY clip: Rect.T := Rect.Full;
    READONLY pt: Point.T;
    fnt: Font.T := Font.BuiltIn;
    READONLY chars: ARRAY OF CHAR;
    op: PaintOp.T := PaintOp.TransparentFg;
    READONLY dl :=  ARRAY OF Displacement{});
    <* LL.sup < v *>
(* Like "PaintText" applied to the characters in "chars". *)

(* \subsection{Synchronization of painting requests} *)

(* To improve painting performance, Trestle combines painting commands
   into batches, and sends them to the server a batch at a time.

   Most applications can ignore the batching, but the procedures in this
   section can be of use in applications where the timing of paint
   operations is critical.

   For example, when replacing one line of
   text with another in a non-self-clearing font, the old text must
   be erased before the new text is painted.  If the painting command
   that erases the old text happens to fall at the end of a batch,
   there may be a delay of several milliseconds between the time it
   affects the screen and the time the following paint text command affects
   the screen, which can produce an undesirable flickering effect.  The
   chances of this happening can be greatly reduced by enclosing the
   two commands in a {\it group}, using the following two procedures:
   \index{paint batch} \index{batch (of painting commands)} *)


PROCEDURE BeginGroup(v: Leaf; sizeHint: INTEGER := 0);
<* LL.sup < v *>
(* Begin a group of painting commands. *)

PROCEDURE EndGroup(v: Leaf); <* LL.sup < v *>
(* End the current group of painting commands. *)

(* If a group of painting commands are bracketed by "BeginGroup" and
   "EndGroup", Trestle will try to avoid introducing delays between
   the commands, such as might otherwise be introduced by batching.
   Trestle assumes that you will generate the painting commands and
   the "EndGroup" in rapid succession.

   Increasing the value of "sizeHint" may improve atomicity, at
   the cost of throughput.  The maximum
   useful value of "sizeHint" is the total size in bytes of the painting
   commands in the group, which you can compute using the interface
   "PaintPrivate". *)


PROCEDURE Sync(v: Leaf; wait := TRUE); <* LL.sup < v *>
(* Force all painting commands issued to "v" prior to the call to be
   executed. If "wait" = FALSE then Sync just flushes the output queue
   and returns. Otherwise, Sync waits until it believes the commands
   in the output queue have been completed. *)


(* \subsection{Screen capture} *)


PROCEDURE Capture(
    v: T;
    READONLY clip: Rect.T;
    VAR (*out*) br: Region.T)
    : ScrnPixmap.T; <* LL.sup < v *>
(* Return a pixmap containing the part of "v"'s screen in
   the rectangle "rect". *)

(* The screentype of the result will be the same as the screentype of
   "v".  Because a "VBT"'s screen is forgetful, it may be impossible
   to read the requested region.  In this case "br" is set to contain
   all positions of pixels that were not copied.  Naturally, Trestle
   makes "br" as small as it can.  If none of the bits are available,
   the result may be "NIL".
   \index{reading the screen} *)

(* \subsection{Controlling the cursor shape} *)

(* Every "VBT" "v" contains a field "cursor(v)", which is set with the following procedure: *)

PROCEDURE SetCursor(v: T; cs: Cursor.T);
<* LL.sup < v *>
(* Set "cursor(v)" to "cs". *)

(* A split displays the cursor of its mouse focus, or of its current
   child if its mouse focus is "NIL".  Only if the cursor of the relevant
   child is "Cursor.DontCare" or if there is no relevant child does the
   split display its own cursor.
   \index{cursor shape, how to change}

   To be more precise, the shape of the cursor over the top level
   window "v" is determined by the following recursive procedure:

| GetCursor(v) =
|   IF NOT ISTYPE(v, Split) THEN
|     RETURN cursor(v)
|   ELSE
|     IF mouseFocus(v) # NIL THEN
|       cs := GetCursor(mouseFocus(v))
|     ELSIF current(v) # NIL THEN
|       cs := GetCursor(current(v))
|     ELSE
|       cs := Cursor.DontCare
|     END;
|     IF cs = Cursor.DontCare THEN
|       RETURN cursor(v)
|     ELSE
|       RETURN cs
|     END
|   END
 *)


(* \subsection{Selections} *)

(* Trestle maintains an internal table of named selections,
   which initially contains several selections of general use,
   and which can be extended by users: *)


TYPE Selection = RECORD sel: CARDINAL END;

PROCEDURE GetSelection(name: TEXT): Selection;
<* LL arbitrary *>
(* Return the selection with the given name, creating it if necessary. *)

PROCEDURE SelectionName(s: Selection): TEXT;
<* LL arbitrary *>
(* Return the name used to create "s", or "NIL" if "s" is unknown. *)

VAR (*CONST*)
  NilSel:  Selection (* := GetSelection("NilSel") *);
  Forgery: Selection (* := GetSelection("Forgery") *);
  KBFocus: Selection (* := GetSelection("KBFocus") *);
  Target:  Selection (* := GetSelection("Target") *);
  Source:  Selection (* := GetSelection("Source") *);


(* "NilSel" and "Forgery" are reserved for Trestle's internal use.
   The owner of "KBFocus" (the keyboard focus) is the "VBT" that
   receives keystrokes.
   \index{input or keyboard focus} \index{keyboard focus}

   We offer the following suggestions for the use of target and
   source selections:
   \index{target selection}
   \index{source selection}

    \medskip\bulletitem The target selection.  If text, this should
      be underlined black or reverse video.  The selection gesture
      should not require modifiers like shift or control.

    \medskip\bulletitem The source selection.  If text, this should
      be underlined gray.  The source gesture should be a modified
      version of the gesture for making the target selection.

   \medskip An operation like ``copy'' should replace the
   target selection with the value of the source selection. *)


(* The following exception declaration provides for the errors
   that can occur in dealing with selections. *)

EXCEPTION Error(ErrorCode);

TYPE ErrorCode =
  {EventNotCurrent, TimeOut, Uninstalled, Unreadable,
   Unwritable, UnownedSelection, WrongType};

(* Explanation of error codes:

   \medskip\bulletitem "EventNotCurrent":  Raised by attempts to access a
   selection with an event time that is not current.

   \medskip\bulletitem "TimeOut":  If you attempt to read or write a selection,
   and the selection owner's method does not return for an unreasonably
   long time, then Trestle stops waiting and raises this exception.

   \medskip\bulletitem "Uninstalled":  Raised by event-time operations on
   uninstalled "VBTs"; that is, on "VBTs" none of whose ancestors have
   been connected to a window system by one of the installation
   procedures in the "Trestle" interface.

   \medskip\bulletitem "Unreadable", "Unwritable":  Raised by attempts to read
   an unreadable selection, or write an unwritable selection.

   \medskip\bulletitem "UnownedSelection":  Raised by attempts to read, write,
   or deliver miscellaneous codes to the owner of an unowned selection.

   \medskip\bulletitem "WrongType":  Raised by attempts to read or write a
   selection with a type not supported by the selection owner.  *)


(* \subsection{Acquiring and releasing selection ownership} *)

PROCEDURE Acquire(
    v: T;
    s: Selection;
    t: TimeStamp)
  RAISES {Error}; <* LL.sup < v *>
(* Make "v" the owner of selection "s", provided that "t" is the current
   event. *)

(* If "Acquire(v, s, t)" is successful, the previous owner of the
   selection will receive a miscellaneous code of type "Lost" (even
   if the owner is "v").  The window system affected is the one
   to which "v" is connected.  The possible error codes are
   "EventNotCurrent" and "Uninstalled".  *)


PROCEDURE Release(v: T; s: Selection);
<* LL.sup < v *>
(* If the current owner of "s" is "v", then a "Lost" code is queued
   for delivery to "v" and the owner of "s" becomes "NIL" *)

(* The window system affected is the one to which "v" is connected.
   "Release" is a no-op if the current owner is not "v" or if "v" is
   not installed.  *)

(* \subsection{The miscellaneous method} *)

(* Trestle calls a "VBT"'s "misc" method to deliver miscellaneous
   codes.  The method will be called with "LL.sup = mu", and takes an
   argument of type "MiscRec".  \index{misc method}

   Trestle maintains an internal table of named miscellaneous code
   types, which initially contains several types of general interest,
   and which can be extended by users.  *)

TYPE MiscRec = RECORD
  type: MiscCodeType;
  detail: MiscCodeDetail;
  time: TimeStamp;
  selection: Selection;
END;

  MiscCodeType = RECORD typ: CARDINAL END;
  MiscCodeDetail = ARRAY [0 .. 1] OF INTEGER;

PROCEDURE GetMiscCodeType(name: TEXT): MiscCodeType;
<* LL arbitrary *>
(* Return the MiscCodeType with the given name, creating it if
   necessary.  *)

PROCEDURE MiscCodeTypeName(type: MiscCodeType): TEXT;
<* LL arbitrary *>
(* Return the name used to create "s", or "NIL" if "s" is unknown. *)

CONST
   NullDetail = MiscCodeDetail {0, ..};

(* The interface "MiscDetail" provides some convenient procedures for
   encoding a REF as an integer for internal miscellaneous codes. *)

VAR (*CONST*)
   Deleted: MiscCodeType;
   Disconnected: MiscCodeType;
   TakeSelection: MiscCodeType;
   Lost: MiscCodeType;
   TrestleInternal: MiscCodeType;
   Moved: MiscCodeType;

(* These ``variables'' are really constants for the following codes:

| GetMiscCodeType("Deleted")
| GetMiscCodeType("Disconnected")
| GetMiscCodeType("TakeSelection")
| GetMiscCodeType("Lost")
| GetMiscCodeType("TrestleInternal")
| GetMiscCodeType("Moved")

   The method call "v.misc(cd)" sends "v" the misc code relevant
   to "cd.selection" as part of the event "cd.time".  The meaning of
   the "type" and "detail" fields is up to the application, except for
   the following.

   A "Deleted" code is delivered to a top-level window when it is
   explicitly deleted from its server, either by a user command to the window
   manager or under program control.  A "Disconnected" code is delivered
   to a top-level window when it is disconnected from its server, either
   because the server crashed or because the network connection was
   lost.  A "TakeSelection" code is delivered to a top-level window
   when the user has gestured that it would like the window to acquire
   the indicated selection; most often the keyboard focus.  (The nature
   of the gesture is between the user and the window manager.  Many
   applications also acquire the keyboard focus in response to mouse
   clicks.)  A "Lost" code with "selection = s" will be delivered to
   a window when it loses ownership of "s".  "TrestleInternal" codes
   are reserved for the implementation.  A "Moved" code is delivered to a
   top-level window when it is moved in a way that does not cause a
   rescreen or a reshape to be delivered.

   The timestamp in a "TakeSelection" code is the timestamp for the
   current event and is therefore valid for event-time operations.  The
   timestamps in "Deleted", "Disconnected", "Lost", "Moved" codes are not.
   The selection field is relevant in "Lost" and "TakeSelection" codes;
   it is irrelevant in "Deleted", "Disconnected", "Moved" codes.

   *)


(* \subsection{Sending miscellaneous codes} *)

(* You can send a miscellanous code to the owner of a selection by using
   the following procedure: *)

PROCEDURE Put(
    v: T;
    s: Selection;
    t: TimeStamp;
    type: MiscCodeType;
    READONLY detail := NullDetail)
  RAISES {Error}; <* LL.sup < v *>
(* Create a "MiscRec" with the given fields and enqueue it for delivery
  to the owner of selection "s", if "t" is the current event-time.
  *)

(* The window system affected is the one to which "v" is connected.  The
   possible error codes are "EventNotCurrent", "Uninstalled", and
   "UnownedSelection".  If the selection is unowned it is possible that
   the "Put" will be silently ignored. *)

(* \subsection{Circumventing event-time} *)

(* The following procedure offers an escape from the event-time protocol.
   For example, a long-running thread that has no idea what the current
   event time is can forge a miscellaneous code to itself and use its
   timestamp to acquire the keyboard focus. (Your users may not like it
   if you do this.) *)


PROCEDURE Forge(
    v: T;
    type: MiscCodeType;
    READONLY detail := NullDetail)
  RAISES {Error}; <* LL.sup < v *>
(* Create a "MiscRec" with the given "type" and "detail" fields, with
   selection field "Forgery", and with a newly created timestamp and
   enqueue it for delivery to "v".  *)

(* The timestamp will be valid for event-time operations (provided that
   it is used promptly).  Forging codes that have meaning to the window
   manager (e.g., a "Deleted" code) could have unexpected effects if
   they are delivered to installed windows or their descendants.  The
   only possible error code is "Uninstalled".  *)

(* \subsection{Communicating selection values} *)

(* When you read the value of a Trestle selection you get a result of
   type "Value": *)

TYPE
  Value <: Value_Public;
  Value_Public =
    OBJECT METHODS toRef(): REFANY RAISES {Error} END;


(* Call the "toRef" method to convert the "Value" into a "REFANY".

  The simplest way to construct a "Value" is with the following
  procedure: *)

PROCEDURE FromRef(r: REFANY): Value;
<* LL.sup <= mu *>
(* Return a "Value" "v" such that "v.toRef()" is equal to the result
   of pickling and unpickling "r".  *)

(* On a system without pickles, the value "r" must have type "TEXT".
   If "r" does not have type "TEXT", any exceptions raised by pickling
   lead to checked run-time errors.

   Using "FromRef" leads to synchronous transmission of selection
   values---that is, the value is transferred as part of the call to
   "Read" or "Write".  To get asynchronous behavior, allocate your own
   "Values" and override the "toRef" method.  Trestle will transmit
   the "Value" to the other application, and only when that application
   calls the "toRef" method will your "toRef" method be called.

   The "toRef" method in a "Value" will be called with "LL.sup <= mu".
   The "toRef" method can raise the error "Unreadable" if, for example,
   the address space of the selection owner has been destroyed.  It can
   also raise the error "WrongType" if the underlying "REFANY" cannot be
   represented in the address space calling the method; this can only
   happen with non-"TEXT" selections.

   The procedure "Ready" tests whether a value is synchronous or
   asynchronous: *)

PROCEDURE Ready(v: Value): BOOLEAN; <* LL.sup <= mu *>
(* Return "TRUE" if calling "v.toRef()" will return quickly; return
   "FALSE" if calling "v.toRef()" might be slow or block. *)

(* Finally, here are the procedures for reading and writing selections: *)

PROCEDURE Read(
    v: T;
    s: Selection;
    t: TimeStamp;
    tc: INTEGER := -1)
    : Value
  RAISES {Error}; <* LL.sup <= mu *>
(* Return the value of selection "s" as a reference of type "tc",
  if "t" is the current event-time. *)

(* If "tc = -1", "Read" uses the typecode for "TEXT".  The window system
   affected is the one to which "v" is connected.  The "KBFocus"
   selection is always unreadable.  If the selection owner's read method
   is erroneous, calling the "toRef" method of the returned "Value"
   may produce a reference with a typecode other than "tc".  The
   possible error codes are "EventNotCurrent", "Uninstalled",
   "Unreadable", "WrongType", "TimeOut", and "UnownedSelection".  *)


PROCEDURE Write(
    v: T;
    s: Selection;
    t: TimeStamp;
    val: Value;
    tc: INTEGER := -1)
  RAISES {Error}; <* LL.sup <= mu *>
(* Replace the selection "s" with the value "val", which encodes a
   reference with typecode "tc", assuming "t" is the current event-time.
   *)

(* If "tc = -1", "Write" uses the typecode for "TEXT".  The window
   system affected is the one to which "v" is connected.  The "KBFocus"
   selection is always unwritable.  The possible error codes are
   "EventNotCurrent", "Uninstalled", "Unwritable", "TimeOut", and
   "WrongType".  *)


(* \subsection{The read and write methods} *)

(* Trestle calls a "VBT"'s read and write methods to access any
   selections that it owns.  The method will be called with "LL.sup
   <= mu" (see below). \index{read method}

   The signature of the read method is

| (s: Selection; tc: CARDINAL): Value RAISES {Error}

   Trestle calls "v.read(s, tc)" whenever "v" is the owner of
   selection "s" and some application passes "s" and "tc" to "Read".
   The method should return the value of the selection, or raise
   "Error(Unreadable)" if for some reason the value cannot be delivered,
   or "Error(WrongType)" if the selection cannot be converted to the
   requested type.  The methods will be called with "LL.sup <= mu";
   in fact, if the caller of "Read" is in the same address space, "LL"
   for the method call is the same as "LL" for the caller of "Read",
   else "LL" for the method call is "{}".

   The signature of the write method is

| (s: Selection; val: Value; tc: CARDINAL)
| RAISES {Error}

   Trestle calls "v.write(s, val, tc)" whenever "v" is the owner of
   selection "s" and some application passes "s", "val", and "tc" to
   "Write".  The method should replace the selection with the value
   of "val", or raise the exception with error code "Unwritable" if
   for some reason the selection is not writable, or with error code
   "WrongType" if the selection cannot be written with the requested
   type.  Trestle does not enforce any consistency between "tc" and
   the typecode of the reference "val.toRef()".  For example, if
   "val.toRef()" is "NIL", the meaning could be determined by "tc".
   The locking level is the same as for the read method.  \index{write
   method}

   While a read or write method is active in a descendant of an
   installed window, Trestle will block the delivery to that window
   of any mouse or key events, misc codes, or cursor positions.  If the
   computations are long, it is therefore preferable to do them
   asynchronously, to avoid blocking the user.  *)


(* \subsection{Controlling the shape of a VBT} *)

(* The preferred shape of a "VBT" is represented by a pair of
   records of type "SizeRange", one for each axis: *)

TYPE SizeRange = RECORD lo, pref, hi: CARDINAL END;

CONST DefaultShape =
  SizeRange{lo := 0, pref := 0, hi := 99999};

(* If a "VBT"'s preferred shape in the axis "ax" is the "SizeRange"
   "sh", then the desirable sizes for the "VBT" in axis "ax"
   range from "sh.lo" to "sh.hi-1", and its preferred size is
   "sh.pref".

   A "SizeRange" "sh" is illegal unless "sh.lo <= sh.pref < sh.hi".

   When a parent "VBT" divides its screen up between its children, it
   tries to satisfy its children's shape requirements, which it finds
   by calling the children's shape method.

   The signature of the shape method is

| (ax: Axis.T; n: CARDINAL): SizeRange

   The behavior of the shape method depends on whether "n" is zero.
   The call "v.shape(ax, 0)" returns the preferred shape for "v" in
   the "ax" axis, assuming nothing is known about its size in the other
   axis.  If "n#0", the call "sh := v.shape(ax, n)" returns the
   preferred shape for "v" in the "ax" axis assuming that "v"'s size
   in the other axis is "n". When the method is called, "LL.sup = mu.v".

   It is a checked runtime error for a shape method to return
   an illegal size range.  A common error is to return an illegal
   size range with "sh.lo = sh.hi".

   The child must not assume that its shape requirement is satisfied,
   since, for example, the requirements of a split's children can be
   inconsistent.

   The default "shape" method for a "Leaf" returns "DefaultShape".

   When the preferred shape of a "VBT" changes, you should
   call "NewShape":  *)


PROCEDURE NewShape(v: T);
<* LL.sup >= mu.v AND LL.sup < v *>
(* Notify "v"'s parent that its preferred size range may have changed. *)

(* Typically, the parent will mark itself, and any change will take effect
  at the time of the next redisplay.  Notice that the locking level
  allows "NewShape" to be called from a "reshape" or "rescreen" method;
  it can also be called from a thread that has "mu" locked.  *)


(* \subsection{Putting properties on a VBT} *)

(* Associated with each window is a ``property set'', which is a set
   of non-nil traced references.  \index{property set, of window} *)


PROCEDURE PutProp(v: T; ref: REFANY); <* LL.sup < v *>
(* Add "ref" to "v"'s property set, replacing any existing reference of
   the same type as "ref".  This is a checked runtime error if "ref" is
   "NIL". *)


PROCEDURE GetProp(v: T; tc: INTEGER): REFANY;
<* LL.sup < v *>
(* Return the element of "v"'s property set with typecode "tc", or
   "NIL" if no such element exists. *)


PROCEDURE RemProp(v: T; tc: INTEGER); <* LL.sup < v *>
(* Remove the element with typecode "tc" from "v"'s property set, if one
   exists. *)


(* \subsection{Discarding a VBT} *)

(* It is good form to call "VBT.Discard(v)" when "v" is about to be
   garbage-collected:  \index{discard method} *)

PROCEDURE Discard(v: T); <* LL.sup = mu *>
(* Prepare for and call "v.discard()".  *)

(* The discard method will be called with "LL.sup = mu", and takes
   no argument.  The method should perform any class-dependent cleanup
   that is needed.  The default discard method is a no-op. *)

END VBT.



