(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Trestle.def, by gnelson and msm. *)
(* Last modified on Wed May 19 12:22:47 PDT 1993 by msm     *)
(*      modified on Mon Feb 24 13:58:14 PST 1992 by muller  *)
(*      modified on Sat Dec 21 16:12:26 PST 1991 by gnelson *)
(*      modified on Thu Jul 26 14:53:37 PDT 1990 by steveg *)
<*PRAGMA LL*>

(* The "Trestle" interface provides routines for connecting to window
   systems; installing, decorating, and moving top-level windows, and 
   performing related operations.  *)

INTERFACE Trestle;

IMPORT VBT, Rect, Point, Region, ScrnPixmap, TrestleComm;

TYPE
  T <: ROOT;

(* A "Trestle.T" identifies an instance of a window system.  All the 
   routines in this interface that take a "Trestle.T" accept the value "NIL",
   which represents the default window system obtained by calling
   "Connect(NIL)". *)

PROCEDURE Install(
    v: VBT.T;
    applName: TEXT := NIL;
    inst: TEXT := NIL;
    windowTitle: TEXT := NIL;
    iconTitle: TEXT := NIL;
    bgColorR: REAL := -1.0;
    bgColorG: REAL := -1.0;
    bgColorB: REAL := -1.0;
    iconWindow: VBT.T := NIL;
    trsl: T := NIL)
  RAISES {TrestleComm.Failure}; <* LL.sup <= VBT.mu *>
(* Initiate the installation of "v" as a decorated top-level window of the
   window system "trsl".  *)

(* "Install" may return before the installation is complete.  "Install"
   is a checked runtime error if "v" is not detached, or if "v" is in
   the process of being installed.  The position of the window
   on the screen depends on the window manager.

   The text "applName" is the application name; it defaults to the
   application name from the process environment.

   The text "inst" distinguishes windows with the same application name. 
   For example, a text editor might use the full path name of the 
   file being edited as the instance. The default is the value of 
   the environment variable "WINSTANCE".

   Trestle does not require that the pair "(applName, inst)" be unique, but
   session management tools will work more smoothly if it is.

   The text "windowTitle" will be placed in the window's title bar when
   the window is not iconic.  It defaults to the concatenation of
   "applName", a space, and "inst", or just to "applName" if "inst"
   is "NIL".

   The icon for the window will contain the text "iconTitle" together
   with "iconWindow" (if it is not "NIL").  For example, "iconWindow"
   might be a small "BitmapVBT".  Some window managers ignore
   "iconWindow".  The default for "iconTitle" is "inst", or 
   "applName" if "inst" is "NIL".

   The triple "bgColorR", "bgColorG", "bgColorB" specify the red, green,
   and blue components of the background color for the window and icon
   titles.  If they are defaulted, the window manager's default
   background color will be used; if they are not defaulted they should
   be between "0.0" and "1.0".  Some window managers ignore the
   background color.

   An installed window's maximum, minimum, and preferred size will be
   reported to the window manager, initially and whenever they change.
   However, a "StableVBT" filter is inserted above each installed
   window, so that a new preferred size will not be reported if the
   window's current size satisfies the new max and min constraints.
   Use "StableVBT.Disable" to force a new preferred size.

   It is a checked runtime error if either "v" or "iconWindow" is 
   already installed.

   Installing a window inserts one or more filters above it, including
   a "HighlightVBT", a "StableVBT", and filters that make
   screen-independent resources work.  *)


PROCEDURE AwaitDelete(v: VBT.T); <* LL = {} *>
(* Wait until "v" is deleted or disconnected from whatever window system it
   is installed on.  *)

(* "AwaitDelete" will not return until after the "Deleted" or
   "Disconnected" code has been delivered and processed by the window.
   It is a noop if "v" is already deleted or is not installed.  *)


PROCEDURE Delete(v: VBT.T); <* LL.sup = VBT.mu *>
(* Delete "v" from wherever it is installed.  *)

(* "Delete" automatically releases any selections owned by "v" or any of
   "v"'s descendants.  Before "Delete(v)" returns, lost codes will be
   delivered for any such selections.  If "v" owned the mouse focus,
   "v" will also receive a synthesized mouse transition of type
   "LastUp".  Then "v" will receive a "Deleted" code, and finally
   "Delete" will return.  At this point "v" is disconnected and can
   be re-installed.  *)


PROCEDURE Decorate(
    v: VBT.T;
    instance: TEXT := NIL;
    windowTitle: TEXT := NIL;
    iconTitle: TEXT := NIL;
    bgColorR: REAL := -1.0;
    bgColorG: REAL := -1.0;
    bgColorB: REAL := -1.0;
    applName: TEXT := NIL;
    iconWindow: VBT.T := NIL)
  RAISES {TrestleComm.Failure}; <* LL.sup = VBT.mu *>
(* Change the decorations of "v" to the given values *)

(* Any parameter that is defaulted will not be changed, unless v has
   been "Attached" since it was last decorated, in which case the
   default value is computed as in "Install".  "Decorate" is a noop
   if "v" is not an attached window, or if "v" is installed without
   decorations.  *)

PROCEDURE GetDecoration(v: VBT.T;
  VAR instance, windowTitle, iconTitle, applName: TEXT;
  VAR bgColorR, bgColorG, bgColorB: REAL;
  VAR iconWindow: VBT.T): BOOLEAN; <* LL.sup = VBT.mu *>
(* If "v" is decorated, fetch "v"'s decorations, and return "TRUE".  
  Otherwise, return "FALSE". *)
  
 
(* \subsection{Window placement} *)


PROCEDURE Attach(v: VBT.T; trsl: T := NIL)
  RAISES {TrestleComm.Failure}; <* LL.sup = VBT.mu *>
(* Attach "v" to the window system "trsl", leaving it invisible. *)

(* "Attach" is like "Install", except (1) the locking level is
   different, (2) the attachment is completed before "Attach" returns,
   (3) the window becomes undecorated, and (4) the window remains
   invisible until you call "Overlap", "Iconize", or "MoveNear".  Before
   calling one of these, most clients will want to call "Decorate". *)

PROCEDURE Overlap(
    v: VBT.T;
    id: ScreenID;
    READONLY nw: Point.T)
  RAISES {TrestleComm.Failure}; <* LL.sup = VBT.mu *>
(* Move the northwest corner of "v" to the point "nw" on the screen "id". *)

(* If "v" is undecorated, this produces a window with no title bar or
   border, and the user will probably not be able to move, iconize or
   delete the window; this is a bad idea unless you're implementing
   pop-up or pull-down menus.  If "id" is out-of-range for this Trestle
   instance, the window will appear on the default screen. *)

PROCEDURE Iconize(v: VBT.T) 
  RAISES {TrestleComm.Failure}; <* LL.sup = VBT.mu *>
(* Make the window "v" become iconic. *)

PROCEDURE MoveNear(v, w: VBT.T) 
  RAISES {TrestleComm.Failure}; <* LL.sup = VBT.mu *>
(* Move the window "v" to be near the window "w".  *)

(* The exact effect of "MoveNear" depends on the window manager.  
   If "w" is "NIL" or is not installed where "v" is, then
   "MoveNear" will attempt to bring "v" to the attention of the user;
   in particular, if "v" is an overlapping window, "v" will be brought
   to the top; if "v" is an icon, it will be deiconized; if "v" is
   in the invisible state produced by "Attach", it will be opened in some 
   visible place.  
   
   "Overlap", "Iconize", and "MoveNear" are all no-ops if "v" is not
   installed.  The effects of "Iconize" and "MoveNear" are undefined
   for undecorated windows.  *)

PROCEDURE InstallOffscreen(
    v: VBT.T;
    width, height: CARDINAL;
    preferredScreenType: VBT.ScreenType)
  RAISES {TrestleComm.Failure}; <* LL.sup = VBT.mu *>
(* Give "v" a domain with the given dimensions in the off-screen memory
   of the window system to which it is attached.  *)

(* "InstallOffscreen" rescreens "v" to "preferredScreenType", or
   something as much like it as supported for off-screen windows.  The
   window "v" must be in the floating state produced by "Attach".  The
   usual purpose is to paint on "v" and then use "VBT.Capture" to
   retrieve the contents of its screen as a pixmap.  You should delete
   "v" when you are done with it.  Until "v" is deleted, you should
   not pass it to "Overlap", "Iconize", "MoveNear" or "InstallOffscreen". *)

 (* \subsection{Enumerating and positioning screens} *)

(* A window system may have multiple screens.  Each screen is
   identified by an integer.  *)

TYPE ScreenID = INTEGER;

CONST NoScreen: ScreenID = -1;

TYPE ScreenOfRec = RECORD 
  id: ScreenID; 
  q: Point.T; 
  trsl: T; 
  dom: Rect.T 
END;

PROCEDURE ScreenOf(
  v: VBT.T; READONLY p: Point.T)
  : ScreenOfRec; <* LL.sup < v *>
(* Return information about where "v" is installed. *)

(* If "v" is an installed window, or a child of an installed window,
    then after "res := ScreenOf(v, p)" we have 

\medskip\bulletitem "res.id" is the "ID" of the screen currently
containing "v";
   
\medskip\bulletitem "res.q" is the point in screen coordinates that
 corresponds to the point "p" in window coordinates;

\medskip\bulletitem "res.trsl" is the window system on which "v" is
installed; and
   
\medskip\bulletitem
"res.dom" is the domain of the screen "res.id".  

\medskip The point "p" need not be in the domain of "v".  If "v" is
not installed, then "res.trsl" will be "NIL", "res.id" will be
"NoScreen", and the other fields will be arbitrary.  If the window
manager is moving "v" between screens when "ScreenOf" is called, then
"res.id" will be "NoScreen" and "res.dom" and "res.q" will be arbitrary.
*)

TYPE
  Screen = RECORD
    id: ScreenID;
    dom: Rect.T;
    delta: Point.T;
    type: VBT.ScreenType
  END;
  ScreenArray = REF ARRAY OF Screen;


PROCEDURE GetScreens(trsl: T := NIL): ScreenArray 
  RAISES {TrestleComm.Failure}; <* LL.sup = VBT.mu *>
(* Return an array of descriptors of the screens of the window system
   "trsl".  *)
   
(* For each "Screen" "s" in the returned array, the rectangle "s.dom"
   is the domain of the "VBT" at the root of the screen.  The screens
   all lie in a global coordinate system, within which the user moves
   the cursor.  The point "p" in screen coordinates corresponds to the
   point "p+s.delta" in global coordinates.  (Some window systems don't
   support this; in which case "s.delta" will be set to "Point.Origin"
   for all screens.)  The value "s.type" is the screentype of the screen's
   root "VBT".  "GetScreens" returns "NIL" if the window system has
   no screens.  *)

(* \subsection{Reading pixels from a screen} *)


PROCEDURE Capture(
    id: ScreenID;
    READONLY clip: Rect.T;
    VAR (* out *) br: Region.T;
    trsl: T := NIL)
    : ScrnPixmap.T
  RAISES {TrestleComm.Failure};
  <* LL.sup = VBT.mu *>
(* Read the contents of "clip" from screen "id" of "trsl".  *)

(* "Capture(id, clip, br, trsl)" is like "VBT.Capture(r, clip, br)", 
   where "r" is the "VBT" at the root of screen "id" of the window 
   system "trsl".  *)

(* \subsection{Checking on recent input activity} *)


PROCEDURE AllCeded(trsl: T := NIL): BOOLEAN 
    RAISES {TrestleComm.Failure}; <* LL.sup = VBT.mu *>
(* Return whether there is pending input from "trsl". *) 

(* If a program calls "AllCeded(t)" and "TRUE" is returned, then there
   are no mouse clicks or keystrokes on their way to any top-level
   windows installed by the program on "t".  For example, when the VT100
   terminal emulator has observed a key-down and waited for half a
   second and observed no key-up and concludes that it should go into
   auto-repeat mode, it verifies that "AllCeded" returns "TRUE" to make
   sure that the up transition is not on its way, to avoid erroneously
   entering auto-repeat mode.  *)


PROCEDURE TickTime(trsl: T := NIL): INTEGER;
<* LL.sup <= VBT.mu *>
(* Return the number of microseconds per "VBT.TimeStamp", in events
   reported to "VBTs" connected to the window system "trsl".  *)

(* \subsection{Connecting to a window system} *)


PROCEDURE Connect(inst: TEXT := NIL): T
  RAISES {TrestleComm.Failure}; <* LL.sup <= VBT.mu *>
(* Connect to the window system named "inst".  *)

(* In general, the format and interpretation of "inst" are
   implementation-dependent.  Here are the rules when using an 
   X server:
   
   If "inst" is "NIL", it defaults to the value of the environment
   variable "DISPLAY", unless this variable is undefined, in which
   case it defaults to ":0".
   
   The syntax of "inst" should be: 

| <machine name>(":" | "::")<number>("" | "." <number>)

   where "<machine name>" is an arbitrary string of characters (possibly
   empty) and "<number>" is a non-negative decimal integer.  It denotes
   an X server according to the rules on page 27 of the second edition
   of {\it X Window System}, by Scheifler et.  al., Digital Press, 1990
   \cite{XSpec}.

   For example, "nemesia:0" denotes the first window system on the
   machine "nemesia", and ":0" denotes the first window system on
   the machine calling "Connect".

   The exception is raised if the designated window system doesn't
   exist or cannot be connected to. *)
   
END Trestle.


