(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Feb 24 13:58:22 PST 1992 by muller                   *)

INTERFACE TrestleAux;

IMPORT Trestle, VBT, TrestleComm, Rect, Point;

FROM Trestle IMPORT ScreenID, Unimplemented, 

EXCEPTION Unimplemented;

PROCEDURE SetColorMap(v: VBT.T; cm: ScrnColorMap.T);
<* LL.sup = VBT.mu *>
(* Tell the window manager that "cm" is the preferred color map for 
   the installed window "v".  This is a no-op if "cm" is inappropriate 
   for "v"'s screentype. *)

PROCEDURE SetScreens(
    sa: ScreenArray;
    trsl: T := NIL)
    : BOOLEAN
  RAISES {TrestleComm.Failure, Unimplemented};
(* Reposition each screen sa^[i].id of the Trestle instance trsl so that its
   delta to the global coordinate system is sa^[i].delta, and return TRUE.
   Return FALSE if this could not be done; for example, if sa refers to
   non-existent screens, or if the request places screens so that they overlap
   or fail to abut. LL arbitrary. *)

(* Controlling screens *)

(* The following two procedures allow clients to bypass Trestle and use the
   screens directly. For example, they can be used by the screensaver process
   to blank the screens if the workstation has been inactive for a long
   period. At the root of each trestle screen there is actually a a stack of
   VBTs, called the screen stack. The top VBT is connected to the screen. All
   the rest are unconnected to the screen and have empty domains. Initially,
   each stack contains the VBT that Trestle carves up into windows. *)

PROCEDURE TakeOver(
    id: ScreenID;
    v: VBT.T;
    trsl: T := NIL)
  RAISES {TrestleComm.Failure, Unimplemented};
(* Push v onto the stack for screen id and connect it to the screen in place
   of the previous top element of the screen stack, which is reformatted to be
   empty. v must not already be installed. v has a highlighter inserted above
   it, so you can set highlights on it if you like. LL <= VBT.mu *)

PROCEDURE Restore(
    id: ScreenID;
    v: VBT.T)
  RAISES {TrestleComm.Failure, Unimplemented};
(* Schedules v to be popped off the screen stack for screen id, connecting the
   new top element to the screen, and reformatting v to be empty. v is
   nullified, and may be reused. v will receive a "Deleted" code, which will
   be the last event delivered to v, just as in Delete. LL <= VBT.mu *)

PROCEDURE TakeOverMouse(
    id: ScreenID;
    v: VBT.T;
    trsl: T := NIL)
  RAISES {TrestleComm.Failure};
(* Make v receive all mouse events. The window v supercedes any current
   TakeOverMouse window. (See MouseCage.def.) LL <= VBT.mu *)

PROCEDURE ReleaseMouse(id: ScreenID; v: VBT.T) RAISES {TrestleComm.Failure};
(* Schedules v to stop receiving all mouse events. If there is a previous
   window that has called TakeOverMouse and has not called ReleaseMouse, then
   that window will now receive all mouse events. (See MouseCage.def.) LL <=
   VBT.mu *)

PROCEDURE SetHighlight(
    id: ScreenID;
    READONLY r: Rect.T;
    border: CARDINAL;
    trsl: T := NIL)
  RAISES {TrestleComm.Failure};
(* Like HighlightVBT.SetRect, but it highlights on the screen id of the
   trestle t. If r is empty, the highlighting is removed. The exact rule for
   highlighting the region depends on the window manager. LL <= VBT.mu *)

PROCEDURE AddParent(
    prnt: VBT.T;
    id: ScreenID;
    trsl: T := NIL)
  RAISES {TrestleComm.Failure, Unimplemented};
(* This is like JoinVBT.AddParent except that it operates on screen id of
   trestle t. It is a noop if that screen doesn't exist. LL <= VBT.mu *)

PROCEDURE RemParent(
    prnt: VBT.T;
    id: ScreenID;
    trsl: T := NIL)
  RAISES {TrestleComm.Failure, Unimplemented};
(* This is just like JoinVBT.RemParent except that it operates on screen id of
   trestle t. It is a noop if that screen doesn't exist or prnt is not a
   parent. LL <= VBT.mu *)

PROCEDURE WarpCursor(
    id: ScreenID;
    READONLY pt: Point.T;
    trsl: T := NIL)
  RAISES {TrestleComm.Failure};
(* Moves the cursor of the trestle trsl to point pt of screen id. If the screen
   doesn't exist, this is a noop; if pt is outside the screen, the cursor is
   moved to the point on the screen nearest pt. LL <= VBT.mu *)

TYPE
  Config <: REFANY;

(* A Trestle.Config represents an arrangement of the top-level windows of a
   window system. Essentially it is a list of name-position pairs for windows
   on the screen. The exact definition of a position depends on the window
   manager. *)

EXCEPTION
  BadConfig (TEXT);

TYPE
  ConfigClosure = OBJECT
    METHODS
      apply(id: ScreenID; c: Config)
    END;
  (* called with LL = VBT.mu *)

PROCEDURE GetConfig(
    id: ScreenID;
    p: ConfigClosure;
    trsl: T := NIL)
  RAISES {TrestleComm.Failure, Unimplemented};
(* Fetch the configuration c of the given screen of the Trestle instance t,
   and arrange that p.apply(id, c) will soon be called. LL = VBT.mu *)

PROCEDURE SetConfig(
    id: ScreenID;
    c: Config;
    trsl: T := NIL)
  RAISES {TrestleComm.Failure, Unimplemented, BadConfig};
(* Move windows of the Trestle instance trsl around to make the configuration of
   screen id be c. Windows on screen id that are not in the configuration c
   will be iconized or moved in some window-manager-dependent way. Similarly,
   windows included in c that do not exist will (of necessity) be omitted from
   the configuration; a tiling window manager will adjust for this in a
   window-manager-dependent way. If the config includes two windows with the
   same name, or if two windows exist with the same name, the naming ambiguity
   will be resolved non-deterministically. If c is invalid, this operation is
   either a noop or raises BadConfig. LL = VBT.mu *)

PROCEDURE Swap(v, w: VBT.T) RAISES {TrestleComm.Failure, Unimplemented};
<* LL arbitrary *>
(* Exchange the positions of the two windows "v" and "w". This is a noop 
   unless "v" and "w" are both installed on the same window system. *)

PROCEDURE SwapByName(
    v: VBT.T;
    nm: TEXT)
  RAISES {TrestleComm.Failure, Unimplemented}; <* LL arbitrary *>
(* Exchange the positions of the window "v" and the window with full name "nm".
   This is noop unless "v" is installed and the trestle on which it is installed
   has a window named "nm". *)

PROCEDURE GetName(v: VBT.T): TEXT RAISES {TrestleComm.Failure};
<* LL.sup = VBT.mu *>
(* Returns the full name of the installed window v, which is the
   concatenation of "v"'s "applName", a space, "v"'s "inst", a space,
   and a decimal numeral that guarantees uniqueness of names.  (The
   procedures "Install" and "Redecorate" control "v"'s "applName" and
   "inst" attributes.)  The result of "GetName" on an undecorated window
   is an arbitrary text.  *)

PROCEDURE NameList(
    nm: TEXT;
    trsl: T := NIL)
    : REF ARRAY OF TEXT
  RAISES {TrestleComm.Failure}; <* LL.sup = VBT.mu *>
(* Return the list of full names of windows installed on the trestle "trsl" which
   have "nm" as a prefix. The returned array may be padded with "NILs". *)

PROCEDURE MoveNearByName(v: VBT.T; nm: TEXT) RAISES {TrestleComm.Failure};
<* LL.sup = VBT.mu *>
(* Move the"v" to be near the window with full name "nm", as in
   "MoveNear".  If there is no window named "nm", this is like
   "MoveNear(v, NIL)".  *)

PROCEDURE DeleteByName(nm: TEXT; trsl: T := NIL) RAISES {TrestleComm.Failure};
<* LL.sup = VBT.mu *>
(* Schedule the window named "nm" for deletion from the Trestle "t". This has no
   effect if the window is non-existent. *)

PROCEDURE LastCeded(
    trsl: T := NIL)
    : VBT.TimeStamp
  RAISES {TrestleComm.Failure, Unimplemented};
(* Return the time of the last event that has been ceded. This is primarily
   for the benefit of the ScreenSaver process that wakes up every fifteen
   minutes or so and blanks the screen if there has been no user input, in
   order to avoid burning patterns in the phosphor. LL <= VBT.mu *)

(* Getting and setting workstation parameters *)

TYPE
  Parameters =
    REF RECORD
      peekABooCursor: BOOLEAN;
      blackOnWhite: BOOLEAN;
      fastTimeout, slowTimeout: CARDINAL;
      flashNever, flashOnce: BOOLEAN;
      doubleClickInterval: CARDINAL;
      safetyRadius: CARDINAL;
      mouseThreshhold, mouseMultiplier: CARDINAL;
      autorepeat: BOOLEAN;
    END;

(* Associated with every window system is a "Parameters" record 
   whose fields are defined as follows:

   The flag "peekABooCursor" is "TRUE" if the window system hides the
   cursor when a character is typed and restores it when the mouse
   moves; otherwise the flag is "FALSE".

   The value "slowTimeout" is the number of milliseconds the window
   system will wait for a client to respond to an event, assuming the
   client has responded promptly to its last event; "fastTimeout" is
   the number of milliseconds of the window system will wait for a
   client to respond to an event, assuming it has not responded promptly
   to its last event.

   If "flashNever" is set, then when a window does not respond to 
   an event promptly, Trestle stops waiting for it without any
   indication to the user that this has happened.  Otherwise,
   if  "flashOnce" is set, Trestle flickers the window when it
   exceeds its "slowTimeout".  Otherwise, Trestle flickers the
   window whenever it exceeds either the "fastTimeOut" or the 
   "slowTimeout".

   The value "doubleClickInterval" is the maximum number of "VBT.TimeStamp" 
   intervals allowed between two mouse transitions that can be 
   counted as part of a multiple click. 

   The value "safetyRadius" is the maximum motion allowed between two mouse
   transitions that can be counted as part of a mulitiple click.  The
   two positions must be within "safetyRadious" pixels of one another
   in both the "h" and "v" axes.

    The value "mouseThreshhold" and "mouseMultiplier" control the
    relation between mouse motion and cursor motion.  If the mouse moves
    more than "mouseThreshhold" pixels in a single sample interval,
    the excess is scaled by "mouseMultiplier".  This applies separately
    in each coordinate.

    The value "autorepeat" is "TRUE" if the window system uses 
    auto-repeat mode for (some) keys that are held down long enough.  *)
         
PROCEDURE GetParameters(trsl: T := NIL): Parameters RAISES {TrestleComm.Failure};
<* LL.sup <= VBT.mu *>
(* Retrieve the current settings of all parameters. *)

PROCEDURE SetParameters(
    p: Parameters;
    trsl: T := NIL)
  RAISES {TrestleComm.Failure};
  <* LL.sup <= VBT.mu *>
(* Change the parameter record for "trsl" to "p".  If the window system
   "trsl" does not support some of the parameter settings in "p", then
   these parameters will be unchanged; follow with a call to
   "GetParameters" if this matters.  *)

END TrestleAux.
