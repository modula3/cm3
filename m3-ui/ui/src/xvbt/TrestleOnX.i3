(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* by Steve Glassman, Mark Manasse and Greg Nelson *)
(* Last modified on Mon Feb 27 18:13:11 PST 1995 by msm *)
(* modified on Mon Feb 24 13:59:41 PST 1992 by muller *)
(* modified on Wed Nov 20 16:41:02 PST 1991 by gnelson *)
<*PRAGMA LL*>

UNSAFE INTERFACE TrestleOnX;

IMPORT Trestle, X, TrestleComm, VBT;

TYPE Display <: Trestle.T;

PROCEDURE Dpy (t: Display): X.DisplayStar;
(* You must use Enter and Exit (see below) around any calls which
   manipulate the dpy.  If Enter raises an exception, you must not call any
   X procedures using that dpy.  Any X procedure can raise the
   TrestleComm.Failure exception; once that happens, you must not call any
   X procedures. *)

PROCEDURE Drawable (v: VBT.T): X.Drawable; <* LL.sup = VBT.mu *>
(* Return the window id associated with a VBT (or X.None) *)

PROCEDURE Cage (v: VBT.T): X.Window; <* LL.sup = VBT.mu *>
(* Return the window id of the cage window for a VBT (or X.None) *)

<*INLINE*> PROCEDURE Enter (t: Display) RAISES {TrestleComm.Failure};
(* Lock t.  The exception is raised when the X connection is closed.*)

<*INLINE*> PROCEDURE Exit (t: Display; deltaCoverage: [-1 .. 1] := 0)
  RAISES {TrestleComm.Failure};
(* Release the lock on t, after possibly flushing the queue of pending
   operations.

   Each t: T contains a cardinal t.coverage, which is incremented by
   deltaCoverage as the Exit happens.  If the result is zero, the x
   connection is flushed.  A thread making a sequence of calls to
   Enter/Exit can minimize unnecessary flushing by setting deltaCoverage to
   +1 on the first exit, to -1 on the last exit, and to 0 on the other
   exits.

   LL = t. *)

TYPE EventProc = PROCEDURE (t: Display; READONLY ev: X.XEvent);

PROCEDURE EventHook (t: Display; p: EventProc): EventProc; <* LL.sup = t *>
(* This procedure will be called with all events received on the connection
   t which Trestle doesn't want to handle.  This means all events received
   on windows not created by Trestle, or root windows, and certain events
   received on Trestle windows.  The old EventHook is returned; if non-NIL,
   you should pass on any events that you can't handle. *)

VAR Visibility: VBT.MiscCodeType;

CONST
  VisibilityUnobscured        = 0;
  VisibilityPartiallyObscured = 1;
  VisibilityFullyObscured     = 2;

(* When a VBT installed on X changes X visibility state, it receives a
   miscellaneous code with codetype Visibility.  The first element of
   the detail field of the misc code reflects whether the window has
   become fully obscured, partially visible, or fully unobscured.
   Note that X is idiosyncratic in that no obscured message occurs
   when a window is iconified; you'll have to rely on noting that the
   VBT domain has just become empty, instead. *)

END TrestleOnX.
