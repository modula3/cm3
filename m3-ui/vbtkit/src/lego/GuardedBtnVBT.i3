(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Jun 11 10:51:25 PDT 1993 by meehan *)
(*      modified on Mon Feb  1 21:29:51 PST 1993 by mhb    *)
(*      modified on Tue Jun 16 13:08:49 PDT 1992 by muller *)
<* PRAGMA LL *>
<* PRAGMA SUBTYPE *>

(* A "GuardedBtnVBT" protects its child against unintentional mouse
   clicks.  While the guard is displayed, mouse clicks are not
   forwarded.  To remove the guard, click on the button.  The guard is
   restored after the next upclick, chord-cancel, or when the mouse
   leaves the domain of the "VBT".

   Typically, a "GuardedBtnVBT" is placed above a ``dangerous''
   button, like one that terminates an application.  This forces the
   user to click twice to terminate the application---the first time
   to remove the guard, and the second time to invoke the button that
   terminates the application.

   A "GuardedBtnVBT" is much closer to being a VBTkit switch than a
   Trestle button.  There's a "callback" method (invoked when the
   guard is removed), and the guard is a multi-filter.  However, the
   client does not provide a feedback; it is hard-wired into the
   "GuardedBtnVBT" implementation. *)

INTERFACE GuardedBtnVBT;

IMPORT ButtonVBT, PaintOp, VBT;

TYPE
  T <: Public;
  <* SUBTYPE T <: MultiFilter.T *>
  Public =
    ButtonVBT.T OBJECT
    METHODS
      <* LL <= VBT.mu *>
      init (ch: VBT.T; colors: PaintOp.ColorScheme := NIL): T;
      <* LL = VBT.mu *>
      callback (READONLY cd: VBT.MouseRec);
    END;

END GuardedBtnVBT.



