(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* Last modified on Fri Jun 11 14:42:58 PDT 1993 by meehan *)
(*      modified on Fri Feb  5 17:39:05 PST 1993 by mhb    *)
(*      modified on Tue Jun 16 13:09:03 PDT 1992 by muller *)
(* modified on Fri Mar 27 01:48:28 1992 by steveg *)
<* PRAGMA LL *>
<* PRAGMA SUBTYPE *>

(* A "BooleanVBT" is a multi-filter that maintains a Boolean state for
   its VBT-child.

   When the "action" procedure of the button would normally be
   invoked, the value of the state of the "BooleanVBT" is toggled and
   the "callback" method on the "BooleanVBT" is invoked.

   The multi-child of a "BooleanVBT" is defined to be the multi-child
   of the "ButtonVBT". *)

INTERFACE BooleanVBT;

IMPORT ButtonVBT, HighlightVBT, VBT;

TYPE
  <* SUBTYPE T <: MultiFilter.T *>
  T <: Public;
  Public = HighlightVBT.T OBJECT
           METHODS
             <* LL <= VBT.mu *>
             init (button: ButtonVBT.T): T;
             <* LL = VBT.mu *>
             callback (READONLY cd: VBT.MouseRec);
           END;

(* The call "v.init(...)" initializes "v" as a "BooleanVBT" with an
   initial state of "FALSE". The default "callback" method is a no-op.

   Warning: This call modifies the "action" field of "button". *)

PROCEDURE Put (v: T; state: BOOLEAN);
<* LL.sup = VBT.mu *>
(* Set "v"'s state. *)

PROCEDURE Get (v: T): BOOLEAN;
<* LL.sup = VBT.mu *>
(* Returns "v"'s current state. *)

END BooleanVBT.

