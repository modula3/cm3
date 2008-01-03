(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Jun 14 20:56:38 PDT 1993 by meehan *)
(*      modified on Thu Feb  4 17:23:31 PST 1993 by mhb    *)
(*      modified on Tue Jun 16 13:09:00 PDT 1992 by muller *)
(*      modified on Thu Apr  2 21:31:51     1992 by steveg *)
<* PRAGMA LL *>
<* PRAGMA SUBTYPE *>

(* A "ChoiceVBT" multi-filter behaves in concert with other
   "ChoiceVBT"s to implement {\em radio buttons}. Abstractly, a
   "ChoiceVBT" "v" consists of

| state(v)       TRUE `or` FALSE
| group(v)       `a set of "ChoiceVBT"s (the {\em radio group})`

  A group "g" consist of

| selection(g)   `the one member of "g" whose state is "TRUE",`
|                `or "NIL" if there is no such member.`

   "state(v)" is defined as "v = selection (group (v))".

   Structurally, a "ChoiceVBT" is identical to a "BooleanVBT": it is a
   multi-filter that maintains a Boolean state for its VBT-child.  All
   events are forwarded to the VBT-child.

   When the "action" procedure of the button would normally be
   invoked, the value of the state of the "ChoiceVBT" is toggled and
   the "callback" method on the "ChoiceVBT" is invoked.

   The multi-child of a "ChoiceVBT" is defined to be the multi-child
   of the "ButtonVBT". *)

INTERFACE ChoiceVBT;

IMPORT BooleanVBT, ButtonVBT;

TYPE
  <* SUBTYPE T <: MultiFilter.T *>
  T <: Public;
  Public = BooleanVBT.T OBJECT
           METHODS
             <* LL <= VBT.mu *>
             init (button: ButtonVBT.T; group: Group): T;
           END;

(* The call "v.init(...)" initializes "v" as a "ChoiceVBT" with
   an initial state of "FALSE". It is added to the radio group
   "group". *)

TYPE Group <: ROOT;

(* A "Group" is a set of "ChoiceVBT"s.

   A "ChoiceVBT" "v" is added to a group when "v" is initialized.
   When "v" is discarded, it is removed from its group. *)

PROCEDURE Get (v: T): T;
<* LL.sup = VBT.mu *>
(* Return "selection(group(v))" *)

PROCEDURE Put (v: T);
<* LL.sup = VBT.mu *>
(* Equivalent to "selection(group(v)) := v" *)
   
PROCEDURE Clear (v: T);
<* LL.sup = VBT.mu *>
(* Equivalent to "selection(group(v)) := NIL" *)

PROCEDURE Selection (group: Group): T;
<* LL.sup = VBT.mu *>
(* Return "selection(group)" *)

END ChoiceVBT.



