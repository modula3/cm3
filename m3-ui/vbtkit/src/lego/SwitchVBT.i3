(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Jun 15 15:51:00 PDT 1993 by meehan                   *)
(*      modified on Fri Jan 29 23:05:20 PST 1993 by mhb                      *)
(*      modified on Tue Jun 16 13:08:12 PDT 1992 by muller                   *)
<* PRAGMA LL *>
<* PRAGMA SUBTYPE *>

(* A "SwitchVBT" is a switch version of Trestle's "ButtonVBT". *)

INTERFACE SwitchVBT;

IMPORT ButtonVBT, FeedbackVBT, MultiClass, VBT;

TYPE
  <* SUBTYPE T <: MultiFilter.T *>
  T <: Public;
  Public = ButtonVBT.T OBJECT
           METHODS
             <* LL.sup <= VBT.mu *>
             init (f: FeedbackVBT.T): T;
             <* LL.sup = VBT.mu *>
             callback (READONLY cd: VBT.MouseRec);
           END;

(* The call "v.init(f)" initializes "v" as a "SwitchVBT" with child
   "f". The multi-child of "f" is marked as "v"'s multi-child too.

   The default "callback" method is a no-op. *)

(* The following type is useful for creating switches that have the
   same internal structure as a "SwitchVBT.T"; namely, a "Filter.T"
   whose child is a "FeedbackVBT.T". *)

TYPE
  MC <: MultiClass.Filter;

(* The following procedures are useful for some VBTkit switches
    to use as their default "ButtonVBT" methods: *)

PROCEDURE Pre (v: ButtonVBT.T);
<* LL.sup = VBT.mu *>
(* Equivalent to: "Feedback.Excited (Filter.Child(v))" *)

PROCEDURE Post (v: ButtonVBT.T);
<* LL.sup = VBT.mu *>
(* Equivalent to: "Feedback.Normal (Filter.Child(v))" *)

PROCEDURE Cancel (v: ButtonVBT.T);
<* LL.sup = VBT.mu *>
(* Equivalent to: "Feedback.Normal (Filter.Child(v))" *)

END SwitchVBT.



