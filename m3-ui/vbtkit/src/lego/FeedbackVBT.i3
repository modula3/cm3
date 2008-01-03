(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Jun 10 13:58:16 PDT 1993 by meehan *)
(*      modified on Mon Feb  1 15:01:50 PST 1993 by mhb    *)
(*      modified on Tue Jun 16 13:08:55 PDT 1992 by muller *)
<* PRAGMA LL *>

(* A "FeedbackVBT" is a filter that provides some visual feedback for
   its child.

   The essence of a "FeedbackVBT" are its "normal" and "excited"
   methods.  The "normal" method is intended for giving permanent
   feedback, whereas the "excited" method is used for displaying
   transitory feedback (e.g., while a button is pressed).  In
   addition, a feedback maintains a {\it state} flag to
   distinguish between an ``on'' and ``off'' state (e.g., for use
   by a "BooleanVBT").

   Clients should not invoke a "FeedbackVBT"'s "normal" and
   "excited" methods directly.  Instead, use the procedures
   "Normal" and "Excited" in this interface.  The state of a
   "FeedbackVBT" is set using the "SetState" procedure; it is
   queried using the procedure "GetState".

   The default "normal" and "excited" methods are no-ops.  A
   "FeedbackVBT" by itself is not very useful; subtypes are expected
   to override these methods with something useful.  Also, VBTkit
   switches that use "FeedbackVBT"s assume that the "FeedbackVBT" is a
   multi-filter, not simply a filter. *)

INTERFACE FeedbackVBT;

IMPORT Filter, VBT;

TYPE
  T <: Public;
  Public = Filter.T OBJECT
           METHODS
             <* LL <= VBT.mu *>
             init (ch: VBT.T): T;
             <* LL = VBT.mu *>
             normal  ();
             excited ();
           END;

(* The call "v.init(ch)" initializes "v" as a "FeedbackVBT"
   with VBT child "ch".  The
   default "normal" and "excited" methods are no-ops. *)

PROCEDURE Normal (v: T);
<* LL.sup = VBT.mu *>
(* Invoke v's "normal" method. *)

PROCEDURE Excited (v: T);
<* LL.sup = VBT.mu *>
(* Invoke v's "excited" method. *)

PROCEDURE SetState (v: T; state: BOOLEAN);
<* LL.sup = VBT.mu *>
(* Record the "state" and then invoke whichever of "v"'s methods,
   "normal" or "excited", was most recently invoked. If neither method
   has ever been invoked, the "normal" method is invoked. *)

PROCEDURE GetState (v: T): BOOLEAN;
<* LL.sup = VBT.mu *>
(* Return the value of the most recent call to "SetState". 
   The initial state is "FALSE". *)

END FeedbackVBT.
