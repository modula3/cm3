(* Copyright (C) 1993, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* Last modified on Thu Jul 29 14:43:44 PDT 1993 by bharat *)
(* modified on Fri Jul 2 16:33:31 PDT 1993 by mhb *)
<* PRAGMA LL *>

(* A "KnobsVBT" displays little ``knobs'' on its child. *)

INTERFACE KnobsVBT;

IMPORT Filter, Point, VBT;

TYPE
  T <: Public;
  Public = Filter.T OBJECT
             (* drawColor: Pixmap.T;*)
           METHODS
             <* LL <= VBT.mu *>
             init          (ch: VBT.T): T;
           END;
(* The call "v.init(ch)" initialize "v" as a "KnobsVBT".  Initially, "ch"
   is displayed unaltered. *)

PROCEDURE Add (v: T);
<* LL = VBT.mu *>
(* Draw a set of ``knobs'' using "PaintOp.Fg".  This procedure is a no-op
   if the knobs are already displayed. *)

PROCEDURE Remove (v: T);
<* LL = VBT.mu *>
(* Remove the set of ``knobs.'' This procedure is a no-op is the knobs are
   not being displayed. *)

PROCEDURE Inside (v: T; READONLY pt: Point.T): BOOLEAN;
<* LL = VBT.mu *>
(* Returns whether the point "pt" falls within any of the ``knobs.'' *)

PROCEDURE SetSingleMode (v: T; READONLY tv:BOOLEAN);
<* LL = VBT.mu *>
(* Decides how an active set of knobs will be displayed, gray or black  *)
(* black if single, gray if multiple *)

END KnobsVBT.









