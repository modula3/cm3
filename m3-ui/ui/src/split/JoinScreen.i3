(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Sun Jun  5 07:26:52 PDT 1994 by msm     *)

<* PRAGMA LL *>

INTERFACE JoinScreen;

IMPORT VBT, ScreenType, Batch;

TYPE
  Public = VBT.ScreenType OBJECT
           METHODS
             addScreen    (st: ScreenType.T): BOOLEAN;
             removeScreen (st: ScreenType.T): BOOLEAN;
             succ         (st: ScreenType.T; VAR hint: INTEGER): ScreenType.T;
             eval         ();
             init         (): T;
           END;
  T <: Public;

(* If addScreen or removeScreen returns TRUE, you must schedule a call to
   st.eval(), and then schedule a call to rescreen on all VBTs which have
   st as their screentype. *)

PROCEDURE New(): T;
(* Create a screentype suitable for a JoinedVBT. *)

PROCEDURE PaintBatch(v, ch: VBT.T; b: Batch.T);
(* A suitable paintbatch method if ch has as its screentype a T, and
   v has as its screentype a member of that T.  This procedure will
   consume b, so you should have already copied the batch if ch has
   multiple parents. *)

PROCEDURE SetCursor(v: VBT.Split; ch: VBT.T);

END JoinScreen.
