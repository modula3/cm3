(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Batch.def, by msm & cgn, Wed May  6 16:57:44 1987 *)
(* Last modified on Tue Mar 10 18:58:41 1992 by steveg  *)
(*      modified on Mon Feb 24 13:56:33 PST 1992 by muller  *)
(*      modified on Wed Aug 28 17:51:59 PDT 1991 by gnelson *)

(*      modified on Fri Mar  3 20:53:46 PST 1989 by msm *)
<*PRAGMA LL*>

(* A "Batch.T" is a data structure containing a sequence of
   "VBT" painting commands.  Batches are untraced: they must be
   explicitly allocated and freed using the procedures in
   this interface.  *)

INTERFACE Batch;

TYPE T <: ADDRESS;

PROCEDURE New(len: INTEGER := -1): T;
(* Allocate a batch containing at least "len" "Word.Ts". *)
 
(* If "len = -1", the number of "Word.T"s in the result will be
   "VBTTuning.BatchSize".  Initially the clip and scroll source are
   "Rect.Empty".  *)

PROCEDURE Free(VAR ba: T);
(* Return "ba" to the free list and set "ba := NIL".  *)

(* "Free(ba)" is a checked runtime error if "ba" is "NIL". *)

END Batch.
