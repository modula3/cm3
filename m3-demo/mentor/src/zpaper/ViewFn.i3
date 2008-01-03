(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* Last modified on Mon Sep 14 22:16:25 PDT 1992 by mhb            *)

INTERFACE ViewFn;

IMPORT ZPaperViewClass, ChipsVBT;

TYPE
  T <: ZPaperViewClass.T;

PROCEDURE SetChipsVBT (view: T; chips: ChipsVBT.T);
(* Call before startrun method *)

END ViewFn.





