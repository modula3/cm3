(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Apr  1 16:56:25 PST 1994 by heydon                   *)

INTERFACE DblBufferUtil;

IMPORT Batch;

PROCEDURE Tighten(ba: Batch.T);
(* This has the same specification as "BatchUtil.Tighten", but works harder to
   make "GetClip(ba)" small. *)

END DblBufferUtil.
