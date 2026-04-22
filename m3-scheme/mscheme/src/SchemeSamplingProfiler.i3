(* Copyright (c) 2026, Mika Nystrom.  All rights reserved. *)

INTERFACE SchemeSamplingProfiler;
IMPORT TextIntTbl, TextRefTbl;

(* Sampling profiler for MScheme.

   The eval loop writes currentProcName and currentCallerName on each
   procedure call.  A background thread periodically samples them and
   tallies counts.

   Hot-path cost: one boolean test + two pointer writes per call. *)

VAR
  enabled : BOOLEAN := FALSE;
  currentProcName : TEXT := NIL;
  currentCallerName : TEXT := NIL;

PROCEDURE Start(intervalMS : CARDINAL := 1);
  (* Start sampling at the given interval (default 1ms).
     If already running, does nothing. *)

PROCEDURE Stop();
  (* Stop sampling.  Blocks until the sampler thread exits. *)

PROCEDURE Reset();
  (* Clear all accumulated samples. *)

PROCEDURE Results() : TextIntTbl.T;
  (* Return the current flat sample table (name -> count).
     The table is shared; caller should not mutate it. *)

PROCEDURE CallGraph() : TextRefTbl.T;
  (* Return the call graph table.  Each key is a callee name,
     mapping to a TextIntTbl.T of (caller -> count).
     The tables are shared; caller should not mutate them. *)

PROCEDURE Total() : INTEGER;
  (* Return total number of samples collected. *)

CONST Brand = "SchemeSamplingProfiler";

END SchemeSamplingProfiler.
