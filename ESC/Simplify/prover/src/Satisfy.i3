(* Copyright (C) 2002 Hewlett-Packard Company *)
(* Copyright (C) 2000, 2002 Compaq Computer Corporation *)
(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Nov 29 11:48:08 PST 2001 by saxe                     *)
(*      modified on Mon Sep 18 11:49:33 PDT 1995 by detlefs                  *)

(* Attempts to determine if the global "Context" is satisfiable. *)

INTERFACE Satisfy;

IMPORT Prover, AF, Trit;

PROCEDURE Search(
            lit: AF.Lit;
            cl: Prover.ResClosure;
            ccLimit: INTEGER;
            pruning := FALSE): Trit.T
    RAISES { Prover.Error };

(* Executes a resource-limited search for up to "ccLimit" satisfying
   contexts for "lit".  Returns "Trit.False" if any satisfying contexts
   are found; returns "Trit.Unknown" if no satisfying contexts are found
   but search is pruned due to resource limits; otherwise returns
   "Trit.True". 

   If "pruning" is "TRUE", the thoroughness of the search
   is drastically curtailed (as is appropriate, for example, in pruning
   a counterexample context found by an earlier call to "Search").

   If "cl" is non-NIL, calls "cl.apply(p)" for up to "ccLimit"
   "Prover.ProveResult"s "p" such that "p.kind = Counterexample".  The
   different satisfying contexts returned will differ at least by
   selecting different literals from some `rightmost' clause in the
   expansion of "lit".  May also call "cl.apply(p)" for additional
   values of "p" of other "kind"s.

   In no case is the current context modified.*)

PROCEDURE GetIters(): INTEGER;
  (* Returns the number of iterations of the main loop started so far in
     current invocation of "Search".  Used for tracing. *)

END Satisfy.
