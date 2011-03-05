(* Copyright (C) 2002 Hewlett-Packard Company *)
(* Copyright (C) 2000, 2002 Compaq Computer Corporation *)
(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Jun 13 12:10:29 PDT 1995 by detlefs                  *)

INTERFACE PromoteSet;

(* A "PromotedSet.T" represents a bounded set of pairs of "Clause"
   fingerprints and scores. *)

IMPORT Clause;

TYPE
  Public = OBJECT
   METHODS
    init(sz: CARDINAL): T;
    insert(cl: Clause.T; score: REAL);
    member(cl: Clause.T; VAR (*OUT*) score: REAL): BOOLEAN;
    updateScore(cl: Clause.T; score: REAL);
    normalize(min: REAL);
    size(): INTEGER;
  END (* OBJECT *);
  T <: Public;

(* The call "ps.init(n)" initializes "ps" to be empty and have bound "n".

   The call "ps.insert(cl, score)" adds the fingerprint of "cl" and
   the score "score" to "ps", removing the oldest fingerprint in "ps"
   if necessary to limit the number of entries in "ps" to the bound of
   "ps".  If the fingerprint of "cl" is already in "ps", just updates
   its score to "score".

   The call "ps.member(cl, score)" returns "TRUE" iff the fingerprint
   of "cl" is in "ps"; if it returns true, "score" is set to the score
   associated with the fingerprint of "cl".

   If the fingerprint of "cl" is in "ps", the call "ps.updateScore(cl,
   score)" sets the score associated with the fingerprint of "cl" to
   "score".

   The call "ps.normalize(min)" normalizes the scores of all
   fingerprints in "ps" to numbers between "min" and "min+1",
   respecting the order of the original scores.

   The call "ps.size()" returns the number of pairs in "ps".
*)

END PromoteSet.
