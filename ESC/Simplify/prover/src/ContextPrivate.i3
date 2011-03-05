(* Copyright (C) 2002 Hewlett-Packard Company *)
(* Copyright (C) 2000, 2002 Compaq Computer Corporation *)
(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri May 19 23:58:45 PDT 2000 by saxe                     *)
(*      modified on Thu May 30 16:26:10 PDT 1996 by detlefs                  *)

INTERFACE ContextPrivate;

IMPORT AF, Clause, MatchingRule;

PROCEDURE Propagate(lit: AF.Lit);
(* Propagates the constraint described by "lit". *)

<*UNUSED*>
PROCEDURE PropagateProxy(lit: AF.Lit; mr: MatchingRule.T := NIL);
(* Like "propagate", except if "lit.af" is a proxy; expands its true
   non-proxy CNF if that be shown to be sufficiently small, and adds
   the resulting clauses to the current clause list.  If "mr" is
   non-NIL, the resulting clauses should be marked as products of that
   matching rule. *)

PROCEDURE AddClause(c: Clause.T);
(* Adds the clause "c" to the "clauses". *)

PROCEDURE GetNSplits(): INTEGER;

VAR inD1P := FALSE;
(* True iff a depth-1 plunge is in progress. *)

END ContextPrivate.
