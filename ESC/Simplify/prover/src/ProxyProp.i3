(* Copyright (C) 2002 Hewlett-Packard Company *)
(* Copyright (C) 2000, 2002 Compaq Computer Corporation *)
(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Jun 15 10:45:26 PDT 1999 by saxe                     *)
(*      modified on Fri May 31 15:06:17 PDT 1996 by detlefs                  *)

INTERFACE ProxyProp;

(* A "ProxyProp.T" is a propositional variable introduced to represent a
   conjunction of "AF.Lit"'s.  Asserting the "ProxyProp.T" asserts the
   conjunction; denying asserts the disjunction of the negations of
   the literals. *)

IMPORT AF, Clause;
IMPORT Wr;
IMPORT RefList;

TYPE
  Public = AF.T OBJECT
    lits: AF.LitList;
   METHODS
    format(wr: Wr.T; sense: BOOLEAN; level: CARDINAL)
  END (* OBJECT *);
  T <: Public;

REVEAL
  AF.Lit = AF.LitPublic BRANDED OBJECT
    clause: Clause.T := NIL;
  END (* OBJECT *);
    
CONST RightMostScore = LAST(REAL);
(* "RightMostScore" is the score given to clauses that occur in a rightmost
   context, so that they will be split on first. *)

(* If "p" is a "T", then "p" represents the conjunction of "pf.lits". *)

PROCEDURE Init();
(* Must be called before any other procedure of this interface. *)

PROCEDURE New(lits: RefList.T): T;
(* Creates a new "ProxyProp.T" "p", representing the conjunction of
   the literals in "lits". *)

PROCEDURE NewLit(lits: RefList.T; sense: BOOLEAN): AF.Lit;
(* Equivalent to "NEW(AF.Lit, af := New(lits), sense := sense)". *)

PROCEDURE And(l1, l2: AF.Lit): AF.Lit;
(* Equivalent to "NewLit(RefList.List2(l1, l2), TRUE)". *)

PROCEDURE Or(l1, l2: AF.Lit): AF.Lit;
(* Equivalent to "NewLit(RefList.List2(AF.Not(l1), AF.Not(l2)), FALSE)". *)

PROCEDURE Push();
(* Save the state. *)

PROCEDURE Pop();
(* Undo the creation of any "T"'s since the last "Push". *)

PROCEDURE Stats();
(* Print any relevant statistics to standard error. *)

PROCEDURE ConjOfNonProxies(p: T): BOOLEAN;
(* Returns "TRUE" iff "p" represents a conjunction of non-proxy
   literals. *)

PROCEDURE CNFSize(p: T; sense: BOOLEAN; VAR (*OUT*) c, w: CARDINAL);
(* Returns a conservative estimate of the number of conjuncts
   in the CNF of "p IFF sense" in "c", and of the maximum width of a
   clause in the CNF in "w". *)

PROCEDURE CNF(p: T; sense: BOOLEAN): RefList.T;
(* Returns a list of lists of non-proxy literals.  The inner lists
   represent disjunctions and the outer list represents a conjunction,
   the conjunctive normal form of "p IFF sense". *)

PROCEDURE AddLbls(l: AF.Lit; lbls: RefList.T (* OF AF.Label *);
                  sense: BOOLEAN): AF.Lit;
(* Adds all the labels in "lbls" whose sense agrees with "sense" to
   the label list of "l", giving them the sense of "l". *)

END ProxyProp.
