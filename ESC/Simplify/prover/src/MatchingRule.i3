(* Copyright (C) 2002 Hewlett-Packard Company *)
(* Copyright (C) 2000, 2002 Compaq Computer Corporation *)
(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Sat Sep  8 19:39:30 PDT 2001 by saxe                     *)
(*      modified on Fri Jun 21 16:28:28 PDT 1996 by detlefs                  *)

INTERFACE MatchingRule;

IMPORT Enode, PairSet, ParentSet, AF, PredSx;
IMPORT Wr, Atom, Word, Sx;
IMPORT RefList;

(* This interface defines the type representing matching rules. *)

CONST
  MaxPatVars = 64;

TYPE
  Pattern = RefList.T;
  PatVar = BRANDED REF INTEGER;
  Template = Sx.T;
  PatVarNum = [0..MaxPatVars-1];
  PatVarNumSet = SET OF PatVarNum;

  Substitution = ARRAY OF Enode.T;
  ConcSubst = ARRAY PatVarNum OF Enode.T;
(* A substitution is a partial map from atoms to enodes
   represented as an array indexed by pattern variables. *)

  PieceSet = SET OF [0..Word.Size-1];

  PieceRec = RECORD
    piece: Template;
    pcs: PairSet.Sparse;
    pvs: PatVarNumSet;
    trivial, disjointTrivial, enabled: BOOLEAN
  END (* RECORD *);
  PieceRecArr = REF ARRAY OF PieceRec;

  PVRec = RECORD
    pps: PairSet.Sparse;
    enabled: BOOLEAN
  END (* RECORD *);
  PVRecArr = REF ARRAY OF PVRec;

  PatObj = OBJECT
    p: Pattern;
    matched := FALSE;
    hasTrivialPat, hasRepeatedVar: BOOLEAN;
    pcPairs, ppPairs: PairSet.Sparse;
    pars: ParentSet.T;

    pieces: PieceRecArr;
    pvs: PVRecArr;
    multiplePVs := PatVarNumSet{};
  END (* OBJECT *);

CONST
  EmptyPatVarNumSet = PatVarNumSet{};
  EmptySub = Substitution{};

VAR pv: ARRAY [0..MaxPatVars-1] OF PatVar;

(* A "Pattern" is the same as a "PredSx" "<formula>", except that
   "PatVar"'s may appear as legal terms, representing variables to be
   matched against or substituted for.  "Template"'s extend "Sx.T"'s
   by allowing pattern variables. *)

TYPE
  Public = OBJECT
    id: CARDINAL;
    score: REAL;
    immedPromote := FALSE;
    plungeHint := FALSE;
    parId := -1;
    trivial: BOOLEAN;
    width: INTEGER;
    pats: PatObjArr;
    commits: RefList.T (* OF CommittedArr *);
    vars: REF ARRAY OF Atom.T;
    opSym: Atom.T := NIL;
    template: Sx.T;
    clausal: BOOLEAN;
    unit: BOOLEAN;
   METHODS
    init(vars: RefList.T (* OF Atom.T *);
         pats: RefList.T (* OF RefList.T OF Pattern *);
         template: Sx.T;
         unit, clausal: BOOLEAN;
         opSym: Atom.T := NIL;
         immedPromote := FALSE;
         plungeHint := FALSE): T;
    toSx(): Sx.T;
    hash(): Word.T;
  END (* OBJECT *);
  T <: Public;

TYPE
  BoolArr = ARRAY OF BOOLEAN;
  CommittedArr = REF ARRAY OF BoolArr;
  PatObjArr = REF ARRAY OF PatObj;

(* A "MatchingRule.Rule" "mr" has two attributes: "mr.Pats", a set of
   "Pattern"'s and "mr.Templ", a "Template".  The meaning of "mr" is
   that any match of a pattern in "Pats" with a substitution "s"
   should cause the assertion of the formula obtained by instantiating
   "Templ" with "s".  "Templ" is  required to be a clause.

   The "init" method initializes a "MatchingRule.Rule" by associating
   each atom in "vars" (which is required to have no duplicates) with
   a "PatVar", and then substituting that "PatVar" for occurrences of
   the atom in "pats" and "template" to obtain the "Pats" and "Templ"
   of the resulting "MatchingRule.Rule".

   If "mr" is a "MatchingRule.Rule", "mr.ruleType" gives its {\it rule
   type}.  If all patterns in a rule have the same width, then
   "mr.width" is that width.  Otherwise, "mr.width" is 0.
*)

(* A "RefList.T" represents a pattern as follows:

    An integer represents a pattern variable.

    A list "(f l1, ... ln)" represents the application
    of the function "f" to the list of patterns
    "l1", ... "ln".

    A pattern that contains no variables is called a {\it term}.

    If "p" is a non-atomic pattern and 0 <= i < Length(p), we
    write "p[i]" to denote element "i" of p (considered as a list).

  As a consequence, a constant like "NIL" is represented by "(NIL)".
*)

TYPE
  AtomFPublic = AF.T OBJECT
   METHODS
    init(pos: T; neg: PredSx.T): AtomF
  END (* OBJECT *);
  AtomF <: AtomFPublic;

(* If "mraf" is an "AF", then "mraf.init(pos, neg)"
   initializes "mraf" so that asserting it adds the matching rule
   "pos" to the set of active rules, and denying it adds the clause
   "neg" to the clause list. *)

VAR parentRule: T := NIL;

(* When "parentRule" is non-NIL, we are in the process of interning an
   instantiation of "parentRule"; thus, any rules created as a part of
   that interning will be marked as children of "parentRule". *)

PROCEDURE OurApplySubst(pat: REFANY; READONLY s: Substitution): REFANY;
(* Applies substitution "s" to template "pat". *)

PROCEDURE SubstSxCons(READONLY hd: REFANY; tail: RefList.T): RefList.T;
PROCEDURE SubstSxFree(sx: REFANY);

PROCEDURE PatVars(t: Template): PatVarNumSet;

PROCEDURE IsTrivial(pat: Pattern): BOOLEAN;

PROCEDURE PatToPrintableSx(pat: REFANY): REFANY;

PROCEDURE Init();
PROCEDURE Push();
PROCEDURE Pop();

TYPE StatRec = RECORD ruleActivations := 0 END (* RECORD *);
VAR stats: StatRec;
PROCEDURE Stats();


VAR idWr: Wr.T;
(* If non-"NIL", writes the matching rule table to it. *)


END MatchingRule.
