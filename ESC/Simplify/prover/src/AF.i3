(* Copyright (C) 2002 Hewlett-Packard Company *)
(* Copyright (C) 2000, 2002 Compaq Computer Corporation *)

(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Wed May 15 21:58:49 PDT 2002 by saxe                         *)
(*      modified on Fri Oct 23 14:56:01 PDT 1998 by gnelson                      *)
(*      modified on Tue Oct 29 15:49:44 PST 1996 by detlefs                      *)

(* An "AF.T" is an atomic formula; a basic assertion of a theory.
   "AF" maintains a map from "AF.T"'s to information about their truth
   values, and a stack of previously saved states of that map.
*)

INTERFACE AF;

IMPORT PredSx, LabelName;
IMPORT Wr, Word, FPrint;
IMPORT RefList;

TYPE
  TruthVal = { FalseAsserted, False, Unknown, True, TrueAsserted };
  TVSet = SET OF TruthVal;
  PropSet = SET OF [0 .. Word.Size - 9];

  Public = OBJECT
    id: INTEGER;
    scanGeneration: INTEGER := 0;
    tval: BITS 8 FOR TruthVal:= TruthVal.Unknown;
    props: (*BITS Word.Size - 8 FOR*) PropSet
   METHODS
    init(): T;
    assert(lit: Lit): BOOLEAN (* := NIL *);
    get(): TruthVal;
    set(val, asserted: BOOLEAN);

    equalInContext(af2: T): BOOLEAN;
    hashInContext(): Word.T;
    toSx(normForm := FALSE): REFANY;   (* For debugging. *)
    fingerprint(): FPrint.T;
  END (* OBJECT *);
  T <: Public;

(* Bits zero and one of "prop" is used by "Clause.InitClause".  The other bits are at present unused. *)
    
CONST
  TVTrue = TVSet{TruthVal.True, TruthVal.TrueAsserted};
  TVFalse = TVSet{TruthVal.False, TruthVal.FalseAsserted};

(* Each logical theory "Th" is embodied by a different interface, and
   provides one or more subtypes of "AF", and a resettable global
   conjunction "Th.C" of literals of its theory.  Each subtype must
   provide an "assert" method such that "af.assert(lit)" performs
   "Th.C := Th.C AND (af = sense)" and returns "TRUE" iff "Th.C" is
   satisfiable.  If "assert" returns "TRUE", it also propagates any
   equalities between variables that are entailed by "Th.C" by calling
   "Context.Propagate".  We require that this procedure be sound, but
   not necessarily complete; that is, if it returns "FALSE" it must be
   correct; if it returns "TRUE", it may be incorrect.

   The method call "af.fingerprint()" returns a fingerprint of the
   argument terms of the atomic formula.  The fingerprint need not
   reflect the particular type of the atomic formula.  The fingerprint
   may depend on the equivalence relation of the current context, but
   hopefully not on any irrelevant aspects.

   A theory interface must {\it canonicalize} its atomic formulas, by
   ensuring that any two formulas with the same "id" have
   the same semantic meaning.  If "af" is a "T", the call
   "af.init()" initializes and returns "af", giving it a new,
   unique "id", allocating space in an internal table for its
   truth value, which is initialized to "Unknown".

   The call "af.toSx()" returns an S-expression representation of "af".
*)

PROCEDURE Init();
(* Must be called before any other procedure of this interface. *)

PROCEDURE Push();
(* Save the state of the truth value map. *)

PROCEDURE Pop();
(* Restore the truth value map to its previously saved state. *)

TYPE
  LitPublic = OBJECT
    af: T;
    sense := TRUE;
    activate := TRUE;
    rightMost := FALSE;
    lbls: RefList.T := NIL; (* Of Label *)
  END (* OBJECT *);
  Lit <: LitPublic;
  LitList = RefList.T;

  Label = OBJECT
    sense: BOOLEAN;
    name: LabelName.T;
  END (* OBJECT *);


(* The literal "l" represents the assertion that the atomic formula
   "l.af" has the truth value "l.sense".  If "l.activate" is "TRUE",
   asserting this literal will cause any associated enodes to become
   eligible for matching.  Each literal is associated uniquely with a
   clause; "l.clause" is set by the init method of the clause of which
   "l" is a member.  "l.rightMost" is true of literals that are
   rightmost in their clause; this may effect the order of case
   splits.
*)

VAR (* CONST *)
  trueLit, falseLit: Lit;
  trueAF: T;
(* These are the canonical representations of the literatls "TRUE" and
   "FALSE", and the atomic formula for "TRUE". *)

PROCEDURE Status(lit: Lit): TruthVal;
(* If the truth value of "lit.af" is known, return "Trit.True" if that
   truth value matches "lit.sense", otherwise return "Trit.False".
   If the truth value of "lit.af" is not known, return "Trit.Unknown".
*)

PROCEDURE DeepStatus(lit: Lit; lo, hi: TruthVal): TruthVal;
(* Like "Status" but recurses down the tree of proxies, thus sometimes
   returning a more accurate answer.  Projects its answer into the interval
   "lo..hi" *)

PROCEDURE Not(lit: Lit): Lit;
(* Return the negation of "lit". *)

PROCEDURE LitCopy(lit: Lit): Lit;
(* Returns a new "Lit" "l" such that "l.af = lit.af" and "l.sense
   = lit.sense"; all other fields have their default values. *)

PROCEDURE LitAddLabel(lit: Lit; lblName: LabelName.T; sense: BOOLEAN);
(* Adds the "lblName" to the label list of "lit", to be asserted if
   "lit" is ever asserted in the sense "sense". *)

PROCEDURE LitEquiv(l1, l2: Lit): BOOLEAN;
(* Returns "TRUE" iff "l1.sense = l2.sense" and "l1.af" and "l2.af"
   have the same "id" (which implies that they are semantically
   equivalent).
*)

PROCEDURE LitEqual(l1, l2: Lit): BOOLEAN;
(* Returns "TRUE" iff "l1.af" = "l2.af", "l1.sense = l2.sense",
   and the label lists of "l1" and "l2" are equivalent as sets. *)

PROCEDURE LitToSx(lit: Lit; normForm := FALSE): PredSx.T;
(* Returns an S-expression representing "lit"; if "normForm" is
   "TRUE", assumes that "Enode.ToSx" can be used to get a canonical
   representation of each term. *)

PROCEDURE PrintLit(wr: Wr.T; lit: Lit; normForm := FALSE);
(* Prints "lit" on "wr", and flushes "wr".  Uses "LitToSx(lit,
   normForm)" to form the S-expression to be printed. *)

PROCEDURE PrintLit2(wr: Wr.T; lit: Lit; sense := TRUE);
(* Version that recursively prints out the structure of ProxyProps. *)

PROCEDURE LitFP(lit: Lit): FPrint.T;
(* Returns a fingerprint of "lit".  May change if the egraph
   equivalence relation changes, but small or irrelevant egraph
   changes are unlikely to change the fingerprint. *)

PROCEDURE LitListCopy(lits: LitList): LitList;
(* Returns a list containing copies (in the sense of "LitCopy", above)
   of the literals in "lits". *)

PROCEDURE LitListSortD(lits: LitList): LitList;
(* Destructively sorts the list of literals "lits" into a canonical order,
   returning the sorted list. *)

PROCEDURE Stats();

END AF.
