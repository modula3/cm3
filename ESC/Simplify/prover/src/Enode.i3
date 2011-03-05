(* Copyright (C) 2002 Hewlett-Packard Company *)
(* Copyright (C) 2000, 2002 Compaq Computer Corporation *)
(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Wed May 15 21:48:34 PDT 2002 by saxe                     *)
(*      modified on Fri Nov  1 10:29:16 PST 1996 by detlefs                  *)

(* This interface represents a resettable conjunction of equalities
   and distinctions between terms containing uninterpreted function
   symbols.  Call this conjunction "C"; by {\it resettable}, we mean
   that we also represent a stack "SC" of previous values of "C", and
   provide the ability to return to them.
*)

INTERFACE Enode;

<*PRAGMA SPEC*>

IMPORT AF, Atom, FPrint, RefList;

TYPE
  SimplexMisc <: ROOT;
  OrdersMisc <: SimplexMisc;  (* This order choice is arbitrary. *)
  Misc <: OrdersMisc;
  Public = BRANDED OBJECT
    id: INTEGER
   METHODS
    getId(): INTEGER;
    getFP(): FPrint.T;
    getMisc(alloc := TRUE): Misc;
    shortId(): INTEGER;
  END (* OBJECT *);
  T <: Public;
  FSym <: T;

  Equality <: AF.T;
  DistClass <: AF.T;

(* An "Enode.T" (or enode) represents a term.  The call "e.getId()"
   returns an identifier for "e" that is distinct from the identifier
   of any enode not equivalent to "e" at the time of the call.  The
   call "e.getFP()" returns a fingerprint of the node.  The call 
   "e.getMisc(alloc)" returns the "Misc" object associated with
   "e"; if "alloc" is "TRUE" allocates a new one and associates with
   "e" if necessary.
   
   "e.shortId()" returns a hash of "e", independent of subsequent merges.
*)

<*SPEC VAR aMisc: MAP T TO Misc *>
(* Maps enodes to their Misc records. *)
<*SPEC VAR aRoot: MAP T TO T *>
(* Maps enodes to the roots of their equivalence classes. *)

<*SPEC T.getMisc(e, alloc) 
       MODIFIES aMisc[e]
       ENSURES RES = aMisc'[e]
           AND (alloc IMPLIES RES # NIL)
           AND (NOT alloc IMPLIES aMisc' = aMisc) *>

PROCEDURE FromSym(sym: Atom.T; fsym := FALSE): T;
(* {\def\dq{\char'042} Requires "sym # NIL".  Return the unique enode
   representing the variable (if "fsym" is "FALSE") or function symbol
   (if "fsym" is "TRUE") "t", creating it if necessary.}
*)

<*SPEC FromInt(i) ENSURES RES # NIL *>
PROCEDURE FromInt(i: INTEGER): T;
(* Return the unique enode representing the integer "i", creating it
   if necessary.  Enode representing integers are automatically
   assumed to be constants; see "IsConst" below.
*)

PROCEDURE IntHasEnode(i: INTEGER): BOOLEAN;
(* Returns "TRUE" iff an enode representing the integer "i" exists. *)

PROCEDURE FromLongReal(lr: LONGREAL): T;
(* Return the unique enode representing the LONGREAL "lr", creating it
   if necessary.
*)

<*SPEC Root(e) REQUIRES e # NIL ENSURES RES = aRoot[e] AND RES # NIL *>
PROCEDURE Root(e: T): T;
(* Return the unique representative of the equivalence class of "e". *)

PROCEDURE Cons(e, f: T): T;
(* Return an enode whose "car" is equivalent to "e", and whose
   "cdr" is equivalent to "f", creating it if necessary. *)

PROCEDURE Activate(n: T);
(* Ensures that "n" is active, returning "TRUE" and pushing an undo
   record if "n" transitioned from inactive to active. *)
<*SPEC Activate(n) *>

PROCEDURE MakePreInterned(n: T);
(* Make "n" be preinterned.  This is undoable. *)

PROCEDURE NewEq(e, f: T; sense := TRUE): AF.Lit;
(* Return a literal "l" such that the call "l.af.assert(l)" adds
   the identity "e = f", if "l.sense" is "TRUE", or "e # f", if "l.sense"
   is "FALSE", to "C". *)

PROCEDURE NewDist(terms: RefList.T; sense: BOOLEAN): AF.Lit;
(* If "sense" is "TRUE", returns a literal such that asserting
   "l.af.assert(l)" adds the conjunction of all distinction
   between terms in "terms" to "C".  If "l.sense" is "FALSE",
   Return a literal "l" such that the call "l.af.assert(l)" adds
   a clause implying the equality of some pair of terms in "terms" to "C".
*)

TYPE Mapper = PROCEDURE(e: T; data: REFANY): BOOLEAN;

PROCEDURE MapOverTruePreds(psym: Atom.T; m: Mapper; data: REFANY): BOOLEAN;
(* For every parent enode "p" with "psym" as function symbol, calls
   "m(p, data)", returning "FALSE" if the call does.  If every call
   returns "TRUE", returns "TRUE". *)

PROCEDURE Args(p: T; VAR (*OUT*) res: ARRAY OF T; 
               VAR (*OUT*) resRes: RefList.T);
(* Requires "p" to be a parent enode whose "car" is a function symbol.
   Returns the arguments of the function application, using "res" and
   then "resRes" for any other arguments. *)

PROCEDURE MakePredSym(e: T);
(* Declares "e" to be a predicate symbol. *)

PROCEDURE IsPredSym(e: T): BOOLEAN;
(* Returns whether "e" is a predicate symbol. *)

PROCEDURE IsPredTerm(e: T): BOOLEAN;
(* Returns whether "e" is an application of a predicate symbol or
   an application of "select" to an application of a predicate map
   symbol. *)

VAR (* CONST *) enil, eTrue: T;
                trueSym: Atom.T;
                plusFS, minusFS, timesFS: FSym;

PROCEDURE Init();
(* "C := TRUE, SC := []" *)

PROCEDURE Push();
(* Save the current state of the conjunction "C".  That is, set
   "SC:hipush(C)".
*)

PROCEDURE Pop();
(* Restore "C" to its last saved state.  That is, set "C :=
   SC:hipop()".
*)

PROCEDURE ComputeSxSizes();
(* Compute an internal map "sxSz" that maps each term "t" to 
   a minimal-sized S-expression representing "t", given the current
   value of "C". *)

PROCEDURE ToSx(t: T): REFANY;
(* Return "sxSz[t]".  Note that this will be invalid if "C" has
   changed since "ComputeSxSizes" was last called. *)

PROCEDURE Top(): RefList.T;
(* Return "C" as a list of S-expressions representing literals.
   Require that "ComputeSxSizes" has been called since the last change
   to "C". *)

PROCEDURE IsConst(e: T);
(* Asserts that "e", which is required to be a leaf, is a constant.
   If an equivalence class contains a constant node, then a constant
   node is guaranteed to be selected as the equivalence class
   representative to which congruence class representatives are
   equated in the output of "Top". *)

PROCEDURE Status(eq: Equality): AF.TruthVal;
(* Returns "TruthVal.TrueAsserted" if it is known in the current
   context that "a = b", "TruthVal.FalseAsserted" if it is known that
   "a # b", and "TruthVal.Unknown" otherwise. *)

PROCEDURE GEStatus(a, b: T): AF.TruthVal;
(* Returns "TruthVal.TrueAsserted" if "a" and "b" are equivalent or if
   they are (equivalent) to integer constants with " a >= b".  Returns
   "TruthVal.FalseAsserted" if "a" and "b" are (equivalent to) integer
   constants with "a < b".  Returns "TruthVal.Unknown" otherwise. *)

PROCEDURE FingerP(e: T): FPrint.T;
(* Returns a fingerprint of "e".  May change if the egraph
   equivalence relation changes, but small or irrelevant egraph
   changes are unlikely to change the fingerprint. *)

(* These is exposed only for debugging purposes. *)
PROCEDURE DbgToSx(e: T): REFANY;
PROCEDURE Stats();

END Enode.
