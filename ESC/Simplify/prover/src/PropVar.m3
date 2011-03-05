(* Copyright (C) 2002 Hewlett-Packard Company *)
(* Copyright (C) 2000, 2002 Compaq Computer Corporation *)
(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Mar 18 21:59:38 PST 1996 by detlefs                  *)

(* An "PropVar.T" is a proposition variable. *)

MODULE PropVar;

IMPORT AF, PredSx;
IMPORT Atom, FPrint, Sx;
IMPORT RefList, AtomRefTbl, RefSeq;

REVEAL
  T = AF.T BRANDED OBJECT
    sym: Atom.T;
   OVERRIDES
    assert := Assert;
    toSx := ToSx;
    fingerprint := FP;
  END (* OBJECT *);

VAR
  propSymTab: AtomRefTbl.T;
  undoStack: RefSeq.T;
  initDone := FALSE; (* Assumed initialized by the linker. *)

PROCEDURE Init() =
  BEGIN
    IF NOT initDone THEN
      undoStack := NEW(RefSeq.T).init(20);
      propSymTab := NEW(AtomRefTbl.Default).init(100);
      initDone := TRUE
    END (* IF *)
  END Init;

PROCEDURE New(sym: Atom.T): T =
  VAR pv: T; pvRA: REFANY; BEGIN
    IF propSymTab.get(sym, pvRA) THEN
      pv := pvRA
    ELSE
      pv := NEW(T, sym := sym).init();
      EVAL propSymTab.put(sym, pv)
    END (* IF *);
    RETURN pv;
  END New;

PROCEDURE Assert(<*UNUSED*> pv: T; <*UNUSED*>lit: AF.Lit): BOOLEAN =
  BEGIN
    RETURN TRUE
  END Assert;

VAR (*CONST*) pvSym := Atom.FromText("PROPVAR");

PROCEDURE ToSx(pv: T; <*UNUSED*> normForm: BOOLEAN): REFANY =
  BEGIN
    IF pv.sym = NIL THEN
      RETURN RefList.List2(pvSym, Sx.FromInt(pv.id))
    ELSE
      RETURN pv.sym
    END (* IF *)
  END ToSx;

PROCEDURE FP(pv: T): FPrint.T =
  BEGIN RETURN FPrint.FromText(Atom.ToText(pv.sym))
  END FP;

PROCEDURE Push() =
  BEGIN undoStack.addhi(NIL)
  END Push;

PROCEDURE Pop() =
  BEGIN
    LOOP
      VAR top: Atom.T := undoStack.remhi(); ra: REFANY; BEGIN
        IF top = NIL THEN
          RETURN
        ELSE
          EVAL propSymTab.delete(top, ra)
        END (* IF *)
      END (* BEGIN *)
    END (* LOOP *)
  END Pop;

PROCEDURE Top(): RefList.T =
  VAR iter := propSymTab.iterate();
      key: Atom.T; val: REFANY;
      res: RefList.T := NIL;
  BEGIN
    WHILE iter.next(key, val) DO
      VAR pv: T := val; BEGIN
        CASE pv.get() OF
        | AF.TruthVal.TrueAsserted =>
            res := RefList.Cons(pv.sym, res)
        | AF.TruthVal.FalseAsserted =>
            res := RefList.Cons(RefList.List2(PredSx.notSym, pv.sym), res)
        ELSE
        END (* CASE *)
      END (* BEGIN *)
    END (* WHILE *);
    RETURN res
  END Top;

BEGIN
END PropVar.
