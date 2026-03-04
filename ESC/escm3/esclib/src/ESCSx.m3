(* Copyright (c) 2026, Mika Nystrom.  All rights reserved. *)

MODULE ESCSx;

IMPORT Atom, RefList, ESCNameMap;

(* Binary S-expression: (op a b) *)
PROCEDURE MkBin(op: Atom.T; a, b: REFANY): RefList.T =
  BEGIN RETURN RefList.List3(op, a, b) END MkBin;

(* Unary S-expression: (op a) *)
PROCEDURE MkUn(op: Atom.T; a: REFANY): RefList.T =
  BEGIN RETURN RefList.List2(op, a) END MkUn;

PROCEDURE MkAnd(a, b: RefList.T): RefList.T =
  BEGIN RETURN MkBin(atAnd, a, b) END MkAnd;

PROCEDURE MkOr(a, b: RefList.T): RefList.T =
  BEGIN RETURN MkBin(atOr, a, b) END MkOr;

PROCEDURE MkNot(a: RefList.T): RefList.T =
  BEGIN RETURN MkUn(atNot, a) END MkNot;

PROCEDURE MkImplies(a, b: RefList.T): RefList.T =
  BEGIN RETURN MkBin(atImplies, a, b) END MkImplies;

PROCEDURE MkIff(a, b: RefList.T): RefList.T =
  BEGIN RETURN MkBin(atIff, a, b) END MkIff;

PROCEDURE MkAndList(conjuncts: RefList.T): RefList.T =
  BEGIN
    IF conjuncts = NIL THEN RETURN List1(atTrue) END;
    IF conjuncts.tail = NIL THEN
      RETURN NARROW(conjuncts.head, RefList.T);
    END;
    RETURN RefList.Cons(atAnd, conjuncts);
  END MkAndList;

PROCEDURE MkForall(vars: RefList.T; pats: RefList.T;
                   body: RefList.T): RefList.T =
  BEGIN
    IF pats # NIL THEN
      RETURN RefList.Cons(atForall,
               RefList.List3(vars, RefList.Cons(atPats, pats), body));
    ELSE
      RETURN RefList.List3(atForall, vars, body);
    END;
  END MkForall;

PROCEDURE MkExists(vars: RefList.T; body: RefList.T): RefList.T =
  BEGIN RETURN RefList.List3(atExists, vars, body) END MkExists;

PROCEDURE MkEq(a, b: REFANY): RefList.T =
  BEGIN RETURN MkBin(atEq, a, b) END MkEq;

PROCEDURE MkNeq(a, b: REFANY): RefList.T =
  BEGIN RETURN MkBin(atNeq, a, b) END MkNeq;

PROCEDURE MkLt(a, b: REFANY): RefList.T =
  BEGIN RETURN MkBin(atLt, a, b) END MkLt;

PROCEDURE MkLe(a, b: REFANY): RefList.T =
  BEGIN RETURN MkBin(atLe, a, b) END MkLe;

PROCEDURE MkGt(a, b: REFANY): RefList.T =
  BEGIN RETURN MkBin(atGt, a, b) END MkGt;

PROCEDURE MkGe(a, b: REFANY): RefList.T =
  BEGIN RETURN MkBin(atGe, a, b) END MkGe;

PROCEDURE MkPlus(a, b: REFANY): RefList.T =
  BEGIN RETURN MkBin(atPlus, a, b) END MkPlus;

PROCEDURE MkMinus(a, b: REFANY): RefList.T =
  BEGIN RETURN MkBin(atMinus, a, b) END MkMinus;

PROCEDURE MkTimes(a, b: REFANY): RefList.T =
  BEGIN RETURN MkBin(atTimes, a, b) END MkTimes;

PROCEDURE MkSelect(map, index: REFANY): RefList.T =
  BEGIN RETURN MkBin(atSelect, map, index) END MkSelect;

PROCEDURE MkStore(map, index, val: REFANY): RefList.T =
  BEGIN RETURN RefList.Cons(atStore, RefList.List3(map, index, val)) END MkStore;

PROCEDURE MkNumber(arr, dim: REFANY): RefList.T =
  BEGIN RETURN RefList.List3(atNumber, arr, dim) END MkNumber;

PROCEDURE MkLabel(name: Atom.T; body: RefList.T): RefList.T =
  BEGIN RETURN RefList.List3(atLbl, name, body) END MkLabel;

PROCEDURE MkIsPred(typePred: Atom.T; x: REFANY): RefList.T =
  BEGIN
    RETURN MkEq(RefList.List2(typePred, x), ESCNameMap.AtTrue);
  END MkIsPred;

PROCEDURE MkSubtype(tc1, tc2: Atom.T): RefList.T =
  BEGIN
    RETURN MkEq(MkBin(atSubtype, tc1, tc2), ESCNameMap.AtTrue);
  END MkSubtype;

PROCEDURE MkDistinct(atoms: RefList.T): RefList.T =
  BEGIN RETURN RefList.Cons(atDistinct, atoms) END MkDistinct;

PROCEDURE Cons(head: REFANY; tail: RefList.T): RefList.T =
  BEGIN RETURN RefList.Cons(head, tail) END Cons;

PROCEDURE List1(a: REFANY): RefList.T =
  BEGIN RETURN RefList.List1(a) END List1;

PROCEDURE List2(a, b: REFANY): RefList.T =
  BEGIN RETURN RefList.List2(a, b) END List2;

PROCEDURE List3(a, b, c: REFANY): RefList.T =
  BEGIN RETURN RefList.List3(a, b, c) END List3;

BEGIN
  atAnd := Atom.FromText("AND");
  atOr := Atom.FromText("OR");
  atNot := Atom.FromText("NOT");
  atImplies := Atom.FromText("IMPLIES");
  atIff := Atom.FromText("IFF");
  atForall := Atom.FromText("FORALL");
  atExists := Atom.FromText("EXISTS");
  atPats := Atom.FromText("PATS");
  atNopats := Atom.FromText("NOPATS");
  atMpat := Atom.FromText("MPAT");
  atEq := Atom.FromText("EQ");
  atNeq := Atom.FromText("NEQ");
  atLt := Atom.FromText("<");
  atLe := Atom.FromText("<=");
  atGt := Atom.FromText(">");
  atGe := Atom.FromText(">=");
  atPlus := Atom.FromText("+");
  atMinus := Atom.FromText("-");
  atTimes := Atom.FromText("*");
  atSelect := Atom.FromText("select");
  atStore := Atom.FromText("store");
  atNumber := Atom.FromText("NUMBER");
  atAddr := Atom.FromText("ADDR");
  atLbl := Atom.FromText("LBL");
  atLblneg := Atom.FromText("LBLNEG");
  atLblpos := Atom.FromText("LBLPOS");
  atSubtype := Atom.FromText("SUBTYPE");
  atSubtype1 := Atom.FromText("SUBTYPE1");
  atTypecode := Atom.FromText("TYPECODE");
  atDistinct := Atom.FromText("DISTINCT");
  atTrue := ESCNameMap.AtTrue;
  atFalse := ESCNameMap.AtFalse;
  atDefpred := Atom.FromText("DEFPRED");
  atDefpredmap := Atom.FromText("DEFPREDMAP");
  atBgPush := Atom.FromText("BG_PUSH");
  atBgPop := Atom.FromText("BG_POP");
  atOrd := Atom.FromText("ORD");
END ESCSx.
