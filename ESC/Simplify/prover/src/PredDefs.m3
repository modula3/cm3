(* Copyright (C) 2002 Hewlett-Packard Company *)
(* Copyright (C) 2000, 2002 Compaq Computer Corporation *)
(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Sep 10 16:28:44 PDT 1996 by detlefs                  *)

MODULE PredDefs;

IMPORT PredSx, MatchingRule, Prover, Enode;
IMPORT Atom;
IMPORT RefList, AtomRefTbl, AtomRefTblList, RefSeq;

TYPE Entry = OBJECT ind: BOOLEAN; def: T END (* OBJECT *);

VAR preds: AtomRefTbl.T; (* --> Entry *)
    undo: RefSeq.T;



PROCEDURE DeclPred(predSym: Atom.T; vars: RefList.T; ind: Atom.T;
                   def: PredSx.T)
    RAISES { Prover.Error } =
  VAR i := 0; sub := NEW(AtomRefTblList.T).init(); BEGIN
    WHILE vars # NIL DO
      EVAL sub.put(vars.head, MatchingRule.pv[i]);
      vars := vars.tail; INC(i)
    END (* WHILE *);
    IF ind # NIL THEN
      EVAL sub.put(ind, MatchingRule.pv[i])
    END (* IF *);
    VAR tmpl := PredSx.Sub(def, sub);
        res := NEW(T, args := i, tmpl := tmpl);
        entRA: REFANY; ent: Entry;
    BEGIN
      IF preds.get(predSym, entRA) THEN
        ent := entRA;
        IF tmpl = NIL OR ent.def.tmpl # NIL THEN
          RAISE Prover.Error("Multiple definitions for predicate '" &
                Atom.ToText(predSym) & "'.")
        ELSE
          ent.def.tmpl := tmpl
        END (* IF *)
      ELSE
        VAR b := preds.put(predSym, NEW(Entry, ind := ind # NIL, def := res));
        BEGIN
          <*ASSERT NOT b*>
          undo.addhi(predSym);
          IF tmpl # NIL THEN undo.addhi(predSym) END (* IF *)
        END (* BEGIN *)
      END (* IF *)
    END (* BEGIN *);
    Enode.MakePredSym(Enode.FromSym(predSym, fsym := TRUE))
  END DeclPred;

PROCEDURE GetPredDef(predSym: Atom.T; VAR def: T): BOOLEAN =
  VAR ra: REFANY; b := preds.get(predSym, ra);
      ent: Entry := ra;
  BEGIN
    IF b AND NOT ent.ind AND ent.def.tmpl # NIL THEN
      def := ent.def; RETURN TRUE
    ELSE
      RETURN FALSE
    END (* IF *)
  END GetPredDef;

PROCEDURE IsPredSym(predSym: Atom.T): BOOLEAN =
  VAR ra: REFANY; BEGIN
    RETURN preds.get(predSym, ra) AND NOT NARROW(ra, Entry).ind
  END IsPredSym;

PROCEDURE GetPredMapDef(predSym: Atom.T; VAR def: T): BOOLEAN =
  VAR ra: REFANY; b := preds.get(predSym, ra);
      ent: Entry := ra;
  BEGIN
    IF b AND ent.ind AND ent.def.tmpl # NIL THEN
      def := ent.def; RETURN TRUE
    ELSE
      RETURN FALSE
    END (* IF *)
  END GetPredMapDef;

PROCEDURE IsPredMapSym(predSym: Atom.T): BOOLEAN =
  VAR ra: REFANY; BEGIN
    RETURN preds.get(predSym, ra) AND NARROW(ra, Entry).ind
  END IsPredMapSym;

PROCEDURE TmplInSense(def: T; sense: BOOLEAN): MatchingRule.Template =
  BEGIN
    IF sense THEN
      IF def.posTmpl = NIL THEN
        def.posTmpl := PredSx.ProcessLabels(PredSx.Desugar(def.tmpl),
                                            FALSE, NPatVarList(def.args))
      END (* IF *);
      RETURN def.posTmpl
    ELSE
      IF def.negTmpl = NIL THEN
        def.negTmpl := PredSx.ProcessLabels(
                           PredSx.Not(PredSx.Desugar(def.tmpl)),
                           FALSE, NPatVarList(def.args))
      END (* IF *);
      RETURN def.negTmpl
    END (* IF *)
  END TmplInSense;

PROCEDURE NPatVarList(n: INTEGER): RefList.T =
  VAR res: RefList.T := NIL; BEGIN
    WHILE n > 0 DO
      DEC(n);
      res := RefList.Cons(MatchingRule.pv[n], res)
    END (* WHILE *);
    RETURN res
  END NPatVarList;

PROCEDURE Push() =
  BEGIN
    undo.addhi(NIL)
  END Push;

PROCEDURE Pop() =
  BEGIN
    LOOP
      VAR top := undo.remhi(); BEGIN
        IF top = NIL THEN EXIT END (* IF *);
        VAR ra: REFANY; b := preds.get(top, ra);
            ent: Entry := ra;
        BEGIN
          <*ASSERT b *>
          IF ent.def.tmpl # NIL THEN
            ent.def.tmpl := NIL
          ELSE
            b := preds.delete(top, ra); <*ASSERT b*>
          END (* IF *)
        END (* BEGIN *)
      END (* BEGIN *)
    END (* LOOP *)
  END Pop;

PROCEDURE Top(): RefList.T =
  BEGIN
    RETURN NIL
  END Top;

PROCEDURE Init() =
  BEGIN
    preds := NEW(AtomRefTbl.Default).init();
    undo := NEW(RefSeq.T).init()
  END Init;

BEGIN
END PredDefs.
