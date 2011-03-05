(* Copyright (C) 2002 Hewlett-Packard Company *)
(* Copyright (C) 2000, 2002 Compaq Computer Corporation *)
(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri May 24 17:18:08 PDT 1996 by detlefs                  *)

INTERFACE PredDefs;

IMPORT PredSx, MatchingRule, Prover;
IMPORT Atom;
IMPORT RefList;

TYPE
  T = OBJECT
    args: INTEGER;
    tmpl, posTmpl, negTmpl: MatchingRule.Template := NIL;
  END (* OBJECT *);

PROCEDURE DeclPred(predSym: Atom.T; vars: RefList.T; ind: Atom.T;
                   def: PredSx.T)
  RAISES { Prover.Error };

PROCEDURE GetPredDef(predSym: Atom.T; VAR def: T): BOOLEAN;

PROCEDURE IsPredSym(predSym: Atom.T): BOOLEAN;

PROCEDURE GetPredMapDef(predSym: Atom.T; VAR def: T): BOOLEAN;

PROCEDURE IsPredMapSym(predMapSym: Atom.T): BOOLEAN;

PROCEDURE TmplInSense(def: T; sense: BOOLEAN): MatchingRule.Template;

PROCEDURE Push();

PROCEDURE Pop();

PROCEDURE Top(): RefList.T;

PROCEDURE Init();

END PredDefs.
