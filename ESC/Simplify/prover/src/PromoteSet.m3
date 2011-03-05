(* Copyright (C) 2002 Hewlett-Packard Company *)
(* Copyright (C) 2000, 2002 Compaq Computer Corporation *)
(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu May  2 12:59:23 PDT 2002 by saxe                     *)
(*      modified on Tue Mar 19 18:41:42 PST 1996 by detlefs                  *)

MODULE PromoteSet;

(* A "PromotedSet.T" represents a bounded set of pairs of "Clause"
   fingerprints and scores. *)

IMPORT Clause, ClausePrivate, FPrint;
IMPORT FPRealTbl, FPSeq;

REVEAL
  T = Public BRANDED OBJECT
    bnd: CARDINAL;
    scores: FPRealTbl.Default;
    lru: FPSeq.T;
   OVERRIDES
    init := Init;
    insert := Insert;
    member := Member;
    updateScore := UpdateScore;
    normalize := Normalize;
    size := Size;
  END (* OBJECT *);

PROCEDURE Init(self: T; bnd: CARDINAL): T =
  BEGIN
    self.bnd := bnd;
    self.scores := NEW(FPRealTbl.Default).init(MIN(bnd,100));
    self.lru := NEW(FPSeq.T).init(MIN(bnd,100));
    RETURN self
  END Init;

PROCEDURE Insert(self: T; cl: Clause.T; score: REAL) =
  BEGIN
    IF NOT self.scores.put(cl.fp, score) THEN
      self.lru.addhi(cl.fp);
      IF self.lru.size() > self.bnd THEN
        VAR remFP := self.lru.remlo(); dum: REAL; BEGIN
          EVAL self.scores.delete(remFP, dum)
        END (* BEGIN *)
      END (* IF *)
    END (* IF *)
  END Insert;

PROCEDURE Member(self: T; cl: Clause.T; VAR (*OUT*) score: REAL): BOOLEAN =
  BEGIN RETURN self.scores.get(cl.fp, score)
  END Member;

PROCEDURE UpdateScore(self: T; cl: Clause.T; score: REAL) =
  VAR oldScore: REAL; BEGIN
    IF self.scores.get(cl.fp, oldScore) THEN
      EVAL self.scores.put(cl.fp, score)
    END (* IF *)
  END UpdateScore;

PROCEDURE Normalize(self: T; min: REAL) =
  VAR newScores :=
          NEW(FPRealTbl.Default).init(MIN(self.bnd,2*self.scores.size()));
      iter := self.scores.iterate(); fp: FPrint.T; score: REAL;
  BEGIN
    WHILE iter.next(fp, score) DO
      EVAL newScores.put(fp, NormScore(score) + min)
    END (* WHILE *);
    self.scores := newScores
  END Normalize;

PROCEDURE NormScore(score: REAL): REAL =
  BEGIN RETURN 1.0 - 1.0/(score + 1.0)
  END NormScore;

PROCEDURE Size(self: T): INTEGER =
  BEGIN RETURN self.lru.size() END Size;

BEGIN
END PromoteSet.
