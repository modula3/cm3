(* Copyright (C) 2002 Hewlett-Packard Company *)
(* Copyright (C) 2000, 2002 Compaq Computer Corporation *)
(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Wed Jun 26 14:56:54 PDT 1996 by detlefs                  *)
(*      modified on Thu Mar 21 18:31:12 1996 by gnelson                      *)

MODULE ClauseList;

IMPORT Clause, ClausePrivate, ClauseListUndoRec;
IMPORT ClauseListUndoRecSeq;
IMPORT Wr;

PROCEDURE Add(sentinel, c: Clause.T; writeUndo: BOOLEAN) =
  BEGIN
    c.pred := sentinel;
    c.succ := sentinel.succ;
    IF sentinel.succ # NIL THEN
      sentinel.succ.pred := c
    END (* IF *);
    sentinel.succ := c;
    IF writeUndo THEN
      undoStack.addhi(ClauseListUndoRec.T{tag := ClauseListUndoRec.Tag.Add,
                                          sentinel := sentinel,
                                          c := c})
    END (* IF *)
  END Add;

PROCEDURE Delete(sentinel, c: Clause.T; writeUndo := TRUE) =
  BEGIN
    <*ASSERT c.pred # NIL*>
    c.pred.succ := c.succ;
    IF c.succ # NIL THEN c.succ.pred := c.pred END (* IF *);
    IF writeUndo THEN
      undoStack.addhi(ClauseListUndoRec.T{tag := ClauseListUndoRec.Tag.Delete,
                                          sentinel := sentinel,
                                          c := c})
    END (* IF *);
  END Delete;

VAR undoStack: ClauseListUndoRecSeq.T;

PROCEDURE Init() =
  BEGIN
    undoStack := NEW(ClauseListUndoRecSeq.T).init(20);
  END Init;

PROCEDURE Push() =
  BEGIN
    undoStack.addhi(ClauseListUndoRec.T{tag := ClauseListUndoRec.Tag.Mark,
                                        sentinel := NIL,
                                        c := NIL})
  END Push;

PROCEDURE Pop() =
  BEGIN
    LOOP
      VAR rec := undoStack.remhi(); BEGIN
        CASE rec.tag OF
        | ClauseListUndoRec.Tag.Mark =>
            EXIT
        | ClauseListUndoRec.Tag.Add =>
            Delete(rec.sentinel, rec.c, writeUndo := FALSE)
        | ClauseListUndoRec.Tag.Delete =>
            Add(rec.sentinel, rec.c, writeUndo := FALSE)
        END (* CASE *)
      END (* BEGIN *)
    END (* LOOP *)
  END Pop;

PROCEDURE DBGPrint(wr: Wr.T; sentinel: Clause.T) =
  VAR c := sentinel.succ; BEGIN
    WHILE c # NIL DO
      ClausePrivate.DBGPrint(wr, c); c := c.succ
    END (* WHILE *)
  END DBGPrint;

BEGIN
END ClauseList.

