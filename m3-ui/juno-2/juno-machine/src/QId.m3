(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Sat Jul 10 01:51:09 PDT 1993 by heydon                   *)

MODULE QId;

IMPORT Atom, Word, Text;

PROCEDURE Equal(READONLY qid1, qid2: T): BOOLEAN =
  BEGIN
    RETURN qid1.id0 = qid2.id0 AND qid1.id1 = qid2.id1
  END Equal;

PROCEDURE Hash(READONLY qid: T): Word.T =
  VAR res := Text.Hash(Atom.ToText(qid.id1)); BEGIN
    IF qid.id0 # NIL THEN
      res := Word.Xor(res, Text.Hash(Atom.ToText(qid.id0)))
    END;
    RETURN res
  END Hash;

BEGIN END QId.
