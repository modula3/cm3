(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Oct 31 09:11:47 PST 1994 by heydon                   *)

INTERFACE QId;

(* Interface for defining a simple qualified identifier type and "Equal" and
   "Hash" procedures on that type so it may be used as a Key in a table. *)

IMPORT Atom, Word;

CONST Brand = "QId";

TYPE T = REF RECORD id0, id1: Atom.T END;

PROCEDURE Hash(READONLY qid: T): Word.T;
PROCEDURE Equal(READONLY qid1, qid2: T): BOOLEAN;

END QId.
