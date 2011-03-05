(* Copyright (C) 2002 Hewlett-Packard Company *)
(* Copyright (C) 2000, 2002 Compaq Computer Corporation *)
(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Aug 29 15:00:32 PDT 2000 by saxe                     *)
(*      modified on Fri Jun 28 11:30:06 PDT 1996 by detlefs                  *)

(* This interface defines the types used to represent label names (see msg
   on src.sparta, detlefs 9/14/95), and the global set of asserted
   label names.  In addition, it represents an undoable global set "S" of
   asserted label names.
*)

INTERFACE LabelName;

IMPORT Atom, Wr, Word;
IMPORT RefList, AtomList;

TYPE T <: ROOT;

PROCEDURE Init();

PROCEDURE MkAtom(a: Atom.T): T;

PROCEDURE MkQuant(t: T; args: RefList.T): T;

PROCEDURE MkAnd(t: T; i, n: CARDINAL): T;
(* We require that all calls to "MkAnd" with a given "t" have the same
   value for "n", and that "1 <= i <= n". *)

VAR splitCntr := 0;

PROCEDURE MkOr(t: T; uniq: INTEGER): T;

(* Adds "t" to the set of labels that apply to the current context.
   Returns "FALSE" if assertion of "t" results in assertion of an
   at-sign-containing label that has already been reported as part
   of a counterexample context. *)
PROCEDURE Assert(t: T): BOOLEAN;

PROCEDURE Push();

PROCEDURE Pop();

PROCEDURE Atoms(markReported: BOOLEAN := FALSE): AtomList.T;

PROCEDURE StartProof();

PROCEDURE Print(wr: Wr.T; t: T);

PROCEDURE Equal(READONLY t1, t2: T): BOOLEAN;

PROCEDURE Hash(READONLY t: T): Word.T;

PROCEDURE Stats();

VAR traceTriggers: INTEGER := 0;

VAR anyAtSignLabels: BOOLEAN := FALSE;

END LabelName.
