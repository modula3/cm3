(* Copyright (C) 2002 Hewlett-Packard Company *)
(* Copyright (C) 2000, 2002 Compaq Computer Corporation *)
(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Aug 17 12:31:47 PDT 2000 by rustan                   *)
(*      modified on Mon Sep 30 17:33:23 PDT 1996 by detlefs                  *)

INTERFACE ParentSet;

IMPORT Enode;

CONST Width = BITSIZE(INTEGER);

CONST Brand = "ParentSet";

TYPE
  T = SET OF [0..Width-1];

(* Represents a set of function symbol enodes. *)

PROCEDURE Empty(): T;
(* Return the empty set of enodes. *)

PROCEDURE IsEmpty(ps: T): BOOLEAN;
(* Returns "TRUE" iff "ps" is empty. *)

PROCEDURE MakeEmpty(VAR ps: T);
(* Ensures that "ps" represents the empty set. *)

PROCEDURE AddParentD(VAR res: T; p: Enode.T);
(* res := res union p. *)

PROCEDURE UnionD(VAR ps1: T; ps2: T);
(* ps1 := union of ps1 and ps2. *)

PROCEDURE Member(ps: T; id: INTEGER): BOOLEAN;
(* Returns "TRUE" iff "id" is a member of "ps". *)

PROCEDURE Overlap(ps1, ps2: T): BOOLEAN;
(* Returns whether the intersection of "ps1" and "ps2" is non-empty. *)

PROCEDURE Size(ps: T): CARDINAL;
(* Returns the number of elements in "ps". *)

END ParentSet.
