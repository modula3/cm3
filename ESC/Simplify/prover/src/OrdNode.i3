(* Copyright (C) 2002 Hewlett-Packard Company *)
(* Copyright (C) 2000, 2002 Compaq Computer Corporation *)
(* Copyright 95 Digital Equipment Corporation.
   Digital Internal Use Only
   Last modified on Mon May  1 17:47:51 PDT 2000 by saxe   
        modified on Thu Sep  5 12:09:54 PDT 1996 by detlefs
*)

(* An "OrdNode" is the representative of an enode in a partial order. *)

INTERFACE OrdNode;

IMPORT Enode;
IMPORT Word;
IMPORT AtomSet;

TYPE
  T = OBJECT
    mark := 0;  (* logically a local variable of Orders.Top *)
    e: Enode.T;
    ords: AtomSet.T := NIL
  END (* OBJECT *);

(* If "t" is an "OrdNode.T", then "t" represents the enode "t.e", and
   "t.ords" is the set of partial orders in which "t" occurs. 
   (Partial orders are represented by their non-strict function symbol.)
*)

PROCEDURE Equal(READONLY on1, on2: T): BOOLEAN;
(* Returns "TRUE" iff "on1" and "on2" represent the same enode. *)

PROCEDURE Hash(READONLY on: T): Word.T;
(* Returns a hash function for "on"; if "on1" and "on2" are equal, so
   are their hash values. *)

PROCEDURE Compare(READONLY on1, on2: T): [-1..1];
(* Returns an arbitrary comparison for "on1" and "on2", but one that
   is stable for the intersection of the lifetimes of "on1" and "on2".
*)

END OrdNode.
