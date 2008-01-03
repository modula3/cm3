(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Jun 16 16:46:22 PDT 1992 by muller                   *)

INTERFACE SortedHashTable;

TYPE
  ItemNode = RECORD key: REAL; data: INTEGER; next: REF ItemNode; END;
  T = REF ARRAY OF REF ItemNode;

PROCEDURE New(size: INTEGER): T;

PROCEDURE Clear(table: T);
(* initializes the pointer to NIL *)

PROCEDURE Insert(table: T; new_key: REAL; new_data: INTEGER): BOOLEAN;
(* inserted in sorted order, smaller ones first *)

END SortedHashTable.
