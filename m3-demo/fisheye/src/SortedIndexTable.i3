(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Jun 16 16:46:21 PDT 1992 by muller                   *)

INTERFACE SortedIndexTable;

IMPORT SortedHashTable;

TYPE

  Item = RECORD key: REAL; data: INTEGER; END;  
  T = REF RECORD number: INTEGER; items: REF ARRAY OF Item; END;

PROCEDURE New(size: INTEGER): T;

PROCEDURE Clear(table: T);

PROCEDURE Insert(table: T; item: Item): BOOLEAN;
(* inserted in sorted order, returns true if succeeds *)

PROCEDURE Reverse(table: T);
(* reverses the order of items *)

PROCEDURE CopySortedIndexTable(fromSortedIndexTable: T; 
     	       toSortedIndexTable: T; n: INTEGER): BOOLEAN;

PROCEDURE CopySortedHashTable(fromSortedHastTable: SortedHashTable.T; 
     	       	        toSortedIndexTable: T; n: INTEGER): BOOLEAN;
(* all the (key,data) pairs are copied into IndexTable, copied
as largest ones first *)

PROCEDURE Print(table: T);

END SortedIndexTable.

