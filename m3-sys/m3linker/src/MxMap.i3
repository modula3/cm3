(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: MxMap.i3                                              *)
(* Last Modified On Tue Mar 15 15:41:04 PST 1994 By kalsow     *)

INTERFACE MxMap;

TYPE T     <: REFANY; (* Key -> Value *)
TYPE Key   = INTEGER;
TYPE Value = REFANY;

PROCEDURE New (initalSize: CARDINAL): T;
(* builds, initializes and returns a new empty map *)

PROCEDURE Get (t: T;  k: Key): Value;
(* returns the element of t with key value k, NIL if no such element *)

CONST MISSING = -1;
PROCEDURE GetIndex (t: T;  k: Key): INTEGER;
(* returns the index of the element of t with key value k,
   MISSING if no such element *)

PROCEDURE GetDirect (t: T;  index: INTEGER): Value;
(* returns the value of the element in t in slot index. *)

PROCEDURE Insert (t: T;  k: Key;  v: Value);
(* inserts (k, v) into t. *)

PROCEDURE Delete (t: T;  k: Key);
(* delete the entry of t with key value k *)

TYPE Binding  = RECORD key: Key := 0;  value: Value := NIL; END;
TYPE Contents = REF ARRAY OF Binding;

PROCEDURE GetData (t: T): Contents;
(* returns the internal hash table *)

END MxMap.

