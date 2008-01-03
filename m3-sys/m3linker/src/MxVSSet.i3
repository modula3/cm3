(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: MxVSSet.i3                                            *)
(* Last Modified On Mon Aug  1 14:56:13 PDT 1994 By kalsow     *)

INTERFACE MxVSSet;

IMPORT MxVS;

TYPE T   <: REFANY;
TYPE Elt = MxVS.T;
 

PROCEDURE New (initalSize: CARDINAL): T;
(* builds, initializes and returns a new empty map *)

PROCEDURE Get (t: T;  elt: Elt): Elt;
(* returns the element of 't' with the same name as 'elt',
   MxVS.NoVS if no such element. *)

PROCEDURE Insert (t: T;  elt: Elt);
(* inserts 'elt' into 't'. *)

TYPE Contents = REF ARRAY OF Elt;
PROCEDURE GetData (t: T): Contents;
(* returns the internal hash table *)

END MxVSSet.

