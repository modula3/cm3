(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: TypeTbl.i3                                            *)
(* Last modified on Fri Jul 29 13:49:37 PDT 1994 by kalsow     *)

INTERFACE TypeTbl;

IMPORT Type;

TYPE T <: REFANY;

PROCEDURE Put (VAR t: T;  key: Type.T;  value: REFANY): REFANY;
(* Add the pair "(key,value)" to "t" and return the previous value.
   If "t" is "NIL", create a fresh one. *)

PROCEDURE Get (t: T;  key: Type.T): REFANY;
(* If a pair "(u,v)" exists such that Type.IsEqual(u,key), return "v".
   Othewise, return NIL *)

PROCEDURE Reset (t: T);
(* Remove all entries from 't' *)

END TypeTbl.
