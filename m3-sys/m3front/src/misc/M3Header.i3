(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: M3Header.i3                                           *)
(* Last modified on Mon Jul 11 11:51:40 PDT 1994 by kalsow     *)

INTERFACE M3Header;

IMPORT M3Front;

TYPE IDList = M3Front.IDList;

PROCEDURE Parse (): IDList;

END M3Header.
