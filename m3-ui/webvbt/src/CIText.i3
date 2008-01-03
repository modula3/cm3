(* Copyright (C) 1995, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Jul  4 09:59:07 PDT 1995 by mhb                      *)

INTERFACE CIText;

IMPORT Word;

CONST 
  Brand = "CIText";

TYPE
  T = TEXT;

PROCEDURE Equal(k1, k2: T): BOOLEAN;

PROCEDURE Hash(k1: T): Word.T;

END CIText.
