(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Thu Apr 28 09:52:02 PDT 1994 by najork                   *)
(*       Created on Wed Apr 27 15:57:28 PDT 1994 by najork                   *)


INTERFACE BinpackAux;

IMPORT IntList, RealList;

PROCEDURE IntListToText (l : IntList.T) : TEXT;
PROCEDURE RealListToText (l : RealList.T) : TEXT;

END BinpackAux.
