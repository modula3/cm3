(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Thu Jan  5 15:28:03 PST 1995 by najork                   *)
(*       Created on Wed Apr 27 15:57:28 PDT 1994 by najork                   *)


MODULE BinpackAux;

IMPORT IntList, RealList;

PROCEDURE IntListToText (<* UNUSED *> l : IntList.T) : TEXT =
  BEGIN
    RETURN "<an IntList>";
  END IntListToText;


PROCEDURE RealListToText (<* UNUSED *> l : RealList.T) : TEXT =
  BEGIN
    RETURN "<a RealList>";
  END RealListToText;


BEGIN
END BinpackAux.
