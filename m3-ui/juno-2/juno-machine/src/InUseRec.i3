(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Oct 31 09:12:21 PST 1994 by heydon                   *)
(*      modified on Sun Jun  5 15:47:38 PDT 1994 by gnelson                  *)

INTERFACE InUseRec;

IMPORT RTVal;

CONST Brand = "InUseRec";

TYPE
  T = RECORD
    numInUse: RTVal.Number;
    textInUse: RTVal.Text;
    pairInUse: RTVal.Pair
  END;

END InUseRec.
