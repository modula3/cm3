(* Copyright (C) 1993, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Jan  9 12:14:58 PST 1995 by najork                   *)
(*      modified on Wed Jul 21 13:03:27 PDT 1993 by mann                     *)
(*      modified on Mon Jul 19 11:26:38 PDT 1993 by perl                     *)

INTERFACE MiscFmt;

PROCEDURE Char(c:  CHAR): TEXT;
(* Format c as an obliq char constant *)

TYPE RefIntArray = REF ARRAY OF INTEGER;

PROCEDURE IntArray(ia:  RefIntArray): TEXT;
(* Format ia as an obliq array of integers constant *)

TYPE RefTextArray = REF ARRAY OF TEXT;

PROCEDURE TextArray(ta:  RefTextArray): TEXT;
(* Format ta as an obliq array of text constant *)

END MiscFmt.
