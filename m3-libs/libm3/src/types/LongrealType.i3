(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* moved to m3core; compatibility *)

INTERFACE LongrealType;

IMPORT LongReal;

TYPE T = LongReal.T;
CONST Brand = LongReal.Brand;
CONST Equal = LongReal.Equal;
CONST Hash = LongReal.Hash;
CONST Compare = LongReal.Compare;

END LongrealType.
