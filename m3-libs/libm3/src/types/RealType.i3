(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* moved to m3core; compatibility *)

INTERFACE RealType;

IMPORT Real;

TYPE T = Real.T;
CONST Brand = Real.Brand;
CONST Equal = Real.Equal;
CONST Hash = Real.Hash;
CONST Compare = Real.Compare;

END RealType.
