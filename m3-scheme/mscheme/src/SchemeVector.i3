(* $Id$ *)

(*
  Copyright (c) 2008, Generation Capital Ltd.  All rights reserved.

  Author: Mika Nystrom <mika@alum.mit.edu>
*)

INTERFACE SchemeVector;
IMPORT SchemeObject;

TYPE T = REF ARRAY OF SchemeObject.T;

CONST Brand = "SchemeVector";

END SchemeVector.
