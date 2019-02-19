(* $Id$ *)

(*
  Copyright (c) 2008, Generation Capital Ltd.  All rights reserved.

  Author: Mika Nystrom <mika@alum.mit.edu>
*)

INTERFACE SchemeEnvironmentSuper;
IMPORT NetObj;

(* this interface exists solely to make Scheme.i3 not have circular imports *)

TYPE T = NetObj.T;

END SchemeEnvironmentSuper.
