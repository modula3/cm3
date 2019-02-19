(* $Id$ *)

(*
  Copyright (c) 2008, Generation Capital Ltd.  All rights reserved.

  Author: Mika Nystrom <mika@alum.mit.edu>
*)

INTERFACE SchemeClosureClass;
IMPORT SchemeClosure;
IMPORT SchemeObject, SchemeEnvironment;

REVEAL
  SchemeClosure.T <: Private;

TYPE
  Private = SchemeClosure.Public OBJECT
    params, body : SchemeObject.T;
    env          : SchemeEnvironment.Instance;
  END;

END SchemeClosureClass.
