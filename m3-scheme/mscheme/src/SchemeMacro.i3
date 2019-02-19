(* $Id$ *)

(*
  Copyright (c) 2008, Generation Capital Ltd.  All rights reserved.

  Author: Mika Nystrom <mika@alum.mit.edu>
*)

INTERFACE SchemeMacro;
IMPORT SchemeClosure, Scheme;
FROM Scheme IMPORT Object, E;
IMPORT SchemePair;

TYPE Pair = SchemePair.T;

TYPE
  T <: Public;

  Public = SchemeClosure.T OBJECT METHODS
    expand(interpreter : Scheme.T; oldPair : Pair; args : Object) : Pair RAISES { E } ;
  END;

PROCEDURE MacroExpand(interpreter : Scheme.T; x : Object) : Object RAISES { E };

CONST Brand = "SchemeMacro";

END SchemeMacro.
