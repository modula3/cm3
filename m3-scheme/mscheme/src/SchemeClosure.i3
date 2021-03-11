(* $Id$ *)


(*
  Copyright (c) 2008, Generation Capital Ltd.  All rights reserved.

  Author: Mika Nystrom <mika@alum.mit.edu>
*)
INTERFACE SchemeClosure;
IMPORT SchemeProcedure, SchemeEnvironment, SchemeSymbol;
IMPORT SchemeObject;

TYPE
  T <: Public;

  Public = SchemeProcedure.T OBJECT METHODS
    init(parms, body : SchemeObject.T; 
         env : SchemeEnvironment.Instance;
         bind := FALSE) : T;
  END;

CONST Brand = "SchemeClosure";

END SchemeClosure.
