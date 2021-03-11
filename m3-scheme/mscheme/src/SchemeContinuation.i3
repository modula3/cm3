(* $Id$ *)

(*
  Copyright (c) 2008, Generation Capital Ltd.  All rights reserved.

  Author: Mika Nystrom <mika@alum.mit.edu>
*)

INTERFACE SchemeContinuation;
IMPORT SchemeProcedure;
FROM Scheme IMPORT Object;

TYPE 
  T <: Public;

  Public = SchemeProcedure.T OBJECT
    cc : TEXT := NIL; (* the unique identifier passed thru the E *)
    value : Object := NIL;
  METHODS
    init(cc : TEXT) : T;
  END;

CONST Brand = "SchemeContinuation";

END SchemeContinuation.

