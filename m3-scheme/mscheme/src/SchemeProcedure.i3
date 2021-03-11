(* $Id$ *)

(*
  Copyright (c) 2008, Generation Capital Ltd.  All rights reserved.

  Author: Mika Nystrom <mika@alum.mit.edu>
*)

INTERFACE SchemeProcedure;
IMPORT Scheme; 
FROM Scheme IMPORT Object, E;

TYPE
  T <: Public;

  Public = OBJECT METHODS
    format() : TEXT;

    apply(interp : Scheme.T; args : Object) : Object RAISES { E }; 
    (* abstract *)

    (* optimizations, standard one just calls apply above 
       Please see a comment inside Scheme.m3: Scheme.Eval to 
       explain what this is about; also see the implementation
       in SchemePrimitive.m3.
    *)
    apply2(interp : Scheme.T; a1, a2 : Object) : Object RAISES { E };

    apply1(interp : Scheme.T; a1 : Object) : Object RAISES { E };
  END;

CONST Brand = "SchemeProcedure";

PROCEDURE Proc(x : Object) : T RAISES { E }; 

END SchemeProcedure.
    
    
