(* $Id$ *)

MODULE UseFortran; 
IMPORT Env;

PROCEDURE True() : BOOLEAN = BEGIN RETURN Env.Get("FORTRANMATH") # NIL END True;

BEGIN END UseFortran.
