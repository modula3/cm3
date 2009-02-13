
(* -----------------------------------------------------------------------1- *)
(* Copyright 2007, Rodney M. Bates.                                          *)
(* rodney.bates@wichita.edu                                                  *)
(* Licensed under the Gnu Public License, version 2 or later.                *)
(* -----------------------------------------------------------------------2- *)

INTERFACE UnsafeUtils 

; PROCEDURE IntOfRefany ( Ref : REFANY ) : INTEGER 

; TYPE TypeCodeTyp = CARDINAL 

; PROCEDURE ObjectSize ( TC : TypeCodeTyp ) : INTEGER 

; END UnsafeUtils 
. 
