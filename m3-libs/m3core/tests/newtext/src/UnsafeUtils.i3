
(* -----------------------------------------------------------------------1- *)
(* Copyright 2007, Rodney M. Bates.                                          *)
(* rodney.bates@wichita.edu                                                  *)
(* Licensed under the Gnu Public License, version 2 or later.                *)
(* -----------------------------------------------------------------------2- *)

INTERFACE UnsafeUtils 

; PROCEDURE IntOfRefany ( Ref : REFANY ) : INTEGER 

; TYPE TypeCodeTyp = CARDINAL 

; PROCEDURE RefSize ( TC : TypeCodeTyp ) : INTEGER 
  (* Does not include method table 
     (which evey subtype of {UNTRACED} ROOT has)
     Nor shape (which every heap-allocated open array has.
  *)  

; PROCEDURE ObjectSize ( TC : TypeCodeTyp ) : INTEGER 
  (* Assumes TC is for an OBJECT type. *) 

; PROCEDURE OpenArraySize ( TC : TypeCodeTyp ; DimensionCt : CARDINAL ) 
  : INTEGER 
  (* Assumes TC is for REF some open array. *) 

; END UnsafeUtils 
. 
