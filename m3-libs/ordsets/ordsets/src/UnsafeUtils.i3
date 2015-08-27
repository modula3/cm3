  
(* -----------------------------------------------------------------------1- *)
(* File UnsafeUtils.i3  Modula-3 source code.                                *)
(* Copyright 2010 .. 2012, Rodney M. Bates.                                  *)
(* rbates@acm.org                                                            *)
(* Licensed under the Gnu Public License, version 2 or later.                *)
(* -----------------------------------------------------------------------2- *)

INTERFACE UnsafeUtils 

; IMPORT Word 

; PROCEDURE IntOfRefany ( Ref : REFANY ) : INTEGER 

; PROCEDURE RefanyOfInt ( I : INTEGER ) : REFANY 

; PROCEDURE NULLOfInt ( I : INTEGER ) : NULL

; PROCEDURE PtrTo8CharArray ( W : Word . T ) 
  : UNTRACED REF ARRAY [ 0 .. 7 ] OF CHAR

; TYPE TypeCodeTyp = CARDINAL 

; PROCEDURE ObjectSize ( TC : TypeCodeTyp ) : INTEGER 

; END UnsafeUtils 
. 
