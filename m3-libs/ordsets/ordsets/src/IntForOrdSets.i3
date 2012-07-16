  
(* -----------------------------------------------------------------------1- *)
(* File InfForOrdSets.ig  Modula-3 source code.                              *)
(* Copyright 2010 .. 2012, Rodney M. Bates.                                  *)
(* rbates@acm.org                                                            *)
(* Licensed under the Gnu Public License, version 2 or later.                *)
(* -----------------------------------------------------------------------2- *)

INTERFACE IntForOrdSets 

; TYPE T = INTEGER 

; CONST NullElem = FIRST ( T ) 

; TYPE ValidElemT = [ NullElem + 1 .. LAST ( T ) ]  

; CONST Brand = "Integer"

; END IntForOrdSets 
. 
