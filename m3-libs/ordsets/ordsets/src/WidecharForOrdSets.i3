  
(* -----------------------------------------------------------------------1- *)
(* File WidecharForOrdSets.i3  Modula-3 source code.                         *)
(* Copyright 2010 .. 2012, Rodney M. Bates.                                  *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *) 
(* -----------------------------------------------------------------------2- *)

INTERFACE WidecharForOrdSets 

; TYPE T = WIDECHAR

; CONST NullElem = FIRST ( T ) 
; CONST FirstValid = VAL ( ORD ( NullElem ) + 1 , T ) 

; TYPE ValidElemT = [ FirstValid .. LAST ( T ) ]  

; CONST Brand = "Widechar"

; END WidecharForOrdSets 
. 
