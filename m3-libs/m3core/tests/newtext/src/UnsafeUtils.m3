
(* -----------------------------------------------------------------------1- *)
(* Copyright 2007, Rodney M. Bates.                                          *)
(* rodney.bates@wichita.edu                                                  *)
(* Licensed under the Gnu Public License, version 2 or later.                *)
(* -----------------------------------------------------------------------2- *)

UNSAFE MODULE UnsafeUtils 

(* From m3core: *) 
; IMPORT RTHeapRep 
; IMPORT RTType 
 
(* VISIBLE: *) 
; PROCEDURE IntOfRefany ( Ref : REFANY ) : INTEGER 

  = BEGIN (* IntOfRefany *) 
      RETURN LOOPHOLE ( Ref , INTEGER ) 
    END IntOfRefany  

(* VISIBLE: *) 
; PROCEDURE ObjectSize ( TC : TypeCodeTyp ) : INTEGER 

  = BEGIN 
      RETURN
        RTType . Get ( TC ) . dataSize (* DIV BitsPerAddrUnit *) 
        + ADRSIZE ( RTHeapRep . Header ) 
    END ObjectSize 

; BEGIN (* UnsafeUtils *) 
  END UnsafeUtils 
. 
