  
(* -----------------------------------------------------------------------1- *)
(* File UnsafeUtils.m3  Modula-3 source code.                                *)
(* Copyright 2010 .. 2012, Rodney M. Bates.                                  *)
(* rbates@acm.org                                                            *)
(* Licensed under the Gnu Public License, version 2 or later.                *)
(* -----------------------------------------------------------------------2- *)

UNSAFE MODULE UnsafeUtils 

(* From m3core: *) 
; IMPORT RTHeapRep 
; IMPORT RTType 
; IMPORT Word 
 
(* VISIBLE: *) 
; PROCEDURE IntOfRefany ( Ref : REFANY ) : INTEGER 

  = BEGIN (* IntOfRefany *) 
      RETURN LOOPHOLE ( Ref , INTEGER ) 
    END IntOfRefany  

(* VISIBLE: *) 
; PROCEDURE RefanyOfInt ( I : INTEGER ) : REFANY 

  = BEGIN (* RefanyOfInt *) 
      RETURN LOOPHOLE ( I , REFANY ) 
    END RefanyOfInt   

(* VISIBLE: *) 
; PROCEDURE NULLOfInt ( I : INTEGER ) : NULL

  = BEGIN (* RefanyOfInt *) 
      RETURN LOOPHOLE ( I , NULL ) 
    END NULLOfInt   

(* VISIBLE: *) 
; PROCEDURE PtrTo8CharArray ( W : Word . T ) 
  : UNTRACED REF ARRAY [ 0 .. 7 ] OF CHAR 

  = BEGIN 
      RETURN ( LOOPHOLE ( ADR ( W ) , UNTRACED REF ARRAY [ 0 .. 7 ] OF CHAR ) )
    END PtrTo8CharArray 

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
