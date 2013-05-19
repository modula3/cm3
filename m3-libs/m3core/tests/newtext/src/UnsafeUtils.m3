
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

(* How many of these even need to be UNSAFE? *) 

(* VISIBLE: *) 
; PROCEDURE RefSize ( TC : TypeCodeTyp ) : INTEGER 
  (* Does not include method table 
     (which evey subtype of {UNTRACED} ROOT has)
     Nor shape (which every heap-allocated open array has.
  *)  

  = BEGIN 
      RETURN
        RTType . Get ( TC ) . dataSize (* DIV BitsPerAddrUnit *) 
        + ADRSIZE ( RTHeapRep . Header ) 
    END RefSize 

(* VISIBLE: *) 
; PROCEDURE ObjectSize ( TC : TypeCodeTyp ) : INTEGER 
  (* Assumes TC is for an OBJECT type. *) 

  = BEGIN 
      RETURN
        RefSize ( TC ) + ADRSIZE ( ADDRESS ) 
    END ObjectSize 

(* VISIBLE: *) 
; PROCEDURE OpenArraySize ( TC : TypeCodeTyp ; DimensionCt : CARDINAL ) 
  : INTEGER 
  (* Assumes TC is for REF some open array. *) 

  = BEGIN 
      RETURN
        RefSize ( TC ) 
        + ADRSIZE ( ADDRESS ) 
        + DimensionCt * ADRSIZE ( CARDINAL ) 
    END OpenArraySize 

; BEGIN (* UnsafeUtils *) 
  END UnsafeUtils 
. 
