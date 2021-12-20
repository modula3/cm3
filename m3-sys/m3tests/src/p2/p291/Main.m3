(* ----------------------------------------------------------------------1- *)
(* File Main.m3 for Modula3 compiler test p291                              *)
(* Copyright 2021, Rodney M. Bates.                                         *)
(* rodney.m.bates@acm.org                                                   *)
(* Licensed under the MIT license.                                          *)
(* ----------------------------------------------------------------------2- *)

(* Test for a bug wherein RecordExpr.Evaluate was folding record fields
   in place even when the entire record constructor is not constant.
   When the folded field is a constant array constructor, this stripped
   away the Constant.T from in front of it.  Then when the array constructor
   is in a different unit, that undermined the ability of runtime code
   to refer to it, when building the containing, non-constant record
   constructor.
*)

MODULE Main

; IMPORT RTIO
; IMPORT Wr 

; IMPORT TInttt

; TYPE TIntttN = RECORD
                 n : CARDINAL
               ; x : TInttt . Int
               END

; VAR FailureCt : INTEGER := 0 

; PROCEDURE WL ( T : TEXT )
  = BEGIN 
      RTIO . PutText ( T ) 
    ; RTIO . PutText ( Wr . EOL ) 
    ; RTIO . Flush ( )
    END WL 


; PROCEDURE CheckInt ( act , exp : INTEGER )
  = BEGIN
      RTIO . PutText ( "  " ) 
    ; IF act = exp
      THEN
        RTIO . PutInt ( act ) 
      ELSE
        RTIO . PutText ( "Got: " ) 
      ; RTIO . PutInt ( act ) 
      ; RTIO . PutText ( ", but expected: " ) 
      ; RTIO . PutInt ( exp ) 
      ; INC ( FailureCt ) 
      END
    ; RTIO . PutText ( Wr . EOL ) 
    ; RTIO . Flush ( )
    END CheckInt 

; PROCEDURE Accept ( READONLY Form2 : TInttt . Int ; Lo : INTEGER )
  = VAR LExp := Lo 
  ; BEGIN
      FOR FI := FIRST ( TInttt . Int ) TO LAST ( TInttt . Int )
      DO
        CheckInt ( Form2 [ FI ] , LExp )
      ; INC ( LExp ) 
      END (*FOR*) 
    END Accept

; PROCEDURE AcceptInRec ( READONLY Form3 : TIntttN ; Lo : INTEGER )
  = VAR LExp := Lo 
  ; BEGIN
      FOR FI := FIRST ( TInttt . Int ) TO LAST ( TInttt . Int )
      DO
        CheckInt ( Form3 . x [ FI ] , LExp )
      ; INC ( LExp ) 
      END (*FOR*) 
    END AcceptInRec 

; PROCEDURE Work ( ) 
  = VAR four : CARDINAL := 4
  ; BEGIN
      WL ( "Val2:" ) 
    ; Accept ( TInttt . Val2 , TInttt . Lo2 )
    
    ; WL ( "Val3, inside a record, with keywords:" ) 
    ; AcceptInRec ( TIntttN { n := four , x := TInttt . Val3 } , TInttt . Lo3 )
    
    ; WL ( "Val4, inside a record, no keywords:" ) 
    ; AcceptInRec ( TIntttN { four , TInttt . Val4 } , TInttt . Lo4 )
    
    ; IF FailureCt <= 0
      THEN
        RTIO . PutText ( "All as expected." ) 
      ELSE
        RTIO . PutInt ( FailureCt ) 
      ; RTIO . PutText ( " values not as expected." ) 
      END (*IF*)
    ; RTIO . PutText ( Wr . EOL ) 
    ; RTIO . Flush ( )
    END Work 

; BEGIN
    Work ( ) 
  END Main
.

 
