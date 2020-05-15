(* ----------------------------------------------------------------------1- *)
(* File Main.m3 for Modula3 compiler test p280                              *)
(* Copyright 2020, Rodney M. Bates.                                         *)
(* rodney.m.bates@acm.org                                                   *)
(* Licensed under the MIT license.                                          *)
(* ----------------------------------------------------------------------2- *)

(* Tests of parameters with SET, ARRAY, and RECORD types. *)

MODULE Main

; IMPORT Common 

; IMPORT TestEnum 
; IMPORT TestFloat 
; IMPORT TestRef 
; IMPORT TestProc 
; IMPORT TestSmallSet 
; IMPORT TestSet 
; IMPORT TestRecord 
; IMPORT TestSmallArray 
; IMPORT TestArray

; IMPORT TestArrayMO_0_0 
; IMPORT TestArrayMO_0_1 
; IMPORT TestArrayMO_0_2 
; IMPORT TestArrayMO_0_3 

; IMPORT TestArrayMO_1_0 
; IMPORT TestArrayMO_1_1 
; IMPORT TestArrayMO_1_2 
; IMPORT TestArrayMO_1_3 

; IMPORT TestArrayMO_2_0 
; IMPORT TestArrayMO_2_1 
; IMPORT TestArrayMO_2_2 
; IMPORT TestArrayMO_2_3 

; IMPORT TestArrayMO_3_0 
; IMPORT TestArrayMO_3_1 
; IMPORT TestArrayMO_3_2 
; IMPORT TestArrayMO_3_3 

; PROCEDURE PVOA ( VAR FVOA : ARRAY OF CHAR )
  = VAR C : CHAR
  ; VAR Bkpt : INTEGER 
  ; BEGIN
      C := FVOA [ 0 ]
    ; Bkpt := 11 
    END PVOA

; PROCEDURE CVOA ( )
  = VAR Buff : ARRAY [ 0 .. 17 ] OF CHAR
  ; BEGIN
      PVOA ( Buff ) 
    END CVOA 

; BEGIN
    Common . Init ( ) 
  (* ; UnsafeUtils . Init ( ) *)
(* This is just to test Common.ArrayImage3D. 
  ; Common . Message ( Common . ArrayImage3D ( Common . Array3DVal ) )
*) 
  
  ; TestEnum . TestAll ( ) 
  ; TestFloat . TestAll ( ) 
  ; TestRef . TestAll ( ) 
  ; TestProc . TestAll ( ) 
  ; TestSmallSet . TestAll ( ) 
  ; TestSet . TestAll ( ) 
  ; TestRecord . TestAll ( ) 
  ; TestSmallArray . TestAll ( ) 
  ; TestArray . TestAll ( )

  ; TestArrayMO_0_0 . TestAll ( )
  ; TestArrayMO_0_1 . TestAll ( )
  ; TestArrayMO_0_2 . TestAll ( )
  ; TestArrayMO_0_3 . TestAll ( )

  ; TestArrayMO_1_0 . TestAll ( )
  ; TestArrayMO_1_1 . TestAll ( )
  ; TestArrayMO_1_2 . TestAll ( )
  ; TestArrayMO_1_3 . TestAll ( )

  ; TestArrayMO_2_0 . TestAll ( )
  ; TestArrayMO_2_1 . TestAll ( )
  ; TestArrayMO_2_2 . TestAll ( )
  ; TestArrayMO_2_3 . TestAll ( )

  ; TestArrayMO_3_0 . TestAll ( )
  ; TestArrayMO_3_1 . TestAll ( )
  ; TestArrayMO_3_2 . TestAll ( )
  ; TestArrayMO_3_3 . TestAll ( )

  ; CVOA ( )
  ; Common . Report ( ) 
  END Main
.

