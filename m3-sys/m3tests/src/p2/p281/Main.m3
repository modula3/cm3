(* Const Array Decl Bugs (#25), reported by darko20 on 03/24/2018. *)

MODULE Main

; IMPORT Test 

; CONST A1 = ARRAY OF ARRAY OF INTEGER { ARRAY OF INTEGER { 33876 } }

; CONST A2 = ARRAY OF ARRAY OF INTEGER { ARRAY OF INTEGER { } }

; TYPE R = RECORD A := 27784 END

; CONST B1 = ARRAY OF R { R { } }
; CONST C1 = B1 [ 0 ] . A 

; CONST B2 = ARRAY OF R { R { 19902 } }
; CONST C2 = B2 [ 0 ] . A 

; BEGIN
    Test . checkI ( A1 [ 0 , 0 ] , 33876 )
  ; Test . checkI ( NUMBER ( A2 ) , 1 ) 
  ; Test . checkI ( NUMBER ( A2 [ 0 ] ) , 0 ) 
  ; Test . checkI ( C1 , 27784 ) 
  ; Test . checkI ( C2 , 19902 )

  ; Test . done ( ) 
  END Main
.

