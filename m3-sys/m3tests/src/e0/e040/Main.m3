MODULE Main

; TYPE Arr = ARRAY [ 0 .. 5 ] OF CHAR

; PROCEDURE Q ( QF : ARRAY OF CHAR )

  = BEGIN
      EVAL QF
    END Q

; PROCEDURE P ( PF : Arr )

  = BEGIN
      Q ( SUBARRAY ( 'Y' , 2   , 4   ) )  
    ; Q ( SUBARRAY ( PF  , 'Z' , 4   ) ) 
    ; Q ( SUBARRAY ( PF  , 2   , 'X' ) ) 
    ; Q ( SUBARRAY ( 'Y' , 'Z' , 'X' ) ) 
    END P

; BEGIN
    P ( Arr { 'A' , 'B' , 'C' , 'D' , 'E' , .. } ) 
  END Main
.
