(* Copyright Rodney M. Bates 2016. *)
(* Licensed under the MIT License. *) 

(* Test various combinations involving mixes of fixed and open arrays,
   with matching shapes. 
*) 

MODULE Main 
; IMPORT IO 

; CONST C : ARRAY [ 0 .. 0 ] OF INTEGER 
    = ARRAY OF INTEGER { 7 } 

; CONST D : ARRAY [ 0 .. 3 ] OF INTEGER 
    = ARRAY OF INTEGER { 1 , 3 , 5 , 7 } 

; PROCEDURE Write 
    ( <* NOWARN *> F : ARRAY OF INTEGER ) 

  = VAR N , RI , J : INTEGER 
  ; BEGIN 
      N := NUMBER ( F )
    ; IO . PutInt ( N )  
    ; IO . Put ( " { " ) 
    ; IF N > 0 
      THEN
        RI := FIRST ( F )
      ; LOOP 
          IO . PutInt ( F [ RI ] )  
        ; IF RI = LAST ( F ) 
          THEN 
            IO . PutChar ( ' ' ) 
          ; EXIT 
          ELSE 
            IO . Put ( ", " ) 
          ; INC ( RI ) 
          END (* IF *) 
        END (* FOR *)  
      END (* IF *) 
    ; IO . Put ( "}\n") 
    ; J := N * 5 (* Just a convenient place to set a breakpoint. *) 
    END Write 

; PROCEDURE P 
    ( READONLY F : ARRAY OF INTEGER := ARRAY [ 0 .. 1 ] OF INTEGER { 9 , 11 } ) 
  = BEGIN
      Write ( F ) 
    END P 

; PROCEDURE Q ( ) 
  = BEGIN
      P ( ) 
    ; P ( ARRAY OF INTEGER { 13 } )
    ; P ( ARRAY BOOLEAN OF INTEGER { 14 , 15 } )
    END Q 

; PROCEDURE R 
    ( F : ARRAY [ 0 .. 1 ] OF INTEGER := ARRAY OF INTEGER { 17 , 19 } ) 
  = BEGIN
      Write ( F ) 
    END R 

; PROCEDURE S ( ) 
  = BEGIN
      R ( ) 
    ; R ( ARRAY OF INTEGER { 21 , 22 } )
    ; R ( ARRAY BOOLEAN OF INTEGER { 25 , 27 } )
    END S 

; BEGIN
    Write ( C ) 
  ; Write ( D ) 
  ; Write ( ARRAY OF INTEGER { } ) 
  ; Q ( )
  ; S ( )  
  END Main
. 

