MODULE Main
; IMPORT Test
; TYPE XX = BITS 6 FOR [0..5]
; TYPE YY = ARRAY [2..6] OF XX
; TYPE ZZ = RECORD F2 , F3 , F4 , F5 , F6 : XX END

; VAR GYY := YY { 0 , 1 , 2 , 4 , 5 }
; VAR GZZ := ZZ { 5 , 1 , 2 , 3 , 4 } 

; PROCEDURE P ( FYY : YY ; FZZ : ZZ )
  = VAR VYY : YY
  ; VAR VZZ : ZZ
  ; VAR Breakpoint : INTEGER 
  ; BEGIN
      VYY := FYY
    ; VZZ := FZZ
    
    ; Test.checkI ( FYY [ 2 ] , 0 )
    ; Test.checkI ( FYY [ 3 ] , 1 )
    ; Test.checkI ( FYY [ 4 ] , 2 )
    ; Test.checkI ( FYY [ 5 ] , 4 )
    ; Test.checkI ( FYY [ 6 ] , 5 )

    ; Test.checkI ( FZZ . F2 , 5 )
    ; Test.checkI ( FZZ . F3 , 1 )
    ; Test.checkI ( FZZ . F4 , 2 )
    ; Test.checkI ( FZZ . F5 , 3 )
    ; Test.checkI ( FZZ . F6 , 4 )

    ; Breakpoint := 13
    
    ; Test.checkI ( VYY [ 2 ] , 0 )
    ; Test.checkI ( VYY [ 3 ] , 1 )
    ; Test.checkI ( VYY [ 4 ] , 2 )
    ; Test.checkI ( VYY [ 5 ] , 4 )
    ; Test.checkI ( VYY [ 6 ] , 5 )

    ; Test.checkI ( VZZ . F2 , 5 )
    ; Test.checkI ( VZZ . F3 , 1 )
    ; Test.checkI ( VZZ . F4 , 2 )
    ; Test.checkI ( VZZ . F5 , 3 )
    ; Test.checkI ( VZZ . F6 , 4 )

    ; Breakpoint := 19 

    END P
    
; BEGIN
    P ( GYY , GZZ )
  ; Test . done ( ) 
  END Main
.



