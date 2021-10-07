(* ----------------------------------------------------------------------1- *)
(* File Display.m3 for Modula3 compiler test p288                           *)
(* Copyright 2021, Rodney M. Bates.                                         *)
(* rodney.m.bates@acm.org                                                   *)
(* Licensed under the MIT license.                                          *)
(* ----------------------------------------------------------------------2- *)

MODULE Display

; IMPORT Fmt
; IMPORT Stdio
; IMPORT Thread 
; IMPORT Wr

; VAR WrT : Wr . T

; CONST Prefix = "    "

; PROCEDURE Put ( Text : TEXT )
    = <* FATAL Thread . Alerted , Wr . Failure *>
      BEGIN
        Wr . PutText ( WrT , Text ) 
      END Put

; PROCEDURE PutLine ( Text : TEXT )
  = BEGIN
      Put ( Text ) 
    ; Put ( Wr . EOL ) 
    END PutLine 

; PROCEDURE A ( READONLY DFormF : ArrT )
    = BEGIN 
        Put ( Prefix )
      ; Put ( Fmt . Int ( DFormF [ 0 ] ) ) 
      ; Put ( ", " ) 
      ; Put ( Fmt . Int ( DFormF [ 1 ] ) ) 
      ; Put ( ", " ) 
      ; Put ( Fmt . Int ( DFormF [ 2 ] ) ) 
      ; Put ( Wr . EOL ) 
      END A

; PROCEDURE R ( READONLY DFormR : RecT ) 
    = BEGIN
        Put ( Prefix )
      ; Put ( Fmt . Int ( DFormR .Fld0 ) ) 
      ; Put ( ", " ) 
      ; Put ( Fmt . Int ( DFormR .Fld1 ) ) 
      ; Put ( ", " ) 
      ; Put ( Fmt . Int ( DFormR .Fld2 ) ) 
      ; Put ( Wr . EOL ) 
      END R
      
; PROCEDURE S ( READONLY DFormS : SetT ) 
    = VAR Seen : BOOLEAN
    ; BEGIN
        Put ( Prefix )
      ; Seen := FALSE 
      ; FOR RI := FIRST ( Rng ) TO LAST ( Rng )
        DO
          IF RI IN DFormS
          THEN
            IF Seen
            THEN Put ( ", " )
            ELSE Seen := TRUE 
            END
          ; Put ( Fmt . Int ( RI ) )
          END (* IF *) 
        END (* FOR *) 
      ; Put ( Wr . EOL ) 
      END S

; PROCEDURE Rec ( VAR Outer : OuterRecT )
  = BEGIN
      A ( Outer . A ) 
    ; R ( Outer . R ) 
    ; S ( Outer . S ) 
    END Rec 

; BEGIN
    WrT := Stdio . stdout 
  END Display
.


