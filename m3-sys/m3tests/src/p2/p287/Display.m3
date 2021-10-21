(* ----------------------------------------------------------------------1- *)
(* File Display.m3 for Modula3 compiler test p287                           *)
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

; PROCEDURE F ( READONLY DFormF : ArrFT )
    = BEGIN 
        Put ( Prefix )
      ; Put ( Fmt . Int ( DFormF [ 0 ] ) ) 
      ; Put ( ", " ) 
      ; Put ( Fmt . Int ( DFormF [ 1 ] ) ) 
      ; Put ( ", " ) 
      ; Put ( Fmt . Int ( DFormF [ 2 ] ) ) 
      ; Put ( Wr . EOL ) 
      END F

; VAR MaxElts := 6 
      
; PROCEDURE O ( READONLY DFormO : ArrOT ) 
    = VAR I , EltCt , Number : INTEGER
    ; BEGIN
        Put ( Prefix )
      ; Number := NUMBER ( DFormO )
      ; I := FIRST ( DFormO ) 
      ; Put ( "NUMBER=[" ) 
      ; Put ( Fmt . Int ( Number ) )  
      ; Put ( "]" ) 
      ; EltCt := 0
      ; IF Number > 8 
        THEN
        (* This is tricky to test.  In a once-failing case, NUMBER and
           LAST were ridiculously high, should have been 0 and <FIRST),
           yet DFormO[0] segfaulted. *)
          EltCt := - 1 (* Just to set a breakpoint. *) 
        ELSE  
          LOOP 
            IF EltCt >= Number THEN EXIT END 
          ; IF EltCt >= MaxElts 
            THEN
              Put ( ", ..." ) 
            ; EXIT 
            END 
          ; Put ( ", " )
          ; Put ( Fmt . Int ( DFormO [ I ] ) ) 
          ; INC ( EltCt ) 
          ; INC ( I ) 
          END  (* LOOP *)
        END 
      ; Put ( Wr . EOL ) 
      END O
      
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

; BEGIN
    WrT := Stdio . stdout 
  END Display
.


