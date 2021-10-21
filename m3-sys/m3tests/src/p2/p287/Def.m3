(* ----------------------------------------------------------------------1- *)
(* File Def.m3 for Modula3 compiler test p287                               *)
(* Copyright 2021, Rodney M. Bates.                                         *)
(* rodney.m.bates@acm.org                                                   *)
(* Licensed under the MIT license.                                          *)
(* ----------------------------------------------------------------------2- *)

MODULE Def

; IMPORT Wr 

; IMPORT Display 
; IMPORT Named

; PROCEDURE LF
    ( READONLY LFForm := LArrFC )
    = BEGIN
        Display . Put ( "  Default value named in exported interface Def, fixed array:" )
      ; Display . Put ( Wr . EOL ) 
      ; Display . F ( LFForm )
      END LF 

; PROCEDURE NF
    ( READONLY NFForm := Named . ArrFC )
    = BEGIN
        Display . Put ( "  Default value named in imported interface Named, fixed array:" )
      ; Display . Put ( Wr . EOL ) 
      ; Display . F ( NFForm )
      END NF 

; PROCEDURE NO
    ( READONLY NOForm := Named . ArrOC )
    = BEGIN
        Display . Put ( "  Default value named in imported interface Named, open array:" ) 
      ; Display . Put ( Wr . EOL ) 
      ; Display . O ( NOForm )
      END NO 

; PROCEDURE NR
    ( READONLY NRForm := Named . RecC ) 
    = BEGIN
        Display . Put ( "  Default value named in imported interface Named, record:" ) 
      ; Display . Put ( Wr . EOL ) 
      ; Display . R ( NRForm )
      END NR 

; PROCEDURE NS
    ( READONLY NSForm := Named . SetC ) 
    = BEGIN
        Display . Put ( "  Default value named in imported interface Named, set:" ) 
      ; Display . Put ( Wr . EOL ) 
      ; Display . S ( NSForm )
      END NS 

; PROCEDURE AF
    ( READONLY AFForm := ARRAY [ 0 .. 2 ] OF INTEGER { 1 , 10 , 19 } )  
    = VAR Break : INTEGER := 13
    ; BEGIN
        Display . Put ( "  Default value in module, fixed array:" ) 
      ; Display . Put ( Wr . EOL )
      ; Break := 15 
      ; Display . F ( AFForm )
      END AF
    
; PROCEDURE AO
    ( READONLY AOForm := ARRAY OF INTEGER { 1 , 12 , 23 , 34 , 45 } ) 
    = BEGIN
        Display . Put ( "  Default value in module, open array:" ) 
      ; Display . Put ( Wr . EOL ) 
      ; Display . O ( AOForm )
      END AO 

; PROCEDURE AR
    ( READONLY ARForm := RECORD Fld0 , Fld1 , Fld2 : INTEGER END { 2 , 7 , 12 } ) 
    = BEGIN
        Display . Put ( "  Default value in module, record:" ) 
      ; Display . Put ( Wr . EOL ) 
      ; Display . R ( ARForm )
      END AR 

; PROCEDURE ASs
    ( READONLY ASForm := SET OF Display . Rng { 17 , 97 , 129 } ) 
    = BEGIN
        Display . Put ( "  Default value in module, set:" ) 
      ; Display . Put ( Wr . EOL ) 
      ; Display . S ( ASForm )
      END ASs

; PROCEDURE InModuleCalls ( ) 
    = BEGIN 
        Display . Put ( "Calls from Def.m3" ) 
      ; Display . Put ( Wr . EOL ) 
      ; LF ( ) 

      ; NF ( ) 
      ; NO ( ) 
      ; NR ( ) 
      ; NS ( ) 

      ; AF ( ) 
      ; AO ( ) 
      ; AR ( ) 
      ; ASs ( )
    
      ; Display . Put ( Wr . EOL ) 
      END InModuleCalls 

; BEGIN
  END Def
.

