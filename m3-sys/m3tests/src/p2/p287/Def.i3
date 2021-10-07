(* ----------------------------------------------------------------------1- *)
(* File Def.i3 for Modula3 compiler test p287                               *)
(* Copyright 2021, Rodney M. Bates.                                         *)
(* rodney.m.bates@acm.org                                                   *)
(* Licensed under the MIT license.                                          *)
(* ----------------------------------------------------------------------2- *)

INTERFACE Def

; IMPORT Display 
; IMPORT Named 

; CONST LArrFC = ARRAY [ 0 .. 2 ] OF INTEGER { 1 , 4 , 7 } 

; PROCEDURE LF ( READONLY LFForm := LArrFC )

; PROCEDURE NF ( READONLY NFForm := Named . ArrFC ) 
; PROCEDURE NO ( READONLY NOForm := Named . ArrOC ) 

; PROCEDURE NR ( READONLY NRForm := Named . RecC ) 

; PROCEDURE NS ( READONLY NSForm := Named . SetC ) 

; PROCEDURE AF ( READONLY AFForm := ARRAY [ 0 .. 2 ] OF INTEGER { 1 , 6 , 11 } ) 
; PROCEDURE AO ( READONLY AOForm := ARRAY OF INTEGER { 1 , 8 , 15 , 22 } ) 

; PROCEDURE AR ( READONLY ARForm := RECORD Fld0 , Fld1 , Fld2 : INTEGER END { 2 , 5 , 8 } ) 

; PROCEDURE ASs ( READONLY ASForm := SET OF Display . Rng { 16 , 96 , 128 } ) 

; PROCEDURE InModuleCalls ( )

; VAR Forzzz : INTEGER 

; END Def
.

