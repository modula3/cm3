(* ----------------------------------------------------------------------1- *)
(* File Support.i3 for Modula3 compiler test p269                           *)
(* Copyright 2019, Rodney M. Bates.                                         *)
(* rodney.m.bates@acm.org                                                   *)
(* Licensed under the MIT license.                                          *)
(* ----------------------------------------------------------------------2- *)

INTERFACE Support

; IMPORT Wr

; VAR WrT : Wr . T 

; PROCEDURE WEOL ( )

; PROCEDURE WL ( T : TEXT )

; PROCEDURE W ( String : TEXT )

; PROCEDURE Reset ( )

; PROCEDURE StartProg ( ProgLabel := "" )

; PROCEDURE StartTest ( Label : TEXT := "" ) 

; PROCEDURE Note ( Explanation : TEXT := "" )  

; PROCEDURE NoteFailure ( Explanation : TEXT := "" )  

; PROCEDURE EndTest ( Display := FALSE )

; PROCEDURE Flush ( ) 

; TYPE ProcTyp = PROCEDURE ( ) RAISES ANY 

; PROCEDURE TestMustFail ( Proc : ProcTyp ; Label : TEXT )

; PROCEDURE Report ( ) 

; END Support
. 
