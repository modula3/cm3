(* ----------------------------------------------------------------------1- *)
(* File Support.m3 for Modula3 compiler test p269                           *)
(* Copyright 2019, Rodney M. Bates.                                         *)
(* rodney.m.bates@acm.org                                                   *)
(* Licensed under the MIT license.                                          *)
(* ----------------------------------------------------------------------2- *)

MODULE Support

; IMPORT Fmt
; IMPORT Stdio
; IMPORT Text 
; IMPORT Thread 
; IMPORT Wr

; IMPORT Globals 

(* EXPORTED: *)
; PROCEDURE W ( String : TEXT )
  = <* FATAL Thread.Alerted , Wr . Failure *>
    BEGIN
      Wr . PutText ( WrT , String ) 
    END W

(* EXPORTED: *)
; PROCEDURE WEOL ( )
  = BEGIN
      W ( Wr . EOL ) 
    END WEOL 

(* EXPORTED: *)
; PROCEDURE WL ( T : TEXT )
  = <* FATAL Thread.Alerted , Wr . Failure *>
    BEGIN
      Wr . PutText ( WrT , T ) 
    ; Wr . PutText ( WrT , Wr . EOL ) 
    END WL

; CONST IndentText = "   "
; CONST IntPadCt = 6

; VAR GProgLabel : TEXT := "<unnamed>"
; VAR GTestCt : INTEGER := 0 
; VAR GFailureCt : INTEGER := 0 
; VAR GTestLabel : TEXT := ""
; VAR GTestLabelDisplayed := FALSE
; VAR GFailed := FALSE

(* EXPORTED: *)
; PROCEDURE Reset ( )
  = BEGIN
      GProgLabel := "<unnamed>" 
    ; GTestCt := 0
    ; GFailureCt := 0
    ; GTestLabel := ""
    ; GTestLabelDisplayed := FALSE
    ; GFailed := FALSE 
    END Reset

; PROCEDURE StartProg ( ProgLabel := "" )
  = BEGIN
      IF ProgLabel = NIL OR Text . Equal ( ProgLabel , "" )
      THEN GProgLabel := "<unnamed test program>"
      ELSE GProgLabel := ProgLabel 
      END (* IF *)
    ; W ( "Test program  " )
    ; W ( GProgLabel )
    ; WEOL ( ) 
    END StartProg

; PROCEDURE DisplayTest ( NL : BOOLEAN )
  = BEGIN
     IF NOT GTestLabelDisplayed
     THEN
       W ( "Test No " )
     ; W ( Fmt . Int ( GTestCt + 1 ) ) 
     ; W ( ": " ) 
     ; W ( GTestLabel )
     ; IF NL THEN WEOL ( ) END 
     ; GTestLabelDisplayed := TRUE
     END 
    END DisplayTest 

(* EXPORTED: *)
; PROCEDURE StartTest ( Label : TEXT := "" )
  = BEGIN
      GTestLabel := Label & ": " 
    ; GTestLabelDisplayed := FALSE
    ; GFailed := FALSE
    END StartTest

(* EXPORTED: *)
; PROCEDURE Note ( Explanation : TEXT := "" )  
  = BEGIN
      DisplayTest ( TRUE )
    ; W ( IndentText )
    ; WL ( Explanation )
    END Note

(* EXPORTED: *)
; PROCEDURE NoteFailure ( Explanation : TEXT := "" )  
  = BEGIN
      DisplayTest ( TRUE )

    ; W ( IndentText )
    ; W ( "###FAILED###: " )
    ; WL ( Explanation )
    ; GFailed := TRUE 
    ; INC ( GFailureCt ) 
    END NoteFailure

(* EXPORTED: *)
; PROCEDURE EndTest ( Display := FALSE )
  = BEGIN
      IF Display AND NOT GFailed
      THEN
        DisplayTest ( FALSE )
      ; W ( " Succeeded." )
      ; WEOL ( )
      END
      
    ; GTestLabel := "" 
    ; INC ( GTestCt ) 
    END EndTest

(* EXPORTED: *)
; PROCEDURE TestMustFail ( Proc : ProcTyp ; Label : TEXT )

  = BEGIN
      TRY
        Proc ( )
      ; NoteFailure ( "Failed to raise an expected exception: " & Label ) 
      EXCEPT
      ELSE
        IF Globals . GDisplaySuccess 
        THEN
          Note ( "Raised an exception, as expected: " & Label ) 
        END 
      END
    END TestMustFail

(* EXPORTED: *)
; PROCEDURE Report ( ) 
  = BEGIN
      W ( "Results summary of " )
    ; W ( GProgLabel )
    ; WEOL ( ) 

    ; W ( Fmt . Pad ( Fmt . Int ( GTestCt ) , IntPadCt ) ) 
    ; W ( " Tests performed." )
    ; WEOL ( )

    ; IF GFailureCt <= 0
      THEN
        W ( IndentText )
      ; WL ( "    None Failed; Test Program Succeeded." ) 
      ELSE
        W ( Fmt . Pad ( Fmt . Int ( GFailureCt ) , IntPadCt ) ) 
      ; WL ( " of them Failed." )
      ; W ( "### Test Program " ) 
      ; W ( GProgLabel ) 
      ; WL ( ", FAILED ###" ) 
      END
    END Report

(* EXPORTED: *)
; PROCEDURE Flush ( )
  = <* FATAL Thread.Alerted , Wr . Failure *>
    BEGIN
      Wr . Flush ( WrT ) 
    END Flush 

; BEGIN
    WrT := Stdio . stdout
  ; Reset ( ) 
  END Support
. 
