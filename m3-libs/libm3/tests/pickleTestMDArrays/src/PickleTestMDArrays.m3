(* ----------------------------------------------------------------------1- *)
(* File PickleTestMDArrays.m3 Modula-3 source code.                         *)
(* Copyright 2010 .. 2016, Rodney M. Bates.                                 *)
(* rodney.m.bates@acm.org                                                   *)
(* Licensed under the MIT license.                                          *)
(* ----------------------------------------------------------------------2- *)

MODULE PickleTestMDArrays EXPORTS Main 

; IMPORT FileWr
; IMPORT FileRd 
; IMPORT Fmt 
; IMPORT OSError
; IMPORT Pickle2 AS Pickle 
; IMPORT Rd
; IMPORT Stdio
; IMPORT Thread 
; IMPORT Wr

; <* FATAL Thread . Alerted *>  
  <* FATAL OSError . E *>  
  <* FATAL Wr . Failure *>  
  <* FATAL Rd . Failure *>  
  <* FATAL Rd . EndOfFile *>  
  <* FATAL Pickle . Error *>  

  CONST Filename = "Test.pkl"

; TYPE A2 = ARRAY OF ARRAY OF INTEGER 
; TYPE A4 = ARRAY OF ARRAY OF ARRAY OF ARRAY OF INTEGER 

; TYPE RA2 = REF A2
; TYPE RA4 = REF A4

; VAR VA2 , VRA2 : RA2
; VAR VA4 , VRA4 : RA4

; VAR WrT , FWrT : Wr . T 
; VAR FRdT : Rd . T 

; VAR GTestCt , GRereadCt , GFailCt : INTEGER := 0 

; PROCEDURE Work ( N : INTEGER ) 

  = VAR LExp : INTEGER 

  ; BEGIN
      Wr . PutText ( WrT , "Initializing arrays ... " ) 
    ; Wr . Flush ( WrT ) 
    ; VA2 := NEW ( RA2 , N , N ) 
    ; VA4 := NEW ( RA4 , N , N , N , N ) 
    ; FOR R1 := 0 TO N - 1   
      DO
       FOR R2 := 0 TO N - 1   
        DO
          VA2 ^ [ R1 , R2 ] := R1 * N + R2 
        ; INC ( GTestCt ) 
        END (* FOR *) 
      END (* FOR *) 

    ; FOR R1 := 0 TO N - 1   
      DO
       FOR R2 := 0 TO N - 1   
        DO
          FOR R3 := 0 TO N - 1   
          DO
           FOR R4 := 0 TO N - 1   
            DO
              VA4 ^ [ R1 , R2 , R3 , R4 ] 
                := R1 * N * N * N 
                   + R2 * N * N 
                   + R3 * N 
                   + R4 
            ; INC ( GTestCt ) 
            END (* FOR *) 
          END (* FOR *) 
        END (* FOR *) 
      END (* FOR *) 
    ; Wr . PutText ( WrT , "done, " ) 
    ; Wr . PutText ( WrT , Fmt . Int ( GTestCt ) )  
    ; Wr . PutText ( WrT , "values. " ) 
    ; Wr . PutText ( WrT , Wr . EOL ) 
    ; Wr . Flush ( WrT ) 

    ; Wr . PutText ( WrT , "Writing pickle ... " ) 
    ; Wr . Flush ( WrT ) 
    ; FWrT := FileWr . Open ( Filename ) 
    ; Pickle . Write ( FWrT , VA2 ) 
    ; Pickle . Write ( FWrT , VA4 ) 
    ; Wr . Close ( FWrT ) 
    ; Wr . PutText ( WrT , "done." ) 
    ; Wr . PutText ( WrT , Wr . EOL ) 
    ; Wr . Flush ( WrT ) 

    ; Wr . PutText ( WrT , "Reading pickle ... " ) 
    ; Wr . Flush ( WrT ) 
    ; FRdT := FileRd . Open ( Filename ) 
    ; VRA2 := Pickle . Read ( FRdT ) 
    ; VRA4 := Pickle . Read ( FRdT )
    ; Rd . Close ( FRdT ) 
    ; Wr . PutText ( WrT , "done." ) 
    ; Wr . PutText ( WrT , Wr . EOL ) 
    ; Wr . Flush ( WrT ) 

    ; FOR R1 := 0 TO N - 1   
      DO
       FOR R2 := 0 TO N - 1   
        DO
          IF VRA2 ^ [ R1 , R2 ] # VA2 ^ [ R1 , R2 ]  
          THEN 
            Wr . PutText ( WrT , "2-d array differs at " ) 
          ; Wr . PutText ( WrT , Fmt . Int ( R1 ) ) 
          ; Wr . PutText ( WrT , ", " ) 
          ; Wr . PutText ( WrT , Fmt . Int ( R2 ) ) 
          ; Wr . PutText ( WrT , ", expected " ) 
          ; Wr . PutText ( WrT , Fmt . Int ( VA2 ^ [ R1 , R2 ] ) )
          ; Wr . PutText ( WrT , ", got " ) 
          ; Wr . PutText ( WrT , Fmt . Int ( VRA2 ^ [ R1 , R2 ] ) )
          ; Wr . PutText ( WrT , Wr . EOL ) 
          ; Wr . Flush ( WrT ) 
          ; INC ( GFailCt ) 
          END (* IF *) 
        ; INC ( GRereadCt ) 
        END (* FOR *) 
      END (* FOR *) 

    ; FOR R1 := 0 TO N - 1   
      DO
       FOR R2 := 0 TO N - 1   
        DO
          FOR R3 := 0 TO N - 1   
          DO
           FOR R4 := 0 TO N - 1   
            DO
               LExp := R1 * N * N * N 
                       + R2 * N * N 
                       + R3 * N 
                       + R4 
            ; IF VRA4 ^ [ R1 , R2 , R3 , R4 ] # LExp 
              THEN 
                Wr . PutText ( WrT , "4-d array differs at " ) 
              ; Wr . PutText ( WrT , Fmt . Int ( R1 ) ) 
              ; Wr . PutText ( WrT , ", " ) 
              ; Wr . PutText ( WrT , Fmt . Int ( R2 ) ) 
              ; Wr . PutText ( WrT , ", " ) 
              ; Wr . PutText ( WrT , Fmt . Int ( R3 ) ) 
              ; Wr . PutText ( WrT , ", " ) 
              ; Wr . PutText ( WrT , Fmt . Int ( R4 ) ) 
              ; Wr . PutText ( WrT , ", expected " ) 
              ; Wr . PutText ( WrT , Fmt . Int ( LExp ) ) 
              ; Wr . PutText ( WrT , ", got " ) 
              ; Wr . PutText ( WrT , Fmt . Int ( VRA4 ^ [ R1 , R2 , R3 , R4 ] ) )
              ; Wr . PutText ( WrT , Wr . EOL ) 
              ; Wr . Flush ( WrT ) 
              ; INC ( GFailCt ) 
              END (* IF *) 
            ; INC ( GRereadCt ) 
            END (* FOR *) 
          END (* FOR *) 
        END (* FOR *) 
      END (* FOR *) 

    ; Wr . PutText ( WrT , Fmt . Int ( GRereadCt ) )  
    ; Wr . PutText ( WrT , " values unpickled, " ) 
    ; IF GRereadCt = GTestCt 
      THEN Wr . PutText ( WrT , "as expected." ) 
      ELSE 
        Wr . PutText ( WrT , Fmt . Int ( GTestCt )  ) 
      ; Wr . PutText ( WrT , "were expected." ) 
      END (* IF *)
    ; Wr . PutText ( WrT , Wr . EOL ) 
    ; Wr . Flush ( WrT ) 
    ; IF GFailCt = 0 
      THEN 
        Wr . PutText ( WrT , "All values unpickled as expected." ) 
      ELSE 
        Wr . PutText ( WrT , Fmt . Int ( GFailCt ) ) 
      ; Wr . PutText ( WrT , " values not as exected." ) 
      END (* IF *) 
    ; Wr . PutText ( WrT , Wr . EOL ) 
    ; Wr . Flush ( WrT ) 
 
    END Work


; BEGIN 
    WrT := Stdio . stdout 
  ; Work ( 4 ) 
  ; Wr . Close ( WrT ) 
  END PickleTestMDArrays
.

