(* ----------------------------------------------------------------------1- *)
(* File PickleTestLongint.m3 Modula-3 source code.                          *)
(* Copyright 2010 .. 2016, Rodney M. Bates.                                 *)
(* rodney.m.bates@acm.org                                                   *)
(* Licensed under the MIT license.                                          *)
(* ----------------------------------------------------------------------2- *)

MODULE PickleTestLongint EXPORTS Main 

(* Write a pickle with LONGINTs in various settings, then reread it and
   verify the values come back as expected.  

   For a really good test, write and read on machines with different
   word sizes and endiannesses. This needs some command-line options to
   do this, as it currently writes that same file it reads, all in one
   execution. 

*) 

; IMPORT Fmt
; IMPORT IO 
; IMPORT Long 
; IMPORT Pickle2 
; IMPORT Rd 
; IMPORT Stdio 
; IMPORT Thread 
; IMPORT Wr 

; <* FATAL Thread . Alerted *> 
  <* FATAL Rd . Failure *> 
  <* FATAL Rd . EndOfFile *> 
  <* FATAL Wr . Failure *> 
  <* FATAL Pickle2 . Error *> 

  CONST FileName = "A.pickle" 

; TYPE <* STRICTALIGN *> T 
  = RECORD 
      A : BITS 8 FOR [ 0 .. 255 ] 
    ; B : (* BITS 32 FOR *) INTEGER 
    ; C : LONGINT 
    ; D : [ 0 .. 255 ] 
    ; E : [ -128 .. 127] 
    ; F : [ 0 .. 65535 ] 
    ; G : [ - 32768 .. 32767 ] 
    ; H : [ 0 .. 16_7FFFFFFF ]
    END

; PROCEDURE LoBits ( Val : LONGINT ) : INTEGER

  = VAR LVal : LONGINT
  ; VAR LResult : INTEGER

  ; BEGIN
      LVal := Val
    ; IF LVal > 16_7FFFFFFFL
      THEN DEC ( LVal , 16_100000000L )
      END (* IF *)
    ; LResult := VAL ( ORD ( LVal ) , INTEGER )
    ; RETURN LResult
    END LoBits

; PROCEDURE PL ( Val : LONGINT )

  = VAR Lo1 , Lo0 : INTEGER
  ; VAR Lo1L , X , Lo0L : LONGINT

  ; BEGIN
      Lo1L := Long . Shift ( Val , -32 )
    ; X := Long . Shift ( Val , 32 )
    ; Lo0L := Long . Shift ( X , - 32 )
    ; Lo1 := LoBits ( Lo1L )
    ; Lo0 := LoBits ( Lo0L )

    ; Wr . PutText ( Stdio . stdout , "16_" )
    ; Wr . PutText
        ( Stdio . stdout
        , Fmt . Pad ( Fmt . Unsigned ( Lo1 , base := 16 ) , 8 , '0' )
        )
    ; Wr . PutText
        ( Stdio . stdout
        , Fmt . Pad ( Fmt . Unsigned ( Lo0 , base := 16 ) , 8 , '0' )
        )
    ; Wr . PutText ( Stdio . stdout , "L" )
    END PL

; CONST AVal = 254
; CONST BVal = ( ( 1 * 256 + 2 ) * 256 + 4 ) * 256 + 8  
; CONST CVal0 = 16_0102040810204080L 
; CONST CVal = ( ( ( ( ( ( 1L * 256L + 2L ) * 256L + 4L ) * 256L + 8L ) * 256L + 16L ) 
                 * 256L + 32L ) * 256L + 64L ) * 256L + 128L
; CONST DVal = 237
; CONST EVal = - 94
; CONST FVal = 42782
; CONST GVal = - 26677
; CONST HVal = 16_0f4dc379

; PROCEDURE Write ( ) 

  = VAR R : REF T 
  ; VAR WrT : Wr . T 

  ; BEGIN 
      R := NEW ( REF T ) 
    ; R . A := AVal
    ; R . B := BVal 
    ; R . C := CVal0 
    ; R . C := CVal 
    ; R . D := DVal 
    ; R . E := EVal 
    ; R . F := FVal 
    ; R . G := GVal 
    ; R . H := HVal 
    ; WrT := IO . OpenWrite ( FileName ) 
    ; Pickle2 . Write ( WrT , R ) 
    ; Wr . Close ( WrT ) 
    ; Wr . PutText ( Stdio . stdout , "Wrote " & FileName )  
    ; Wr . PutText ( Stdio . stdout , Wr . EOL )  
    END Write  

; PROCEDURE CheckInt ( Name : TEXT ; Val , Expected : INTEGER ) 

  = BEGIN 
      Wr . PutText ( Stdio . stdout , Name )  
    ; Wr . PutText ( Stdio . stdout , " = " )  
    ; Wr . PutText ( Stdio . stdout , "16_" )  
    ; Wr . PutText 
        ( Stdio . stdout , Fmt . Pad ( Fmt . Int ( Val , 16 ) , 2 , '0' ) )  
    ; Wr . PutText ( Stdio . stdout , " " )  
    ; Wr . PutText ( Stdio . stdout , Fmt . Int ( Val ) )  
    ; IF Val = Expected 
      THEN 
        Wr . PutText ( Stdio . stdout , ", as expected." )  
      ELSE 
        Wr . PutText ( Stdio . stdout , ", EXPECTED: " )  
      ; Wr . PutText ( Stdio . stdout , "16_" )  
      ; Wr . PutText 
          ( Stdio . stdout , Fmt . Pad ( Fmt . Int ( Expected , 16 ) , 2 , '0' ) )  
      ; Wr . PutText ( Stdio . stdout , " " )  
      ; Wr . PutText ( Stdio . stdout , Fmt . Int ( Expected ) )  
      END (* IF *) 
    ; Wr . PutText ( Stdio . stdout , Wr . EOL )  

    END CheckInt 

; PROCEDURE Read ( ) 

  = VAR R : REF T 
  ; VAR RdT : Rd . T 

  ; BEGIN 
      Wr . PutText ( Stdio . stdout , "Reading " & FileName )  
    ; Wr . PutText ( Stdio . stdout , Wr . EOL )  
    ; RdT := IO . OpenRead ( FileName ) 
    ; R := NARROW ( Pickle2 . Read ( RdT ) , REF T )  

    ; CheckInt ( "A" , R . A , 254 ) 
    ; CheckInt ( "B" , R . B , BVal ) 
 
    ; Wr . PutText ( Stdio . stdout , "C = " )  
 (* ; Wr . PutText 
        ( Stdio . stdout , Fmt . Pad ( Fmt . LongInt ( R . C , 16 ) , 16 , '0' ) ) *) 
    ; PL ( R . C ) 
    ; Wr . PutText ( Stdio . stdout , " " )  
    ; Wr . PutText ( Stdio . stdout , Fmt . LongInt ( R . C ) )  
    ; IF R . C = CVal 
      THEN 
        Wr . PutText ( Stdio . stdout , ", as expected." )  
      ELSE 
        Wr . PutText ( Stdio . stdout , ", EXPECTED: " )  
      ; PL ( CVal ) 
      ; Wr . PutText ( Stdio . stdout , " " )  
      ; PL ( CVal ) 
      END (* IF *) 
    ; Wr . PutText ( Stdio . stdout , Wr . EOL )  

    ; CheckInt ( "D" , R . D , DVal ) 
    ; CheckInt ( "E" , R . E , EVal ) 
    ; CheckInt ( "F" , R . F , FVal ) 
    ; CheckInt ( "G" , R . G , GVal ) 
    ; CheckInt ( "H" , R . H , HVal ) 


    ; Wr . Flush ( Stdio . stdout ) 
    ; Rd . Close ( RdT ) 
    END Read  

; BEGIN 
    Write ( ) 
  ; Read ( ) 
  END PickleTestLongint  
. 

