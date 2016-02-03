  
(* -----------------------------------------------------------------------1- *)
(* File GenBitNoArrays.m3  Modula-3 source code.                             *)
(* Copyright 2010 .. 2012, Rodney M. Bates.                                  *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *) 
(* -----------------------------------------------------------------------2- *)

MODULE GenBitNoArrays EXPORTS Main 

; IMPORT FileWr  
; IMPORT Fmt 
; IMPORT OSError 
; IMPORT Thread 
; IMPORT Word 
; IMPORT Wr 

; TYPE ByteTyp = [ 0 .. 16_FF ] 

; TYPE ValProcTyp 
  = PROCEDURE ( ByteVal : ByteTyp ) RAISES { Thread . Alerted , Wr . Failure } 

; VAR WrT : Wr . T 

; PROCEDURE Least1Bit ( ByteVal : ByteTyp ) 
  RAISES { Thread . Alerted , Wr . Failure } 

  = VAR LVal : Word . T 
  ; VAR LBitNo : INTEGER 

  ; BEGIN 
      LVal := ByteVal + 16_100 (* Guard bit to ensure stop. *) 
    ; LBitNo := 0 
    ; WHILE Word . And ( LVal , 16_1 ) = 0 
      DO 
        LVal := Word . Shift ( LVal , - 1 ) 
      ; INC ( LBitNo ) 
      END (* LOOP *) 
    ; Wr . PutText ( WrT , Fmt . Int ( LBitNo ) ) 
    END Least1Bit 

; PROCEDURE Least0Bit ( ByteVal : ByteTyp ) 
  RAISES { Thread . Alerted , Wr . Failure } 

  = VAR LVal : Word . T 
  ; VAR LBitNo : INTEGER 

  ; BEGIN 
      LVal := Word . And ( ByteVal , 16_FF ) (* Guard 0-bit to ensure stop. *) 
    ; LBitNo := 0 
    ; WHILE Word . And ( LVal , 16_1 ) = 1 
      DO 
        LVal := Word . Shift ( LVal , - 1 ) 
      ; INC ( LBitNo ) 
      END (* LOOP *) 
    ; Wr . PutText ( WrT , Fmt . Int ( LBitNo ) ) 
    END Least0Bit 

; PROCEDURE Greatest0Bit ( ByteVal : ByteTyp ) 
  RAISES { Thread . Alerted , Wr . Failure } 

  = VAR LVal : Word . T 
  ; VAR LBitNo : INTEGER 

  ; BEGIN 
      LVal := Word . Shift ( ByteVal , 1 ) (* Guard 0-bit on right. *)
    ; LBitNo := 7
    ; WHILE Word . And ( LVal , 16_100 ) # 0 
      DO 
        LVal := Word . Shift ( LVal , 1 ) 
      ; DEC ( LBitNo ) 
      END (* LOOP *) 
    ; IF LBitNo < 0 
      THEN LBitNo := 8 (* What is the use of this? *) 
      END (* IF *) 
    ; Wr . PutText ( WrT , Fmt . Int ( LBitNo ) ) 
    END Greatest0Bit 

; PROCEDURE Greatest1Bit ( ByteVal : ByteTyp ) 
  RAISES { Thread . Alerted , Wr . Failure } 

  = VAR LVal : Word . T 
  ; VAR LBitNo : INTEGER 

  ; BEGIN 
      LVal := Word . Shift ( ByteVal , 1 ) + 16_1 (* Guard 1-bit on right. *)
    ; LBitNo := 7
    ; WHILE Word . And ( LVal , 16_100 ) = 0 
      DO 
        LVal := Word . Shift ( LVal , 1 ) 
      ; DEC ( LBitNo ) 
      END (* LOOP *) 
    ; IF LBitNo < 0 
      THEN LBitNo := 8 (* What is the use of this? *) 
      END (* IF *) 
    ; Wr . PutText ( WrT , Fmt . Int ( LBitNo ) ) 
    END Greatest1Bit 

; PROCEDURE NoOf1Bits ( ByteVal : ByteTyp ) 
  RAISES { Thread . Alerted , Wr . Failure }  

  = VAR LVal : Word . T 
  ; VAR LBitCt : INTEGER 

  ; BEGIN 
      LVal := ByteVal 
    ; LBitCt := 0 
    ; FOR RI := 0 TO 7 
      DO 
        INC ( LBitCt , Word . And ( LVal , 16_1 ) )  
      ; LVal := Word . Shift ( LVal , - 1 ) 
      END (* FOR *) 
    ; Wr . PutText ( WrT , Fmt . Int ( LBitCt ) ) 
    END NoOf1Bits  

; PROCEDURE GenArray ( Name : TEXT ; ValProc : ValProcTyp ) 

  = VAR LSs : CARDINAL 

  ; <* FATAL Thread . Alerted , Wr . Failure *> 
    BEGIN 
      Wr . PutText ( WrT , "; CONST " ) 
    ; Wr . PutText ( WrT , Name ) 
    ; Wr . PutText ( WrT , Wr . EOL ) 
    ; Wr . PutText ( WrT , "    = ARRAY [ 0 .. 16_FF ] OF [ 0 .. 8 ]" ) 
    ; Wr . PutText ( WrT , Wr . EOL ) 
    ; Wr . PutText ( WrT , "        { " )
    ; LSs := 0 
    ; LOOP 
        Wr . PutText ( WrT , "(* " ) 
      ; Wr . PutText ( WrT , Fmt . Pad ( Fmt . Int ( LSs ) , 3 ) ) 
      ; Wr . PutText ( WrT , " 16_" ) 
      ; Wr . PutText ( WrT , Fmt . Pad ( Fmt . Int ( LSs , 16 ) , 2 , '0' ) )
      ; Wr . PutText ( WrT , " 2_" ) 
      ; Wr . PutText ( WrT , Fmt . Pad ( Fmt . Int ( LSs , 2 ) , 8 , '0' ) )
      ; Wr . PutText ( WrT , " *) " ) 
      ; ValProc ( LSs ) 
      ; IF LSs = 16_FF 
        THEN 
          Wr . PutText ( WrT , Wr . EOL ) 
        ; EXIT 
        ELSE 
          INC ( LSs ) 
        ; IF LSs MOD 2 = 0 
          THEN 
            Wr . PutText ( WrT , Wr . EOL ) 
          ; Wr . PutText ( WrT , "        , " ) 
          ELSE 
            Wr . PutText ( WrT , " , " ) 
          END (* IF *) 
        END (* IF *) 
      END (* LOOP *) 
    ; Wr . PutText ( WrT , "        } " )
    ; Wr . PutText ( WrT , Wr . EOL ) 
    ; Wr . PutText ( WrT , Wr . EOL ) 
    END GenArray 

; PROCEDURE GenInterface ( ) 
  
  = <* FATAL Thread . Alerted , Wr . Failure *> 
    BEGIN 
      Wr . PutText ( WrT , Wr . EOL ) 
    ; Wr . PutText 
        ( WrT , "(* -----------------------------------------------------------------------1- *)" )
    ; Wr . PutText ( WrT , Wr . EOL ) 
    ; Wr . PutText 
        ( WrT , "(* File BitNoTable.i3  Modula-3 source code.                                 *)" )
    ; Wr . PutText ( WrT , Wr . EOL ) 
    ; Wr . PutText 
        ( WrT , "(* Copyright 2010 .. 2012, Rodney M. Bates.                                  *)" )
    ; Wr . PutText ( WrT , Wr . EOL ) 
    ; Wr . PutText 
        ( WrT , "(* rodney.m.bates@acm.org                                                    *)" )
    ; Wr . PutText ( WrT , Wr . EOL ) 
    ; Wr . PutText 
        ( WrT , "(* Licensed under the MIT License.                                           *)" )
    ; Wr . PutText ( WrT , Wr . EOL ) 
    ; Wr . PutText 
        ( WrT , "(* -----------------------------------------------------------------------2- *)" )
    ; Wr . PutText ( WrT , Wr . EOL ) 
    ; Wr . PutText ( WrT , Wr . EOL ) 

    ; Wr . PutText ( WrT , "(* Mechanically generated by GenBitNoArrays. *)" ) 
    ; Wr . PutText ( WrT , Wr . EOL ) 
    ; Wr . PutText ( WrT , Wr . EOL ) 

    ; Wr . PutText ( WrT , "INTERFACE BitNoTable " ) 
    ; Wr . PutText ( WrT , Wr . EOL ) 
    ; Wr . PutText ( WrT , Wr . EOL ) 
 (* ; GenArray ( "Least0BitNoInByte" , Least0Bit ) No longer needed. *) 
    ; GenArray ( "Least1BitNoInByte" , Least1Bit ) 
 (* ; GenArray ( "Greatest0BitNoInByte" , Greatest0Bit ) No longer needed. *) 
    ; GenArray ( "Greatest1BitNoInByte" , Greatest1Bit ) 
    ; GenArray ( "NoOf1BitsInByte" , NoOf1Bits ) 
    ; Wr . PutText ( WrT , "; END BitNoTable " ) 
    ; Wr . PutText ( WrT , Wr . EOL ) 
    ; Wr . PutText ( WrT , ". " ) 
    ; Wr . PutText ( WrT , Wr . EOL ) 
    ; Wr . PutText ( WrT , Wr . EOL ) 
    END GenInterface  

; BEGIN 
    <* FATAL Thread . Alerted , Wr . Failure , OSError . E *> 
    BEGIN 
      WrT := FileWr . Open ( "BitNoTable.i3" )   
    ; GenInterface ( ) 
    ; Wr . Flush ( WrT ) 
    ; Wr . Close ( WrT ) 
    END 
  END GenBitNoArrays
. 
