MODULE TestUniCodec EXPORTS Main 

; IMPORT FileRd 
; IMPORT FileWr 
; IMPORT Fmt
; IMPORT OSError  
; IMPORT Rd 
; IMPORT Stdio 
; IMPORT Text 
; FROM Thread IMPORT Alerted 
; IMPORT UniEncoding 
; FROM UniEncoding IMPORT Encoding , EncImage 
; IMPORT UniCodec 
; FROM UniCodec IMPORT Widechar 
; FROM Word IMPORT Plus , Minus 
; IMPORT Wr 

; VAR GEnc := Encoding . ISO8859_1 
; VAR GEncSet := SET OF Encoding { Encoding . ISO8859_1 .. LAST ( Encoding ) } 
; VAR GWrite : BOOLEAN := FALSE 
; VAR GRead : BOOLEAN := FALSE 
; VAR GWrT : Wr . T 
; VAR GRdT : Rd . T 

; PROCEDURE Msg ( T : TEXT ) 

  = <* FATAL Wr . Failure , Alerted *> 
    BEGIN 
      Wr . PutText ( Stdio . stdout , T ) 
    ; Wr . PutText ( Stdio . stdout , Wr . EOL ) 
    END Msg

; PROCEDURE Flush ( ) 

  = <* FATAL Wr . Failure , Alerted *> 
    BEGIN 
      Wr . Flush ( Stdio. stdout ) 
    END Flush 

; PROCEDURE WchImage ( Wch : Widechar ) : TEXT 

  = BEGIN
      RETURN "16_" & Fmt . Int ( ORD ( Wch ) , 16 ) 
    END WchImage 

; PROCEDURE FileName ( Prefix : TEXT ; Enc : Encoding ) : TEXT 

  = BEGIN 
      RETURN Prefix & UniEncoding . EncImage ( Enc ) 
    END FileName 

; PROCEDURE EncodingFromText ( T : TEXT ) : Encoding 

  = BEGIN 
      FOR RE := FIRST ( Encoding ) TO LAST ( Encoding ) 
      DO
        IF Text . Equal ( EncImage ( RE ) , T ) 
        THEN RETURN RE 
        END (* IF *)  
      END (* FOR *) 
    ; RETURN Encoding . Null 
    END EncodingFromText 

; PROCEDURE Init ( ) 

  = BEGIN 
      GEnc := Encoding . ISO8859_1 
    ; GWrite := FALSE 
    ; GRead := FALSE 
    END Init 

; TYPE ProcOfWch = PROCEDURE ( Wch : Widechar ) RAISES ANY  

; PROCEDURE ForAllLegalDo ( Enc : Encoding ; Callback : ProcOfWch ) 
  RAISES ANY  

  = BEGIN 
      CASE Enc 
      OF Encoding . Null 
      , Encoding . Internal 
      => (* Forget it. *) 

      | Encoding . ISO8859_1 
      => FOR RWch := FIRST ( Widechar ) TO VAL ( 16_FF , Widechar )  
        DO Callback ( RWch ) 
        END (* FOR *) 

      | Encoding . CM3WC
      => FOR RWch := FIRST ( Widechar ) TO VAL ( 16_FFFF , Widechar )  
        DO Callback ( RWch ) 
        END (* FOR *) 

      | Encoding . UCS2  
      , Encoding . UCS2LE 
      , Encoding . UCS2BE 
      => FOR RWch := FIRST ( Widechar ) TO VAL ( 16_D7FF , Widechar )  
        DO Callback ( RWch ) 
        END (* FOR *) 
      (* Skip the surrogates. *) 
      ; FOR RWch := VAL ( 16_E000 , Widechar ) TO VAL ( 16_FFFF , Widechar )
        DO Callback ( RWch ) 
        END (* FOR *) 

      ELSE 
        FOR RWch := FIRST ( Widechar ) TO VAL ( 16_D7FF , Widechar )  
        DO Callback ( RWch ) 
        END (* FOR *) 
      (* Skip the surrogates. *) 
      ; FOR RWch := VAL ( 16_E000 , Widechar ) TO LAST ( Widechar )   
        DO Callback ( RWch ) 
        END (* FOR *) 
      END (* CASE *) 
    END ForAllLegalDo 

; PROCEDURE ExpectedCharCt ( Enc : Encoding ) : CARDINAL 

  = BEGIN 
      CASE Enc 
      OF Encoding . Null 
      , Encoding . Internal 
      => RETURN 0 

      | Encoding . ISO8859_1 
      => RETURN 16_100

      | Encoding . CM3WC
      => RETURN 16_10000

      | Encoding . UCS2  
      , Encoding . UCS2LE 
      , Encoding . UCS2BE 
      => RETURN Plus ( Minus ( 16_D800 , ORD ( FIRST ( Widechar ) ) ) 
                     , Minus ( 16_10000 , 16_E000 )
                     ) 

      ELSE 
        RETURN Plus ( Minus ( 16_D800 , ORD ( FIRST ( Widechar ) ) ) 
                    , Minus ( ORD ( LAST ( Widechar ) ) , 16_DFFF )
                    ) 
      END (* CASE *) 
    END ExpectedCharCt 

; PROCEDURE WriteAll ( Enc : Encoding ) 

  = VAR LFileName : TEXT 
  ; VAR WaWrT : Wr . T 
  ; VAR WaCharNo : CARDINAL 

  ; PROCEDURE WaWriteWch ( Wch : Widechar )
    = <* FATAL Wr . Failure , Alerted *> 
      BEGIN 
        IF FALSE AND WaCharNo MOD 50 = 0 AND WaCharNo > 0 
        THEN
          Wr . PutText ( WaWrT , Wr . EOL ) 
        END (* IF *) 
      ; <* FATAL UniCodec . Range *> 
        BEGIN 
          UniCodec . Encode ( Enc , WaWrT , Wch ) 
        END 
      ; INC ( WaCharNo ) 
      END WaWriteWch

  ; <* FATAL Wr . Failure , Alerted , OSError . E *> 
    BEGIN (* WriteAll *) 
      LFileName := FileName ( "AllChars" , Enc ) 
    ; WaWrT := FileWr . Open ( LFileName ) 
    ; Msg ( "Writing file " & LFileName ) 
    ; Flush ( ) 
    ; GWrT := WaWrT 
    ; WaCharNo := 0 
    ; <* FATAL ANY *> 
      BEGIN ForAllLegalDo ( Enc , WaWriteWch ) 
      END (* Block *) 
 (* ; Wr . PutText ( WaWrT , Wr . EOL ) *) 
    ; Wr . Close ( WaWrT ) 
    ; Msg ( "Wrote file " & LFileName & ", "
            & Fmt . Int ( WaCharNo ) & " characters." 
          )
    ; IF WaCharNo # ExpectedCharCt ( Enc ) 
      THEN
        Msg ( "Expected " & Fmt . Int ( ExpectedCharCt ( Enc ) ) 
              & ", but got " & Fmt . Int ( WaCharNo ) 
            ) 
      END (* IF *) 
    ; Flush ( ) 
    ; EVAL ( WaCharNo ) 
    END WriteAll 

; CONST GFirstEOLChar = '\n' 

; PROCEDURE ConsumeEOL ( RdT : Rd . T ; VAR Wch : Widechar ) : BOOLEAN 

  = BEGIN 
      IF ORD ( Wch ) # ORD ( GFirstEOLChar )  
      THEN RETURN FALSE 
      ELSE RETURN TRUE
      END (* IF *) 
    END ConsumeEOL 

; PROCEDURE ReadAll ( Enc : Encoding ) 

  = VAR LFileName : TEXT 
  ; VAR RaRdT : Rd . T 
  ; VAR RaCharNo : CARDINAL 

  ; PROCEDURE RaReadWch ( Wch : Widechar ) RAISES { Rd . EndOfFile } 

    = VAR LWch : Widechar 

    ; <* FATAL Rd . Failure , Alerted *> 
      BEGIN 
        LWch := UniCodec . Decode ( Enc , RaRdT ) 
      ; IF FALSE AND RaCharNo MOD 50 = 0 AND RaCharNo > 0 
        THEN (* Expect EOL. *) 
          IF ORD ( LWch ) # ORD ( Text . GetChar ( Wr . EOL , 0 ) )
          THEN 
            Msg 
              ( "Missing expected EOL at char no " & Fmt . Int ( RaCharNo ) )
          ELSE 
            IF Text . Length ( Wr . EOL ) = 2 
            THEN
              LWch := UniCodec . Decode ( Enc , RaRdT ) 
            ; IF ORD ( LWch ) # ORD ( Text . GetChar ( Wr . EOL , 0 ) )
              THEN 
                Msg ( "Missing expected EOL tail at char no " 
                      & Fmt . Int ( RaCharNo )
                    ) 
              END (* IF *) 
            END (* IF *) 
          ; LWch 
              := UniCodec . Decode ( Enc , RaRdT ) (* Consume last of EOL. *)
          END (* IF *) 
        END (* IF *) 
      ; IF LWch # Wch 
        THEN 
          Msg ( "Read " & WchImage ( LWch ) & ", expected " & WchImage ( Wch )
                & " for char no " & Fmt . Int ( RaCharNo ) 
              ) 
        END (* IF *) 
      ; INC ( RaCharNo ) 
      END RaReadWch

  ; <* FATAL Rd . Failure , Alerted , OSError . E *> 
    BEGIN (* ReadAll *) 
      LFileName := FileName ( "AllChars" , Enc ) 
    ; RaRdT := FileRd . Open ( LFileName ) 
    ; Msg ( "Reading file " & LFileName ) 
    ; Flush ( ) 
    ; GRdT := RaRdT 
    ; RaCharNo := 0 
    ; TRY 
        ForAllLegalDo ( Enc , RaReadWch ) 
      ; IF NOT Rd . EOF ( RaRdT ) 
        THEN 
          Msg ( "File " & LFileName & " Not emptied." )
        END (* IF *) 
      EXCEPT 
      Rd . EndOfFile 
      => Msg ( "Premature EOF expecting char no " & Fmt . Int ( RaCharNo ) ) 
      ELSE (* Can't happen. *) 
      END (* EXCEPT*) 
    ; Rd . Close ( RaRdT ) 
    ; Msg ( "Read file " & LFileName & ", "
            & Fmt . Int ( RaCharNo ) & " characters." 
          ) 
    ; Flush ( ) 
    ; EVAL ( RaCharNo ) 
    END ReadAll 

; PROCEDURE Work ( ) 

  = BEGIN 
      Init ( ) 
    ; GWrite := TRUE 
    ; GRead := TRUE 
    ; FOR RE := Encoding . ISO8859_1 TO LAST ( Encoding ) 
      DO 
        IF RE IN GEncSet 
        THEN 
          Msg ( "------------------------------------------------" ) 
        ; GEnc := RE 
        ; IF GWrite 
          THEN WriteAll ( GEnc ) 
          END (* IF *) 
        ; IF GRead 
          THEN ReadAll ( GEnc ) 
          END (* IF *) 
        END (* IF *) 
      END (* FOR *) 
    END Work 

; BEGIN 
    Work ( ) 
  END TestUniCodec
. 

