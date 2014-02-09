MODULE SmallBuffRd

; IMPORT Rd 
; IMPORT RdClass 

; VAR BufferSize : CARDINAL := 3 

; REVEAL T = Rd . T BRANDED "SmallBuffRd.T" OBJECT 
      Buff : REF ARRAY OF CHAR 
    OVERRIDES
      seek := Seek 
    ; length := Length 
    END 

; PROCEDURE InitBuff ( Buff : REF ARRAY OF CHAR ; lo : CARDINAL ) 

  = BEGIN 
      FOR RI := FIRST ( Buff ^ ) TO LAST ( Buff ^ ) 
      DO 
        Buff ^ [ RI ] := VAL ( RI + lo , CHAR ) 
      END (* FOR *)  
    END InitBuff 

; PROCEDURE Init ( RdT : T ) 

  = BEGIN 
      RdT . buff := NEW ( REF ARRAY OF CHAR , BufferSize )  
    ; RdT . st := 0 
    ; RdT . lo := 0 
    ; RdT . hi := NUMBER ( RdT . buff ^ )
    ; RdT . closed := FALSE 
    ; RdT . seekable := FALSE 
    ; RdT . intermittent := FALSE 
    ; InitBuff ( RdT . buff , RdT . lo )  
    END Init 

; PROCEDURE Seek ( Self : T ; n : CARDINAL ; <*UNUSED*> dontBlock : BOOLEAN ) 
  : RdClass . SeekResult

  = BEGIN 
      Self . lo := n 
    ; Self . hi := n + NUMBER ( Self . buff ^ ) 
    ; InitBuff ( Self . buff , n ) 
    ; RETURN RdClass . SeekResult . Ready 
    END Seek  

; PROCEDURE Length ( <*UNUSED*> Self : T ) : INTEGER 

  = BEGIN 
      RETURN ORD ( LAST ( CHAR ) ) 
    END Length 

(* EXPORTED *) 
; PROCEDURE New ( ) : T 

  = VAR LResult : T 

  ; BEGIN 
      LResult := NEW ( T ) 
    ; Init ( LResult ) 
    ; RETURN LResult 
    END New 

; BEGIN 
  END SmallBuffRd
. 
 
