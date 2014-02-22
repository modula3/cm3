INTERFACE UniRdClass 

; IMPORT Rd  
; IMPORT UniCodec  
; FROM UniCodec IMPORT Widechar  
; FROM UniEncoding IMPORT Encoding  
; IMPORT UniRd
; IMPORT Word 

; REVEAL UniRd . T 
  = MUTEX 
    BRANDED "UniRd.T" OBJECT 
      Source : Rd . T := NIL 
    ; DecWideChar : UniCodec . DecProc := NIL 
    ; Index : Word . T := 0 (* In Unicode characters. *) 
    ; PrevSourceIndex : Word . T := 0 
      (* ^Source (byte) index as it was at the beginning of the most recently
         returned Unicode character. *) 
    ; PostponedWCh : Widechar 
    ; MaxBytesPerChar : [ 0 .. 4 ] := 0 
    ; Enc : Encoding := Encoding . Null 
    ; HasPostponedWCh : BOOLEAN := FALSE 
    END 

; END UniRdClass 
. 
