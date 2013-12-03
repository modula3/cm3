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
    ; Index : Word . T := 0 
    ; PostponedWCh : Widechar 
    ; MaxBytesPerChar : [ 0 .. 4 ] := 0 
    ; Enc : Encoding := Encoding . Null 
    ; HasPostponedWCh : BOOLEAN := FALSE 
    END 

; END UniRdClass 
. 
