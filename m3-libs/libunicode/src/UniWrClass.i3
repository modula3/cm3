INTERFACE UniWrClass 

; IMPORT UniCodec  
; FROM UniEncoding IMPORT Encoding  
; IMPORT UniWr
; IMPORT Wr  

; REVEAL UniWr . T 
  = MUTEX 
    BRANDED "UniWr.T" OBJECT 
      Sink : Wr . T 
    ; Enc : Encoding 
    ; EncWideChar : UniCodec . EncProc  
    END 

; END UniWrClass 
. 
