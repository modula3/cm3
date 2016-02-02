(* Copyright (C) Rodney M. Bates 2016. *)
(* rodney.m.bates@acm.org *) 
(* Licensed under the MIT License. *) 

INTERFACE UniWrClass 

(* Importable Revelation for UniWr.T *)  

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
