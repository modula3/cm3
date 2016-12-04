(* Copyright (C) Rodney M. Bates 2016. *)
(* rodney.m.bates@acm.org *) 
(* Licensed under the MIT License. *) 

INTERFACE UniRdClass 

(* Importable Revelation for UniRd.T *)  

; IMPORT Rd  
; IMPORT UniCodec  
; FROM UniEncoding IMPORT Encoding  
; IMPORT UniRd
; IMPORT Word 

; REVEAL UniRd . T 
  = MUTEX 
    BRANDED "UniRd.T" OBJECT 
      Source : Rd . T := NIL 
    ; DecWideChar : UniCodec . DecProc := NIL 
    ; Index : Word . T := 0 (* In Unicode characters. *) 
    ; CurSourceIndex : CARDINAL := 0 
      (* ^Source (byte) index as it was at the end of the most recently
         returned Unicode character. *) 
    ; UnGetByteCt : [ 0 .. 4 ] := 0 
    ; MaxBytesPerChar : [ 0 .. 4 ] := 0 
    ; Enc : Encoding := Encoding . Null 
    END 

; END UniRdClass 
. 
