INTERFACE UnsafeUniWr 

(* Writer for character stream with one of several encodings. *) 

; FROM Thread IMPORT Alerted 
; FROM UniCodec IMPORT Range 
; FROM UniCodec IMPORT Widechar  
; IMPORT UniWr 
; IMPORT Wr 
; FROM Wr IMPORT Failure 

; PROCEDURE FastPutWideChar ( Stream : UniWr . T ; Wch : Widechar ) 
  RAISES { Range , Failure , Alerted } 
  (* Encode Wch, using Enc(Stream), and write it to Sink(Stream) *) 
  
; CONST (* PROCEDURE *) FastPutChar = FastPutWideChar 
  (* With Wch passed by value and CHAR assignable to WIDECHAR, only one 
     procedure is needed. *) 

; PROCEDURE FastPutString 
    ( Stream : UniWr . T ; READONLY ArrCh : ARRAY OF CHAR ) 
  RAISES { Failure , Alerted } 
  (* Encode each character of ArrCh, using Enc(Stream), and write it to 
     Sink(Stream) 
  *) 

; PROCEDURE FastPutWideString 
    ( Stream : UniWr . T ; READONLY ArrWch : ARRAY OF Widechar ) 
  RAISES { Range , Failure , Alerted } 
  (* Encode each character of ArrWch, using Enc(Stream), and write it to 
     Sink(Stream) 
  *) 

; PROCEDURE FastPutText ( Stream : UniWr . T ; String : TEXT ) 
  RAISES { Range , Failure , Alerted }
  (* Encode each character of String, using Enc(Stream), and write it to 
     Sink(Stream) 
  *) 

; CONST (* PROCEDURE *) FastPutWideText = FastPutText 
  (* Wide/narrow is hidden inside TEXT.  Either procedure does the same. *) 

; END UnsafeUniWr 
. 

