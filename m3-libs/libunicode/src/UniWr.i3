(* Copyright (C) Rodney M. Bates 2016. *)
(* rodney.m.bates@acm.org *) 
(* Licensed under the MIT License. *) 

INTERFACE UniWr 

(* Synchronized Writer for character stream with one of several encodings. 
   This runs as a filter, writing bytes to a Wr.T, named "Sink".  It
   locks Sink during any individual operation of this interface.  It is
   possible bypass a UniWr.T and directly operate on its Sink, but think 
   about the synchronization consequences.  

   This is quite similar to Wr, but there is an essential difference.
   In Wr, when a procedure has "Wide" in its name, it means both: 
     1) The character(s) are passed in variables of type WIDECHAR, and 
     2) The encoding in the stream consists of exactly two bytes, LSB first,
        for each character.
   Similarly, for non-wide, the value(s) are passed in CHAR, and one byte 
   for each is written to the stream. 

   In UniWr, "Wide" has only meaning 1).  The encoding is taken from the Stream.
   Procedures that take characters in variables of type CHAR just treat them
   as low values. 
*)  

; FROM Thread IMPORT Alerted 
; FROM UniCodec IMPORT Range 
; FROM UniCodec IMPORT Widechar  
; FROM UniEncoding IMPORT Encoding  
; IMPORT Wr  
; FROM Wr IMPORT Failure 

; TYPE T <: ROOT 

; PROCEDURE Init ( Stream : T ; Sink : Wr . T ; Enc : Encoding ) : T  
  (* Initialize Stream to write to Sink in encoding Enc, then return it. *) 

; PROCEDURE New ( Sink : Wr . T ; Enc : Encoding ) : T 
  (* Equivalent to Init(NEW(T),Sink,Enc) *)  

; PROCEDURE Sink ( Stream : T ) : Wr . T 
  (* The Wr.T used by Stream. *) 

; PROCEDURE Enc ( Stream : T ) : Encoding 
  (* The encoding used by Stream. *) 

; PROCEDURE PutChar ( Stream : T ; Ch : CHAR ) 
  RAISES { Failure , Alerted } 
  (* Encode Ch, using Enc(Stream), and write it to Sink(Stream) *) 
  
; PROCEDURE PutWideChar ( Stream : T ; Wch : Widechar ) 
  RAISES { Range , Failure , Alerted } 
  (* Encode Wch, using Enc(Stream), and write it to Sink(Stream) *) 
  
; PROCEDURE PutString ( Stream : T ; READONLY ArrCh : ARRAY OF CHAR ) 
  RAISES { Failure , Alerted } 
  (* Encode each character of ArrCh, using Enc(Stream), and write it to 
     Sink(Stream) 
  *) 

; PROCEDURE PutWideString ( Stream : T ; READONLY ArrWch : ARRAY OF Widechar ) 
  RAISES { Range , Failure , Alerted } 
  (* Encode each character of ArrWch, using Enc(Stream), and write it to 
     Sink(Stream) 
  *) 

; PROCEDURE PutText ( Stream : T ; String : TEXT ) 
  RAISES { Range , Failure , Alerted }
  (* Encode each character of String, using Enc(Stream), and write it to 
     Sink(Stream) 
  *) 

; CONST (* PROCEDURE *) PutWideText = PutText 
  (* Wide/narrow is hidden inside TEXT.  Either procedure does the same. *) 

; END UniWr 
. 

