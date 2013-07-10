MODULE UniWr 

(* Writer for character stream with one of several encodings. *) 

; FROM Thread IMPORT Alerted 
; IMPORT UniCodec 
; FROM UniCodec IMPORT IsBE , Range 
; FROM UniCodec IMPORT Widechar  
; FROM UniEncoding IMPORT Encoding  
; IMPORT UniWrClass 
; IMPORT UnsafeUniCodec 
; IMPORT UnsafeUniWr 
; IMPORT Wr 
; FROM Wr IMPORT Failure 
; IMPORT WrClass 

; REVEAL WrClass . Private <: MUTEX 

(* EXPORTED: *) 
; PROCEDURE Init ( Stream : T ; Sink : Wr . T ; Enc : Encoding ) : T  
  (* Initialize Stream to write to Sink in encoding Enc, then return it. *) 

  = BEGIN 
      LOCK Stream 
      DO 
        Stream . Sink := Sink 
      ; Stream . Enc := Enc 
      ; Stream . EncWideChar 
          := UnsafeUniCodec . EncTable [ IsBE , Enc ] 
      ; RETURN Stream  
      END (* LOCK *) 
    END Init 

(* EXPORTED: *) 
; PROCEDURE New ( Sink : Wr . T ; Enc : Encoding ) : T 
  (* Equivalent to Init(NEW(T),Sink,Enc) *) 

  = BEGIN 
      RETURN Init ( NEW ( T ) , Sink , Enc ) 
    END New 

(* EXPORTED: *) 
; PROCEDURE Sink ( Stream : T ) : Wr . T 
  (* The Wr.T used by Stream. *) 

  = BEGIN
      LOCK Stream 
      DO 
        RETURN Stream . Sink 
      END (* LOCK *) 
    END Sink 

(* EXPORTED: *) 
; PROCEDURE Enc ( Stream : T ) : Encoding 
  (* The encoding used by Stream. *) 

  = BEGIN
      LOCK Stream 
      DO 
        RETURN Stream . Enc 
      END (* LOCK *) 
    END Enc

(* EXPORTED: *) 
; PROCEDURE PutWideChar ( Stream : T ; Wch : Widechar ) 
  RAISES { Range , Failure , Alerted } 
  (* Encode Wch, using Enc(Stream), and write it to Sink(Stream) *) 
  
  = BEGIN 
      LOCK Stream 
      DO LOCK Stream . Sink 
        DO 
          UnsafeUniWr . FastPutWideChar ( Stream , Wch ) 
        END (* LOCK *) 
      END (* LOCK *) 
    END PutWideChar 

(* EXPORTED: *) 
; PROCEDURE PutString ( Stream : T ; READONLY ArrCh : ARRAY OF CHAR ) 
  RAISES { Failure , Alerted } 
  (* Encode each character of ArrCh, using Enc(Stream), and write it to 
     Sink(Stream) 
  *) 

  = BEGIN  
      LOCK Stream 
      DO LOCK Stream . Sink 
        DO 
          UnsafeUniWr . FastPutString ( Stream , ArrCh ) 
        END (* LOCK *) 
      END (* LOCK *) 
    END PutString 

(* EXPORTED: *) 
; PROCEDURE PutWideString ( Stream : T ; READONLY ArrWch : ARRAY OF Widechar ) 
  RAISES { Range , Failure , Alerted } 
  (* Encode each character of ArrWch, using Enc(Stream), and write it to 
     Sink(Stream) 
  *) 

  = BEGIN 
      LOCK Stream 
      DO LOCK Stream . Sink 
        DO 
          UnsafeUniWr . FastPutWideString ( Stream , ArrWch ) 
        END (* LOCK *) 
      END (* LOCK *) 
    END PutWideString 

(* EXPORTED: *) 
; PROCEDURE PutText ( Stream : T ; String : TEXT ) 
  RAISES { Range , Failure , Alerted }
  (* Encode each character of String, using Enc(Stream), and write it to 
     Sink(Stream) 
  *) 

  = BEGIN (* PutText *) 
      LOCK Stream 
      DO LOCK Stream . Sink 
        DO 
          UnsafeUniWr . FastPutText ( Stream , String ) 
        END (* LOCK *) 
      END (* LOCK *) 
    END PutText  

; BEGIN (* UniWr *) 
  END UniWr 
. 

