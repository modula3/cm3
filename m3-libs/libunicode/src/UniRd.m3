MODULE UniRd 

(* Synchronized reader for character stream with one of several encodings. *) 

; IMPORT Rd 
; FROM Rd IMPORT EndOfFile , Failure 
; IMPORT RdClass 
; FROM Thread IMPORT Alerted 
; IMPORT UniCodec 
; FROM UniCodec IMPORT IsBE 
; FROM UniCodec IMPORT Widechar  
; FROM UniEncoding IMPORT Encoding  
; IMPORT UniRdClass 
; IMPORT UnsafeUniCodec 
; IMPORT UnsafeUniRd 
; IMPORT Word 

; REVEAL RdClass . Private <: MUTEX 

(* EXPORTED: *) 
; PROCEDURE Init ( Stream : T ; Source : Rd . T ; Enc : Encoding ) : T  
  (* Initialize Stream to write to Source in encoding Dec, then return it. *) 

  = BEGIN 
      LOCK Stream 
      DO 
        Stream . Source := Source 
      ; Stream . DecWideChar := UnsafeUniCodec . DecTable [ IsBE , Enc ] 
      ; Stream . Index := 0 
      ; Stream . MaxBytesPerChar := UniCodec . MaxBytesPerChar ( Enc ) 
      ; Stream . Enc := Enc 
      ; Stream . HasPostponedWCh := FALSE  
      ; RETURN Stream  
      END (* LOCK *) 
    END Init 

(* EXPORTED: *) 
; PROCEDURE New ( Source : Rd . T ; Enc : Encoding ) : T 
  (* Equivalent to Init(NEW(T),Source,Enc) *) 

  = BEGIN 
      RETURN Init ( NEW ( T ) , Source , Enc ) 
    END New 

(* EXPORTED: *) 
; PROCEDURE Source ( Stream : T ) : Rd . T 
  (* The Rd.T used by Stream. *) 

  = BEGIN
      LOCK Stream 
      DO RETURN Stream . Source 
      END (* LOCK *) 
    END Source 

(* EXPORTED: *) 
; PROCEDURE Enc ( Stream : T ) : Encoding 
  (* The encoding used by Stream. *) 

  = BEGIN
      LOCK Stream 
      DO RETURN Stream . Enc 
      END (* LOCK *) 
    END Enc

(* EXPORTED: *) 
; PROCEDURE EOF ( Stream : T ) : BOOLEAN 
  RAISES { Failure , Alerted } 
  (* TRUE iff Stream is at end-of-file. *) 

  = BEGIN 
      LOCK Stream 
      DO LOCK Stream . Source 
        DO RETURN UnsafeUniRd . FastEOF ( Stream )  
        END (* LOCK *) 
      END (* LOCK *) 
    END EOF 

(* EXPORTED: *) 
; PROCEDURE CharsReady ( Stream : T ) : CARDINAL 
  RAISES { Failure } 
  (* A number of characters that can be read without indefinite waiting.
     The EOF counts as one "character" here.  This number may very pessimistic. 
  *) 

  = BEGIN
      LOCK Stream 
      DO LOCK Stream . Source 
        DO RETURN UnsafeUniRd . FastCharsReady ( Stream )  
        END (* LOCK *) 
      END (* LOCK *) 
    END CharsReady  

(* EXPORTED: *) 
; PROCEDURE GetWideChar ( Stream : T ) : Widechar  
  RAISES { EndOfFile , Failure , Alerted } 
  (* Decode, consume, and return a character from Source(Stream), 
     using Enc(Stream) 
  *) 
  
  = BEGIN 
      LOCK Stream 
      DO LOCK Stream . Source 
        DO RETURN UnsafeUniRd . FastGetWideChar ( Stream ) 
        END (* LOCK *) 
      END (* LOCK *) 
    END GetWideChar 

(* EXPORTED: *) 
; PROCEDURE UnGetWideChar ( Stream : T ; Wch : Widechar ) 
  (* Push Wch onto the front of Stream, where the next attempt to decode a
     character from Stream will fetch it, prior to decoding from the 
     remainder of Stream.  Discard any previously ungotten but not-refetched
     WIDECHAR. 
  *) 
  (* WARNING! Currently unimplemented.  A NOOP *) 
  
  = BEGIN
      LOCK Stream 
      DO LOCK Stream . Source 
        DO UnsafeUniRd . FastUnGetWideChar ( Stream , Wch )  
        END (* LOCK *) 
      END (* LOCK *) 
    END UnGetWideChar 

(* EXPORTED: *) 
; PROCEDURE GetWideSub ( Stream : T ; VAR (*OUT*) ArrWch : ARRAY OF Widechar ) 
  : CARDINAL
  RAISES { Failure , Alerted } 
  (* Decode and consume characters from Source(Stream), using Enc(Stream), 
     storing them into ArrWch, until Source(Stream) is at end-of-file, or ArrWch
     is filled.  Return the actual number of decoded characters stored 
     into ArrWch. *)  

  = BEGIN 
      LOCK Stream 
      DO LOCK Stream . Source 
        DO 
          RETURN UnsafeUniRd . FastGetWideSub ( Stream , ArrWch ) 
        END (* LOCK *) 
      END (* LOCK *) 
    END GetWideSub 

(* EXPORTED: *) 
; PROCEDURE GetSub ( Stream : T ; VAR (*OUT*) ArrCh : ARRAY OF CHAR ) : CARDINAL
  RAISES { Range , Failure , Alerted } 
  (* Decode and consume characters from Source(Stream), using Enc(Stream), 
     storing them into ArrCh, until Source(Stream) is at end-of-file, or ArrCh
     is filled, or a decoded character value is not in CHAR.  In the latter 
     case, consume but do not store the problem character and raise
     Range(Wch,N), where Wch is the problem character and N is the number of 
     previous characters stored.  Otherwise, return the actual number of 
     decoded characters stored into ArrCh. *)  

  = BEGIN 
      LOCK Stream 
      DO LOCK Stream . Source 
        DO 
          RETURN UnsafeUniRd . FastGetSub ( Stream , ArrCh ) 
        END (* LOCK *) 
      END (* LOCK *) 
    END GetSub 

(* EXPORTED: *) 
; PROCEDURE GetWideSubLine 
    ( Stream : T ; VAR (*OUT*) ArrWch : ARRAY OF Widechar ) 
  : CARDINAL
  RAISES { Failure , Alerted } 
  (* Decode and consume characters from Source(Stream), using Enc(Stream), 
     storing them into ArrCh, until Source(Stream) is at end-of-file, or ArrCh
     is filled, or an end-of-line sequence has been read and stored.  
     Return the actual number of decoded characters stored into ArrWch.  
     Include any end-of-line sequence in the returned count and store it in
     ArrWch. 

     Consistent with the Unicode standard, an end-of-line consists of any of:

       LF =  W'\x000A' = W'\n'  
       CR =  W'\x000D' = W'\r'  
       CR immediately followed by LF 
       FF =  W'\x000C' = W'\f'  
       VT =  W'\x0009' = W'\t'  
       NEL = W'\x0085'  
       LS =  W'\x2028'  
       PS =  W'\x2029'  

     If only one character of a two-character end-of-line sequence would fit 
     in ArrWch, leave both unstored and unconsumed. 
  *) 

  = BEGIN 
      LOCK Stream 
      DO LOCK Stream . Source 
        DO 
          RETURN UnsafeUniRd . FastGetWideSubLine ( Stream , ArrWch ) 
        END (* LOCK *) 
      END (* LOCK *) 
    END GetWideSubLine 

(* EXPORTED: *) 
; PROCEDURE GetSubLine 
    ( Stream : T ; VAR (*OUT*) ArrCh : ARRAY OF CHAR ) : CARDINAL
  RAISES { Range , Failure , Alerted } 
  (* Like GetWideSubLine, but return the characters in an ARRAY OF CHAR,
     raising Range({Wch,Loc}) if an otherwise to-be-returned character 
     is not in CHAR, where Wch is the out-of-range character,
     and Loc is the number of characters stored.  
  *) 

  = BEGIN (* GetSubLine *) 
      LOCK Stream 
      DO LOCK Stream . Source 
        DO 
          RETURN UnsafeUniRd . FastGetSubLine ( Stream , ArrCh  ) 
        END (* LOCK *) 
      END (* LOCK *) 
    END GetSubLine 

(* EXPORTED: *) 
; PROCEDURE GetText ( Stream : T ; Len : CARDINAL ) : TEXT  
  RAISES { Failure , Alerted }
  (* Decode and consume  characters from Source(Stream), using Enc(Stream), 
     until Len characters have been decoded or Source(Stream) is at 
     end-of-file.  Return the decoded characters as a TEXT. 
  *) 

  = BEGIN (* GetText *) 
      LOCK Stream 
      DO LOCK Stream . Source 
        DO 
          RETURN UnsafeUniRd . FastGetText ( Stream , Len ) 
        END (* LOCK *) 
      END (* LOCK *) 
    END GetText  

(* EXPORTED: *) 
; PROCEDURE GetLine ( Stream : T ) : TEXT 
  RAISES { EndOfFile , Failure , Alerted }
  (* Like GetWideSubLine, but return the decoded string in a TEXT, with no
     size limit.  Unlike Rd.GetLine, do include the end-of-line sequence,
     if it exists in Stream, at the end of the returned TEXT.  You may need
     this to know which EOL sequence it was.  
  *) 

  = BEGIN (* GetLine *) 
      LOCK Stream 
      DO LOCK Stream . Source 
        DO 
          RETURN UnsafeUniRd . FastGetLine ( Stream ) 
        END (* LOCK *) 
      END (* LOCK *) 
    END GetLine  

(* EXPORTED: *) 
; PROCEDURE Index ( Stream : T ) : Word . T  
  (* Number of Unicode characters that have been read from Stream.
     (Not fixed-sized code units.)  May overflow by wrapping. *) 

  = BEGIN 
      LOCK Stream 
      DO RETURN UnsafeUniRd . FastIndex ( Stream ) 
      END (* LOCK *) 
    END Index 

(* EXPORTED: *) 
; PROCEDURE AvgBytesPerChar ( Stream : T ) : CARDINAL 
  (* Average number of encoded bytes per character, of what has been read. 
     Zero if nothing read. *) 

  = BEGIN (* AvgBytesPerChar *) 
      LOCK Stream 
      DO LOCK Stream . Source 
        DO 
          RETURN UnsafeUniRd . FastAvgBytesPerChar ( Stream ) 
        END (* LOCK *) 
      END (* LOCK *) 
    END AvgBytesPerChar 

; BEGIN (* UniRd *) 
  END UniRd 
. 

