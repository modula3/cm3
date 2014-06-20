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
; IMPORT UnsafeRd 
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
      ; Stream . PrevSourceIndex := 0 (* Dead, defensive. *)  
      ; Stream . MaxBytesPerChar := UniCodec . MaxBytesPerChar ( Enc ) 
      ; Stream . Enc := Enc
      ; Stream . UngetByteCt := 0  
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
; PROCEDURE UnGetWideChar ( Stream : T ) : BOOLEAN (* Succeeded. *) 
  (* Push back the last decoded character read from Stream, pushing it back
     onto Stream.Source, in encoded form.  This is guaranteed to work only
     if the last operation on Stream was GetWideChar, GetChar, GetWideSub,
     or GetSub or an UnsafeUniRd.Fast* version thereof.  Result FALSE means 
     the operation did not happen, because of a violation of this condition.
  *) 
  (* WARNING! Currently unimplemented.  A NOOP.  Always returns FALSE. *) 
  
  = BEGIN
      LOCK Stream 
      DO LOCK Stream . Source 
        DO RETURN UnsafeUniRd . FastUnGetWideChar ( Stream )  
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
; PROCEDURE Close ( Stream : T ) RAISES { Failure , Alerted }
  (* Close Stream and its source. *) 
 
  = BEGIN 
      LOCK Stream 
      DO LOCK Stream . Source 
        DO 
          UnsafeRd . FastClose ( Stream . Source )
        (* We'll leave fields of Stream unchanged, for maybe debugging help.
           Stream can't be used while Source is closed, and can't be reopened 
           other than by UniRd.Init, which will reinitialize the fields. *) 
        END (* LOCK *) 
      END (* LOCK *) 
    END Close 

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

(* EXPORTED: *) 
; PROCEDURE Length ( Stream : T ) : INTEGER RAISES { Failure , Alerted }
  (* Try to return the length of Stream, in Unicode characters (not code-units.)
     If Stream is closed or intermittent, or there is otherwise insufficient
     information, return -1.  If Stream has a fixed-size encoding, a 
     nonnegative value will be exact.  Otherwise, it will be an estimate.
  *)  

  = BEGIN 
      LOCK Stream DO
        LOCK Stream . Source DO 
          RETURN UnsafeUniRd . FastLength ( Stream ) 
        END (* LOCK *) 
      END (* LOCK *) 
    END Length 

(* EXPORTED: *) 
; PROCEDURE Intermittent( Stream : T ) : BOOLEAN RAISES { }

  = BEGIN RETURN Rd . Intermittent ( Stream . Source ) 
    END Intermittent

(* EXPORTED: *) 
; PROCEDURE Seekable( Stream : T ) : BOOLEAN RAISES { }

  = BEGIN RETURN Rd . Seekable ( Stream . Source ) 
    END Seekable

(* EXPORTED: *) 
; PROCEDURE Closed( Stream : T ) : BOOLEAN RAISES { }

  = BEGIN RETURN Rd . Closed ( Stream . Source ) 
    END Closed

; BEGIN (* UniRd *) 
  END UniRd 
. 

