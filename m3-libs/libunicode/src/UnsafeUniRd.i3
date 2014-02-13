INTERFACE UnsafeUniRd 

(* Unsynchronized reader for character stream with one of several encodings. *) 

; FROM Thread IMPORT Alerted 
; FROM UniCodec IMPORT Widechar  
; IMPORT UniRd 
; FROM UniRd IMPORT Range 
; IMPORT Rd 
; FROM Rd IMPORT EndOfFile , Failure 
; IMPORT Word 

; CONST EOLLen = 1 (* OSConfig . EOLLen *) 

; PROCEDURE FastEOF ( Stream : UniRd . T ) : BOOLEAN 
  RAISES { Failure , Alerted } 
  (* TRUE iff Stream is at end-of-file. *) 
  (* PRE: Stream and Stream.Source are locked. *) 

; PROCEDURE FastCharsReady ( Stream : UniRd . T ) : CARDINAL 
  RAISES { Failure } 
  (* A number of characters that can be read without indefinite waiting.
     The EOF counts as one "character" here.  This number may very pessimistic. 
  *) 
  (* PRE: Stream and Stream.Source are locked. *) 

; PROCEDURE FastGetWideChar ( Stream : UniRd . T ) : Widechar  
  RAISES { EndOfFile , Failure , Alerted } 
  (* Decode, consume, and return a character from Source(Stream), 
     using Enc(Stream) 
  *) 
  (* PRE: Stream and Stream.Source are locked. *) 

; CONST (* PROCEDURE *) FastGetChar = FastGetWideChar 
  (* With return by value and CHAR assignable to WIDECHAR, only one procedure 
     is needed. *) 

; PROCEDURE FastUnGetWideChar ( Stream : UniRd . T ; Wch : Widechar ) 
  (* Push Wch onto the front of Stream, where the next attempt to decode a
     character from Stream will fetch it, prior to decoding from the 
     remainder of Stream.  Discard any previously ungotten but not-refetched
     WIDECHAR. 
  *) 
  (* PRE: Stream and Stream.Source are locked. *) 
  (* WARNING! Currently unimplemented.  A NOOP *) 

; PROCEDURE FastGetWideSub 
    ( Stream : UniRd . T ; VAR (*OUT*) ArrWch : ARRAY OF Widechar ) 
  : CARDINAL
  RAISES { Failure , Alerted } 
  (* Decode and consume characters from Source(Stream), using Enc(Stream), 
     storing them into ArrWch, until Source(Stream) is at end-of-file, or ArrWch
     is filled.  Return the actual number of decoded characters stored 
     into ArrWch. 
  *)  
  (* PRE: Stream and Stream.Source are locked. *) 

; PROCEDURE FastGetSub 
    ( Stream : UniRd . T ; VAR (*OUT*) ArrCh : ARRAY OF CHAR ) : CARDINAL
  RAISES { Range , Failure , Alerted } 
  (* Decode and consume characters from Source(Stream), using Enc(Stream), 
     storing them into ArrCh, until Source(Stream) is at end-of-file, or ArrCh
     is filled, or a decoded character value is not in CHAR.  In the latter 
     case, consume but do not store the problem character and raise
     Range(Wch,N), where Wch is the problem character and N is the number of 
     previous characters stored.  Otherwise, return the actual number of 
     decoded characters stored into ArrCh. 
  *)  
  (* PRE: Stream and Stream.Source are locked. *) 

; PROCEDURE FastGetWideSubLine 
    ( Stream : UniRd . T ; VAR (*OUT*) ArrWch : ARRAY OF Widechar ) 
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
  (* PRE: Stream and Stream.Source are locked. *) 

; PROCEDURE FastGetSubLine 
    ( Stream : UniRd . T ; VAR (*OUT*) ArrCh : ARRAY OF CHAR ) 
  : CARDINAL
  RAISES { Range , Failure , Alerted } 
  (* Like FastGetWideSubLine, but return the characters in an ARRAY OF CHAR,
     raising Range({Wch,Loc}) if an otherwise to-be-returned character 
     is not in CHAR, where Wch is the out-of-range character,
     and Loc is the number of characters stored.  
  *) 
  (* PRE: Stream and Stream.Source are locked. *) 

; PROCEDURE FastGetText ( Stream : UniRd . T ; Len : CARDINAL ) : TEXT 
  RAISES { Failure , Alerted }
  (* Decode and consume  characters from Source(Stream), using Enc(Stream), 
     until Len characters have been decoded or Source(Stream) is at 
     end-of-file.  Return the decoded characters as a TEXT. 
  *) 
  (* PRE: Stream and Stream.Source are locked. *) 

; CONST (* PROCEDURE *) FastGetWideText = FastGetText 
  (* Wide/narrow is hidden inside TEXT.  Either procedure does the same. *) 

; PROCEDURE FastGetLine ( Stream : UniRd . T ) : TEXT 
  RAISES { EndOfFile , Failure , Alerted }
  (* Like FastGetWideSubLine, but return the decoded string in a TEXT, with no
     size limit.  Unlike Rd.GetLine, do include the end-of-line sequence,
     if it exists in Stream, at the end of the returned TEXT.  You may need
     this to know which EOL sequence it was.  
  *) 
  (* PRE: Stream and Stream.Source are locked. *) 

; PROCEDURE FastIndex ( Stream : UniRd . T ) : Word . T  
  (* Number of Unicode characters that have been read from Stream.
     (Not fixed-sized code units.)  May overflow by wrapping. *) 
  (* PRE: Stream is locked, but Stream.Source need not be. *) 

; PROCEDURE FastAvgBytesPerChar ( Stream : UniRd . T ) : CARDINAL 
  (* Average number of encoded bytes per character, of what has been read. 
     Zero if nothing read. 
  *) 
  (* PRE: Stream and Stream.Source are locked. *) 

; PROCEDURE FastLength ( Stream : UniRd . T ) : INTEGER 
  RAISES { Failure , Alerted }
  (* Try to return the length of Stream, in Unicode characters (not code-units.)
     If Stream is closed or intermittent, or there is otherwise insufficient
     information, return -1.  If Stream has a fixed-size encoding, a 
     nonnegative value will be exact.  Otherwise, it will be an estimate.
  *)  
  (* PRE: Stream and Stream.Source are locked. *) 

; END UnsafeUniRd 
. 

