MODULE UnsafeUniRd 

(* Unsynchronized reader for character stream with one of several encodings. *) 

; IMPORT Text 
; FROM Thread IMPORT Alerted 
; IMPORT UniCodec 
; FROM UniCodec IMPORT Widechar 
; FROM UniEncoding IMPORT Encoding 
; IMPORT UniRd 
; FROM UniRd IMPORT Range 
; IMPORT UniRdClass 
; IMPORT UnsafeRd 
; IMPORT Rd 
; FROM Rd IMPORT EndOfFile , Failure 
; IMPORT Word 

(* NOTE 1: Use the LOOPHOLE technique here to convert to a narrower type, 
           while avoiding compiler-generated range check already known by the 
           algorithm to be infallible. *) 
(* NOTE 2: When CHAR<:WIDECHAR, and Widechar=WIDECHAR, could remove unnecessary
           ORD and VAL conversions.  (But maybe it is better to just leave
           as-is. *) 

; CONST LFWch = VAL ( ORD ( W'\n' ) , Widechar ) 
; CONST LFCh = '\n' 
; CONST CRWch = VAL ( ORD ( W'\r' ) , Widechar ) 
; CONST CRCh = '\r' 
; CONST FFWch = VAL ( ORD ( W'\f' ) , Widechar ) 
; CONST VTWch = VAL ( ORD ( W'\t' ) , Widechar ) 
; CONST NELWch = VAL ( ORD ( W'\x0085' ) , Widechar ) 
; CONST LSWch = VAL ( ORD ( W'\x2028' ) , Widechar ) 
; CONST PSWch = VAL ( ORD ( W'\x2029' ) , Widechar ) 
(*2*)   

(* EXPORTED: *) 
; PROCEDURE FastEOF ( Stream : UniRd . T ) : BOOLEAN 
  RAISES { Failure , Alerted } 
  (* TRUE iff Stream is at end-of-file. *) 
  (* PRE: Stream and Stream.Source are locked. *) 

  = BEGIN 
      RETURN UnsafeRd . FastEOF ( Stream . Source )
    END FastEOF 

(* EXPORTED: *) 
; PROCEDURE FastCharsReady ( Stream : UniRd . T ) : CARDINAL 
  RAISES { Failure } 
  (* A number of characters that can be read without indefinite waiting.
     The EOF counts as one "character" here.  This number may very pessimistic. 
  *) 
  (* PRE: Stream and Stream.Source are locked. *) 

  = VAR LSourceBytesReady , LCharsReady , LPostponedCt : CARDINAL 

  ; BEGIN 
      IF Stream . MaxBytesPerChar = 0 
      THEN RETURN 0 
      ELSE 
        LSourceBytesReady := UnsafeRd . FastCharsReady ( Stream . Source )
      ; LCharsReady 
          := ( LSourceBytesReady - 1 (* For EOF *) ) 
             DIV Stream . MaxBytesPerChar 
      ; LPostponedCt := 0 
      ; RETURN LCharsReady + LPostponedCt + 1 (* For EOF *)  
      END (* IF *) 
    END FastCharsReady 

(* EXPORTED: *) 
; PROCEDURE FastGetWideChar ( Stream : UniRd . T ) : Widechar  
  RAISES { EndOfFile , Failure , Alerted } 
  (* Decode, consume, and return a character from Source(Stream), 
     using Enc(Stream) 
  *) 
  (* PRE: Stream and Stream.Source are locked. *) 
  
  = VAR LWch : Widechar

  ; BEGIN 
      IF UnsafeRd . FastEOF ( Stream . Source ) 
      THEN RAISE Rd . EndOfFile 
      ELSE (* Dispatch to appropriate encoding procedure. *) 
        Stream . PrevSourceIndex := UnsafeRd . FastIndex ( Stream . Source ) 
      ; LWch := Stream . DecWideChar ( Stream . Source ) 
      ; INC ( Stream . Index ) 
      ; RETURN LWch 
      END (* IF *) 
    END FastGetWideChar 

(* EXPORTED: *) 
; PROCEDURE FastUnGetCodePoint ( Stream : UniRd . T ) : BOOLEAN (* Succeeded. *) 
  (* Push back the last decoded code point read from Stream, pushing it back
     onto Stream.Source, in encoded form.  This is guaranteed to work only
     if the last operation on Stream was GetWideChar, GetChar, GetWideSub,
     or GetSub, or an UnsafeUniRd.Fast* version thereof.  Result FALSE means 
     the operation did not happen, because of a violation of this condition.
  *) 
  (* PRE: Stream and Stream.Source are locked. *) 
  (* WARNING! Currently unimplemented.  A NOOP.  Always returns FALSE. *) 

  = BEGIN 
(* TODO: Implement this. *) 
(* IMPLEMENTME: *) 
      RETURN FALSE
    END FastUnGetCodePoint 
  
(* TODO: Special-case for when the encoding allows to directly access the
         Rd buffer (ISO8859_1 & CHAR or CM3WC and WIDECHAR) and avoid looping
         through individual characters.  
*)  

(* EXPORTED: *) 
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

  = VAR LI : CARDINAL 
  ; VAR LLast : INTEGER 

  ; BEGIN 
      LLast := LAST ( ArrWch ) 
    ; TRY (* EXCEPT *) 
        LOOP 
          IF LI > LLast OR UnsafeRd . FastEOF ( Stream . Source ) 
          THEN 
            INC ( Stream . Index , LI ) 
          ; RETURN LI 
          ELSE 
            Stream . PrevSourceIndex := UnsafeRd . FastIndex ( Stream . Source )
          ; <* FATAL EndOfFile *> (* Can't happen. *) BEGIN 
              ArrWch [ LI ] := Stream . DecWideChar ( Stream . Source ) 
            END (* Block. *) 
          ; INC ( LI ) 
          END (* IF *) 
        END (* LOOP *) 
      EXCEPT (* From Rd.EOF or Stream.DecWideChar. *) 
      Failure ( Arg ) 
      => INC ( Stream . Index , LI ) 
      ; RAISE Failure ( Arg ) 
      | Alerted 
      => INC ( Stream . Index , LI ) 
      ; RAISE  Alerted 
      END (* EXCEPT *) 
    END FastGetWideSub 

(* EXPORTED: *) 
; PROCEDURE FastGetSub 
    ( Stream : UniRd . T ; VAR (*OUT*) ArrCh : ARRAY OF CHAR ) 
  : CARDINAL
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

  = VAR LI : CARDINAL 
  ; VAR LLast : INTEGER 
  ; VAR LWch : Widechar 

  ; BEGIN    
      LLast := LAST ( ArrCh ) 
    ; TRY (* EXCEPT *)
        LOOP 
          IF LI > LLast OR UnsafeRd . FastEOF ( Stream . Source ) 
          THEN 
            INC ( Stream . Index , LI ) 
          ; RETURN LI 
          ELSE 
            Stream . PrevSourceIndex := UnsafeRd . FastIndex ( Stream . Source )
          ; <* FATAL EndOfFile *> (* Can't happen. *) BEGIN 
              LWch := Stream . DecWideChar ( Stream . Source ) 
            END (* Block. *) 
          ; IF ORD ( LWch ) > ORD ( LAST ( CHAR ) ) 
(*2*) 
            THEN 
              INC ( Stream . Index , LI + 1 ) 
            ; RAISE Range ( UniRd . RangeInfo { LWch , LI } )
            ELSE 
              ArrCh [ LI ] := VAL ( ORD ( LWch ) , CHAR ) 
(*1*) 
(*2*) 
            ; INC ( LI ) 
            END (* IF *) 
          END (* IF *) 
        END (* LOOP *) 
      EXCEPT (* From Rd.EOF or Stream.DecWideChar. *) 
      Failure ( Arg ) 
      => INC ( Stream . Index , LI ) 
      ; RAISE Failure ( Arg ) 
      | Alerted 
      => INC ( Stream . Index , LI ) 
      ; RAISE  Alerted 
      END (* EXCEPT *) 
    END FastGetSub 

; PROCEDURE UnGetStreamChars ( Stream : UniRd . T ; BackToIndex : CARDINAL ) 

  (* We could have up to two Unicode Code points, CR and LF.  If the encoding is
     UTF-32, this would be 8 CHARS we will unget to Stream . Source, and the 
     decoder will have ungotten zero.  For other encodings and these specific 
     code points, the decoder might have ungotten at most one CHAR and we will 
     unget at most 4.  So UnGetCharMulti maximum of 8 will be enough, if nobody 
     else is sneaking around the side of our Stream to Stream.Source and 
     ungetting that way. *) 

  = VAR LCt : [ 1 .. 8 ]

  ; BEGIN 
      LCt := UnsafeRd . FastIndex ( Stream . Source ) - BackToIndex 
    (* Let's do a hard-coded, fixed-size binary search for speed. *) 
    ; IF LCt >= 5 
      THEN 
        <* ASSERT UnsafeRd . FastUnGetCharMulti ( Stream . Source ) *> 
        <* ASSERT UnsafeRd . FastUnGetCharMulti ( Stream . Source ) *> 
        <* ASSERT UnsafeRd . FastUnGetCharMulti ( Stream . Source ) *> 
        <* ASSERT UnsafeRd . FastUnGetCharMulti ( Stream . Source ) *> 
        DEC ( LCt , 4 ) 
      END (* IF *) 
    ; IF LCt >= 3 
      THEN 
        <* ASSERT UnsafeRd . FastUnGetCharMulti ( Stream . Source ) *> 
        <* ASSERT UnsafeRd . FastUnGetCharMulti ( Stream . Source ) *> 
        DEC ( LCt , 2 ) 
      END (* IF *) 
    ; IF LCt = 2 
      THEN 
        <* ASSERT UnsafeRd . FastUnGetCharMulti ( Stream . Source ) *> 
        <* ASSERT UnsafeRd . FastUnGetCharMulti ( Stream . Source ) *> 
      ELSE 
        <* ASSERT UnsafeRd . FastUnGetCharMulti ( Stream . Source ) *> 
      END (* IF *) 
    END UnGetStreamChars 

(* EXPORTED *) 
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

  = VAR LI : CARDINAL 
  ; VAR LLast : INTEGER 
  ; VAR LPrevSourceIndex1 , LPrevSourceIndex2 : CARDINAL 
  ; VAR LWch1 , LWch2 : Widechar 

  ; BEGIN 
      LLast := LAST ( ArrWch ) 
    ; IF LLast < 0 (* Empty ArrWch, no space for anything. *)  
      THEN RETURN 0 
      ELSE 
        LI := 0 
      ; TRY (* EXCEPT *) 
        (* ASSERT: There is space for one character at ArrWch[0]. *) 
          IF UnsafeRd . FastEOF ( Stream . Source ) 
          THEN RETURN 0
          ELSE (* Can and must read a character. *) 
            LPrevSourceIndex1 := UnsafeRd . FastIndex ( Stream . Source )
          ; <* FATAL EndOfFile *> (* Can't happen. *) BEGIN 
              LWch1 := Stream . DecWideChar ( Stream . Source ) 
            END (* Block. *) 
          END (* IF *) 
        (* INVARIANT: LWch1 contains the next char to consider storing. 
                      LPrevSourceIndex1 is its starting CHAR index in Stream.Source.
                      There is space for one character at ArrWch[LI]. *) 
        ; LOOP 
            IF ORD ( LWch1 ) = ORD ( LFWch ) 
               OR ORD ( LWch1 ) = ORD ( NELWch ) 
               OR ORD ( LWch1 ) = ORD ( VTWch ) 
               OR ORD ( LWch1 ) = ORD ( FFWch ) 
               OR ORD ( LWch1 ) = ORD ( PSWch ) 
               OR ORD ( LWch1 ) = ORD ( LSWch ) 
(*2*)
            THEN (* Unambiguously single-char new-line.  Store and return. *)  
              Stream . PrevSourceIndex := LPrevSourceIndex1 
            ; ArrWch [ LI ] := LWch1 
            ; INC ( LI ) 
            ; INC ( Stream . Index , LI ) 
            ; RETURN LI 
            ELSIF ORD ( LWch1 ) = ORD ( CRWch ) 
(*2*)
            THEN (* CR.  Could be start of CRLF. *) 
              IF UnsafeRd . FastEOF ( Stream . Source ) 
              THEN (* CR alone.  Store and return. *) 
                Stream . PrevSourceIndex := LPrevSourceIndex1 
              ; ArrWch [ LI ] := LWch1 
              ; INC ( LI ) 
              ; INC ( Stream . Index , LI ) 
              ; RETURN LI 
              ELSE (* Another code point follows the CR. *) 
                LPrevSourceIndex2 
                  := UnsafeRd . FastIndex ( Stream . Source )
              ; <* FATAL EndOfFile *> (* Can't happen. *) BEGIN 
                  LWch2 := Stream . DecWideChar ( Stream . Source ) 
                END (* Block. *) 
              ; IF ORD ( LWch2 ) = ORD ( LFWch ) 
(*2*)
                THEN (* CR and LF. *) 
                  IF LI < LLast 
                  THEN (* Room for both CR and LF. *)  
                    Stream . PrevSourceIndex := LPrevSourceIndex2 
                    (* ^A subsequent UnGetWideChar will unget only the LF. *) 
                  ; ArrWch [ LI ] := LWch1 
                  ; INC ( LI ) 
                  ; ArrWch [ LI ] := LWch2 
                  ; INC ( LI ) 
                  ; INC ( Stream . Index , LI ) 
                  ; RETURN LI 
                  ELSE (* They won't both fit.  Unget both. *)
                    UnGetStreamChars ( Stream , LPrevSourceIndex1 ) 
                  ; INC ( Stream . Index , LI ) 
                  ; RETURN LI 
                  END (* IF *) 
                ELSE (* CR & non_LF.  Store CR and unget LWch2. *) 
                  Stream . PrevSourceIndex := LPrevSourceIndex1 
                ; ArrWch [ LI ] := LWch1 
                ; INC ( LI ) 
                ; UnGetStreamChars ( Stream , LPrevSourceIndex2 ) 
                ; INC ( Stream . Index , LI ) 
                ; RETURN LI 
                END (* IF *) 
              END (* IF *) 
            ELSE (* No kind of new-line at all. *) 
              ArrWch [ LI ] := LWch1 
            ; INC ( LI ) 
            ; IF LI > LLast OR UnsafeRd . FastEOF ( Stream . Source ) 
              THEN (* We are done W/O a new line. *)  
                Stream . PrevSourceIndex := LPrevSourceIndex1 
              ; INC ( Stream . Index , LI ) 
              ; RETURN LI 
              ELSE (* Time to read another code point. *) 
                LPrevSourceIndex1 
                  := UnsafeRd . FastIndex ( Stream . Source )
              ; <* FATAL EndOfFile *> (* Can't happen. *) BEGIN 
                  LWch1 := Stream . DecWideChar ( Stream . Source ) 
                END (* Block. *) 
              (* And loop. *) 
              END (* IF *) 
            END (* IF *) 
          END (* LOOP *) 
        EXCEPT (* From Rd.EOF or Stream.DecWideChar. *) 
        Failure ( Arg ) 
        => INC ( Stream . Index , LI ) 
        ; RAISE Failure ( Arg ) 
        | Alerted  
        => INC ( Stream . Index , LI ) 
        ; RAISE  Alerted 
        END (* EXCEPT *) 
      END (* IF *) 
    END FastGetWideSubLine 

(* EXPORTED *) 
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

  = VAR LI : CARDINAL 
  ; VAR LLast : INTEGER 
  ; VAR LWch1 , LWch2 : Widechar 
  ; VAR LPrevSourceIndex1 , LPrevSourceIndex2 : CARDINAL 

  ; BEGIN 
      LLast := LAST ( ArrCh ) 
    ; IF LLast < 0 (* Empty ArrCh, no space for anything. *)  
      THEN RETURN 0 
      ELSE 
        LI := 0 
      ; TRY (* EXCEPT *) 
          IF UnsafeRd . FastEOF ( Stream . Source ) 
          THEN RETURN 0 
          ELSE (* Can and must read a character. *) 
            LPrevSourceIndex1 := UnsafeRd . FastIndex ( Stream . Source )
          ; <* FATAL EndOfFile *> (* Can't happen. *) BEGIN 
              LWch1 := Stream . DecWideChar ( Stream . Source ) 
            END (* Block. *) 
          END (* IF *) 
        (* INVARIANT: LWch1 contains the next char to consider storing.
                      LPrevSourceIndex1 is its starting CHAR index in Stream.Source.
                      There is space for one character at ArrWch[LI]. *) 
        ; LOOP 
            IF ORD ( LWch1 ) = ORD ( LFWch ) 
               OR ORD ( LWch1 ) = ORD ( FFWch ) 
               OR ORD ( LWch1 ) = ORD ( VTWch ) 
               OR ORD ( LWch1 ) = ORD ( NELWch ) 
(*2*)
            THEN (* Unambiguously single-char new-line and it's in CHAR.  
                    Store and return. *)  
              Stream . PrevSourceIndex := LPrevSourceIndex1 
            ; ArrCh [ LI ] := VAL ( ORD ( LWch1 ) , CHAR ) 
            ; INC ( LI ) 
            ; INC ( Stream . Index , LI ) 
            ; RETURN LI 
            ELSIF ORD ( LWch1 ) = ORD ( PSWch ) 
                  OR ORD ( LWch1 ) = ORD ( LSWch ) 
(*2*)
            THEN (* Single-char new-line, but not in CHAR. *)  
(* CHECK: Index, PrevSourceIndex *)
              INC ( Stream . Index , LI + 1 ) 
            ; RAISE Range ( UniRd . RangeInfo { LWch1 , LI } )
            ELSIF ORD ( LWch1 ) = ORD ( CRWch ) 
(*2*) 
            THEN (* CR.  Could be start of CRLF. *) 
              IF UnsafeRd . FastEOF ( Stream . Source ) 
              THEN (* CR alone.  Store and return. *) 
                Stream . PrevSourceIndex := LPrevSourceIndex1 
              ; ArrCh [ LI ] := CRCh  
              ; INC ( LI ) 
              ; INC ( Stream . Index , LI ) 
              ; RETURN LI 
              ELSE (* Another code point follows the CR. *) 
                LPrevSourceIndex2 := UnsafeRd . FastIndex ( Stream . Source )
              ; <* FATAL EndOfFile *> (* Can't happen. *) BEGIN 
                  LWch2 := Stream . DecWideChar ( Stream . Source ) 
                END (* Block. *) 
              ; IF ORD ( LWch2 ) = ORD ( LFWch ) 
(*2*)
                THEN (* CR and LF. *) 
                  IF LI < LLast 
                  THEN (* Room for both CR and LF. *)  
                    Stream . PrevSourceIndex := LPrevSourceIndex2 
                    (* ^A subsequent UnGetWideChar will unget only the LF. *) 
                  ; ArrCh [ LI ] := CRCh 
                  ; INC ( LI ) 
                  ; ArrCh [ LI ] := LFCh  
                  ; INC ( LI ) 
                  ; INC ( Stream . Index , LI ) 
                  ; RETURN LI 
                  ELSE (* They won't both fit.  Unget both. *)
                    UnGetStreamChars ( Stream , LPrevSourceIndex1 )  
                  ; INC ( Stream . Index , LI ) 
                  ; RETURN LI 
                  END (* IF *) 
                ELSE (* CR & non_LF.  Store CR and unget LWch2. *) 
                  Stream . PrevSourceIndex := LPrevSourceIndex1 
                ; ArrCh [ LI ] := CRCh  
                ; INC ( LI ) 
                ; UnGetStreamChars ( Stream , LPrevSourceIndex2 )  
                ; INC ( Stream . Index , LI ) 
                ; RETURN LI 
                END (* IF *) 
              END (* IF *) 
            ELSE (* No kind of new-line at all. *) 
              IF ORD ( LWch1 ) > ORD ( LAST ( CHAR ) ) 
(*2*) 
              THEN 
                INC ( Stream . Index , LI + 1 ) 
              ; RAISE Range ( UniRd . RangeInfo { LWch1 , LI } )
              ELSE 
                ArrCh [ LI ] := VAL ( ORD ( LWch1 ) , CHAR ) 
(*1*) 
(*2*) 
              ; INC ( LI ) 
              ; IF LI > LLast OR UnsafeRd . FastEOF ( Stream . Source ) 
                THEN (* We are done W/O a new line. *)  
                  Stream . PrevSourceIndex := LPrevSourceIndex1 
                ; INC ( Stream . Index , LI ) 
                ; RETURN LI 
                ELSE (* Time to read another code point. *) 
                  LPrevSourceIndex1 := UnsafeRd . FastIndex ( Stream . Source )
                ; <* FATAL EndOfFile *> (* Can't happen. *) BEGIN 
                    LWch1 := Stream . DecWideChar ( Stream . Source ) 
                  END (* Block. *) 
                (* And loop. *) 
                END (* IF *) 
              END (* IF *) 
            END (* IF *) 
          END (* LOOP *) 
        EXCEPT (* From Rd.EOF or Stream.DecWideChar. *) 
        Failure ( Arg ) 
        => INC ( Stream . Index , LI ) 
        ; RAISE Failure ( Arg ) 
        | Alerted 
        => INC ( Stream . Index , LI ) 
        ; RAISE  Alerted 
        END (* EXCEPT *) 
      END (* IF *) 
    END FastGetSubLine 

(* EXPORTED *) 
; PROCEDURE FastGetText ( Stream : UniRd . T ; Len : CARDINAL ) : TEXT 
  RAISES { Failure , Alerted }
  (* Decode and consume characters from Source(Stream), using Enc(Stream), 
     until Len characters have been decoded or Source(Stream) is at 
     end-of-file.  Return the decoded characters as a TEXT. 
  *) 
  (* PRE: Stream and Stream.Source are locked. *) 

  = CONST ChunkSize = 512
  ; VAR LChunk : ARRAY [ 0 .. ChunkSize - 1 ] OF Widechar 
  ; VAR LI , LChunkI : CARDINAL 
  ; VAR LResult : TEXT  

  ; BEGIN 
      IF Len = 0 
      THEN RETURN ""  
      ELSE 
        IF UnsafeRd . FastEOF ( Stream . Source )
        THEN RETURN "" 
        ELSE 
          LI := 0 
        ; LChunkI := 0 
        (* And fall through to get some chars.  *) 
        END (* IF *) 
      ; LResult := NIL 
      ; TRY (* EXCEPT *)
          LOOP 
            (* INVARIANT: There is a character to read and length for it. *) 
            IF LChunkI > LAST ( LChunk ) 
            THEN (* Flush already-full chunk and start a new one. *)
              IF LResult = NIL (* Likely. *) 
              THEN 
                LResult := Text . FromWideChars ( LChunk )
              ELSE 
                LResult := LResult & Text . FromWideChars ( LChunk )
              END (* IF *) 
            ; LChunkI := 0 
            END (* IF *)  
          ; Stream . PrevSourceIndex := UnsafeRd . FastIndex ( Stream . Source )
          ; <* FATAL EndOfFile *> (* Can't happen. *) BEGIN  
              LChunk [ LChunkI ] := Stream . DecWideChar ( Stream . Source ) 
            END (* Block. *) 
          ; INC ( LChunkI ) 
          ; INC ( LI ) 
          ; IF LI >= Len OR UnsafeRd . FastEOF ( Stream . Source ) 
            THEN (* We are done. *) 
              IF LChunkI > 0 
              THEN (* Flush partially filled chunk. *)
                INC ( Stream . Index , LI ) 
              ; IF LResult = NIL (* Likely. *) 
                THEN 
                  RETURN 
                    Text. FromWideChars ( SUBARRAY ( LChunk , 0 , LChunkI ) )
                ELSE 
                  INC ( Stream . Index , LI ) 
                ; RETURN  
                    LResult 
                    & Text. FromWideChars ( SUBARRAY ( LChunk , 0 , LChunkI ) )
                END (* IF *) 
              ELSIF LResult = NIL 
              THEN RETURN "" 
              ELSE RETURN LResult 
              END (* IF *) 
         (* ELSE loop *) 
            END (* IF *) 
          END (* LOOP *) 
        EXCEPT (* From Rd.EOF or Stream.DecWideChar. *) 
        Failure ( Arg ) 
        => INC ( Stream . Index , LI ) 
        ; RAISE Failure ( Arg ) 
        | Alerted 
        => INC ( Stream . Index , LI ) 
        ; RAISE  Alerted 
        END (* EXCEPT *) 
      END (* IF *) 
    END FastGetText

(* EXPORTED *) 
; PROCEDURE FastGetLine ( Stream : UniRd . T ) : TEXT 
  RAISES { EndOfFile , Failure , Alerted }
  (* Like FastGetWideSubLine, but return the decoded string in a TEXT, with no
     size limit.  Unlike Rd.GetLine, do include the end-of-line sequence,
     if it exists in Stream, at the end of the returned TEXT.  You may need
     this to know which EOL sequence it was.  
  *) 
  (* PRE: Stream and Stream.Source are locked. *) 

  = VAR LPrevSourceIndex : CARDINAL 
  ; VAR LI , LChunkI : CARDINAL 
  ; VAR LWch1 , LWch2 : Widechar 
  ; VAR LResult : TEXT  
  ; CONST ChunkSize = 512 
  ; VAR LChunk : ARRAY [ 0 .. ChunkSize - 1 ] OF Widechar 

  ; BEGIN 
      LI := 0 
    ; LChunkI := 0 
    ; TRY (* EXCEPT *) 
       Stream . PrevSourceIndex := UnsafeRd . FastIndex ( Stream . Source )
     ; LWch1 := Stream . DecWideChar ( Stream . Source ) 
       (* ^Which could raise EndOfFile. *) 
      ; LResult := NIL 
      (* INVARIANT: LWch1 contains the next char to store. 
                    Stream.PrevSourceIndex is its starting index. *) 
      ; LOOP (* Store LWch1. *)  
          IF LChunkI >= LAST ( LChunk ) 
             (* Allow 2 spaces for possible CR & LF. *)
          THEN (* Flush already-(almost)-full chunk and start a new one. *)
            IF LResult = NIL (* Likely. *) 
            THEN 
              LResult 
                := TextFromWideChars ( SUBARRAY ( LChunk , 0 , LChunkI ) )
            ELSE 
              LResult 
                := LResult 
                   & TextFromWideChars ( SUBARRAY ( LChunk , 0 , LChunkI ) )
            END (* IF *) 
          ; LChunkI := 0 
          END (* IF *)
        ; LChunk [ LChunkI ] := LWch1 
        ; INC ( LChunkI ) 
        ; INC ( LI ) 
        (* Beyond here, LWch1 has been stored in the being-built result. *) 
        ; IF ORD ( LWch1 ) = ORD ( LFWch ) 
             OR ORD ( LWch1 ) = ORD ( NELWch ) 
             OR ORD ( LWch1 ) = ORD ( VTWch ) 
             OR ORD ( LWch1 ) = ORD ( FFWch ) 
             OR ORD ( LWch1 ) = ORD ( PSWch ) 
             OR ORD ( LWch1 ) = ORD ( LSWch ) 
(*2*)
          THEN (* It was an unambiguously single-char new-line. *)  
            EXIT 
          ELSIF ORD ( LWch1 ) = ORD ( CRWch ) 
(*2*)
          THEN (* It was CR.  Could be start of CRLF. *) 
            IF UnsafeRd . FastEOF ( Stream . Source ) 
            THEN (* CR alone. *) 
              EXIT 
            ELSE (* Another char follows the CR. *) 
              LPrevSourceIndex := UnsafeRd . FastIndex ( Stream . Source )
            ; <* FATAL EndOfFile *> (* Can't happen. *) BEGIN 
                LWch2 := Stream . DecWideChar ( Stream . Source ) 
(* WARNING: exception is never raised: Rd.EndOfFile--why? *) 
              END (* Block. *) 
            ; IF ORD ( LWch2 ) = ORD ( LFWch )
              THEN (* CR & LF.  Store the LF. *) 
                Stream . PrevSourceIndex := LPrevSourceIndex  
              ; LChunk [ LChunkI ] := LWch2 (* There will be space. *)  
              ; INC ( LChunkI ) 
              ; INC ( LI ) 
              ; EXIT 
              ELSE (* CR & non-LF.  Unget the non-LF code point. *) 
                UnGetStreamChars ( Stream , LPrevSourceIndex ) 
              ; EXIT 
              END (* IF *) 
            END (* IF *) 
          ELSE (* It was not a new-line of any kind. *) 
            IF UnsafeRd . FastEOF ( Stream . Source ) 
            THEN (* Line has ended at EOF without a new-line sequence. *) 
              EXIT 
            ELSE (* Get another code point. *)  
              Stream . PrevSourceIndex 
                := UnsafeRd . FastIndex ( Stream . Source )
            ; <* FATAL EndOfFile *> (* Can't happen. *) BEGIN 
                LWch1 := Stream . DecWideChar ( Stream . Source ) 
(* WARNING: exception is never raised: Rd.EndOfFile--why? *) 
              END (* Block. *) 
            (* And loop. *) 
            END (* IF *) 
          END (* IF *) 
        END (* LOOP *) 
        (* Flush partially filled chunk. *)
      ; IF LResult = NIL (* Likely. *) 
        THEN 
          INC ( Stream . Index , LI ) 
        ; RETURN TextFromWideChars ( SUBARRAY ( LChunk , 0 , LChunkI ) )
        ELSE 
          INC ( Stream . Index , LI ) 
        ; RETURN  
            LResult 
            & TextFromWideChars ( SUBARRAY ( LChunk , 0 , LChunkI ) )
        END (* IF *) 
      EXCEPT (* From Rd.EOF or Stream.DecWideChar. *) 
      Failure ( Arg ) 
      => INC ( Stream . Index , LI ) 
      ; RAISE Failure ( Arg ) 
      | Alerted 
      => INC ( Stream . Index , LI ) 
      ; RAISE  Alerted 
      END (* EXCEPT *) 
    END FastGetLine 

(* EXPORTED: *) 
; PROCEDURE FastIndex ( Stream : UniRd . T ) : Word . T  
  (* Number of Unicode characters that have been read from Stream.
     (Not fixed-sized code units.)  May overflow by wrapping. *) 
  (* PRE: Stream is locked, but Stream.Source need not be. *) 

  = BEGIN 
      RETURN Stream . Index 
    END FastIndex 

(* EXPORTED: *) 
; PROCEDURE FastAvgBytesPerChar ( Stream : UniRd . T ) : CARDINAL 
  (* Average number of encoded bytes per character, of what has been read. 
     Zero if nothing read. *) 
  (* PRE: Stream and Stream.Source are locked. *) 

  = VAR LByteIndex : CARDINAL 
  ; VAR LCharIndex : Word . T
(* REVIEW? What to do about overflows in these values? *)  

  ; BEGIN 
      LByteIndex := UnsafeRd . FastIndex ( Stream . Source ) 
    ; LCharIndex := Stream . Index 
    ; IF LCharIndex = 0 
      THEN RETURN 0 
      ELSE RETURN LByteIndex DIV LCharIndex 
      END (* IF *)  
    END FastAvgBytesPerChar 

(* TODO: Get these constants from some meaningful real data. *) 

; CONST MinBytesForAvg2 = 40 
  (* ^Min bytes to consider the stream average meaningful, 2-byte code units. *)
; CONST AvgBytesPerChar2 = 2.1
  (* ^An average for streams in general, 2-byte code units.  *) 
; CONST MinBytesForAvg1 = 40 
; CONST AvgBytesPerChar1 = 1.2

(* EXPORTED: *) 
; PROCEDURE FastLength ( Stream : UniRd . T ) : INTEGER 
  RAISES { Failure , Alerted }
  (* Try to return the length of Stream, in Unicode characters (not code-units.)
     If Stream is closed or intermittent, or there is otherwise insufficient
     information, return -1.  If Stream has a fixed-size encoding, a 
     nonnegative value will be exact.  Otherwise, it will be an estimate.
  *)  
  (* PRE: Stream and Stream.Source are locked. *) 

  = VAR LByteLength : INTEGER 
  ; VAR LByteIndex : CARDINAL 
  ; VAR LBytesPerChar : REAL 

  ; BEGIN 
      IF UnsafeRd . FastClosed ( Stream . Source ) THEN RETURN - 1 END
    ; IF UnsafeRd . FastIntermittent ( Stream . Source ) THEN RETURN - 1 END
    ; CASE Stream . Enc 
      OF Encoding . Null 
      , Encoding . Internal
        => RETURN - 1 

      | Encoding . ISO8859_1
        => RETURN UnsafeRd . FastLength ( Stream . Source )  

      | Encoding . CM3WC 
        => RETURN UnsafeRd . FastLength ( Stream . Source ) DIV 2 

      | Encoding . UTF32 
      , Encoding . UTF32BE 
      , Encoding . UTF32LE
        => RETURN UnsafeRd . FastLength ( Stream . Source ) DIV 4 

      | Encoding . UTF8 
        => LByteLength 
             := UnsafeRd . FastLength ( Stream . Source ) - 1 (* Null byte. *)
        ; IF LByteLength = 0 THEN RETURN 1 (* Null character. *) END (* IF *) 
        ; LByteIndex := UnsafeRd . FastIndex ( Stream . Source ) 
        ; IF LByteIndex < MinBytesForAvg1  
          THEN LBytesPerChar := AvgBytesPerChar1  
          ELSE LBytesPerChar := FLOAT ( FastAvgBytesPerChar ( Stream ) )  
          END (* IF *)  
        ; RETURN CEILING ( FLOAT ( LByteLength ) / LBytesPerChar , INTEGER )
                 + 1 (* Null character. *)  

      | Encoding . UCS2 
      , Encoding . UCS2LE 
      , Encoding . UCS2BE 
      , Encoding . UTF16 
      , Encoding . UTF16LE 
      , Encoding . UTF16BE 
        => LByteLength := UnsafeRd . FastLength ( Stream . Source ) 
        ; LByteIndex := UnsafeRd . FastIndex ( Stream . Source ) 
        ; IF LByteIndex < MinBytesForAvg2  
          THEN LBytesPerChar := AvgBytesPerChar2  
          ELSE LBytesPerChar := FLOAT ( FastAvgBytesPerChar ( Stream ) )  
          END (* IF *)  
        ; RETURN CEILING ( FLOAT ( LByteLength ) / LBytesPerChar , INTEGER )
      END (* CASE *) 
    END FastLength 

(* Testing: *) 

; PROCEDURE TextFromWideChars ( READONLY ArrWch : ARRAY OF Widechar ) : TEXT 
  (* Simulate Text.FromWideChars.  Temporary, for testing, while 
     Widechar # WIDECHAR. *) 

  = VAR LLast : CARDINAL 
  ; VAR ArrWC : ARRAY [ 0 .. 1023 ] OF WIDECHAR 
; VAR LWch : Widechar 
; VAR LWC : WIDECHAR
; VAR LOrd : INTEGER 
; VAR LReval : WIDECHAR 

  ; BEGIN 
      LLast := LAST ( ArrWch ) 
    ; FOR RI := 0 TO LLast  
      DO 

  LWch := ArrWch [ RI ] 
; LOrd := ORD ( LWch ) 
; LWC := VAL ( LOrd , WIDECHAR ) 
;
        ArrWC [ RI ] := VAL ( ORD ( ArrWch [ RI ] ) , WIDECHAR )
; LReval := ArrWC [ RI ] 
      END (* FOR *) 
    ; RETURN Text . FromWideChars ( SUBARRAY ( ArrWC , 0 , LLast + 1 ) ) 
    END TextFromWideChars 

; BEGIN (* UnsafeUniRd *) 
  END UnsafeUniRd 
. 

