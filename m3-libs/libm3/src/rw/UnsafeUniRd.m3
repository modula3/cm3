MODULE UnsafeUniRd 

(* Unsynchronized reader for character stream with one of several encodings. *) 

; IMPORT Text 
; FROM Thread IMPORT Alerted 
; IMPORT UniCodec 
; FROM UniCodec IMPORT Widechar  
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
(* NOTE 2: When CHAR<:WIDECHAR, and Widechar=WIDECHAR, remove unnecessary
           ORD and VAL conversions. *) 
(* NOTE 3: Here is a place postponement is done.  3a: of one character. 
           3b: of two characters. *) 

; CONST LFWch = VAL ( ORD ( W'\n' ) , Widechar ) 
; CONST LFCh = '\n' 
; CONST CRWch = VAL ( ORD ( W'\r' ) , Widechar ) 
; CONST CRCh = '\r' 
; CONST FFWch = VAL ( ORD ( W'\f' ) , Widechar ) 
; CONST VTWch = VAL ( ORD ( W'\t' ) , Widechar ) 
; CONST NELWch = VAL ( ORD ( W'\x0085' ) , Widechar ) 
; CONST LSWch = VAL ( ORD ( W'\x2028' ) , Widechar ) 
; CONST PSWch = VAL ( ORD ( W'\x2029' ) , Widechar ) 
; CONST CRLFWch = VAL ( ORD (  W'\xF8FF' ) , Widechar )
(* U+E000 .. U+F8FF are reserved by Unicode for internal uses, the upper end
   for "Corporate-wide" private internal use, lower for application use.  
   We use U+F8FF as a code point in UniRd.T.PostponedWCh to mean both 
   CR and LF are postponed. *) 
(*2*)   

; CONST CRText = W"\r" 
; CONST CRLFText = W"\r\n" 

(* EXPORTED: *) 
; PROCEDURE FastEOF ( Stream : UniRd . T ) : BOOLEAN 
  RAISES { Failure , Alerted } 
  (* TRUE iff Stream is at end-of-file. *) 
  (* PRE: Stream and Stream.Source are locked. *) 

  = BEGIN 
      RETURN 
        NOT Stream . HasPostponedWCh 
        AND UnsafeRd . FastEOF ( Stream . Source )
    END FastEOF 

(* EXPORTED: *) 
; PROCEDURE FastCharsReady ( Stream : UniRd . T ) : CARDINAL 
  RAISES { Failure } 
  (* A number of characters that can be read without indefinite waiting.
     The EOF counts as one "character" here.  This number may very pessimistic. 
  *) 
  (* PRE: Stream is locked, but not Stream.Source. *) 

  = VAR LSourceBytesReady , LCharsReady , LPostponedCt : CARDINAL 

  ; BEGIN 
      IF Stream . MaxBytesPerChar = 0 
      THEN RETURN 0 
      ELSE 
(* TODO: An already-locked CharsReady (UnsafeRd.FastCharsReady) would allow
         this to be called with Stream.Source already locked, which would
         make for a more consistent interface in UnsafeUniRd.i3. *) 
        LSourceBytesReady := Rd . CharsReady ( Stream . Source ) 
      ; LCharsReady 
          := ( LSourceBytesReady - 1 (* For EOF *) ) 
             DIV Stream . MaxBytesPerChar 
      ; IF Stream . HasPostponedWCh 
        THEN 
          IF Stream . PostponedWCh = CRLFWch 
          THEN LPostponedCt := 2
          ELSE LPostponedCt := 1
          END (* IF *) 
        ELSE LPostponedCt := 0 
        END (* IF *) 
      ; RETURN LCharsReady + LPostponedCt + 1 (* For EOF *)  
      END (* IF *) 
    END FastCharsReady 

(* EXPORTED: *) 
; PROCEDURE FastUnGetWideChar ( Stream : UniRd . T ; Wch : Widechar ) 
  (* Push Wch onto the front of Stream, where the next attempt to decode a
     character from Stream will fetch it, prior to decoding from the 
     remainder of Stream.  Discard any previously ungotten but not-refetched
     WIDECHAR. 
  *) 
  (* PRE: Stream and Stream.Source are locked. *) 
  (* WARNING! Currently unimplemented.  A NOOP *) 

  = BEGIN 
(* IMPLEMENTME: *) 
    END FastUnGetWideChar 
  
(* EXPORTED: *) 
; PROCEDURE FastGetWideChar ( Stream : UniRd . T ) : Widechar  
  RAISES { EndOfFile , Failure , Alerted } 
  (* Decode, consume, and return a character from Source(Stream), 
     using Enc(Stream) 
  *) 
  (* PRE: Stream and Stream.Source are locked. *) 
  
  = VAR LWch : Widechar 

  ; BEGIN 
      IF Stream . HasPostponedWCh 
      THEN 
        IF Stream . PostponedWCh = CRLFWch 
        THEN 
          Stream . PostponedWCh := LFWch  
        ; INC ( Stream . Index ) 
        ; RETURN CRWch 
        ELSE 
          Stream . HasPostponedWCh := FALSE 
        ; INC ( Stream . Index ) 
        ; RETURN Stream . PostponedWCh
        END (* IF *) 
      ELSE (* Dispatch to appropriate encoding procedure. *) 
        LWch := Stream . DecWideChar ( Stream . Source ) 
      ; INC ( Stream . Index ) 
      ; RETURN LWch 
      END (* IF *) 
    END FastGetWideChar 

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
    ; IF Stream . HasPostponedWCh 
      THEN 
        IF LLast < 0 (* Empty ArrWch, no space for anything. *)  
        THEN RETURN 0 
        ELSE 
          IF Stream . PostponedWCh = CRLFWch 
          THEN (* CR & LF have been postponed. *)  
            ArrWch [ 0 ] := CRWch 
          ; LI := 1  
          ; IF LLast > 0  
            THEN (* The LF will fit too. *) 
              ArrWch [ 1 ] := LFWch 
            ; LI := 2  
            ; Stream . HasPostponedWCh := FALSE 
            (* And fall through for possibly more chars. *) 
            ELSE (* Only the CR did fit. *)  
              Stream . PostponedWCh := LFWch  
            ; INC ( Stream . Index ) 
            ; RETURN 1  
            END (* IF *) 
          ELSE (* Some other char was postponed. *)  
            ArrWch [ 0 ] := Stream . PostponedWCh 
          ; LI := 1  
          ; Stream . HasPostponedWCh := FALSE 
          (* And fall through for possibly more chars. *) 
          END (* IF *) 
        END (* IF *) 
      END (* IF *) 
    ; TRY (* EXCEPT *) 
        LOOP 
          IF LI > LLast OR UnsafeRd . FastEOF ( Stream . Source ) 
          THEN 
            INC ( Stream . Index , LI ) 
          ; RETURN LI 
          ELSE 
            <* FATAL EndOfFile *> (* Can't happen. *) BEGIN 
              ArrWch [ LI ] := Stream . DecWideChar ( Stream . Source ) 
            END (* Block. *) 
          ; INC ( LI ) 
          END (* IF *) 
        END (* LOOP *) 
      EXCEPT (* Rd.EOF or Stream.DecWideChar. *) 
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
    ; IF Stream . HasPostponedWCh 
      THEN 
        IF LLast < 0 (* Empty ArrCh, no space for anything. *)  
        THEN RETURN 0 
        ELSE 
          IF Stream . PostponedWCh = CRLFWch 
          THEN (* CR & LF have been postponed. *)  
            ArrCh [ 0 ] := CRCh 
          ; LI := 1  
          ; IF LLast > 0  
            THEN (* The LF will fit too. *) 
              ArrCh [ 1 ] := LFCh 
            ; LI := 2 
            ; Stream . HasPostponedWCh := FALSE 
            (* And fall through for possibly more chars. *) 
            ELSE (* Only the CR did fit. *)  
              Stream . PostponedWCh := LFWch  
            ; INC ( Stream . Index ) 
            ; RETURN 1  
            END (* IF *) 
          ELSE (* Some other char was postponed. *)  
            Stream . HasPostponedWCh := FALSE 
          ; IF ORD ( Stream . PostponedWCh ) > ORD ( LAST ( CHAR ) ) 
(*2*) 
            THEN 
              INC ( Stream . Index ) 
            ; RAISE Range ( UniRd . RangeInfo { Stream . PostponedWCh , 0 } )
            ELSE 
              ArrCh [ 0 ] := VAL ( ORD ( Stream . PostponedWCh ) , CHAR )
(*1*) 
(*2*) 
            ; LI := 1  
            (* And fall through for possibly more chars. *) 
            END (* IF *) 
          END (* IF *) 
        END (* IF *) 
      END (* IF *) 
    ; TRY (* EXCEPT *)
        LOOP 
          IF LI > LLast OR UnsafeRd . FastEOF ( Stream . Source ) 
          THEN 
            INC ( Stream . Index , LI ) 
          ; RETURN LI 
          ELSE 
            <* FATAL EndOfFile *> (* Can't happen. *) BEGIN 
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
      EXCEPT (* Rd.EOF or Stream.DecWideChar. *) 
      Failure ( Arg ) 
      => INC ( Stream . Index , LI ) 
      ; RAISE Failure ( Arg ) 
      | Alerted 
      => INC ( Stream . Index , LI ) 
      ; RAISE  Alerted 
      END (* EXCEPT *) 
    END FastGetSub 

(* EXPORTED *) 
; PROCEDURE FastGetWideSubLine 
    ( Stream : UniRd . T ; VAR (*OUT*) ArrWch : ARRAY OF Widechar ) 
  : CARDINAL
  RAISES { Failure , Alerted } 
  (* Decode and consume characters from Source(Stream), using Enc(Stream), 
     storing them into ArrWch, until Source(Stream) is at end-of-file, a
     new-line sequence has been stored, or ArrWch is filled.  
     Return the actual number of decoded characters stored into ArrWch. 

     A new-line sequence is one of CR, LF, CR&LF, NEL, VT, FF, LS, or PS
     (See Unicode 5.8).  In the unusual case that there is a CR&LF sequence,
     but only the CR will fit in ArrWch, do not store either character and 
     return with one unused space in ArrWch.  The client can distinguish this
     by ArrWch not full, not EOF, and no new-line character at the end.   
      
  *)  
  (* PRE: Stream and Stream.Source are locked. *) 

  = VAR LI : CARDINAL 
  ; VAR LLast : INTEGER 
  ; VAR LWch1 , LWch2 : Widechar 

  ; BEGIN 
      LLast := LAST ( ArrWch ) 
    ; IF LLast < 0 (* Empty ArrWch, no space for anything. *)  
      THEN RETURN 0 
      ELSE 
        LI := 0 
      ; TRY (* EXCEPT *) 
        (* ASSERT: There is space for one character at ArrWch[0]. *) 
          IF Stream . HasPostponedWCh 
          THEN (* Use previously postponed character(s). *) 
            IF Stream . PostponedWCh = CRLFWch 
            THEN (* CR & LF have been postponed. *) 
              IF LLast <= 0  
              THEN (* Insufficient space for 2 chars.  Continue the 
                      postponement. *)
                RETURN 0 
              ELSE (* Space for CR & LF. *) 
                ArrWch [ 0 ] := CRWch 
              ; ArrWch [ 1 ] := LFWch 
              ; Stream . HasPostponedWCh := FALSE 
              ; INC ( Stream . Index , 2 ) 
              ; RETURN 2 
              END (* IF *) 
            ELSE (* Single postponed char. *)  
              LWch1 := Stream . PostponedWCh 
            ; Stream . HasPostponedWCh := FALSE 
            (* And fall through for possibly more chars. *) 
            END (* IF *) 
          ELSIF UnsafeRd . FastEOF ( Stream . Source ) 
          THEN RETURN 0
          ELSE (* Can and must read a character. *) 
            <* FATAL EndOfFile *> (* Can't happen. *) BEGIN 
              LWch1 := Stream . DecWideChar ( Stream . Source ) 
            END (* Block. *) 
          END (* IF *) 
        (* INVARIANT: LWch1 contains the next char to consider storing. 
                      AND there is space for one character at ArrWch[LI]. *) 
        ; LOOP 
            IF ORD ( LWch1 ) = ORD ( LFWch ) 
               OR ORD ( LWch1 ) = ORD ( NELWch ) 
               OR ORD ( LWch1 ) = ORD ( VTWch ) 
               OR ORD ( LWch1 ) = ORD ( FFWch ) 
               OR ORD ( LWch1 ) = ORD ( PSWch ) 
               OR ORD ( LWch1 ) = ORD ( LSWch ) 
(*2*)
            THEN (* Unambiguously single-char new-line.  Store and return. *)  
              ArrWch [ LI ] := LWch1 
            ; INC ( LI ) 
            ; INC ( Stream . Index , LI ) 
            ; RETURN LI 
            ELSIF ORD ( LWch1 ) = ORD ( CRWch ) 
(*2*)
            THEN (* CR.  Could be start of CRLF. *) 
              IF UnsafeRd . FastEOF ( Stream . Source ) 
              THEN (* CR alone.  Store and return. *) 
                ArrWch [ LI ] := LWch1 
              ; INC ( LI ) 
              ; INC ( Stream . Index , LI ) 
              ; RETURN LI 
              ELSE (* Another char follows the CR. *) 
                <* FATAL EndOfFile *> (* Can't happen. *) BEGIN 
                  LWch2 := Stream . DecWideChar ( Stream . Source ) 
                END (* Block. *) 
              ; IF ORD ( LWch2 ) = ORD ( LFWch ) 
(*2*)
                THEN (* CR and LF. *) 
                  IF LI < LLast 
                  THEN (* Room for both CR and LF. *)  
                    ArrWch [ LI ] := LWch1 
                  ; INC ( LI ) 
                  ; ArrWch [ LI ] := LWch2 
                  ; INC ( LI ) 
                  ; INC ( Stream . Index , LI ) 
                  ; RETURN LI 
                  ELSE (* They won't both fit.  Postpone both. *)
(*3b*) 
                    Stream . PostponedWCh := CRLFWch 
                  ; Stream . HasPostponedWCh := TRUE  
                  ; INC ( Stream . Index , LI ) 
                  ; RETURN LI 
                  END (* IF *) 
                ELSE (* CR & non_LF.  Store CR and postpone LWch2. *) 
(*3a*) 
                  ArrWch [ LI ] := LWch1 
                ; INC ( LI ) 
                ; Stream . PostponedWCh := LWch2  
                ; Stream . HasPostponedWCh := TRUE  
                ; INC ( Stream . Index , LI ) 
                ; RETURN LI 
                END (* IF *) 
              END (* IF *) 
            ELSE (* No kind of new-line at all. *) 
              ArrWch [ LI ] := LWch1 
            ; INC ( LI ) 
            ; IF LI > LLast OR UnsafeRd . FastEOF ( Stream . Source ) 
              THEN 
                INC ( Stream . Index , LI ) 
              ; RETURN LI 
              ELSE 
                <* FATAL EndOfFile *> (* Can't happen. *) BEGIN 
                  LWch1 := Stream . DecWideChar ( Stream . Source ) 
                END (* Block. *) 
              (* And loop. *) 
              END (* IF *) 
            END (* IF *) 
          END (* LOOP *) 
        EXCEPT (* Rd.EOF or Stream.DecWideChar. *) 
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
  (* Decode and consume characters from Source(Stream), using Enc(Stream), 
     storing them into ArrCh, until Source(Stream) is at end-of-file, a
     new-line sequence has been stored, or ArrCh is filled.  
     Return the actual number of decoded characters stored into ArrCh. 

     A new-line sequence is one of CR, LF, CR&LF, NEL, VT, FF, LS, or PS
     (See Unicode 5.8).  In the unusual case that there is a CR&LF sequence,
     but only the CR will fit in ArrCh, do not store either character and 
     return with one unused space in ArrCh.  The client can distinguish this
     by ArrCh not full, not EOF, and no new-line character at the end.   
  *)  
  (* PRE: Stream and Stream.Source are locked. *) 

  = VAR LI : CARDINAL 
  ; VAR LLast : INTEGER 
  ; VAR LWch1 , LWch2 : Widechar 

  ; BEGIN 
      LLast := LAST ( ArrCh ) 
    ; IF LLast < 0 (* Empty ArrCh, no space for anything. *)  
      THEN RETURN 0 
      ELSE 
        LI := 0 
      ; TRY (* EXCEPT *) 
          IF Stream . HasPostponedWCh 
          THEN (* Use previously postponed character(s). *) 
            IF Stream . PostponedWCh = CRLFWch 
            THEN (* CR & LF have been postponed. *) 
              IF LI >= LLast 
              THEN (* Insufficient space for 2 chars.  Continue the 
                      postponement. *)
                RETURN 0 
              ELSE (* Space for CR & LF. *) 
                ArrCh [ 0 ] := CRCh 
              ; ArrCh [ 1 ] := LFCh 
              ; LI := 2 
              ; Stream . HasPostponedWCh := FALSE 
              ; INC ( Stream . Index , 2 ) 
              ; RETURN 2 
              END (* IF *) 
            ELSE (* Single postponed char. *)  
              LWch1 := Stream . PostponedWCh 
            ; Stream . HasPostponedWCh := FALSE 
            (* And fall through for possibly more chars. *) 
            END (* IF *) 
          ELSIF UnsafeRd . FastEOF ( Stream . Source ) 
          THEN RETURN 0 
          ELSE
            <* FATAL EndOfFile *> (* Can't happen. *) BEGIN 
              LWch1 := Stream . DecWideChar ( Stream . Source ) 
            END (* Block. *) 
          END (* IF *) 
        (* INVARIANT: LWch1 contains the next char to consider storing. *) 
        ; LOOP 
            IF ORD ( LWch1 ) = ORD ( LFWch ) 
               OR ORD ( LWch1 ) = ORD ( NELWch ) 
               OR ORD ( LWch1 ) = ORD ( VTWch ) 
               OR ORD ( LWch1 ) = ORD ( FFWch ) 
(*2*)
            THEN (* Unambiguously single-char new-line and it's in CHAR.  
                    Store and return. *)  
              ArrCh [ LI ] := VAL ( ORD ( LWch1 ) , CHAR ) 
            ; INC ( LI ) 
            ; INC ( Stream . Index , LI ) 
            ; RETURN LI 
            ELSIF ORD ( LWch1 ) = ORD ( PSWch ) 
                  OR ORD ( LWch1 ) = ORD ( LSWch ) 
(*2*)
            THEN (* Single-char new-line, but not in CHAR. *)  
              INC ( Stream . Index , LI + 1 ) 
            ; RAISE Range ( UniRd . RangeInfo { LWch1 , LI } )
            ELSIF ORD ( LWch1 ) = ORD ( CRWch ) 
(*2*)
            THEN (* CR.  Could be start of CRLF. *) 
              IF UnsafeRd . FastEOF ( Stream . Source ) 
              THEN (* CR alone.  Store and return. *) 
                ArrCh [ LI ] := CRCh  
              ; INC ( LI ) 
              ; INC ( Stream . Index , LI ) 
              ; RETURN LI 
              ELSE (* Another char follows the CR. *) 
                <* FATAL EndOfFile *> (* Can't happen. *) BEGIN 
                  LWch2 := Stream . DecWideChar ( Stream . Source ) 
                END (* Block. *) 
              ; IF ORD ( LWch2 ) = ORD ( LFWch ) 
(*2*)
                THEN (* CR and LF. *) 
                  IF LI < LLast 
                  THEN (* Room for both CR and LF. *)  
                    ArrCh [ LI ] := CRCh 
                  ; INC ( LI ) 
                  ; ArrCh [ LI ] := LFCh  
                  ; INC ( LI ) 
                  ; INC ( Stream . Index , LI ) 
                  ; RETURN LI 
                  ELSE (* They won't both fit.  Postpone both. *)
(*3b*)
                    Stream . PostponedWCh := CRLFWch 
                  ; Stream . HasPostponedWCh := TRUE  
                  ; INC ( Stream . Index , LI ) 
                  ; RETURN LI 
                  END (* IF *) 
                ELSE (* CR & non_LF.  Store CR and postpone LWch2. *) 
(*3a*)
                  ArrCh [ LI ] := CRCh  
                ; INC ( LI ) 
                ; Stream . PostponedWCh := LWch2  
                ; Stream . HasPostponedWCh := TRUE  
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
                THEN 
                  INC ( Stream . Index , LI ) 
                ; RETURN LI 
                ELSE 
                  <* FATAL EndOfFile *> (* Can't happen. *) BEGIN 
                    LWch1 := Stream . DecWideChar ( Stream . Source ) 
                  END (* Block. *) 
                (* And loop. *) 
                END (* IF *) 
              END (* IF *) 
            END (* IF *) 
          END (* LOOP *) 
        EXCEPT (* Rd.EOF or Stream.DecWideChar. *) 
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
     storing them into the result TEXT, until Source(Stream) is at end-of-file,
     or the length of the result is Len.  
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
        IF Stream . HasPostponedWCh 
        THEN 
          IF Stream . PostponedWCh = CRLFWch 
          THEN 
            IF Len = 1 
            THEN (* Space for the CR only. *) 
              Stream . PostponedWCh := LFWch 
            ; INC ( Stream . Index ) 
            ; RETURN CRText 
            ELSE (* Room for both. *) 
              Stream . HasPostponedWCh := FALSE   
            ; IF Len = 2 OR UnsafeRd . FastEOF ( Stream . Source ) 
              THEN (* Postponed CR & LF only. *)  
                INC ( Stream . Index , 2 ) 
              ; RETURN CRLFText 
              ELSE 
                LChunk [ 0 ] := CRWch 
              ; LChunk [ 1 ] := LFWch 
              ; LI := 2  
              ; LChunkI := 2  
              (* And fall through to get more chars. *) 
              END (* IF *) 
            END (* IF *)           
          ELSE (* Single postponed char. *) 
            Stream . HasPostponedWCh := FALSE             
            
          ; IF Len = 1 OR UnsafeRd . FastEOF ( Stream . Source ) 
            THEN (* Postponed char only. *)  
              INC ( Stream . Index ) 
            ; RETURN Text . FromWideChar ( Stream . PostponedWCh ) 
            ELSE 
              LChunk [ 0 ] := Stream . PostponedWCh 
            ; LI := 1 
            ; LChunkI := 1 
            (* And fall through to get more chars.  *) 
            END (* IF *) 
          END (* IF *) 
        ELSE (* Nothing postponed and waiting. *)  
          IF UnsafeRd . FastEOF ( Stream . Source )
          THEN RETURN "" 
          ELSE 
            LI := 0 
          ; LChunkI := 0 
          (* And fall through to get some chars.  *) 
          END (* IF *) 
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
          ; <* FATAL EndOfFile *> (* Can't happen. *) BEGIN  
              LChunk [ LChunkI ] := Stream . DecWideChar ( Stream . Source ) 
            END (* Block. *) 
          ; INC ( LChunkI ) 
          ; INC ( LI ) 
          ; IF LI >= Len OR UnsafeRd . FastEOF ( Stream . Source ) 
            THEN (* We are done. *) 
              IF LChunkI > 0 
              THEN (* Flush partially filled chunk. *)
                IF LResult = NIL (* Likely. *) 
                THEN 
                  INC ( Stream . Index , LI ) 
                ; RETURN 
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
  (* Decode and consume characters from Source(Stream), using Enc(Stream), 
     storing them into the result TEXT, until Source(Stream) is at end-of-file,
     or a new-line sequence has been consumed and stored.  A new-line sequence 
     is one of CR, LF, CR&LF, NEL, VT, FF, LS, or PS (See Unicode 5.8).  
  *) 
  (* PRE: Stream and Stream.Source are locked. *) 

  = CONST ChunkSize = 512 
  ; VAR LChunk : ARRAY [ 0 .. ChunkSize - 1 ] OF Widechar 
  ; VAR LI , LChunkI : CARDINAL 
  ; VAR LWch1 , LWch2 : Widechar 
  ; VAR LResult : TEXT  

  ; BEGIN 
      LI := 0 
    ; LChunkI := 0 
    ; TRY (* EXCEPT *) 
        IF Stream . HasPostponedWCh 
        THEN 
          Stream . HasPostponedWCh := FALSE             
        ; IF Stream . PostponedWCh = CRLFWch 
          THEN 
            INC ( Stream . Index , 2 ) 
          ; RETURN CRLFText 
          ELSE 
            LWch1:= Stream . PostponedWCh 
          END (* IF *) 
        ELSE (* Nothing was postponed.  Read a character. *)  
          LWch1 := Stream . DecWideChar ( Stream . Source ) 
          (* ^Which could raise EndOfFile. *) 
        END (* IF *) 
      ; LResult := NIL 
      (* INVARIANT: LWch1 contains the next char to store. *) 
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
              <* FATAL EndOfFile *> (* Can't happen. *) BEGIN 
                LWch2 := Stream . DecWideChar ( Stream . Source ) 
(* WARNING: exception is never raised: Rd.EndOfFile--why? *) 
              END (* Block. *) 
            ; IF ORD ( LWch2 ) = ORD ( LFWch )
              THEN (* CR & LF.  Store the LF. *) 
                LChunk [ LChunkI ] := LWch2 (* There will be space. *)  
              ; INC ( LChunkI ) 
              ; INC ( LI ) 
              ; EXIT 
              ELSE (* CR & non-LF.  Postpone the other char. *) 
(*3a*)
                Stream . PostponedWCh := LWch2 
              ; Stream . HasPostponedWCh := TRUE 
              ; EXIT 
              END (* IF *) 
            END (* IF *) 
          ELSE (* It was not a new-line. *) 
            IF UnsafeRd . FastEOF ( Stream . Source ) 
            THEN (* Line has ended at EOF without a new-line sequence. *) 
              EXIT 
            ELSE 
              <* FATAL EndOfFile *> (* Can't happen. *) BEGIN 
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
      EXCEPT (* Rd.EOF or Stream.DecWideChar. *) 
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
  (* Number of characters that have been read from Stream.  
     May overflow by wrapping. *) 
  (* PRE: Stream is locked, but Stream.Source need not be. *) 

  = BEGIN 
      RETURN Stream . Index 
    END FastIndex 

(* EXPORTED: *) 
; PROCEDURE FastAvgBytesPerChar ( Stream : UniRd . T ) : CARDINAL 
  (* Average number of encoded bytes per character, of what has been read. 
     Zero if nothing read. *) 
  (* PRE: Stream is locked, but not Stream.Source. *) 

  = VAR LByteIndex : CARDINAL 
  ; VAR LCharIndex : Word . T
(* REVIEW? What to do about overflows in these values? *)  

  ; BEGIN 
      LByteIndex := Rd . Index ( Stream . Source ) 
(* TODO: An already-locked Index (UnsafeRd.FastIndex) would allow
         this to be called with Stream.Source already locked, which would
         make for a more consistent interface in UnsafeUniRd.i3. *) 
    ; LCharIndex := Stream . Index 
    ; IF LCharIndex = 0 
      THEN RETURN 0 
      ELSE RETURN LByteIndex DIV LCharIndex 
      END (* IF *)  
    END FastAvgBytesPerChar 

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

