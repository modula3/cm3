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
           ORD and VAL conversions.  
           (But maybe it is better to just leave as-is, to provide long-term
           bootstrapping capability. *) 

; CONST LFWch  = VAL ( ORD ( W'\n' ) , Widechar ) 
; CONST LFCh   = '\n' 
; CONST CRWch  = VAL ( ORD ( W'\r' ) , Widechar ) 
; CONST CRCh   = '\r' 
; CONST FFWch  = VAL ( ORD ( W'\f' ) , Widechar ) 
; CONST VTWch  = VAL ( ORD ( W'\t' ) , Widechar ) 
; CONST NELWch = VAL ( ORD ( W'\x0085' ) , Widechar ) 
; CONST LSWch  = VAL ( ORD ( W'\x2028' ) , Widechar ) 
; CONST PSWch  = VAL ( ORD ( W'\x2029' ) , Widechar ) 
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

  = VAR LSourceBytesReady , LCharsReady : CARDINAL 

  ; BEGIN 
      IF Stream . MaxBytesPerChar = 0 
      THEN RETURN 0 
      ELSE 
        LSourceBytesReady := UnsafeRd . FastCharsReady ( Stream . Source )
      ; LCharsReady 
          := ( LSourceBytesReady - 1 (* For EOF *) ) 
             DIV Stream . MaxBytesPerChar 
      ; RETURN LCharsReady + 1 (* For EOF *)  
      END (* IF *) 
    END FastCharsReady 

(* EXPORTED: *) 
; PROCEDURE FastGetWideChar ( Stream : UniRd . T ) : Widechar  
  RAISES { EndOfFile , Failure , Alerted } 
  (* Decode, consume, and return a character from Source(Stream), 
     using Enc(Stream) 
  *) 
  (* PRE: Stream and Stream.Source are locked. *) 
  
  = VAR LStartSourceIndex : CARDINAL
  ; VAR LWch : Widechar

  ; BEGIN 
      LStartSourceIndex := UnsafeRd . FastIndex ( Stream . Source )
    ; LWch := Stream . DecWideChar ( Stream . Source ) 
            (* ^Allow EndOfFile, Failure, or Alerted to propagate out. *) 
    ; INC ( Stream . Index ) 
    ; Stream . CurSourceIndex := UnsafeRd . FastIndex ( Stream . Source ) 
    ; Stream . UnGetByteCt := Stream . CurSourceIndex - LStartSourceIndex 
    ; RETURN LWch 
    END FastGetWideChar 

(* EXPORTED: *) 
; PROCEDURE FastUnGetCodePoint ( Stream : UniRd . T ) : BOOLEAN (* Succeeded. *)
  (* Push back the last decoded code point read from Stream, pushing it back
     onto Stream.Source, in encoded form.  This is guaranteed to work only
     if the last operation on Stream was GetWideChar, GetChar, GetWideSub,
     or GetSub, or an UnsafeUniRd.Fast* version thereof.  Result FALSE means 
     the operation did not happen, because of a violation of this condition,
     or because somebody has bypassed Stream and directly [un]gotten chars
     from Stream.Source.
  *) 
  (* PRE: Stream and Stream.Source are locked. *) 

  = VAR LCurrentIndex : Word . T 

  ; BEGIN
      IF Stream . UnGetByteCt = 0 
      THEN (* Not a place from whence to unget. *) 
        RETURN FALSE 
      END (* IF *) 
    ; LCurrentIndex := UnsafeRd . FastIndex ( Stream . Source ) 
    ; IF Stream . CurSourceIndex # LCurrentIndex  
      THEN (* We are not at the index point following the most recent 
              code-point-consuming operation. *) 
        RETURN FALSE 
      END (* IF *) 
    ; IF UnsafeRd . FastUnGetCharMulti 
           ( Stream . Source , Stream . UnGetByteCt ) 
         = Stream . UnGetByteCt 
      THEN 
        Stream . CurSourceIndex 
          := Stream . CurSourceIndex - Stream . UnGetByteCt  
      ; Stream . UnGetByteCt := 0 
      ; DEC ( Stream . Index ) 
      ; RETURN TRUE
      ELSE (* Too many ungotten characters in Stream.Source. *) 
        RETURN FALSE
      END (* IF *) 
    END FastUnGetCodePoint 
  
(* TODO: Special-case for when the encoding allows to directly access the
         Rd buffer (e.g., ISO8859_1 & CHAR) and avoid looping through 
         individual characters.  
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

  = VAR LI : CARDINAL := 0 
  ; VAR LNumber : CARDINAL := NUMBER ( ArrWch ) 
  ; VAR LStartSourceIndex : CARDINAL := UnsafeRd . FastIndex ( Stream . Source )
  ; VAR LPreSourceIndex : CARDINAL 

  ; BEGIN 
      IF LNumber <= 0 THEN RETURN 0 END (* IF *) 
    ; IF UnsafeRd . FastEOF ( Stream . Source ) THEN RETURN 0 END (* IF *) 
      (* ^Allow Failure or Alerted to propagate out. *) 
    ; <* FATAL EndOfFile *> (* Can't happen. *) 
      BEGIN 
        ArrWch [ LI ] := Stream . DecWideChar ( Stream . Source ) 
                      (* ^Allow Failure or Alerted to propagate out. *) 
      END (* Block *) 
    ; TRY (* FINALLY *)
        LOOP
          INC ( LI ) 
          (* INVARIANTs: - 0 < LI = number of code points decoded and stored. 
                         - LStartSourceIndex is 1st byte index of most recently
                           stored code point.
          *) 
        ; IF LI >= LNumber 
             OR UnsafeRd . FastEOF ( Stream . Source ) 
                (* ^Could raise Failure or Alerted. *) 
          THEN (* We are done. *) 
            EXIT  
          ELSE 
            LPreSourceIndex := UnsafeRd . FastIndex ( Stream . Source )
          ; <* FATAL EndOfFile *> (* Can't happen. *) 
            BEGIN 
              ArrWch [ LI ] := Stream . DecWideChar ( Stream . Source ) 
                            (* ^Could raise Failure or Alerted. *) 
            END (* Block *) 
          ; LStartSourceIndex := LPreSourceIndex 
          (* And loop. *) 
          END (* IF *) 
        END (* LOOP *) 
      FINALLY (* For exceptions or normal returns. *) 
        INC ( Stream . Index , LI ) 
      ; Stream . CurSourceIndex := UnsafeRd . FastIndex ( Stream . Source )
      ; Stream . UnGetByteCt := Stream . CurSourceIndex - LStartSourceIndex 
      END (* FINALLY *) 
    (* Normal return: *) 
    ; RETURN LI 
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

  = VAR LI : CARDINAL := 0 
  ; VAR LNumber : CARDINAL := NUMBER ( ArrCh )  
  ; VAR LStartSourceIndex : CARDINAL := UnsafeRd . FastIndex ( Stream . Source )
  ; VAR LPreSourceIndex : CARDINAL 
  ; VAR LWch : Widechar 

  ; BEGIN    
      IF LNumber <= 0 THEN RETURN 0 END (* IF *) 
    ; IF UnsafeRd . FastEOF ( Stream . Source ) THEN RETURN 0 END (* IF *) 
      (* ^Allow Failure or Alerted to propagate out. *) 
    ; <* FATAL EndOfFile *> (* Can't happen. *) 
      BEGIN 
        LWch := Stream . DecWideChar ( Stream . Source ) 
             (* ^Allow Failure or Alerted to propagate out. *) 
      END (* Block *) 
    ; TRY (* FINALLY *)
        LOOP
          (* INVARIANTs: - LI = number of code points stored. 
                         - LI+1 = number of code points decoded. 
                         - A code point has been decoded and is in LWch. 
                         - LStartSourceIndex is its 1st encoded byte index.
          *) 
          IF ORD ( LWch ) > ORD ( LAST ( CHAR ) ) 
(*2*) 
          THEN (* Out-of-range of CHAR. *)  
            RAISE Range ( UniRd . RangeInfo { LWch , LI } )
          ELSE 
            ArrCh [ LI ] := VAL ( ORD ( LWch ) , CHAR ) 
(*1*) 
(*2*) 
          ; INC ( LI ) 
          (* INVARIANT: LI is the number of code points decoded and stored. *) 
          ; IF LI >= LNumber 
               OR UnsafeRd . FastEOF ( Stream . Source ) 
               (* ^Could raise Failure or Alerted. *) 
            THEN (* We are done. *) 
              EXIT 
            ELSE 
              LPreSourceIndex := UnsafeRd . FastIndex ( Stream . Source )
            ; <* FATAL EndOfFile *> (* Can't happen. *) 
              BEGIN 
                LWch := Stream . DecWideChar ( Stream . Source ) 
                     (* ^Could raise Failure or Alerted. *) 
              END (* Block *) 
            ; LStartSourceIndex := LPreSourceIndex 
            (* And loop. *) 
            END (* IF *) 
          END (* IF *)  
        END (* LOOP *) 
      FINALLY (* For exceptions or normal returns. *) 
        INC ( Stream . Index , LI ) 
      ; Stream . CurSourceIndex := UnsafeRd . FastIndex ( Stream . Source )
      ; Stream . UnGetByteCt := Stream . CurSourceIndex - LStartSourceIndex 
      END (* FINALLY *) 
    (* Normal return: *) 
    ; RETURN LI 
    END FastGetSub 

; <*INLINE*> 
  PROCEDURE UnGetSourceBackTo ( Stream : UniRd . T ; BackToIndex : CARDINAL ) 
    : BOOLEAN (* Success. *) 

  (* We could have up to two Unicode Code points, CR and LF.  If the encoding is
     UTF-32, this would be 8 CHARS we will unget to Stream . Source, and the 
     decoder will have ungotten zero.  For other encodings and these specific 
     code points, the decoder might have ungotten at most one CHAR and we will 
     unget at most 4.  So UnGetCharMulti maximum of 8 will be enough, if nobody 
     else is sneaking around the side of our Stream to Stream.Source and 
     ungetting that way. *) 

  = VAR LCt : [ 1 .. 8 ]
  ; VAR LActual : CARDINAL 

  ; BEGIN 
      LCt := UnsafeRd . FastIndex ( Stream . Source ) - BackToIndex 
    ; LActual := UnsafeRd . FastUnGetCharMulti ( Stream . Source , LCt ) 
    ; RETURN LActual = LCt  
    END UnGetSourceBackTo 

(* EXPORTED *) 
; PROCEDURE FastGetWideSubLine 
    ( Stream : UniRd . T ; VAR (*OUT*) ArrWch : ARRAY OF Widechar ) 
  : CARDINAL
  RAISES { Failure , Alerted } 
  (* Decode and consume characters from Source(Stream), using Enc(Stream), 
     storing them into ArrWch, until Source(Stream) is at end-of-file, or 
     ArrWch is filled, or an end-of-line sequence has been read.  
     Return the actual number of decoded characters stored into ArrWch.  
     Include any end-of-line sequence in the returned count and store it in
     ArrWch, except if only one character of a two-character end-of-line 
     sequence would fit in ArrWch, leave both unstored and unconsumed. 

     Consistent with the Unicode standard, an end-of-line consists of any of:

       LF =  W'\x000A' = W'\n'  
       CR =  W'\x000D' = W'\r'  
       CR immediately followed by LF 
       FF =  W'\x000C' = W'\f'  
       VT =  W'\x0009' = W'\t'  
       NEL = W'\x0085'  
       LS =  W'\x2028'  
       PS =  W'\x2029'  

  *) 
  (* PRE: Stream and Stream.Source are locked. *) 

  = VAR LI : CARDINAL := 0  
  ; VAR LNumber : CARDINAL := NUMBER ( ArrWch ) 
  ; VAR LStartSourceIndex : CARDINAL := UnsafeRd . FastIndex ( Stream . Source )
  ; VAR LPreSourceIndex0 , LPreSourceIndex1 , LPreSourceIndex2 : CARDINAL  
  ; VAR LWch1 , LWch2 : Widechar 
  ; VAR LGotWch2 : BOOLEAN := FALSE 

  ; BEGIN 
      IF LNumber <= 0 THEN RETURN 0 END (* IF *) 
    (* ASSERT: There is space for one character at ArrWch[0]. *) 
    ; IF UnsafeRd . FastEOF ( Stream . Source ) THEN RETURN 0 END (* IF *) 
      (* ^Allow Failure or Alerted to propagate out. *) 
    ; LPreSourceIndex0 := LStartSourceIndex (* Dead. *) 
    ; <* FATAL EndOfFile *> (* Can't happen. *) 
      BEGIN 
        LWch1 := Stream . DecWideChar ( Stream . Source ) 
              (* ^Allow Failure or Alerted to propagate out. *) 
      END (* Block. *) 
    ; IF LNumber = 1 AND ORD ( LWch1 ) = ORD ( CRWch )
(*2*)
      THEN (* Handle this case specially, to avoid altering 
              Stream . CurCurSourceIndex and Stream . UnGetByteCt, when no
              net consumption occurs. *) 
        LPreSourceIndex2 := UnsafeRd . FastIndex ( Stream . Source ) 
      ; TRY 
          IF NOT UnsafeRd . FastEOF ( Stream . Source ) 
              (* ^Allow Failure or Alerted to propagate out. *) 
          THEN 
            <* FATAL EndOfFile *> (* Can't happen. *) 
            BEGIN 
              LWch2 := Stream . DecWideChar ( Stream . Source ) 
                    (* ^Allow Failure or Alerted to propagate out. *) 
            END (* Block. *)
          ; LGotWch2 := TRUE 
          END (* IF *) 
        FINALLY 
          WITH W = UnGetSourceBackTo ( Stream , LPreSourceIndex2 )
          DO <* ASSERT W *> 
          END (* WITH *)
        ; IF LGotWch2 AND ORD ( LWch2 ) = ORD ( LFWch )
(*2*)
          THEN (* CR & LF, won't both fit. *) 
            WITH W = UnGetSourceBackTo ( Stream , LStartSourceIndex )
            DO <* ASSERT W *> 
            END (* WITH *)
          (* LI = 0.  Will RETURN with no net consumption and no changes to 
             Stream . CurSourceIndex or Stream . UnGetByteCt. *) 
          ELSE 
            ArrWch [ 0 ] := CRWch 
          ; LI := 1 
          ; INC ( Stream . Index ) 
          ; Stream . CurSourceIndex := UnsafeRd . FastIndex ( Stream . Source ) 
          ; Stream . UnGetByteCt := Stream . CurSourceIndex - LStartSourceIndex
          END (* IF *) 
        END (* FINALLY *) 
      ; RETURN LI 
      END (* IF *) 
    ; TRY (* FINALLY *) 
        LOOP 
      (* INVARIANTs: - LWch1 contains the next code point to consider storing. 
                     - LStartSourceIndex is its 1st encoded byte index.
                     - LPreSourceIndex0 is the 1st encoded byte index
                       of the previous delivered code point, or 
                       LStartSourceIndes if none.
                     - There is space for a Widechar at ArrWch[LI]. *) 
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
          ; EXIT 
          ELSIF ORD ( LWch1 ) = ORD ( CRWch ) 
(*2*)
          THEN (* CR.  Could be start of CRLF. *) 
            IF UnsafeRd . FastEOF ( Stream . Source ) 
            (* ^Which could raise Failure or Alerted. *) 
            THEN (* CR alone at EOF.  Store and return. *) 
              ArrWch [ LI ] := LWch1 
            ; INC ( LI ) 
            ; EXIT 
            ELSE (* Another code point follows the CR. *) 
              LPreSourceIndex2 := UnsafeRd . FastIndex ( Stream . Source )
           (* ^Start of following code point, if we get one. *) 
            ; <* FATAL EndOfFile *> (* Can't happen. *) 
              BEGIN 
                LWch2 := Stream . DecWideChar ( Stream . Source ) 
                      (* ^Which could raise Failure or Alerted. *) 
              END (* Block. *) 
            ; IF ORD ( LWch2 ) = ORD ( LFWch ) 
(*2*)
              THEN (* CR and LF. *) 
                IF LI + 1 < LNumber 
                THEN (* Room for both CR and LF. Store them and return. *)  
                  ArrWch [ LI ] := CRWch 
                ; INC ( LI ) 
                ; ArrWch [ LI ] := LFWch 
                ; INC ( LI ) 
                ; LStartSourceIndex := LPreSourceIndex2 
              (* ^A subsequent UnGetWideChar will unget only the LF. *) 
                ; EXIT 
                ELSE (* They won't both fit.  Unget both and return. *)
                  WITH W = UnGetSourceBackTo ( Stream , LStartSourceIndex ) 
                  DO <* ASSERT W *> 
                  END (* WITH *)
                ; LStartSourceIndex := LPreSourceIndex0 
                ; EXIT 
                END (* IF *) 
              ELSE (* CR & non-LF.  Store CR and unget LWch2. *) 
                ArrWch [ LI ] := CRWch 
              ; INC ( LI ) 
              ; WITH W = UnGetSourceBackTo ( Stream , LPreSourceIndex2 ) 
                DO <* ASSERT W *> 
                END (* WITH *)
              ; EXIT 
              END (* IF *) 
            END (* IF *) 
          ELSE (* No kind of new-line at all. *) 
            ArrWch [ LI ] := LWch1 
          ; INC ( LI ) 
          ; IF LI >= LNumber 
               OR UnsafeRd . FastEOF ( Stream . Source ) 
               (* ^Could raise Failure or Alerted. *) 
            THEN (* We are done W/O a new line. *)  
              EXIT 
            ELSE (* Time to read another code point. *) 
              LPreSourceIndex0 := LPreSourceIndex1 
            ; LPreSourceIndex1 := UnsafeRd . FastIndex ( Stream . Source )
            ; <* FATAL EndOfFile *> (* Can't happen. *) 
              BEGIN 
                LWch1 := Stream . DecWideChar ( Stream . Source ) 
                      (* ^Could raise Failure or Alerted. *) 
              END (* Block. *) 
            ; LStartSourceIndex := LPreSourceIndex1 
            (* And loop. *) 
            END (* IF *) 
          END (* IF *) 
        END (* LOOP *) 
      FINALLY (* For exceptions or normal returns. *)
        INC ( Stream . Index , LI ) 
      ; Stream . CurSourceIndex := UnsafeRd . FastIndex ( Stream . Source )
      ; Stream . UnGetByteCt := Stream . CurSourceIndex - LStartSourceIndex 
      END (* FINALLY *) 
    ; RETURN LI 
    END FastGetWideSubLine 

(* EXPORTED *) 
; PROCEDURE FastGetSubLine 
    ( Stream : UniRd . T ; VAR (*OUT*) ArrCh : ARRAY OF CHAR ) 
  : CARDINAL
  RAISES { Range , Failure , Alerted } 
  (* Like FastGetWideSubLine, but return stored characters in an ARRAY OF CHAR.
     If an otherwise to-be-returned character is not in CHAR, consume but do
     not store it and raise Range(Wch,N), where Wch is the out-of-range 
     character, and N is the number of characters stored.  
  *) 
  (* PRE: Stream and Stream.Source are locked. *) 

  = VAR LI : CARDINAL := 0  
  ; VAR LNumber : CARDINAL := NUMBER ( ArrCh ) 
  ; VAR LStartSourceIndex : CARDINAL := UnsafeRd . FastIndex ( Stream . Source )
  ; VAR LPreSourceIndex0 , LPreSourceIndex1 , LPreSourceIndex2 : CARDINAL  
  ; VAR LWch1 , LWch2 : Widechar 
  ; VAR LGotWch2 : BOOLEAN := FALSE 

  ; BEGIN 
      IF LNumber <= 0 THEN RETURN 0 END (* IF *) 
    (* ASSERT: There is space for one CHAR at ArrCh[0]. *) 
    ; IF UnsafeRd . FastEOF ( Stream . Source ) THEN RETURN 0 END (* IF *) 
      (* ^Allow Failure or Alerted to propagate out. *) 
    ; LPreSourceIndex0 := LStartSourceIndex (* Dead. *) 
    ; <* FATAL EndOfFile *> (* Can't happen. *) 
      BEGIN 
        LWch1 := Stream . DecWideChar ( Stream . Source ) 
              (* ^Allow Failure or Alerted to propagate out. *) 
      END (* Block. *) 
    ; IF LNumber = 1 AND ORD ( LWch1 ) = ORD ( CRWch )
(*2*)
      THEN (* Handle this case specially, to avoid altering 
              Stream . CurCurSourceIndex and Stream . UnGetByteCt, when no
              net consumption occurs. *) 
        LPreSourceIndex2 := UnsafeRd . FastIndex ( Stream . Source ) 
      ; TRY 
          IF NOT UnsafeRd . FastEOF ( Stream . Source ) 
              (* ^Allow Failure or Alerted to propagate out. *) 
          THEN 
            <* FATAL EndOfFile *> (* Can't happen. *) 
            BEGIN 
              LWch2 := Stream . DecWideChar ( Stream . Source ) 
                    (* ^Allow Failure or Alerted to propagate out. *) 
            END (* Block. *)
          ; LGotWch2 := TRUE 
          END (* IF *) 
        FINALLY 
          WITH W = UnGetSourceBackTo ( Stream , LPreSourceIndex2 )
          DO <* ASSERT W *> 
          END (* WITH *)
        ; IF LGotWch2 AND ORD ( LWch2 ) = ORD ( LFWch )
(*2*)
          THEN (* CR & LF, won't both fit. *) 
            WITH W = UnGetSourceBackTo ( Stream , LStartSourceIndex )
            DO <* ASSERT W *> 
            END (* WITH *)
          (* LI = 0.  Will RETURN with no net consumption and no changes to 
             Stream . CurSourceIndex or Stream . UnGetByteCt. *) 
          ELSE 
            ArrCh [ 0 ] := CRCh 
          ; LI := 1 
          ; INC ( Stream . Index ) 
          ; Stream . CurSourceIndex := UnsafeRd . FastIndex ( Stream . Source ) 
          ; Stream . UnGetByteCt := Stream . CurSourceIndex - LStartSourceIndex
          END (* IF *) 
        END (* FINALLY *) 
      ; RETURN LI 
      END (* IF *) 
    ; TRY (* FINALLY *) 
        LOOP 
      (* INVARIANTs: - LWch1 contains the next code point to consider storing. 
                     - LStartSourceIndex is its 1st encoded byte index.
                     - LPreSourceIndex0 is the 1st encoded byte index
                       of the previous delivered code point, or 
                       LStartSourceIndes if none.
                     - There is space for a CHAR at ArrCh[LI]. *) 
          IF ORD ( LWch1 ) > ORD ( LAST ( CHAR ) ) 
          THEN 
            RAISE Range ( UniRd . RangeInfo { LWch1 , LI } ) 
          ELSIF ORD ( LWch1 ) = ORD ( LFWch ) 
                OR ORD ( LWch1 ) = ORD ( NELWch ) 
                OR ORD ( LWch1 ) = ORD ( VTWch ) 
                OR ORD ( LWch1 ) = ORD ( FFWch ) 
(*2*)
          THEN (* Unambiguously single-char new-line.  Store and return. *)  
            ArrCh [LI ] := VAL ( ORD ( LWch1 ) , CHAR ) 
(*1*)
          ; INC ( LI ) 
          ; EXIT 
          ELSIF ORD ( LWch1 ) = ORD ( CRWch ) 
(*2*)
          THEN (* CR.  Could be start of CRLF. *) 
            IF UnsafeRd . FastEOF ( Stream . Source ) 
            (* ^Which could raise Failure or Alerted. *) 
            THEN (* CR alone at EOF.  Store and return. *) 
              ArrCh [LI ] := VAL ( ORD ( LWch1 ) , CHAR ) 
(*1*)
            ; INC ( LI ) 
            ; EXIT 
            ELSE (* Another code point follows the CR. *) 
              LPreSourceIndex2 := UnsafeRd . FastIndex ( Stream . Source )
            ; <* FATAL EndOfFile *> (* Can't happen. *) 
              BEGIN 
                LWch2 := Stream . DecWideChar ( Stream . Source ) 
                      (* ^Which could raise Failure or Alerted. *) 
              END (* Block. *) 
            ; IF ORD ( LWch2 ) = ORD ( LFWch ) 
(*2*)
              THEN (* CR and LF. *) 
                IF LI + 1 < LNumber 
                THEN (* Room for both CR and LF. Store them and return. *)  
                  ArrCh [LI ] := CRCh 
                ; INC ( LI ) 
                ; ArrCh [ LI ] := LFCh  
                ; INC ( LI ) 
                ; LStartSourceIndex := LPreSourceIndex2 
              (* ^A subsequent UnGetWideChar will unget only the LF. *) 
                ; EXIT 
                ELSE (* They won't both fit.  Unget both and return. *)
                  WITH W = UnGetSourceBackTo ( Stream , LStartSourceIndex ) 
                  DO <* ASSERT W *> 
                  END (* WITH *)
                ; LStartSourceIndex := LPreSourceIndex0 
                ; EXIT 
                END (* IF *) 
              ELSE (* CR & non-LF.  Store CR and unget LWch2. *) 
                ArrCh [LI ] := CRCh
(*1*)
              ; INC ( LI ) 
              ; WITH W = UnGetSourceBackTo ( Stream , LPreSourceIndex2 ) 
                DO <* ASSERT W *> 
                END (* WITH *)
              ; EXIT 
              END (* IF *) 
            END (* IF *) 
          ELSE (* No kind of new-line at all. *) 
            ArrCh [LI ] := VAL ( ORD ( LWch1 ) , CHAR ) 
(*1*)
          ; INC ( LI ) 
          ; IF LI >= LNumber 
               OR UnsafeRd . FastEOF ( Stream . Source ) 
               (* ^Could raise Failure or Alerted. *) 
            THEN (* We are done W/O a new line. *)  
              EXIT 
            ELSE (* Time to read another code point. *) 
              LPreSourceIndex0 := LStartSourceIndex 
            ; LPreSourceIndex1 := UnsafeRd . FastIndex ( Stream . Source )
            ; <* FATAL EndOfFile *> (* Can't happen. *) 
              BEGIN 
                LWch1 := Stream . DecWideChar ( Stream . Source ) 
                      (* ^Could raise Failure or Alerted. *)  
              END (* Block. *) 
            ; LStartSourceIndex := LPreSourceIndex1 
            (* And loop. *) 
            END (* IF *) 
          END (* IF *) 
        END (* LOOP *) 
      FINALLY (* For exceptions or normal returns. *)
        INC ( Stream . Index , LI ) 
      ; Stream . CurSourceIndex := UnsafeRd . FastIndex ( Stream . Source )
      ; Stream . UnGetByteCt := Stream . CurSourceIndex - LStartSourceIndex 
      END (* FINALLY *) 
    ; RETURN LI 
    END FastGetSubLine 

(* EXPORTED *) 
; PROCEDURE FastGetText ( Stream : UniRd . T ; Len : CARDINAL ) : TEXT 
  RAISES { Failure , Alerted }
  (* Decode and consume characters from Source(Stream), using Enc(Stream), 
     until Len characters have been decoded or Source(Stream) is at 
     end-of-file.  Return the decoded characters as a TEXT. 
  *) 
  (* PRE: Stream and Stream.Source are locked. *) 

  = VAR LI : CARDINAL := 0  
  ; VAR LChunkI : CARDINAL := 0  
  ; VAR LStartSourceIndex : CARDINAL := UnsafeRd . FastIndex ( Stream . Source )
  ; VAR LPreSourceIndex : CARDINAL 
  ; VAR LResult : TEXT := NIL 
  ; VAR LWch : Widechar 
  ; CONST ChunkSize = 256
  ; VAR LChunk : ARRAY [ 0 .. ChunkSize - 1 ] OF Widechar 

  ; BEGIN 
      IF Len <= 0 THEN RETURN "" END (* IF *) 
    ; IF UnsafeRd . FastEOF ( Stream . Source )
      (* ^Allow Failure or Alerted to propagate out. *) 
      THEN RETURN "" 
      END (* IF *) 
    ; <* FATAL EndOfFile *> (* Can't happen. *) 
      BEGIN  
        LWch := Stream . DecWideChar ( Stream . Source ) 
             (* ^Allow Failure or Alerted to propagate out. *) 
      END (* Block. *) 
    ; TRY (* FINALLY *)
        LOOP 
          (* INVARIANTs: - LI = number of code points stored.
                         - LI + 1 = # of code points decoded and consumed. 
                         - The last decoded code point is in LWch. 
                         - LStartSourceIndex is its 1st encoded byte index. *)
          (* Make space to store the code point in LWch. *) 
          IF LChunkI > LAST ( LChunk ) 
          THEN (* Flush already-full chunk and start a new one. *)
            IF LResult = NIL (* Likely. *) 
            THEN 
              LResult := TextFromWideChars ( LChunk )
            ELSE 
              LResult := LResult & TextFromWideChars ( LChunk )
            END (* IF *) 
          ; LChunkI := 0 
          END (* IF *)  
        ; LChunk [ LChunkI ] := LWch 
        ; INC ( LChunkI ) 
        ; INC ( LI )
        (* INVARIANT: LI is the number of code points decoded and stored. *)  
        ; IF LI >= Len OR UnsafeRd . FastEOF ( Stream . Source ) 
                       (* ^May raise Failure or Alerted. *) 
          THEN (* We are done. *) 
            EXIT 
          ELSE 
            LPreSourceIndex := UnsafeRd . FastIndex ( Stream . Source )
          ; <* FATAL EndOfFile *> (* Can't happen. *) 
            BEGIN  
              LWch := Stream . DecWideChar ( Stream . Source ) 
                            (* ^May raise Failure or Alerted. *) 
            END (* Block. *) 
          ; LStartSourceIndex := LPreSourceIndex 
          (* And loop. *) 
          END (* IF *) 
        END (* LOOP *) 
      FINALLY (* For exceptions or normal returns. *)  
        INC ( Stream . Index , LI ) 
      ; Stream . CurSourceIndex := UnsafeRd . FastIndex ( Stream . Source ) 
      ; Stream . UnGetByteCt := Stream . CurSourceIndex - LStartSourceIndex 
      END (* FINALLY *)
    (* Normal return.  Flush partially filled chunk. *)
    ; IF LChunkI > 0 
      THEN (* Flush partially filled chunk. *)
        IF LResult = NIL (* Likely. *) 
        THEN 
          RETURN 
            Text. FromWideChars ( SUBARRAY ( LChunk , 0 , LChunkI ) )
        ELSE 
          RETURN  
            LResult 
            & Text. FromWideChars ( SUBARRAY ( LChunk , 0 , LChunkI ) )
        END (* IF *) 
      ELSIF LResult = NIL 
      THEN RETURN "" 
      ELSE RETURN LResult 
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

  = VAR LI : CARDINAL := 0  
  ; VAR LChunkI : CARDINAL := 0  
  ; VAR LStartSourceIndex : CARDINAL 
  ; VAR LPreSourceIndex1 , LPreSourceIndex2 : CARDINAL 
  ; VAR LWch1 , LWch2 : Widechar 
  ; VAR LResult : TEXT := NIL 
  ; CONST ChunkSize = 256 
  ; VAR LChunk : ARRAY [ 0 .. ChunkSize - 1 ] OF Widechar 

  ; BEGIN 
      LStartSourceIndex := UnsafeRd . FastIndex ( Stream . Source )
    ; LWch1 := Stream . DecWideChar ( Stream . Source ) 
            (* ^Which could raise EndOfFile, Failure, or Alerted. *) 
    ; TRY (* FINALLY *) 
        LOOP 
        (* INVARIANTs: - LWch1 contains the next code point to be stored. 
                       - LStartSourceIndex is its 1st encoded byte index. *) 
        (* Ensure space for LWch1 and one more, for possibly CR & LF. *)   
          IF LChunkI >= LAST ( LChunk ) 
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
        (* Store LWch1. *) 
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
            (* ^May raise Failure or Alerted. *) 
            THEN (* CR alone at EOF. *) 
              EXIT 
            ELSE (* Another char follows the CR. *) 
              LPreSourceIndex2 := UnsafeRd . FastIndex ( Stream . Source )
              (* ^Start of the following code point. *) 
            ; <* FATAL EndOfFile *> (* Can't happen. *) 
(* WARNING: exception is never raised: Rd.EndOfFile--why? *) 
              BEGIN 
                LWch2 := Stream . DecWideChar ( Stream . Source ) 
                      (* ^May raise Failure or Alerted. *) 
              END (* Block. *) 
            ; IF ORD ( LWch2 ) = ORD ( LFWch )
              THEN (* CR & LF.  Store the LF. *) 
                LChunk [ LChunkI ] := LFWch (* There will be space. *)  
              ; INC ( LChunkI ) 
              ; INC ( LI ) 
              ; LStartSourceIndex := LPreSourceIndex2 
                (* A subsequent UnGetWideChar will unget only the LF.*) 
              ; EXIT 
              ELSE (* CR & non-LF.  Unget the non-LF code point. *) 
                WITH W = UnGetSourceBackTo ( Stream , LPreSourceIndex2 ) 
                DO <* ASSERT W *> 
                END (* WITH *)
              ; EXIT 
              END (* IF *) 
            END (* IF *) 
          ELSE (* It was not a new-line of any kind. *) 
            IF UnsafeRd . FastEOF ( Stream . Source ) 
            (* ^Could raise Failure or Alerted. *) 
            THEN (* Line has ended at EOF without a new-line sequence. *) 
              EXIT 
            ELSE (* Get another code point. *)  
              LPreSourceIndex1 := UnsafeRd . FastIndex ( Stream . Source )
            ; <* FATAL EndOfFile *> (* Can't happen. *) 
(* WARNING: exception is never raised: Rd.EndOfFile--why? *) 
              BEGIN 
                LWch1 := Stream . DecWideChar ( Stream . Source ) 
                      (* ^May raise Failure or Alerted. *) 
              END (* Block. *)
            ; LStartSourceIndex := LPreSourceIndex1  
            (* And loop. *) 
            END (* IF *) 
          END (* IF *) 
        END (* LOOP *) 
      FINALLY (* For exceptions or normal returns. *)  
        INC ( Stream . Index , LI ) 
      ; Stream . CurSourceIndex := UnsafeRd . FastIndex ( Stream . Source ) 
      ; Stream . UnGetByteCt := Stream . CurSourceIndex - LStartSourceIndex 
      END (* FINALLY *)
    (* Normal return.  Flush partially filled chunk. *)
    ; IF LResult = NIL (* Likely. *) 
      THEN 
        RETURN TextFromWideChars ( SUBARRAY ( LChunk , 0 , LChunkI ) )
      ELSE 
        RETURN  
          LResult 
          & TextFromWideChars ( SUBARRAY ( LChunk , 0 , LChunkI ) )
      END (* IF *) 
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

; CONST TextFromWideChars = Text . FromWideChars 
(* ; CONST TextFromWideChars = LocTextFromWideChars *) 

; <*UNUSED*>
  PROCEDURE LocTextFromWideChars ( READONLY ArrWch : ARRAY OF Widechar ) : TEXT 
  (* Simulate Text.FromWideChars.  Temporary, while Widechar # WIDECHAR
     and for debugging. *) 

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
      ; ArrWC [ RI ] := VAL ( ORD ( ArrWch [ RI ] ) , WIDECHAR )
      ; LReval := ArrWC [ RI ] 
      END (* FOR *) 
    ; RETURN Text . FromWideChars ( SUBARRAY ( ArrWC , 0 , LLast + 1 ) ) 
    END LocTextFromWideChars 

; BEGIN (* UnsafeUniRd *) 
  END UnsafeUniRd 
. 

