(* Copyright (C) Rodney M. Bates 2016. *)
(* rodney.m.bates@acm.org *) 
(* Licensed under the MIT License. *) 

UNSAFE MODULE UnsafeUniCodec 

(* Coding and decoding for the unicode encoding schemes, as well as 
   a couple of others.
*) 

; IMPORT Compiler
; IMPORT Rd 
; FROM Rd IMPORT EndOfFile  
; IMPORT RdClass 
; FROM Thread IMPORT Alerted 
; FROM UnsafeRd IMPORT FastGetChar , FastUnGetCharMulti , FastEOF 
; FROM UnsafeWr IMPORT FastPutChar 
; IMPORT UniEncoding 
; FROM UniEncoding IMPORT Encoding 
; FROM UniCodec IMPORT IsBE , Range 
; FROM UniCodec IMPORT Widechar  
; IMPORT Word 
; FROM Word 
  IMPORT Or , And , RightShift , LeftShift , LE , LT , GT , Minus , Plus
; IMPORT Wr 
; IMPORT WrClass 

; REVEAL WrClass . Private <: MUTEX 
; REVEAL RdClass . Private <: MUTEX 

; CONST IsLE = Compiler . ThisEndian = Compiler . ENDIAN . LITTLE  
; CONST IsLEPlusMinus = 2 * ORD ( IsLE ) - 1 (* +1 if LE, -1 if BE. *) 

; TYPE ArrChWch = ARRAY [ 0 .. 3 ] OF CHAR 
  (* Array of CHAR, covering WIDECHAR. *) 
  (* If WIDECHAR were other than 4 CHARS, lots of things would have to be
     recoded, not just this array type. *) 

(* Subscripts to ArrChWch that make it a little-endian numbering of the CHARs
   in a WIDECHAR, regardless of the host endianness. *) 
; CONST ArrChWch0 = ( NUMBER ( ArrChWch ) - 1 ) * ORD ( IsBE )  
; CONST ArrChWch1 = ArrChWch0 + IsLEPlusMinus * 1 
; CONST ArrChWch2 = ArrChWch0 + IsLEPlusMinus * 2 
; CONST ArrChWch3 = ArrChWch0 + IsLEPlusMinus * 3 

; TYPE ArrChWt = ARRAY [ 0 .. BYTESIZE ( Word . T ) - 1 ] OF CHAR 
  (* Array of CHAR covering Word.T. *) 

(* Subscript to ArrChWt that makes it a little-endian numbering of the CHARs
   in a Word.T, regardless of the host endianness. *) 
; CONST ArrChWt0 = ( NUMBER ( ArrChWt ) - 1 ) * ORD ( IsBE ) 

; CONST WideCharsPerWt = BYTESIZE ( Word . T ) DIV 4 (* BYTESIZE ( WIDECHAR ) *)
; TYPE ArrWchWt = ARRAY [ 0 .. WideCharsPerWt - 1 ] OF BITS 32 FOR Widechar  
  (* Array of WIDECHAR covering Word.T. *) 

(* Subscript to ArrWchWt that makes it a little-endian numbering of the 
   WIDECHARs in a Word.T, regardless of the host endianness. *) 
; CONST ArrWchWt0 = ( WideCharsPerWt - 1 ) * ORD ( IsBE ) 

; PROCEDURE FastEncode ( Enc : Encoding ; Sink : Wr . T ; Wch : Widechar ) 
  RAISES { Alerted , Wr . Failure , Range } 
  (* Raise Range if Wch is not valid for Enc. 
     Otherwise, Encode Wch, using Enc, and write to Sink.  
  *) 

  = BEGIN 
    (* Dispatch to appropriate encoding procedure. *) 
      EncTable [ IsBE , Enc ] ( Sink , Wch ) 
    END FastEncode 

; PROCEDURE FastDecode ( Enc : Encoding ; Source : Rd . T ) : Widechar 
   RAISES { EndOfFile , Rd . Failure , Alerted } 
  (* Read and decode a code point from Source, using Enc, and return
     it as a WIDECHAR.  Return any ill-formed input as the Unicode "replacement
     character", VAL(16_FFFD,WIDECHAR) 
  *)  

  = BEGIN 
    (* Dispatch to appropriate decoding procedure. *) 
      RETURN DecTable [ IsBE , Enc ] ( Source ) 
    END FastDecode 

; PROCEDURE FastEncNull 
    ( <* UNUSED *> Sink : Wr . T ; <* UNUSED *> Wch : Widechar ) 
  RAISES { Alerted , Wr . Failure , Range } <* NOWARN*> 
  (* A NOOP.  Placeholder for Encoding.Null. *) 

  = BEGIN (* Do nothing. *) 
    END FastEncNull

; PROCEDURE FastDecNull ( <* UNUSED *> Source : Rd . T ) : Widechar
  RAISES { EndOfFile , Rd . Failure , Alerted } <* NOWARN*> 
  (* A NOOP.  Placeholder for Encoding.Null. *) 

  = BEGIN (* FastDecNull *) 
      RETURN UniEncoding . ReplacementWch
    END FastDecNull

; PROCEDURE FastEncInternal 
    ( <* UNUSED *> Sink : Wr . T ; <* UNUSED *> Wch : Widechar ) 
  RAISES { Alerted , Wr . Failure , Range } <* NOWARN*> 
  (* A NOOP. Placeholder for Encoding.Internal, which is used for
     in-memory representations normally hidden by abstractions. 
  *) 

  = BEGIN 
    END FastEncInternal

; PROCEDURE FastDecInternal ( <* UNUSED *> Source : Rd . T ) : Widechar
   RAISES { EndOfFile , Rd . Failure , Alerted } <* NOWARN*> 
  (* A NOOP. Placeholder for Encoding.Internal, which is used for
     in-memory representations normally hidden by abstractions. 
  *) 

  = BEGIN (* FastDecInternal *) 
      RETURN UniEncoding . ReplacementWch
    END FastDecInternal

; PROCEDURE FastEncISO8859_1 ( Sink : Wr . T ; Wch : Widechar ) 
  RAISES { Alerted , Wr . Failure , Range } 
  (* Raise Range if ORD(Wch) > 16_FF.  Otherwise, write Wch to Sink, 
     in ISO 8859-1 encoding.  This encoding is just byte identity.   
     It is the same as original Modula-3 PutChar. 
  *) 

  = BEGIN 
      IF GT ( ORD ( Wch ) , 16_FF ) 
      THEN RAISE Range 
      ELSE 
        FastPutChar ( Sink , LOOPHOLE ( Wch , ArrChWch ) [ ArrChWch0 ] ) 
      END (* IF *) 
    END FastEncISO8859_1

; PROCEDURE FastDecISO8859_1 ( Source : Rd . T ) : Widechar 
  RAISES { EndOfFile , Rd . Failure , Alerted } 
  (* Read and decode a code point from Source, using ISO 8859-1 encoding, 
     and return it as a WIDECHAR.  This encoding is the same as original 
     Modula-3 GetChar and is an identity on a single byte.  ORD(<result>)
     will be <= 16_FF.  No ill-formed cases exist. 
  *)  

  = BEGIN (* FastDecISO8859_1 *) 
      RETURN 
        LOOPHOLE ( ORD ( FastGetChar ( Source ) ) , ArrWchWt ) [ ArrWchWt0 ]
    END FastDecISO8859_1

; PROCEDURE FastEncCM3WC ( Sink : Wr . T ; Wch : Widechar )
  RAISES { Alerted , Wr . Failure , Range } 
  (* Raise Range if ORD(Wch) > 16_FFFF.  Otherwise, Encode Wch, using
     CM3WC encoding, and write to Sink.  This is just 2-byte fixed-size 
     little endian, the same as the original Cm3 PutWideChar. 
  *)   

  = BEGIN 
      IF GT ( ORD ( Wch ) , 16_FFFF ) 
      THEN RAISE Range 
      ELSE 
        FastPutChar ( Sink , LOOPHOLE ( Wch , ArrChWch ) [ ArrChWch0 ] ) 
      ; FastPutChar ( Sink , LOOPHOLE ( Wch , ArrChWch ) [ ArrChWch1 ] ) 
      END (* IF *) 
    END FastEncCM3WC

; PROCEDURE FastDecCM3WC ( Source : Rd . T ) : Widechar 
  RAISES { EndOfFile , Rd . Failure , Alerted } 
  (* Read and decode a code point from Source, using CM3WC encoding, and return
     it as a WIDECHAR.  This is just 2-byte fixed-size little endian, 
     the same as the original Cm3 GetWideChar.  ORD(<result>) will 
     be <= 16_FFFF.  Just zero-extend an odd byte before EOF.  
     No ill-formed cases exist. 
  *)  

  = VAR B0 , B1 : Word . T (* Byte code units, in order read. *) 
  ; VAR ResultWt : Word . T 

  ; BEGIN (* FastDecCM3WC *) 
      B0 := ORD ( FastGetChar ( Source ) ) 
    ; IF FastEOF ( Source ) 
      THEN 
        B1 := 0 (* As in original CM3 GetWideChar. *)  
      ELSE 
        B1 := ORD ( FastGetChar ( Source ) ) 
      END (* IF *) 
    ; ResultWt := Or ( LeftShift ( B1 , 8 ) , B0 )
    ; RETURN LOOPHOLE ( ResultWt , ArrWchWt ) [ ArrWchWt0 ] 
    END FastDecCM3WC

; PROCEDURE FastEncUCS2LE ( Sink : Wr . T ; Wch : Widechar )
  RAISES { Alerted , Wr . Failure , Range } 
  (* Raise Range if ORD(Wch) > 16_FFFF or Wch is a surrogate code.  
     Otherwise, Encode Wch, using UCS2LE encoding, and write to Sink  
     as 2-byte fixed-size little endian. 
  *) 

  = BEGIN 
      IF GT ( ORD ( Wch ) , 16_FFFF ) 
         OR And ( 16_F800 , ORD ( Wch ) ) = 16_D800 
      THEN RAISE Range 
      ELSE 
        FastPutChar ( Sink , LOOPHOLE ( Wch , ArrChWch ) [ ArrChWch0 ] ) 
      ; FastPutChar ( Sink , LOOPHOLE ( Wch , ArrChWch ) [ ArrChWch1 ] ) 
      END (* IF *) 
    END FastEncUCS2LE

; PROCEDURE FastDecUCS2LE ( Source : Rd . T ) : Widechar 
  RAISES { EndOfFile , Rd . Failure , Alerted } 
  (* Read and decode a code point from Source, using UCS2LE encoding, and 
     return it as a WIDECHAR.  ORD(<result>) will be <= 16_FFFF.  Return 
     any ill-formed input as the Unicode "replacement character", 
     VAL(16_FFFD,WIDECHAR) 
  *)  

  = VAR B0 , B1 : Word . T (* Byte code units, in order read. *) 
  ; VAR ResultWt : Word . T 

  ; BEGIN (* FastDecUCS2LE *) 
      B0 := ORD ( FastGetChar ( Source ) ) 
    ; IF FastEOF ( Source ) 
      THEN 
        RETURN UniEncoding . ReplacementWch (* For B0. *)
      ELSE 
        B1 := ORD ( FastGetChar ( Source ) ) 
      ; ResultWt := Or ( LeftShift ( B1 , 8 ) , B0 )
      ; IF And ( 16_F800 , ResultWt ) = 16_D800 
        THEN RETURN UniEncoding . ReplacementWch (* For surrogate code. *)
        ELSE RETURN LOOPHOLE ( ResultWt , ArrWchWt ) [ ArrWchWt0 ] 
        END (* IF *) 
      END (* IF *) 
    END FastDecUCS2LE

; PROCEDURE FastEncUCS2BE ( Sink : Wr . T ; Wch : Widechar )
  RAISES { Alerted , Wr . Failure , Range } 
  (* Raise Range if ORD(Wch) > 16_FFFF or Wch is a surrogate code.  
     Otherwise, Encode Wch, using UCS2BE encoding, and write to Sink  
     as 2-byte fixed-size big endian. 
  *) 

  = BEGIN 
      IF GT ( ORD ( Wch ) , 16_FFFF ) 
      THEN RAISE Range 
      ELSE 
        FastPutChar ( Sink , LOOPHOLE ( Wch , ArrChWch ) [ ArrChWch1 ] ) 
      ; FastPutChar ( Sink , LOOPHOLE ( Wch , ArrChWch ) [ ArrChWch0 ] ) 
      END (* IF *) 
    END FastEncUCS2BE

; PROCEDURE FastDecUCS2BE ( Source : Rd . T ) : Widechar 
  RAISES { EndOfFile , Rd . Failure , Alerted } 
  (* Read and decode a code point from Source, using UCS2BE encoding, and 
     return it as a WIDECHAR.  ORD(<result>) will be <= 16_FFFF.  Return 
     any ill-formed input as the Unicode "replacement character", 
     VAL(16_FFFD,WIDECHAR) 
  *)  

  = VAR B0 , B1 : Word . T (* Byte values, in temporal order read. *) 
  ; VAR ResultWt : Word . T 

  ; BEGIN (* FastDecUCS2BE *) 
      B0 := ORD ( FastGetChar ( Source ) ) 
    ; IF FastEOF ( Source ) 
      THEN 
        RETURN UniEncoding . ReplacementWch (* For B0. *)
      ELSE 
        B1 := ORD ( FastGetChar ( Source ) ) 
      ; ResultWt := Or ( LeftShift ( B0 , 8 ) , B1 )
      ; IF And ( 16_F800 , ResultWt ) = 16_D800 
        THEN RETURN UniEncoding . ReplacementWch (* For surrogate code. *)
        ELSE RETURN LOOPHOLE ( ResultWt , ArrWchWt ) [ ArrWchWt0 ] 
        END (* IF *) 
      END (* IF *) 
    END FastDecUCS2BE

; PROCEDURE FastEncUTF8 ( Sink : Wr . T ; Wch : Widechar ) 
 RAISES { Alerted , Wr . Failure , Range } 
  (* Raise Range if Wch is a surrogate code point.  Otherwise, encode Wch, 
     using UTF8, and write to Sink.  
  *) 

  = VAR B0 , B1 , B2 , B3 : Word . T (* Byte code units, in order written. *) 

  ; BEGIN 
      IF LE ( ORD ( Wch ) , 16_7F ) 
      THEN (* One byte encoding.  At most 7 nonzero bits in Ch. 
              Tag bit is implicitly already set. *) 
        FastPutChar ( Sink , LOOPHOLE ( Wch , ArrChWch ) [ ArrChWch0 ] )  
      ELSIF LE ( ORD ( Wch ) , 16_7FF )
      THEN (* Two byte encoding.  At most, 11 nonzero bits in Ch. *) 
        B0 := Or ( 16_C0 , RightShift ( ORD ( Wch ) , 6 ) )  
      ; B1 := Or ( 16_80 , And ( 16_3F , ORD ( Wch ) ) ) 
      ; FastPutChar ( Sink , LOOPHOLE ( B0 , ArrChWt ) [ ArrChWt0 ] ) 
      ; FastPutChar ( Sink , LOOPHOLE ( B1 , ArrChWt ) [ ArrChWt0 ] ) 
      ELSIF LE ( ORD ( Wch ) , 16_FFFF )
      THEN (* Three byte encoding.  At most, 16 nonzero bits in Ch. *) 
        IF And ( 16_F800 , ORD ( Wch ) ) = 16_D800 
        THEN (* Surrogate.  Not a Unicode scalar.  Illegal as unencoded. *) 
          RAISE Range  
        ELSE   
          B0 := Or ( 16_E0 , RightShift ( ORD ( Wch ) , 12 ) ) 
        ; B1 := Or ( 16_80 , And ( 16_3F , RightShift ( ORD ( Wch ) , 6 ) ) ) 
        ; B2 := Or ( 16_80 , And ( 16_3F , ORD ( Wch ) ) ) 
        ; FastPutChar ( Sink , LOOPHOLE ( B0 , ArrChWt ) [ ArrChWt0 ] ) 
        ; FastPutChar ( Sink , LOOPHOLE ( B1 , ArrChWt ) [ ArrChWt0 ] ) 
        ; FastPutChar ( Sink , LOOPHOLE ( B2 , ArrChWt ) [ ArrChWt0 ] ) 
        END (* IF *) 
      ELSE (* Four byte encoding. At most, 21 nonzero bits in Ch. *) 
        B0 := Or ( 16_F0 , RightShift ( ORD ( Wch ) , 18 ) ) 
      ; B1 := Or ( 16_80 , And ( 16_3F , RightShift ( ORD ( Wch ) , 12 ) ) ) 
      ; B2 := Or ( 16_80 , And ( 16_3F , RightShift ( ORD ( Wch ) , 6 ) ) ) 
      ; B3 := Or ( 16_80 , And ( 16_3F , ORD ( Wch ) ) ) 
      ; FastPutChar ( Sink , LOOPHOLE ( B0 , ArrChWt ) [ ArrChWt0 ] ) 
      ; FastPutChar ( Sink , LOOPHOLE ( B1 , ArrChWt ) [ ArrChWt0 ] ) 
      ; FastPutChar ( Sink , LOOPHOLE ( B2 , ArrChWt ) [ ArrChWt0 ] ) 
      ; FastPutChar ( Sink , LOOPHOLE ( B3 , ArrChWt ) [ ArrChWt0 ] ) 
      END (* IF *) 
    END FastEncUTF8

; PROCEDURE FastDecUTF8 ( Source : Rd . T ) : Widechar 
  RAISES { EndOfFile , Rd . Failure , Alerted } 
  (* Read and decode a code point from Source, using UTF8 encoding, and return
     it as a WIDECHAR.  Return any ill-formed input as the Unicode "replacement
     character", VAL(16_FFFD,WIDECHAR) 
  *)  

  = VAR B0 , B1 , B2 , B3 : Word . T (* Byte code units, in order read. *) 
  ; VAR V0 , V1 : Word . T 
    (* ^Extracted, right-justified data bits from the correspondingly-named 
       Bi's. *)
  ; VAR ResultWt : Word.T  

  ; BEGIN (* FastDecUTF8 *) 
      B0 := ORD ( FastGetChar ( Source ) ) 

      (* 1-byte encoding. *) 

    ; IF And ( 16_80 , B0 ) = 0 
      THEN (* Tag for 1-byte value. *) 
        RETURN LOOPHOLE ( B0 , ArrWchWt ) [ ArrWchWt0 ] 

      (* 2-byte encoding. *) 

      ELSIF And ( 16_E0 , B0 ) = 16_C0 
      THEN (* Tag for 2-byte value. *) 
        V0 := And ( 16_1F , B0 ) (* Data bits of B0. *)
      ; IF LT ( V0 , 2 ) (* Ill-formed data bits in byte 0. *) 
           OR FastEOF ( Source ) 
        THEN RETURN UniEncoding . ReplacementWch (* For B0 *)
        ELSE 
          B1 := ORD ( FastGetChar ( Source ) )
        ; IF And ( 16_C0 , B1 ) # 16_80 
          THEN (* Ill-formed tag bits in byte 1. *) 
            EVAL FastUnGetCharMulti ( Source ) (* B1. *) 
            (* ^ B1 is a legal B0 of a following code point. *) 
          ; RETURN UniEncoding . ReplacementWch (* For B0 *)
          ELSE 
            ResultWt := Or ( LeftShift ( V0 , 6 ) , And ( 16_3F , B1 ) ) 
          ; RETURN LOOPHOLE ( ResultWt , ArrWchWt ) [ ArrWchWt0 ] 
          END (* IF *) 
        END (* IF *)  

      (* 3-byte encoding. *) 

      ELSIF And ( 16_F0 , B0 ) = 16_E0 
      THEN (* Tag for 3-byte code. *) 
        IF FastEOF ( Source ) 
        THEN RETURN UniEncoding . ReplacementWch (* For B0 *) 
        ELSE 
          B1 := ORD ( FastGetChar ( Source ) )
        ; IF And ( 16_C0 , B1 ) # 16_80 
          THEN (* Ill-formed tag bits in byte 1. *) 
            EVAL FastUnGetCharMulti ( Source ) (* B1. *)  
            (* ^ B1 is a legal B0 of a following code point. *) 
          ; RETURN UniEncoding . ReplacementWch (* For B0 *)
          ELSE 
            V0 := And ( 16_F , B0 ) (* Data bits of B0. *)
          ; IF V0 = 0 AND And ( 16_20 , B1 ) # 16_20 
               OR V0 = 16_D AND And ( 16_20 , B1 ) # 0 
            THEN (* Ill-formed data bits in byte 1. *)  
              (* B1 can not be a legal B0 of a following code point. *) 
(* CHECK: Should we consume B2 if it is tagged as an additional byte? *) 
              RETURN UniEncoding . ReplacementWch (* For B0, B1 *)
            ELSE 
              IF FastEOF ( Source ) 
              THEN RETURN UniEncoding . ReplacementWch (* For B0, B1 *)
              ELSE 
                B2 := ORD ( FastGetChar ( Source ) )
              ; IF And ( 16_C0 , B2 ) # 16_80 
                THEN (* Ill-formed tag bits in byte 2. *) 
                  EVAL FastUnGetCharMulti ( Source ) (* B2. *)  
                  (* ^ B2 is a legal B0 of a following code point. *) 
                ; RETURN UniEncoding . ReplacementWch (* For B0, B1 *) 
                ELSE (* Well-formed 3-byte code. *)  
                  ResultWt 
                   := Or ( Or ( LeftShift ( V0 , 12 ) 
                              , LeftShift ( And ( 16_3F , B1 ) , 6 ) 
                              ) 
                         , And ( 16_3F , B2 )
                         ) 
                ; RETURN LOOPHOLE ( ResultWt , ArrWchWt ) [ ArrWchWt0 ] 
                END (* IF *) 
              END (* IF *) 
            END (* IF *) 
          END (* IF *) 
        END (* IF *) 

      (* 4-byte encoding. *) 

      ELSIF And ( 16_F8 , B0 ) = 16_F0 
      THEN (* Tag for 4-byte code. *) 
        IF FastEOF ( Source ) 
        THEN RETURN UniEncoding . ReplacementWch (* For B0 *)
        ELSE 
          B1 := ORD ( FastGetChar ( Source ) )
        ; IF And ( 16_C0 , B1 ) # 16_80 
          THEN (* Ill-formed tag bits in byte 1. *) 
            EVAL FastUnGetCharMulti ( Source ) (* B1. *) 
            (* ^ B1 is a legal B0 of a following code point. *) 
          ; RETURN UniEncoding . ReplacementWch (* For B0 *)
          ELSE 
            V0 := And ( 16_7 , B0 ) (* Data bits of B0. *)  
          ; V1 := And ( 16_3F , B1 ) (* Data bits of B1. *) 
          ; IF V0 = 0 AND LT ( V1 , 16_10 )  
               OR V0 = 16_4 AND GT ( V1 , 16_F ) 
            THEN (* Ill-formed data bits in byte 1. *)  
              (* B1 can not be a legal B0 of a following code point. *) 
(* CHECK: Should we consume B2 & B3 if they are tagged as additional bytes? *) 
              RETURN UniEncoding . ReplacementWch (* For B0, B1 *)
            ELSIF FastEOF ( Source ) 
            THEN RETURN UniEncoding . ReplacementWch (* For B0, B1 *)
            ELSE 
              B2 := ORD ( FastGetChar ( Source ) )
            ; IF And ( 16_C0 , B2 ) # 16_80 
              THEN (* Ill-formed tag bits in byte 2. *) 
                EVAL FastUnGetCharMulti ( Source ) (* B2 *)   
                (* ^ B2 is a legal B0 of a following code point. *) 
              ; RETURN UniEncoding . ReplacementWch (* For B0, B1 *)
              ELSIF FastEOF ( Source ) 
              THEN RETURN UniEncoding . ReplacementWch (* For B0, B1, B2 *)
              ELSE 
                B3 := ORD ( FastGetChar ( Source ) )
              ; IF And ( 16_C0 , B3 ) # 16_80 
                THEN (* Ill-formed tag bits in byte 3. *) 
                  EVAL FastUnGetCharMulti ( Source ) (* B3 *)  
                  (* ^ B3 is a legal B0 of a following code point. *) 
                ; RETURN UniEncoding . ReplacementWch (* For B0, B1, B2 *) 
                ELSE (* Well-formed 4-byte code. *) 
                  ResultWt 
                    := Or ( Or ( LeftShift ( V0 , 18 ) 
                               , LeftShift ( V1 , 12 ) 
                               ) 
                          , Or ( LeftShift ( And ( 16_3F , B2 ) , 6 ) 
                               , And ( 16_3F , B3 )
                               ) 
                          ) 
                ; RETURN <* NOWARN*>
(* COMPILER BUG?? ^On this line, CM3 says:
                   "warning: function may not return a value (FastDecUTF8)" *) 
                    LOOPHOLE ( ResultWt , ArrWchWt ) [ ArrWchWt0 ]  
                END (* IF *) 
              END (* IF *) 
            END (* IF *) 
          END (* IF *) 
        END (* IF *) 
      END (* IF *) 
    END FastDecUTF8

; PROCEDURE FastEncUTF16LE ( Sink : Wr . T ; Wch : Widechar ) 
  RAISES { Alerted , Wr . Failure , Range } 
  (* Raise Range if Wch is a surrogate code point.  Otherwise, encode Wch, 
     using UTF16LE encoding, and write to Sink.  
  *) 

  = VAR B0 , B1 , B2 , B3 : Word . T 
        (* ^Byte values, in temporal order of emission. *)
    (* In western human-readable order: B1, B0, B3, B2 *) 
  ; VAR x : Word . T 

  ; BEGIN 
      IF LE ( ORD ( Wch ) , 16_FFFF ) 
      THEN (* Will fit in 16-bit code unit. *) 
        IF And ( 16_F800 , ORD ( Wch ) ) = 16_D800 
        THEN (* Surrogate.  Not a Unicode scalar.  Illegal as unencoded. *) 
          RAISE Range  
        ELSE   
          B0 := And ( 16_FF , ORD ( Wch ) ) 
        ; B1 := RightShift ( ORD ( Wch ) , 8 ) 
        ; FastPutChar ( Sink , LOOPHOLE ( B0 , ArrChWt ) [ ArrChWt0 ] ) 
        ; FastPutChar ( Sink , LOOPHOLE ( B1 , ArrChWt ) [ ArrChWt0 ] ) 
        END (* IF *) 
      ELSE (* Will require two 16-bit code units. *)
        x := Minus ( RightShift ( ORD ( Wch ) , 10 ) , 16_40 ) 
      ; B0 := And ( 16_FF , x ) 
      ; B1 := Or ( 16_D8 , RightShift ( x , 8 ) ) 
      ; B2 := And ( 16_FF , ORD ( Wch ) ) 
      ; B3 := Or ( 16_DC , And ( 16_3 , RightShift ( ORD ( Wch ) , 8 ) ) )  
      ; FastPutChar ( Sink , LOOPHOLE ( B0 , ArrChWt ) [ ArrChWt0 ] ) 
      ; FastPutChar ( Sink , LOOPHOLE ( B1 , ArrChWt ) [ ArrChWt0 ] ) 
      ; FastPutChar ( Sink , LOOPHOLE ( B2 , ArrChWt ) [ ArrChWt0 ] ) 
      ; FastPutChar ( Sink , LOOPHOLE ( B3 , ArrChWt ) [ ArrChWt0 ] ) 
      END (* IF *) 
    END FastEncUTF16LE

; PROCEDURE FastDecUTF16LE ( Source : Rd . T ) : Widechar 
  RAISES { EndOfFile , Rd . Failure , Alerted } 
  (* Read and decode a code point from Source, using UTF16LE, and return
     it as a WIDECHAR.  Return any ill-formed input as the Unicode "replacement
     character", VAL(16_FFFD,WIDECHAR) 
  *)  

  = VAR B0 , B1 , B2 , B3 : Word . T (* Byte values, in temporal order read. *) 
    (* In western human-readable order: B1, B0, B3, B2 *) 
  ; VAR ResultWt : Word . T 

  ; BEGIN (* FastDecUTF16LE *) 
      B0 := ORD ( FastGetChar ( Source ) ) 
    ; IF FastEOF ( Source ) 
      THEN (* Only half a code unit. *)  
        RETURN UniEncoding . ReplacementWch (* For B0 *)
      ELSE 
        B1 := ORD ( FastGetChar ( Source ) ) 
      END (* IF *) 
    ; IF And ( 16_F8 , B1 ) # 16_D8 
      THEN (* Non-surrogate.  A one-code-unit encoding. *) 
        ResultWt := Or ( LeftShift ( B1 , 8 ) , B0 ) 
      ; RETURN LOOPHOLE ( ResultWt , ArrWchWt ) [ ArrWchWt0 ] 
      ELSE (* Surrogate. *) 
        IF And ( 16_04 , B1 ) = 16_04
        THEN (* Ill-formed: Code point starts with 2nd surrogate. *) 
          RETURN UniEncoding . ReplacementWch (* For B0, B1 *)
        ELSE (* Code point begins with 1st surrogate. *) 
          IF FastEOF ( Source ) 
          THEN (* No 2nd surrogate. *) 
            RETURN UniEncoding . ReplacementWch (* For B0, B1 *)
          ELSE 
            B2 := ORD ( FastGetChar ( Source ) ) 
          ; IF FastEOF ( Source ) 
            THEN (* Only one byte of following surrogate. *)  
              EVAL FastUnGetCharMulti ( Source ) (* B2. *) 
              (* ^This is uncertain. If client switched to a different
                 encoding, could it would want this byte? *)  
            ; RETURN UniEncoding . ReplacementWch (* For B0, B1 *)
            ELSE 
              B3 := ORD ( FastGetChar ( Source ) ) 
            ; IF And ( 16_FC , B3 ) # 16_DC 
              THEN (* Ill-formed.  Not a 2nd surrogate. *) 
              (* B3:B2 are a following well-formed UTF16 code unit. *) 
              (* NOTE: This is the only place where we need 2-byte
                       lookahead.  We had to read B3 to check for tag
                       bits, which, in little-endian, is read after
                       B2.  But now we know B3:B2 are a legal first
                       code unit for the next code point, so we need to
                       save them for reprocessing on the next call.  
              *)
                EVAL FastUnGetCharMulti ( Source ) (* B3 *) 
              ; EVAL FastUnGetCharMulti ( Source ) (* B2 *) 
              ; RETURN UniEncoding . ReplacementWch (* For B0, B1 *)
              ELSE (* 2nd surrogate. *) 
                ResultWt 
                  := Or ( Plus ( Or ( LeftShift ( And ( 16_3 , B1 ) , 18 ) 
                                    , LeftShift ( B0 , 10 ) 
                                    ) 
                               , 16_10000 
                               ) 
                        , Or ( LeftShift ( And ( 16_3 , B3 ) , 8 ) 
                             , B2 
                             )
                        ) 
              ; RETURN LOOPHOLE ( ResultWt , ArrWchWt ) [ ArrWchWt0 ] 
              END (* IF *) 
            END (* IF *) 
          END (* IF *) 
        END (* IF *) 
      END (* IF *) 
    END FastDecUTF16LE

; PROCEDURE FastEncUTF16BE ( Sink : Wr . T ; Wch : Widechar ) 
  RAISES { Alerted , Wr . Failure , Range } 
  (* Raise Range if Wch is a surrogate code point.  Otherwise, encode Wch, 
     using UTF16BE encoding, and write to Sink.  
  *) 

  = VAR B0 , B1 , B2 , B3 : Word . T 
        (* ^Byte values, in temporal order of emission. *)
    (* In western human-readable order: B0, B1, B2, B3 *) 
  ; VAR x : Word . T 

  ; BEGIN 
      IF LE ( ORD ( Wch ) , 16_FFFF ) 
      THEN (* Will fit in 16-bit code unit. *) 
        IF And ( 16_F800 , ORD ( Wch ) ) = 16_D800 
        THEN (* Surrogate.  Not a Unicode scalar.  Illegal as unencoded. *) 
          RAISE Range  
        ELSE   
          B0 := RightShift ( ORD ( Wch ) , 8 ) 
        ; B1 := And ( 16_FF , ORD ( Wch ) ) 
        ; FastPutChar ( Sink , LOOPHOLE ( B0 , ArrChWt ) [ ArrChWt0 ] ) 
        ; FastPutChar ( Sink , LOOPHOLE ( B1 , ArrChWt ) [ ArrChWt0 ] ) 
        END (* IF *) 
      ELSE (* Will require two 16-bit code units. *)
        x := Minus ( RightShift ( ORD ( Wch ) , 10 ) , 16_40 ) 
      ; B0 := Or ( 16_D8 , RightShift ( x , 8 ) ) 
      ; B1 := And ( 16_FF , x ) 
      ; B2 := Or ( 16_DC , And ( 16_3 , RightShift ( ORD ( Wch ) , 8 ) ) )
      ; B3 := And ( 16_FF , ORD ( Wch ) ) 
      ; FastPutChar ( Sink , LOOPHOLE ( B0 , ArrChWt ) [ ArrChWt0 ] ) 
      ; FastPutChar ( Sink , LOOPHOLE ( B1 , ArrChWt ) [ ArrChWt0 ] ) 
      ; FastPutChar ( Sink , LOOPHOLE ( B2 , ArrChWt ) [ ArrChWt0 ] ) 
      ; FastPutChar ( Sink , LOOPHOLE ( B3 , ArrChWt ) [ ArrChWt0 ] ) 
      END (* IF *) 
    END FastEncUTF16BE

; PROCEDURE FastDecUTF16BE ( Source : Rd . T ) : Widechar 
  RAISES { EndOfFile , Rd . Failure , Alerted } 
  (* Read and decode a code point from Source, using UTF16BE, and return
     it as a WIDECHAR.  Return any ill-formed input as the Unicode "replacement
     character", VAL(16_FFFD,WIDECHAR) 
  *)  

  = VAR B0 , B1 , B2 , B3 : Word . T (* Byte values, in temporal order read. *) 
    (* In western human-readable order: B0, B1, B2, B3 *) 
  ; VAR ResultWt : Word . T 

  ; BEGIN (* FastDecUTF16BE *) 
      B0 := ORD ( FastGetChar ( Source ) ) 
    ; IF FastEOF ( Source ) 
      THEN (* Only half a code unit. *)  
        RETURN UniEncoding . ReplacementWch (* For B0 *)
      ELSE 
        B1 := ORD ( FastGetChar ( Source ) ) 
      ; IF And ( 16_F8 , B0 ) # 16_D8 
        THEN (* Non-surrogate.  A one-code-unit encoding. *) 
          ResultWt := Or ( LeftShift ( B0 , 8 ) , B1 ) 
        ; RETURN LOOPHOLE ( ResultWt , ArrWchWt ) [ ArrWchWt0 ] 
        ELSE (* Surrogate. *) 
          IF And ( 16_04 , B0 ) = 16_04  
          THEN (* Ill-formed: Code point starts with 2nd surrogate. *) 
            RETURN UniEncoding . ReplacementWch  (* For B0, B1 *)
          ELSE (* Code point begins with 1st surrogate. *) 
            IF FastEOF ( Source ) 
            THEN (* No 2nd surrogate. *) 
              RETURN UniEncoding . ReplacementWch (* For B0, B1 *)
            ELSE 
              B2 := ORD ( FastGetChar ( Source ) ) 
            ; IF And ( 16_FC , B2 ) # 16_DC 
              THEN (* Ill-formed.  Not a 2nd surrogate. *) 
              (* B2 begins a following well-formed initial UTF16 code unit. *)
                EVAL FastUnGetCharMulti ( Source ) (* B2. *) 
              ; RETURN UniEncoding . ReplacementWch (* For B0, B1 *)
              ELSE (* 2nd surrogate. *) 
                IF FastEOF ( Source ) 
                THEN (* Only one byte of following 2nd surrogate. *)  
                  EVAL FastUnGetCharMulti ( Source ) (* B2. *) 
                  (* ^This is uncertain. If client switched to a different
                     encoding, could it would want this byte? *)  
                ; RETURN UniEncoding . ReplacementWch (* For B0, B1 *)
                ELSE 
                  B3 := ORD ( FastGetChar ( Source ) ) 
                ; ResultWt 
                    := Or ( Plus ( Or ( LeftShift ( And ( 16_3 , B0 ) , 18 ) 
                                      , LeftShift ( B1 , 10 ) 
                                      ) 
                                 , 16_10000 
                                 ) 
                          , Or ( LeftShift ( And ( 16_3 , B2 ) , 8 ) 
                               , B3 
                               )
                          ) 
                ; RETURN LOOPHOLE ( ResultWt , ArrWchWt ) [ ArrWchWt0 ] 
                END (* IF *) 
              END (* IF *) 
            END (* IF *) 
          END (* IF *) 
        END (* IF *) 
      END (* IF *) 
    END FastDecUTF16BE

; PROCEDURE FastEncUTF32LE ( Sink : Wr . T ; Wch : Widechar ) 
  RAISES { Alerted , Wr . Failure , Range } 
  (* Raise Range if Wch is a surrogate code point.  Otherwise, encode Wch, 
     using UTF32LE encoding, and write to Sink.  
  *) 

  = BEGIN
      IF And ( 16_FFF800 , ORD ( Wch ) ) = 16_D800 
      THEN (* Surrogate. *) 
        RAISE Range 
      ELSE  
        FastPutChar ( Sink , LOOPHOLE ( Wch , ArrChWch ) [ ArrChWch0 ] ) 
      ; FastPutChar ( Sink , LOOPHOLE ( Wch , ArrChWch ) [ ArrChWch1 ] ) 
      ; FastPutChar ( Sink , LOOPHOLE ( Wch , ArrChWch ) [ ArrChWch2 ] ) 
      ; FastPutChar ( Sink , LOOPHOLE ( Wch , ArrChWch ) [ ArrChWch3 ] ) 
      END (* IF *) 
    END FastEncUTF32LE

; PROCEDURE FastDecUTF32LE ( Source : Rd . T ) : Widechar 
  RAISES { EndOfFile , Rd . Failure , Alerted } 
  (* Read and decode a code point from Source, using UTF32LE encoding, 
     and return it as a WIDECHAR.  Return any ill-formed input as the Unicode 
     "replacement character", VAL(16_FFFD,WIDECHAR) 
  *)  

  = VAR B0 , B1 , B2 , B3 : Word . T (* Byte values, in temporal order read. *) 
    (* In western human-readable order: B3, B2, B1, B0 *) 
  ; VAR ResultWt : Word . T 

  ; BEGIN (* FastDecUTF32LE *) 
      B0 := ORD ( FastGetChar ( Source ) ) 
    ; IF FastEOF ( Source ) 
      THEN RETURN UniEncoding . ReplacementWch (* For B0 *)
      ELSE 
        B1 := ORD ( FastGetChar ( Source ) ) 
      ; IF FastEOF ( Source ) 
        THEN RETURN UniEncoding . ReplacementWch (* For B0, B1 *) 
        ELSE 
          B2 := ORD ( FastGetChar ( Source ) ) 
        ; IF FastEOF ( Source ) 
          THEN RETURN UniEncoding . ReplacementWch (* For B0, B1, B2 *)
          ELSE 
            B3 := ORD ( FastGetChar ( Source ) ) 
          ; ResultWt 
              := Or ( Or ( LeftShift ( B3 , 24 ) , LeftShift ( B2 , 16 ) )
                    , Or ( LeftShift ( B1 , 8 ) , B0 ) 
                    ) 
          ; IF GT ( ResultWt , 16_10FFFF ) (* Too large. *) 
               OR And ( 16_FFF800 , ResultWt ) = 16_D800 (* Surrogate. *) 
            THEN RETURN UniEncoding . ReplacementWch (* For B0, B1, B2, B3 *)
            ELSE RETURN LOOPHOLE ( ResultWt , ArrWchWt ) [ ArrWchWt0 ] 
            END (* IF *) 
          END (* IF *) 
        END (* IF *) 
      END (* IF *) 
    END FastDecUTF32LE

; PROCEDURE FastEncUTF32BE ( Sink : Wr . T ; Wch : Widechar ) 
  RAISES { Alerted , Wr . Failure , Range } 
  (* Raise Range if Wch is a surrogate code point.  Otherwise, encode Wch, 
     using UTF32BE encoding, and write to Sink.  
  *) 

  = BEGIN 
      IF And ( 16_FFF800 , ORD ( Wch ) ) = 16_D800 
      THEN (* Surrogate. *) 
        RAISE Range 
      ELSE  
        FastPutChar ( Sink , LOOPHOLE ( Wch , ArrChWch ) [ ArrChWch3 ] ) 
      ; FastPutChar ( Sink , LOOPHOLE ( Wch , ArrChWch ) [ ArrChWch2 ] ) 
      ; FastPutChar ( Sink , LOOPHOLE ( Wch , ArrChWch ) [ ArrChWch1 ] ) 
      ; FastPutChar ( Sink , LOOPHOLE ( Wch , ArrChWch ) [ ArrChWch0 ] ) 
      END (* IF *) 
    END FastEncUTF32BE

; PROCEDURE FastDecUTF32BE ( Source : Rd . T ) : Widechar 
  RAISES { EndOfFile , Rd . Failure , Alerted } 
  (* Read and decode a code point from Source, using UTF32BE encoding, 
     and return it as a WIDECHAR.  Return any ill-formed input as the Unicode 
     "replacement character", VAL(16_FFFD,WIDECHAR) 
  *)  

  = VAR B0 , B1 , B2 , B3 : Word . T (* Byte values, in temporal order read. *) 
    (* In western human-readable order: B0, B1, B2, B3 *) 
  ; VAR ResultWt : Word . T 

  ; BEGIN (* FastDecUTF32BE *) 
      B0 := ORD ( FastGetChar ( Source ) ) 
    ; IF FastEOF ( Source ) 
      THEN RETURN UniEncoding . ReplacementWch (* For B0 *) 
      ELSE 
        B1 := ORD ( FastGetChar ( Source ) ) 
      ; IF FastEOF ( Source ) 
        THEN RETURN UniEncoding . ReplacementWch (* For B0, B1 *)
        ELSE 
          B2 := ORD ( FastGetChar ( Source ) ) 
        ; IF FastEOF ( Source ) 
          THEN RETURN UniEncoding . ReplacementWch (* For B0, B1, B2 *)
          ELSE 
            B3 := ORD ( FastGetChar ( Source ) ) 
          ; ResultWt 
              := Or ( Or ( LeftShift ( B0 , 24 ) , LeftShift ( B1 , 16 ) )
                    , Or ( LeftShift ( B2 , 8 ) , B3 ) 
                    ) 
          ; IF GT ( ResultWt , 16_10FFFF ) (* Too large. *) 
               OR And ( 16_FFF800 , ResultWt ) = 16_D800 (* Surrogate. *) 
            THEN RETURN UniEncoding . ReplacementWch (* For B0, B1, B2, B3 *) 
            ELSE RETURN LOOPHOLE ( ResultWt , ArrWchWt ) [ ArrWchWt0 ] 
            END (* IF *) 
          END (* IF *) 
        END (* IF *) 
      END (* IF *) 
    END FastDecUTF32BE

; BEGIN (* UnsafeUniCodec *)
  END UnsafeUniCodec 
. 
