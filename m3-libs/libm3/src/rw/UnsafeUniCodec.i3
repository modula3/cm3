
(* Coding and decoding for the unicode encoding schemes, as well as 
   a some others.
*) 

INTERFACE UnsafeUniCodec 

; IMPORT Rd 
; FROM Rd IMPORT EndOfFile  
; FROM Thread IMPORT Alerted 
; FROM UniEncoding IMPORT Encoding 
; FROM UniCodec IMPORT DecProc , DecTblTyp , EncProc , EncTblTyp , IsBE , Range 
; FROM UniCodec IMPORT Widechar  
; IMPORT Wr 

; CONST EncTable
    = ARRAY BOOLEAN (* Big endian. *) OF EncTblTyp 
      { EncTblTyp (* Little endian. *)
          { (* Null *)      FastEncNull     
          , (* Internal *)  FastEncInternal  
          , (* ISO8859_1 *) FastEncISO8859_1
          , (* CM3WC *)     FastEncCM3WC
          , (* UCS2 *)      FastEncUCS2LE
          , (* UCS2LE *)    FastEncUCS2LE
          , (* UCS2BE *)    FastEncUCS2BE
          , (* UTF8 *)      FastEncUTF8
          , (* UTF16 *)     FastEncUTF16LE
          , (* UTF16LE *)   FastEncUTF16LE
          , (* UTF16BE *)   FastEncUTF16BE
          , (* UTF32 *)     FastEncUTF32LE
          , (* UTF32LE *)   FastEncUTF32LE
          , (* UTF32BE *)   FastEncUTF32BE
          } 

      , EncTblTyp (* Big endian. *)
          { (* Null *)      FastEncNull       
          , (* Internal *)  FastEncInternal  
          , (* ISO8859_1 *) FastEncISO8859_1
          , (* CM3WC *)     FastEncCM3WC
          , (* UCS2 *)      FastEncUCS2BE
          , (* UCS2LE *)    FastEncUCS2LE
          , (* UCS2BE *)    FastEncUCS2BE
          , (* UTF8 *)      FastEncUTF8
          , (* UTF16 *)     FastEncUTF16BE
          , (* UTF16LE *)   FastEncUTF16LE
          , (* UTF16BE *)   FastEncUTF16BE
          , (* UTF32 *)     FastEncUTF32BE
          , (* UTF32LE *)   FastEncUTF32LE
          , (* UTF32BE *)   FastEncUTF32BE
          } 
      } 

; CONST DecTable
    = ARRAY BOOLEAN (* Big endian. *) OF DecTblTyp 
      { DecTblTyp (* Little endian. *)
          { (* Null *)      FastDecNull     
          , (* Internal *)  FastDecInternal  
          , (* ISO8859_1 *) FastDecISO8859_1
          , (* CM3WC *)     FastDecCM3WC
          , (* UCS2 *)      FastDecUCS2LE
          , (* UCS2LE *)    FastDecUCS2LE
          , (* UCS2BE *)    FastDecUCS2BE
          , (* UTF8 *)      FastDecUTF8
          , (* UTF16 *)     FastDecUTF16LE
          , (* UTF16LE *)   FastDecUTF16LE
          , (* UTF16BE *)   FastDecUTF16BE
          , (* UTF32 *)     FastDecUTF32LE
          , (* UTF32LE *)   FastDecUTF32LE
          , (* UTF32BE *)   FastDecUTF32BE
          } 

      , DecTblTyp (* Big endian. *)
          { (* Null *)      FastDecNull       
          , (* Internal *)  FastDecInternal  
          , (* ISO8859_1 *) FastDecISO8859_1
          , (* CM3WC *)     FastDecCM3WC
          , (* UCS2 *)      FastDecUCS2BE
          , (* UCS2LE *)    FastDecUCS2LE
          , (* UCS2BE *)    FastDecUCS2BE
          , (* UTF8 *)      FastDecUTF8
          , (* UTF16 *)     FastDecUTF16BE
          , (* UTF16LE *)   FastDecUTF16LE
          , (* UTF16BE *)   FastDecUTF16BE
          , (* UTF32 *)     FastDecUTF32BE
          , (* UTF32LE *)   FastDecUTF32LE
          , (* UTF32BE *)   FastDecUTF32BE
          } 
      } 

; PROCEDURE FastEncode ( Enc : Encoding ; Sink : Wr . T ; Wch : Widechar ) 
  RAISES { Alerted , Wr . Failure , Range } 
  (* Raise Range if Wch is not valid for Enc. 
     Otherwise, Encode Wch, using Enc, and write to Sink.  
  *) 

; PROCEDURE FastDecode ( Enc : Encoding ; Source : Rd . T ) : Widechar 
   RAISES { EndOfFile , Rd . Failure , Alerted } 
  (* Read and decode a code point from Source, using Enc, and return
     it as a WIDECHAR.  Return any ill-formed input as the Unicode "replacement
     character", VAL(16_FFFD,WIDECHAR) 
  *)  

; PROCEDURE FastEncNull ( Sink : Wr . T ; Wch : Widechar ) 
  RAISES { Alerted , Wr . Failure , Range } 
  (* A NOOP.  Placeholder for Encoding.Null. *) 

; PROCEDURE FastDecNull ( Source : Rd . T ) : Widechar
  RAISES { EndOfFile , Rd . Failure , Alerted } 
  (* A NOOP.  Placeholder for Encoding.Null. *) 

; PROCEDURE FastEncInternal ( Sink : Wr . T ; Wch : Widechar ) 
  RAISES { Alerted , Wr . Failure , Range } 
  (* A NOOP. Placeholder for Encoding.Internal, which is used for
     in-memory representations normally hidden by abstractions. 
  *) 

; PROCEDURE FastDecInternal ( Source : Rd . T ) : Widechar
   RAISES { EndOfFile , Rd . Failure , Alerted } 
  (* A NOOP. Placeholder for Encoding.Internal, which is used for
     in-memory representations normally hidden by abstractions. 
  *) 

; PROCEDURE FastEncISO8859_1 ( Sink : Wr . T ; Wch : Widechar ) 
  RAISES { Alerted , Wr . Failure , Range } 
  (* Raise Range if ORD(Wch) > 16_FF.  Otherwise, write Wch to Sink, 
     in ISO 8859-1 encoding.  This encoding is just byte identity.   
     It is the same as original Modula-3 PutChar. 
  *) 

; PROCEDURE FastDecISO8859_1 ( Source : Rd . T ) : Widechar 
  RAISES { EndOfFile , Rd . Failure , Alerted } 
  (* Read and decode a code point from Source, using ISO 8859-1 encoding, 
     and return it as a WIDECHAR.  This encoding is the same as original 
     Modula-3 GetChar and is an identity on a single byte.  ORD(<result>)
     will be <= 16_FF.  No ill-formed cases exist. 
  *)  

; PROCEDURE FastEncCM3WC ( Sink : Wr . T ; Wch : Widechar )
  RAISES { Alerted , Wr . Failure , Range } 
  (* Raise Range if ORD(Wch) > 16_FFFF.  Otherwise, Encode Wch, using
     CM3WC encoding, and write to Sink.  This is just 2-byte fixed-size 
     little endian, the same as the original Cm3 PutWideChar. 
  *)   

; PROCEDURE FastDecCM3WC ( Source : Rd . T ) : Widechar 
  RAISES { EndOfFile , Rd . Failure , Alerted } 
  (* Read and decode a code point from Source, using CM3WC encoding, and return
     it as a WIDECHAR.  This is just 2-byte fixed-size little endian, 
     the same as the original Cm3 GetWideChar.  ORD(<result>) will 
     be <= 16_FFFF.  Just zero-extend an odd byte before EOF.  
     No ill-formed cases exist. 
  *)  

; CONST FastEncUCS2 : EncProc = EncTable [ IsBE , Encoding . UCS2 ] 
  (* PROCEDURE FastEncUCS2 ( Sink : Wr . T ; Wch : Widechar )
     RAISES { Alerted , Wr . Failure , Range } *)  
  (* Raise Range if ORD(Wch) > 16_FFFF or Wch is a surrogate code.  
     Otherwise, encode Wch, using UCS2 encoding, native endian, 
     and write to Sink as 2-byte fixed size.  
  *) 

; CONST FastDecUCS2 : DecProc = DecTable [ IsBE , Encoding . UCS2 ] 
  (* PROCEDURE FastDecUCS2 ( Source : Rd . T ) : Widechar 
     RAISES { EndOfFile , Rd . Failure , Alerted } *) 
  (* Read and decode a code point from Source, using Enc, and return
     it as a WIDECHAR.  ORD(<result>) will be <= 16_FFFF.  Return any 
     ill-formed input as the Unicode "replacement character", 
     VAL(16_FFFD,WIDECHAR) 
  *)  

; PROCEDURE FastEncUCS2LE ( Sink : Wr . T ; Wch : Widechar )
  RAISES { Alerted , Wr . Failure , Range } 
  (* Raise Range if ORD(Wch) > 16_FFFF or Wch is a surrogate code.  
     Otherwise, Encode Wch, using UCS2LE encoding, and write to Sink  
     as 2-byte fixed-size little endian. 
  *) 

; PROCEDURE FastDecUCS2LE ( Source : Rd . T ) : Widechar 
  RAISES { EndOfFile , Rd . Failure , Alerted } 
  (* Read and decode a code point from Source, using UCS2LE encoding, and 
     return it as a WIDECHAR.  ORD(<result>) will be <= 16_FFFF.  Return 
     any ill-formed input as the Unicode "replacement character", 
     VAL(16_FFFD,WIDECHAR) 
  *)  

; PROCEDURE FastEncUCS2BE ( Sink : Wr . T ; Wch : Widechar )
  RAISES { Alerted , Wr . Failure , Range } 
  (* Raise Range if ORD(Wch) > 16_FFFF or Wch is a surrogate code.  
     Otherwise, Encode Wch, using UCS2BE encoding, and write to Sink  
     as 2-byte fixed-size big endian. 
  *) 

; PROCEDURE FastDecUCS2BE ( Source : Rd . T ) : Widechar 
  RAISES { EndOfFile , Rd . Failure , Alerted } 
  (* Read and decode a code point from Source, using UCS2BE encoding, and 
     return it as a WIDECHAR.  ORD(<result>) will be <= 16_FFFF.  Return 
     any ill-formed input as the Unicode "replacement character", 
     VAL(16_FFFD,WIDECHAR) 
  *)  

; PROCEDURE FastEncUTF8 ( Sink : Wr . T ; Wch : Widechar ) 
 RAISES { Alerted , Wr . Failure , Range } 
  (* Raise Range if Wch is a surrogate code point.  Otherwise, encode Wch, 
     using UTF8, and write to Sink.  
  *) 

; PROCEDURE FastDecUTF8 ( Source : Rd . T ) : Widechar 
  RAISES { EndOfFile , Rd . Failure , Alerted } 
  (* Read and decode a code point from Source, using UTF8 encoding, and return
     it as a WIDECHAR.  Return any ill-formed input as the Unicode "replacement
     character", VAL(16_FFFD,WIDECHAR) 
  *)  

; CONST FastEncUTF16 : EncProc = EncTable [ IsBE , Encoding . UTF16 ] 
  (* PROCEDURE FastEncUTF16 ( Sink : Wr . T ; Wch : Widechar ) 
     RAISES { Alerted , Wr . Failure , Range } *) 
  (* Raise Range if Wch is a surrogate code point.  Otherwise, encode Wch, 
     using UTF16 encoding, native endian, and write to Sink.  
  *) 

; CONST FastDecUTF16 : DecProc = DecTable [ IsBE , Encoding . UTF16 ] 
  (* PROCEDURE FastDecUTF16 ( Source : Rd . T ) : Widechar 
     RAISES { EndOfFile , Rd . Failure , Alerted } *) 
  (* Read and decode a code point from Source, using UTF16, native endian, 
     and return it as a WIDECHAR.  Return any ill-formed input as the Unicode 
     "replacement character", VAL(16_FFFD,WIDECHAR) 
  *)  

; PROCEDURE FastEncUTF16LE ( Sink : Wr . T ; Wch : Widechar ) 
  RAISES { Alerted , Wr . Failure , Range } 
  (* Raise Range if Wch is a surrogate code point.  Otherwise, encode Wch, 
     using UTF16LE encoding, and write to Sink.  
  *) 

; PROCEDURE FastDecUTF16LE ( Source : Rd . T ) : Widechar 
  RAISES { EndOfFile , Rd . Failure , Alerted } 
  (* Read and decode a code point from Source, using UTF16LE, and return
     it as a WIDECHAR.  Return any ill-formed input as the Unicode "replacement
     character", VAL(16_FFFD,WIDECHAR) 
  *)  

; PROCEDURE FastEncUTF16BE ( Sink : Wr . T ; Wch : Widechar ) 
  RAISES { Alerted , Wr . Failure , Range } 
  (* Raise Range if Wch is a surrogate code point.  Otherwise, encode Wch, 
     using UTF16BE encoding, and write to Sink.  
  *) 

; PROCEDURE FastDecUTF16BE ( Source : Rd . T ) : Widechar 
  RAISES { EndOfFile , Rd . Failure , Alerted } 
  (* Read and decode a code point from Source, using UTF16BE, and return
     it as a WIDECHAR.  Return any ill-formed input as the Unicode "replacement
     character", VAL(16_FFFD,WIDECHAR) 
  *)  

; CONST FastEncUTF32 : EncProc = EncTable [ IsBE , Encoding . UTF32 ] 
  (* PROCEDURE FastEncUTF32 ( Sink : Wr . T ; Wch : Widechar ) 
     RAISES { Alerted , Wr . Failure , Range } *) 
  (* Raise Range if Wch is a surrogate code point.  Otherwise, encode Wch, 
     using UTF32 encoding, native endian, and write to Sink.  
  *) 

; CONST FastDecUTF32 : DecProc = DecTable [ IsBE , Encoding . UTF32 ] 
  (* PROCEDURE FastDecUTF32 ( Source : Rd . T ) : Widechar 
     RAISES { EndOfFile , Rd . Failure , Alerted } *) 
  (* Read and decode a code point from Source, using UTF32, native endian, 
     and return it as a WIDECHAR.  Return any ill-formed input as the Unicode 
     "replacement character", VAL(16_FFFD,WIDECHAR) 
  *)  

; PROCEDURE FastEncUTF32LE ( Sink : Wr . T ; Wch : Widechar ) 
  RAISES { Alerted , Wr . Failure , Range } 
  (* Raise Range if Wch is a surrogate code point.  Otherwise, encode Wch, 
     using UTF32LE encoding, and write to Sink.  
  *) 

; PROCEDURE FastDecUTF32LE ( Source : Rd . T ) : Widechar 
  RAISES { EndOfFile , Rd . Failure , Alerted } 
  (* Read and decode a code point from Source, using UTF32LE encoding, 
     and return it as a WIDECHAR.  Return any ill-formed input as the Unicode 
     "replacement character", VAL(16_FFFD,WIDECHAR) 
  *)  

; PROCEDURE FastEncUTF32BE ( Sink : Wr . T ; Wch : Widechar ) 
  RAISES { Alerted , Wr . Failure , Range } 
  (* Raise Range if Wch is a surrogate code point.  Otherwise, encode Wch, 
     using UTF32BE encoding, and write to Sink.  
  *) 

; PROCEDURE FastDecUTF32BE ( Source : Rd . T ) : Widechar 
  RAISES { EndOfFile , Rd . Failure , Alerted } 
  (* Read and decode a code point from Source, using UTF32BE encoding, 
     and return it as a WIDECHAR.  Return any ill-formed input as the Unicode 
     "replacement character", VAL(16_FFFD,WIDECHAR) 
  *)  

; END UnsafeUniCodec 
. 
