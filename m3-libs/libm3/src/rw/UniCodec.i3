
(* Coding an decoding for the unicode encoding schemes, as well as 
   a some others.
*) 

INTERFACE UniCodec 

; IMPORT Endian 
; IMPORT Rd 
; FROM Rd IMPORT EndOfFile  
; FROM Thread IMPORT Alerted 
; IMPORT UniEncoding 
; FROM UniEncoding IMPORT Encoding 
; IMPORT Wr 

; EXCEPTION Range 

; TYPE Widechar = WIDECHAR (* For testing when WIDECHAR isn't wide enough. *)  
(* ; TYPE Widechar = [ 0 .. 16_10FFFF ] *) 
  (* Temporary, for testing.  When WIDECHAR becomes a character type over
     this range, and with BYTESIZE = 4, we can replace this by WIDECHAR. *)  

; CONST IsBE = Endian . Value = Endian . T . Big 

; TYPE EncProc 
    = PROCEDURE ( Sink : Wr . T ; Wch : Widechar ) 
      RAISES { Alerted , Wr . Failure , Range } 

; TYPE EncTblTyp = ARRAY Encoding OF EncProc 

; CONST EncTable
    = ARRAY BOOLEAN (* Big endian. *) OF EncTblTyp 
      { EncTblTyp (* Little endian. *)
          { (* Null *)      EncNull     
          , (* Internal *)  EncInternal  
          , (* ISO8859_1 *) EncISO8859_1
          , (* CM3WC *)     EncCM3WC
          , (* UCS2 *)      EncUCS2LE
          , (* UCS2LE *)    EncUCS2LE
          , (* UCS2BE *)    EncUCS2BE
          , (* UTF8 *)      EncUTF8
          , (* UTF16 *)     EncUTF16LE
          , (* UTF16LE *)   EncUTF16LE
          , (* UTF16BE *)   EncUTF16BE
          , (* UTF32 *)     EncUTF32LE
          , (* UTF32LE *)   EncUTF32LE
          , (* UTF32BE *)   EncUTF32BE
          } 

      , EncTblTyp (* Big endian. *)
          { (* Null *)      EncNull       
          , (* Internal *)  EncInternal  
          , (* ISO8859_1 *) EncISO8859_1
          , (* CM3WC *)     EncCM3WC
          , (* UCS2 *)      EncUCS2BE
          , (* UCS2LE *)    EncUCS2LE
          , (* UCS2BE *)    EncUCS2BE
          , (* UTF8 *)      EncUTF8
          , (* UTF16 *)     EncUTF16BE
          , (* UTF16LE *)   EncUTF16LE
          , (* UTF16BE *)   EncUTF16BE
          , (* UTF32 *)     EncUTF32BE
          , (* UTF32LE *)   EncUTF32LE
          , (* UTF32BE *)   EncUTF32BE
          } 
      } 

; TYPE DecProc 
    = PROCEDURE ( Source : Rd . T ) : Widechar  
      RAISES { EndOfFile , Rd . Failure , Alerted } 

; TYPE DecTblTyp = ARRAY Encoding OF DecProc 

; CONST DecTable
    = ARRAY BOOLEAN (* Big endian. *) OF DecTblTyp 
      { DecTblTyp (* Little endian. *)
          { (* Null *)      DecNull     
          , (* Internal *)  DecInternal  
          , (* ISO8859_1 *) DecISO8859_1
          , (* CM3WC *)     DecCM3WC
          , (* UCS2 *)      DecUCS2LE
          , (* UCS2LE *)    DecUCS2LE
          , (* UCS2BE *)    DecUCS2BE
          , (* UTF8 *)      DecUTF8
          , (* UTF16 *)     DecUTF16LE
          , (* UTF16LE *)   DecUTF16LE
          , (* UTF16BE *)   DecUTF16BE
          , (* UTF32 *)     DecUTF32LE
          , (* UTF32LE *)   DecUTF32LE
          , (* UTF32BE *)   DecUTF32BE
          } 

      , DecTblTyp (* Big endian. *)
          { (* Null *)      DecNull       
          , (* Internal *)  DecInternal  
          , (* ISO8859_1 *) DecISO8859_1
          , (* CM3WC *)     DecCM3WC
          , (* UCS2 *)      DecUCS2BE
          , (* UCS2LE *)    DecUCS2LE
          , (* UCS2BE *)    DecUCS2BE
          , (* UTF8 *)      DecUTF8
          , (* UTF16 *)     DecUTF16BE
          , (* UTF16LE *)   DecUTF16LE
          , (* UTF16BE *)   DecUTF16BE
          , (* UTF32 *)     DecUTF32BE
          , (* UTF32LE *)   DecUTF32LE
          , (* UTF32BE *)   DecUTF32BE
          } 
      } 

; PROCEDURE Encode ( Enc : Encoding ; Sink : Wr . T ; Wch : Widechar ) 
  RAISES { Alerted , Wr . Failure , Range } 
  (* Raise Range if Wch is not valid for Enc. 
     Otherwise, Encode Wch, using Enc, and write to Sink.  
  *) 

; PROCEDURE Decode ( Enc : Encoding ; Source : Rd . T ) : Widechar 
   RAISES { EndOfFile , Rd . Failure , Alerted } 
  (* Read and decode a code point from Source, using Enc, and return
     it as a WIDECHAR.  Return any ill-formed input as the Unicode "replacement
     character", VAL(16_FFFD,WIDECHAR) 
  *)  

; PROCEDURE EncNull ( Sink : Wr . T ; Wch : Widechar ) 
  RAISES { Alerted , Wr . Failure , Range } 
  (* A NOOP.  Placeholder for Encoding.Null. *) 

; PROCEDURE DecNull ( Source : Rd . T ) : Widechar
  RAISES { EndOfFile , Rd . Failure , Alerted } 
  (* A NOOP.  Placeholder for Encoding.Null. *) 

; PROCEDURE EncInternal ( Sink : Wr . T ; Wch : Widechar ) 
  RAISES { Alerted , Wr . Failure , Range } 
  (* A NOOP. Placeholder for Encoding.Internal, which is used for
     in-memory representations normally hidden by abstractions. 
  *) 

; PROCEDURE DecInternal ( Source : Rd . T ) : Widechar
   RAISES { EndOfFile , Rd . Failure , Alerted } 
  (* A NOOP. Placeholder for Encoding.Internal, which is used for
     in-memory representations normally hidden by abstractions. 
  *) 

; PROCEDURE EncISO8859_1 ( Sink : Wr . T ; Wch : Widechar ) 
  RAISES { Alerted , Wr . Failure , Range } 
  (* Raise Range if ORD(Wch) > 16_FF.  Otherwise, write Wch to Sink, 
     in ISO 8859-1 encoding.  This encoding is just byte identity.   
     It is the same as original Modula-3 PutChar. 
  *) 

; PROCEDURE DecISO8859_1 ( Source : Rd . T ) : Widechar 
  RAISES { EndOfFile , Rd . Failure , Alerted } 
  (* Read and decode a code point from Source, using ISO 8859-1 encoding, 
     and return it as a WIDECHAR.  This encoding is the same as original 
     Modula-3 GetChar and is an identity on a single byte.  ORD(<result>)
     will be <= 16_FF.  No ill-formed cases exist. 
  *)  

; PROCEDURE EncCM3WC ( Sink : Wr . T ; Wch : Widechar )
  RAISES { Alerted , Wr . Failure , Range } 
  (* Raise Range if ORD(Wch) > 16_FFFF.  Otherwise, Encode Wch, using
     CM3WC encoding, and write to Sink.  This is just 2-byte fixed-size 
     little endian, the same as the original Cm3 PutWideChar. 
  *)   

; PROCEDURE DecCM3WC ( Source : Rd . T ) : Widechar 
  RAISES { EndOfFile , Rd . Failure , Alerted } 
  (* Read and decode a code point from Source, using CM3WC encoding, and return
     it as a WIDECHAR.  This is just 2-byte fixed-size little endian, 
     the same as the original Cm3 GetWideChar.  ORD(<result>) will 
     be <= 16_FFFF.  Just zero-extended an odd byte before EOF.  
     No ill-formed cases exist. 
  *)  

; CONST EncUCS2 : EncProc = EncTable [ IsBE , Encoding . UCS2 ] 
  (* PROCEDURE EncUCS2 ( Sink : Wr . T ; Wch : Widechar )
     RAISES { Alerted , Wr . Failure , Range } *)  
  (* Raise Range if ORD(Wch) > 16_FFFF or Wch is a surrogate code.  
     Otherwise, encode Wch, using UCS2 encoding, native endian, 
     and write to Sink as 2-byte fixed size.  
  *) 

; CONST DecUCS2 : DecProc = DecTable [ IsBE , Encoding . UCS2 ] 
  (* PROCEDURE DecUCS2 ( Source : Rd . T ) : Widechar 
     RAISES { EndOfFile , Rd . Failure , Alerted } *) 
  (* Read and decode a code point from Source, using Enc, and return
     it as a WIDECHAR.  ORD(<result>) will be <= 16_FFFF.  Return any 
     ill-formed input as the Unicode "replacement character", 
     VAL(16_FFFD,WIDECHAR) 
  *)  

; PROCEDURE EncUCS2LE ( Sink : Wr . T ; Wch : Widechar )
  RAISES { Alerted , Wr . Failure , Range } 
  (* Raise Range if ORD(Wch) > 16_FFFF or Wch is a surrogate code.  
     Otherwise, Encode Wch, using UCS2LE encoding, and write to Sink  
     as 2-byte fixed-size little endian. 
  *) 

; PROCEDURE DecUCS2LE ( Source : Rd . T ) : Widechar 
  RAISES { EndOfFile , Rd . Failure , Alerted } 
  (* Read and decode a code point from Source, using UCS2LE encoding, and 
     return it as a WIDECHAR.  ORD(<result>) will be <= 16_FFFF.  Return 
     any ill-formed input as the Unicode "replacement character", 
     VAL(16_FFFD,WIDECHAR) 
  *)  

; PROCEDURE EncUCS2BE ( Sink : Wr . T ; Wch : Widechar )
  RAISES { Alerted , Wr . Failure , Range } 
  (* Raise Range if ORD(Wch) > 16_FFFF or Wch is a surrogate code.  
     Otherwise, Encode Wch, using UCS2BE encoding, and write to Sink  
     as 2-byte fixed-size big endian. 
  *) 

; PROCEDURE DecUCS2BE ( Source : Rd . T ) : Widechar 
  RAISES { EndOfFile , Rd . Failure , Alerted } 
  (* Read and decode a code point from Source, using UCS2BE encoding, and 
     return it as a WIDECHAR.  ORD(<result>) will be <= 16_FFFF.  Return 
     any ill-formed input as the Unicode "replacement character", 
     VAL(16_FFFD,WIDECHAR) 
  *)  

; PROCEDURE EncUTF8 ( Sink : Wr . T ; Wch : Widechar ) 
 RAISES { Alerted , Wr . Failure , Range } 
  (* Raise Range if Wch is a surrogate code point.  Otherwise, encode Wch, 
     using UTF8, and write to Sink.  
  *) 

; PROCEDURE DecUTF8 ( Source : Rd . T ) : Widechar 
  RAISES { EndOfFile , Rd . Failure , Alerted } 
  (* Read and decode a code point from Source, using UTF8 encoding, and return
     it as a WIDECHAR.  Return any ill-formed input as the Unicode "replacement
     character", VAL(16_FFFD,WIDECHAR) 
  *)  

; CONST EncUTF16 : EncProc = EncTable [ IsBE , Encoding . UTF16 ] 
  (* PROCEDURE EncUTF16 ( Sink : Wr . T ; Wch : Widechar ) 
     RAISES { Alerted , Wr . Failure , Range } *) 
  (* Raise Range if Wch is a surrogate code point.  Otherwise, encode Wch, 
     using UTF16 encoding, native endian, and write to Sink.  
  *) 

; CONST DecUTF16 : DecProc = DecTable [ IsBE , Encoding . UTF16 ] 
  (* PROCEDURE DecUTF16 ( Source : Rd . T ) : Widechar 
     RAISES { EndOfFile , Rd . Failure , Alerted } *) 
  (* Read and decode a code point from Source, using UTF16, native endian, 
     and return it as a WIDECHAR.  Return any ill-formed input as the Unicode 
     "replacement character", VAL(16_FFFD,WIDECHAR) 
  *)  

; PROCEDURE EncUTF16LE ( Sink : Wr . T ; Wch : Widechar ) 
  RAISES { Alerted , Wr . Failure , Range } 
  (* Raise Range if Wch is a surrogate code point.  Otherwise, encode Wch, 
     using UTF16LE encoding, and write to Sink.  
  *) 

; PROCEDURE DecUTF16LE ( Source : Rd . T ) : Widechar 
  RAISES { EndOfFile , Rd . Failure , Alerted } 
  (* Read and decode a code point from Source, using UTF16LE, and return
     it as a WIDECHAR.  Return any ill-formed input as the Unicode "replacement
     character", VAL(16_FFFD,WIDECHAR) 
  *)  

; PROCEDURE EncUTF16BE ( Sink : Wr . T ; Wch : Widechar ) 
  RAISES { Alerted , Wr . Failure , Range } 
  (* Raise Range if Wch is a surrogate code point.  Otherwise, encode Wch, 
     using UTF16BE encoding, and write to Sink.  
  *) 

; PROCEDURE DecUTF16BE ( Source : Rd . T ) : Widechar 
  RAISES { EndOfFile , Rd . Failure , Alerted } 
  (* Read and decode a code point from Source, using UTF16BE, and return
     it as a WIDECHAR.  Return any ill-formed input as the Unicode "replacement
     character", VAL(16_FFFD,WIDECHAR) 
  *)  

; CONST EncUTF32 : EncProc = EncTable [ IsBE , Encoding . UTF32 ] 
  (* PROCEDURE EncUTF32 ( Sink : Wr . T ; Wch : Widechar ) 
     RAISES { Alerted , Wr . Failure , Range } *) 
  (* Raise Range if Wch is a surrogate code point.  Otherwise, encode Wch, 
     using UTF32 encoding, native endian, and write to Sink.  
  *) 

; CONST DecUTF32 : DecProc = DecTable [ IsBE , Encoding . UTF32 ] 
  (* PROCEDURE DecUTF32 ( Source : Rd . T ) : Widechar 
     RAISES { EndOfFile , Rd . Failure , Alerted } *) 
  (* Read and decode a code point from Source, using UTF32, native endian, 
     and return it as a WIDECHAR.  Return any ill-formed input as the Unicode 
     "replacement character", VAL(16_FFFD,WIDECHAR) 
  *)  

; PROCEDURE EncUTF32LE ( Sink : Wr . T ; Wch : Widechar ) 
  RAISES { Alerted , Wr . Failure , Range } 
  (* Raise Range if Wch is a surrogate code point.  Otherwise, encode Wch, 
     using UTF32LE encoding, and write to Sink.  
  *) 

; PROCEDURE DecUTF32LE ( Source : Rd . T ) : Widechar 
  RAISES { EndOfFile , Rd . Failure , Alerted } 
  (* Read and decode a code point from Source, using UTF32LE encoding, 
     and return it as a WIDECHAR.  Return any ill-formed input as the Unicode 
     "replacement character", VAL(16_FFFD,WIDECHAR) 
  *)  

; PROCEDURE EncUTF32BE ( Sink : Wr . T ; Wch : Widechar ) 
  RAISES { Alerted , Wr . Failure , Range } 
  (* Raise Range if Wch is a surrogate code point.  Otherwise, encode Wch, 
     using UTF32BE encoding, and write to Sink.  
  *) 

; PROCEDURE DecUTF32BE ( Source : Rd . T ) : Widechar 
  RAISES { EndOfFile , Rd . Failure , Alerted } 
  (* Read and decode a code point from Source, using UTF32BE encoding, 
     and return it as a WIDECHAR.  Return any ill-formed input as the Unicode 
     "replacement character", VAL(16_FFFD,WIDECHAR) 
  *)  

; PROCEDURE MaxBytesPerChar ( Enc : Encoding ) : CARDINAL 
  (* The maximum number of bytes to represent any character, in Enc. *) 

; PROCEDURE MinChars ( Enc : Encoding ; Bytes : CARDINAL ) : CARDINAL 
  (* The Minimum number of WIDECHARs encodable in Bytes bytes, using Enc. *) 

; END UniCodec 
. 
