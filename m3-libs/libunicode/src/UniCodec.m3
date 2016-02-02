(* Copyright (C) Rodney M. Bates 2016. *)
(* rodney.m.bates@acm.org *) 
(* Licensed under the MIT License. *) 

MODULE UniCodec 

(* Coding and decoding for the unicode encoding schemes, as well as 
   a couple of others.
*) 

; IMPORT Rd 
; FROM Rd IMPORT EndOfFile  
; IMPORT RdClass 
; FROM Thread IMPORT Alerted 
; IMPORT UnsafeUniCodec 
; IMPORT UniEncoding 
; FROM UniEncoding IMPORT Encoding 
; IMPORT Wr 
; IMPORT WrClass 

; REVEAL WrClass . Private <: MUTEX 
; REVEAL RdClass . Private <: MUTEX 

(* Unicode replacement code point, for ill-formed encoded values. *) 
; CONST ReplacementWt = 16_FFFD (* As a Word.T. *) 
; CONST ReplacementWch = VAL ( ReplacementWt , Widechar ) (* As a WIDECHAR. *)  
(* TODO^ These are duplicated here and in UnsafeUniCodec.m3.  We really
   need the bigger WIDECHAR to do this properly.  Maybe leave as-is, to
   preserve long-term bootstrapability. *) 

; PROCEDURE Encode ( Enc : Encoding ; Sink : Wr . T ; Wch : Widechar ) 
  RAISES { Alerted , Wr . Failure , Range } 
  (* Raise Range if Wch is not valid for Enc. 
     Otherwise, Encode Wch, using Enc, and write to Sink.  
  *) 

  = BEGIN 
    (* Dispatch to appropriate encoding procedure. *) 
      LOCK Sink 
      DO UnsafeUniCodec . EncTable [ IsBE , Enc ] ( Sink , Wch ) 
      END (* LOCK *) 
    END Encode 

; PROCEDURE Decode ( Enc : Encoding ; Source : Rd . T ) : Widechar 
   RAISES { EndOfFile , Rd . Failure , Alerted } 
  (* Read and decode a code point from Source, using Enc, and return
     it as a WIDECHAR.  Return any ill-formed input as the Unicode "replacement
     character", VAL(16_FFFD,WIDECHAR) 
  *)  

  = BEGIN 
    (* Dispatch to appropriate decoding procedure. *) 
      LOCK Source 
      DO RETURN UnsafeUniCodec . DecTable [ IsBE , Enc ] ( Source ) 
      END (* LOCK *) 
    END Decode 

; PROCEDURE EncNull ( Sink : Wr . T ; Wch : Widechar ) 
  RAISES { Alerted , Wr . Failure , Range } 
  (* A NOOP.  Placeholder for Encoding.Null. *) 

  = BEGIN (* Do nothing. *) 
    END EncNull

; PROCEDURE DecNull ( Source : Rd . T ) : Widechar
  RAISES { EndOfFile , Rd . Failure , Alerted } 
  (* A NOOP.  Placeholder for Encoding.Null. *) 

  = BEGIN (* DecNull *) 
      RETURN ReplacementWch
    END DecNull

; PROCEDURE EncInternal ( Sink : Wr . T ; Wch : Widechar ) 
  RAISES { Alerted , Wr . Failure , Range } 
  (* A NOOP. Placeholder for Encoding.Internal, which is used for
     in-memory representations normally hidden by abstractions. 
  *) 

  = BEGIN 
    END EncInternal

; PROCEDURE DecInternal ( Source : Rd . T ) : Widechar
   RAISES { EndOfFile , Rd . Failure , Alerted } 
  (* A NOOP. Placeholder for Encoding.Internal, which is used for
     in-memory representations normally hidden by abstractions. 
  *) 

  = BEGIN (* DecInternal *) 
      RETURN ReplacementWch
    END DecInternal

; PROCEDURE EncISO8859_1 ( Sink : Wr . T ; Wch : Widechar ) 
  RAISES { Alerted , Wr . Failure , Range } 
  (* Raise Range if ORD(Wch) > 16_FF.  Otherwise, write Wch to Sink, 
     in ISO 8859-1 encoding.  This encoding is just byte identity.   
     It is the same as original Modula-3 PutChar. 
  *) 

  = BEGIN 
      LOCK Sink 
      DO UnsafeUniCodec . FastEncISO8859_1 ( Sink , Wch ) 
      END (* LOCK *) 
    END EncISO8859_1

; PROCEDURE DecISO8859_1 ( Source : Rd . T ) : Widechar 
  RAISES { EndOfFile , Rd . Failure , Alerted } 
  (* Read and decode a code point from Source, using ISO 8859-1 encoding, 
     and return it as a WIDECHAR.  This encoding is the same as original 
     Modula-3 GetChar and is an identity on a single byte.  ORD(<result>)
     will be <= 16_FF.  No ill-formed cases exist. 
  *)  

  = BEGIN (* DecISO8859_1 *) 
      LOCK Source 
      DO RETURN UnsafeUniCodec . FastDecISO8859_1 ( Source )
      END (* LOCK *) 
    END DecISO8859_1

; PROCEDURE EncCM3WC ( Sink : Wr . T ; Wch : Widechar )
  RAISES { Alerted , Wr . Failure , Range } 
  (* Raise Range if ORD(Wch) > 16_FFFF.  Otherwise, Encode Wch, using
     CM3WC encoding, and write to Sink.  This is just 2-byte fixed-size 
     little endian, the same as the original Cm3 PutWideChar. 
  *)   

  = BEGIN 
      LOCK Sink 
      DO UnsafeUniCodec . FastEncCM3WC ( Sink , Wch ) 
      END (* LOCK *) 
    END EncCM3WC

; PROCEDURE DecCM3WC ( Source : Rd . T ) : Widechar 
  RAISES { EndOfFile , Rd . Failure , Alerted } 
  (* Read and decode a code point from Source, using CM3WC encoding, and return
     it as a WIDECHAR.  This is just 2-byte fixed-size little endian, 
     the same as the original Cm3 GetWideChar.  ORD(<result>) will 
     be <= 16_FFFF.  Just zero-extend an odd byte before EOF.  
     No ill-formed cases exist. 
  *)  

  = BEGIN (* DecCM3WC *) 
      LOCK Source 
      DO RETURN UnsafeUniCodec . FastDecCM3WC ( Source ) 
      END (* LOCK *) 
    END DecCM3WC

; PROCEDURE EncUCS2LE ( Sink : Wr . T ; Wch : Widechar )
  RAISES { Alerted , Wr . Failure , Range } 
  (* Raise Range if ORD(Wch) > 16_FFFF or Wch is a surrogate code.  
     Otherwise, Encode Wch, using UCS2LE encoding, and write to Sink  
     as 2-byte fixed-size little endian. 
  *) 

  = BEGIN 
      LOCK Sink 
      DO UnsafeUniCodec . FastEncUCS2LE ( Sink , Wch ) 
      END (* LOCK *) 
    END EncUCS2LE

; PROCEDURE DecUCS2LE ( Source : Rd . T ) : Widechar 
  RAISES { EndOfFile , Rd . Failure , Alerted } 
  (* Read and decode a code point from Source, using UCS2LE encoding, and 
     return it as a WIDECHAR.  ORD(<result>) will be <= 16_FFFF.  Return 
     any ill-formed input as the Unicode "replacement character", 
     VAL(16_FFFD,WIDECHAR) 
  *)  

  = BEGIN (* DecUCS2LE *) 
      LOCK Source 
      DO RETURN UnsafeUniCodec . FastDecUCS2LE ( Source ) 
      END (* LOCK *) 
    END DecUCS2LE

; PROCEDURE EncUCS2BE ( Sink : Wr . T ; Wch : Widechar )
  RAISES { Alerted , Wr . Failure , Range } 
  (* Raise Range if ORD(Wch) > 16_FFFF or Wch is a surrogate code.  
     Otherwise, Encode Wch, using UCS2BE encoding, and write to Sink  
     as 2-byte fixed-size big endian. 
  *) 

  = BEGIN 
      LOCK Sink 
      DO UnsafeUniCodec . FastEncUCS2BE ( Sink , Wch ) 
      END (* LOCK *) 
    END EncUCS2BE

; PROCEDURE DecUCS2BE ( Source : Rd . T ) : Widechar 
  RAISES { EndOfFile , Rd . Failure , Alerted } 
  (* Read and decode a code point from Source, using UCS2BE encoding, and 
     return it as a WIDECHAR.  ORD(<result>) will be <= 16_FFFF.  Return 
     any ill-formed input as the Unicode "replacement character", 
     VAL(16_FFFD,WIDECHAR) 
  *)  

  = BEGIN (* DecUCS2BE *) 
      LOCK Source 
      DO RETURN UnsafeUniCodec . FastDecUCS2BE ( Source ) 
      END (* LOCK *) 
    END DecUCS2BE

; PROCEDURE EncUTF8 ( Sink : Wr . T ; Wch : Widechar ) 
  RAISES { Alerted , Wr . Failure , Range } 
  (* Raise Range if Wch is a surrogate code point.  Otherwise, encode Wch, 
     using UTF8, and write to Sink.  
  *) 

  = BEGIN 
      LOCK Sink 
      DO UnsafeUniCodec . FastEncUTF8 ( Sink , Wch ) 
      END (* LOCK *) 
    END EncUTF8

; PROCEDURE DecUTF8 ( Source : Rd . T ) : Widechar 
  RAISES { EndOfFile , Rd . Failure , Alerted } 
  (* Read and decode a code point from Source, using UTF8 encoding, and return
     it as a WIDECHAR.  Return any ill-formed input as the Unicode "replacement
     character", VAL(16_FFFD,WIDECHAR) 
  *)  

  = BEGIN (* DecUTF8 *) 
      LOCK Source 
      DO RETURN UnsafeUniCodec . FastDecUTF8 ( Source ) 
      END (* LOCK *) 
    END DecUTF8

; PROCEDURE EncUTF16LE ( Sink : Wr . T ; Wch : Widechar ) 
  RAISES { Alerted , Wr . Failure , Range } 
  (* Raise Range if Wch is a surrogate code point.  Otherwise, encode Wch, 
     using UTF16LE encoding, and write to Sink.  
  *) 

  = BEGIN 
      LOCK Sink 
      DO UnsafeUniCodec . FastEncUTF16LE ( Sink , Wch ) 
      END (* LOCK *) 
    END EncUTF16LE

; PROCEDURE DecUTF16LE ( Source : Rd . T ) : Widechar 
  RAISES { EndOfFile , Rd . Failure , Alerted } 
  (* Read and decode a code point from Source, using UTF16LE, and return
     it as a WIDECHAR.  Return any ill-formed input as the Unicode "replacement
     character", VAL(16_FFFD,WIDECHAR) 
  *)  

  = BEGIN (* DecUTF16LE *) 
      LOCK Source 
      DO RETURN UnsafeUniCodec . FastDecUTF16LE ( Source ) 
      END (* LOCK *) 
    END DecUTF16LE

; PROCEDURE EncUTF16BE ( Sink : Wr . T ; Wch : Widechar ) 
  RAISES { Alerted , Wr . Failure , Range } 
  (* Raise Range if Wch is a surrogate code point.  Otherwise, encode Wch, 
     using UTF16BE encoding, and write to Sink.  
  *) 

  = BEGIN 
      LOCK Sink 
      DO UnsafeUniCodec . FastEncUTF16BE ( Sink , Wch ) 
      END (* LOCK *) 
    END EncUTF16BE

; PROCEDURE DecUTF16BE ( Source : Rd . T ) : Widechar 
  RAISES { EndOfFile , Rd . Failure , Alerted } 
  (* Read and decode a code point from Source, using UTF16BE, and return
     it as a WIDECHAR.  Return any ill-formed input as the Unicode "replacement
     character", VAL(16_FFFD,WIDECHAR) 
  *)  

  = BEGIN (* DecUTF16BE *) 
      LOCK Source 
      DO RETURN UnsafeUniCodec . FastDecUTF16BE ( Source ) 
      END (* LOCK *) 
    END DecUTF16BE

; PROCEDURE EncUTF32LE ( Sink : Wr . T ; Wch : Widechar ) 
  RAISES { Alerted , Wr . Failure , Range } 
  (* Raise Range if Wch is a surrogate code point.  Otherwise, encode Wch, 
     using UTF32LE encoding, and write to Sink.  
  *) 

  = BEGIN
      LOCK Sink 
      DO UnsafeUniCodec . FastEncUTF32LE ( Sink , Wch ) 
      END (* LOCK *) 
    END EncUTF32LE

; PROCEDURE DecUTF32LE ( Source : Rd . T ) : Widechar 
  RAISES { EndOfFile , Rd . Failure , Alerted } 
  (* Read and decode a code point from Source, using UTF32LE encoding, 
     and return it as a WIDECHAR.  Return any ill-formed input as the Unicode 
     "replacement character", VAL(16_FFFD,WIDECHAR) 
  *)  

  = BEGIN (* DecUTF32LE *) 
      LOCK Source 
      DO RETURN UnsafeUniCodec . FastDecUTF32LE ( Source ) 
      END (* LOCK *) 
    END DecUTF32LE

; PROCEDURE EncUTF32BE ( Sink : Wr . T ; Wch : Widechar ) 
  RAISES { Alerted , Wr . Failure , Range } 
  (* Raise Range if Wch is a surrogate code point.  Otherwise, encode Wch, 
     using UTF32BE encoding, and write to Sink.  
  *) 

  = BEGIN 
      LOCK Sink 
      DO UnsafeUniCodec . FastEncUTF32BE ( Sink , Wch ) 
      END (* LOCK *) 
    END EncUTF32BE

; PROCEDURE DecUTF32BE ( Source : Rd . T ) : Widechar 
  RAISES { EndOfFile , Rd . Failure , Alerted } 
  (* Read and decode a code point from Source, using UTF32BE encoding, 
     and return it as a WIDECHAR.  Return any ill-formed input as the Unicode 
     "replacement character", VAL(16_FFFD,WIDECHAR) 
  *)  

  = BEGIN (* DecUTF32BE *) 
      LOCK Source 
      DO RETURN UnsafeUniCodec . FastDecUTF32BE ( Source ) 
      END (* LOCK *) 
    END DecUTF32BE

; PROCEDURE MaxBytesPerChar ( Enc : Encoding ) : CARDINAL 
  (* The maximum number of bytes to represent any character, in Enc. *) 

  = BEGIN 
      CASE Enc 
      OF Encoding . Null 
      => RETURN 0 
      | Encoding . ISO8859_1 => RETURN 1 
      | Encoding . CM3WC 
      , Encoding . UCS2 
      , Encoding . UCS2LE 
      , Encoding . UCS2BE 
      => RETURN 2 
      | Encoding . UTF8 
      , Encoding . UTF16 
      , Encoding . UTF16LE 
      , Encoding . UTF16BE 
      , Encoding . UTF32 
      , Encoding . UTF32LE 
      , Encoding . UTF32BE 
      , Encoding . Internal 
      => RETURN 4 
      END (* CASE *) 
    END MaxBytesPerChar 

; PROCEDURE MinChars ( Enc : Encoding ; Bytes : CARDINAL ) : CARDINAL 
  (* The Minimum number of WIDECHARs encodable in Bytes bytes, using Enc. *) 

  = BEGIN 
      CASE Enc 
      OF Encoding . Null 
      => RETURN 0 
      | Encoding . ISO8859_1 => RETURN Bytes 
      | Encoding . CM3WC 
      , Encoding . UCS2 
      , Encoding . UCS2LE 
      , Encoding . UCS2BE 
      => RETURN Bytes DIV 2 
      | Encoding . UTF8 
      , Encoding . UTF16 
      , Encoding . UTF16LE 
      , Encoding . UTF16BE 
      , Encoding . UTF32 
      , Encoding . UTF32LE 
      , Encoding . UTF32BE 
      , Encoding . Internal 
      => IF ORD ( LAST ( WIDECHAR ) ) = 16_FFFF 
         THEN RETURN Bytes DIV 2 
         ELSE RETURN Bytes DIV 4 
         END (* IF *) 
      END (* CASE *) 
    END MinChars 

; BEGIN (* UniCodec *)
  END UniCodec 
. 
