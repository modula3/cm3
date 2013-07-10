INTERFACE UniEncoding

; TYPE Encoding 
    = { Null      (* Use for unknown or irrelevant. *) 
      , Internal  (* A pseudo-encoding for TEXT, which has its own hidden 
                     representation. *) 
      , ISO8859_1 (* ISO8859-1. The original Modula-3 encoding for CHAR. 
                     Exactly one byte for every character value.  Byte-for-byte
                     identity.  Can only handle values in CHAR *)
      , CM3WC     (* Exactly two bytes for every character value.  The old 
                     16-bit WIDECHAR of Critical Mass Modula-3.  Can only handle
                     values up to VAL(16_FFFF,WIDECHAR).  Always little endian.
                     Unicode surrogate codes receive no special treatment. *) 
      , UCS2      (* Exactly two bytes for every character value.  Can only 
                     handle values up to VAL(16_FFFF,WIDECHAR).  Surrogate code
                     points 16_D800..16_DFFF are not allowed. Endianness is
                     unspecified. *) 
      , UCS2LE    (* UCS2, little endian. *) 
      , UCS2BE    (* UCS2, big endian. *) 
      , UTF8      (* Unicode UTF8.  Variable-length.  Can handle all of 
                     WIDECHAR.  Endianness is irrelevant. *) 
      , UTF16     (* Unicode UTF16.  Variable-length.  Can handle all of 
                     WIDECHAR.  Endianness is unspecified. *) 
      , UTF16LE   (* UTF16, little endian. *) 
      , UTF16BE   (* UTF16, big endian. *) 
      , UTF32     (* Unicode UTF32.  Exactly four bytes for every character.  
                     Can handle all of WIDECHAR. Endianness is unspecified. *) 
      , UTF32LE   (* UTF32, little endian. *) 
      , UTF32BE   (* UTF32, big endian. *) 
      } 

; PROCEDURE EncImage ( Enc : Encoding ) : TEXT 

; TYPE EncodingSet = SET OF Encoding 

; CONST EndianAmbiguous 
    = EncodingSet { Encoding . UCS2 , Encoding . UTF16 , Encoding . UTF32 } 

; END UniEncoding 
. 
