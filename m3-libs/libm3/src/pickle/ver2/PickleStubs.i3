(*
 * For information about this program, contact Blair MacIntyre            
 * (bm@cs.columbia.edu) or Steven Feiner (feiner@cs.columbia.edu)         
 * at the Computer Science Dept., Columbia University,                    
 * 1214 Amsterdam Ave. Mailstop 0401, New York, NY, 10027.                
 *                                                                        
 * Copyright (C) 1995, 1996 by The Trustees of Columbia University in the 
 * City of New York.  Blair MacIntyre, Computer Science Department.       
 *
 * This file is released under the same conditions as Pickle.m3. See COPYRIGHT.
 *
 *)

INTERFACE PickleStubs;

IMPORT Rd, Wr, Thread, Pickle2 AS Pickle;
FROM Swap IMPORT Int32;

CONST ReplacementWt = 16_FFFD; (* As a Word.T. *) 

TYPE
  Byte8 = BITS 8 FOR [0..255];
  UInt32 = BITS 32 FOR [0 .. 16_7FFFFFFF];


(* Programmers of Picklers may use these routines to read and write
   specific pieces of data in a portable fashion.

   Automatic conversion between the data representations is performed
   wherever possible.  If automatic conversion is impossible, a
   "Pickle.Error" exception is raised. *)

(*
\paragraph{Pickle Stub Procedures.}
\index{Pickle stubs}
*)

PROCEDURE OutRef (writer: Pickle.Writer; r: REFANY) 
  RAISES {Pickle.Error, Wr.Failure, Thread.Alerted};
(* Marshal the data structure reachable from "r".  Equivalent to 
   "wr.write(r)". *)

PROCEDURE InRef (reader: Pickle.Reader; tc := -1): REFANY
  RAISES {Pickle.Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted};
(* Unmarshal a marshaled subtype of "REFANY" as pickled by "OutRef". 
   Equivalent to "rd.read()". *)

PROCEDURE OutChars (writer: Pickle.Writer; READONLY chars: ARRAY OF CHAR)
  RAISES {Wr.Failure, Thread.Alerted};
(* Marshal a char array in native format. *)

(* WC21 is a variable-length encoding of widechar values, used only in
   pickles and net objects.  The standard Unicode encodings explicitly disallow
   surrogate code points as unencoded values.  Programs may have a legitimate 
   need to store and/or manipulate surrogate values in memory, and these should
   be picklable too.  WC21 supports the entire code point range, which requires
   21 bits.

   The first byte has 7 (least significant) data bits and one bit (msb of the 
   byte) that, if set, indicates another byte follows.  If present, the second 
   byte is just like the first and supplies the next more significant 7 data 
   bits.  If it calls for a third byte, that contains the 7 most significant 
   data bits.  The bytes are always in least- to most-significant order in the 
   pickle, regardless of endianness of writing or reading machine.  
*) 

PROCEDURE OutWC21(wr: Wr.T; Wch: WIDECHAR)
  RAISES {Wr.Failure, Thread.Alerted};
(* Marshal one wide char in surrogate-tolerant WC21 incoding. *) 

PROCEDURE OutWideChars(writer: Pickle.Writer; READONLY arr: ARRAY OF WIDECHAR)
    RAISES {Wr.Failure, Thread.Alerted};
(* Marshal a wide char array in native format. *)

PROCEDURE OutText (writer: Pickle.Writer; t: TEXT)
  RAISES {Wr.Failure, Thread.Alerted};
(* Marshal a TEXT. *)

PROCEDURE OutBytes (writer: Pickle.Writer; READONLY bytes: ARRAY OF Byte8)
  RAISES {Wr.Failure, Thread.Alerted};
(* Marshal a byte array. *)

PROCEDURE OutInteger (writer: Pickle.Writer; i: INTEGER)
  RAISES {Wr.Failure, Thread.Alerted};
(* Marshal an integer in native format. *)

PROCEDURE OutLongint (writer: Pickle.Writer; i: LONGINT)
  RAISES {Wr.Failure, Thread.Alerted};
(* Marshal an integer in native format. *)

PROCEDURE OutInt32 (writer: Pickle.Writer; i: Int32) 
  RAISES {Wr.Failure, Thread.Alerted}; 
(* Marshal a 32-bit integer in native format. *)

PROCEDURE OutByte (writer: Pickle.Writer; i: Byte8) 
  RAISES {Wr.Failure, Thread.Alerted};
(* Marshal a byte. *)

PROCEDURE OutBoolean (writer: Pickle.Writer; bool: BOOLEAN)
  RAISES {Wr.Failure, Thread.Alerted};
(* Marshal a boolean value. *)

PROCEDURE OutReal (writer: Pickle.Writer; r: REAL) 
  RAISES {Wr.Failure, Thread.Alerted};
(* Marshal a real in native format. *)

PROCEDURE OutLongreal (writer: Pickle.Writer; card: LONGREAL)
  RAISES {Wr.Failure, Thread.Alerted};
(* Marshal a longreal in native format. *)

PROCEDURE OutExtended (writer: Pickle.Writer; card: EXTENDED)
  RAISES {Wr.Failure, Thread.Alerted};
(* Marshal an extended in native format. *)

PROCEDURE OutCardinal (writer: Pickle.Writer; card: CARDINAL)
  RAISES {Wr.Failure, Thread.Alerted};
(* Marshal a cardinal in native format. *)

PROCEDURE OutLongcard (writer: Pickle.Writer; card: LONGCARD)
  RAISES {Wr.Failure, Thread.Alerted};
(* Marshal a cardinal in native format. *)

PROCEDURE InChars (reader: Pickle.Reader; VAR chars: ARRAY OF CHAR)
  RAISES {Pickle.Error, Rd.Failure, Thread.Alerted};
(* Unmarshal a char array of length "NUMBER(chars)". *)

PROCEDURE InWC21(rd: Rd.T): UInt32 
RAISES{Rd.EndOfFile, Rd.Failure, Thread.Alerted}; 
(* Unmarshal one WIDECHAR value in WC21 encoding and return in a 32-bit int,
   where caller can range check. *) 

PROCEDURE InWideChars(reader: Pickle.Reader; VAR chars: ARRAY OF WIDECHAR)
    RAISES {Pickle.Error, Rd.Failure, Thread.Alerted};
(* Unmarshal a wide char array of length "NUMBER(chars)". *)

PROCEDURE InText(reader: Pickle.Reader) : TEXT
   RAISES {Pickle.Error, Rd.Failure, Thread.Alerted};
(* Unmarshal a TEXT. *)

PROCEDURE InBytes (reader: Pickle.Reader; VAR bytes: ARRAY OF Byte8)
  RAISES {Pickle.Error, Rd.Failure, Thread.Alerted};
(* Unmarshal a byte array of length "NUMBER(bytes)". *)

PROCEDURE InInteger (reader: Pickle.Reader;
                     min            := FIRST(INTEGER);
                     max            := LAST(INTEGER)   ): INTEGER
  RAISES {Pickle.Error, Rd.Failure, Thread.Alerted};
(* Unmarshal an integer, checking that its value is in "[min..max]". *)

PROCEDURE InLongint (reader: Pickle.Reader;
                     min            := FIRST(LONGINT);
                     max            := LAST(LONGINT)   ): LONGINT
  RAISES {Pickle.Error, Rd.Failure, Thread.Alerted};
(* Unmarshal an integer, checking that its value is in "[min..max]". *)

PROCEDURE InInt32 (reader: Pickle.Reader; min := FIRST(Int32); 
                   max := LAST(Int32)): Int32
  RAISES {Pickle.Error, Rd.Failure, Thread.Alerted};
(* Unmarshal a 32-bit integer, checking that its value is in "[min..max]". *)

PROCEDURE InByte (reader: Pickle.Reader; max := LAST(Byte8)): Byte8
  RAISES {Pickle.Error, Rd.Failure, Thread.Alerted};
(* Unmarshal a byte, checking that its value is in "[0..max]". *)

PROCEDURE InBoolean (reader: Pickle.Reader): BOOLEAN
  RAISES {Pickle.Error, Rd.Failure, Thread.Alerted};
(* Unmarshal a boolean value. *)

PROCEDURE InReal (reader: Pickle.Reader): REAL
  RAISES {Pickle.Error, Rd.Failure, Thread.Alerted};
(* Unmarshal a real value. *)

PROCEDURE InLongreal (reader: Pickle.Reader): LONGREAL
  RAISES {Pickle.Error, Rd.Failure, Thread.Alerted};
(* Unmarshal a longreal value. *)

PROCEDURE InExtended (reader: Pickle.Reader): EXTENDED
  RAISES {Pickle.Error, Rd.Failure, Thread.Alerted};
(* Unmarshal an extended value. *)

PROCEDURE InCardinal (reader: Pickle.Reader; lim: CARDINAL := LAST(CARDINAL)):
  CARDINAL RAISES {Pickle.Error, Rd.Failure, Thread.Alerted};
(* Unmarshal a cardinal, checking that its value is in "[0..lim]". *)

PROCEDURE InLongcard (reader: Pickle.Reader; lim: LONGCARD := LAST(LONGCARD)):
  LONGCARD RAISES {Pickle.Error, Rd.Failure, Thread.Alerted};
(* Unmarshal a cardinal, checking that its value is in "[0..lim]". *)

END PickleStubs.

