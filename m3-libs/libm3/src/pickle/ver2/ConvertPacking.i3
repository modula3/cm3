(* 
 * For information about this program, contact Blair MacIntyre            
 * (bm@cs.columbia.edu) or Steven Feiner (feiner@cs.columbia.edu)         
 * at the Computer Science Dept., Columbia University,                    
 * 1214 Amsterdam Ave. Mailstop 0401, New York, NY, 10027.                
 *                                                                       
 * Copyright (C) 1995, 1996 by The Trustees of Columbia University in the
 * City of New York.  Blair MacIntyre, Computer Science Department.
 *
 *)

(* "ConvertPacking" is a module that provides assistance in converting
   a data value from one representation to another, where the
   representations are represented by "RTPacking.T"s.

   When an instance of "ConvertPacking.T" is initialized, an internal
   "program" is created describing how to convert an instance of type
   "typecode" from memory format "from" to memory format "to".

   When a conversion is desired, "convert" should be called.  The
   first parameter to convert is the address of the data area to be
   filled in.  It should be an address of the sort returned by  
   "RTHeap.GetDataAdr(ref)".   The remaining two parameters are used
   during the conversion process.  The first is called to acquire data
   from the source.  The second is used to fill in references:  it is
   up to the caller to provide a suitable reference each time "getRef"
   is called.

*)


UNSAFE INTERFACE ConvertPacking;

IMPORT RTPacking, PklAction, Thread, Rd, Wr;

EXCEPTION Error(TEXT);

CONST Brand = "ConvertPacking 1.0";

TYPE
  T <: Public;

  Public = OBJECT METHODS
    init(typecode: INTEGER; from: RTPacking.T; local: RTPacking.T; 
         VAR (*OUT*) nDim, fromEltPack, toEltPack: INTEGER): T RAISES {Error};
    getDim(VAR (*OUT*) nDim, fromEltPack, toEltPack: INTEGER);
    convertRead(dest: ADDRESS; v: ReadVisitor; 
                progRepCt: INTEGER): ADDRESS RAISES 
        {Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted};
    write(src: ADDRESS; v: WriteVisitor; progRepCt: INTEGER): ADDRESS RAISES 
        {Error, Wr.Failure, Thread.Alerted};
    print();
    printProgram();
  END;

(* "convertRead()" will use the visitor "v" to fill in the data area
   pointed to by "dest".  "dest" should point to the beginning of the
   data address of a traced reference type with typecode "typecode".
   If "typecode" is an Open Array type, then the resulting converter
   will be set up to convert one instance of the open array element.
   Furthermore, "init" and "getDim" will return the number of open
   dimensions and element packing (bit size of an element, including
   any alignment packing) of the open array.  It is up to the
   caller to call "convert" the appropriate number of times with the
   correct dest address each time.

   "convertRead" will call "v.readData" each time it needs some data from
   the conversion source, "v.skipData" to skip a number of data bytes
   from the source and "v.readRef" to obtain a reference.  It returns
   the next address after the block of data filled in by the
   conversion. 

   "write" will write out the contents of the object pointed to by src,
   in the analogous manner to "convertRead", without doing any conversions.  
   It assumes the local data format is the "local" packing passed to "init".

*)

TYPE ReadVisitor <: RVPublic;
     RVPublic = OBJECT METHODS 
       readData(VAR data: ARRAY OF CHAR) RAISES
        {Rd.EndOfFile, Rd.Failure, Thread.Alerted};
       skipData(length: INTEGER) RAISES
        {Rd.EndOfFile, Rd.Failure, Thread.Alerted};
       readRef(type: RefType): REFANY RAISES
        {Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted};
       readChar(): CHAR  
        RAISES { Rd.EndOfFile, Rd.Failure, Thread.Alerted };
       readWC21(): UInt32
        RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted}; 
     END;

(* When "v.readData" is called, the array "data" should be filled in with
   bytes from the conversion source.  When "v.skipData" is called,
   "length" data bytes in the conversion source should be skipped
   over. *)

TYPE WriteVisitor <: WVPublic;
     WVPublic = OBJECT METHODS 
       writeData(VAR data: ARRAY OF CHAR) RAISES
        {Wr.Failure, Thread.Alerted};
       skipData(length: INTEGER) RAISES
        {Wr.Failure, Thread.Alerted};
       writeRef(type: RefType; ref: REFANY) RAISES
        {Error, Wr.Failure, Thread.Alerted};
       writeChar(value: CHAR)  
        RAISES { Wr.Failure, Thread.Alerted };
       writeWC21(intVal: UInt32) 
        RAISES { Wr.Failure, Thread.Alerted };
     END;

(* When "v.writeData" is called, the array "data" should be writen
   output destination.  When "v.skipData" is called,
   "length" data bytes in the destination should be skipped
   over. *)

TYPE
  RefType = PklAction.RefType;  (* => {Ref, UntracedRef, Proc} *)

(* "v.readRef" or "v.writeRef" is called each time the "convertRead"
   or "write" methods (respectively) hit a reference data entry that
   needs to be dealt with.  Because of the way Pickling works, the
   single "RefType" parameter is sufficient for the unpickling code to
   obtain the required reference. *)

PROCEDURE New(typecode: INTEGER; from: RTPacking.T; local: RTPacking.T; 
              VAR (*OUT*) nDim, fromEltPack, toEltPack: INTEGER): T 
  RAISES {Error};
(* Same as NEW(T).init(typecode, from, to, nDim, fromEltPack, toEltPack); *)

(* These are the data conversions we currently support. *)
TYPE
  CPKind = {Copy, Swap, Copy32to64, Copy64to32, Swap32to64, Swap64to32,
            Copy16to32, Copy32to16, Swap16to32, Swap32to16
           };

PROCEDURE GetWordKind(from: RTPacking.T; local: RTPacking.T): CPKind;
(* The result is good for all ordinal types except LONGINT. *) 

PROCEDURE GetLongintKind(from: RTPacking.T; local: RTPacking.T): CPKind;
(* The result is good only for LONGINT. *) 

PROCEDURE GetWidecharKind(from: RTPacking.T; local: RTPacking.T): CPKind;
(* The result is good only for WIDECHAR. *) 

TYPE UInt32 = BITS 32 FOR [0 .. 16_7FFFFFFF];

(* WC21 is a variable-length encoding of widechar values, used only in
   pickles.  The standard Unicode encodings explicitly disallow surrogate
   code points as unencoded values.  Programs may have a legitimate need to
   store and/or manipulate surrogate values in memory, and these should be
   picklable too.  WC21 supports the entire code point range, which requires
   21 bits.

   The first byte has 7 (least significant) data bits and one bit (msb of the 
   byte) that, if set, indicates another byte follows.  If present, the second 
   byte is just like the first and supplies the next more significant 7 data 
   bits.  If it calls for a third byte, that contains the 7 most significant 
   data bits.  The bytes are always in least- to most-significant order in the 
   pickle, regardless of endianness of writing or reading machine.  
*) 

PROCEDURE WriteWC21(wr: Wr.T; intVal: UInt32)
  RAISES {Wr.Failure, Thread.Alerted}; 

PROCEDURE ReadWC21(rd: Rd.T): UInt32 
RAISES{Rd.EndOfFile, Rd.Failure, Thread.Alerted}; 
(* Read one WIDECHAR value in WC21 encoding and return in a 32-bit int. *) 

END ConvertPacking. 
