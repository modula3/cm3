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
                number: INTEGER): ADDRESS RAISES 
        {Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted};
    write(src: ADDRESS; v: WriteVisitor; number: INTEGER): ADDRESS RAISES 
        {Error, Wr.Failure, Thread.Alerted};
    print();
    printProgram();
  END;

(* "convert()" will uses the visitor "v" to fill in the data area
   pointed to be "dest".  "dest" should point to the beginning of the
   data address of a traced reference type with typecode "typecode".
   If "typecode" is an Open Array type, then the resulting converter
   will be set up to convert one instance of the open array element.
   Furthermore, "init" and "getDim" will return the number of
   dimensions and element packing of the open array.  It is up to the
   caller to call "convert" the appropriate number of times with the
   correct address each time.

   "convert" will call "v.readData" each time it needs some data from
   the conversion source, "v.skipData" to skip a number of data bytes
   from the source and "v.readRef" to obtain a reference.  It returns
   the next address after the block of data filled in by the
   conversion. 

   "write" will write out a reference in the analogous manner to
   "convert", without doing any conversions.  It assumes the local
   data format is the "local" packing passed to "init".

*)

TYPE ReadVisitor <: RVPublic;
     RVPublic = OBJECT METHODS 
       readData(VAR data: ARRAY OF CHAR) RAISES
        {Rd.EndOfFile, Rd.Failure, Thread.Alerted};
       skipData(length: INTEGER) RAISES
        {Rd.EndOfFile, Rd.Failure, Thread.Alerted};
       readRef(type: RefType): REFANY RAISES
        {Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted};
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
  CPKind = {Copy, Swap, Copy32to64, Copy64to32, Swap32to64, Swap64to32};

PROCEDURE GetWordKind(from: RTPacking.T; local: RTPacking.T): CPKind;
(* The result is good for all ordinal types except LONGINT. *) 

PROCEDURE GetLongintKind(from: RTPacking.T; local: RTPacking.T): CPKind;
(* The result is good only for LONGINT. *) 

END ConvertPacking. 
