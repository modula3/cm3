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

UNSAFE INTERFACE PklTipeMap;

IMPORT Rd, Wr, RTPacking, Thread, ConvertPacking;

EXCEPTION Error(TEXT);

TYPE
  TypeCode = INTEGER; 

PROCEDURE Read (v: ConvertPacking.ReadVisitor; r: REFANY; tc: TypeCode; 
                from: RTPacking.T; to: RTPacking.T; 
                READONLY shape: ARRAY OF INTEGER)  
    RAISES { Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted };

(* Read in "r" with type "tc" using "v".  The packing of the data in
   the file is defined by "v.from", the packing of the data in memory
   in "v.to".  "shape" is the number of dimensions of the reference, if it is
   an Open Array.  Otherwise, it is ignored.  Proper conversions are
   applied.  It is assumed that "r" has been properly allocated to
   handle the incoming data.  *)

PROCEDURE Write (v: ConvertPacking.WriteVisitor; r: REFANY; tc: TypeCode; 
                 from: RTPacking.T;  READONLY shape: ARRAY OF INTEGER;
                 n: INTEGER)  
    RAISES { Error, Wr.Failure, Thread.Alerted };

(* Write "r" using "v".  The data is writen in the local data format. *)

END PklTipeMap. 
