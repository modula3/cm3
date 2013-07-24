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

INTERFACE PklAction;

CONST Brand = "Tipe Conversion Action 1.0";

(* These are the instuctions for a specialized program that is built once for
   the fields/elements of a heap object type and interpreted possibly many 
   times, once for each instance of the type that is read or written.  
   Two different interpreters for reading (ConvertPacking.Convert) and
   writing (ConvertPacking.Write) use these same program types but move data
   in opposite directions (memory<=>stream).  Reading and writing of a 
   single type can use the same program, if the RTPacking is the same.  
   Otherwise, they use different programs.  Write programs do no conversions. 
*) 

TYPE 
  T = OBJECT kind: PAKind; unitCt: INTEGER END;

TYPE
  PAKind = {
     (* When used in an action program executed by the Write interpreter, only
        Copy, Skip, ReadRef, CopyWC21to32, Copy16to32, and CopyWC21to16 are legal.  
        All fetch from memory, which must be properly aligned for the action, 
        and write to a stream, with no alignment restrictions.  
        CopyWC21to32, CopyWC21to16, and ReadRef are named as for a read
        conversion, but the sense is reversed by the Write interpreter.
        This allows a single program to be interpreted by either interpreter, 
        whenever the from- and to- RTpacking are the same.    

        When used in an action program executed by the Convert interpreter,
        all are legal.  All read from a stream, with no alignment restrictions,
        and store in memory, which must be properly aligned for the action.
     *) 
     (* For moving data between same size, same endian *)
     Copy,                          (* Straight copy of unitCt bytes. *)

     (* Skipping bytes on input, output or both *)
     SkipFrom,                      (* Skip unitCt input bytes. *)
     SkipTo,                        (* Skip unitCt output bytes. *)
     Skip,                          (* Skip unitCt bytes of both *)

     (* For moving packed data between different endian machines.
        Only need one, since a set of packed fields being converted
        between different endian, different size must fit in the
        smaller word size (32 bits) *)
     SwapPacked,                    (* Copy and swap around the fields *)

     (* For moving data between same size, different endian *)
     Swap16,                        (* Copy unitCt 16 bit words, swapping. *)
     Swap32,                        (* Copy unitCt 32 bit words, swapping. *)
     Swap64,                        (* Copy unitCt 64 bit words, swapping. *)

     (* For moving data between different size, same endian *)
     Copy32to64,                    (* Copy unitCt 32 bit words to 64
                                       bit words. *)
     Copy64to32,                    (* Copy unitCt 64 bit words to 32
                                       bit words. *)
     Copy16to32,                    (* Copy unitCt 16 bit words to 32
                                       bit words. *)
     Copy32to16,                    (* Copy unitCt 32 bit words to 16
                                       bit words. *)
     CopyWC21to32,                  (* Copy unitCt WC21-encoded characters to 
                                       32-bit words. *) 
     CopyWC21to16,                  (* Copy unitCt WC21-encoded characters to 
                                       16-bit words. *) 

     (* For moving data between different size, different endian *)
     Swap32to64,		    (* Swap unitCt 32 bit words to 64
                                       bit words, swapping. *)
     Swap64to32,		    (* Swap unitCt 64 bit words to 32
                                       bit words, swapping. *)
     Swap16to32,                    (* Swap unitCt 16 bit words to 32
                                       bit words. *)
     Swap32to16,                    (* Swap unitCt 32 bit words to 16
                                       bit words. *)

     ReadRef,
     Done
  };


  Copy32to64 = T OBJECT signed: BOOLEAN END; (* Also used for swap. *) 
  Copy64to32 = T OBJECT signed: BOOLEAN END; (* Also used for swap. *)
(* TODO: ^These can be merged. *) 

  SwapPacked = T OBJECT 
    size  : CARDINAL; (* Size of word to pack into, in bytes. *) 
    field : REF ARRAY OF CARDINAL; (* Sizes of fields in bits. *) 
  END;

  Ref = T OBJECT refType: RefType; END;
  RefType = {Ref, UntracedRef, Proc};

END PklAction.
