(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Tue May  4 10:58:30 PDT 1993 by swart    *)

INTERFACE Swap;

(* An interface useful for writing code that has to explicitly deal
   with byte ordering and/or word length issues. *)

TYPE Endian = {Big, Little};

VAR endian: Endian;
  (* This variable is set at initialization.  If the endian is not
     one of Big or Little, initialization will generate a checked runtime
     error. *)

CONST
  FirstInt32 = -1 - 16_7FFFFFFF;
    (* This value is computed in such a way that it will have the same value
       on a 32 bit machine and on a 64 bit machine.  On a 32 bit
       2's complement machine this is FIRST(INTEGER). *)

TYPE
  Int32 = BITS 32 FOR [FirstInt32 .. 16_7FFFFFFF];
  (* The subrange of integer that can be represented in an INTEGER
     on a 32 bit 2's complement machine.  *)

PROCEDURE Swap4 (i: Int32): Int32;
  (* Swaps the bottom four bytes of the argument leaving any remaining
     bytes, which should just hold the sign bits, as they were. *)

TYPE
  Int16 = BITS 16 FOR [-16_8000 .. 16_7FFF];

PROCEDURE Swap2 (i: Int16): Int16;
  (* Swaps the bottom two bytes of the argument leaving any remaining
     bytes, which should just hold the sign bits, as they were. *)

TYPE
  UInt16 = BITS 16 FOR [0 .. 16_FFFF];

PROCEDURE Swap2U (i: UInt16): UInt16;
  (* Swaps the bottom two bytes of the argument setting the remaining
     bytes to zero. *)

TYPE
  Int64On32 = RECORD a, b: Int32;  END;

  Int64Pad =
    BITS 64 - BITSIZE(INTEGER) FOR SET OF [0 .. 63 - BITSIZE(INTEGER)];

  Int64On64 = RECORD
                v  : INTEGER;
                pad: Int64Pad  := Int64Pad{};
              END;

(* These are two 64 bit types that are useful when writing code that deals
   with 64 bit integers and checks at runtime which type to use.  Since
   each type is always 64 bits long the compiler will not complain about
   LOOPHOLEs in the piece of the code that is not being executed. 

   Note a compiler bug precludes using record constructors for
   Int64On64. *)

PROCEDURE Swap8(READONLY i: Int64On32): Int64On32;
  (* Equivalent to Int64On32{a := Swap4(i.b); b := Swap4(i.a)}. *)

PROCEDURE SwapInt(i: INTEGER): INTEGER;
  (* Swaps all of the bytes of the given integer argument. *)

END Swap.
