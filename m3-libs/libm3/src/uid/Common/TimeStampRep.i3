(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Thu Jul 15 16:48:03 PDT 1993 by swart *)
INTERFACE TimeStampRep;

(* This interface documents the format of a TimeStamp.T.  This
   representation must be followed on all implementations.  *)

IMPORT Swap;

TYPE
  T =
    RECORD
      time: Swap.Int32;
      (* The number of seconds in between the time this timestamp was
         generated and midnight January 1, 1970 UTC, exclusive of leap
         seconds expressed as a 4 byte positive bigendian integer. *)

      fineTime: BITS 8 FOR [0..255];
      (* The fractional part of the number of seconds, expressed in
         number of 1/256ths of a second. *)

      fineCounter: BITS 8 FOR [0..255];
      (* An additional counter to allow up to 256 calls to NEW in the
         same process within a single 1/256 of a second interval. *)

      pidHigh: Swap.UInt16;
      pidLow: Swap.UInt16;
      (* A unique to the machine ID of the process generating the
         TimeStamp.  This ID must not be reused during the indicated
         1/256 of a second interval.  The byte order of pid is not
         important as it is used only for uniqueness within a machine. *)

      machineHigh: BITS 16 FOR ARRAY [0..1] OF BITS 8 FOR [0..255];
      machineLow: BITS 32 FOR ARRAY [0..3] OF BITS 8 FOR [0..255];
      (* A value that is unique during the time "time" above to the machine
         that generated the time stamp.  If bytes 0 and 1 are zero then
         bytes 2 through 5 are an IP address expressed in 4 byte Unix
         network standard order.  Otherwise the value is the 6 byte adaptor
         ID taken from the same ID name space as Ethernet adaptors.  Other
         conventions used for this field should be documented in this file
         to ensure that there are no collisions between address types.
         This value is produced by MachineID.Get(). *)
    END;

(* Time stamps are compared using standard lexicographic order on the
   bytes. *)

(* The hash function is the XOR of the time stamp LOOPHOLEd into
   four *little endian* 32 bit signed integers.  See TimeStamp.m3 for
   the code which claims to implement this on both big endian, little endian,
   32 and 64 bit machines. *)

END TimeStampRep.
