(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(* Last modified on Fri Feb 10 11:32:28 PST 1995 by kalsow     *)
(*      modified on Thu Jul 15 15:59:09 PDT 1993 by swart      *)

INTERFACE MachineID;

TYPE T = RECORD r: ARRAY [0..5] OF BITS 8 FOR [0..255] END;

PROCEDURE Get(): T;
  (* Returns an ID that is intended to be unique for the machine running
     this process until the next time this machine reboots.  Thus
     this value is unique for this machine during the life of the calling
     process.

     Typically this value is the hardware Ethernet ID of this machine's
     network adaptor.  On a machine without a network adaptor we
     return this machine's IP address which is padded with
     leading zero bytes.  If neither is available this routine generates
     a checked runtime error.

     Consider using TimeStamp.T to generate true unique IDs, or if shorter
     IDs are needed and probabilistic guarantees are enough, consider
     using the fingerprint of a TimeStamp.T.

  *)

PROCEDURE CanGet (VAR(*OUT*) t: T): BOOLEAN;
(* Returns "TRUE" and sets "t" if the machine ID can be determined.
   Otherwise sets "t" to zeroes and returns "FALSE".   Machines that
   don't have network configurations will not have Machine IDs. *)

END MachineID.
