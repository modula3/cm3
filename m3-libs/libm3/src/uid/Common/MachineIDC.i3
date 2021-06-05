(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(* Last modified on Fri Feb 10 11:32:28 PST 1995 by kalsow     *)
(*      modified on Thu Jul 15 15:59:09 PDT 1993 by swart      *)

INTERFACE MachineIDC;
FROM Ctypes IMPORT int, unsigned_char_star;

<*EXTERNAL MachineIDC__CanGet*>
PROCEDURE CanGet (t: unsigned_char_star(*OUT VAR MachineID.T*)): int;
(* Returns "TRUE" and sets "t" if the machine ID can be determined.
   Otherwise sets "t" to zeroes and returns "FALSE".   Machines that
   don't have network configurations will not have Machine IDs. *)

END MachineIDC.
