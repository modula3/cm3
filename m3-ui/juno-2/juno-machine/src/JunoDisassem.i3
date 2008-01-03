(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Oct 16 16:49:48 PDT 1992 by heydon                   *)

INTERFACE JunoDisassem;

IMPORT Wr, Thread, JunoRT AS RT;

PROCEDURE P (bs: RT.ByteStream; wr: Wr.T) RAISES {Wr.Failure, Thread.Alerted};
(* Disassemble a bytestream, sending the assembly program to the specified
   writer. The values of any "globals" referenced by "PUSHG" or "POPG"
   instructions are written after the instruction disassembly. *)

END JunoDisassem.
