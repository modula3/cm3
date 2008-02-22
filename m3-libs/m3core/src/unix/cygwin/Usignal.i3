(* Copyright (C) 1990, Digital Equipment Corporation.                 *)
(* All rights reserved.                                               *)
(* See the file COPYRIGHT for a full description.                     *)
(*                                                                    *)
(* Last modified on Mon Jan  5 11:11:07 GMT 1998 by rrw               *)
(*      modified on Fri Feb 24 15:18:21 PST 1995 by kalsow            *)
(*      modified on Tue Feb 14 20:58:12 GMT 1995 by rrw1000@cam.ac.uk *)
(*      modified on Tue Mar  2 17:18:02 PST 1993 by muller            *)

(* $Id$ *)

(* This file was generated from Usignal.i3.c. Do not edit it. *)

INTERFACE Usignal;

FROM Ctypes IMPORT int, unsigned_int;

(*** <signal.h> ***)

CONST
  SIGINT = 16_00000002; (* interrupt *)
  SIGKILL = 16_00000009; (* kill (cannot be caught or ignored) *)

TYPE
  SignalHandler = ADDRESS;
  SignalActionHandler = ADDRESS;


(*** kill(2) - send signal to a process ***)

<*EXTERNAL*> PROCEDURE kill (pid, sig: int): int;


END Usignal.
