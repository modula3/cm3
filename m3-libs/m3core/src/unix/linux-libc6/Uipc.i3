(* Copyright (C) 1990, Digital Equipment Corporation.                 *)
(* All rights reserved.                                               *)
(* See the file COPYRIGHT for a full description.                     *)
(*                                                                    *)
(* Last modified on Fri Feb 24 14:57:30 PST 1995 by kalsow            *)
(*      modified on Tue Feb 14 20:18:07 GMT 1995 by rrw1000@cam.ac.uk *)
(*      modified on Sat Apr 16 by rrw1000@hermes.cam.ac.uk            *)
(*      modified on Sat Mar 17 07:02:31 1990 by muller                *)

INTERFACE Uipc;

FROM Ctypes IMPORT unsigned_short, long, int;

(*** <sys/ipc.h> ***)

TYPE
  struct_ipc_perm = RECORD
        key:    int;                    (* key *)
        uid:    unsigned_short;         (* owner's user id *)
        gid:    unsigned_short;         (* owner's group id *)
        cuid:   unsigned_short;         (* creator's user id *)
        cgid:   unsigned_short;         (* creator's group id *)
        mode:   unsigned_short;         (* access modes *)
        seq:    unsigned_short;         (* slot usage sequence number *)
      END;


(* Common IPC Definitions. *)
CONST
  (* Mode bits. *)
  (* IPC_ALLOC appears not to be used anymore ??!! - rrw *)
  IPC_ALLOC  = 8_0100000;               (* entry currently allocated *)
  IPC_CREAT  = 8_0001000;               (* create entry if key doesn't exist *)
  IPC_EXCL   = 8_0002000;               (* fail if key exists *)
  IPC_NOWAIT = 8_0004000;               (* error if request must wait *)

  (* Keys. *)
  IPC_PRIVATE : long = 0;               (* private key *)

  (* Control Commands. *)
  IPC_RMID = 0;		(* remove identifier *)
  IPC_SET  = 1; 	(* set options *)
  IPC_STAT = 2;		(* get options *)
  IPC_INFO = 3;         (* see ipcs *)

END Uipc.
