(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* Last modified on Wed Jun 27 12:54:55 1990 by piet@cs.ruu.nl *)
(*      modified on Sat Mar 17 07:02:31 1990 by muller        *)

INTERFACE Uipc;

FROM Ctypes IMPORT short, unsigned_short, long;

(*** <sys/ipc.h> ***)

TYPE
  struct_ipc_perm = RECORD
        uid:    unsigned_short;         (* owner's user id *)
        gid:    unsigned_short;         (* owner's group id *)
        cuid:   unsigned_short;         (* creator's user id *)
        cgid:   unsigned_short;         (* creator's group id *)
        mode:   unsigned_short;         (* access modes *)
        seq:    unsigned_short;         (* slot usage sequence number *)
        key:    long;                   (* key *)
      END;

  struct_ipcmap = RECORD
	m_size: short;
	m_addr: unsigned_short;
      END;


(* Common IPC Definitions. *)
CONST
  (* Mode bits. *)
  IPC_ALLOC  = 8_0100000;               (* entry currently allocated *)
  IPC_CREAT  = 8_0001000;               (* create entry if key doesn't exist *)
  IPC_EXCL   = 8_0002000;               (* fail if key exists *)
  IPC_NOWAIT = 8_0004000;               (* error if request must wait *)
  IPC_LOCKED = 8_0040000;		(* structure is locked *)
  IPC_WANTED = 8_0004000;		(* process waiting for lock *)

  (* Keys. *)
  IPC_PRIVATE : long = 0;               (* private key *)

  (* Control Commands. *)
  IPC_RMID = 0;		(* remove identifier *)
  IPC_SET  = 1; 	(* set options *)
  IPC_STAT = 2;		(* get options *)


END Uipc.
