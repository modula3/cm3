(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* Last modified on Mon Mar 28 07:52:07 PST 1994 by kalsow        *)
(*      modified on Sat Mar 17 07:02:31 1990 by muller        *)

INTERFACE Uipc;

FROM Ctypes IMPORT char, char_star, long;
FROM Utypes IMPORT uid_t, gid_t, mode_t, ushort_t, key_t;

(*** <sys/ipc.h> ***)

TYPE
  mtyp_t = long;

  struct_ipc_perm = RECORD
        uid:    uid_t;         (* owner's user id *)
        gid:    gid_t;         (* owner's group id *)
        cuid:   uid_t;         (* creator's user id *)
        cgid:   gid_t;         (* creator's group id *)
        mode:   mode_t;        (* access modes *)
        seq:    ushort_t;      (* slot usage sequence number *)
        key:    key_t;         (* key *)
      END;


(*--------------------------------------------------------- _XOPEN_SOURCE ---*)

CONST
  (* common IPC operation flag definitions *) 
  IPC_CREAT  = 8_0001000;               (* create entry if key doesn't exist *)
  IPC_EXCL   = 8_0002000;               (* fail if key exists *)
  IPC_NOWAIT = 8_0004000;               (* error if request must wait *)

  (* Keys. *)
  IPC_PRIVATE : key_t = 0;              (* private key *)

  (* Control Commands. *)
  IPC_RMID = 0;		(* remove identifier *)
  IPC_SET  = 1; 	(* set options *)
  IPC_STAT = 2;		(* get options *)

(*----------------------------------------------------------- _OSF_SOURCE ---*)
  IPC_ALLOC = 8_0100000;           (* entry currently allocated *)
  IPC_R     = 8_0000400;           (* read or receive permission *)
  IPC_W     = 8_0000200;           (* write or send permission *)


<*EXTERNAL*>
PROCEDURE ftok (path_name: char_star; project_id: char): key_t;

END Uipc.
