(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* Last modified on Sat Jun 27 15:25:52 PDT 1992 by muller        *)

INTERFACE Ushm;

FROM Ctypes IMPORT int, void_star;
FROM Utypes IMPORT time_t, pid_t, shmatt_t, caddr_t, key_t, u_int;
FROM Uipc   IMPORT struct_ipc_perm, IPC_R, IPC_W;

(*** <sys/shm.h> ***)

(*
**      IPC Shared Memory Facility.
*)

(*
**      Implementation Constants.
*)

CONST
  (*  Implementation constants. *)
  (* SHMLBA = getpagesize (); *)


  (* Operation Flags. *)
  SM_RDONLY = 8_010000;        (* attach read-only (else read-write) *)
  SM_RND    = 8_020000;        (* round attach address to SHMLBA *)

(* Structure Definitions. *)

TYPE
  (* There is a shared mem id data structure for each shared memory
     and mapped file segment in the system. *)

  struct_shmid_ds = RECORD
    shm_perm:   struct_ipc_perm;   (* operation permission struct *)
    shm_segsz:  int;               (* size of segment in bytes *)
    shm_lpid:   pid_t;             (* pid of last shmop *)
    shm_cpid:   pid_t;             (* pid of creator *)
    shm_nattch: shmatt_t;          (* current # attached *)
    shm_atime:  time_t;            (* last shmat time *)
    shm_dtime:  time_t;            (* last shmdt time *)
    shm_ctime:  time_t;            (* last change time *)
  END;


CONST
  (* ipc_perm Mode Definitions. *)
  SHM_R = IPC_R; (* read permission *)
  SHM_W = IPC_W; (* write permission *)

TYPE
  struct_shminfo = RECORD
    shmmax: int;   (* max shared memory segment size *)
    shmmin: int;   (* min shared memory segment size *)
    shmmni: int;   (* number of shared memory identifiers *)
    shmseg: int;   (* max attached shared memory segments per process *)
  END;


(*** shmctl(2) - shared memory control operations ***)

<*EXTERNAL*> PROCEDURE shmctl (shmid, cmd: int; 
                               buf: UNTRACED REF struct_shmid_ds): int;

(*** shmget(2) - get shared memory segment ***)

<*EXTERNAL*> PROCEDURE shmget (key: key_t; size, semflg: u_int): int;


(*** shmat(2), shmdt(2) - shared memory operations ***)

<*EXTERNAL*>
PROCEDURE shmat (shmid: int; shmaddr: void_star; shmflg: int): void_star;
<*EXTERNAL*>
PROCEDURE shmdt (addr: UNTRACED REF caddr_t): int;


END Ushm.
