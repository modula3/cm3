(* Copyright (C) 1990, Digital Equipment Corporation.                 *)
(* All rights reserved.                                               *)
(* See the file COPYRIGHT for a full description.                     *)
(*                                                                    *)
(* Last modified on Sat Feb 25 09:22:51 PST 1995 by kalsow            *)
(*      modified on Tue Feb 14 20:42:49 GMT 1995 by rrw1000@cam.ac.uk *)
(*      modified on Fri Apr 30 14:44:50 PDT 1993 by muller            *)
(*      modified on Wed Dec 23 17:44:54 PST 1992 by jdd               *)

UNSAFE INTERFACE Usem;

FROM Ctypes IMPORT short, int;
FROM Utypes IMPORT ushort, time_t;
FROM Uexec  IMPORT wait_queue_star;
FROM Uipc   IMPORT struct_ipc_perm;

(*** <sys/sem.h> ***)

(*
**      IPC Semaphore Facility.
*)

(*
**      Implementation Constants.
*)

CONST

  PZERO = 25;                     (* I got this value from param.h; em *)

  PSEMN = (PZERO + 3);     (* sleep priority waiting for greater value *)
  PSEMZ = (PZERO + 2);     (* sleep priority waiting for zero *)

(*
**      Permission Definitions.
*)

  SEM_A = 8_0200;    (* alter permission *)
  SEM_R = 8_0400;    (* read permission *)

(*
**      Semaphore Operation Flags.
*)

  SEM_UNDO = 8_010000;  (* set up adjust on exit entry *)

(*
**      Semctl Command Definitions.  (from /usr/include/linux/sem.h)
*)

  GETPID  = 11;       (* get sempid *)
  GETVAL  = 12;       (* get semval *)
  GETALL  = 13;       (* get all semval's *)
  GETNCNT = 14;       (* get semncnt *)
  GETZCNT = 15;       (* get semzcnt *)
  SETVAL  = 16;       (* set semval *)
  SETALL  = 17;       (* set all semval's *)

(*
**      Structure Definitions.
*)

(*
**      There is one semaphore id data structure for each set of semaphores
**              in the system.
*)

TYPE
  struct_semid_ds = RECORD
    sem_perm  : struct_ipc_perm;           (* operation permission struct *)
    sem_otime : time_t;                    (* last semop time *)
    sem_ctime : time_t;                    (* last change time *)
    sem_base  : UNTRACED REF struct_sem;   (* ptr to first semaphore in set *)
    eventn    : wait_queue_star;
    eventz    : wait_queue_star;
    undo      : UNTRACED REF struct_sem_undo;
    sem_nsems : ushort;                    (* # of semaphores in set *)
  END;

(*
**      There is one semaphore structure for each semaphore in the system.
*)

  struct_sem = RECORD
    sempid  : short;         (* pid of last operation *)
    semval  : ushort;        (* semaphore text map address *)
    semncnt : short;         (* # awaiting semval > cval *)
    semzcnt : ushort;        (* # awaiting semval = 0 *)
  END;

(*
**      There is one undo structure per process in the system.
*)

  struct_sem_undo = RECORD
    proc_next : UNTRACED REF struct_sem_undo;
    id_next   : UNTRACED REF struct_sem_undo;
    semid     : int;
    semadj    : short;
    sem_num   : ushort;
  END;

(*
** semaphore information structure
*)

  struct_seminfo = RECORD
    semmap: int;         (* # of entries in semaphore map *)
    semmni: int;         (* # of = semaphore; identifiers *)
    semmns: int;         (* # of semaphores in system *)
    semmnu: int;         (* # of undo structures in system *)
    semmsl: int;         (* max # of semaphores per id *)
    semopm: int;         (* max # of operations per semop call *)
    semume: int;         (* max # of undo entries per process *)
    semusz: int;         (* size in bytes of undo structure *)
    semvmx: int;         (* semaphore maximum value *)
    semaem: int;         (* adjust on exit max value *)
  END;

(*
**      User semaphore template for semop system calls.
*)

  struct_sembuf = RECORD
      sem_num: ushort;        (* semaphore # *)
      sem_op:  short;         (* semaphore operation *)
      sem_flg: short;         (* operation flags *)
    END;

(*
 * Sizing constants
 *)

CONST

  SEMMNI = 128; (* Was 10 - rrw *)
  SEMMSL = 32;  (* Was 25 - rrw *)
  SEMMNS = (SEMMNI * SEMMSL);
  SEMMNU = SEMMNS;
  SEMOPM = 32; (* Was 10 - rrw *)
  SEMUME = SEMOPM;
  SEMVMX = 32767;
  SEMAEM = 16384; (* SEMVMX >> 1 *)
  SEMUSZ = 20;
  SEMMAP = SEMMNS;


TYPE 
   union = union_val;

   union_val = int;
   union_buf = UNTRACED REF struct_semid_ds;
(* union_array = ARRAY OF ushort *)



END Usem.
