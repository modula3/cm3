(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* Last modified on Wed Dec 23 17:45:12 PST 1992 by jdd            *)
(*      modified on Wed Jun 27 16:36:26 1990 by piet@cs.ruu.nl *)
(*      modified on Tue Mar 20 04:31:49 1990 by muller        *)

INTERFACE Usem;

FROM Ctypes IMPORT short, int;
FROM Utypes IMPORT ushort, time_t, key_t;
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

  SEM_UNDO = 8_10000;  (* set up adjust on exit entry *)

(*
**      Semctl Command Definitions.
*)

  GETNCNT = 3;       (* get semncnt *)
  GETPID  = 4;       (* get sempid *)
  GETVAL  = 5;       (* get semval *)
  GETALL  = 6;       (* get all semval's *)
  GETZCNT = 7;       (* get semzcnt *)
  SETVAL  = 8;       (* set semval *)
  SETALL  = 9;       (* set all semval's *)

(*
**      Structure Definitions.
*)

(*
**      There is one semaphore id data structure for each set of semaphores
**              in the system.
*)

TYPE

  struct_semid_ds = RECORD
      sem_perm: struct_ipc_perm;            (* operation permission struct *)
      sem_base: UNTRACED REF struct_sem;    (* ptr to first semaphore in set *)
      sem_nsems: ushort;                    (* # of semaphores in set *)
      sem_otime: time_t;                    (* last semop time *)
      sem_ctime: time_t;                    (* last change time *)
    END;

(*
**      There is one semaphore structure for each semaphore in the system.
*)

  struct_sem = RECORD
      semval:    ushort;        (* semaphore text map address *)
      sempid:    short;         (* pid of last operation *)
      semncnt:   short;         (* # awaiting semval > cval *)
      semzcnt:   ushort;        (* # awaiting semval = 0 *)
    END;

(*
**      There is one undo structure per process in the system.
*)

  struct_sem_undo = RECORD
    un_np: UNTRACED REF struct_sem_undo;(* ptr to next active undo structure *)
    un_cnt: short;                      (* # of active entries *)
    un_ent: ARRAY [0..0] OF RECORD      (* undo entries (one minimum) *)
                un_aoe: short;                (* adjust on exit values *)
                un_num: short;                (* semaphore # *)
                un_id:  int;                  (* semid *)
              END;
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

  SEMMAP = 10;
  SEMMNI = 10;
  SEMMNS = 60;
  SEMMNU = 30;
  SEMMSL = 25;
  SEMOPM = 10;
  SEMUME = 10;
  SEMVMX = 32767;
  SEMAEM = 16384;


(*** semctl(2) - semaphore control operations ***)

<*EXTERNAL*> PROCEDURE semctl (semid, semnum, cmd: int; arg: union): int;

TYPE 
   union = union_val;

   union_val = int;
   union_buf = UNTRACED REF struct_semid_ds;
(* union_array = ARRAY OF ushort *)


(*** semget(2) - get set of semaphores ***)

<*EXTERNAL*> PROCEDURE semget (key: key_t; nsems: int; semflg: int): int;


(*** semop(2) - semaphore operations ***)

<*EXTERNAL*>
PROCEDURE semop (semid: int; 
                 sops: ARRAY [0..0] OF UNTRACED REF struct_sembuf;
                 nsops: int): int;


END Usem.
