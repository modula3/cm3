(* Copyright (C) 1990, Digital Equipment Corporation.                 *)
(* All rights reserved.                                               *)
(* See the file COPYRIGHT for a full description.                     *)
(*                                                                    *)
(* Last modified on Mon Jan  5 11:10:22 GMT 1998 by rrw               *)
(*      modified on Fri Feb 24 15:11:49 PST 1995 by kalsow            *)
(*      modified on Tue Feb 14 20:42:49 GMT 1995 by rrw1000@cam.ac.uk *)
(*      modified on Thu Nov 10 21:16:28 PST 1994 by hayter            *)
(*      modified on Fri Apr 30 14:45:16 PDT 1993 by muller            *)

INTERFACE Ushm;

FROM Ctypes IMPORT int, char_star;
FROM Utypes IMPORT key_t;

(*** <sys/shm.h> ***)

(*
**      IPC Shared Memory Facility.
*)

(*
**      Implementation Constants.
*)

CONST
                        (* segment low boundary address multiple *)
                        (* (SMLBA must be a power of 2) *)
  (* SMLBA = ctob(CLSIZE);   where does ctob come from ? - em *)

                        (* for SYSTEM V compatibility           *)
  (* SHMLBA = SMLBA;         see SMLBA - em *)

(*
**      Permission Definitions.
*)

  SM_R = 8_0400; (* read permission *)
  SM_W = 8_0200;  (* write permission *)

                        (* for SYSTEM V compatibility           *)
  SHM_R = SM_R;
  SHM_W = SM_W;

(*
**      ipc_perm Mode Definitions.
*)

  SM_CLEAR = 8_00000; (* clear segment on next attach *)
    (* Linux does not seem to have this (was 1000) - hayter *)
  SM_DEST  = 8_01000; (* destroy seg when # attached = 0 *)
    (* this was 2000 - hayter *)
  SM_LOCKED = 8_02000; (* Segment will not be swapped *)

                        (* for SYSTEM V compatibility           *)
  SHM_INIT = SM_CLEAR;
  SHM_DEST = SM_DEST;

(*
**      Message Operation Flags.
*)

  SM_RDONLY = 8_010000;        (* attach read-only (else read-write) *)
  SM_RND    = 8_020000;        (* round attach address to SHMLBA *)
  SM_REMAP  = 8_040000;        (* Take-over region on attach *)

                        (* for SYSTEM V compatibility           *)
  SHM_RDONLY = SM_RDONLY;
  SHM_RND    = SM_RND;
  SHM_REMAP  = SM_REMAP;

(*
 *  sizing constant (the balance are in /sys/h/param.h and /sys/conf/param.c.
 *)
  SMMNI = 128;        (* Max number SM segments in system *)

(*
**      Structure Definitions.
*)

CONST

(* NOTE:  These values must align with X* flag values in text.h *)
  SMLOCK =  8_0010;           (* Being swapped in or out *)
  SMWANT =  8_0020;           (* Wanted for swapping *)
  SMNOSW =  8_0100;           (* Lock segment in memory *)

(* shared memory specific commands for shmctl syscall *)
  SHM_LOCK   = 11;       (* Lock segment in memory *)
  SHM_UNLOCK = 12;       (* Unlock segment in memory *)

(*
 * sminfo structure is used by the system for bounds checking.  All of 
 * the elements in this structure are initialized in /sys/conf/param.c.
 *
 * Three of the elements are configurable via the "config" program
 * (smmax, smmin, smbrk).  If they are not config'ed, then they all
 * have defaults.
 *)  

TYPE

(*** shmget(2) - get shared memory segment ***)

<*EXTERNAL*>
PROCEDURE shmget (key: key_t; size, semflg: int): int;

(*** shmat(2), shmdt(2) - shared memory operations ***)

<*EXTERNAL*>
PROCEDURE shmat (shmid: int; shmaddr: char_star; shmflg: int): char_star;

<*EXTERNAL*>
PROCEDURE shmdt (shmaddr: char_star): int;

END Ushm.
