(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(*                                                            *)
(* Last modified on Fri Sep 24 09:47:19 PDT 1993 by kalsow    *)
(*      modified on Tue Mar 20 04:49:58 1990 by muller        *)

INTERFACE Ushm;

FROM Ctypes IMPORT char, short, int;
FROM Utypes IMPORT time_t, key_t, size_t, ushort, swblk_t;
FROM Uipc   IMPORT struct_ipc_perm;

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

  SM_CLEAR = 8_01000; (* clear segment on next attach *)
  SM_DEST  = 8_02000; (* destroy seg when # attached = 0 *)

                        (* for SYSTEM V compatibility           *)
  SHM_INIT = SM_CLEAR;
  SHM_DEST = SM_DEST;

(*
**      Message Operation Flags.
*)

  SM_RDONLY = 8_010000;        (* attach read-only (else read-write) *)
  SM_RND    = 8_020000;        (* round attach address to SHMLBA *)

                        (* for SYSTEM V compatibility           *)
  SHM_RDONLY = SM_RDONLY;
  SHM_RND    = SM_RND;

(*
 *  sizing constant (the balance are in /sys/h/param.h and /sys/conf/param.c.
 *)
  SMMNI = 100;        (* Max number SM segments in system *)

(*
**      Structure Definitions.
*)


(*
**      There is a shared mem id struct for each segment in the system.
*)

TYPE
  struct_smem = RECORD
                                (* SM_PERM must be the first    *)
                                (* element in the structure.    *)
      sm_perm:    struct_ipc_perm;          (* permission struct *)
      sm_daddr:   UNTRACED REF swblk_t;    (* disk addrs of DMTEXT page segs *)
      sm_ptdaddr: swblk_t;                  (* disk address of page table *)
      sm_size:    size_t ;                  (* segment size (bytes) *)

(*      sm_caddr:   UNTRACED REF struct_proc; (* ptr to linked proc, if loaded *)*)
      sm_caddr: ADDRESS;                    (* where is struct_proc ? - em *)
      
(*      sm_ptaddr:  UNTRACED REF struct_pte;  (* ptr to assoc page table *) *)
      sm_ptaddr:  ADDRESS;                  (* where is struct_pte ? - em *)

      sm_rssize:  size_t;                   (* SM resource set size (pages) *)
      sm_lpid:    ushort;                   (* pid of last smop *)
      sm_cpid:    ushort;                   (* pid of creator *)
      sm_count:   char;                     (* reference count *)
      sm_ccount:  char;                     (* number of loaded references *)
      sm_lcount:  short;                  (* number of processes locking SMS *)
      sm_flag:    short;                    (* traced, written flags *)
      sm_poip:    short;                    (* page out in progress count *)
      sm_atime:   time_t;                   (* last smat time *)
      sm_dtime:   time_t;                   (* last smdt time *)
      sm_ctime:   time_t;                   (* last change time *)
    END;

                        (* for SYSTEM V compatibility           *)
  (*  key_t = long;     already defined in Utypes.i3 - em *)

  struct_shmid_ds = RECORD
                                (* SM_PERM must be the first    *)
                                (* element in the structure.    *)
      shm_perm:    struct_ipc_perm;          (* permission struct *)
      sm_daddr:   UNTRACED REF swblk_t;    (* disk addrs of DMTEXT page segs *)
      sm_ptdaddr: swblk_t;                  (* disk address of page table *)
      shm_segsz:  size_t ;                  (* segment size (bytes) *)

(*      sm_caddr:   UNTRACED REF struct_proc; (* ptr to linked proc, if loaded *)*)
      sm_caddr: ADDRESS;                    (* where is struct_proc ? - em *)
      
(*      sm_ptaddr:  UNTRACED REF struct_pte;  (* ptr to assoc page table *) *)
      sm_ptaddr:  ADDRESS;                  (* where is struct_pte ? - em *)

      sm_rssize:  size_t;                   (* SM resource set size (pages) *)
      shm_lpidL:  ushort;                   (* pid of last smop *)
      shm_cpid:   ushort;                   (* pid of creator *)
      shm_nattch: char;                     (* reference count *)
      sm_ccount:  char;                     (* number of loaded references *)
      sm_lcount:  short;                  (* number of processes locking SMS *)
      sm_flag:    short;                    (* traced, written flags *)
      sm_poip:    short;                    (* page out in progress count *)
      shm_atime:  time_t;                   (* last smat time *)
      shm_dtime:  time_t;                   (* last smdt time *)
      shm_ctime:  time_t;                   (* last change time *)
    END;


CONST

(* NOTE:  These values must align with X* flag values in text.h *)
  SMLOCK =  8_0010;           (* Being swapped in or out *)
  SMWANT =  8_0020;           (* Wanted for swapping *)
  SMNOSW =  8_0100;           (* Lock segment in memory *)

(* shared memory specific commands for shmctl syscall *)
  SHM_LOCK   = 3;       (* Lock segment in memory *)
  SHM_UNLOCK = 4;       (* Unlock segment in memory *)

(*
 * sminfo structure is used by the system for bounds checking.  All of 
 * the elements in this structure are initialized in /sys/conf/param.c.
 *
 * Three of the elements are configurable via the "config" program
 * (smmax, smmin, smbrk).  If they are not config'ed, then they all
 * have defaults.
 *)  

TYPE
  struct_sminfo = RECORD
      smmax: int;       (* max shared memory segment size *)
      smmin: int;       (* min shared memory segment size *)
      smmni: int;       (* # of shared memory identifiers *)
      smseg: int;       (* max attached shared memory segs per proc *)
      smbrk: int;       (* gap (in clicks) used between data and SM *)
      smsmat: int;      (* max shmem attach addr (clicks) *)
    END;

                        (* for SYSTEM V compatibility *)
  struct_shminfo = RECORD
      shmmax: int;       (* max shared memory segment size *)
      shmmin: int;       (* min shared memory segment size *)
      shmmni: int;       (* # of shared memory identifiers *)
      shmseg: int;       (* max attached shared memory segs per proc *)
      shmbrk: int;       (* gap (in clicks) used between data and SM *)
      smsmat: int;       (* max shmem attach addr (clicks) *)
    END;


(*** shmctl(2) - shared memory control operations ***)

<*EXTERNAL*> PROCEDURE shmctl (shmid, cmd: int; 
                               buf: UNTRACED REF struct_shmid_ds): int;



(*** shmget(2) - get shared memory segment ***)

<*EXTERNAL*> PROCEDURE shmget (key: key_t; size, semflg: int): int;


(*** shmat(2), shmdt(2) - shared memory operations ***)

<*EXTERNAL*> PROCEDURE shmat (shmid: int; shmaddr: ADDRESS; shmflg: int): int;
<*EXTERNAL*> PROCEDURE shmdt (shmaddr: ADDRESS): int;


END Ushm.
