(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* Last modified on Wed Mar  4 11:53:08 PST 1992 by muller        *)

INTERFACE Ushm;

FROM Ctypes IMPORT short, int, char_star, unsigned_short, void_star;
FROM Utypes IMPORT time_t, key_t, size_t, swblk_t, pid_t;
FROM Uipc   IMPORT struct_ipc_perm;
FROM Udmap  IMPORT struct_dmap_star;
(* FROM Uproc  IMPORT struct_proc_star; *)

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
                                 (* SM_PERM must be the first               *)
                                 (* element in the structure.               *)
    sm_perm: struct_ipc_perm;    (* permission struct                    (s)*)
    sm_dmap: struct_dmap_star;   (* disk map of shm segment              (p)*)
    sm_ptdaddr: swblk_t;         (* disk address of page table           (p)*)
    sm_size: int;                (* segment size (bytes)                 (p)*)

(* The field sm_caddr is really a struct_proc_star, but
   this require a whole bunch of other interfaces we don't have yet.
   I'll make it void_star for now.  EM. *)
(*    sm_caddr: struct_proc_star;*)  (* ptr to linked proc, if loaded        (s)*)
    sm_caddr: void_star;

(*    sm_ptaddr: struct_pte_star;*)  (* ptr to assoc page table              (p)*)
    sm_ptaddr: void_star;


    sm_rssize: size_t;           (* SM resource set size (pages)         (s)*)
    sm_lpid: pid_t;              (* pid of last smop                     (.)*)
    sm_cpid: pid_t;              (* pid of creator                       (p)*)
    sm_count: unsigned_short;    (* reference count                      (x)*)
    sm_ccount: unsigned_short;   (* number of loaded references          (s)*)
    sm_lcount: unsigned_short;   (* number of processes locking SMS      (s)*)
    sm_flag: short;              (* traced, written flags        (see below)*)
    sm_poip: short;              (* page out in progress count           (s)*)
    sm_atime: time_t;            (* last smat time                       (u)*)
    sm_dtime: time_t;            (* last smdt time                       (u)*)
    sm_ctime: time_t;            (* last change time                     (u)*)
    END;
  struct_smem_star = UNTRACED REF struct_smem;

  (* sys/shm.h uses a bunch of #define to provide what seems to be
     alternate naming:
	#define shmid_ds        smem
	#define shm_perm        sm_perm
	#define shm_segsz       sm_size
	#define shm_cpid        sm_cpid
	#define shm_lpid        sm_lpid
	#define shm_nattch      sm_count
	#define shm_atime       sm_atime
	#define shm_dtime       sm_dtime
	#define shm_ctime       sm_ctime
     I guess that struct shmid_ds was the old form of that type, which
     when then extended in struct smem; fields that existed in
     struct shmid_ds were given a #define, new fields, did not get one.
     Because we cannot simulate the effect of #define, I suppose it is
     better to provide a struct_shmid_ds with "old" style names only. *)

  struct_shmid_ds = RECORD
                                  (* SM_PERM must be the first               *)
                                  (* element in the structure.               *)
    shm_perm: struct_ipc_perm;    (* permission struct                    (s)*)
    shm_dmap: struct_dmap_star;   (* disk map of shm segment              (p)*)
    shm_ptdaddr: swblk_t;         (* disk address of page table           (p)*)
    shm_size: int;                (* segment size (bytes)                 (p)*)
(* same as above *)
(*    shm_caddr: struct_proc_star; *) (* ptr to linked proc, if loaded        (s)*)
    shm_caddr: void_star;
(*    shm_ptaddr: struct_pte_star;*)  (* ptr to assoc page table              (p)*)
    shm_ptaddr: void_star;

    shm_rssize: size_t;           (* SM resource set size (pages)         (s)*)
    shm_lpid: pid_t;              (* pid of last smop                     (.)*)
    shm_cpid: pid_t;              (* pid of creator                       (p)*)
    shm_count: unsigned_short;    (* reference count                      (x)*)
    shm_ccount: unsigned_short;   (* number of loaded references          (s)*)
    shm_lcount: unsigned_short;   (* number of processes locking SMS      (s)*)
    shm_flag: short;              (* traced, written flags        (see below)*)
    shm_poip: short;              (* page out in progress count           (s)*)
    shm_atime: time_t;            (* last smat time                       (u)*)
    shm_dtime: time_t;            (* last smdt time                       (u)*)
    shm_ctime: time_t;            (* last change time                     (u)*)
    END;

  struct_shmid_ds_star = UNTRACED REF struct_shmid_ds;

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

<*EXTERNAL*>
PROCEDURE shmctl (shmid, cmd: int; buf: struct_shmid_ds_star): int;



(*** shmget(2) - get shared memory segment ***)

<*EXTERNAL*>
PROCEDURE shmget (key: key_t; size, semflg: int): int;


(*** shmat(2), shmdt(2) - shared memory operations ***)

<*EXTERNAL*>
PROCEDURE shmat (shmid: int; shmaddr: char_star; shmflg: int): char_star;

<*EXTERNAL*>
PROCEDURE shmdt (shmaddr: char_star): int;


END Ushm.
