(* Copyright (C) 1994, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* Last modified on Wed Mar  4 11:53:08 PST 1992 by muller        *)

INTERFACE Uproc;

FROM Ctypes IMPORT int, int_star;
FROM Utypes IMPORT swblk_t;

(*** <sys/proc.h> ***)

TYPE
  struct_proc = RECORD
    p_link: struct_proc_star;	(* linked list of running processes *)
    p_rlink: struct_proc_star;
    p_nxt: struct_proc_star;	(* linked list of allocated proc slots *)
    p_prev: struct_proc_star_star;	(* also zombies, and free proc's *)
    p_addr: struct_pte_star;	(* u-area kernel map address *)
    proc_map: struct_proc_map ; (* machine-dependent VM info *)
    	p_usrpri: s_char;	(* user-priority based on p_cpu and p_nice *)
    	p_pri: s_char;		(* priority, negative is high *)
    p_cpu: char;		(* cpu usage for scheduling *)
    p_stat: char;
    p_time: char;		(* resident time for scheduling *)
    p_nice: s_char;		(* nice for cpu usage *)
    p_slptime: char;	(* time since last block *)
    p_cursig: char;
    p_sig: int;		(* signals pending to this process *)
    p_sigmask: int;	(* current signal mask *)
    p_sigignore: int;	(* signals being ignored *)
    p_sigcatch: int;	(* signals being caught by user *)
    p_sched: int;	(* contains old SLOAD *)
    p_select: int;	(* contains old SSEL *)
    p_vm: int;		(* contains old: SVFORK, SVFDONE, SNOVM,
			 * SKEEP, SPHYSIO, SPAGE, SULOCK, SSWAP,
			 * SPAGI, SUANOM, SSEQL, SLOCK, SPTECHG,
			 * SEXECDN *)
    p_affinity: int;	(* MASK of CPUS on which can run *)
    p_cpumask: int;	(* last cpu on which the process ran *)
    p_trace: int;	(* contains old STRC, SWTED *)
    p_type: int;		(* contains old SSYS, SWEXIT and SLOGIN *)
    p_file: int;		(* contains old SLKDONE *)
    p_sigflag: int;	(* contains new SNOCLDSTP *)
    p_mips_flag: int;	(* contains SFIXADE *)
    p_uid: uid_t;		(* eff. user id, used to direct tty signals *)
    p_suid: uid_t;		(* saved set uid, used to direct tty signals *)
    p_sgid: uid_t;		(* saved set group id, used by setgid *)
    p_pgrp: short;		(* name of process group leader *)
    p_pid: short;		(* unique process id *)
    p_ppid: short;		(* process id of parent *)
    p_xstat: u_short;	(* Exit status for wait *)
    p_progenv: u_short;	(* process compatibility mode *)
    p_poip: short;		(* page outs in progress *)
    p_sid: pid_t;		(* session id (for POSIX job control) *)
    p_ru: struct_rusage_star;	(* mbuf holding exit information *)
    p_tsize: size_t;	(* size of text (clicks) *)
    p_dsize: size_t;	(* size of data space (clicks) *)
    p_ssize: size_t;	(* copy of stack size (clicks) *)
    p_rssize: size_t; 	(* current resident set size in clicks *)
    p_maxrss: size_t;	(* copy of u.u_limit[MAXRSS] *)
    p_swrss: size_t;	(* resident set size before last swap *)
    p_wchan: caddr_t;	(* event process is awaiting *)
    p_exist: int;	(* if P_DYING, process will exit *)
    p_ref: int;		(* count of processes relying on p *)
    p_waitchk: int;	(* coordinates wait with setting ZOMB and STOP *)
    p_textp: struct_text_star;	(* pointer to text structure *)
    p_xlink: struct_proc_star;	(* linked list of procs sharing same text *)
    p_cpticks: short;	(* ticks of cpu time *)
    p_pctcpu: int;	(* %cpu for this process during p_time *)
    p_fp: long;		(* generate SIGFPE on all fp interrupts *)
    p_puac: u_short;	(* print/don't print unaligned access msgs *)
    p_ndx: short;		(* proc index for memall (because of vfork) *)
    p_idhash: short;	(* hashed based on p_pid for kill+exit+... *)
    p_pptr: struct_proc_star;	(* pointer to process structure of parent *)
    p_cptr: struct_proc_star;	(* pointer to youngest living child *)
    p_osptr: struct_proc_star;	(* pointer to older sibling processes *)
    p_ysptr: struct_proc_star;	(* pointer to younger siblings *)
    p_debug: char_star;	(* for ptrace/procxmt communciation *)
    p_realtimer: struct_itimerval;
    p_quota: struct_quota_star;	(* quotas for this process *)
    p_ttyp; struct_tty_star;    (* controlling tty pointer 	*)
    p_smsize; size_t;	(* size of SM space (clicks)	*)
    p_smbeg: int;	(* page table offset of first SMS *)
    p_smend: int;	(* page table offset of end of	*)
			(*	last attached SMS.	*)
    p_smcount: int;      (* count of SMS attached to proc *)
    p_sm: struct_p_sm_star;     (* shared memory related elements	*)
    p_hlock: struct_lock_t_star;
    p_dmap: struct_dmap_star;	(* disk map for data segment *)
    p_smap: struct_dmap_star;	(* disk map for stack segment *)
    p_cdmap: struct_dmap_star;	(* shadows data/stack swap *)
    p_csmap: struct_dmap_star;	(* used during fork/exec*)

    p_vpcontext: struct_vpcontext_star;
					(* ptr to proc's vp context area *)
    END;

  struct_p_sm = RECORD
    sm_p: struct_smem_star;	(* pointer to SM struct	*)
				(* linked list of procs		*)
				(* sharing the same shared	*)
				(* memory segment.		*)
    sm_link: struct_proc_star;
    sm_saddr: int;		(* starting addr of SMS *)
    sm_eaddr: int;		(* ending addr of SMS *)
    sm_pflag: int;	(* R/W permissions	*)
    sm_lock: short;	(* this proc has locked SMS *)
    END;
  struct_p_sm_star = UNTRACED REF struct_p_sm;


CONST
  PIDHSZ = 64;

  UAC_MSGON = 1      (* print unaligned access messages, dflt *)
  UAC_MSGOFF = 0     (* don't print unaligned access messages *)

(* stat codes *)
CONST
  SSLEEP = 1;		(* awaiting an event *)
  SWAIT  = 2;		(* (abandoned state) *)
  SRUN   = 3;		(* running *)
  SIDL   = 4;		(* intermediate state in process creation *)
  SZOMB  = 5;		(* intermediate state in process termination *)
  SSTOP  = 6;		(* process being traced *)

(* flag codes *)
(* These have been divied up into several fields:
 *	p_vm, p_sched, p_type, p_trace, p_file
 *	u_oweupc, u_sigflag.
 *	Note that STIMO isn't used anywhere
 * Both ps and pstat count on there being no overlap in bits,
 * so that these fields can be or'red together for printing
 *)
CONST
  SLOAD	    = 16_0000001; (* p_sched: in core *)
  SSYS      = 16_0000002; (* p_type:  or pager process *)
  SLOCK     = 16_0000004; (* p_vm: process being swapped out *)
  SSWAP     = 16_0000008; (* p_vm: save area flag *)
  STRC      = 16_0000010; (* p_trace: process is being traced *)
  SWTED     = 16_0000020; (* p_trace: another tracing flag *)
  SULOCK    = 16_0000040; (* p_vm: user settable lock in core *)
  SPAGE     = 16_0000080; (* p_vm: process in page wait state *)
  SKEEP     = 16_0000100; (* p_vm: another flag to prevent swap out *)
  SOMASK    = 16_0000200; (* u_sigflag: restore old mask after taking signal *)
  SWEXIT    = 16_0000400; (* p_type:  on exiting *)
  SPHYSIO   = 16_0000800; (* p_vm: doing physical i/o (bio.c) *)
  SVFORK    = 16_0001000; (* p_vm: process resulted from vfork() *)
  SVFDONE   = 16_0002000; (* p_vm: another vfork flag *)
  SNOVM     = 16_0004000; (* p_vm: no vm, parent in a vfork() *)
  SPAGI     = 16_0008000; (* p_vm: init data space on demand, from inode *)
  SSEQL     = 16_0010000; (* p_vm: user warned of sequential vm behavior *)
  SUANOM    = 16_0020000; (* p_vm: user warned of random vm behavior *)
  STIMO     = 16_0040000; (* NOT_USED: timing out during sleep *)
  (* was SDETACH *)
  SXCTDAT   = 16_0080000; (* p_vm: an icache flush on pagein *)
  SNOCLDSTP = 16_0100000; (* p_sigflagPOSIX child stop flag *)
  (* was SOUSIG *)
  SOUSIG    = 16_0100000; (* u_sigflag: using old signal mechanism *)
  SOWEUPC   = 16_0200000; (* u_oweupc: owe process an addupc() call at next ast *)
  SSEL      = 16_0400000; (* p_sched: selecting; wakeup/waiting danger *)
  SLOGIN    = 16_0800000; (* p_type:  login process (legit child of init) *)
  SPTECHG   = 16_1000000; (* p_vm: pte's for process have changed *)
  SNFSPGN   = 16_2000000; (* p_vm: uninterruptible pagin over nfs *)
  SLKDONE   = 16_4000000; (* p_file: Sys-V file locking applied *)
  SFIXADE   = 16_8000000; (* fixup unalligned address errors *)
  SIDLEP    = 16_20000000; (* idle process *)

(* flags for p_fp *)
  P_FP_SIGINTR1 = 1;
  P_FP_SIGINTR2	= 2;

  SEXECDN  = 0x10000000;  (* p_vm: exec() done - needed for POSIX pgrp test *)

  P_VM_NO_OP   = SPTECHG;

(* for affinity *)
  PRIMARY  = 1;
  ALLCPU  = -1;

(* for p_exist *)
  P_ALIVE = 0;	(* process will stick around if ref'ed *)
  P_DYING = 1;	(* no longer legal to ref *)
  P_DEAD  = 2;	(* done with exit: parent may now clear *)

(*
 * There is one bit for every active process slot
 *)

(*
 * This algorithm works out to 39 usec for any mask set.
 *  For fully populated masks, this is 1.22 usec/proc,
 *		for half full, this is 2.44 usec/proc
 *		for 1/4  full, this is 4.88 usec/proc
 *
 * If we assume a half populated process table of 1024 entries,
 * clustered entirely in the top 2/3s of the table, you get:
 *
 *	1024/32 = 32 masks
 *	32 * (2/3) = 22 used masks
 *	22 masks * 39 usec/mask = .858 microseconds/table
 *)

(*  
 * We have handcrafted while loops from If's and Goto's, so
 * that if a user of the macro issues a Break, it will break
 * out of the For loop, and not one of the inner While loops
 *)

END Uproc.
