(* Copyright (C) 1994, Digital Equipment Corporation            *)
(* All rights reserved.                                         *)
(* See the file COPYRIGHT for a full description.               *)
(*                                                              *)
(* Last modified on Fri Nov 11 08:39:07 PST 1994 by kalsow      *)
(*      modified on Sun Nov  6 16:39:26 MET 1994 by Olaf Wagner *)
(*      modified on Sat Apr 16 by rrw1000@hermes.cam.ac.uk      *)
(*      modified on Tue Jun  8 16:26:41 PDT 1993 by mcjones     *)
(*      modified on Mon Jan 11 14:34:49 PST 1993 by muller      *)
(*                                                              *)
(* ow 01.10.1994 adjusted file mode flags for FreeBSD           *)
(* ow Sun Nov  6 16:39:26 MET 1994                              *)
(*    Most of the system calls have been checked and adjusted   *)
(*    for FreeBSD now. Still to be done completedly: ioctl      *)

INTERFACE Unix;

FROM Word IMPORT Or, And, Shift;

FROM Ctypes IMPORT short, int, long, char_star, 
                   char_star_star;
FROM Utypes IMPORT dev_t, off_t, mode_t, pid_t, (*tcflag_t,*) u_long, 
                   (*cc_t, speed_t,*) uid_t, gid_t, size_t;
FROM Utime IMPORT struct_timeval;

CONST
  MaxPathLen = 1024;

(*** file flags ***)
CONST
  FREAD =      16_0000;        (* descriptor read/receive'able *)
  FWRITE =     16_0001;        (* descriptor write/send'ale *)
  FCREAT =     16_0200;        (* create if nonexistant *)
  FEXCL =      16_0800;        (* error if already created *)
  FTRUNC  =    16_0400;        (* truncate to zero length *)
  FAPPEND =    16_0008;        (* append on each write *)
  FNBLOCK =    16_0004;        (* POSIX no delay *)
  FNDELAY =    FNBLOCK;        (* no delay *)
  FSYNCRON =   16_0080;        (* write file syncronously *)
  FMARK =      16_1000;        (* makr during gc() *)
  FDEFER =     16_2000;        (* fefer for next gc pass *)
  FASYNC =     16_0040;        (* signal pgrp when data ready *)
  FSHLOCK =    16_0010;        (* shared lock present *)
  FEXLOCK =    16_0020;        (* exclusive lock present *)

CONST
  MSETUID = 8_4000;
  MSETGID = 8_2000;
  MSTICKY = 8_1000;
  MROWNER = 8_0400;
  MWOWNER = 8_0200;
  MXOWNER = 8_0100;
  MRGROUP = 8_0040;
  MWGROUP = 8_0020;
  MXGROUP = 8_0010;
  MROTHER = 8_0004;
  MWOTHER = 8_0002;
  MXOTHER = 8_0001;
  Mrwrr = MROWNER + MWOWNER + MRGROUP + MROTHER;
  Mrwrwrw = MROWNER + MWOWNER + MRGROUP + MWGROUP + MROTHER + MWOTHER;

(*** access - determine the accessibility of file ***)
<*EXTERNAL*> PROCEDURE access (path: char_star; mod: int): int;
(* ok *)

(*** acct - turn accounting on or off ***)
<*EXTERNAL*> PROCEDURE acct (path: char_star): int;
(* ok *)

(*** brk, sbrk - change data segment space allocation *)
<*EXTERNAL*> PROCEDURE brk (addr: ADDRESS): int;
(* ok *)
<*EXTERNAL*> PROCEDURE sbrk (inc: int): char_star;
(* ok *)

(*** chdir - change working directory ***)
<*EXTERNAL*> PROCEDURE chdir (path: char_star): int;
(* ok *)

(*** chmod, fchmod - change mde of file ***)
<*EXTERNAL*> PROCEDURE chmod (path: char_star; mode: mode_t): int;
<*EXTERNAL*> PROCEDURE fchmod (fd, mode: mode_t): int;
(* ok *)

(*** chown, fchown - change owner and group of a file ***)
<*EXTERNAL*> PROCEDURE chown (path: char_star; owner: uid_t; group: gid_t): int;
<*EXTERNAL*> PROCEDURE fchown (fd: int; owner: uid_t; group: gid_t): int;
(* ok *)

(*** chroot - change root directory ***)
<*EXTERNAL*> PROCEDURE chroot (dirname: char_star): int;
(* ok *)

(*** close - delete a descriptor ***)
<*EXTERNAL*> PROCEDURE close (d: int): int;
(* ok *)

(*** creat - create a new file ***)
<*EXTERNAL*> PROCEDURE creat (name: char_star; mode: mode_t): int;
(* ok, but obsolete *)

(*** dup, dup2 - duplicate an open file descriptor ***)
<*EXTERNAL*> PROCEDURE dup (oldd: int): int;
<*EXTERNAL*> PROCEDURE dup2 (oldd, newd: int): int;
(* ok *)

(*** execve - execute a file ***)
<*EXTERNAL*> PROCEDURE execve (name: char_star; 
                           argv, envp: char_star_star): int;
(* ok *)

(*** exit - terminate a process ***)
<*EXTERNAL*> PROCEDURE exit (i: int);
(* ok *)

(*** _exit - terminate a process without performing C I/O library cleanup ***)
<*EXTERNAL "_exit"*> PROCEDURE underscore_exit (i: int);
(* ok *)

(*** fcntl - file control ***)
CONST   (* request *)
  F_DUPFD =  0;   (* Duplicate fd *)
  F_GETFD =  1;   (* Get close-on-exec flag *)
  F_SETFD =  2;   (* Set close-on-exec flag *)
  F_GETFL =  3;   (* Get fd status flags *)
  F_SETFL =  4;   (* Set fd status flags *)

  (* in these three cases, you need to pass LOOPHOLE (ADR (v), int) 
     for arg, where v is a variable of type struct_flock *)
  F_GETOWN = 5;   (* Set owner *)
  F_SETOWN = 6;   (* Get owner *)

  F_GETLK  = 7;   (* Get file lock *)
  F_SETLK  = 8;   (* Set file lock *)
  F_SETLKW = 9;   (* Set file lock and wait *)

CONST (* fd flags *)
  FD_CLOEXEC = 1;    (* Close file descriptor on exec() *)

TYPE
  struct_flock = RECORD
    l_type:   short; (* see below *)
    l_whence: short;
    l_start:  off_t   := 0;
    l_len:    off_t   := 0;
    l_pid:    pid_t   := 0;
  END;
(* ok *)

CONST (* l_type values *)
  F_RDLCK = 1; (* Read lock *) 
  F_WRLCK = 3; (* Write lock *)
  F_UNLCK = 2; (* Remove lock(s) *)

<*EXTERNAL "ufcntl"*> PROCEDURE fcntl (fd, request, arg: int): int;
(* ok *)

(*** flock - apply or remove an advisory lock on an open file ***)
CONST
  LOCK_SH = 1;   (* shared lock *)
  LOCK_EX = 2;   (* exclusive lock *)
  LOCK_NB = 4;   (* don't block when locking *)
  LOCK_UN = 8;   (* unlock *)

<*EXTERNAL*> PROCEDURE flock (fd, operation: int): int;
(* ok *)

(*** fork - create a new process ***)
<*EXTERNAL*> PROCEDURE fork (): pid_t;
(* ok *)

(*** fsync - synchronize a file's in-core state with that on disk ***)
<*EXTERNAL*> PROCEDURE fsync (fd: int): int;
(* ok *)

(*** getdirentries - gets directory entries in a generic directory format ***)
<*EXTERNAL*> PROCEDURE getdirentries (fd: int; buf: ADDRESS;
                                  nbytes: int; VAR basep: long): int;
(* ok *)

(*** getdomainname, setdomainname - get or set name of current domain ***)
<*EXTERNAL*> PROCEDURE getdomainname (name: char_star; namelen: int): int;
<*EXTERNAL*> PROCEDURE setdomainname (name: char_star; namelen: int): int;
(* ok *)

(*** getdtablesize - get descriptor table size ***)
<*EXTERNAL*> PROCEDURE getdtablesize (): int;
(* ok *)

(*** getgroups - get group access list ***)
<*EXTERNAL*> PROCEDURE getgroups (gidsetsize: int; VAR gidset: int): int;
(* ok *)

(*** gethostid, sethostid - get/set unique identifier of current host ***)
<*EXTERNAL*> PROCEDURE gethostid (): long;
<*EXTERNAL*> PROCEDURE sethostid (hostid: long): int;
(* ok *)


(*** gethostname, sethostname - get/set name of current host ***)
<*EXTERNAL*> PROCEDURE gethostname (name: char_star; namelen: int): int;
<*EXTERNAL*> PROCEDURE sethostname (name: char_star; namelen: int): int;
(* ok *)

(*** getpagesize - get system page size ***)
<*EXTERNAL*> PROCEDURE getpagesize (): int;
(* ok *)

(*** getwd - get current working directory pathname ***)
<*EXTERNAL*> PROCEDURE getwd (pathname: char_star): char_star;
<*EXTERNAL*> PROCEDURE getcwd (pathname: char_star; size: size_t): char_star;
(* ok *)

(*** ioctl - control device ***)
(* this is a temptative declaration of the types of the arguments. 
   it shoulb probably done in another way, so it is commented. We really need
   only the sizes of these records; see below

TYPE
  struct_sgttyb_ULTRIX = RECORD
	sg_ispeed: char;		(* input speed *)
	sg_ospeed: char;		(* output speed *)
	sg_erase:  char;		(* erase character *)
	sg_kill:   char;		(* kill character *)
	sg_flags:  short;		(* mode flags *)
        END;
(* ok *)

  struct_tchars = RECORD
	t_intrc:  char;		/* Interrupt			*/
	t_quitc:  char;		/* Quit 			*/
	t_startc: char;		/* Start output 		*/
	t_stopc:  char;		/* Stop output			*/
	t_eofc:   char; 	/* End-of-file (EOF)		*/
	t_brkc:   char; 	/* Input delimiter (like nl)	*/
	END;
(* ok *)

  struct_ltchars = RECORD
	t_suspc:  char;		/* Stop process signal		*/
	t_dsuspc: char;		/* Delayed stop process signal	*/
	t_rprntc: char;		/* Reprint line 		*/
	t_flushc: char;		/* Flush output (toggles)	*/
	t_werasc: char;		/* Word erase			*/
	t_lnextc: char;		/* Literal next character	*/
	END;
(* ok *)

  struct_winsize = RECORD
	ws_row, ws_col:       u_short; 	/* Window charact. size */
	ws_xpixel, ws_ypixel: u_short;	/* Window pixel size	*/
	END;
(* ok *)

(*
  struct_termio = RECORD
	c_iflag:   u_short;	/* input modes */
	c_oflag:   u_short;	/* output modes */
	c_cflag:   u_short;	/* control modes */
	c_lflag:   u_short;	/* line discipline modes */
	c_line:    char;	/* line discipline */
	c_cc[NCC]: u_char;	/* control chars */
	END;
*)

  struct_termios = RECORD
	c_iflag:    tcflag_t;		/* Input Modes 		*/
	c_oflag:    tcflag_t;        	/* Output Modes		*/
	c_cflag:    tcflag_t;        	/* Control Modes	*/
	c_lflag:    tcflag_t;        	/* Local Modes 		*/
	c_cc[NCCS]: cc_t;          	/* Control Characters	*/
	c_ispeed,
	c_ospeed:   speed_t;
	END;
(* ok *)

(* This is how far I got. I don't think anybody will ever need it,
   so I stop here. ow Sun Nov  6 17:12:47 MET 1994 *)
  struct_rtentry = RECORD
	u_long	        rt_hash;	/* to speed lookups */
	struct_sockaddr rt_dst;	       /* key */
	struct_sockaddr rt_gateway;	/* value */
	short	        rt_flags;	/* up/down?, host/net */
	short      	rt_refcnt;	/* # held references */
	u_long	        rt_use;		/* raw # packets forwarded */
	struct_ifnet    *rt_ifp;       /* the answer: interface to use */
	struct_rtentry *rt_next;       /* next in list */
	END;

  struct_ifreq_1 = RECORD
	char	ifr_name[16];		/* if name, e.g. "en0" */
	struct_sockaddr ifru_addr;
        END;
  struct_ifreq_2 = RECORD
	char	ifr_name[16];		/* if name, e.g. "en0" */
	u_short         ifru_flags;
	END;
  struct_ifreq_3 = RECORD
	char	ifr_name[16];		/* if name, e.g. "en0" */
	int	ifru_metric;
	END;
  struct_ifreq_4 = RECORD
	char	ifr_name[16];		/* if name, e.g. "en0" */
	caddr_t ifru_data;
	END;

  struct_ifconf_1 = RECORD
	int	ifc_len;		/* size of associated buffer */
	caddr_t	ifcu_buf;
	END;
  struct_ifconf_2 = RECORD
	int	ifc_len;		/* size of associated buffer */
	struct	ifreq *ifcu_req;
	END;

  struct_ctrreq_1 = RECORD
	char	ctr_name[IFNAMSIZ];	/* if name */
	char	ctr_type;		/* type of interface */
	struct_estat ctrc_ether;/* ethernet counters */
	END;
  struct_ctrreq_2 = RECORD
	char	ctr_name[IFNAMSIZ];	/* if name */
	char	ctr_type;		/* type of interface */
	struct_dstat ctrc_ddcmp;/* DDCMP pt-to-pt counters */
	END;

  struct_ifdevea = RECORD
        char    ifr_name[IFNAMSIZ];             /* if name, e.g. "en0" */
        u_char default_pa[6];                   /* default hardware address */
        u_char current_pa[6];                   /* current physical address */
	END;

  struct_arpreq = RECORD
	struct_sockaddr arp_pa;		/* protocol address */
	struct_sockaddr arp_ha;		/* hardware address */
	int             arp_flags;	/* flags */
	END;

  struct_ifstate = RECORD
        char    ifr_name[IFNAMSIZ];     /* if name, e.g. "en0" */
	u_short	if_family;		/* current family ownership */
	u_short	if_next_family;		/* next family ownership */
	u_short	if_mode:3,		/* mode of device */
		if_ustate:1,		/* user requested state */
		if_nomuxhdr:1,		/* if set, omit mux header */
		if_dstate:4,		/* current state of device */
		if_xferctl:1,		/* xfer control to nxt family */
		if_rdstate:1,		/* read current state */
		if_wrstate:1,		/* set current state */
		if_reserved:4;
	END;

  struct_solicit_1 = RECORD
	END;

  struct_response_1 = RECORD
	END;

  struct_lat_ucom = RECORD
	END;

  struct_lat_ini = RECORD
	END;

#define MAXLTADESTSIZE 16
#define MAXLTASERVSIZE 16
#define MAXLTAPORTSIZE 16
 
  struct_ltattyi = RECORD
	char lta_dest_port[MAXLTADESTSIZE+1];
	char lta_server_name[MAXLTASERVSIZE+1];
	char lta_server_port[MAXLTAPORTSIZE+1];
	END;

  struct_pt = RECORD
	END;

  struct_el = RECORD
	END;

  struct_mtop = RECORD
	short	mt_op;			/* Operations defined below	*/
	daddr_t mt_count;		/* How many of them		*/
	END;

  struct_mtget = RECORD
	short	mt_type;		/* Type of device defined below */
	short	mt_dsreg;		/* ``drive status'' register	*/
	short	mt_erreg;		/* ``error'' register		*/
	short	mt_resid;		/* Residual count		*/
	END;

  struct_dkop = RECORD
	short	dk_op;			/* Operations defined below	*/
	daddr_t dk_count;		/* How many of them		*/
	END;

  struct_dkget = RECORD
	short	dk_type;		/* Type of device defined below */
	short	dk_dsreg;		/* ``drive status'' register	*/
	short	dk_erreg;		/* ``error'' register		*/
	short	dk_resid;		/* Residual count		*/
	END;

  struct_dkacc = RECORD
	short	dk_opcode;		/* Operation code for access	*/
	long	dk_lbn;			/* Disk sector number		*/
	long	dk_length;		/* Length of transfer		*/
	unsigned dk_status;		/* Status of operation		*/
	unsigned dk_flags;		/* Status flags			*/
	END;

  struct_devget = RECORD
	short	category;		/* Category			*/
	short	bus;			/* Bus				*/
	char	interface[DEV_SIZE];	/* Interface (string)		*/
	char	device[DEV_SIZE];	/* Device (string)		*/
	short	adpt_num;		/* Adapter number		*/
	short	nexus_num;		/* Nexus or node on adapter no. */
	short	bus_num;		/* Bus number			*/
	short	ctlr_num;		/* Controller number		*/
	short	rctlr_num;		/* Remote controller number	*/
	short	slave_num;		/* Plug or line number		*/
	char	dev_name[DEV_SIZE];	/* Ultrix device pneumonic	*/
	short	unit_num;		/* Ultrix device unit number	*/
	unsigned soft_count;		/* Driver soft error count	*/
	unsigned hard_count;		/* Driver hard error count	*/
	long	stat;			/* Generic status mask		*/
	long	category_stat;		/* Category specific mask	*/
	END;
*)

CONST
  IOCPARM_MASK = 16_1fff;          
  IOC_VOID  = Shift (1, 29);       (* no parameters *)
  IOC_OUT   = Shift (1, 30);       (* copy out parameters *)
  IOC_IN    = Shift (1, 31);       (* copy in parameters *)
  IOC_INOUT = Or (IOC_IN, IOC_OUT);


  NOARG  = IOC_VOID;
  R      = IOC_OUT;
  W      = IOC_IN;
  RW     = IOC_INOUT;
(* if we had the structure declarations, we would use these 
  INT    = Shift (And (BYTESIZE (INTEGER),              IOCPARM_MASK), 16);
  SG     = Shift (And (BYTESIZE (struct_sgttyb_ULTRIX), IOCPARM_MASK), 16);
  TCHAR  = Shift (And (BYTESIZE (struct_tchars),        IOCPARM_MASK), 16);
  LC     = Shift (And (BYTESIZE (struct_ltchars),       IOCPARM_MASK), 16);
  WS     = Shift (And (BYTESIZE (struct_winsize),       IOCPARM_MASK), 16);
  TIO    = Shift (And (BYTESIZE (struct_termio),        IOCPARM_MASK), 16);
  IOS    = Shift (And (BYTESIZE (struct_termios),       IOCPARM_MASK), 16);
  RTE    = Shift (And (BYTESIZE (struct_rtentry),       IOCPARM_MASK), 16);
  IFR    = Shift (And (BYTESIZE (struct_ifreq),         IOCPARM_MASK), 16);
  IFC    = Shift (And (BYTESIZE (struct_ifconf),        IOCPARM_MASK), 16);
  CTR    = Shift (And (BYTESIZE (struct_ctrreq),        IOCPARM_MASK), 16);
  IFD    = Shift (And (BYTESIZE (struct_ifdevea),       IOCPARM_MASK), 16);
  ARP    = Shift (And (BYTESIZE (struct_arpreq),        IOCPARM_MASK), 16);
  IFS    = Shift (And (BYTESIZE (struct_ifstate),       IOCPARM_MASK), 16);
  SOL    = Shift (And (BYTESIZE (struct_solicit_1),     IOCPARM_MASK), 16);
  RES    = Shift (And (BYTESIZE (struct_response_1),    IOCPARM_MASK), 16);
  LAU    = Shift (And (BYTESIZE (struct_lat_ucom),      IOCPARM_MASK), 16);
  LAI    = Shift (And (BYTESIZE (struct_lat_ini),       IOCPARM_MASK), 16);
  LTA    = Shift (And (BYTESIZE (struct_ltattyi),       IOCPARM_MASK), 16);
  PT     = Shift (And (BYTESIZE (struct_pt),            IOCPARM_MASK), 16);
  EL     = Shift (And (BYTESIZE (struct_el),            IOCPARM_MASK), 16);
  MTO    = Shift (And (BYTESIZE (struct_mtop),          IOCPARM_MASK), 16);
  MTG    = Shift (And (BYTESIZE (struct_mtget),         IOCPARM_MASK), 16);
  DKO    = Shift (And (BYTESIZE (struct_dkop),          IOCPARM_MASK), 16);
  DKG    = Shift (And (BYTESIZE (struct_dkget),         IOCPARM_MASK), 16);
  DKA    = Shift (And (BYTESIZE (struct_dkacc),         IOCPARM_MASK), 16);
  DEV    = Shift (And (BYTESIZE (struct_devget),        IOCPARM_MASK), 16);
  
but instead we use these *)

  INT    = Shift (And (BYTESIZE (INTEGER),              IOCPARM_MASK), 16);
  SG     = Shift (And (0,                               IOCPARM_MASK), 16);
  TCHAR  = Shift (And (0,                               IOCPARM_MASK), 16);
  LC     = Shift (And (0,                               IOCPARM_MASK), 16);
  WS     = Shift (And (0,                               IOCPARM_MASK), 16);
  TIO    = Shift (And (0,                               IOCPARM_MASK), 16);
  IOS    = Shift (And (0,                               IOCPARM_MASK), 16);
  RTE    = Shift (And (0,                               IOCPARM_MASK), 16);
  IFR    = Shift (And (0,                               IOCPARM_MASK), 16);
  IFC    = Shift (And (0,                               IOCPARM_MASK), 16);
  CTR    = Shift (And (0,                               IOCPARM_MASK), 16);
  IFD    = Shift (And (0,                               IOCPARM_MASK), 16);
  ARP    = Shift (And (0,                               IOCPARM_MASK), 16);
  IFS    = Shift (And (0,                               IOCPARM_MASK), 16);
  SOL    = Shift (And (0,                               IOCPARM_MASK), 16);
  RES    = Shift (And (0,                               IOCPARM_MASK), 16);
  LAU    = Shift (And (0,                               IOCPARM_MASK), 16);
  LAI    = Shift (And (0,                               IOCPARM_MASK), 16);
  LTA    = Shift (And (0,                               IOCPARM_MASK), 16);
  PT     = Shift (And (0,                               IOCPARM_MASK), 16);
  EL     = Shift (And (0,                               IOCPARM_MASK), 16);
  MTO    = Shift (And (0,                               IOCPARM_MASK), 16);
  MTG    = Shift (And (0,                               IOCPARM_MASK), 16);
  DKO    = Shift (And (0,                               IOCPARM_MASK), 16);
  DKG    = Shift (And (0,                               IOCPARM_MASK), 16);
  DKA    = Shift (And (0,                               IOCPARM_MASK), 16);
  DEV    = Shift (And (0,                               IOCPARM_MASK), 16);
  
CONST (* the tty i/o controls *)
  TC = Shift (ORD ('t'), 8);

  TIOCGETD = Or (Or (R, INT), Or (TC, 0));      (* get line discipline *)
  TIOCSETD = Or (Or (W, INT), Or (TC, 1));      (* set line discipline *)
    OTTYDISC =   16_00;         (* Old, v7 std tty driver       *)
    NETLDISC =   16_01;         (* Line discipline for berk net *)
    NTTYDISC =   16_02;         (* New tty discipline           *)
    TABLDISC =   16_03;         (* Hitachi tablet discipline    *)
    NTABLDISC =  16_04;         (* Gtco tablet discipline       *)
    HCLDISC =    16_05;         (* Half cooked discipline       *)
    TERMIODISC = 16_06;         (* termio line discipline       *)
    SLPDISC =    16_07;         (* BSD Serial Line IP           *)
        (* Line disc #'s 16-23 are reserved for local extension.*)
  TIOCHPCL = Or (NOARG,       Or (TC, 2));      (* hangup on last close *)
  TIOCMODG = Or (Or (R, INT), Or (TC, 3));      (* get modem control state *)
  TIOCMODS = Or (Or (R, INT), Or (TC, 4));      (* set modem control state *)
    TIOCM_LE =  16_001;         (* line enable *)
    TIOCM_DTR = 16_002;         (* data terminal ready *)
    TIOCM_RTS = 16_004;         (* request to send *)
    TIOCM_ST  = 16_008;         (* secondary transmit *)
    TIOCM_SR =  16_010;         (* secondary receive *)
    TIOCM_CTS = 16_020;         (* clear to send *)
    TIOCM_CAR = 16_040;         (* carrier detect *)
    TIOCM_CD =  TIOCM_CAR;
    TIOCM_RNG = 16_080;         (* ring *)
    TIOCM_RI =  TIOCM_RNG;
    TIOCM_DSR = 16_100;         (* data set ready *)
  TIOCGETP =  Or (Or (R, SG),  Or (TC,  8));    (* get tty params *)
  TIOCSETP =  Or (Or (W, SG),  Or (TC,  9));    (* set tty params *)
  TIOCSETN =  Or (Or (W, SG),  Or (TC, 10));    (* no flush tty *)
  TIOCEXCL =  Or (NOARG,       Or (TC, 13));    (* set exclusive use of tty *)
  TIOCNXCL =  Or (NOARG,       Or (TC, 14));    (* reset exclus. use of tty *)
  TIOCFLUSH = Or (Or (W, INT), Or (TC, 16));    (* flush buffers *)  
  TIOCSETC =  Or (Or (W, TC),  Or (TC, 17));    (* set special chars *)
  TIOCGETC =  Or (Or (R, TC),  Or (TC, 18));    (* get special chars *)


    TANDEM = 16_0001;           (* send stopc on output q. full *)
    CBREAK = 16_0002;           (* half-cooked mode *)


  (* locals, from 127 down *)

  TIOCLBIS = Or (Or (W, INT), Or (TC, 127));    (* bis local mode bits *)
  TIOCLBIC = Or (Or (W, INT), Or (TC, 126)); (* bic local mode bits *)
  TIOCLSET = Or (Or (W, INT), Or (TC, 125)); (* set local modes *)
  TIOCLGET = Or (Or (R, INT), Or (TC, 124)); (* get local modes *)
    CRTBS =	Shift (1, 16); 	(* Do backspacing for crt	*)
    PRTERA =	Shift (1, 17); 	(* \ ... / erase		*)
    CRTERA =	Shift (1, 18); 	(* " \b " to wipe out char	*)
    TILDE =	Shift (1, 19); 	(* Hazeltine tilde kludge	*)
    MDMBUF =	Shift (1, 20); 	(* Start/stop output on c.intr. *)
    LITOUT =	Shift (1, 21); 	(* Literal output		*)
    TOSTOP =	Shift (1, 22); 	(* SIGSTOP on background output *)
    FLUSHO =	Shift (1, 23); 	(* Flush output to terminal	*)
    NOHANG =	Shift (1, 24); 	(* No SIGHUP on carrier drop	*)
    AUTOFLOW =	Shift (1, 25); 	(* IAUTO hardware start/stop	*)
    CRTKIL =	Shift (1, 26); 	(* Kill line with " \b "	*)
    PASS8 =	Shift (1, 27); 	(* Allow 8-bit with canonical   *)
    CTLECH =	Shift (1, 28); 	(* Echo control chars as ^X	*)
    PENDIN =	Shift (1, 29); 	(* tp->t_rawq needs reread	*)
    DECCTQ =	Shift (1, 30); 	(* Only ^Q starts after ^S	*)
    BNOFLSH =	Shift (1, 31); 	(* No output flush on signal	*)
    LCRTBS =    Shift (CRTBS, -16);
    LPRTERA =   Shift (PRTERA, -16);
    LCRTERA =   Shift (CRTERA, -16);
    LTILDE =    Shift (TILDE, -16);
    LMDMBUF =   Shift (MDMBUF, -16);
    LLITOUT =   Shift (LITOUT, -16);
    LTOSTOP =   Shift (TOSTOP, -16);
    LFLUSHO =   Shift (FLUSHO, -16);
    LNOHANG =   Shift (NOHANG, -16);
    LAUTOFLOW = Shift (AUTOFLOW, -16);
    LCRTKIL =   Shift (CRTKIL, -16);
    LPASS8 =    Shift (PASS8, -16);
    LCTLECH =   Shift (CTLECH, -16);
    LPENDIN =   Shift (PENDIN, -16);
    LDECCTQ =   Shift (DECCTQ, -16);
    LBNOFLSH =  Shift (BNOFLSH, -16);
  TIOCSBRK =  Or (NOARG,       Or (TC, 123)); (* set break bit *)
  TIOCCBRK =  Or (NOARG,       Or (TC, 122)); (* clear break bit *)
  TIOCSDTR =  Or (NOARG,       Or (TC, 121)); (* set data term. ready *)
  TIOCCDTR =  Or (NOARG,       Or (TC, 120)); (* clear data term. ready *)
  TIOCGPGRP = Or (Or (R, INT), Or (TC, 119)); (* get pgrp of tty *)
  TIOCSPGRP = Or (Or (W, INT), Or (TC, 118)); (* set pgrp of tty *)
  TIOCSLTC  = Or (Or (R, LC),  Or (TC, 117)); (* set loc. sp. chars *)
  TIOCGLTC  = Or (Or (R, LC),  Or (TC, 116)); (* get loc. sp. chars *)
  TIOCOUTQ =  Or (Or (R, INT), Or (TC, 115)); (* output queue size *)
  TIOCSTI =   Or (Or (W, INT), Or (TC, 114)); (* simulate term. input *)
  TIOCNOTTY = Or (NOARG,       Or (TC, 113)); (* void tty association *)
  TIOCPKT =   Or (Or (W, INT), Or (TC, 112)); (* Pty: setclr. p. mode *)
    TIOCPKT_DATA  =      16_00;     (* Data packet *)
    TIOCPKT_FLUSHREAD =  16_01;     (* Flush packet *)
    TIOCPKT_FLUSHWRITE = 16_02;     (* Flush packet *)
    TIOCPKT_STOP =       16_04;     (* Stop output *)
    TIOCPKT_START =      16_08;     (* Start output *)
    TIOCPKT_NOSTOP =     16_10;     (* No more ^S, ^Q *)
    TIOCPKT_DOSTOP =     16_20;     (* Now do ^S ^Q *)
    TIOCPKT_IOCTL =      16_40;     (* Wake up if change term char. *)
    
  TIOCSTOP =    Or (NOARG,       Or (TC, 111)); (* Stop output, like ^S *)
  TIOCSTART =   Or (NOARG,       Or (TC, 110)); (* Start out., like ^Q  *)
  TIOCMSET =    Or (Or (W, INT), Or (TC, 109)); (* Set all modem bits   *)
  TIOCMBIS =    Or (Or (W, INT), Or (TC, 108)); (* Bis modem bits       *)
  TIOCMBIC =    Or (Or (W, INT), Or (TC, 107)); (* Bic modem bits       *)
  TIOCMGET =    Or (Or (R, INT), Or (TC, 106)); (* Get all modem bits   *)
  TIOCREMOTE =  Or (Or (W, INT), Or (TC, 105)); (* Remote input editing *)
  TIOCGWINSZ =  Or (Or (R, WS),  Or (TC, 104)); (* Get win. sz. *)
  TIOCSWINSZ =  Or (Or (W, WS),  Or (TC, 103)); (* Set win. sz. *)
  TIOCUCNTL =   Or (Or (W, INT), Or (TC, 102)); (* Pty: set/clr u.c.mode*)
  TIOCSMLB =    Or (NOARG,       Or (TC, 101)); (* Turn on loopback mode*)
  TIOCCMLB =    Or (NOARG,       Or (TC, 100)); (* Turn off loop. mode  *)
  TIOCNMODEM =  Or (Or (W, INT), Or (TC, 99)); (* Ignore modem status   *)
  TIOCMODEM =   Or (Or (W, INT), Or (TC, 98)); (* Look at modem status *)
  TIOCWONLINE = Or (NOARG,       Or (TC, 97)); (* Wait on online device*)
  TIOCNCAR =    Or (NOARG,       Or (TC, 96)); (* Ignore soft carrier   *)
  TIOCCAR =     Or (NOARG,       Or (TC, 95)); (* Don't ignore s. car. *)
  TCSBRK =      Or (NOARG,       Or (TC, 94)); (* Flush q's w/ cnd. brk*)
  TCXONC =      Or (NOARG,       Or (TC, 93)); (* Start/stop control    *)
  TCFLSH =      Or (NOARG,       Or (TC, 92)); (* Cnd. q flushing       *)
  TCGETA =      Or (Or (R, TIO), Or (TC, 91)); (* Get parameters        *)
  TCSETA =      Or (Or (W, TIO), Or (TC, 90)); (* Set parameters        *)
  TCSETAW =     Or (Or (W, TIO), Or (TC, 89)); (* Drain & set           *)
  TCSETAF =     Or (Or (W, TIO), Or (TC, 88)); (* Drain, flush, & set   *)
  TIOCMASTER =  Or (Or (W, INT), Or (TC, 87)); (* master ctrls flags   *)
  TIOAUTO =     Or (NOARG,       Or (TC, 86)); (* Autoflow status       *)
  TIOCSINUSE =  FIOSINUSE;              (* Test and set mutex   *)
  TIOCCINUSE =  FIOCINUSE;              (* Clear mutex          *)
  
  TCGETP =     Or (Or (R, IOS), Or (TC, 85));    (* Get parameters      *)
  TCSANOW =    Or (Or (W, IOS), Or (TC, 84));    (* Set parameters      *)
  TCSADRAIN =  Or (Or (W, IOS), Or (TC, 83)); (* Drain & set            *)
  TCSADFLUSH = Or (Or (W, IOS), Or (TC, 82)); (* Drain, flush, & set    *)


  (* File i/o controls *)
  FC = Shift (ORD ('f'), 8);

  FIOCLEX =   Or (NOARG,       Or (FC,   1));  (* Set exclusive use on fd*)
  FIONCLEX =  Or (NOARG,       Or (FC,   2));  (* Remove exclusive use   *)
  FIOSINUSE = Or (NOARG,       Or (FC,   3));  (* Test & set IINUSE in inode *)
  FIOCINUSE = Or (NOARG,       Or (FC,   4));  (* Clear mutex            *)
  FIONREAD =  Or (Or (R, INT), Or (FC, 127)); (* Get # bytes to read    *)
  FIONBIO =   Or (Or (W, INT), Or (FC, 126)); (* Set/clear non-bl.i/o *)
  FIOASYNC =  Or (Or (W, INT), Or (FC, 125)); (* Set/clear async i/o    *)
  FIOSETOWN = Or (Or (W, INT), Or (FC, 124)); (* Set owner              *)
  FIOGETOWN = Or (Or (R, INT), Or (FC, 123)); (* Get owner              *)
  FIONBUF =   Or (Or (W, INT), Or (FC, 122)); (* N_buff i/o buf *)
  FIONONBUF = Or (NOARG,       Or (FC, 121)); (* N_buff i/o on buf      *)
  FIONBDONE = Or (Or (W, INT), Or (FC, 120)); (* N_buff i/o done buf    *)

  (* Socket i/o controls *)
  SC = Shift (ORD ('s'), 8);
  RC = Shift (ORD ('r'), 8);
  IC = Shift (ORD ('i'), 8);

  SIOCSHIWAT =     Or (Or (W, INT),  Or (SC,  0));  (* Set high watermark *)
  SIOCGHIWAT =     Or (Or (R, INT),  Or (SC,  1));  (* Get high watermark *)
  SIOCSLOWAT =     Or (Or (W, INT),  Or (SC,  2));  (* Set low watermark  *)
  SIOCGLOWAT =     Or (Or (R, INT),  Or (SC,  3));  (* Get low watermark  *)
  SIOCATMARK =     Or (Or (R, INT),  Or (SC,  7));  (* At oob mark?       *)
  SIOCSPGRP =      Or (Or (W, INT),  Or (SC,  8));  (* Set process group  *)
  SIOCGPGRP =      Or (Or (R, INT),  Or (SC,  9));  (* Get process group  *)
  SIOCADDRT =      Or (Or (W, RTE),  Or (RC, 10));  (* Add route  *)
  SIOCDELRT =      Or (Or (W, RTE),  Or (RC, 11));  (* Delete route *)
  SIOCSIFADDR =    Or (Or (W, IFR),  Or (IC, 12));  (* Set ifnet ad.*)
  SIOCGIFADDR =    Or (Or (RW, IFR), Or (IC, 13));  (*  Get ifnet ad.*)
  SIOCSIFDSTADDR = Or (Or (W, IFR),  Or (IC, 14));  (* Set p-p addr.*)
  SIOCGIFDSTADDR = Or (Or (RW, IFR), Or (IC, 15));  (* Get p-p addr.*)
  SIOCSIFFLAGS =   Or (Or (W, IFR),  Or (IC, 16));  (* Set ifnet fl.*)
  SIOCGIFFLAGS =   Or (Or (RW, IFR), Or (IC, 17));  (* Get ifnet fl.*)
  SIOCGIFBRDADDR = Or (Or (RW, IFR), Or (IC, 18));  (*  Get broad.ad.*)
  SIOCSIFBRDADDR = Or (Or (W, IFR),  Or (IC, 19));  (* Set broad.ad.*)
  SIOCGIFCONF =    Or (Or (RW, IFC), Or (IC, 20));  (*  Get ifnet ls.*)
  SIOCGIFNETMASK = Or (Or (RW, IFR), Or (IC, 21));  (*  Get n.a.mask *)
  SIOCSIFNETMASK = Or (Or (W, IFR),  Or (IC, 22));  (* Set n.a.mask *)
  SIOCSPHYSADDR =  Or (Or (RW, IFR), Or (IC, 23));  (*  Set phys. ad.*)
  SIOCADDMULTI =   Or (Or (RW, IFR), Or (IC, 24));  (*  Add m.c. ad. *)
  SIOCDELMULTI =   Or (Or (RW, IFR), Or (IC, 25));  (*  Dele. m.c.ad.*)
  SIOCRDCTRS =     Or (Or (RW, CTR), Or (IC, 26));  (*  Read if cntr.*)
  SIOCRDZCTRS =    Or (Or (RW, CTR), Or (IC, 27));  (*  Read/0 if c. *)
  SIOCRPHYSADDR =  Or (Or (RW, IFD), Or (IC, 28));  (*  Read phy. ad.*)
  SIOCSARP =       Or (Or (W, ARP),  Or (IC, 30));  (* Set arp entry *)
  SIOCGARP =       Or (Or (RW, ARP), Or (IC, 31));  (* Get arp entry *)
  SIOCDARP =       Or (Or (W, ARP),  Or (IC, 32));  (* Del. arp ent. *)
  SIOCENABLBACK =  Or (Or (W, IFR),  Or (IC, 33));  (* Set in.ex.lb. *)
  SIOCDISABLBACK = Or (Or (W, IFR),  Or (IC, 34));  (* Cl.in.ex.lpb. *)
  SIOCSTATE =      Or (Or (RW, IFS), Or (IC, 35));  (*  Device state *)
  LIOCSOL =        Or (Or (RW, SOL), Or (IC, 36));  (*  send solicit msg *)
  LIOCRES =        Or (Or (RW, RES), Or (IC, 37));  (*  get response msg *)
  LIOCCMD =        Or (Or (RW, LAU), Or (IC, 38));  (*  send command msg *)
  LIOCINI =        Or (Or (RW, LAI), Or (IC, 39));  (*  lat tty init *)
  SIOCARPREQ =     Or (Or (RW, IFR), Or (IC, 40));  (*  arp request pkt *)
  SIOCGIFMETRIC =  Or (Or (RW, IFR), Or (IC, 41));  (* get IF metric *)
  SIOCSIFMETRIC =  Or (Or (W, IFR),  Or (IC, 42));  (* set IF metric *)
  LIOCTTYI =       Or (Or (R, LTA),  Or (IC, 43));  (* lat tty info *)


  (* Disk partition table i/o controls *)
  PC = Shift (ORD ('p'), 8);

  DIOCGETPT = Or (Or (R, PT), Or (PC, 1));   (* Get disk paritition  *)
  DIOCSETPT = Or (Or (W, PT), Or (PC, 2));   (* Set disk paritition  *)
  DIOCDGTPT = Or (Or (R, PT), Or (PC, 3));   (* Get default disk par. *)

  (* Error logging i/o controls *)
  EC = Shift (ORD ('e'), 8);

  ELSETPID =  Or (Or (RW, EL),  Or (EC, 0));  (* Set proc. id. *)
  ELGETPID =  Or (Or (R, INT),  Or (EC, 1));  (* Get proc. id. *)
  ELMOVPTR =  Or (Or (W, INT),  Or (EC, 2));  (* Update elpts. *)
  ELREINIT =  Or (NOARG,        Or (EC, 3));  (* Reinit elbuf  *)
  ELCLRPID =  Or (NOARG,        Or (EC, 4));  (* Clr. proc.id. *)
  ELWARNOFF = Or (NOARG,        Or (EC, 5));  (* disable warn. *)
  ELWARNON =  Or (NOARG,        Or (EC, 6));  (* disable warn. *)
  ELGETTIME = Or (Or (R, INT),  Or (EC, 7));  (* Get strt time *)

  (* Tape i/o controls *)
  MC = Shift (ORD ('m'), 8);

  MTIOCTOP = Or (Or (W, MTO), Or (MC, 1));              (* Do a tape op. *)
  MTIOCGET = Or (Or (R, MTG), Or (MC, 2));              (* Get status   *)

  (* Disk i/o controls *)
  DC = Shift (ORD ('d'), 8);

  DKIOCHDR = Or (NOARG,        Or (DC, 1));             (* Header r/w   *)
  DKIOCDOP = Or (Or (W, DKO),  Or (DC, 2));             (* Do a disk op. *)
  DKIOCGET = Or (Or (R, DKG),  Or (DC, 3));             (* Get status   *)
  DKIOCACC = Or (Or (RW, DKA), Or (DC, 4));             (* Disk access  *)

  (* Generic device information i/o controls *)
  VC = Shift (ORD ('v'), 8);

  DEVIOCGET = Or (Or (R, DEV), Or (VC, 1));             (* Get dev.info. *)



CONST
  R_OK = 8_4;
  W_OK = 8_2;
  X_OK = 8_1;
  F_OK = 8_0;

(* Somebody will have to work really hard to get all those ioctl
   parameters right. Beware when using them! *)
<*EXTERNAL *> PROCEDURE ioctl (d: int; request: u_long; 
                                         argp: ADDRESS): int;
(* ok *)

(*** link - link to a file ***)
<*EXTERNAL*> PROCEDURE link (name1, name2: char_star): int;
(* ok *)

(*** lseek, tell - move read/write pointer ***)
CONST (* whence *)
  L_SET  = 0;
  L_INCR = 1;
  L_XTND = 2;

<*EXTERNAL*> PROCEDURE lseek (d: int; offset: off_t; whence: int): off_t;
(* ok *)

(*** mkdir - make a directory file ***)
<*EXTERNAL*> PROCEDURE mkdir (path: char_star; mode: mode_t): int;
(* ok *)

(*** mknod - make a directory or a special file ***)
CONST (* mode *)
  fifo_special =               8_010000;
  character_special =          8_020000;
  directory =                  8_040000;
  block_special =              8_060000;
  ordinary_file =              8_000000;
  ordinary_filea =             8_100000;
  regular_file =               8_100000;
  symbolic_link =              8_120000;
  socket =                     8_140000;
  set_uid_on_exec =            8_004000;
  set_gid_on_exec =            8_002000;
  save_text_image_after_exec = 8_001000;

  (* lower bits used for the access permissions *)

<*EXTERNAL*> PROCEDURE mknod (path: char_star; mode: mode_t; dev: dev_t): int;
(* ok *)

(*** mount, umount - mount or unmount a file system ***)
CONST (* rwflag *)
  writable = 0;
  write_protected = 1;

<*EXTERNAL*> PROCEDURE mount (type: int;
                              dir: char_star; flags: int; 
                              data: ADDRESS): int;
(* ok *)

<*EXTERNAL*> PROCEDURE unmount (dir: char_star; flags: int): int;
(* ok *)


(*** open - open for reading or writing ***)
CONST (* flags *)
  O_RDONLY =    8_0;            (* open for reading *)
  O_WRONLY =    8_1;            (* open for writing *)
  O_RDWR   =    8_2;            (* open for read & write *)
  O_CREAT  =    FCREAT;         (* open with file create *)
  O_EXCL   =    FEXCL;          (* error on create if file exists *)
  O_NOCTTY =    8_000;
  O_TRUNC  =    FTRUNC;         (* open with truncation *)
  O_APPEND =    FAPPEND;        (* append on each write *)
  O_NONBLOCK =  FNBLOCK;        (* POSIX non-blocking I/O *)
  O_NDELAY =    FNDELAY;        (* non-blocking open *)
  O_FSYNC =     FSYNCRON;       (* syncronous write *)

  M3_NONBLOCK = O_NONBLOCK;  (* -1 => would block, 0 => EOF *)

<*EXTERNAL "uopen" *> PROCEDURE open (name: char_star; 
                                       flags, mode: int): int;
(* ok *)

(*** pipe - create an interprocess channel ***)
CONST
  readEnd = 0;
  writeEnd = 1;
<*EXTERNAL*> PROCEDURE pipe (VAR fildes: ARRAY [0..1] OF int): int;
(* ok *)

(* not implemented
(*** plock - lock process, text, or data in memory ***)
CONST (* op *)
  UNLOCK =   0;         (* unlock all segments *)
  PROCLOCK = 1;         (* lock text and data into memory *)
  TXTLOCK =  2;         (* lock text segment only *)
  DATLOCK =  4;         (* lock data segment ony *)
<*EXTERNAL*> PROCEDURE plock (op: int): int;
*)

(*** profil - execution time profile ***)
<*EXTERNAL*> PROCEDURE profil (buff: ADDRESS; 
                               size, offset, scale: int): int;
(* ok *)

(*** ptrace - process trace ***)
<*EXTERNAL*> PROCEDURE ptrace (request: int; pid: pid_t;
                               addr: ADDRESS;
                               data: int): int;
(* ok *)

(*** readlink - read value of a symbolic link ***)
<*EXTERNAL*> PROCEDURE readlink (path: char_star; buf: ADDRESS; bufsize: int): int;
(* ok *)

(*** reboot - reboot system or halt processor ***)
CONST (* howto *)
  RB_HALT =    16_8;            (* dont' reboot, just halt *)
  RB_ASKNAME = 16_1;            (* ask for file name to reboot from *)
  RB_SINGLE =  16_2;            (* reboot to single user only *)
  RB_AUTOREBOOT = 16_0;         (* flag for system auto-booting itself *)

<*EXTERNAL*> PROCEDURE reboot (howto: int): int;
(* ok *)

(*** rename - change the name of a file ***)
<*EXTERNAL*> PROCEDURE rename (from, to: char_star): int;
(* ok *)

(*** rmdir - remove a directory file ***)
<*EXTERNAL*> PROCEDURE rmdir (path: char_star): int;
(* ok *)

(*** select - synchronous I/O mutiplexing ***)
CONST
  MAX_FDSET = 256;

TYPE
  FDSet = SET OF [0 .. MAX_FDSET - 1];

<*EXTERNAL*> PROCEDURE select (nfds: int;
                           readfds, writefds, exceptfds: UNTRACED REF FDSet;
                           timeout: UNTRACED REF struct_timeval): int;
(* ok *)

(*** setgroups - set group access list ***)
<*EXTERNAL*> PROCEDURE setgroups (ngroups: int; VAR gidset: int): int;
(* ok *)

(* not implemented
(*** setquota - enable/disable quotas on a file system ***)
<*EXTERNAL*> PROCEDURE setquota (special, file: char_star): int;
*)

(*** shutdown - shut down full-duplex connection ***)
<*EXTERNAL*> PROCEDURE shutdown (s, how: int): int;
(* ok *)

(*** swapon - add a swap device for interleaved paging/swapping ***)
<*EXTERNAL*> PROCEDURE swapon (special: char_star): int;
(* ok *)

(*** symlink - make symbolic link to a file ***)
<*EXTERNAL*> PROCEDURE symlink (name1, name2: char_star): int;
(* ok *)

(*** sync - update super-block ***)
<*EXTERNAL*> PROCEDURE sync ();
(* ok *)

(*** truncate, ftruncate - truncate a file to a specified length ***)
<*EXTERNAL*> PROCEDURE truncate (path: char_star; length: off_t): int;
<*EXTERNAL*> PROCEDURE ftruncate (fd, length: off_t): int;
(* ok *)

(* not implemented 
(*** ulimit - get and set user limits ***)
<*EXTERNAL*> PROCEDURE ulimit (cmd: int; newlimit: long): long;
*)

(*** umask - set file creation mask ***)
<*EXTERNAL*> PROCEDURE umask (numask: mode_t): mode_t;
(* ok *)

(*** unlink - remove directory entry ***)
<*EXTERNAL*> PROCEDURE unlink (path: char_star): int;
(* ok *)

(*** utimes - set file times ***)
<*EXTERNAL*> PROCEDURE utimes (file: char_star;
                    tvp: UNTRACED REF ARRAY [0..1] OF struct_timeval): int;
(* ok *)

(*** vfork - spawn new process in a virtual memory efficient way ***)
<*EXTERNAL*> PROCEDURE vfork (): int;
(* ok *)

(* not implemented, obsolete
(*** vhangup - virtually hang up the current control terminal ***)
<*EXTERNAL*> PROCEDURE vhangup (): int;
*)

(* not implemented
(*** rexec(3x) - return stream to a remote command ***)
<*EXTERNAL*> PROCEDURE rexec (VAR ahost: char_star; 
                              inport: u_short;
                              user, passwd, cmd: char_star;
                              fd2p: int_star): int;
*)

(*** isatty(3) ***)
<*EXTERNAL*> PROCEDURE isatty (filedes: int): int;
(* ok *)

(*** system(3) ***)
<*EXTERNAL*> PROCEDURE system (string: char_star): int;
(* ok *)


END Unix.
