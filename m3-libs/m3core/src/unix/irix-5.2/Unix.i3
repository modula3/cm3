(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Fri Oct 28 09:54:38 PDT 1994 by kalsow         *)
(*      modified on Wed Oct 12 11:55:34 PDT 1994 by ericv          *)
(*      modified on Thu Nov 12 11:50:14 PST 1992 by muller         *)

INTERFACE Unix;

FROM Word IMPORT Or, And, Shift;

FROM Ctypes IMPORT short, int, long, char_star, char_star_star, int_star;
FROM Utypes IMPORT u_short, off_t, size_t, pid_t, gid_t;
FROM Utime IMPORT struct_timeval;

(*** <unistd.h> ***)

(* compile-time symbolic constants,
** Support does not mean the feature is enabled.
** Use pathconf/sysconf to obtain actual configuration value.
** 
*)
CONST
  POSIX_JOB_CONTROL	 = 1;
  POSIX_SAVED_IDS	 = 1;
  POSIX_REALTIME_SIGNALS	 = 1;
  POSIX_ASYNCHRONOUS_IO	 = 1;
  POSIX_PRIORITIZED_IO	 = 1;

  POSIX_VERSION	 = 199009;

  POSIX_VDISABLE  = 0; (* Disable special character functions *)

  XOPEN_VERSION  = 3;

(* command names for POSIX sysconf *)

  SC_ARG_MAX              = 1;
  SC_CHILD_MAX            = 2;
  SC_CLK_TCK              = 3;
  SC_NGROUPS_MAX          = 4;
  SC_OPEN_MAX             = 5;
  SC_JOB_CONTROL          = 6;
  SC_SAVED_IDS            = 7;
  SC_VERSION              = 8;
  SC_PASS_MAX		 = 9;
  SC_LOGNAME_MAX		 = 10;
  SC_PAGESIZE		 = 11;
  SC_XOPEN_VERSION 	 = 12;
  SC_NACLS_MAX    = 13;
  SC_NPROC_CONF   = 14;
  SC_NPROC_ONLN   = 15;
  SC_STREAM_MAX   = 16;
  SC_TZNAME_MAX   = 17;
  SC_AIO_MAX		 = 18;
  SC_AIO_LISTIO_MAX	 = 19;	
  SC_RTSIG_MAX		 = 20;
  SC_SIGQUEUE_MAX	 = 21;
  SC_ASYNCHRONOUS_IO	 = 22;
  SC_REALTIME_SIGNALS	 = 23;
  SC_PRIORITIZED_IO	 = 24;

(* command names for POSIX pathconf *)

  PC_LINK_MAX             = 1;
  PC_MAX_CANON            = 2;
  PC_MAX_INPUT            = 3;
  PC_NAME_MAX             = 4;
  PC_PATH_MAX             = 5;
  PC_PIPE_BUF             = 6;
  PC_CHOWN_RESTRICTED     = 7;
  PC_NO_TRUNC             = 8;
  PC_VDISABLE             = 9;


(*** sysconf(3c) - get configurable system variables ***)
<*EXTERNAL*> PROCEDURE sysconf (name: int): long;

(*** pathconf(2) - get configurable pathname variables ***)
<*EXTERNAL*> PROCEDURE fpathconf (fildes: int; name: int): long;
<*EXTERNAL*> PROCEDURE pathconf (path: char_star; name: int): long;


CONST
  GF_PATH	 = "/etc/group";	(* Path name of the "group" file *)
  PF_PATH	 = "/etc/passwd";	(* Path name of the "passwd" file *)

  STDIN_FILENO	 = 0;
  STDOUT_FILENO	 = 1;
  STDERR_FILENO	 = 2;

(*** <dirent.h> ***)

CONST
  MaxNameLen = 255;
  MaxPathLen = 4096;

(*** <sys/file.h> ***)

(* flags *)
CONST
  FMASK		 = 16_90FF;

  FOPEN		 = 16_FFFFFFFF;
  FREAD		 = 16_01;
  FWRITE	 = 16_02;

  FNDELAY	 = 16_04;
  FAPPEND	 = 16_08;
  FSYNC		 = 16_10;
  FNONBLOCK	 = 16_80;
  FASYNC	 = 16_1000;
  FNONBLK	 = FNONBLOCK;
  FNBLOCK        = FNONBLOCK;        (* POSIX no delay *)
  FDIRECT	 = 16_8000;

(* open-only modes *)
  FCREAT	 = 16_0100;
  FTRUNC	 = 16_0200;
  FEXCL		 = 16_0400;
  FNOCTTY	 = 16_0800;


(*** <unistd.h> ***)

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

(* MODE MASKS *)

(* de facto standard definitions *)

  S_IFMT	 = 16_F000;	(* type of file *)
  S_IAMB	 = 16_1FF;	(* access mode bits *)
  S_IFIFO	 = 16_1000;	(* fifo *)
  S_IFCHR	 = 16_2000;	(* character special *)
  S_IFDIR	 = 16_4000;	(* directory *)
  S_IFNAM	 = 16_5000;  (* XENIX special named file *)
  S_INSEM  = 16_1;	(* XENIX semaphore subtype of IFNAM *)
  S_INSHD  = 16_2;	(* XENIX shared data subtype of IFNAM *)

  S_IFBLK	 = 16_6000;	(* block special *)
  S_IFREG	 = 16_8000;	(* regular *)
  S_IFLNK	 = 16_A000;	(* symbolic link *)
  S_IFSOCK	 = 16_C000;	(* socket *)

  S_ISUID	 = 16_800;	(* set user id on execution *)
  S_ISGID	 = 16_400;	(* set group id on execution *)

  S_ISVTX	 = 16_200;	(* save swapped text even after use *)

  S_IREAD	 = 8_0400;	(* read permission, owner *)
  S_IWRITE	 = 8_0200;	(* write permission, owner *)
  S_IEXEC	 = 8_0100;	(* execute/search permission, owner *)
  S_ENFMT	 = S_ISGID;	(* record locking enforcement flag *)

(* the following macros are for POSIX conformance *)

  S_IRWXU	 = 8_0700;		(* read, write, execute: owner *)
  S_IRUSR	 = 8_0400;		(* read permission: owner *)
  S_IWUSR	 = 8_0200;		(* write permission: owner *)
  S_IXUSR	 = 8_0100;		(* execute permission: owner *)
  S_IRWXG	 = 8_0070;		(* read, write, execute: group *)
  S_IRGRP	 = 8_0040;		(* read permission: group *)
  S_IWGRP	 = 8_0020;		(* write permission: group *)
  S_IXGRP	 = 8_0010;		(* execute permission: group *)
  S_IRWXO	 = 8_0007;		(* read, write, execute: other *)
  S_IROTH	 = 8_0004;		(* read permission: other *)
  S_IWOTH	 = 8_0002;		(* write permission: other *)
  S_IXOTH	 = 8_0001;		(* execute permission: other *)

(*** <unistd.h> ***)

(* Symbolic constants for the "access" routine: *)
  R_OK	 = 8_04;	(* Test for Read permission *)
  W_OK	 = 8_02;	(* Test for Write permission *)
  X_OK	 = 8_01;	(* Test for eXecute permission *)
  F_OK	 = 8_00;	(* Test for existence of File *)
  EFF_ONLY_OK 	 = 8_10;	(* Test using effective ids *)
  EX_OK		 = 8_20;	(* Test for Regular, executable file *)

(*** access - determine the accessibility of file ***)
<*EXTERNAL*> PROCEDURE access (path: char_star; mod: int): int;

(*** acct - turn accounting on or off ***)
<*EXTERNAL*> PROCEDURE acct (path: char_star): int;

(*** brk, sbrk - change data segment space allocation *)
<*EXTERNAL*> PROCEDURE brk (addr: ADDRESS): int;
<*EXTERNAL*> PROCEDURE sbrk (inc: int): int;

(*** chdir - change working directory ***)
<*EXTERNAL*> PROCEDURE chdir (path: char_star): int;

(*** chmod, fchmod - change mde of file ***)
<*EXTERNAL*> PROCEDURE chmod (path: char_star; mode: int): int;
<*EXTERNAL*> PROCEDURE fchmod (fd, mode: int): int;

(*** chown, fchown - change owner and group of a file ***)
<*EXTERNAL*> PROCEDURE chown (path: char_star; owner, group: int): int;
<*EXTERNAL*> PROCEDURE fchown (fd, owner, group: int): int;

(*** chroot - change root directory ***)
<*EXTERNAL*> PROCEDURE chroot (dirname: char_star): int;

(*** close - delete a descriptor ***)
<*EXTERNAL*> PROCEDURE close (d: int): int;

(*** creat - create a new file ***)
<*EXTERNAL*> PROCEDURE creat (name: char_star; mode: int): int;

(*** dup, dup2 - duplicate an open file descriptor ***)
<*EXTERNAL*> PROCEDURE dup (oldd: int): int;
<*EXTERNAL*> PROCEDURE dup2 (oldd, newd: int): int;

(*** execve - execute a file ***)
<*EXTERNAL*> PROCEDURE execve (name: char_star; 
                           argv, envp: char_star_star): int;

(*** exit - terminate a process ***)
<*EXTERNAL*> PROCEDURE exit (i: int);

(*** _exit - terminate a process without performing C I/O library cleanup ***)
<*EXTERNAL "_exit"*> PROCEDURE underscore_exit (i: int);


(*** <sys/fcntl.h> ***)

(*
 * Flag values accessible to open(2) and fcntl(2)
 * (the first three and O_DIRECT can only be set by open).
 *)
CONST
  O_RDONLY     = 0;
  O_WRONLY     = 1;
  O_RDWR       = 2;
  O_NDELAY     = 16_04;	(* non-blocking I/O *)
  O_APPEND     = 16_08;	(* append (writes guaranteed at the end) *)
  O_SYNC       = 16_10;	(* synchronous write option *)
  O_NONBLOCK   = 16_80;	(* non-blocking I/O (POSIX) *)
  O_DIRECT     = 16_8000;	(* direct I/O *)

  M3_NONBLOCK = O_NONBLOCK; (* -1 => would block, 0 => EOF *)

(*
 * Flag values accessible only to open(2).
 *)
  O_CREAT      = 16_100;	(* open with file create (uses third open arg) *)
  O_TRUNC      = 16_200;	(* open with truncation *)
  O_EXCL       = 16_400;	(* exclusive open *)
  O_NOCTTY     = 16_800;	(* don't allocate controlling tty (POSIX) *)

(* fcntl(2) requests *)
  F_DUPFD      = 0;	(* Duplicate fildes *)
  F_GETFD      = 1;	(* Get fildes flags *)
  F_SETFD      = 2;	(* Set fildes flags *)
  F_GETFL      = 3;	(* Get file flags *)
  F_SETFL      = 4;	(* Set file flags *)

  F_SETLK      = 6;	(* Set file lock *)
  F_SETLKW     = 7;	(* Set file lock and wait *)

  F_CHKFL      = 8;	(* Unused *)
  F_ALLOCSP    = 10;	(* Reserved *)
  F_FREESP     = 11;	(* Free file space *)

  F_SETBSDLK   = 12;	(* Set Berkeley record lock *)
  F_SETBSDLKW  = 13;	(* Set Berkeley record lock and wait *)
  F_GETLK      = 14;	(* Get file lock *)

  F_DIOINFO    = 30;	(* get direct I/O parameters *)

  F_RSETLK     = 20;	(* Remote SETLK for NFS *)
  F_RGETLK     = 21;	(* Remote GETLK for NFS *)
  F_RSETLKW    = 22;	(* Remote SETLKW for NFS *)

(* only for sockets *)
  F_GETOWN     = 23;	(* Get owner (socket emulation) *)
  F_SETOWN     = 24;	(* Set owner (socket emulation) *)

(*
 * File segment locking types.
 *)
  F_RDLCK      = 1;	(* Read lock *)
  F_WRLCK      = 2;	(* Write lock *)
  F_UNLCK      = 3;	(* Remove lock(s) *)

(*
 * POSIX constants 
 *)

  O_ACCMODE    = 3;	(* Mask for file access modes *)
  FD_CLOEXEC   = 1;	(* close on exec flag *)

TYPE
  flock_t = struct_flock;
  struct_flock = RECORD
	l_type    : short;
	l_whence  : short;
	l_start   : off_t := 0;
	l_len     : off_t := 0;		(* len == 0 means until end of file *)
        l_sysid   : long  := 0;
        l_pid     : pid_t := 0;
	pad       := ARRAY [0..3] OF long { 0, .. };	(* reserve area *)
  END;

<*EXTERNAL*> PROCEDURE fcntl (fd, request, arg: int): int;

(*** flock - apply or remove an advisory lock on an open file ***)
CONST
  LOCK_SH = 1;   (* shared lock *)
  LOCK_EX = 2;   (* exclusive lock *)
  LOCK_NB = 4;   (* don't block when locking *)
  LOCK_UN = 8;   (* unlock *)

<*EXTERNAL*> PROCEDURE flock (fd, operation: int): int;

(*** <unistd.h> -- system V file locking ***)

CONST
  F_ULOCK  = 0;	(* Unlock a previously locked region *)
  F_LOCK   = 1;	(* Lock a region for exclusive use *)
  F_TLOCK  = 2;	(* Test and lock a region for exclusive use *)
  F_TEST   = 3;	(* Test a region for other processes locks *)

<*EXTERNAL*> PROCEDURE lockf (fildes, function: int; size: long): int;


(*** fork - create a new process ***)
<*EXTERNAL*> PROCEDURE fork (): int;

(*** fsync - synchronize a file's in-core state with that on disk ***)
<*EXTERNAL*> PROCEDURE fsync (fd: int): int;

(*** getdomainname, setdomainname - get or set name of current domain ***)
<*EXTERNAL*> PROCEDURE getdomainname (name: char_star; namelen: int): int;
<*EXTERNAL*> PROCEDURE setdomainname (name: char_star; namelen: int): int;

(*** getdtablesize - get descriptor table size ***)
<*EXTERNAL*> PROCEDURE getdtablesize (): int;

(*** getgroups - get group access list ***)
<*EXTERNAL*> PROCEDURE getgroups (setlen: int; VAR gidset: gid_t): int;

(*** gethostid, sethostid - get/set unique identifier of current host ***)
<*EXTERNAL*> PROCEDURE gethostid (): long;
<*EXTERNAL*> PROCEDURE sethostid (hostid: long): int;

(*** gethostname, sethostname - get/set name of current host ***)
<*EXTERNAL*> PROCEDURE gethostname (name: char_star; namelen: int): int;
<*EXTERNAL*> PROCEDURE sethostname (name: char_star; namelen: int): int;

(*** getpagesize - get system page size ***)
<*EXTERNAL*> PROCEDURE getpagesize (): int;

(*** getwd - get current working directory pathname ***)
<*EXTERNAL*> PROCEDURE getwd (pathname: char_star): char_star;


(*** <sys/ioccom.h> ***)

CONST
  IOCPARM_MASK = 16_ff;                 (* 256 bytes at most in parameters *)
  IOC_VOID  = Shift (1, 29);       (* no parameters *)
  IOC_OUT   = Shift (1, 30);       (* copy out parameters *)
  IOC_IN    = Shift (1, 31);       (* copy in parameters *)
  IOC_INOUT = Or (IOC_IN, IOC_OUT);


  NOARG  = IOC_VOID;
  R      = IOC_OUT;
  W      = IOC_IN;
  RW     = IOC_INOUT;

(***************

/* the 0x20000000 is so we can distinguish new ioctl's from old */
#define	_IOC(f,n,x,y)	((int)((f)|(((n)&IOCPARM_MASK)<<16)|((x)<<8)|(y)))
#define	_IO(x,y)	_IOC(IOC_VOID, 0, x, y)
#define	_IOR(x,y,t)	_IOC(IOC_OUT, sizeof(t), x, y)
#define	_IORN(x,y,n)	_IOC(IOC_OUT, n, x, y)
#define	_IOW(x,y,t)	_IOC(IOC_IN, sizeof(t), x, y)
#define	_IOWN(x,y,n)	_IOC(IOC_IN, n, x, y)
/* this should be _IORW, but stdio got there first */
#define	_IOWR(x,y,t)	_IOC(IOC_INOUT, sizeof(t), x, y)

*****************)

(*** <sys/termios.h> ***)

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


(*** <sys/filio.h> ***)

  (* File i/o controls *)
  FC = Shift (ORD ('f'), 8);

  FIOCLEX =   Or (NOARG,       Or (FC,   1));  (* Set exclusive use on fd*)
  FIONCLEX =  Or (NOARG,       Or (FC,   2));  (* Remove exclusive use   *)
  FIONREAD =  Or (Or (R, INT), Or (FC, 127)); (* Get # bytes to read    *)
  FIONBIO =   Or (Or (W, INT), Or (FC, 126)); (* Set/clear non-bl.i/o *)
  FIOASYNC =  Or (Or (W, INT), Or (FC, 125)); (* Set/clear async i/o    *)
  FIOSETOWN = Or (Or (W, INT), Or (FC, 124)); (* Set owner              *)
  FIOGETOWN = Or (Or (R, INT), Or (FC, 123)); (* Get owner              *)

(*** <net/soioctl.h> ***)

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
  SIOCGIFCONF =    Or (Or (RW, IFC), Or (IC, 20));  (*  Get ifnet ls.*)
  SIOCGENADDR =    Or (Or (RW, IFR), Or (IC, 85));  (*  Get ifnet ls.*)

  SIOCRPHYSADDR = SIOCGENADDR; (* read physical address *)


(*** <sys/termio.h> ***)
(*
 * Terminal types
 *)
CONST
  TERM_NONE  = 0;	(* tty *)
  TERM_TEC   = 1;	(* TEC Scope *)
  TERM_V61   = 2;	(* DEC VT61 *)
  TERM_V10   = 3;	(* DEC VT100 *)
  TERM_TEX   = 4;	(* Tektronix 4023 *)
  TERM_D40   = 5;	(* TTY Mod 40/1 *)
  TERM_H45   = 6;	(* Hewlitt-Packard 45 *)
  TERM_D42   = 7;	(* TTY Mod 40/2B *)

(*
 * Terminal flags
 *)
  TM_NONE    = 8_000;	(* use default flags *)
  TM_SNL     = 8_001;	(* special newline flag *)
  TM_ANL     = 8_002;	(* auto newline on column 80 *)
  TM_LCF     = 8_004;	(* last col of last row special *)
  TM_CECHO   = 8_010;	(* echo terminal cursor control *)
  TM_CINVIS  = 8_020;	(* do not send esc seq to user *)
  TM_SET     = 8_200;	(* must be on to set/res flags *)


<*EXTERNAL*> PROCEDURE ioctl (d, request: int; argp: ADDRESS): int;

(*** link - link to a file ***)

<*EXTERNAL*> PROCEDURE link (name1, name2: char_star): int;


(*** lseek, tell - move read/write pointer ***)

CONST (* whence *)
  SEEK_SET	 = 0;	(* Set file pointer to "offset" *)
  SEEK_CUR	 = 1;	(* Set file pointer to current plus "offset" *)
  SEEK_END	 = 2;	(* Set file pointer to EOF plus "offset" *)
  L_SET  = 0;
  L_INCR = 1;
  L_XTND = 2;

<*EXTERNAL*> PROCEDURE lseek (d: int; offset: off_t; whence: int): int;

<*EXTERNAL*> PROCEDURE tell (d: int): int;


(*** mkdir - make a directory file ***)

<*EXTERNAL*> PROCEDURE mkdir (path: char_star; mode: int): int;


(*** mknod - make a directory or a special file ***)

CONST (* mode *)
  fifo_special =               S_IFIFO;
  character_special =          S_IFCHR;
  directory =                  S_IFDIR;
  block_special =              S_IFBLK;
  ordinary_file =              S_IFREG;
  ordinary_filea =             S_IFREG;
  set_uid_on_exec =            S_ISUID;
  set_gid_on_exec =            S_ISGID;
  save_text_image_after_exec = S_ISVTX;

  (* lower bits used for the access permissions *)

<*EXTERNAL*> PROCEDURE mknod (path: char_star; mode, dev: int): int;


(*** mount, umount - mount or unmount a file system ***)

<*EXTERNAL*> PROCEDURE mount (spec, dir: char_star; mflag: int;
                 fstyp, dataptr: char_star; datalen: int): int;

<*EXTERNAL*> PROCEDURE umount (file: char_star): int;


(*** open - open for reading or writing ***)

<*EXTERNAL*> PROCEDURE open (name: char_star; flags, mode: int): int;


(*** pipe - create an interprocess channel ***)
CONST
  readEnd = 0;
  writeEnd = 1;
<*EXTERNAL*> PROCEDURE pipe (VAR fildes: ARRAY [0..1] OF int): int;


(*** plock - lock process, text, or data in memory ***)

(*
 * flags for locking procs and texts and pages
 *)
CONST (* op *)
  UNLOCK =   0;         (* unlock all segments *)
  PROCLOCK = 1;         (* lock text and data into memory *)
  TXTLOCK =  2;         (* lock text segment only *)
  DATLOCK =  4;         (* lock data segment ony *)
  PGLOCK  =  8;

<*EXTERNAL*> PROCEDURE plock (op: int): int;


(*** profil - execution time profile ***)

<*EXTERNAL*> PROCEDURE profil (buff: ADDRESS; 
                           size, offset, scale: int): int;


(*** ptrace - process trace ***)

<*EXTERNAL*> PROCEDURE ptrace (request: int; pid: pid_t;
                           addr: ADDRESS;
                           data: int): int;


(*** readlink - read value of a symbolic link ***)

<*EXTERNAL*> PROCEDURE readlink (path: char_star; buf: ADDRESS; bufsize: size_t): int;

(*** rename - change the name of a file ***)

<*EXTERNAL*> PROCEDURE rename (from, to: char_star): int;

(*** rmdir - remove a directory file ***)

<*EXTERNAL*> PROCEDURE rmdir (path: char_star): int;

(*** select - synchronous I/O mutiplexing ***)

CONST
  MAX_FDSET = 1024;

TYPE
  FDSet = SET OF [0 .. MAX_FDSET - 1];

<*EXTERNAL*> PROCEDURE select (nfds: int;
                           readfds, writefds, exceptfds: UNTRACED REF FDSet;
                           timeout: UNTRACED REF struct_timeval): int;

(*** setgroups - set group access list ***)

<*EXTERNAL*> PROCEDURE setgroups (ngroups: int; VAR gidset: gid_t): int;

(*** symlink - make symbolic link to a file ***)

<*EXTERNAL*> PROCEDURE symlink (name1, name2: char_star): int;


(*** sync - update super-block ***)

<*EXTERNAL*> PROCEDURE sync (): int;

(*** truncate, ftruncate - truncate a file to a specified length ***)

<*EXTERNAL*> PROCEDURE truncate (path: char_star; length: off_t): int;

<*EXTERNAL*> PROCEDURE ftruncate (fd, length: off_t): int;


(*** ulimit - get and set user limits ***)

(*
 * The following are codes which can be
 * passed to the ulimit system call. (Xenix compatible.)
 *)
CONST
  UL_GFILLIM           = 1;	(* get file limit *)
  UL_SFILLIM           = 2;	(* set file limit *)
  UL_GMEMLIM           = 3;	(* get process size limit *)
  UL_GDESLIM           = 4;	(* get file descriptor limit *)
  UL_GTXTOFF           = 64;	(* get text offset *)

(*
 * The following are symbolic constants required for
 * X/Open Conformance.   They are the equivalents of
 * the constants above.
 *)

  UL_GETFSIZE           = UL_GFILLIM;	(* get file limit *)
  UL_SETFSIZE           = UL_SFILLIM;	(* set file limit *)

<*EXTERNAL*> PROCEDURE ulimit (cmd: int; newlimit: long): long;

(*** umask - set file creation mask ***)

<*EXTERNAL*> PROCEDURE umask (numask: int): int;

(*** unlink - remove directory entry ***)

<*EXTERNAL*> PROCEDURE unlink (path: char_star): int;

(*** utimes - set file times ***)

<*EXTERNAL*> PROCEDURE utimes (file: char_star;
                    tvp: UNTRACED REF ARRAY [0..1] OF struct_timeval): int;


(*** vfork - spawn new process in a virtual memory efficient way ***)

<*EXTERNAL fork*> PROCEDURE vfork (): int;

(*** vhangup - virtually hang up the current control terminal ***)

<*EXTERNAL*> PROCEDURE vhangup (): int;

(*** rexec(3x) - return stream to a remote command ***)

<*EXTERNAL*> PROCEDURE rexec (VAR ahost: char_star; 
                              inport: u_short;
                              user, passwd, cmd: char_star;
                              fd2p: int_star): int;

(*** isatty(3) ***)
<*EXTERNAL*> PROCEDURE isatty (filedes: int): int;

(*** system(3) ***)
<*EXTERNAL*> PROCEDURE system (string: char_star): int;

(*** end(3) & co ***)
<*EXTERNAL*> VAR end: int;
<*EXTERNAL*> VAR etext: int;
<*EXTERNAL*> VAR edata: int;
<*EXTERNAL*> VAR eprol: int;

END Unix.
