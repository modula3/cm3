(* Copyright (C) 1993, Digital Equipment Corporation                  *)
(* All rights reserved.                                               *)
(* See the file COPYRIGHT for a full description.                     *)
(*                                                                    *)
(* Last modified on Fri Feb 24 15:05:21 PST 1995 by kalsow            *)
(*      modified on Tue Feb 14 20:34:10 GMT 1995 by rrw1000@cam.ac.uk *)
(*      modified on Tue Jun  8 16:26:41 PDT 1993 by mcjones           *)
(*      modified on Mon Jan 11 14:34:49 PST 1993 by muller            *)

(* $Id: Unix.i3,v 1.6 2008-02-16 19:45:21 jkrell Exp $ *)

INTERFACE Unix;

FROM Word IMPORT Or, And, Shift;

FROM Ctypes IMPORT short, int, long, const_char_star, char_star, char_star_star;
FROM Utypes IMPORT off_t, size_t, uid_t, gid_t;
FROM Utime IMPORT struct_timeval;

TYPE
  ptrdiff_t = INTEGER;

CONST
  MaxPathLen = 1024;

(*** file flags ***)
(* I'm not sure about these - take this with a pinch of salt -rrw *)

CONST
  FREAD =      8_000001;        (* descriptor read/receive'able *)
  FWRITE =     8_000002;        (* descriptor write/send'ale *)
  FCREAT =     8_000100;        (* create if nonexistant *)
  FEXCL =      8_000200;        (* error if already created *)
  FTRUNC  =    8_001000;        (* truncate to zero length *)
  FAPPEND =    8_002000;        (* append on each write *)
  FNBLOCK =    8_004000;        (* POSIX no delay *)
  FNDELAY =    FNBLOCK;         (* no delay *)
  FSYNCRON =   8_010000;        (* write file syncronously *)
  FASYNC  =    8_020000;        (* Signal pgrp when data ready *)

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

<*EXTERNAL*> PROCEDURE access (path: const_char_star; mod: int): int;

(*** sbrk - change data segment space allocation *)

<*EXTERNAL*> PROCEDURE sbrk (inc: ptrdiff_t): int;


(*** chdir - change working directory ***)

<*EXTERNAL*> PROCEDURE chdir (path: const_char_star): int;


(*** chmod, fchmod - change mde of file ***)

<*EXTERNAL*> PROCEDURE chmod (path: const_char_star; mode: int): int;
<*EXTERNAL*> PROCEDURE fchmod (fd, mode: int): int;


(*** chown, fchown - change owner and group of a file ***)

<*EXTERNAL*> PROCEDURE chown (path: const_char_star; owner: uid_t; group: gid_t): int;
<*EXTERNAL*> PROCEDURE fchown (fd: int; owner: uid_t; group: gid_t): int;

(*** chroot - change root directory ***)

<*EXTERNAL*> PROCEDURE chroot (dirname: const_char_star): int;


(*** close - delete a descriptor ***)

<*EXTERNAL*> PROCEDURE close (d: int): int;


(*** creat - create a new file ***)

<*EXTERNAL*> PROCEDURE creat (name: const_char_star; mode: int): int;


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

(*** fcntl - file control ***)

CONST   (* request *)
  F_DUPFD =  0;   (* Duplicate fd *)
  F_GETFD =  1;   (* Get close-on-exec flag *)
  F_SETFD =  2;   (* Set close-on-exec flag *)
  F_GETFL =  3;   (* Get fd status flags *)
  F_SETFL =  4;   (* Set fd status flags *)

  F_GETOWN = 5;   (* Get owner *)
  F_SETOWN = 6;   (* Set owner *)

  (* in these three cases, you need to pass LOOPHOLE (ADR (v), int) 
     for arg, where v is a variable of type struct_flock *)
  F_GETLK  = 7;   (* Get file lock *)
  F_SETLK  = 8;   (* Set file lock *)
  F_SETLKW = 9;   (* Set file lock and wait *)

CONST (* fd flags *)
  FD_CLOEXEC = 1;    (* Close file descriptor on exec() *)


TYPE
  struct_flock = RECORD
    l_type:   short; (* see below *)
    l_whence: short;
    l_start:  long   := 0;
    l_len:    long   := 0;
    l_pid:    int    := 0;
  END;

CONST (* l_type values *)
  F_RDLCK = 0; (* Read lock *) 
  F_WRLCK = 1; (* Write lock *)
  F_UNLCK = 2; (* Remove lock(s) *)

  (* only on linux... *)
  F_EXLCK = 4; (* exclusive lock *)
  F_SHLCK = 8; (* shared lock *)

<*EXTERNAL*> PROCEDURE fcntl (fd, request, arg: int): int;

(*** flock - apply or remove an advisory lock on an open file ***)

CONST
  LOCK_SH = 1;   (* shared lock *)
  LOCK_EX = 2;   (* exclusive lock *)
  LOCK_NB = 4;   (* don't block when locking *)
  LOCK_UN = 8;   (* unlock *)

<*EXTERNAL*> PROCEDURE flock (fd, operation: int): int;

(*** fork - create a new process ***)

<*EXTERNAL*> PROCEDURE fork (): int;

(*** fsync - synchronize a file's in-core state with that on disk ***)

<*EXTERNAL*> PROCEDURE fsync (fd: int): int;


(*** getdirentries - gets directory entries in a generic directory format ***)

(* FIXME: disabled
<*EXTERNAL*> PROCEDURE getdirentries (fd: int; buf: ADDRESS;
                                  nbytes: int; VAR basep: long): int;
*)

(*** getdomainname, setdomainname - get or set name of current domain ***)

<*EXTERNAL*> PROCEDURE getdomainname (name: char_star; namelen: int): int;
<*EXTERNAL*> PROCEDURE setdomainname (name: char_star; namelen: int): int;

(*** getdtablesize - get descriptor table size ***)

<*EXTERNAL*> PROCEDURE getdtablesize (): int;

(*** getgroups - get group access list ***)

<*EXTERNAL*> PROCEDURE getgroups (gidsetsize: int; VAR gidset: int): int;

(*** gethostid, sethostid - get/set unique identifier of current host ***)

<*EXTERNAL*> PROCEDURE gethostid (): int;

<*EXTERNAL*> PROCEDURE sethostid (hostid: int): int;


(*** gethostname, sethostname - get/set name of current host ***)

<*EXTERNAL*> PROCEDURE gethostname (name: char_star; namelen: int): int;

<*EXTERNAL*> PROCEDURE sethostname (name: char_star; namelen: int): int;

(*** getpagesize - get system page size ***)

<*EXTERNAL*> PROCEDURE getpagesize (): int;

(*** getwd - get current working directory pathname ***)

<*EXTERNAL*> PROCEDURE getwd (pathname: char_star): char_star;
<*EXTERNAL*> PROCEDURE getcwd (pathname: char_star; size: size_t): char_star;

CONST

  IOCPARM_MASK = 16_7f;                 (* 128 bytes at most in parameters *)
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

but instead we use these *)

  INT    = Shift (And (BYTESIZE (INTEGER),              IOCPARM_MASK), 16);

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

CONST
  R_OK = 8_4;
  W_OK = 8_2;
  X_OK = 8_1;
  F_OK = 8_0;

<*EXTERNAL*> PROCEDURE ioctl (d, request: int; argp: ADDRESS): int;

(*** link - link to a file ***)

<*EXTERNAL*> PROCEDURE link (name1, name2: const_char_star): int;


(*** lseek, tell - move read/write pointer ***)

CONST (* whence *)
  L_SET  = 0;
  L_INCR = 1;
  L_XTND = 2;

<*EXTERNAL*> PROCEDURE lseek (d: int; offset: off_t; whence: int): off_t;

<*EXTERNAL*> PROCEDURE tell (d: int): long;

(*** mkdir - make a directory file ***)

<*EXTERNAL*> PROCEDURE mkdir (path: const_char_star; mode: int): int;

(*** open - open for reading or writing ***)

CONST (* flags *)
  O_RDONLY =    8_0;            (* open for reading *)
  O_WRONLY =    8_1;            (* open for writing *)
  O_RDWR   =    8_2;            (* open for read & write *)
  O_CREAT  =    FCREAT;         (* open with file create *)
  O_EXCL   =    FEXCL;          (* error on create if file exists *)
  O_NOCTTY =    8_400;
  O_TRUNC  =    FTRUNC;         (* open with truncation *)
  O_APPEND =    FAPPEND;        (* append on each write *)
  O_NONBLOCK =  FNBLOCK;        (* POSIX non-blocking I/O *)
  O_NDELAY =    FNDELAY;        (* non-blocking open *)
  O_FSYNC =     FSYNCRON;       (* syncronous write *)

  M3_NONBLOCK = O_NDELAY; (* -1 => would block, 0 => EOF *)

<*EXTERNAL*> PROCEDURE open (name: const_char_star; flags, mode: int): int;


(*** pipe - create an interprocess channel ***)
CONST
  readEnd = 0;
  writeEnd = 1;
<*EXTERNAL*> PROCEDURE pipe (VAR fildes: ARRAY [0..1] OF int): int;

(*** readlink - read value of a symbolic link ***)

<*EXTERNAL*> PROCEDURE readlink (path: const_char_star; buf: ADDRESS; bufsize: int): int;

(*** rename - change the name of a file ***)

<*EXTERNAL*> PROCEDURE rename (from, to: const_char_star): int;

(*** rmdir - remove a directory file ***)

<*EXTERNAL*> PROCEDURE rmdir (path: const_char_star): int;

(*** select - synchronous I/O mutiplexing ***)

CONST
  MAX_FDSET = 256;

TYPE
  FDSet = SET OF [0 .. MAX_FDSET - 1];

<*EXTERNAL*> PROCEDURE select (nfds: int;
                           readfds, writefds, exceptfds: UNTRACED REF FDSet;
                           timeout: UNTRACED REF struct_timeval): int;

(*** symlink - make symbolic link to a file ***)

<*EXTERNAL*> PROCEDURE symlink (name1, name2: const_char_star): int;

(*** sync - update super-block ***)

<*EXTERNAL*> PROCEDURE sync (): int;

(*** truncate, ftruncate - truncate a file to a specified length ***)

<*EXTERNAL*> PROCEDURE truncate (path: const_char_star; length: off_t): int;

<*EXTERNAL*> PROCEDURE ftruncate (fd: int; length: off_t): int;

(*** unlink - remove directory entry ***)

<*EXTERNAL*> PROCEDURE unlink (path: const_char_star): int;

(*** utimes - set file times ***)

<*EXTERNAL*> PROCEDURE utimes (file: char_star;
                    tvp: UNTRACED REF ARRAY [0..1] OF struct_timeval): int;

(*** vfork - spawn new process in a virtual memory efficient way ***)

<*EXTERNAL*> PROCEDURE vfork (): int;

(*** isatty(3) ***)
<*EXTERNAL*> PROCEDURE isatty (filedes: int): int;

(*** system(3) ***)
<*EXTERNAL*> PROCEDURE system (string: const_char_star): int;


END Unix.
