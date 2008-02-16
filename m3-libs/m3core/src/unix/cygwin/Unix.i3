(* Copyright (C) 1993, Digital Equipment Corporation                  *)
(* All rights reserved.                                               *)
(* See the file COPYRIGHT for a full description.                     *)
(*                                                                    *)
(* Last modified on Fri Feb 24 15:05:21 PST 1995 by kalsow            *)
(*      modified on Tue Feb 14 20:34:10 GMT 1995 by rrw1000@cam.ac.uk *)
(*      modified on Tue Jun  8 16:26:41 PDT 1993 by mcjones           *)
(*      modified on Mon Jan 11 14:34:49 PST 1993 by muller            *)

(* $Id$ *)

INTERFACE Unix;

FROM Word IMPORT Or, And, Shift;

FROM Ctypes IMPORT short, int, long, const_char_star, char_star, char_star_star;
FROM Utypes IMPORT off_t, size_t, pid_t;
FROM Utime IMPORT struct_timeval;

TYPE
  ptrdiff_t = INTEGER;

CONST
  MaxPathLen = 1024;

(*** file flags ***)

CONST
  FREAD =      8_000001;        (* descriptor read/receive'able *)
  FWRITE =     8_000002;        (* descriptor write/send'ale *)
  FAPPEND   = 16_0008;          (* append on each write *)
  FCREAT    = 16_0200;          (* create if nonexistant *)
  FTRUNC    = 16_0400;          (* truncate to zero length *)
  FEXCL     = 16_0800;          (* error if already created *)
  FNONBLOCK = 16_4000;          (* non blocking I/O (POSIX style) *)
  FNDELAY   = FNONBLOCK;        (* non blocking I/O (4.2 style) *)

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
CONST
 (* parameters to access *)
  F_OK = 0; (* exist *)
  X_OK = 1; (* executable *)
  W_OK = 2; (* writable *)
  R_OK = 4; (* readable *)
<*EXTERNAL*> PROCEDURE access (path: const_char_star; mod: int): int;


(*** chdir - change working directory ***)

<*EXTERNAL*> PROCEDURE chdir (path: const_char_star): int;

(*** close - delete a descriptor ***)

<*EXTERNAL*> PROCEDURE close (d: int): int;

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
    l_start:  off_t   := 0L;
    l_len:    off_t   := 0L;
    l_pid:    pid_t   := 0;
  END;

CONST (* l_type values *)
  F_RDLCK = 1; (* Read lock *) 
  F_WRLCK = 2; (* Write lock *)
  F_UNLCK = 3; (* Remove lock(s) *)

<*EXTERNAL*> PROCEDURE fcntl (fd, request, arg: int): int;

(*** flock - apply or remove an advisory lock on an open file ***)

CONST
  LOCK_SH = 1;   (* shared lock *)
  LOCK_EX = 2;   (* exclusive lock *)
  LOCK_NB = 4;   (* don't block when locking *)
  LOCK_UN = 8;   (* unlock *)

<*EXTERNAL*> PROCEDURE flock (fd, operation: int): int;

(*** fsync - synchronize a file's in-core state with that on disk ***)

<*EXTERNAL*> PROCEDURE fsync (fd: int): int;

(*** getdtablesize - get descriptor table size ***)

<*EXTERNAL*> PROCEDURE getdtablesize (): int;

(*** gethostname, sethostname - get/set name of current host ***)

<*EXTERNAL*> PROCEDURE gethostname (name: char_star; namelen: int): int;

(*** getpagesize - get system page size ***)

<*EXTERNAL*> PROCEDURE getpagesize (): int;

(*** getwd - get current working directory pathname ***)

<*EXTERNAL*> PROCEDURE getwd (pathname: char_star): char_star;
<*EXTERNAL*> PROCEDURE getcwd (pathname: char_star; size: size_t): char_star;

CONST

  IOCPARM_MASK = 16_7f;                 (* 128 bytes at most in parameters *)
  IOC_OUT   = Shift (1, 30);       (* copy out parameters *)
  IOC_IN    = Shift (1, 31);       (* copy in parameters *)
  IOC_INOUT = Or (IOC_IN, IOC_OUT);

  R      = IOC_OUT;
  
  INT    = Shift (And (BYTESIZE (INTEGER),              IOCPARM_MASK), 16);

  (* File i/o controls *)
  FC = Shift (ORD ('f'), 8);

  FIONREAD =  Or (Or (R, INT), Or (FC, 127)); (* Get # bytes to read    *)

<*EXTERNAL*> PROCEDURE ioctl (d, request: int; argp: ADDRESS): int;


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
  O_TRUNC  =    FTRUNC;         (* open with truncation *)
  O_APPEND =    FAPPEND;        (* append on each write *)
  O_NDELAY =    FNDELAY;        (* non-blocking open *)

  M3_NONBLOCK = O_NDELAY; (* -1 => would block, 0 => EOF *) 

<*EXTERNAL*> PROCEDURE open (name: const_char_star; flags, mode: int): int;

(*** creat - create a new file ***)

<*EXTERNAL*> PROCEDURE creat (name: const_char_star; mode: int): int;

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

(*** system(3) ***)
<*EXTERNAL*> PROCEDURE system (string: const_char_star): int;

END Unix.
