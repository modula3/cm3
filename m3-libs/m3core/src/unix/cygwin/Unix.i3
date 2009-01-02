(* Copyright (C) 1993, Digital Equipment Corporation                  *)
(* All rights reserved.                                               *)
(* See the file COPYRIGHT for a full description.                     *)

INTERFACE Unix;

FROM Ctypes IMPORT int, const_char_star, char_star, char_star_star;
FROM Utypes IMPORT off_t, size_t;
FROM Utime IMPORT struct_timeval;
IMPORT Usysdep;

CONST
  readEnd = 0;
  writeEnd = 1;
  MaxPathLen = Usysdep.MaxPathLen;
  MSETUID = Usysdep.MSETUID;
  MSETGID = Usysdep.MSETGID;
  MSTICKY = Usysdep.MSTICKY;
  MROWNER = Usysdep.MROWNER;
  MWOWNER = Usysdep.MWOWNER;
  MXOWNER = Usysdep.MXOWNER;
  MRGROUP = Usysdep.MRGROUP;
  MWGROUP = Usysdep.MWGROUP;
  MXGROUP = Usysdep.MXGROUP;
  MROTHER = Usysdep.MROTHER;
  MWOTHER = Usysdep.MWOTHER;
  MXOTHER = Usysdep.MXOTHER;
  Mrwrwrw = MROWNER + MWOWNER + MRGROUP + MWGROUP + MROTHER + MWOTHER;
  F_OK = Usysdep.F_OK;
  X_OK = Usysdep.X_OK;
  W_OK = Usysdep.W_OK;
  R_OK = Usysdep.R_OK;
  P_NOWAIT = Usysdep.P_NOWAIT;
  F_SETFD = Usysdep.F_SETFD;
  F_GETFL = Usysdep.F_GETFL;
  F_SETFL = Usysdep.F_SETFL;
  FIONREAD = Usysdep.FIONREAD;
  O_RDONLY = Usysdep.O_RDONLY;
  O_RDWR = Usysdep.O_RDWR;
  O_CREAT = Usysdep.O_CREAT;
  O_EXCL = Usysdep.O_EXCL;
  O_TRUNC = Usysdep.O_TRUNC;
  O_NDELAY = Usysdep.O_NDELAY;
  M3_NONBLOCK = Usysdep.M3_NONBLOCK;

(* some of these functions will need C wrappers for portability to systems
that have both 32bit and 64functions; e.g. open, lseek, creat, ftruncate, maybe pipe, ioctl *)

<*EXTERNAL*> PROCEDURE access (path: const_char_star; mod: int): int;
<*EXTERNAL*> PROCEDURE chdir (path: const_char_star): int;
<*EXTERNAL*> PROCEDURE close (d: int): int;
<*EXTERNAL*> PROCEDURE dup2 (oldd, newd: int): int;
<*EXTERNAL*> PROCEDURE execve (name: const_char_star;  argv, envp: char_star_star): int;
<*EXTERNAL*> PROCEDURE spawnve (mode: int; name: const_char_star; argv, envp: char_star_star): int;
<*EXTERNAL*> PROCEDURE exit (i: int);
<*EXTERNAL "_exit"*> PROCEDURE underscore_exit (i: int);
<*EXTERNAL*> PROCEDURE fcntl (fd, request, arg: int): int;
<*EXTERNAL*> PROCEDURE fsync (fd: int): int;
<*EXTERNAL*> PROCEDURE getdtablesize (): int;
<*EXTERNAL*> PROCEDURE gethostname (name: char_star; namelen: int): int;
<*EXTERNAL*> PROCEDURE getpagesize (): int;
<*EXTERNAL*> PROCEDURE getcwd (pathname: char_star; size: size_t): char_star;
<*EXTERNAL*> PROCEDURE ioctl (d, request: int; argp: ADDRESS): int;
<*EXTERNAL*> PROCEDURE lseek (d: int; offset: off_t; whence: int): off_t;
<*EXTERNAL*> PROCEDURE mkdir (path: const_char_star; mode: int): int;
<*EXTERNAL*> PROCEDURE open (name: const_char_star; flags, mode: int): int;
<*EXTERNAL*> PROCEDURE creat (name: const_char_star; mode: int): int;
<*EXTERNAL*> PROCEDURE pipe (VAR fildes: ARRAY [0..1] OF int): int;
<*EXTERNAL*> PROCEDURE readlink (path: const_char_star; buf: ADDRESS; bufsize: int): int;
<*EXTERNAL*> PROCEDURE rename (from, to: const_char_star): int;
<*EXTERNAL*> PROCEDURE rmdir (path: const_char_star): int;
<*EXTERNAL*> PROCEDURE symlink (name1, name2: const_char_star): int;
<*EXTERNAL*> PROCEDURE ftruncate (fd: int; length: off_t): int;
<*EXTERNAL*> PROCEDURE unlink (path: const_char_star): int;
<*EXTERNAL*> PROCEDURE utimes (file: const_char_star; tvp: UNTRACED REF ARRAY [0..1] OF struct_timeval): int;
<*EXTERNAL*> PROCEDURE vfork (): int;

CONST
  MAX_FDSET = Usysdep.MAX_FDSET;

TYPE
  FDSet = SET OF [0 .. MAX_FDSET - 1];

<*EXTERNAL*> PROCEDURE select (nfds: int; readfds, writefds, exceptfds: UNTRACED REF FDSet; timeout: UNTRACED REF struct_timeval): int;

END Unix.
