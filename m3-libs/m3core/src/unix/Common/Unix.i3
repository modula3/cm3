(* Copyright (C) 1993, Digital Equipment Corporation                  *)
(* All rights reserved.                                               *)
(* See the file COPYRIGHT for a full description.                     *)

INTERFACE Unix;

FROM Cstddef IMPORT size_t;
FROM Ctypes IMPORT int, const_char_star, char_star, char_star_star;
FROM Utime IMPORT struct_timeval;
FROM Utypes IMPORT mode_t, off_t;
IMPORT Usysdep;

CONST
  MaxPathLen = Usysdep.MaxPathLen;

  MSETUID = Usysdep.MSETUID;
  MSETGID = Usysdep.MSETGID;
  MSTICKY = Usysdep.MSTICKY;
  MROWNER = 8_0400;
  MWOWNER = 8_0200;
  MXOWNER = 8_0100;
  MRGROUP = 8_0040;
  MWGROUP = 8_0020;
  MXGROUP = 8_0010;
  MROTHER = 8_0004;
  MWOTHER = 8_0002;
  MXOTHER = 8_0001;

  Mrwrwrw = MROWNER + MWOWNER + MRGROUP + MWGROUP + MROTHER + MWOTHER;

  F_OK = 0;
  X_OK = 1;
  W_OK = 2;
  R_OK = 4;

<*EXTERNAL*> PROCEDURE sbrk (inc: INTEGER): char_star;
<*EXTERNAL*> PROCEDURE access (path: const_char_star; mode: int): int;
<*EXTERNAL*> PROCEDURE chdir (path: const_char_star): int;
<*EXTERNAL*> PROCEDURE close (d: int): int;
<*EXTERNAL*> PROCEDURE dup2 (oldd, newd: int): int;
<*EXTERNAL*> PROCEDURE execve (name: const_char_star;  argv, envp: char_star_star): int;

<*EXTERNAL*> PROCEDURE exit (i: int);
<*EXTERNAL "_exit"*> PROCEDURE underscore_exit (i: int);

CONST
  F_SETFD = 2; (* Set close-on-exec flag *)
  F_GETFL = 3; (* Get fd status flags *)
  F_SETFL = 4; (* Set fd status flags *)

<*EXTERNAL*> PROCEDURE fcntl (fd, request, arg: int): int;
<*EXTERNAL*> PROCEDURE fsync (fd: int): int;
<*EXTERNAL*> PROCEDURE getdtablesize (): int;
<*EXTERNAL*> PROCEDURE gethostname (name: char_star; namelen: size_t): int;
<*EXTERNAL*> PROCEDURE getpagesize (): int;
<*EXTERNAL*> PROCEDURE getcwd (pathname: char_star; size: size_t): char_star;

CONST
  FIONREAD = 16_4004667f;

<*EXTERNAL*> PROCEDURE ioctl (d, request: int; argp: ADDRESS): int;
<*EXTERNAL "m3_lseek"*> PROCEDURE lseek (d: int; offset: off_t; whence: int): off_t;
<*EXTERNAL*> PROCEDURE mkdir (path: const_char_star; mode: mode_t): int;

CONST
  O_RDONLY = Usysdep.O_RDONLY;
  O_RDWR = Usysdep.O_RDWR;
  O_CREAT = Usysdep.O_CREAT;
  O_EXCL = Usysdep.O_EXCL;
  O_TRUNC = Usysdep.O_TRUNC;
  O_NONBLOCK = Usysdep.O_NONBLOCK;
  O_NDELAY = O_NONBLOCK; (* compat *)
  M3_NONBLOCK = O_NONBLOCK; (* compat *)

<*EXTERNAL "m3_open"*> PROCEDURE open (name: const_char_star; flags: int; mode: mode_t): int;
<*EXTERNAL "m3_creat"*> PROCEDURE creat (name: const_char_star; mode: mode_t): int;

CONST
  readEnd = 0;
  writeEnd = 1;
<*EXTERNAL*> PROCEDURE pipe (VAR fildes: ARRAY [0..1] OF int): int;

<*EXTERNAL*> PROCEDURE readlink (path: const_char_star; buf: ADDRESS; bufsize: int): int;
<*EXTERNAL*> PROCEDURE rename (from, to: const_char_star): int;
<*EXTERNAL*> PROCEDURE rmdir (path: const_char_star): int;
<*EXTERNAL*> PROCEDURE symlink (name1, name2: const_char_star): int;
<*EXTERNAL "m3_ftruncate"*> PROCEDURE ftruncate (fd: int; length: off_t): int;
<*EXTERNAL*> PROCEDURE unlink (path: const_char_star): int;
<*EXTERNAL*> PROCEDURE utimes (file: const_char_star; tvp: UNTRACED REF ARRAY [0..1] OF struct_timeval): int;
<*EXTERNAL*> PROCEDURE vfork (): int;

CONST
  MAX_FDSET = Usysdep.MAX_FDSET;

TYPE
  FDSet = SET OF [0 .. MAX_FDSET - 1];

<*EXTERNAL*> PROCEDURE select (nfds: int; readfds, writefds, exceptfds: UNTRACED REF FDSet; timeout: UNTRACED REF struct_timeval): int;

END Unix.
