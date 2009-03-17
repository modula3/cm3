(* Copyright (C) 1993, Digital Equipment Corporation                  *)
(* All rights reserved.                                               *)
(* See the file COPYRIGHT for a full description.                     *)

<*EXTERNAL*> INTERFACE Unix;

FROM Cstddef IMPORT size_t;
FROM Ctypes IMPORT int, const_char_star, char_star, char_star_star;
FROM Utime IMPORT struct_timeval;
FROM Utypes IMPORT off_t;
IMPORT Usysdep;

CONST
  MaxPathLen = Usysdep.MaxPathLen;

(*CONST*)
<*EXTERNAL Unix__MSETUID*> VAR MSETUID: int; (* set user id on execution *)
<*EXTERNAL Unix__MSETGID*> VAR MSETGID: int; (* set group id on execution *)
<*EXTERNAL Unix__MSTICKY*> VAR MSTICKY: int; (* save swapped text even after use *)

(* owner *)
<*EXTERNAL Unix__MROWNER*> VAR MROWNER: int; (* readable by owner *)
<*EXTERNAL Unix__MWOWNER*> VAR MWOWNER: int; (* writable by owner *)
<*EXTERNAL Unix__MXOWNER*> VAR MXOWNER: int; (* executable by owner *)
(* group *)
<*EXTERNAL Unix__MRGROUP*> VAR MRGROUP: int; (* readable by group *)
<*EXTERNAL Unix__MWGROUP*> VAR MWGROUP: int; (* writable by group *)
<*EXTERNAL Unix__MXGROUP*> VAR MXGROUP: int; (* executable by group *)
(* other *)
<*EXTERNAL Unix__MROTHER*> VAR MROTHER: int; (* readable by other *)
<*EXTERNAL Unix__MWOTHER*> VAR MWOTHER: int; (* writable by other *)
<*EXTERNAL Unix__MXOTHER*> VAR MXOTHER: int; (* executable by other *)

(* readable/writable by all, executable by none *)
<*EXTERNAL Unix__Mrwrwrw*> VAR Mrwrwrw: int; (* MROWNER + MWOWNER + MRGROUP + MWGROUP + MROTHER + MWOTHER *)

(* CONST *)
<*EXTERNAL Unix__F_OK*> VAR F_OK: int;
<*EXTERNAL Unix__X_OK*> VAR X_OK: int;
<*EXTERNAL Unix__W_OK*> VAR W_OK: int;
<*EXTERNAL Unix__R_OK*> VAR R_OK: int;
PROCEDURE access (path: const_char_star; mode: int): int;

PROCEDURE sbrk (inc: INTEGER): char_star;
PROCEDURE chdir (path: const_char_star): int;
PROCEDURE close (d: int): int;
PROCEDURE dup2 (oldd, newd: int): int;
PROCEDURE execve (name: const_char_star;  argv, envp: char_star_star): int;

PROCEDURE exit (i: int);
<*EXTERNAL "_exit"*> PROCEDURE underscore_exit (i: int);

(* CONST *)
<*EXTERNAL Unix__F_SETFD*> VAR F_SETFD: int; (* Set close-on-exec flag *)
<*EXTERNAL Unix__F_GETFL*> VAR F_GETFL: int; (* Get fd status flags *)
<*EXTERNAL Unix__F_SETFL*> VAR F_SETFL: int; (* Set fd status flags *)

<*EXTERNAL Unix__fcntl*>
PROCEDURE fcntl (fd, request, arg: int): int;

PROCEDURE fsync (fd: int): int;
PROCEDURE getdtablesize (): int;
PROCEDURE gethostname (name: char_star; namelen: size_t): int;
PROCEDURE getpagesize (): int;
PROCEDURE getcwd (pathname: char_star; size: size_t): char_star;

(* CONST *)
<*EXTERNAL Unix__FIONREAD*> VAR FIONREAD: int;

<*EXTERNAL Unix__ioctl*>
PROCEDURE ioctl (d, request: int; argp: ADDRESS): int;

<*EXTERNAL Unix__lseek*>
PROCEDURE lseek (d: int; offset: off_t; whence: int): off_t;

<*EXTERNAL Unix__mkdir*>
PROCEDURE mkdir (path: const_char_star; mode: int(*mode_t*)): int;

(* CONST *)
<*EXTERNAL Unix__O_RDONLY*>    VAR O_RDONLY: int;
<*EXTERNAL Unix__O_RDWR*>      VAR O_RDWR: int;
<*EXTERNAL Unix__O_CREAT*>     VAR O_CREAT: int;
<*EXTERNAL Unix__O_EXCL*>      VAR O_EXCL: int;
<*EXTERNAL Unix__O_TRUNC*>     VAR O_TRUNC: int;
<*EXTERNAL Unix__O_NONBLOCK*>  VAR O_NONBLOCK: int;
<*EXTERNAL Unix__O_NDELAY*>    VAR O_NDELAY: int; (* compat *)
<*EXTERNAL Unix__M3_NONBLOCK*> VAR M3_NONBLOCK: int; (* compat *)

<*EXTERNAL Unix__open*>
PROCEDURE open (name: const_char_star; flags: int; mode: int(*mode_t*)): int;

CONST
  readEnd = 0;
  writeEnd = 1;
PROCEDURE pipe (VAR fildes: ARRAY [0..1] OF int): int;

PROCEDURE readlink (path: const_char_star; buf: ADDRESS; bufsize: int): int;
PROCEDURE rename (from, to: const_char_star): int;
PROCEDURE rmdir (path: const_char_star): int;
PROCEDURE symlink (name1, name2: const_char_star): int;
<*EXTERNAL Unix__ftruncate*>
PROCEDURE ftruncate (fd: int; length: off_t): int;
PROCEDURE unlink (path: const_char_star): int;
PROCEDURE utimes (file: const_char_star; tvp: UNTRACED REF ARRAY [0..1] OF struct_timeval): int;
PROCEDURE vfork (): int;

CONST
  MAX_FDSET = Usysdep.MAX_FDSET;

TYPE
  FDSet = SET OF [0 .. MAX_FDSET - 1];

PROCEDURE select (nfds: int; readfds, writefds, exceptfds: UNTRACED REF FDSet; timeout: UNTRACED REF struct_timeval): int;

<*EXTERNAL Unix__Assertions*>
PROCEDURE Assertions();

END Unix.
