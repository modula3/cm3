(* Copyright (C) 1993, Digital Equipment Corporation                  *)
(* All rights reserved.                                               *)
(* See the file COPYRIGHT for a full description.                     *)

INTERFACE Unix;

FROM Cstddef IMPORT size_t;
FROM Ctypes IMPORT int, const_char_star, char_star, char_star_star;
FROM Utime IMPORT struct_timeval;
FROM Utypes IMPORT off_t, mode_t, dev_t, uid_t, gid_t, pid_t;
IMPORT Usysdep;

CONST
  MaxPathLen = Usysdep.MaxPathLen;

(*CONST*)
<*EXTERNAL "Unix__MSETUID"*> VAR MSETUID: int; (* set user id on execution *)
<*EXTERNAL "Unix__MSETGID"*> VAR MSETGID: int; (* set group id on execution *)
<*EXTERNAL "Unix__MSTICKY"*> VAR MSTICKY: int; (* save swapped text even after use *)

(* owner *)
<*EXTERNAL "Unix__MROWNER"*> VAR MROWNER: int; (* readable by owner *)
<*EXTERNAL "Unix__MWOWNER"*> VAR MWOWNER: int; (* writable by owner *)
<*EXTERNAL "Unix__MXOWNER"*> VAR MXOWNER: int; (* executable by owner *)
(* group *)
<*EXTERNAL "Unix__MRGROUP"*> VAR MRGROUP: int; (* readable by group *)
<*EXTERNAL "Unix__MWGROUP"*> VAR MWGROUP: int; (* writable by group *)
<*EXTERNAL "Unix__MXGROUP"*> VAR MXGROUP: int; (* executable by group *)
(* other *)
<*EXTERNAL "Unix__MROTHER"*> VAR MROTHER: int; (* readable by other *)
<*EXTERNAL "Unix__MWOTHER"*> VAR MWOTHER: int; (* writable by other *)
<*EXTERNAL "Unix__MXOTHER"*> VAR MXOTHER: int; (* executable by other *)

(* readable/writable by all, executable by none *)
<*EXTERNAL "Unix__Mrwrwrw"*> VAR Mrwrwrw: int; (* MROWNER + MWOWNER + MRGROUP + MWGROUP + MROTHER + MWOTHER *)

(* CONST *)
<*EXTERNAL "Unix__F_OK"*> VAR F_OK: int; (* existance *)
<*EXTERNAL "Unix__X_OK"*> VAR X_OK: int; (* executable *)
<*EXTERNAL "Unix__W_OK"*> VAR W_OK: int; (* writable *)
<*EXTERNAL "Unix__R_OK"*> VAR R_OK: int; (* readable *)
<*EXTERNAL "Unix__access"*>PROCEDURE access (path: const_char_star; mode: int): int;

<*EXTERNAL "Unix__sbrk"*>PROCEDURE sbrk (inc: INTEGER): char_star;
<*EXTERNAL "Unix__chdir"*>PROCEDURE chdir (path: const_char_star): int;
<*EXTERNAL "Unix__close"*>PROCEDURE close (d: int): int;
<*EXTERNAL "Unix__dup"*>PROCEDURE dup (oldd: int): int;
<*EXTERNAL "Unix__dup2"*>PROCEDURE dup2 (oldd, newd: int): int;
<*EXTERNAL "Unix__execve"*>PROCEDURE execve (name: const_char_star;  argv, envp: char_star_star): int;

<*EXTERNAL "Unix__exit"*>PROCEDURE exit (i: int);
<*EXTERNAL "Unix__underscore_exit"*>PROCEDURE underscore_exit (i: int);

(* CONST *)
<*EXTERNAL "Unix__F_SETFD"*> VAR F_SETFD: int; (* Set close-on-exec flag *)
<*EXTERNAL "Unix__F_GETFL"*> VAR F_GETFL: int; (* Get fd status flags *)
<*EXTERNAL "Unix__F_SETFL"*> VAR F_SETFL: int; (* Set fd status flags *)

<*EXTERNAL "Unix__fcntl"*>PROCEDURE fcntl (fd, request, arg: int): int;

<*EXTERNAL "Unix__fsync"*>PROCEDURE fsync (fd: int): int;
<*EXTERNAL "Unix__getdtablesize"*>PROCEDURE getdtablesize (): int;
<*EXTERNAL "Unix__gethostname"*>PROCEDURE gethostname (name: char_star; namelen: size_t): int;
<*EXTERNAL "Unix__getpagesize"*>PROCEDURE getpagesize (): int;
<*EXTERNAL "Unix__getcwd"*>PROCEDURE getcwd (pathname: char_star; size: size_t): char_star;

(* CONST *)
<*EXTERNAL "Unix__FIONREAD"*> VAR FIONREAD: int;

<*EXTERNAL "Unix__ioctl"*>
PROCEDURE ioctl (d, request: int; argp: ADDRESS): int;

<*EXTERNAL "Unix__lseek"*>
PROCEDURE lseek (d: int; offset: off_t; whence: int): off_t;

<*EXTERNAL "Unix__mkdir"*>
PROCEDURE mkdir (path: const_char_star; mode: int(*mode_t*)): int;

(* CONST *)
<*EXTERNAL "Unix__O_RDONLY"*>    VAR O_RDONLY: int;
<*EXTERNAL "Unix__O_RDWR"*>      VAR O_RDWR: int;
<*EXTERNAL "Unix__O_CREAT"*>     VAR O_CREAT: int;
<*EXTERNAL "Unix__O_EXCL"*>      VAR O_EXCL: int;
<*EXTERNAL "Unix__O_TRUNC"*>     VAR O_TRUNC: int;
<*EXTERNAL "Unix__O_NONBLOCK"*>  VAR O_NONBLOCK: int;
<*EXTERNAL "Unix__O_APPEND"*>    VAR O_APPEND: int; (* append on each write *)
<*EXTERNAL "Unix__O_NDELAY"*>    VAR O_NDELAY: int; (* compat *)
<*EXTERNAL "Unix__M3_NONBLOCK"*> VAR M3_NONBLOCK: int; (* compat *)

<*EXTERNAL "Unix__open"*>
PROCEDURE open (name: const_char_star; flags: int; mode: int(*mode_t*)): int;

CONST
  readEnd = 0;
  writeEnd = 1;
<*EXTERNAL "Unix__pipe"*>PROCEDURE pipe (VAR fildes: ARRAY [0..1] OF int): int;

<*EXTERNAL "Unix__readlink"*>
PROCEDURE readlink (path: const_char_star; buf: ADDRESS; bufsize: INTEGER): INTEGER;
<*EXTERNAL "Unix__rename"*>
PROCEDURE rename (from, to: const_char_star): int;
<*EXTERNAL "Unix__rmdir"*>
PROCEDURE rmdir (path: const_char_star): int;
<*EXTERNAL "Unix__symlink"*>
PROCEDURE symlink (name1, name2: const_char_star): int;
<*EXTERNAL "Unix__truncate"*> 
PROCEDURE  truncate (file: const_char_star; length: off_t): int;
<*EXTERNAL "Unix__ftruncate"*>
PROCEDURE ftruncate (file: int;             length: off_t): int;
<*EXTERNAL "Unix__unlink"*>
PROCEDURE unlink (path: const_char_star): int;
<*EXTERNAL "Unix__utimes"*>
PROCEDURE utimes (file: const_char_star; tvp: UNTRACED REF ARRAY [0..1] OF struct_timeval): int;
<*EXTERNAL "Unix__fork"*>
PROCEDURE fork (): pid_t;

(* Do not wrap vfork in C; doing so violates the Posix standard, because
 callers of vfork cannot return without calling exec or _exit. *)
<*EXTERNAL*>
PROCEDURE vfork (): pid_t;


CONST
  MAX_FDSET = Usysdep.MAX_FDSET;

TYPE
  FDSet = SET OF [0 .. MAX_FDSET - 1];

<*EXTERNAL "Unix__select"*>
PROCEDURE select (nfds: int; readfds, writefds, exceptfds: UNTRACED REF FDSet; timeout: UNTRACED REF struct_timeval): int;

<*EXTERNAL "Unix__mknod"*>
PROCEDURE mknod (path: const_char_star; mode: mode_t; dev: dev_t): int;

<*EXTERNAL "Unix__umask"*>
PROCEDURE umask (numask: mode_t): mode_t;

<*EXTERNAL "Unix__link"*>
PROCEDURE link (name1, name2: const_char_star): int;
<*EXTERNAL "Unix__chmod"*>
PROCEDURE chmod (path: const_char_star; mode: mode_t): int;
<*EXTERNAL "Unix__fchmod"*>
PROCEDURE fchmod (fd, mode: int): int;
<*EXTERNAL "Unix__chown"*>
PROCEDURE chown (path: const_char_star; owner: uid_t; group: gid_t): int;
<*EXTERNAL "Unix__fchown"*>
PROCEDURE fchown (fd: int; owner: uid_t; group: gid_t): int;
<*EXTERNAL "Unix__creat"*>
PROCEDURE creat (name: const_char_star; mode: mode_t): int;

<*EXTERNAL "Unix__isatty"*>
PROCEDURE isatty (file: int): int;

<*EXTERNAL "Unix__system"*>
PROCEDURE system (string: const_char_star): int;

<*EXTERNAL "Unix__Assertions"*>
PROCEDURE Assertions();

(*CONST*) <*EXTERNAL Unix__TIOCCAR*> VAR TIOCCAR: int;
(*CONST*) <*EXTERNAL Unix__TIOCCBRK*> VAR TIOCCBRK: int;
(*CONST*) <*EXTERNAL Unix__TIOCCDTR*> VAR TIOCCDTR: int;
(*CONST*) <*EXTERNAL Unix__TIOCCINUSE*> VAR TIOCCINUSE: int;
(*CONST*) <*EXTERNAL Unix__TIOCCMLB*> VAR TIOCCMLB: int;
(*CONST*) <*EXTERNAL Unix__TIOCEXCL*> VAR TIOCEXCL: int;
(*CONST*) <*EXTERNAL Unix__TIOCFLUSH*> VAR TIOCFLUSH: int;
(*CONST*) <*EXTERNAL Unix__TIOCGETC*> VAR TIOCGETC: int;
(*CONST*) <*EXTERNAL Unix__TIOCGETD*> VAR TIOCGETD: int;
(*CONST*) <*EXTERNAL Unix__TIOCGETP*> VAR TIOCGETP: int;
(*CONST*) <*EXTERNAL Unix__TIOCGLTC*> VAR TIOCGLTC: int;
(*CONST*) <*EXTERNAL Unix__TIOCGPGRP*> VAR TIOCGPGRP: int;
(*CONST*) <*EXTERNAL Unix__TIOCGWINSZ*> VAR TIOCGWINSZ: int;
(*CONST*) <*EXTERNAL Unix__TIOCHPCL*> VAR TIOCHPCL: int;
(*CONST*) <*EXTERNAL Unix__TIOCLBIC*> VAR TIOCLBIC: int;
(*CONST*) <*EXTERNAL Unix__TIOCLBIS*> VAR TIOCLBIS: int;
(*CONST*) <*EXTERNAL Unix__TIOCLGET*> VAR TIOCLGET: int;
(*CONST*) <*EXTERNAL Unix__TIOCLSET*> VAR TIOCLSET: int;
(*CONST*) <*EXTERNAL Unix__TIOCMASTER*> VAR TIOCMASTER: int;
(*CONST*) <*EXTERNAL Unix__TIOCMBIC*> VAR TIOCMBIC: int;
(*CONST*) <*EXTERNAL Unix__TIOCMBIS*> VAR TIOCMBIS: int;
(*CONST*) <*EXTERNAL Unix__TIOCMGET*> VAR TIOCMGET: int;
(*CONST*) <*EXTERNAL Unix__TIOCMODEM*> VAR TIOCMODEM: int;
(*CONST*) <*EXTERNAL Unix__TIOCMODG*> VAR TIOCMODG: int;
(*CONST*) <*EXTERNAL Unix__TIOCMODS*> VAR TIOCMODS: int;
(*CONST*) <*EXTERNAL Unix__TIOCMSET*> VAR TIOCMSET: int;
(*CONST*) <*EXTERNAL Unix__TIOCM_CAR*> VAR TIOCM_CAR: int;
(*CONST*) <*EXTERNAL Unix__TIOCM_CD*> VAR TIOCM_CD: int;
(*CONST*) <*EXTERNAL Unix__TIOCM_CTS*> VAR TIOCM_CTS: int;
(*CONST*) <*EXTERNAL Unix__TIOCM_DSR*> VAR TIOCM_DSR: int;
(*CONST*) <*EXTERNAL Unix__TIOCM_DTR*> VAR TIOCM_DTR: int;
(*CONST*) <*EXTERNAL Unix__TIOCM_LE*> VAR TIOCM_LE: int;
(*CONST*) <*EXTERNAL Unix__TIOCM_RI*> VAR TIOCM_RI: int;
(*CONST*) <*EXTERNAL Unix__TIOCM_RNG*> VAR TIOCM_RNG: int;
(*CONST*) <*EXTERNAL Unix__TIOCM_RTS*> VAR TIOCM_RTS: int;
(*CONST*) <*EXTERNAL Unix__TIOCM_SR*> VAR TIOCM_SR: int;
(*CONST*) <*EXTERNAL Unix__TIOCM_ST*> VAR TIOCM_ST: int;
(*CONST*) <*EXTERNAL Unix__TIOCNCAR*> VAR TIOCNCAR: int;
(*CONST*) <*EXTERNAL Unix__TIOCNMODEM*> VAR TIOCNMODEM: int;
(*CONST*) <*EXTERNAL Unix__TIOCNOTTY*> VAR TIOCNOTTY: int;
(*CONST*) <*EXTERNAL Unix__TIOCNXCL*> VAR TIOCNXCL: int;
(*CONST*) <*EXTERNAL Unix__TIOCOUTQ*> VAR TIOCOUTQ: int;
(*CONST*) <*EXTERNAL Unix__TIOCPKT*> VAR TIOCPKT: int;
(*CONST*) <*EXTERNAL Unix__TIOCPKT_DATA*> VAR TIOCPKT_DATA: int;
(*CONST*) <*EXTERNAL Unix__TIOCPKT_DOSTOP*> VAR TIOCPKT_DOSTOP: int;
(*CONST*) <*EXTERNAL Unix__TIOCPKT_FLUSHREAD*> VAR TIOCPKT_FLUSHREAD: int;
(*CONST*) <*EXTERNAL Unix__TIOCPKT_FLUSHWRITE*> VAR TIOCPKT_FLUSHWRITE: int;
(*CONST*) <*EXTERNAL Unix__TIOCPKT_IOCTL*> VAR TIOCPKT_IOCTL: int;
(*CONST*) <*EXTERNAL Unix__TIOCPKT_NOSTOP*> VAR TIOCPKT_NOSTOP: int;
(*CONST*) <*EXTERNAL Unix__TIOCPKT_START*> VAR TIOCPKT_START: int;
(*CONST*) <*EXTERNAL Unix__TIOCPKT_STOP*> VAR TIOCPKT_STOP: int;
(*CONST*) <*EXTERNAL Unix__TIOCREMOTE*> VAR TIOCREMOTE: int;
(*CONST*) <*EXTERNAL Unix__TIOCSBRK*> VAR TIOCSBRK: int;
(*CONST*) <*EXTERNAL Unix__TIOCSDTR*> VAR TIOCSDTR: int;
(*CONST*) <*EXTERNAL Unix__TIOCSETC*> VAR TIOCSETC: int;
(*CONST*) <*EXTERNAL Unix__TIOCSETD*> VAR TIOCSETD: int;
(*CONST*) <*EXTERNAL Unix__TIOCSETN*> VAR TIOCSETN: int;
(*CONST*) <*EXTERNAL Unix__TIOCSETP*> VAR TIOCSETP: int;
(*CONST*) <*EXTERNAL Unix__TIOCSINUSE*> VAR TIOCSINUSE: int;
(*CONST*) <*EXTERNAL Unix__TIOCSLTC*> VAR TIOCSLTC: int;
(*CONST*) <*EXTERNAL Unix__TIOCSMLB*> VAR TIOCSMLB: int;
(*CONST*) <*EXTERNAL Unix__TIOCSPGRP*> VAR TIOCSPGRP: int;
(*CONST*) <*EXTERNAL Unix__TIOCSTART*> VAR TIOCSTART: int;
(*CONST*) <*EXTERNAL Unix__TIOCSTI*> VAR TIOCSTI: int;
(*CONST*) <*EXTERNAL Unix__TIOCSTOP*> VAR TIOCSTOP: int;
(*CONST*) <*EXTERNAL Unix__TIOCSWINSZ*> VAR TIOCSWINSZ: int;
(*CONST*) <*EXTERNAL Unix__TIOCUCNTL*> VAR TIOCUCNTL: int;
(*CONST*) <*EXTERNAL Unix__TIOCWONLINE*> VAR TIOCWONLINE: int;

END Unix.
