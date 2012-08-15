(* Copyright (C) 1993, Digital Equipment Corporation                  *)
(* All rights reserved.                                               *)
(* See the file COPYRIGHT for a full description.                     *)

INTERFACE Unix;

FROM Cstddef IMPORT size_t;
FROM Ctypes IMPORT int, const_char_star, char_star, char_star_star, unsigned;
FROM Utypes IMPORT off_t, mode_t, dev_t, uid_t, gid_t, pid_t;

CONST
  MaxPathLen = 1024; (* 4096 on Irix? *)

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

(*CONST*)
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

(*CONST*)
<*EXTERNAL "Unix__F_DUPFD"*> VAR F_DUPFD: int;      (* Duplicate fd *)
<*EXTERNAL "Unix__F_GETFD"*> VAR F_GETFD: int;      (* Get close-on-exec flag *)
<*EXTERNAL "Unix__F_SETFD"*> VAR F_SETFD: int;      (* Set close-on-exec flag *)
<*EXTERNAL "Unix__F_GETFL"*> VAR F_GETFL: int;      (* Get fd status flags *)
<*EXTERNAL "Unix__F_SETFL"*> VAR F_SETFL: int;      (* Set fd status flags *)
<*EXTERNAL "Unix__F_GETOWN"*> VAR F_GETOWN: int;    (* Set owner *)
<*EXTERNAL "Unix__F_SETOWN"*> VAR F_SETOWN: int;    (* Get owner *)
<*EXTERNAL "Unix__F_GETLK"*> VAR F_GETLK: int;      (* Get file lock *)
<*EXTERNAL "Unix__F_SETLK"*> VAR F_SETLK: int;      (* Set file lock *)
<*EXTERNAL "Unix__F_SETLKW"*> VAR F_SETLKW: int;    (* Set file lock and wait *)
<*EXTERNAL "Unix__FD_CLOEXEC"*> VAR FD_CLOEXEC: int; (* Close file descriptor on exec() *)

TYPE struct_flock = RECORD
(* sorted by size and then name
   This must match between Unix.i3 and UnixC.c. *)
  l_len:    LONGINT := 0L;
  l_start:  LONGINT := 0L;
  l_pid:    INTEGER := 0;
  l_type:   INTEGER := 0;
  l_whence: INTEGER := 0;
END;

<*EXTERNAL "Unix__fcntl"*>PROCEDURE fcntl (fd: int; request, arg: INTEGER): int;

<*EXTERNAL "Unix__fsync"*>PROCEDURE fsync (fd: int): int;
<*EXTERNAL "Unix__getdtablesize"*>PROCEDURE getdtablesize (): int;
<*EXTERNAL "Unix__gethostname"*>PROCEDURE gethostname (name: char_star; namelen: size_t): int;
<*EXTERNAL "Unix__getpagesize"*>PROCEDURE getpagesize (): int;
<*EXTERNAL "Unix__getcwd"*>PROCEDURE getcwd (pathname: char_star; size: size_t): char_star;

(*CONST*) <*EXTERNAL "Unix__FIONREAD"*> VAR FIONREAD: int;

<*EXTERNAL "Unix__ioctl"*>
PROCEDURE ioctl (d: int; request: INTEGER; argp: ADDRESS): int;

<*EXTERNAL "Unix__lseek"*>
PROCEDURE lseek (d: int; offset: off_t; whence: int): off_t;

<*EXTERNAL "Unix__tell"*>
PROCEDURE tell (d: int): off_t;

<*EXTERNAL "Unix__mkdir"*>
PROCEDURE mkdir (path: const_char_star; mode: int(*mode_t*)): int;

(*CONST*)
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
(* <*EXTERNAL "Unix__fork"*>
  PROCEDURE fork (): pid_t;
  see RTProcess.Fork *)

(* Do not wrap vfork in C; doing so violates the Posix standard, because
 callers of vfork cannot return without calling exec or _exit. *)
<*EXTERNAL*>
PROCEDURE vfork (): pid_t;

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

<*EXTERNAL Unix__sleep*> PROCEDURE sleep (a: unsigned): unsigned;

<*EXTERNAL "Unix__Assertions"*>
PROCEDURE Assertions();

(*CONST*) <*EXTERNAL Unix__ALLDELAY*> VAR ALLDELAY: int;
(*CONST*) <*EXTERNAL Unix__ANYP*> VAR ANYP: int;
(*CONST*) <*EXTERNAL Unix__BS0*> VAR BS0: int;
(*CONST*) <*EXTERNAL Unix__BS1*> VAR BS1: int;
(*CONST*) <*EXTERNAL Unix__BSDELAY*> VAR BSDELAY: int;
(*CONST*) <*EXTERNAL Unix__CBREAK*> VAR CBREAK: int;
(*CONST*) <*EXTERNAL Unix__CR0*> VAR CR0: int;
(*CONST*) <*EXTERNAL Unix__CR1*> VAR CR1: int;
(*CONST*) <*EXTERNAL Unix__CR2*> VAR CR2: int;
(*CONST*) <*EXTERNAL Unix__CR3*> VAR CR3: int;
(*CONST*) <*EXTERNAL Unix__CRDELAY*> VAR CRDELAY: int;
(*CONST*) <*EXTERNAL Unix__CRMOD*> VAR CRMOD: int;
(*CONST*) <*EXTERNAL Unix__CRTBS*> VAR CRTBS: int;
(*CONST*) <*EXTERNAL Unix__CRTERA*> VAR CRTERA: int;
(*CONST*) <*EXTERNAL Unix__CRTKIL*> VAR CRTKIL: int;
(*CONST*) <*EXTERNAL Unix__CTLECH*> VAR CTLECH: int;
(*CONST*) <*EXTERNAL Unix__DECCTQ*> VAR DECCTQ: int;
(*CONST*) <*EXTERNAL Unix__ECHO*> VAR ECHO: int;
(*CONST*) <*EXTERNAL Unix__EVENP*> VAR EVENP: int;
(*CONST*) <*EXTERNAL Unix__FF0*> VAR FF0: int;
(*CONST*) <*EXTERNAL Unix__FF1*> VAR FF1: int;
(*CONST*) <*EXTERNAL Unix__FLUSHO*> VAR FLUSHO: int;
(*CONST*) <*EXTERNAL Unix__L001000*> VAR L001000: int;
(*CONST*) <*EXTERNAL Unix__LCASE*> VAR LCASE: int;
(*CONST*) <*EXTERNAL Unix__LCRTBS*> VAR LCRTBS: int;
(*CONST*) <*EXTERNAL Unix__LCRTERA*> VAR LCRTERA: int;
(*CONST*) <*EXTERNAL Unix__LCRTKIL*> VAR LCRTKIL: int;
(*CONST*) <*EXTERNAL Unix__LCTLECH*> VAR LCTLECH: int;
(*CONST*) <*EXTERNAL Unix__LDECCTQ*> VAR LDECCTQ: int;
(*CONST*) <*EXTERNAL Unix__LFLUSHO*> VAR LFLUSHO: int;
(*CONST*) <*EXTERNAL Unix__LITOUT*> VAR LITOUT: int;
(*CONST*) <*EXTERNAL Unix__LLITOUT*> VAR LLITOUT: int;
(*CONST*) <*EXTERNAL Unix__LMDMBUF*> VAR LMDMBUF: int;
(*CONST*) <*EXTERNAL Unix__LNOFLSH*> VAR LNOFLSH: int;
(*CONST*) <*EXTERNAL Unix__LNOHANG*> VAR LNOHANG: int;
(*CONST*) <*EXTERNAL Unix__LPASS8*> VAR LPASS8: int;
(*CONST*) <*EXTERNAL Unix__LPENDIN*> VAR LPENDIN: int;
(*CONST*) <*EXTERNAL Unix__LPRTERA*> VAR LPRTERA: int;
(*CONST*) <*EXTERNAL Unix__LTILDE*> VAR LTILDE: int;
(*CONST*) <*EXTERNAL Unix__LTOSTOP*> VAR LTOSTOP: int;
(*CONST*) <*EXTERNAL Unix__MDMBUF*> VAR MDMBUF: int;
(*CONST*) <*EXTERNAL Unix__NETLDISC*> VAR NETLDISC: int;
(*CONST*) <*EXTERNAL Unix__NL0*> VAR NL0: int;
(*CONST*) <*EXTERNAL Unix__NL1*> VAR NL1: int;
(*CONST*) <*EXTERNAL Unix__NL2*> VAR NL2: int;
(*CONST*) <*EXTERNAL Unix__NL3*> VAR NL3: int;
(*CONST*) <*EXTERNAL Unix__NLDELAY*> VAR NLDELAY: int;
(*CONST*) <*EXTERNAL Unix__NOFLSH*> VAR NOFLSH: int;
(*CONST*) <*EXTERNAL Unix__NOHANG*> VAR NOHANG: int;
(*CONST*) <*EXTERNAL Unix__NTTYDISC*> VAR NTTYDISC: int;
(*CONST*) <*EXTERNAL Unix__ODDP*> VAR ODDP: int;
(*CONST*) <*EXTERNAL Unix__OTIOCCONS*> VAR OTIOCCONS: int;
(*CONST*) <*EXTERNAL Unix__OTIOCGETD*> VAR OTIOCGETD: int;
(*CONST*) <*EXTERNAL Unix__OTIOCSETD*> VAR OTIOCSETD: int;
(*CONST*) <*EXTERNAL Unix__OTTYDISC*> VAR OTTYDISC: int;
(*CONST*) <*EXTERNAL Unix__PASS8*> VAR PASS8: int;
(*CONST*) <*EXTERNAL Unix__PENDIN*> VAR PENDIN: int;
(*CONST*) <*EXTERNAL Unix__PRTERA*> VAR PRTERA: int;
(*CONST*) <*EXTERNAL Unix__RAW*> VAR RAW: int;
(*CONST*) <*EXTERNAL Unix__TAB0*> VAR TAB0: int;
(*CONST*) <*EXTERNAL Unix__TAB1*> VAR TAB1: int;
(*CONST*) <*EXTERNAL Unix__TAB2*> VAR TAB2: int;
(*CONST*) <*EXTERNAL Unix__TANDEM*> VAR TANDEM: int;
(*CONST*) <*EXTERNAL Unix__TBDELAY*> VAR TBDELAY: int;
(*CONST*) <*EXTERNAL Unix__TERMIODISC*> VAR TERMIODISC: int;
(*CONST*) <*EXTERNAL Unix__TILDE*> VAR TILDE: int;
(*CONST*) <*EXTERNAL Unix__TIOCCAR*> VAR TIOCCAR: int;
(*CONST*) <*EXTERNAL Unix__TIOCCBRK*> VAR TIOCCBRK: int;
(*CONST*) <*EXTERNAL Unix__TIOCCDTR*> VAR TIOCCDTR: int;
(*CONST*) <*EXTERNAL Unix__TIOCCINUSE*> VAR TIOCCINUSE: int;
(*CONST*) <*EXTERNAL Unix__TIOCCMLB*> VAR TIOCCMLB: int;
(*CONST*) <*EXTERNAL Unix__TIOCCONS*> VAR TIOCCONS: int;
(*CONST*) <*EXTERNAL Unix__TIOCDCDTIMESTAMP*> VAR TIOCDCDTIMESTAMP: int;
(*CONST*) <*EXTERNAL Unix__TIOCDRAIN*> VAR TIOCDRAIN: int;
(*CONST*) <*EXTERNAL Unix__TIOCDSIMICROCODE*> VAR TIOCDSIMICROCODE: int;
(*CONST*) <*EXTERNAL Unix__TIOCEXCL*> VAR TIOCEXCL: int;
(*CONST*) <*EXTERNAL Unix__TIOCEXT*> VAR TIOCEXT: int;
(*CONST*) <*EXTERNAL Unix__TIOCFLUSH*> VAR TIOCFLUSH: int;
(*CONST*) <*EXTERNAL Unix__TIOCGDRAINWAIT*> VAR TIOCGDRAINWAIT: int;
(*CONST*) <*EXTERNAL Unix__TIOCGETA*> VAR TIOCGETA: int;
(*CONST*) <*EXTERNAL Unix__TIOCGETC*> VAR TIOCGETC: int;
(*CONST*) <*EXTERNAL Unix__TIOCGETD*> VAR TIOCGETD: int;
(*CONST*) <*EXTERNAL Unix__TIOCGETP*> VAR TIOCGETP: int;
(*CONST*) <*EXTERNAL Unix__TIOCGLTC*> VAR TIOCGLTC: int;
(*CONST*) <*EXTERNAL Unix__TIOCGPGRP*> VAR TIOCGPGRP: int;
(*CONST*) <*EXTERNAL Unix__TIOCGSID*> VAR TIOCGSID: int;
(*CONST*) <*EXTERNAL Unix__TIOCGWINSZ*> VAR TIOCGWINSZ: int;
(*CONST*) <*EXTERNAL Unix__TIOCHPCL*> VAR TIOCHPCL: int;
(*CONST*) <*EXTERNAL Unix__TIOCIXOFF*> VAR TIOCIXOFF: int;
(*CONST*) <*EXTERNAL Unix__TIOCIXON*> VAR TIOCIXON: int;
(*CONST*) <*EXTERNAL Unix__TIOCLBIC*> VAR TIOCLBIC: int;
(*CONST*) <*EXTERNAL Unix__TIOCLBIS*> VAR TIOCLBIS: int;
(*CONST*) <*EXTERNAL Unix__TIOCLGET*> VAR TIOCLGET: int;
(*CONST*) <*EXTERNAL Unix__TIOCLSET*> VAR TIOCLSET: int;
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
(*CONST*) <*EXTERNAL Unix__TIOCMASTER*> VAR TIOCMASTER: int;
(*CONST*) <*EXTERNAL Unix__TIOCMBIC*> VAR TIOCMBIC: int;
(*CONST*) <*EXTERNAL Unix__TIOCMBIS*> VAR TIOCMBIS: int;
(*CONST*) <*EXTERNAL Unix__TIOCMGDTRWAIT*> VAR TIOCMGDTRWAIT: int;
(*CONST*) <*EXTERNAL Unix__TIOCMGET*> VAR TIOCMGET: int;
(*CONST*) <*EXTERNAL Unix__TIOCMODEM*> VAR TIOCMODEM: int;
(*CONST*) <*EXTERNAL Unix__TIOCMODG*> VAR TIOCMODG: int;
(*CONST*) <*EXTERNAL Unix__TIOCMODS*> VAR TIOCMODS: int;
(*CONST*) <*EXTERNAL Unix__TIOCMSDTRWAIT*> VAR TIOCMSDTRWAIT: int;
(*CONST*) <*EXTERNAL Unix__TIOCMSET*> VAR TIOCMSET: int;
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
(*CONST*) <*EXTERNAL Unix__TIOCPTYGNAME*> VAR TIOCPTYGNAME: int;
(*CONST*) <*EXTERNAL Unix__TIOCPTYGRANT*> VAR TIOCPTYGRANT: int;
(*CONST*) <*EXTERNAL Unix__TIOCPTYUNLK*> VAR TIOCPTYUNLK: int;
(*CONST*) <*EXTERNAL Unix__TIOCREMOTE*> VAR TIOCREMOTE: int;
(*CONST*) <*EXTERNAL Unix__TIOCSBRK*> VAR TIOCSBRK: int;
(*CONST*) <*EXTERNAL Unix__TIOCSCONS*> VAR TIOCSCONS: int;
(*CONST*) <*EXTERNAL Unix__TIOCSCTTY*> VAR TIOCSCTTY: int;
(*CONST*) <*EXTERNAL Unix__TIOCSDRAINWAIT*> VAR TIOCSDRAINWAIT: int;
(*CONST*) <*EXTERNAL Unix__TIOCSDTR*> VAR TIOCSDTR: int;
(*CONST*) <*EXTERNAL Unix__TIOCSETA*> VAR TIOCSETA: int;
(*CONST*) <*EXTERNAL Unix__TIOCSETAF*> VAR TIOCSETAF: int;
(*CONST*) <*EXTERNAL Unix__TIOCSETAW*> VAR TIOCSETAW: int;
(*CONST*) <*EXTERNAL Unix__TIOCSETC*> VAR TIOCSETC: int;
(*CONST*) <*EXTERNAL Unix__TIOCSETD*> VAR TIOCSETD: int;
(*CONST*) <*EXTERNAL Unix__TIOCSETN*> VAR TIOCSETN: int;
(*CONST*) <*EXTERNAL Unix__TIOCSETP*> VAR TIOCSETP: int;
(*CONST*) <*EXTERNAL Unix__TIOCSIG*> VAR TIOCSIG: int;
(*CONST*) <*EXTERNAL Unix__TIOCSINUSE*> VAR TIOCSINUSE: int;
(*CONST*) <*EXTERNAL Unix__TIOCSLTC*> VAR TIOCSLTC: int;
(*CONST*) <*EXTERNAL Unix__TIOCSMLB*> VAR TIOCSMLB: int;
(*CONST*) <*EXTERNAL Unix__TIOCSPGRP*> VAR TIOCSPGRP: int;
(*CONST*) <*EXTERNAL Unix__TIOCSTART*> VAR TIOCSTART: int;
(*CONST*) <*EXTERNAL Unix__TIOCSTAT*> VAR TIOCSTAT: int;
(*CONST*) <*EXTERNAL Unix__TIOCSTI*> VAR TIOCSTI: int;
(*CONST*) <*EXTERNAL Unix__TIOCSTOP*> VAR TIOCSTOP: int;
(*CONST*) <*EXTERNAL Unix__TIOCSWINSZ*> VAR TIOCSWINSZ: int;
(*CONST*) <*EXTERNAL Unix__TIOCTIMESTAMP*> VAR TIOCTIMESTAMP: int;
(*CONST*) <*EXTERNAL Unix__TIOCUCNTL*> VAR TIOCUCNTL: int;
(*CONST*) <*EXTERNAL Unix__TIOCWONLINE*> VAR TIOCWONLINE: int;
(*CONST*) <*EXTERNAL Unix__TOSTOP*> VAR TOSTOP: int;
(*CONST*) <*EXTERNAL Unix__TTYDISC*> VAR TTYDISC: int;
(*CONST*) <*EXTERNAL Unix__VTDELAY*> VAR VTDELAY: int;
(*CONST*) <*EXTERNAL Unix__XTABS*> VAR XTABS: int;

END Unix.
