(* Copyright (C) 1993, Digital Equipment Corporation                  *)
(* All rights reserved.                                               *)
(* See the file COPYRIGHT for a full description.                     *)

INTERFACE Unix;

FROM Cstddef IMPORT size_t;
FROM Ctypes IMPORT const_int, int, const_char_star, char_star, char_star_star, unsigned;
FROM Utypes IMPORT off_t, mode_t, dev_t, uid_t, gid_t;

CONST
  MaxPathLen = 1024; (* 4096 on Irix? *)

<*EXTERNAL "Unix__MSETUID"*> VAR MSETUID: const_int; (* set user id on execution *)
<*EXTERNAL "Unix__MSETGID"*> VAR MSETGID: const_int; (* set group id on execution *)
<*EXTERNAL "Unix__MSTICKY"*> VAR MSTICKY: const_int; (* save swapped text even after use *)

(* owner *)
<*EXTERNAL "Unix__MROWNER"*> VAR MROWNER: const_int; (* readable by owner *)
<*EXTERNAL "Unix__MWOWNER"*> VAR MWOWNER: const_int; (* writable by owner *)
<*EXTERNAL "Unix__MXOWNER"*> VAR MXOWNER: const_int; (* executable by owner *)
(* group *)
<*EXTERNAL "Unix__MRGROUP"*> VAR MRGROUP: const_int; (* readable by group *)
<*EXTERNAL "Unix__MWGROUP"*> VAR MWGROUP: const_int; (* writable by group *)
<*EXTERNAL "Unix__MXGROUP"*> VAR MXGROUP: const_int; (* executable by group *)
(* other *)
<*EXTERNAL "Unix__MROTHER"*> VAR MROTHER: const_int; (* readable by other *)
<*EXTERNAL "Unix__MWOTHER"*> VAR MWOTHER: const_int; (* writable by other *)
<*EXTERNAL "Unix__MXOTHER"*> VAR MXOTHER: const_int; (* executable by other *)

(* readable/writable by all, executable by none *)
<*EXTERNAL "Unix__Mrwrwrw"*> VAR Mrwrwrw: const_int; (* MROWNER + MWOWNER + MRGROUP + MWGROUP + MROTHER + MWOTHER *)

<*EXTERNAL "Unix__F_OK"*> VAR F_OK: const_int; (* existance *)
<*EXTERNAL "Unix__X_OK"*> VAR X_OK: const_int; (* executable *)
<*EXTERNAL "Unix__W_OK"*> VAR W_OK: const_int; (* writable *)
<*EXTERNAL "Unix__R_OK"*> VAR R_OK: const_int; (* readable *)
<*EXTERNAL "Unix__access"*>PROCEDURE access (path: const_char_star; mode: int): int;

<*EXTERNAL "Unix__sbrk"*>PROCEDURE sbrk (inc: INTEGER): char_star;
<*EXTERNAL "Unix__chdir"*>PROCEDURE chdir (path: const_char_star): int;
<*EXTERNAL "Unix__close"*>PROCEDURE close (d: int): int;
<*EXTERNAL "Unix__dup"*>PROCEDURE dup (oldd: int): int;
<*EXTERNAL "Unix__dup2"*>PROCEDURE dup2 (oldd, newd: int): int;
<*EXTERNAL "Unix__execve"*>PROCEDURE execve (name: const_char_star;  argv, envp: char_star_star): int;

<*EXTERNAL "Unix__exit"*>PROCEDURE exit (i: int);
<*EXTERNAL "Unix__underscore_exit"*>PROCEDURE underscore_exit (i: int);

<*EXTERNAL "Unix__F_DUPFD"*> VAR F_DUPFD: const_int;      (* Duplicate fd *)
<*EXTERNAL "Unix__F_GETFD"*> VAR F_GETFD: const_int;      (* Get close-on-exec flag *)
<*EXTERNAL "Unix__F_SETFD"*> VAR F_SETFD: const_int;      (* Set close-on-exec flag *)
<*EXTERNAL "Unix__F_GETFL"*> VAR F_GETFL: const_int;      (* Get fd status flags *)
<*EXTERNAL "Unix__F_SETFL"*> VAR F_SETFL: const_int;      (* Set fd status flags *)
<*EXTERNAL "Unix__F_GETOWN"*> VAR F_GETOWN: const_int;    (* Set owner *)
<*EXTERNAL "Unix__F_SETOWN"*> VAR F_SETOWN: const_int;    (* Get owner *)
<*EXTERNAL "Unix__F_GETLK"*> VAR F_GETLK: const_int;      (* Get file lock *)
<*EXTERNAL "Unix__F_SETLK"*> VAR F_SETLK: const_int;      (* Set file lock *)
<*EXTERNAL "Unix__F_SETLKW"*> VAR F_SETLKW: const_int;    (* Set file lock and wait *)
<*EXTERNAL "Unix__FD_CLOEXEC"*> VAR FD_CLOEXEC: const_int; (* Close file descriptor on exec() *)

TYPE struct_flock = RECORD
(* sorted by size and then name
   This must match between Unix.i3 and UnixC.c. *)
  l_len:    LONGINT := 0L;
  l_start:  LONGINT := 0L;
  l_pid:    INTEGER := 0;
  l_type:   INTEGER := 0;
  l_whence: INTEGER := 0;
END;

(* TODO replace this with higher level constructs *)
<*EXTERNAL "Unix__fcntl"*>PROCEDURE fcntl (fd: int; request, arg: INTEGER): int;

<*EXTERNAL "Unix__fsync"*>PROCEDURE fsync (fd: int): int;
<*EXTERNAL "Unix__getdtablesize"*>PROCEDURE getdtablesize (): int;
<*EXTERNAL "Unix__gethostname"*>PROCEDURE gethostname (name: char_star; namelen: size_t): int;
<*EXTERNAL "Unix__getpagesize"*>PROCEDURE getpagesize (): int;
<*EXTERNAL "Unix__getcwd"*>PROCEDURE getcwd (pathname: char_star; size: size_t): char_star;

<*EXTERNAL "Unix__FIONREAD"*> VAR FIONREAD: const_int;

(* TODO replace this with higher level constructs *)
<*EXTERNAL "Unix__ioctl"*> PROCEDURE ioctl (d: int; request: INTEGER; argp: ADDRESS): int;

<*EXTERNAL "Unix__lseek"*>
PROCEDURE lseek (d: int; offset: off_t; whence: int): off_t;

<*EXTERNAL "Unix__tell"*>
PROCEDURE tell (d: int): off_t;

<*EXTERNAL "Unix__mkdir"*>
PROCEDURE mkdir (path: const_char_star; mode: mode_t): int;

<*EXTERNAL "Unix__O_RDONLY"*>    VAR O_RDONLY: const_int;
<*EXTERNAL "Unix__O_RDWR"*>      VAR O_RDWR: const_int;
<*EXTERNAL "Unix__O_CREAT"*>     VAR O_CREAT: const_int;
<*EXTERNAL "Unix__O_EXCL"*>      VAR O_EXCL: const_int;
<*EXTERNAL "Unix__O_TRUNC"*>     VAR O_TRUNC: const_int;
<*EXTERNAL "Unix__O_NONBLOCK"*>  VAR O_NONBLOCK: const_int;
<*EXTERNAL "Unix__O_APPEND"*>    VAR O_APPEND: const_int; (* append on each write *)
<*EXTERNAL "Unix__O_NDELAY"*>    VAR O_NDELAY: const_int; (* compat *)
<*EXTERNAL "Unix__M3_NONBLOCK"*> VAR M3_NONBLOCK: const_int; (* compat *)

<*EXTERNAL "Unix__open"*>
PROCEDURE open (name: const_char_star; flags: int; mode: mode_t): int;

CONST
  readEnd = 0;
  writeEnd = 1;
PROCEDURE pipe (VAR fildes: ARRAY [0..1] OF int): int;

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
 * callers of vfork cannot return without calling exec or _exit.
 * vfork must return int, not pid_t. C compilation will fail
 * otherwise when combining m3c output with hand written C (m3core.h).
 *
 * Do not declare vfork. The type is not always int (sometimes long on Solaris)
 * and there are errors if it is wrong. And the documentation is extremely caveat filled.
 *
 * If you must call vfork, do it from C.
 *
<*EXTERNAL*>
PROCEDURE vfork (): int;
 *)

<*EXTERNAL "Unix__mknod"*>
PROCEDURE mknod (path: const_char_star; mode: mode_t; dev: dev_t): int;

<*EXTERNAL "Unix__umask"*>
PROCEDURE umask (numask: mode_t): mode_t;

<*EXTERNAL "Unix__link"*>
PROCEDURE link (name1, name2: const_char_star): int;
<*EXTERNAL "Unix__chmod"*>
PROCEDURE chmod (path: const_char_star; mode: mode_t): int;
<*EXTERNAL "Unix__fchmod"*>
PROCEDURE fchmod (fd: int; mode: mode_t): int;
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

<*EXTERNAL Unix__ALLDELAY*> VAR ALLDELAY: const_int;
<*EXTERNAL Unix__ANYP*> VAR ANYP: const_int;
<*EXTERNAL Unix__BS0*> VAR BS0: const_int;
<*EXTERNAL Unix__BS1*> VAR BS1: const_int;
<*EXTERNAL Unix__BSDELAY*> VAR BSDELAY: const_int;
<*EXTERNAL Unix__CBREAK*> VAR CBREAK: const_int;
<*EXTERNAL Unix__CR0*> VAR CR0: const_int;
<*EXTERNAL Unix__CR1*> VAR CR1: const_int;
<*EXTERNAL Unix__CR2*> VAR CR2: const_int;
<*EXTERNAL Unix__CR3*> VAR CR3: const_int;
<*EXTERNAL Unix__CRDELAY*> VAR CRDELAY: const_int;
<*EXTERNAL Unix__CRMOD*> VAR CRMOD: const_int;
<*EXTERNAL Unix__CRTBS*> VAR CRTBS: const_int;
<*EXTERNAL Unix__CRTERA*> VAR CRTERA: const_int;
<*EXTERNAL Unix__CRTKIL*> VAR CRTKIL: const_int;
<*EXTERNAL Unix__CTLECH*> VAR CTLECH: const_int;
<*EXTERNAL Unix__DECCTQ*> VAR DECCTQ: const_int;
<*EXTERNAL Unix__ECHO*> VAR ECHO: const_int;
<*EXTERNAL Unix__EVENP*> VAR EVENP: const_int;
<*EXTERNAL Unix__FF0*> VAR FF0: const_int;
<*EXTERNAL Unix__FF1*> VAR FF1: const_int;
<*EXTERNAL Unix__FLUSHO*> VAR FLUSHO: const_int;
<*EXTERNAL Unix__L001000*> VAR L001000: const_int;
<*EXTERNAL Unix__LCASE*> VAR LCASE: const_int;
<*EXTERNAL Unix__LCRTBS*> VAR LCRTBS: const_int;
<*EXTERNAL Unix__LCRTERA*> VAR LCRTERA: const_int;
<*EXTERNAL Unix__LCRTKIL*> VAR LCRTKIL: const_int;
<*EXTERNAL Unix__LCTLECH*> VAR LCTLECH: const_int;
<*EXTERNAL Unix__LDECCTQ*> VAR LDECCTQ: const_int;
<*EXTERNAL Unix__LFLUSHO*> VAR LFLUSHO: const_int;
<*EXTERNAL Unix__LITOUT*> VAR LITOUT: const_int;
<*EXTERNAL Unix__LLITOUT*> VAR LLITOUT: const_int;
<*EXTERNAL Unix__LMDMBUF*> VAR LMDMBUF: const_int;
<*EXTERNAL Unix__LNOFLSH*> VAR LNOFLSH: const_int;
<*EXTERNAL Unix__LNOHANG*> VAR LNOHANG: const_int;
<*EXTERNAL Unix__LPASS8*> VAR LPASS8: const_int;
<*EXTERNAL Unix__LPENDIN*> VAR LPENDIN: const_int;
<*EXTERNAL Unix__LPRTERA*> VAR LPRTERA: const_int;
<*EXTERNAL Unix__LTILDE*> VAR LTILDE: const_int;
<*EXTERNAL Unix__LTOSTOP*> VAR LTOSTOP: const_int;
<*EXTERNAL Unix__MDMBUF*> VAR MDMBUF: const_int;
<*EXTERNAL Unix__NETLDISC*> VAR NETLDISC: const_int;
<*EXTERNAL Unix__NL0*> VAR NL0: const_int;
<*EXTERNAL Unix__NL1*> VAR NL1: const_int;
<*EXTERNAL Unix__NL2*> VAR NL2: const_int;
<*EXTERNAL Unix__NL3*> VAR NL3: const_int;
<*EXTERNAL Unix__NLDELAY*> VAR NLDELAY: const_int;
<*EXTERNAL Unix__NOFLSH*> VAR NOFLSH: const_int;
<*EXTERNAL Unix__NOHANG*> VAR NOHANG: const_int;
<*EXTERNAL Unix__NTTYDISC*> VAR NTTYDISC: const_int;
<*EXTERNAL Unix__ODDP*> VAR ODDP: const_int;
<*EXTERNAL Unix__OTIOCCONS*> VAR OTIOCCONS: const_int;
<*EXTERNAL Unix__OTIOCGETD*> VAR OTIOCGETD: const_int;
<*EXTERNAL Unix__OTIOCSETD*> VAR OTIOCSETD: const_int;
<*EXTERNAL Unix__OTTYDISC*> VAR OTTYDISC: const_int;
<*EXTERNAL Unix__PASS8*> VAR PASS8: const_int;
<*EXTERNAL Unix__PENDIN*> VAR PENDIN: const_int;
<*EXTERNAL Unix__PRTERA*> VAR PRTERA: const_int;
<*EXTERNAL Unix__RAW*> VAR RAW: const_int;
<*EXTERNAL Unix__TAB0*> VAR TAB0: const_int;
<*EXTERNAL Unix__TAB1*> VAR TAB1: const_int;
<*EXTERNAL Unix__TAB2*> VAR TAB2: const_int;
<*EXTERNAL Unix__TANDEM*> VAR TANDEM: const_int;
<*EXTERNAL Unix__TBDELAY*> VAR TBDELAY: const_int;
<*EXTERNAL Unix__TERMIODISC*> VAR TERMIODISC: const_int;
<*EXTERNAL Unix__TILDE*> VAR TILDE: const_int;
<*EXTERNAL Unix__TIOCCAR*> VAR TIOCCAR: const_int;
<*EXTERNAL Unix__TIOCCBRK*> VAR TIOCCBRK: const_int;
<*EXTERNAL Unix__TIOCCDTR*> VAR TIOCCDTR: const_int;
<*EXTERNAL Unix__TIOCCINUSE*> VAR TIOCCINUSE: const_int;
<*EXTERNAL Unix__TIOCCMLB*> VAR TIOCCMLB: const_int;
<*EXTERNAL Unix__TIOCCONS*> VAR TIOCCONS: const_int;
<*EXTERNAL Unix__TIOCDCDTIMESTAMP*> VAR TIOCDCDTIMESTAMP: const_int;
<*EXTERNAL Unix__TIOCDRAIN*> VAR TIOCDRAIN: const_int;
<*EXTERNAL Unix__TIOCDSIMICROCODE*> VAR TIOCDSIMICROCODE: const_int;
<*EXTERNAL Unix__TIOCEXCL*> VAR TIOCEXCL: const_int;
<*EXTERNAL Unix__TIOCEXT*> VAR TIOCEXT: const_int;
<*EXTERNAL Unix__TIOCFLUSH*> VAR TIOCFLUSH: const_int;
<*EXTERNAL Unix__TIOCGDRAINWAIT*> VAR TIOCGDRAINWAIT: const_int;
<*EXTERNAL Unix__TIOCGETA*> VAR TIOCGETA: const_int;
<*EXTERNAL Unix__TIOCGETC*> VAR TIOCGETC: const_int;
<*EXTERNAL Unix__TIOCGETD*> VAR TIOCGETD: const_int;
<*EXTERNAL Unix__TIOCGETP*> VAR TIOCGETP: const_int;
<*EXTERNAL Unix__TIOCGLTC*> VAR TIOCGLTC: const_int;
<*EXTERNAL Unix__TIOCGPGRP*> VAR TIOCGPGRP: const_int;
<*EXTERNAL Unix__TIOCGSID*> VAR TIOCGSID: const_int;
<*EXTERNAL Unix__TIOCGWINSZ*> VAR TIOCGWINSZ: const_int;
<*EXTERNAL Unix__TIOCHPCL*> VAR TIOCHPCL: const_int;
<*EXTERNAL Unix__TIOCIXOFF*> VAR TIOCIXOFF: const_int;
<*EXTERNAL Unix__TIOCIXON*> VAR TIOCIXON: const_int;
<*EXTERNAL Unix__TIOCLBIC*> VAR TIOCLBIC: const_int;
<*EXTERNAL Unix__TIOCLBIS*> VAR TIOCLBIS: const_int;
<*EXTERNAL Unix__TIOCLGET*> VAR TIOCLGET: const_int;
<*EXTERNAL Unix__TIOCLSET*> VAR TIOCLSET: const_int;
<*EXTERNAL Unix__TIOCM_CAR*> VAR TIOCM_CAR: const_int;
<*EXTERNAL Unix__TIOCM_CD*> VAR TIOCM_CD: const_int;
<*EXTERNAL Unix__TIOCM_CTS*> VAR TIOCM_CTS: const_int;
<*EXTERNAL Unix__TIOCM_DSR*> VAR TIOCM_DSR: const_int;
<*EXTERNAL Unix__TIOCM_DTR*> VAR TIOCM_DTR: const_int;
<*EXTERNAL Unix__TIOCM_LE*> VAR TIOCM_LE: const_int;
<*EXTERNAL Unix__TIOCM_RI*> VAR TIOCM_RI: const_int;
<*EXTERNAL Unix__TIOCM_RNG*> VAR TIOCM_RNG: const_int;
<*EXTERNAL Unix__TIOCM_RTS*> VAR TIOCM_RTS: const_int;
<*EXTERNAL Unix__TIOCM_SR*> VAR TIOCM_SR: const_int;
<*EXTERNAL Unix__TIOCM_ST*> VAR TIOCM_ST: const_int;
<*EXTERNAL Unix__TIOCMASTER*> VAR TIOCMASTER: const_int;
<*EXTERNAL Unix__TIOCMBIC*> VAR TIOCMBIC: const_int;
<*EXTERNAL Unix__TIOCMBIS*> VAR TIOCMBIS: const_int;
<*EXTERNAL Unix__TIOCMGDTRWAIT*> VAR TIOCMGDTRWAIT: const_int;
<*EXTERNAL Unix__TIOCMGET*> VAR TIOCMGET: const_int;
<*EXTERNAL Unix__TIOCMODEM*> VAR TIOCMODEM: const_int;
<*EXTERNAL Unix__TIOCMODG*> VAR TIOCMODG: const_int;
<*EXTERNAL Unix__TIOCMODS*> VAR TIOCMODS: const_int;
<*EXTERNAL Unix__TIOCMSDTRWAIT*> VAR TIOCMSDTRWAIT: const_int;
<*EXTERNAL Unix__TIOCMSET*> VAR TIOCMSET: const_int;
<*EXTERNAL Unix__TIOCNCAR*> VAR TIOCNCAR: const_int;
<*EXTERNAL Unix__TIOCNMODEM*> VAR TIOCNMODEM: const_int;
<*EXTERNAL Unix__TIOCNOTTY*> VAR TIOCNOTTY: const_int;
<*EXTERNAL Unix__TIOCNXCL*> VAR TIOCNXCL: const_int;
<*EXTERNAL Unix__TIOCOUTQ*> VAR TIOCOUTQ: const_int;
<*EXTERNAL Unix__TIOCPKT*> VAR TIOCPKT: const_int;
<*EXTERNAL Unix__TIOCPKT_DATA*> VAR TIOCPKT_DATA: const_int;
<*EXTERNAL Unix__TIOCPKT_DOSTOP*> VAR TIOCPKT_DOSTOP: const_int;
<*EXTERNAL Unix__TIOCPKT_FLUSHREAD*> VAR TIOCPKT_FLUSHREAD: const_int;
<*EXTERNAL Unix__TIOCPKT_FLUSHWRITE*> VAR TIOCPKT_FLUSHWRITE: const_int;
<*EXTERNAL Unix__TIOCPKT_IOCTL*> VAR TIOCPKT_IOCTL: const_int;
<*EXTERNAL Unix__TIOCPKT_NOSTOP*> VAR TIOCPKT_NOSTOP: const_int;
<*EXTERNAL Unix__TIOCPKT_START*> VAR TIOCPKT_START: const_int;
<*EXTERNAL Unix__TIOCPKT_STOP*> VAR TIOCPKT_STOP: const_int;
<*EXTERNAL Unix__TIOCPTYGNAME*> VAR TIOCPTYGNAME: const_int;
<*EXTERNAL Unix__TIOCPTYGRANT*> VAR TIOCPTYGRANT: const_int;
<*EXTERNAL Unix__TIOCPTYUNLK*> VAR TIOCPTYUNLK: const_int;
<*EXTERNAL Unix__TIOCREMOTE*> VAR TIOCREMOTE: const_int;
<*EXTERNAL Unix__TIOCSBRK*> VAR TIOCSBRK: const_int;
<*EXTERNAL Unix__TIOCSCONS*> VAR TIOCSCONS: const_int;
<*EXTERNAL Unix__TIOCSCTTY*> VAR TIOCSCTTY: const_int;
<*EXTERNAL Unix__TIOCSDRAINWAIT*> VAR TIOCSDRAINWAIT: const_int;
<*EXTERNAL Unix__TIOCSDTR*> VAR TIOCSDTR: const_int;
<*EXTERNAL Unix__TIOCSETA*> VAR TIOCSETA: const_int;
<*EXTERNAL Unix__TIOCSETAF*> VAR TIOCSETAF: const_int;
<*EXTERNAL Unix__TIOCSETAW*> VAR TIOCSETAW: const_int;
<*EXTERNAL Unix__TIOCSETC*> VAR TIOCSETC: const_int;
<*EXTERNAL Unix__TIOCSETD*> VAR TIOCSETD: const_int;
<*EXTERNAL Unix__TIOCSETN*> VAR TIOCSETN: const_int;
<*EXTERNAL Unix__TIOCSETP*> VAR TIOCSETP: const_int;
<*EXTERNAL Unix__TIOCSIG*> VAR TIOCSIG: const_int;
<*EXTERNAL Unix__TIOCSINUSE*> VAR TIOCSINUSE: const_int;
<*EXTERNAL Unix__TIOCSLTC*> VAR TIOCSLTC: const_int;
<*EXTERNAL Unix__TIOCSMLB*> VAR TIOCSMLB: const_int;
<*EXTERNAL Unix__TIOCSPGRP*> VAR TIOCSPGRP: const_int;
<*EXTERNAL Unix__TIOCSTART*> VAR TIOCSTART: const_int;
<*EXTERNAL Unix__TIOCSTAT*> VAR TIOCSTAT: const_int;
<*EXTERNAL Unix__TIOCSTI*> VAR TIOCSTI: const_int;
<*EXTERNAL Unix__TIOCSTOP*> VAR TIOCSTOP: const_int;
<*EXTERNAL Unix__TIOCSWINSZ*> VAR TIOCSWINSZ: const_int;
<*EXTERNAL Unix__TIOCTIMESTAMP*> VAR TIOCTIMESTAMP: const_int;
<*EXTERNAL Unix__TIOCUCNTL*> VAR TIOCUCNTL: const_int;
<*EXTERNAL Unix__TIOCWONLINE*> VAR TIOCWONLINE: const_int;
<*EXTERNAL Unix__TOSTOP*> VAR TOSTOP: const_int;
<*EXTERNAL Unix__TTYDISC*> VAR TTYDISC: const_int;
<*EXTERNAL Unix__VTDELAY*> VAR VTDELAY: const_int;
<*EXTERNAL Unix__XTABS*> VAR XTABS: const_int;

END Unix.
