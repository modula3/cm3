(* Copyright (c) 1998, Richard Watts                          *)
(* See the COPYRIGHT file for licence info                    *)
(*                                                            *)
(* Last modified on Mon Jan  5 11:47:54 GMT 1998 by rrw       *)

INTERFACE Upaths;

(* paths.h *)

CONST
  DEFAULT = "/usr/bin:/bin";
  STDPATH = "/usr/bin:/bin:/usr/sbin:/sbin";
  BSHELL = "/bin/sh";
  CONSOLE = "/dev/console";
  CSHELL = "/bin/csh";
  DEVDB = "/var/run/dev.db";
  DEVNULL = "/dev/null";
  DRUM = "/dev/drum";
  KLOG = "/proc/kmsg";
  KMEM = "/dev/kmem";
  LASTLOG = "/var/log/lastlog";
  MAILDIR = "/var/spool/mail";
  MAN = "/usr/man";
  MEM = "/dev/mem";
  MNTTAB = "/etc/fstab";
  MOUNTED = "/etc/mtab";
  NOLOGIN = "/etc/nologin";
  PRESERVE = "/var/preserve";
  SENDMAIL = "/usr/sbin/sendmail";
  SHADOW = "/etc/shadow";
  SHELLS = "/etc/shells";
  TTY = "/dev/tty";
  UNIX = "/vmlinux";
  UTMP = "/var/run/utmp";
  UTMP_DB = "/var/run/utmp.db";
  VI = "/usr/bin/vi";
  WTMP = "/var/log/wtmp";

  (* Provide trailing slash, since mostly used for building pathnames *)
  DEV = "/dev/";
  TMP = "/tmp/";
  VARDB = "/var/db/";
  VARRUN = "/var/run/";
  VARTMP = "/var/tmp/";

END Upaths.
 
