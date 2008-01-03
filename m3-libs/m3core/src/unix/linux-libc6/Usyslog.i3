(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Jan  5 11:35:29 GMT 1998 by rrw                      *)
(*      modified on Fri Feb 24 15:22:53 PST 1995 by kalsow                   *)
(*      modified on Tue Feb 14 21:13:50 GMT 1995 by rrw1000@cam.ac.uk        *)
(*      modified on Tue Mar 24 22:25:39 PST 1992 by muller                   *)
(*      modified on Mon Dec 16 09:46:14 PST 1991 by harrison                 *)

INTERFACE Usyslog;

(*
 * These declarations are taken from <syslog.h>
 *
 * You can't quite do everything from Modula-3 because syslog is like printf.
 *
 * Use the Syslog package instead--it's built on top of this and works better
 * from Modula-3.
 *)

FROM Ctypes IMPORT char_star, int;

(* defines for priorities *)
CONST
(*******
  LOG_ALERT = 1;	(* alert -- send to all users *)
  LOG_SALERT = 2;	(* subalert -- send to special users *)
  LOG_EMERG = 3;	(* emergency conditions *)
  LOG_ERROR = 4;	(* error *)
  LOG_ERR = LOG_ERROR; 	(* synonym of LOG_ERROR *)
  LOG_CRIT = 5;	      	(* critical information *)
  LOG_WARNING = 6;	(* warning *)
  LOG_NOTICE = 7;	(* important information *)
  LOG_INFO = 8;	      	(* informational message *)
  LOG_DEBUG = 9;	(* debug level info *)
******)

(* Valid LOG_* definitions for Linux 1.1.73 are :  - rrw *)

    LOG_EMERG = 0;	(* emergency conditions *)
    LOG_ALERT = 1;	(* alert -- send to all users *)
    LOG_CRIT  = 2;     	(* critical information *)
    LOG_ERR   = 3; 	(* error *)
    LOG_WARNING = 4;	(* warning *)
    LOG_NOTICE = 5;	(* important information *)
    LOG_INFO = 6;     	(* informational message *)
    LOG_DEBUG = 7;	(* debug level info *)

    LOG_PRIMASK=16_07; (* Mask to extract priority part *)

(*
 *  Mode parameters to initlog.
 *)
CONST
(* May not be implemented under Linux 1.1.73 - use with care  - rrw *)
  LOG_NULL = 0;	      	(* don't touch log *)
  LOG_SOUT = 1;	      	(* log standard & diag output *)
  LOG_DIAG = 2;	      	(* log diagnostic output *)
  LOG_INDEP = 3;	(* log independently *)
  LOG_CLOSE = 4;	(* close the log *)

CONST
  SVC_OFFSET = 16_08;

 (* Facility codes for Linux *)
  LOG_KERN = 0;
  LOG_USER = 1*SVC_OFFSET;
  LOG_MAIL = 2*SVC_OFFSET;
  LOG_DAEMON = 3*SVC_OFFSET;
  LOG_AUTH = 4*SVC_OFFSET;
  LOG_SYSLOG = 5*SVC_OFFSET;
  LOG_LPR = 6*SVC_OFFSET;
  LOG_NEWS = 7*SVC_OFFSET;
  LOG_UUCP = 8*SVC_OFFSET;
  LOG_CRON = 9*SVC_OFFSET;
  LOG_AUTHPRIV = 10*SVC_OFFSET;
  LOG_FTP = 11*SVC_OFFSET;
  
  (* 12-15 reserved for system use *)
  (* 16 - 23 are reserved for local use *)

(*
 *  Status parameters to initlog.
 *)
CONST
  LOG_PID = 16_0001;	(* log the pid with each message *)
  (* LOG_TIME, LOG_COOLIT, LOG_DGRAM  may not be implemented - rrw *)
  LOG_CONS = 16_02; (* Log on console if errors in sending *)
  LOG_ODELAY = 16_0004;   (* delay open until first syslog() (default) *)
  LOG_NDELAY = 16_0008;	(* don't delay open *)
  LOG_NOWAIT = 16_0010;	(* don't wait for console forks: DEPRECATED *)
  LOG_PERROR = 16_20; (* Log to stderr as well *)

<* EXTERNAL *>
PROCEDURE openlog(
  VALUE ident: char_star;
  VALUE logstat: int);

<* EXTERNAL *>
PROCEDURE syslog(
  VALUE priority: int;
  VALUE message: char_star);

<* EXTERNAL *>
PROCEDURE closelog();

END Usyslog.
