(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* File: Uutmp.i3                                             *)
(* Last modified on Wed Jun 27 17:58:50 1990 by piet@cs.ruu.nl *)
(*      modified on Mon Apr 16 15:59:54 1990 by jerome        *)

INTERFACE Uutmp;

FROM Ctypes IMPORT char, char_star, short, unsigned_long;
FROM Utypes IMPORT pid_t, u_short, time_t;

(*** <utmp.h> ***)

(*
 * Structure of utmp and wtmp files.
 *
 *)

TYPE
    struct_utmp = RECORD
	ut_name: ARRAY [0..7] OF char;		(* user id *)
	ut_id:   ARRAY [0..3] OF char;		(* id *)
	ut_line: ARRAY [0..11] OF char;		(* tty name *)
	ut_pid:  pid_t;
	ut_type: short;
	ut_exit: RECORD e_termination, e_exit: short END;
	ut_reserved1: u_short;
	ut_time: time_t;			(* time on *)
	ut_host: ARRAY [0..15] OF char;		(* host name, if remote *)
	ut_addr: unsigned_long;
     END;

CONST
   UTMP_FILE	= "/etc/utmp";
   WTMP_FILE	= "/etc/wtmp";
   BTMP_FILE	= "/etc/btmp";

(*	Definitions for ut_type						*)

  EMPTY =		0;
  RUN_LVL =		1;
  BOOT_TIME =		2;
  OLD_TIME =		3;
  NEW_TIME =		4;
  INIT_PROCESS =	5;	(* Process spawned by "init" *)
  LOGIN_PROCESS =	6;	(* A "getty" process waiting for login *)
  USER_PROCESS =	7;	(* A user process *)
  DEAD_PROCESS =	8;
  ACCOUNTING =		9;
  UTMAXTYPE =	ACCOUNTING;   (* Largest legal value of ut_type *)


(*	Special strings or formats used in the "ut_line" field when	*)
(*	accounting for something other than a process.			*)
(*	No string for the ut_line field can be more than 11 chars +	*)
(*	a NULL in length.						*)

  RUNLVL_MSG =	"run-level %c";
  BOOT_MSG =	"system boot";
  OTIME_MSG =	"old time";
  NTIME_MSG =	"new time";


(*** getlogin(3) ***)

<*EXTERNAL*> PROCEDURE getlogin (): char_star;


END Uutmp.
