(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* File: Uutmp.i3                                             *)
(* Last modified on Wed Oct  5 15:40:50 PDT 1994 by ericv         *)
(*      modified on Mon Apr 16 15:59:54 1990 by jerome        *)

INTERFACE Uutmp;

FROM Ctypes IMPORT char, char_star, short;
FROM Utypes IMPORT time_t;

(*** <utmp.h> ***)

(*
 * Structure of utmp and wtmp files.
 *
 *)

TYPE
  struct_exit_status = RECORD
    e_termination: short;
    e_exit: short;
  END;

    struct_utmp = RECORD
	ut_user: ARRAY [0..7] OF char;		(* user id *)
	ut_id: ARRAY [0..3] OF char;		(* /etc/inittab id *)
	ut_line: ARRAY [0..11] OF char;		(* device name *)
        ut_pid: short;
        ut_type: short;
        ut_exit: struct_exit_status;
        ut_time: time_t;
     END;

(*	Definitions for ut_type *)

CONST
  EMPTY		 = 0;
  RUN_LVL	 = 1;
  BOOT_TIME	 = 2;
  OLD_TIME	 = 3;
  NEW_TIME	 = 4;
  INIT_PROCESS	 = 5;	(* Process spawned by "init" *)
  LOGIN_PROCESS	 = 6;	(* A "getty" process waiting for login *)
  USER_PROCESS	 = 7;	(* A user process *)
  DEAD_PROCESS	 = 8;
  ACCOUNTING	 = 9;

  UTMAXTYPE	 = ACCOUNTING;	(* Largest legal value of ut_type *)

(*	Special strings or formats used in the "ut_line" field when	*)
(*	accounting for something other than a process.			*)
(*	No string for the ut_line field can be more than 11 chars +	*)
(*	a NULL in length.						*)

CONST
  RUNLVL_MSG	 = "run-level %c";
  BOOT_MSG	 = "system boot";
  OTIME_MSG	 = "old time";
  NTIME_MSG	 = "new time";

CONST
   UTMP_FILE	= "/var/adm/utmp";
   WTMP_FILE	= "/var/adm/wtmp";


(*** getlogin(3) ***)

<*EXTERNAL*> PROCEDURE getlogin (): char_star;


END Uutmp.
