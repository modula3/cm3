(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* File: Uutmp.i3                                             *)
(* Last modified on Mon Apr 16 15:59:54 1990 by jerome        *)

INTERFACE Uutmp;

FROM Ctypes IMPORT char, char_star, long;

(*** <utmp.h> ***)

(*
 * Structure of utmp and wtmp files.
 *
 *)

TYPE
    struct_utmp = RECORD
	ut_line: ARRAY [0..7] OF char;		(* tty name *)
	ut_name: ARRAY [0..7] OF char;		(* user id *)
	ut_host: ARRAY [0..15] OF char;		(* host name, if remote *)
	ut_time: long				(* time on *)
     END;

CONST
   UTMP_FILE	= "/etc/utmp";
   WTMP_FILE	= "/usr/adm/wtmp";

(*	Special strings or formats used in the "ut_line" field when	*)
(*	accounting for something other than a process.			*)
(*	No string for the ut_line field can be more than 7 chars +	*)
(*	a NULL in length.						*)

   EMPTY	= "";
   BOOT_MSG	= "~";
   OTIME_MSG	= "|";
   NTIME_MSG	= "}";


(*** getlogin(3) ***)

<*EXTERNAL*> PROCEDURE getlogin (): char_star;


END Uutmp.
