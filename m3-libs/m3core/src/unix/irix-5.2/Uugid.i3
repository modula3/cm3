(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* Last modified on Thu Oct  6 11:41:14 PDT 1994 by ericv         *)
(*      modified on Fri Mar 16 12:20:16 1990 by muller        *)

INTERFACE Uugid;

FROM Ctypes IMPORT int;
FROM Utypes IMPORT uid_t, gid_t;

(*** <unistd.h> ***)

(*** getuid(2), geteuid(2) - get user identity ***)

<*EXTERNAL*> PROCEDURE getuid (): uid_t;
<*EXTERNAL*> PROCEDURE geteuid (): uid_t;


(*** getgid(2), getegid(2) - get group identity ***)

<*EXTERNAL*> PROCEDURE getgid (): gid_t;
<*EXTERNAL*> PROCEDURE getegid (): gid_t;


(*** setreuid(2) - set real and effective user ID's ***)

<*EXTERNAL*> PROCEDURE setreuid (ruid, euid: uid_t): int;


(*** setregid(2) - set real and effective group ID ***)

<*EXTERNAL*> PROCEDURE setregid (rgid, egid: gid_t): int;


END Uugid.
