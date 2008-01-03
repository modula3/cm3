(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* Last modified on Sun Jun 10 05:28:08 1990 by muller        *)

INTERFACE Uugid;

FROM Ctypes IMPORT int;
FROM Utypes IMPORT u_short;


(*** getuid(2), geteuid(2) - get user identity ***)

<*EXTERNAL*> PROCEDURE getuid (): u_short;
<*EXTERNAL*> PROCEDURE geteuid (): u_short;


(*** getgid(2), getguid(2) - get group identity ***)

<*EXTERNAL*> PROCEDURE getgid (): u_short;
<*EXTERNAL*> PROCEDURE getegid (): u_short;


(*** setreuid(2) - set real and effective user ID's ***)

<*EXTERNAL*> PROCEDURE setreuid (ruid, euid: int): int;


(*** setregid(2) - set real and effective group ID ***)

<*EXTERNAL*> PROCEDURE setregid (rgid, egid: int): int;


END Uugid.
