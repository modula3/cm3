(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* Last modified on Fri Mar 16 12:20:16 1990 by muller        *)

INTERFACE Uugid;

FROM Ctypes IMPORT int;


(*** getuid(2), geteuid(2) - get user identity ***)

<*EXTERNAL*> PROCEDURE getuid (): int;
<*EXTERNAL*> PROCEDURE geteuid (): int;


(*** getgid(2), getguid(2) - get group identity ***)

<*EXTERNAL*> PROCEDURE getgid (): int;
<*EXTERNAL*> PROCEDURE getegid (): int;


(*** setreuid(2) - set real and effective user ID's ***)

<*EXTERNAL*> PROCEDURE setreuid (ruid, euid: int): int;


(*** setregid(2) - set real and effective group ID ***)

<*EXTERNAL*> PROCEDURE setregid (rgid, egid: int): int;


END Uugid.
