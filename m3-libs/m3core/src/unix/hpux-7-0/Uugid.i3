(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* Last modified on Fri Jan  6 09:03:09 PST 1995 by kalsow         *)
(*      modified on Wed Jun 27 17:36:44 1990 by piet@cs.ruu.nl *)
(*      modified on Fri Mar 16 12:20:16 1990 by muller        *)

INTERFACE Uugid;

FROM Ctypes IMPORT int;


(*** getuid(2), geteuid(2) - get user identity ***)

<*EXTERNAL*> PROCEDURE getuid (): int;
<*EXTERNAL*> PROCEDURE geteuid (): int;


(*** getgid(2), getguid(2) - get group identity ***)

<*EXTERNAL*> PROCEDURE getgid (): int;
<*EXTERNAL*> PROCEDURE getegid (): int;

(*** setuid(2), setgid(2) - set user/group identity ***)

<*EXTERNAL*> PROCEDURE setuid (pid: int): int;
<*EXTERNAL*> PROCEDURE setgid (pid: int): int;

(*** setresuid(2) - set real and effective and saved user ID's ***)

<*EXTERNAL*> PROCEDURE setresuid (ruid, euid, suid: int): int;

PROCEDURE setreuid (ruid, euid: int): int;
  (* for compatibility with the other platforms...*)


(*** setresgid(2) - set real and effective and saved group ID ***)

<*EXTERNAL*> PROCEDURE setregid (rgid, egid, sgid: int): int;


END Uugid.
