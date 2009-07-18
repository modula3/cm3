(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

<*EXTERNAL*> INTERFACE Uugid;

FROM Utypes IMPORT gid_t, uid_t;
FROM Ctypes IMPORT int;

<*EXTERNAL "Uugid__geteuid"*>PROCEDURE geteuid (): uid_t;
<*EXTERNAL "Uugid__getegid"*>PROCEDURE getegid (): gid_t;
<*EXTERNAL "Uugid__getuid"*>PROCEDURE getuid(): uid_t;
<*EXTERNAL "Uugid__setreuid"*>PROCEDURE setreuid(ruid: uid_t; euid: uid_t): int;

END Uugid.
