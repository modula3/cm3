(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

INTERFACE Uugid;

FROM Utypes IMPORT gid_t, uid_t;
FROM Ctypes IMPORT int;

<*EXTERNAL "Uugid__geteuid"*>PROCEDURE geteuid (): uid_t;
<*EXTERNAL "Uugid__getegid"*>PROCEDURE getegid (): gid_t;
<*EXTERNAL "Uugid__getuid"*>PROCEDURE getuid(): uid_t;
<*EXTERNAL "Uugid__setreuid"*>PROCEDURE setreuid(ruid: uid_t; euid: uid_t): int;
<*EXTERNAL "Uugid__havegroup"*>PROCEDURE havegroup(qgid : gid_t): int;
(* returns 1 if qgid is member of getgroups(), -1 in case of error, 0 otherwise *)
END Uugid.
