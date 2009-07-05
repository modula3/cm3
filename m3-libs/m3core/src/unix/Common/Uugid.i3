(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

<*EXTERNAL*> INTERFACE Uugid;

FROM Utypes IMPORT gid_t, uid_t;

<*EXTERNAL "Uugid__geteuid"*>PROCEDURE geteuid (): uid_t;
<*EXTERNAL "Uugid__getegid"*>PROCEDURE getegid (): gid_t;

END Uugid.
