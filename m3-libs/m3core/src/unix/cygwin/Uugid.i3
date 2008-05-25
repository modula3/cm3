(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

INTERFACE Uugid;

FROM Utypes IMPORT gid_t, uid_t;

<*EXTERNAL*> PROCEDURE geteuid (): uid_t;
<*EXTERNAL*> PROCEDURE getegid (): gid_t;

END Uugid.
