(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* Last modified on Fri Mar 16 12:20:16 1990 by muller        *)

INTERFACE Uugid;

FROM Utypes IMPORT gid_t, uid_t;

(*** geteuid(2) - get user identity ***)

<*EXTERNAL*> PROCEDURE geteuid (): uid_t;

(*** getegid(2) - get group identity ***)

<*EXTERNAL*> PROCEDURE getegid (): gid_t;

END Uugid.
