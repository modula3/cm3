(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

INTERFACE Utime;

IMPORT Utypes;
FROM Ctypes IMPORT char_star;

TYPE
  time_t = Utypes.time_t;

<*EXTERNAL "Utime__time"*>PROCEDURE time (tloc: UNTRACED REF time_t): time_t;
<*EXTERNAL "Utime__ctime"*>PROCEDURE ctime (READONLY clock: time_t): char_star;
<*EXTERNAL "Utime__tzset"*>PROCEDURE tzset();

END Utime.
