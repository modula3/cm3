(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

INTERFACE UtimeInternal;

IMPORT UTtime;
FROM Utime IMPORT time_t;

TYPE

  struct_tm_star = UNTRACED REF struct_tm;
  struct_tm = RECORD
    Utime.struct_tm base;
    (* NOTE: The following two fields are not always valid, and how
       to manage tm_zone is not clear. *)
    tm_gmtoff:INTEGER;  (* offset from GMT in seconds *)
    tm_zone:  char_star; (* abbreviation of timezone name *)
  END;

<*EXTERNAL "UtimeInternal__mktime"*>PROCEDURE mktime (tm: struct_tm_star): time_t;
<*EXTERNAL "UtimeInternal__localtime"*>PROCEDURE localtime (clock: (*const*) UNTRACED REF time_t): struct_tm_star;
<*EXTERNAL "UtimeInternal__gmtime"*>PROCEDURE gmtime (clock: (*const*) UNTRACED REF time_t): struct_tm_star;
<*EXTERNAL "UtimeInternal__localtime_r"*>PROCEDURE localtime_r (READONLY clock: time_t; result: struct_tm_star): struct_tm_star;
<*EXTERNAL "UtimeInternal__gmtime_r"*>PROCEDURE gmtime_r (READONLY clock: time_t; result: struct_tm_star): struct_tm_star;

END Utime.
