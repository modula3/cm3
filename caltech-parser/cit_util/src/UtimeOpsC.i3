(* $Id$ *)

UNSAFE INTERFACE UtimeOpsC;
FROM Ctypes IMPORT char_star, long_star;

TYPE T = ADDRESS; (* struct_tm_star *)

<*EXTERNAL UtimeOpsC__mktime*>
PROCEDURE mktime(tm : T) : LONGREAL;

<*EXTERNAL UtimeOpsC__localtime_r*>
PROCEDURE localtime_r(time : LONGREAL; result : T) : T;

<*EXTERNAL UtimeOpsC__make_T*>
PROCEDURE make_T() : T;

<*EXTERNAL UtimeOpsC__delete_T*>
PROCEDURE delete_T(t : T);

(***********************************************************************)

TYPE Second = [0..60];  (* N.B. leap second *)
     Minute = [0..59];
     Hour   = [0..23];
     Day    = [1..31];
     Month  = [0..11];  (* 0 = Jan, ... , 11 = Dec *)
     Year   = INTEGER;  (* year in Gregorian alt Julian calendar *)
     WDay   = [0..6];
     Offset = INTEGER;

(* 
  VAR t : T;
  BEGIN
    t        := localtime_r(SomeValidTime);
    Set_<fields>(t, ...); ... ;
    <result> := mktime(t);
  END
  
  To change the Zone, use setenv.
*)

<*EXTERNAL UtimeOpsC__Set_second *>PROCEDURE Set_second(t : T; second : Second);
<*EXTERNAL UtimeOpsC__Set_minute *>PROCEDURE Set_minute(t : T; minute : Minute);
<*EXTERNAL UtimeOpsC__Set_hour   *>PROCEDURE Set_hour  (t : T; hour   : Hour);
<*EXTERNAL UtimeOpsC__Set_day    *>PROCEDURE Set_day   (t : T; day    : Day);
<*EXTERNAL UtimeOpsC__Set_month  *>PROCEDURE Set_month (t : T; month  : Month);
<*EXTERNAL UtimeOpsC__Set_year   *>PROCEDURE Set_year  (t : T; year   : Year);
<*EXTERNAL UtimeOpsC__Set_wday   *>PROCEDURE Set_wday  (t : T; wday   : WDay);
<*EXTERNAL UtimeOpsC__Get_second *>PROCEDURE Get_second(t : T) : Second;
<*EXTERNAL UtimeOpsC__Get_minute *>PROCEDURE Get_minute(t : T) : Minute;
<*EXTERNAL UtimeOpsC__Get_hour   *>PROCEDURE Get_hour  (t : T) : Hour;
<*EXTERNAL UtimeOpsC__Get_day    *>PROCEDURE Get_day   (t : T) : Day;
<*EXTERNAL UtimeOpsC__Get_month  *>PROCEDURE Get_month (t : T) : Month;
<*EXTERNAL UtimeOpsC__Get_year   *>PROCEDURE Get_year  (t : T) : Year;
<*EXTERNAL UtimeOpsC__Get_wday   *>PROCEDURE Get_wday  (t : T) : WDay;
<*EXTERNAL UtimeOpsC__Get_gmtoff *>PROCEDURE Get_gmtoff(t : T) : INTEGER;
<*EXTERNAL UtimeOpsC__Get_zone   *>PROCEDURE Get_zone  (t : T) : char_star;

<*EXTERNAL UtimeOpsC__ctime_r    *>PROCEDURE ctime_r   (clock : long_star; buf : char_star) : char_star;

CONST Brand = "UtimeC";

<*EXTERNAL UtimeOpsC__check_types*>PROCEDURE check_types();

<*EXTERNAL UtimeOpsC__write_double_clock*>
PROCEDURE write_double_clock(time : LONGREAL; buffer : ADDRESS);

END UtimeOpsC.
