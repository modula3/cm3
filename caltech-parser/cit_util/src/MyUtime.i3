INTERFACE MyUtime;
IMPORT Utime;

(*** mktime(3) - convert a struct_tm to a time_t ***)
<*EXTERNAL MyUtime__mktime*> PROCEDURE mktime (tm: Utime.struct_tm_star): Utime.time_t;

END MyUtime.
