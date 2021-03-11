(* $Id$ *)
UNSAFE INTERFACE UtimeR;
FROM Utime IMPORT struct_tm_star;

FROM Ctypes IMPORT char_star, long_star;


(* C time.h re-entrant versions, not declared in Utime.i3 *)

<*EXTERNAL*> PROCEDURE ctime_r     (clock: long_star; buf : char_star): char_star;
<*EXTERNAL*> PROCEDURE asctime_r   (tm: struct_tm_star; buf : char_star): char_star;

<*EXTERNAL*> PROCEDURE localtime_r (clock: long_star; result : struct_tm_star): struct_tm_star;
<*EXTERNAL*> PROCEDURE gmtime_r    (clock: long_star; result : struct_tm_star): struct_tm_star;

END UtimeR.
