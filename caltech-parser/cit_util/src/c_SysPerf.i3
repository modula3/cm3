(* $Id$ *)

UNSAFE INTERFACE c_SysPerf;
IMPORT Ctypes;


(* this is really dumb.. we call getloadavg several times to avoid
   dealing with the way arrays are represented in m3 (the "Hollerith" thing) *)
<* EXTERNAL getloadavg_glue *>
PROCEDURE getloadavg_glue(VAR loadavg : LONGREAL; which : INTEGER) : INTEGER;
 
<*EXTERNAL diskAvail*>
PROCEDURE diskAvail(path : Ctypes.const_char_star; 
                    VAR percentageAvailNonSuperUser : LONGREAL) : INTEGER;
  (* call to statfs(2) *)

END c_SysPerf.
