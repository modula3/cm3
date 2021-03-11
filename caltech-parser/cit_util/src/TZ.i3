(* $Id$ *)

INTERFACE TZ;
IMPORT Date, Time, Pathname, OSError;
IMPORT Word;

(* 
   NOTE 
         that the methods in this interface use the system's idea of
   Time.T, regardless of the OS.  That means that the epoch is Jan 1,
   1970 for Unix systems, and Jan 1, 1601 for Windows systems.  The values
   aren't hard-coded, but assumes that the standard C library follows
   Unix conventions (regardless of host OS --- this is true for Cygwin)
   and that Modula-3's Date implementation follows the host OS conventions.
*)

TYPE
  T <: Public;

  Public = OBJECT METHODS
    init(tz : TEXT; disableChecking := FALSE) : T RAISES { OSError.E };
    
    mktime(d : Date.T) : Time.T;
    (* convert given Date.T to a Time.T under the system's epoch, in the 
       receiving timezone, ignoring time zone fields in the Date.T *)

    localtime(t : Time.T) : Date.T;
    (* convert a given Time.T using the unix timezone functions.
       Note that during a leap second, the time will be off: it will display
       the 59th second for two seconds, i.e.,

       real time                  returned value
         :58.999                    :58.999
         :59.000                    :59.000
                        ...
         :59.999                    :59.999
         :60.000                    :59.000 XXX 
                        ...                 XXX wrong
         :60.999                    :59.999 XXX 
         :00.000                    :00.000
    *)

    name() : TEXT; (* a given name of this timezone, not guaranteed to be
                      unique (mainly for human consumption, debugging, ... ) *)
  END;

CONST Brand = "TZ";

(* why aren't the following things methods? *)

CONST DefPrec = 3;

PROCEDURE FormatSubsecond(tz : T; tm : Time.T; 
                          prec : [0..6] := DefPrec; 
                          simplified := FALSE;
                          printDate, printMillis := TRUE) : TEXT; 
(* format as YYYY-MM-DD@HH:MM:SS.SSS   (normal)
          or YYYYMMDD HH:MM:SS.SSS     (simplified)  
   prec says how many digits after the last decimal
*)

TYPE ParseMode = { Normal, Simplified, Automatic };

EXCEPTION Error;

PROCEDURE ParseSubsecond(tz : T;
                         txt : TEXT; mode := ParseMode.Automatic) : Time.T
  RAISES { Error };

VAR (*CONST*) UnixEpoch : Time.T;
(* this is the Unix system epoch in the local system's timekeeping.
   It's 11,644,473,600 on a Windows system, and 0 on a Unix or MacOS X system. *)

PROCEDURE New(tz : TEXT; disableChecking := FALSE) : T RAISES { OSError.E };
  (* equivalent to NEW(T).init(tz), except that the result may be shared *)

CONST DefaultTZRoot : Pathname.T = "/usr/share/zoneinfo";
      (* override by setting TZROOT env var. *)

PROCEDURE DisableChecking();
  (* disable looking for files in /usr/share/zoneinfo.  Needed on Cygwin? *)

PROCEDURE Equal(a, b : T) : BOOLEAN;

PROCEDURE Hash(a : T) : Word.T;

END TZ.






