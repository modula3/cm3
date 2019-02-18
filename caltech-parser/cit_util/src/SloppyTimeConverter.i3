(* $Id$ *)

INTERFACE SloppyTimeConverter;
IMPORT Time, HMTime, TZ;

(* 
   Time of day handling.

   converting unix time <--> time of day is an expensive operation,
   involving complicated system routines.

   We accelerate the process by storing a timezone-keyed table of 
   "midnights" that we use to convert faster.

   We use midnight of the next day as a starting point.

   These routines do not work properly between the midnight before a DST
   change and the DST change itself.

   Leap seconds?  Who knows.

*)

TYPE
  T <: Public;

  Public = OBJECT METHODS
    init(tz : TZ.T) : T;

    convert(time : Time.T) : HMTime.T;
  END;

CONST Brand = "SloppyTimeConverter";

PROCEDURE NewShared(tz : TZ.T) : T;
  (* return a shared new T, which is unmonitored.

     i.e., if you use this routine such that resulting objects may
     be called from multiple threads, the monitoring must happen
     elsewhere 
  *)

END SloppyTimeConverter.
