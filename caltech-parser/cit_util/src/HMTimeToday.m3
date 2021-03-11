(* $Id$ *)

MODULE HMTimeToday EXPORTS HMTime;
IMPORT TZ, HMTime, XTime AS Time;


PROCEDURE Today(tz : TZ.T; READONLY hm : HMTime.T) : Time.T =
  VAR
    l := tz.localtime(Time.Now());
  BEGIN
    l.hour := hm.hour; l.minute := hm.minute; l.second := hm.second;
    RETURN tz.mktime(l)
  END Today;

BEGIN END HMTimeToday.
