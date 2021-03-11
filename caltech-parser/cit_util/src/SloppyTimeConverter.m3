(* $Id$ *)

MODULE SloppyTimeConverter;
IMPORT XTime AS Time, TZ, HMTime, SortedLongrealRefTbl, TZRefTbl;

REVEAL
  T = Public BRANDED Brand OBJECT
    midnites : SortedLongrealRefTbl.T;
    tz : TZ.T;
  OVERRIDES
    init := Init;
    convert := SloppyTimeConvert;
  END;

PROCEDURE Init(t : T; tz : TZ.T) : T =
  BEGIN
    t.midnites := NEW(SortedLongrealRefTbl.Default).init();
    t.tz := tz;
    RETURN t
  END Init;

PROCEDURE SloppyTimeConvert(self : T; t : Time.T) : HMTime.T =
  VAR nextT := LAST(Time.T);
      r : REFANY;
  CONST SecondsPerDay = 24.0d0 * 3600.0d0;
  BEGIN
    WITH iter = self.midnites.iterateOrdered() DO
      iter.seek(t);
      EVAL iter.next(nextT, r);

      IF nextT > t + SecondsPerDay THEN
        (* too far in the future, make new record *)
        VAR
          tTomorrow := t + SecondsPerDay;
          date      := self.tz.localtime(tTomorrow);
        BEGIN
          date.hour := 0; date.minute := 0; date.second := 0;
          nextT := self.tz.mktime(date);
          EVAL self.midnites.put(nextT,NIL)
        END
      END;
      
      <* ASSERT nextT <= t + SecondsPerDay *>
      WITH thisMidnite = nextT - SecondsPerDay,
           secsSince   = FLOOR(t - thisMidnite),

           hours = secsSince DIV 3600,
           mins  = (secsSince DIV 60) MOD 60,
           secs  = secsSince MOD 60 DO
        RETURN HMTime.T { hours, mins, secs }
      END
    END
  END SloppyTimeConvert;

VAR byTZTbl := NEW(TZRefTbl.Default).init();

PROCEDURE NewShared(tz : TZ.T) : T =
  VAR
    r : REFANY;
  BEGIN
    IF NOT byTZTbl.get(tz, r) THEN 
      r := NEW(T).init(tz);
      EVAL byTZTbl.put(tz, r)
    END;
    RETURN r
  END NewShared;


BEGIN END SloppyTimeConverter.
