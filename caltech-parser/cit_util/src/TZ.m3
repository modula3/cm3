(* $Id$ *)

UNSAFE MODULE TZ;
IMPORT UtimeOpsC, Date, XTime AS Time;
FROM M3toC IMPORT CopyTtoS, FreeCopiedS, CopyStoT;
IMPORT CTZ, Text;
IMPORT Debug;
IMPORT OSError;
IMPORT FS, Env;
IMPORT Ctypes;
IMPORT Word;
IMPORT Thread;
IMPORT Fmt;
IMPORT SchedulerIndirection; (* to conform with CM3 DatePosix.m3... *)
IMPORT UtimeWrap;

REVEAL
  T = Public BRANDED Brand OBJECT
    tz : TEXT;
    hashV : Word.T;
    cache : Cache;
  OVERRIDES
    init := Init;
    mktime := Mktime;
    localtime := Localtime;
    name := Name;
  END;

PROCEDURE Equal(a, b : T) : BOOLEAN =
  BEGIN
    RETURN a = b OR a.tz = b.tz OR Text.Equal(a.tz, b.tz) 
  END Equal;

PROCEDURE Hash(a : T) : Word.T = BEGIN RETURN a.hashV END Hash;

TYPE Cache = RECORD itime : INTEGER (* TRUNC(t : Time.T) *); d : Date.T END;

PROCEDURE Name(t : T) : TEXT = BEGIN RETURN t.tz END Name;

PROCEDURE Init(t : T; tz : TEXT; disableChecking : BOOLEAN) : T RAISES { OSError.E } = 
  BEGIN 
    IF doChecking AND NOT disableChecking THEN
      VAR
        fn := tz;
      BEGIN
        IF Text.GetChar(fn,0) = ':' THEN fn := Text.Sub(fn,1) END;
        EVAL FS.Status(tzDir & "/" & fn); (* RAISEs OSError.E *)
      END
    END;

    t.tz := tz; 
    t.hashV := Text.Hash(tz);
    t.cache.itime := FIRST(INTEGER); (* impossible key *)
    RETURN t 
  END Init;

PROCEDURE SetCurTZ(to : TEXT) =
  (* mu must be locked *)
  BEGIN
    IF NOT Text.Equal(to,CurTZ) THEN
      WITH s = CopyTtoS(to) DO
        SchedulerIndirection.DisableSwitching();
        CTZ.setTZ(s);
        SchedulerIndirection.EnableSwitching();
        FreeCopiedS(s)
      END;
      CurTZ := to
    END
  END SetCurTZ;

PROCEDURE GetOldTZ() : TEXT =
  (* mu must be locked *)
  BEGIN
        SchedulerIndirection.DisableSwitching();
    WITH res = CopyStoT(CTZ.getenv(TZTZ)) DO
        SchedulerIndirection.EnableSwitching();
      IF Debug.GetLevel() > 30 THEN 
        Thread.Release(mu); (* avoid re-entrant locking (Debug uses TZ) *)
        TRY
          Debug.Out("TZ.GetOldTZ: " & res) 
        FINALLY
          Thread.Acquire(mu)
        END
      END;
      RETURN res
    END
  END GetOldTZ;

PROCEDURE Localtime(t : T; timeArg : Time.T) : Date.T =
  VAR
    time := timeArg - UnixEpoch; (* on Unix, UnixEpoch is 0 *)
    itime := TRUNC(time); (* note: not ROUND! *)

    d : Date.T;
  BEGIN
    IF Debug.GetLevel() >= 20 THEN
      Debug.Out("Localtime : " & Fmt.LongReal(timeArg) & " unixepoch=" & Fmt.LongReal(UnixEpoch))
    END;
    (* first of all, see if the conversion is in the same minute
       as we just converted.  If so, modify return value accordingly
       and return it, saving system calls, etc. *)
    LOCK mu DO

      (* tricky note.

         We might think we can use a t-specific mutex here.
         That is so, and we used to do it this way, by calling it 
         a field in t.  However, this won't work if we ever want
         to Pickle a T and transfer that Pickle between Win32 and
         POSIX.

         If we really, really want per-T locks, we need to do 
         something with tables.
       *)
      
      WITH newSecond = t.cache.d.second + (itime - t.cache.itime) DO
        IF newSecond >= 0 AND newSecond <= 59 THEN
          VAR 
            newD := t.cache.d;
          BEGIN
            newD.second := newSecond;
            RETURN newD
          END
        END
      END
    END;
    
            IF Debug.GetLevel() >= 20 THEN
              Debug.Out("TZ.Localtime: time=" & Fmt.LongReal(time))
            END;
    VAR
      tms := UtimeWrap.make_T();
      clock : Ctypes.long;
      oldTZ : TEXT;
    BEGIN
      TRY
        LOCK mu DO
          oldTZ := GetOldTZ();
          TRY
            SetCurTZ(t.tz);
            
            (*
            (* Debug.Out MAY call time functions, which may call us back,
               leading to locking against ourselves *)
            *)
            
            clock := itime;
            
            (* the following code should match what is in DateBsd.m3 ... *)
            (* the main difference is the use of localtime_r rather than
               localtime, in order to eliminate static storage depedencies *)
            
            WITH tm = UtimeOpsC.localtime_r(time,tms) DO
              UtimeOpsC.Set_second(tms, 
                                   MIN(UtimeOpsC.Get_second(tm),59)); 
              (* leap seconds!? *)

              d.second := UtimeOpsC.Get_second(tm);
              d.minute := UtimeOpsC.Get_minute(tm);
              d.hour := UtimeOpsC.Get_hour(tm);
              d.day := UtimeOpsC.Get_day(tm);
              d.month := VAL(UtimeOpsC.Get_month(tm),Date.Month);
              d.year := UtimeOpsC.Get_year(tm);
              d.weekDay := VAL(UtimeOpsC.Get_wday(tm),Date.WeekDay);
              
              d.offset := -(UtimeOpsC.Get_gmtoff(tm));
              d.zone := CopyStoT(UtimeOpsC.Get_zone(tm))
            END
          FINALLY
            UtimeWrap.delete_T(tms);
            SetCurTZ(oldTZ)
          END
        END;
        t.cache.itime := itime; t.cache.d := d;
        RETURN d
        
      FINALLY
      END
    END
  END Localtime;

PROCEDURE Mktime(t : T; d : Date.T) : Time.T =
  BEGIN
    LOCK mu DO
      SetCurTZ(t.tz);
      
      VAR
        tm := UtimeWrap.make_T();
      BEGIN
        TRY
          tm := UtimeOpsC.localtime_r(SomeTimeT, (* any legal time *)
                                      tm);
        BEGIN
          UtimeOpsC.Set_second(tm, d.second);
          UtimeOpsC.Set_minute(tm, d.minute);
          UtimeOpsC.Set_hour(tm, d.hour);
          UtimeOpsC.Set_day(tm, d.day);
          UtimeOpsC.Set_month(tm,ORD(d.month));
          UtimeOpsC.Set_year(tm,d.year);
          WITH res = UtimeOpsC.mktime(tm) DO
            <* ASSERT res >= 0.0d0 *>
            RETURN res
          END
        END
      FINALLY
        UtimeWrap.delete_T(tm);
      END
      END
    END
  END Mktime;

PROCEDURE DisableChecking() = BEGIN doChecking := FALSE END DisableChecking;

VAR doChecking := TRUE;

VAR CurTZ := "";
VAR mu := NEW(MUTEX);

VAR TZTZ := CopyTtoS("TZ");
VAR SomeTimeT := Time.Now();

VAR
  tzDir := DefaultTZRoot;
BEGIN 
  WITH env = Env.Get("TZROOT") DO
    IF env # NIL THEN tzDir := env END
  END;

  WITH s = CopyTtoS(CurTZ) DO
    SchedulerIndirection.DisableSwitching();
    CTZ.setTZ(s);
    SchedulerIndirection.EnableSwitching();
    FreeCopiedS(s)
  END
END TZ.
