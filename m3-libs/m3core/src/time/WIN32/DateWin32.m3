(* Copyright (C) 1989, Digital Equipment Corporation        *)
(* All rights reserved.                                     *)
(* See the file COPYRIGHT for a full description.           *)
(*                                                          *)
(* Last modified on Thu Aug 31 13:15:29 PDT 1995 by steveg  *)
(*      modified on Wed Aug 31 09:46:48 PDT 1994 by kalsow  *)
(*      modified on Mon Feb 15 15:54:51 PST 1993 by mjordan *)
(*      modified on Fri Dec  4 09:13:43 PST 1992 by mcjones *)

UNSAFE MODULE DateWin32 EXPORTS Date;

IMPORT Text, Time;
IMPORT WinDef, WinBase, WinNT, TimeWin32;

REVEAL TimeZone = BRANDED OBJECT METHODS fromTime(t: Time.T): T END;

PROCEDURE FromTimeLocal(<*UNUSED*> z: TimeZone; t: Time.T): T =
  VAR
    ft, lft: WinBase.FILETIME;
    st: WinBase.SYSTEMTIME;
    d: T;
    tz: WinBase.TIME_ZONE_INFORMATION;
    tzrc: WinDef.DWORD;
    status: INTEGER;
    firstDayOfEpoch := FALSE;
  BEGIN
    (* if the time given is before the PC epoch (less than the
       time zone offset) then Window's time calculation messes
       up.  So, if t is in the first day of the PC epoch then
       add 1 day (86400 seconds) do the converstion to a date
       and then subtract the day out of the date.  NOTE: a negative
       time will either give incorrect results or crash. *)
    IF t < 86400.0D0 THEN
      t := t + 86400.0D0;
      <* ASSERT t > 0.0D0 *>
      firstDayOfEpoch := TRUE;
    END;
    ft := TimeWin32.ToFileTime(t);
    status := WinBase.FileTimeToLocalFileTime(ADR(ft), ADR(lft));
    <*ASSERT status # 0*>
    status := WinBase.FileTimeToSystemTime(ADR(lft), ADR(st));
    <*ASSERT status # 0*>
    d := FromSystemTime(st);
    d.offset := ROUND(t - TimeWin32.FromFileTime(lft));
    tzrc := WinBase.GetTimeZoneInformation(ADR(tz));
    CASE tzrc OF
    | 1 (* TIME_ZONE_ID_STANDARD *) =>
        d.zone := CopyTimeZoneName(tz.StandardName);
    | 2 (* TIME_ZONE_ID_DAYLIGHT *) =>
        d.zone := CopyTimeZoneName(tz.DaylightName);
        INC(d.offset, tz.DaylightBias);
    ELSE (* 0 => TIME_ZONE_ID_UNKNOWN, or -1 => call failed *)
        d.zone := "\"Unknown zone\"";
    END;
    IF firstDayOfEpoch THEN
      IF d.day = 2 THEN
        d.day := 1;
      ELSE
        d.day := 31;
        d.month := Month.Dec;
        DEC(d.year);
      END;
    END;
    RETURN d;
  END FromTimeLocal;

PROCEDURE CopyTimeZoneName(
    READONLY name: ARRAY [0 .. 31] OF WinNT.WCHAR): TEXT=
  VAR chars: ARRAY [0..31] OF CHAR; j := 0;
  BEGIN
    FOR i := 0 TO LAST(name) DO
      WITH char = VAL(name[i], CHAR) DO
        IF char = '\000' THEN EXIT
        ELSE
          chars[j] := char; INC(j);
        END;
      END;
    END;
    RETURN Text.FromChars(SUBARRAY(chars, 0, j));
  END CopyTimeZoneName;
      
PROCEDURE FromTimeUTC(<*UNUSED*> z: TimeZone; t: Time.T): T =
  VAR d: T; st: WinBase.SYSTEMTIME; ft: WinBase.FILETIME;  status: INTEGER;
  BEGIN
    ft := TimeWin32.ToFileTime(t);
    status := WinBase.FileTimeToSystemTime(ADR(ft), ADR(st));
    <*ASSERT status # 0 *>
    d := FromSystemTime(st);
    d.offset := 0;
    d.zone := "UT";
    RETURN d
  END FromTimeUTC;

PROCEDURE FromSystemTime(st: WinBase.SYSTEMTIME): T =
(* Set all fields of "d" except "offset" and "zone", from "st". *)
  VAR d: T;
  BEGIN
    d.year := st.wYear;
    d.month := VAL(st.wMonth-1, Month);
    d.day := st.wDay;
    d.hour := st.wHour;
    d.minute := st.wMinute;
    d.second := st.wSecond;
    d.weekDay := VAL(st.wDayOfWeek, WeekDay);
    RETURN d
  END FromSystemTime;

PROCEDURE FromTime(t: Time.T; z: TimeZone := NIL): T =
  BEGIN
    IF z = NIL THEN z := Local END;
    RETURN z.fromTime(t)
  END FromTime;

PROCEDURE ToTime(READONLY d: T): Time.T =
  VAR
    st: WinBase.SYSTEMTIME;
    ft: WinBase.FILETIME;
    t: Time.T;
    status: INTEGER;
  BEGIN
    st.wYear := d.year;
    st.wMonth := ORD(d.month)+1;
    (* st.wDayOfWeek ignored *)
    st.wDay := d.day;
    st.wHour := d.hour;
    st.wMinute := d.minute;
    st.wSecond := d.second;
    st.wMilliseconds := 0;
    status := WinBase.SystemTimeToFileTime(ADR(st), ADR(ft));
    <*ASSERT status # 0*>
    t := TimeWin32.FromFileTime(ft);
    RETURN t + FLOAT(d.offset, LONGREAL)
  END ToTime;

BEGIN
  Local := NEW(TimeZone, fromTime := FromTimeLocal);
  UTC := NEW(TimeZone, fromTime := FromTimeUTC)
END DateWin32.
