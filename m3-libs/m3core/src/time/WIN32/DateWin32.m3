(* Copyright (C) 1989, Digital Equipment Corporation        *)
(* All rights reserved.                                     *)
(* See the file COPYRIGHT for a full description.           *)
(*                                                          *)
(* Last modified on Fri May 16 10:08:38 PDT 1997 by heydon  *)
(*      modified on Wed May 14 16:59:00 PDT 1997 by mcjones *)
(*      modified on Thu Aug 31 13:15:29 PDT 1995 by steveg  *)
(*      modified on Wed Aug 31 09:46:48 PDT 1994 by kalsow  *)
(*      modified on Mon Feb 15 15:54:51 PST 1993 by mjordan *)

UNSAFE MODULE DateWin32 EXPORTS Date;

IMPORT Text, Time;
IMPORT WinDef, WinBase, WinNT, TimeWin32;

REVEAL TimeZone = BRANDED OBJECT METHODS fromTime(t: Time.T): T END;

PROCEDURE FromTimeLocal(<*UNUSED*> z: TimeZone; t: Time.T): T =
(* Implementation note: This implementation is buggy on Windows 95 due
   to the fact that, as of 4/97, the "SystemTimeToTzSpecificLocalTime"
   function is documented as not being implemented on Windows 95. If
   the call to that procedure fails, measures are taken to try to
   compute the correct result. Unfortunately, the "FileTimeToLocalTime"
   function adjusts for daylight savings time based on the current
   time rather than on the time passed as its argument.

   As a result, the returned date may be reported to be in the wrong
   time zone. For example, if the program is run at a time when
   daylight savings is in effect, but "Date.FromTime" is passed a time
   that does not fall in daylight savings time, the returned result
   will have an "hour" value one hour larger than it should, but the
   "offset" field will be one hour smaller than it should be, and the
   "zone" field will incorrectly indicate that daylight savings was in
   effect at that time.

    We decided not to try to duplicate the functionality of 
    "SystemTimeToTzSpecificLocalTime" because some necessary
    information, the rule for deciding when daylight savings time
    is in effect, apparently is not always available.  The Win32
    specification for "TIME_ZONE_INFORMATION" says it may contain
    either a rule applicable to any year, or a pair of dates for the
    current year.  In the latter case, it is not obvious how to handle
    a date in a different year. *)
  CONST SecsPerMin = 60;
  VAR
    ft, lft: WinBase.FILETIME;
    st, lst: WinBase.SYSTEMTIME;
    d: T;
    tz: WinBase.TIME_ZONE_INFORMATION;
    tzrc: WinDef.DWORD;
    status: INTEGER;
    firstDayOfEpoch := FALSE;
  BEGIN
    (* If the time given is before the PC epoch (less than the time
       zone offset) then Windows' time calculation fails.  So: if t is
       in the first day of the PC epoch then add 1 day (86400
       seconds), do the conversion to a date, and finally subtract the
       day out of the date.  NOTE: a negative time will either give
       incorrect results or crash. *)
    IF t < 86400.0D0 THEN
      t := t + 86400.0D0;
      <*ASSERT t > 0.0D0*>
      firstDayOfEpoch := TRUE
    END;
    TimeWin32.ToFileTime(t, ft);
    status := WinBase.FileTimeToSystemTime(ADR(ft), ADR(st));
    <*ASSERT status # 0*>
    tzrc := WinBase.GetTimeZoneInformation(ADR(tz));
    <*ASSERT tzrc # -1 *>
    status := WinBase.SystemTimeToTzSpecificLocalTime(
                ADR(tz), ADR(st), ADR(lst));
    IF status # 0 THEN (* implemented *)
      status := WinBase.SystemTimeToFileTime(ADR(lst), ADR(lft));
      <*ASSERT status # 0*>
    ELSE (* not implemented *)
      (* Unfortunately, FileTimeToLocalTime adjusts for daylight
         savings time based on the current time rather than on the
         time passed as its argument. *)
      status := WinBase.FileTimeToLocalFileTime(ADR(ft), ADR(lft));
      <*ASSERT status # 0*>
      status := WinBase.FileTimeToSystemTime(ADR(lft), ADR(lst));
      <*ASSERT status # 0*>
    END;
    d := FromSystemTime(lst);
    d.offset := ROUND(t - TimeWin32.FromFileTime(lft));
    IF tz.StandardDate.wMonth # 0
       AND d.offset = SecsPerMin * (tz.Bias+tz.StandardBias) THEN
      d.zone := CopyTimeZoneName(tz.StandardName)
    ELSIF tz.DaylightDate.wMonth # 0
          AND d.offset = SecsPerMin * (tz.Bias+tz.DaylightBias) THEN
      d.zone := CopyTimeZoneName(tz.DaylightName)
    ELSE
      d.zone := "[Unknown zone]"
    END;
    IF firstDayOfEpoch THEN
      IF d.day = 2 THEN
        d.day := 1
      ELSE
        d.day := 31;
        d.month := Month.Dec;
        DEC(d.year)
      END
    END;
    RETURN d
  END FromTimeLocal;

PROCEDURE CopyTimeZoneName(
    READONLY name: ARRAY [0 .. 31] OF WinNT.WCHAR): TEXT =
  VAR chars: ARRAY [0..31] OF WIDECHAR; j := 0;
  BEGIN
    WHILE (j < NUMBER(name)) AND (name[j] # 0) DO
      chars[j] := VAL (name[j], WIDECHAR); INC(j)
    END;
    RETURN Text.FromWideChars(SUBARRAY(chars, 0, j))
  END CopyTimeZoneName;

(*******
PROCEDURE CopyTimeZoneName(
    READONLY name: ARRAY [0 .. 31] OF WinNT.WCHAR): TEXT =
  VAR chars: ARRAY [0..31] OF CHAR; j := 0;
  BEGIN
    FOR i := 0 TO LAST(name) DO
      WITH char = VAL(name[i], CHAR) DO
        IF char = '\000' THEN EXIT
        ELSE
          chars[j] := char; INC(j)
        END
      END
    END;
    RETURN Text.FromChars(SUBARRAY(chars, 0, j))
  END CopyTimeZoneName;
*****)
      
PROCEDURE FromTimeUTC(<*UNUSED*> z: TimeZone; t: Time.T): T =
  VAR d: T; st: WinBase.SYSTEMTIME; ft: WinBase.FILETIME;  status: INTEGER;
  BEGIN
    TimeWin32.ToFileTime(t, ft);
    status := WinBase.FileTimeToSystemTime(ADR(ft), ADR(st));
    <*ASSERT status # 0 *>
    d := FromSystemTime(st);
    d.offset := 0;
    d.zone := "UTC";
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
