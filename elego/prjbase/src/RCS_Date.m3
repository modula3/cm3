(* Copyright 1996-1998 John D. Polstra.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgment:
 *      This product includes software developed by John D. Polstra.
 * 4. The name of the author may not be used to endorse or promote products
 *    derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * $Id: RCS_Date.m3,v 1.1.1.1 2010-08-07 16:18:28 wagner Exp $ *)

MODULE RCS_Date;

IMPORT
  Date, FloatMode, Fmt, Lex, Scan, Text, Time, TextSeq, TextUtils;

IMPORT
  MxConfig, SMsg AS Msg;

TYPE 
  TokScan = OBJECT
    t : TextSeq.T;
    i : INTEGER;
    d : TEXT;
  METHODS
    getInt(token := "unknown") : INTEGER RAISES {Error} := GetInt;
    getEnd() RAISES {Error} := GetEnd;
  END;

PROCEDURE GetInt(ts : TokScan; token := "unknown") : INTEGER RAISES {Error} =
  VAR tok : TEXT;
  BEGIN
    IF ts.i >= ts.t.size() THEN
      RAISE Error("no more integers in date " & ts.d &
                  " while looking for " & token);
    END;
    tok := ts.t.get(ts.i);
    INC(ts.i);
    TRY
      RETURN Scan.Int(tok);
    EXCEPT
      Lex.Error, FloatMode.Trap => 
      RAISE Error("invalid integer " & tok & " in date " & ts.d &
                  " while looking for " & token);
    END;
  END GetInt;

PROCEDURE GetEnd(ts : TokScan) RAISES {Error} =
  BEGIN
    IF ts.i < ts.t.size() THEN
      RAISE Error("too many elements in date " & ts.d);
    END;
  END GetEnd; 

PROCEDURE TokScanNew(d : TEXT) : TokScan =
  VAR ts := NEW(TokScan);
  BEGIN
    ts.t := TextUtils.Split(d, ".");
    ts.i := 0;
    ts.d := d;
    RETURN ts;
  END TokScanNew;

PROCEDURE Compare(a, b: T): [-1..1] =
  BEGIN
    WITH aLen = Text.Length(a), bLen = Text.Length(b) DO
      IF aLen = bLen THEN
	RETURN Text.Compare(a, b);
      ELSE
	IF aLen < bLen THEN RETURN -1 ELSE RETURN 1 END;
      END;
    END;
  END Compare;

PROCEDURE Equal(a, b: T): BOOLEAN =
  BEGIN
    RETURN Text.Equal(a, b);
  END Equal;

PROCEDURE FromTime(t: Time.T): T =
  VAR
    date := Date.FromTime(t, Date.UTC);
  BEGIN
    IF 1900 <= date.year AND date.year < 2000 THEN
      DEC(date.year, 1900);
    END;

    RETURN Fmt.Pad(Fmt.Int(date.year), 2, '0')
      & "." & Fmt.Pad(Fmt.Int(ORD(date.month)+1), 2, '0')
      & "." & Fmt.Pad(Fmt.Int(date.day), 2, '0')
      & "." & Fmt.Pad(Fmt.Int(date.hour), 2, '0')
      & "." & Fmt.Pad(Fmt.Int(date.minute), 2, '0')
      & "." & Fmt.Pad(Fmt.Int(date.second), 2, '0');
  END FromTime;

PROCEDURE ToTime(d: T): Time.T
  RAISES {Error} =
  BEGIN
    IF MxConfig.HOST_OS_TYPE() = MxConfig.OS_TYPE.WIN32 THEN
      RETURN ToTimeWin32(d);
    ELSE
      RETURN ToTimePOSIX(d);
    END;
  END ToTime;

PROCEDURE ToTimeApprox(d: T): Time.T =
  BEGIN
    IF MxConfig.HOST_OS_TYPE() = MxConfig.OS_TYPE.WIN32 THEN
      RETURN ToTimeWin32(d, exceptions := FALSE); <* NOWARN *>
    ELSE
      RETURN ToTimePOSIX(d, exceptions := FALSE); <* NOWARN *>
    END;
  END ToTimeApprox;

CONST
  DaysNormal = ARRAY [1..12] OF CARDINAL{  (* To beginning of month. *)
    0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334
  };
  DaysLeap = ARRAY [1..12] OF CARDINAL{
    0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335
  };
  Epoch = 1970;
  EndOfTime = 2100;

PROCEDURE DaysOfMonth(year, month: CARDINAL) : CARDINAL =
  CONST
    DaysOfMonthNormal = ARRAY [1..12] OF CARDINAL{
      31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31
    };
  BEGIN
    IF year MOD 4 = 0 AND month = 2 THEN
      RETURN 29;
    END;
    RETURN DaysOfMonthNormal[month];
  END DaysOfMonth;

PROCEDURE CheckDate(VAR year, month, day, hour, minute, second: CARDINAL;
                    exceptions : BOOLEAN) RAISES {Error} =
  VAR daymax : CARDINAL;
  BEGIN
    IF exceptions THEN
      IF NOT (Epoch <= year AND year < EndOfTime) THEN
        RAISE Error("Year " & Fmt.Int(year) & " out of range in RCS date");
      END;
      IF month < 1 OR month > 12 THEN
        RAISE Error("Month " & Fmt.Int(month) & " out of range in RCS date");
      END;
      daymax := DaysOfMonth(year, month);
      IF day < 1 OR day > daymax THEN
        RAISE Error("Day " & Fmt.Int(day) & " out of range in RCS date");
      END;
      IF hour > 23 THEN
        RAISE Error("Hour " & Fmt.Int(hour) & " out of range in RCS date");
      END;
      IF minute > 59 THEN
        RAISE Error("Minute " & Fmt.Int(minute) & " out of range in RCS date");
      END;
      IF second > 59 THEN
        RAISE Error("Second " & Fmt.Int(second) & " out of range in RCS date");
      END;
    ELSE (* no exceptions *)
      IF NOT (Epoch <= year AND year < EndOfTime) THEN
        Msg.Error("Year " & Fmt.Int(year) & " out of range in RCS date");
        IF year < Epoch THEN
          year := Epoch;
        ELSE
          year := EndOfTime - 1;
        END;
      END;
      IF month < 1 OR month > 12 THEN
        Msg.Error("Month " & Fmt.Int(month) & " out of range in RCS date");
        IF month < 1 THEN
          month := 1;
        ELSE
          month := 12;
        END;
      END;
      daymax := DaysOfMonth(year, month);
      IF day < 1 OR day > daymax THEN
        Msg.Error("Day " & Fmt.Int(day) & " out of range in RCS date");
        IF day < 1 THEN
          day := 1;
        ELSE
          day := daymax;
        END;
      END;
      IF hour > 23 THEN
        Msg.Error("Hour " & Fmt.Int(hour) & " out of range in RCS date");
        hour := 23;
      END;
      IF minute > 59 THEN
        Msg.Error("Minute " & Fmt.Int(minute) & " out of range in RCS date");
        minute := 59;
      END;
      IF second > 59 THEN
        Msg.Error("Second " & Fmt.Int(second) & " out of range in RCS date");
        second := 59;
      END;
    END;
  END CheckDate;

PROCEDURE ToTimeWin32(d: T; exceptions := TRUE): Time.T
  RAISES {Error} =
  VAR
    ts: TokScan;
    year, month, day, hour, minute, second: CARDINAL;
  BEGIN
    ts := TokScanNew(d);
    year := ts.getInt("year");
    month := ts.getInt("month");
    day := ts.getInt("day");
    hour := ts.getInt("hour");
    minute := ts.getInt("minute");
    second := ts.getInt("second");
    ts.getEnd();

    IF year < 100 THEN INC(year, 1900) END;

    (* Be feature-compatible with POSIX. *)
    CheckDate(year, month, day, hour, minute, second, exceptions);

    TRY
      RETURN Date.ToTime(
               Date.T {year, VAL(month - 1, Date.Month), day, 
                       hour, minute, second,
                       0, "UTC", Date.WeekDay.Sun});
    EXCEPT
      Date.Error =>
      IF exceptions THEN
        RAISE Error("Bad date: " & d);
      ELSE
        Msg.Fatal("Bad date: " & d); (* cannot happen :-) *)
        RETURN Time.Now();
      END;
    END;
  END ToTimeWin32;

PROCEDURE ToTimePOSIX(d: T; exceptions := TRUE): Time.T
(* "Date.ToTime" is badly broken on POSIX. *)
  RAISES {Error} =
  VAR
    ts: TokScan;
    year, month, day, hour, minute, second: CARDINAL;
    FirstLeap := (Epoch + 3) DIV 4 * 4;
    numLeap: CARDINAL;	(* Leap years to beginning of specified year. *)
    t: CARDINAL;
  BEGIN
    ts := TokScanNew(d);
    year := ts.getInt("year");
    month := ts.getInt("month");
    day := ts.getInt("day");
    hour := ts.getInt("hour");
    minute := ts.getInt("minute");
    second := ts.getInt("second");
    ts.getEnd();

    IF year < 100 THEN INC(year, 1900) END;

    CheckDate(year, month, day, hour, minute, second, exceptions);

    numLeap := (year - FirstLeap + 3) DIV 4;

    t := (year - Epoch)*365 + numLeap;  (* Days to beginning of year. *)
    IF year MOD 4 = 0 THEN
      INC(t, DaysLeap[month]);
    ELSE
      INC(t, DaysNormal[month]);
    END;
    INC(t, day - 1);
    t := ((t*24 + hour)*60 + minute)*60 + second;
    RETURN FLOAT(t, Time.T);
  END ToTimePOSIX;

PROCEDURE Valid(d: T): BOOLEAN =
  CONST
    Digits = SET OF CHAR{'0'..'9'};
  VAR
    length: CARDINAL;
    firstDot: CARDINAL;
    ts: TokScan;
    year, month, day, hour, minute, second: CARDINAL;
  BEGIN
    length := Text.Length(d);
    CASE length OF
    | 17 => firstDot := 2;
    | 19 => firstDot := 4;
    ELSE
      RETURN FALSE;
    END;

    FOR pos := 0 TO length-1 DO
      WITH ch = Text.GetChar(d, pos) DO
	IF pos >= firstDot AND (pos-firstDot) MOD 3 = 0 THEN
	  IF ch # '.' THEN RETURN FALSE END;
	ELSE
	  IF NOT ch IN Digits THEN RETURN FALSE END;
	END;
      END;
    END;

    ts := TokScanNew(d);
    TRY
      year := ts.getInt();
      month := ts.getInt();
      day := ts.getInt();
      hour := ts.getInt();
      minute := ts.getInt();
      second := ts.getInt();
      ts.getEnd();
    EXCEPT ELSE
      RETURN FALSE;
    END;

    IF length = 17 THEN
      IF NOT 70 <= year THEN RETURN FALSE END;
    ELSE
      IF NOT (2000 <= year AND year < 2100) THEN RETURN FALSE END;
    END;

    IF NOT (1 <= month AND month <= 12) THEN RETURN FALSE END;
    IF NOT (1 <= day AND day <= 31) THEN RETURN FALSE END;
    IF NOT (0 <= hour AND hour <= 23) THEN RETURN FALSE END;
    IF NOT (0 <= minute AND minute <= 59) THEN RETURN FALSE END;
    IF NOT (0 <= second AND second <= 59) THEN RETURN FALSE END;

    RETURN TRUE;
  END Valid;

BEGIN
END RCS_Date.
