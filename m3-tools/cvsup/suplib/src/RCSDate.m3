(* Copyright 1996-2003 John D. Polstra.
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
 * $Id: RCSDate.m3,v 1.1.1.1 2009-04-09 17:01:56 jkrell Exp $ *)

MODULE RCSDate;

IMPORT
  Date, Fmt, RCSError, Text, Time, TokScan;

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
(* "Date.ToTime" is badly broken. *)
  RAISES {RCSError.E} =
  CONST
    DaysNormal = ARRAY [1..12] OF CARDINAL{  (* To beginning of month. *)
      0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334
    };
    DaysLeap = ARRAY [1..12] OF CARDINAL{
      0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335
    };
    Epoch = 1970;
    EndOfTime = 2100;
    FirstLeap = (Epoch + 3) DIV 4 * 4;
  VAR
    ts: TokScan.T;
    year, month, day, hour, minute, second: CARDINAL;
    numLeap: CARDINAL;	(* Leap years to beginning of specified year. *)
    t: CARDINAL;
  BEGIN
    TRY
      ts := TokScan.New(d, SET OF CHAR{'.'});
      year := ts.getInt("year");
      month := ts.getInt("month");
      day := ts.getInt("day");
      hour := ts.getInt("hour");
      minute := ts.getInt("minute");
      second := ts.getInt("second");
      ts.getEnd();

      IF year < 100 THEN INC(year, 1900) END;

      IF NOT (Epoch <= year AND year < EndOfTime) THEN
	RAISE RCSError.E("Year " & Fmt.Int(year) & " out of range");
      END;

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
    EXCEPT TokScan.Error(msg) =>
      RAISE RCSError.E(msg);
    END;
  END ToTime;

PROCEDURE Valid(d: T): BOOLEAN =
  CONST
    Digits = SET OF CHAR{'0'..'9'};
  VAR
    length: CARDINAL;
    firstDot: CARDINAL;
    ts: TokScan.T;
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

    ts := TokScan.New(d, SET OF CHAR{'.'});
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
END RCSDate.
