(* Copyright (C) 1989, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* Last modified on Wed Jun 22 14:52:00 PDT 1994 by kalsow  *)
(*      modified on Sat Dec  5 11:54:29 PST 1992 by mcjones *)

MODULE FmtTime;

IMPORT Date, Time, Convert, Text;

CONST
  MaxBuf = 4  (* WeekDay *)
         + 4  (* Month *)
         + 3  (* date *)
         + 3  (* hour *)
         + 3  (* minutes *)
         + 3  (* seconds *)
         + 40 (* time zone *)
         + 5  (* year *)
         ;

TYPE
  Buffer = RECORD
    len: INTEGER;
    buf: ARRAY [0..MaxBuf] OF CHAR;
  END;
         
PROCEDURE Long(t: Time.T; z: Date.TimeZone := NIL): TEXT =
  BEGIN
    RETURN DateLong(Date.FromTime(t, z))
  END Long;

PROCEDURE Short(t: Time.T; z: Date.TimeZone := NIL): TEXT =
  BEGIN
    RETURN DateShort(Date.FromTime(t, z))
  END Short;

PROCEDURE DateLong(READONLY d: Date.T): TEXT =
  VAR b: Buffer;
  BEGIN
    b.len := 0;
    AddText (b, WeekDay[d.weekDay]);
    AddText (b, Month[d.month]);
    AddInt  (b, d.day,    2, ' ', ' ');
    AddInt  (b, d.hour,   2, '0', ':');
    AddInt  (b, d.minute, 2, '0', ':');
    AddInt  (b, d.second, 2, '0', ' ');
    AddText (b, d.zone);
    AddInt  (b, d.year,   4, ' ', ' ');
    RETURN Text.FromChars (SUBARRAY (b.buf, 0, b.len-1));
  END DateLong;

PROCEDURE DateShort(READONLY d: Date.T): TEXT =
  VAR b: Buffer;
  BEGIN
    b.len := 0;
    AddText (b, Month[d.month]);
    AddInt  (b, d.day,    2, ' ', ' ');
    AddInt  (b, d.hour,   2, '0', ':');
    AddInt  (b, d.minute, 2, '0', ' ');
    RETURN Text.FromChars (SUBARRAY (b.buf, 0, b.len-1));
  END DateShort;

PROCEDURE AddText (VAR b: Buffer;  txt: TEXT) =
  BEGIN
    Text.SetChars (SUBARRAY (b.buf, b.len, NUMBER (b.buf) - b.len), txt);
    INC (b.len, Text.Length (txt));
    b.buf[b.len] := ' ';  INC (b.len);
  END AddText;

PROCEDURE AddInt (VAR b: Buffer;  value, width: INTEGER;  pre, post: CHAR) =
  <*FATAL Convert.Failed*>
  VAR
    buf : ARRAY [0..BITSIZE(INTEGER)] OF CHAR;
    len := Convert.FromInt (buf, value);
  BEGIN
    WHILE (width > len) DO
      b.buf[b.len] := pre;  INC (b.len);
      DEC (width);
    END;
    FOR i := 0 TO len-1 DO
      b.buf[b.len] := buf[i];  INC (b.len);
    END;
    b.buf[b.len] := post;  INC (b.len);
  END AddInt;

BEGIN
END FmtTime.
