(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Thu Nov  7 08:43:45 PST 1991 by kalsow         *)
(*      modified on Wed Jun 20 01:46:13 1990 by muller         *)

(*      modified on Mon Nov 23 09:38:21 1987 by stolfi         *)

MODULE Stat;

IMPORT Wr, Fmt, Math, Thread;

PROCEDURE Init (VAR s: T) =
  BEGIN
    WITH z = s DO
      z.num      := 0.0d+0;
      z.maximum  := 0.0;
      z.minimum  := 0.0;
      z.mean     := 0.0d+0;
      z.variance := 0.0d+0;
    END
  END Init;

PROCEDURE Accum (VAR s: T;  x: REAL) =
  VAR oldnum, tmp: LONGREAL;
  BEGIN
    WITH z = s DO
      IF z.num = 0.0d+0 THEN
        z.maximum := x;
        z.minimum := x
      ELSIF x > z.maximum THEN
        z.maximum := x
      ELSIF x < z.minimum THEN
        z.minimum := x
      END;
      oldnum := z.num;
      z.num := z.num + 1.0d+0;
      tmp := FLOAT (x, LONGREAL) - z.mean;
      z.mean := z.mean + tmp / z.num;
      z.variance := (z.variance + tmp * tmp / z.num) * oldnum / z.num;
    END;
  END Accum;

PROCEDURE Combine (READONLY r, s: T): T =
  VAR tmp: LONGREAL; t: T;
  BEGIN
    WITH z = t DO
      z.maximum := MAX (r.maximum, s.maximum);
      z.minimum := MIN (r.minimum, s.minimum);
      z.num := r.num + s.num;
      z.mean := (r.mean * r.num + s.mean * s.num) / z.num;
      tmp := r.mean - s.mean;
      z.variance := (r.variance * r.num + s.variance * s.num
                       + tmp * tmp * r.num * s.num / z.num) / z.num;
    END;
    RETURN t
  END Combine;

PROCEDURE Num (READONLY s: T): REAL =
  BEGIN
    RETURN FLOAT (s.num)
  END Num;

PROCEDURE Max (READONLY s: T): REAL =
  BEGIN
    RETURN s.maximum
  END Max;

PROCEDURE Min (READONLY s: T): REAL =
  BEGIN
    RETURN s.minimum
  END Min;

PROCEDURE Mean (READONLY s: T): REAL =
  BEGIN
    RETURN FLOAT (s.mean)
  END Mean;

PROCEDURE Var (READONLY s: T): REAL =
  BEGIN
    RETURN FLOAT (s.variance)
  END Var;

PROCEDURE SDev (READONLY s: T): REAL =
  BEGIN
    IF s.num = 0.0d+0 THEN RETURN 0.0 END;
    RETURN FLOAT (Math.sqrt (s.variance * s.num / (s.num - 1.0d+0)))
  END SDev;

PROCEDURE RMS (READONLY s: T): REAL =
  BEGIN
    IF s.num = 0.0d+0 THEN
      RETURN 0.0
    ELSE
      RETURN FLOAT (Math.sqrt (s.variance + s.mean * s.mean))
    END
  END RMS;

PROCEDURE Print (wr: Wr.T;  READONLY s: T) RAISES {Wr.Failure, Thread.Alerted}=
  BEGIN
    WITH z = s DO
      Wr.PutText (wr, "num = " & Fmt.Int (TRUNC (z.num)));
      IF z.num > 0.0d+0 THEN
        Wr.PutText (wr, " [ " & Fmt.Real (Min (s)) & " _ "
                       & Fmt.Real (Max (s)) & " ] mean = "
                       & Fmt.Real (Mean (s)));
        IF z.num >= 2.0d+0 THEN
          Wr.PutText (wr, "  dev = " & Fmt.Real (SDev (s)))
        END;
      END;
    END;
  END Print;

BEGIN
END Stat.

