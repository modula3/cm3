(* Copyright (C) 1989, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* Last modified on Tue Jan  5 13:29:23 PST 1993 by mcjones *)

MODULE TickPosix EXPORTS Tick;

IMPORT Utime, Word;

CONST
  IntModulus = FLOAT(LAST(INTEGER), LONGREAL) + 1.0D0;
  WordModulus = 2.0D0 * IntModulus;
  IntModulusAsWord = Word.Plus(LAST(INTEGER), 1);

VAR (*CONST*) MicrosecondsPerTick, SecondsPerTick: LONGREAL;

PROCEDURE Now(): T =
  VAR
    tv: Utime.struct_timeval;
    tz: Utime.struct_timezone;
    i := Utime.gettimeofday(tv, tz);
    d: LONGREAL;
  BEGIN
    <* ASSERT i=0 *>
    d := (FLOAT(tv.tv_sec, LONGREAL)*1.0D6 + FLOAT(tv.tv_usec, LONGREAL))
         / MicrosecondsPerTick
         MOD WordModulus;
    IF d < IntModulus THEN
      RETURN TRUNC(d)
    ELSE
      RETURN Word.Plus(IntModulusAsWord, TRUNC(d-IntModulus))
    END
  END Now;

PROCEDURE ToSeconds(t: Word.T): LONGREAL =
  BEGIN
    IF Word.LT(t, IntModulusAsWord) THEN
      RETURN FLOAT(t, LONGREAL) * SecondsPerTick
    ELSE
      RETURN (FLOAT(Word.Minus(t, IntModulusAsWord), LONGREAL) + IntModulus)
             * SecondsPerTick
    END
  END ToSeconds;

PROCEDURE FromSeconds(s: LONGREAL): Word.T RAISES {Overflow} =
  BEGIN
    WITH d = s / SecondsPerTick DO
      IF d < IntModulus THEN
        RETURN TRUNC(d)
      ELSIF d < WordModulus THEN
        RETURN Word.Plus(IntModulusAsWord, TRUNC(d-IntModulus))
      ELSE
        RAISE Overflow
      END
    END
  END FromSeconds;

VAR t0, t1: T;
BEGIN
  (* Determine value of "MicrosecondsPerTick" experimentally.  Note that
     this will fail if this thread is descheduled for a tick during the
     loop below. *)
  MicrosecondsPerTick := 1.0D0; (* just for now! *)
  t0 := Now();
  REPEAT t1 := Now() UNTIL t1 # t0;
  MicrosecondsPerTick := FLOAT(t1-t0, LONGREAL);
  SecondsPerTick := MicrosecondsPerTick / 1.0D6
END TickPosix.
