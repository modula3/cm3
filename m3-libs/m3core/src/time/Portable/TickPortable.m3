(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(* Last modified on Mon Jul 18 08:13:40 PDT 1994 by kalsow     *)
(*      modified on Thu Mar 11 16:26:00 PST 1993 by mjordan    *)

MODULE TickPortable EXPORTS Tick;

(* This implementation works in terms of the "Time" interface.
   "SecondsPerTick" is defined as "Time.Grain". *)

IMPORT Word, Time;

TYPE
  TickRange  = [-16_7fffffff-1 .. 16_7fffffff]; (* 32-bit ticks *)
CONST
  IntModulus = FLOAT(LAST(TickRange), LONGREAL) + 1.0D0;
  WordModulus = 2.0D0 * IntModulus;
  IntModulusAsWord = Word.Plus(LAST(TickRange), 1);

VAR (*CONST*) SecondsPerTick: LONGREAL;

PROCEDURE Now(): T=
  VAR t: LONGREAL := Time.Now() / SecondsPerTick MOD WordModulus;
  BEGIN
    IF t < IntModulus THEN
      RETURN TRUNC(t)
    ELSE
      RETURN Word.Plus(IntModulusAsWord, TRUNC(t-IntModulus))
    END
  END Now;

PROCEDURE ToSeconds(t: Word.T): LONGREAL=
  BEGIN
    IF Word.LT(t, IntModulusAsWord) THEN
      RETURN FLOAT(t, LONGREAL) * SecondsPerTick
    ELSE
      RETURN (FLOAT(Word.Minus(t, IntModulusAsWord), LONGREAL) + IntModulus)
             * SecondsPerTick
    END
  END ToSeconds;

PROCEDURE FromSeconds(s: LONGREAL): Word.T RAISES {Overflow}=
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

BEGIN
  SecondsPerTick := Time.Grain;
END TickPortable.

