(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Mon Jan 13 09:26:55 PST 1997 by mcjones    *)
(*      modified on Fri Aug  6 13:40:02 PDT 1993 by kalsow     *)
(*      modified on Tue Jan 30 10:57:23 1990 by muller         *)
(*      modified on Thu Jan 25 22:46:15 PST 1990 by stolfi     *)

UNSAFE MODULE RandomReal;

IMPORT LongRealRep, RealRep, Random, Word;

(* Implements Random.Real and Random.LongReal for IEEE floating-point
   format (both big-endian and little-endian). *)

PROCEDURE Real (r: Random.T): REAL =
  CONST ExponentBias = 127;  (* Exponent bias for REAL *)
        FractionBits = 23;   (* Number of explicit fraction bits *)
        WordSize = 32;       (* Size of INTEGER in bits *)
  VAR frac, exp: INTEGER; result: REAL;
  BEGIN
    (* Generate a random fraction and get its first non-zero word: *)
    exp := ExponentBias - 1;
    frac := r.integer ();
    WHILE (frac = 0) AND (exp >= WordSize) DO
      (* This loop is (almost) never executed: *)
      DEC (exp, WordSize);
      frac := r.integer ();
    END;
 
    (* Normalize: *)
    WHILE (frac > 0) AND (exp > 0) DO
      (* This loop is executed about once on the average. *)
      frac := Word.Shift (frac, 1);
      DEC (exp);
    END;
    IF ((ExponentBias - 1 - exp) MOD WordSize) >  WordSize - FractionBits THEN
      (* Needs more random bits *)
      frac := r.integer ();
    END;

    (* Repack as REAL: *)
    WITH r = LOOPHOLE (result, RealRep.T) DO
      r.sign := 0;
      r.exponent := exp;
      r.significand := Word.Shift (Word.And (frac, 16_7fffffff), 
                                   -(WordSize - 1 - FractionBits));
    END;
    RETURN result;
  END Real;

PROCEDURE Longreal (r: Random.T): LONGREAL =
  CONST ExponentBias = 1023; (* Exponent bias for LONGREAL *)
        FractionBits = 20;   (* Number of fraction bits in high half *)
        WordSize = 32;       (* Size of INTEGER in bits *)
  VAR frac, exp: INTEGER; result: LONGREAL;
  BEGIN
    (* Generate a random fraction and get the first non-zero word: *)
    exp := ExponentBias - 1;
    frac := r.integer ();
    WHILE (frac = 0) AND (exp >= WordSize) DO
      (* This loop is (almost) never executed: *)
      DEC (exp, WordSize);
      frac := r.integer ();
    END;
 
    (* Normalize: *)
    WHILE (frac > 0) AND (exp > 0) DO
      (* This loop is executed about once on the average. *)
      frac := Word.Shift (frac, 1);
      DEC (exp);
    END;
    IF ((ExponentBias - 1 - exp) MOD WordSize) >  WordSize - FractionBits THEN
      (* Needs more random bits *)
      frac := r.integer ();
    END;

    (* Repack as LONGREAL: *)
    WITH lr = LOOPHOLE (result, LongRealRep.T) DO
      lr.sign := 0;
      lr.exponent := exp;
      lr.significand0 := Word.Shift (Word.And (frac, 16_7fffffff),
                                      -(WordSize - 1 - FractionBits));
      lr.significand1 := r.integer (min := -16_7fffffff-1, max :=16_7fffffff);
    END;
    RETURN result;
  END Longreal;

PROCEDURE Extended (r: Random.T): EXTENDED =
  BEGIN
    RETURN LOOPHOLE (Longreal (r), EXTENDED);
  END Extended;

BEGIN
END RandomReal.
