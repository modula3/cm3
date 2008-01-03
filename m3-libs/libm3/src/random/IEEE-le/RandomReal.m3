(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Fri Aug  6 13:40:02 PDT 1993 by kalsow     *)
(*      modified on Tue Jan 30 10:57:23 1990 by muller         *)
(*      modified on Thu Jan 25 22:46:15 PST 1990 by stolfi     *)

UNSAFE MODULE RandomReal;

IMPORT Word, Random;

(*  
  Implements Random.Real and Random.LongReal for IEEE floating-point
  format, as available on the following little-endian machines:
        Acorn RISC,
        Apollo DN3000, DN4000,
        DECstation 3100 (pmax),
        Encore Multimax,
        IBM PC/386,  PC/RT,  R6000,
        HP 9000/300,
        Sun 3,
*)

PROCEDURE Real (r: Random.T): REAL =
  TYPE RealInt = ARRAY [0..BYTESIZE(INTEGER) DIV BYTESIZE(REAL) - 1] OF REAL;
  CONST ExponentBias = 127;  (* Exponent bias for REAL *)
        FractionBits = 23;   (* Number of explicit fraction bits *)
        WordSize = 32;       (* Size of INTEGER in bits *)
  VAR frac, exp: INTEGER;  result: INTEGER;
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
    result := Word.Or (Word.Shift (exp, FractionBits), 
                       Word.Shift (Word.And (frac, 16_7fffffff), 
                                   -(WordSize - 1 - FractionBits))
                      );
    RETURN LOOPHOLE (result, RealInt)[0];
  END Real;

PROCEDURE Longreal (r: Random.T): LONGREAL =
  TYPE Int32 = BITS 32 FOR [-16_7fffffff-1..16_7fffffff];
  TYPE XX = ARRAY [0..1] OF Int32;
  CONST ExponentBias = 1023; (* Exponent bias for LONGREAL *)
        FractionBits = 20;   (* Number of fraction bits in high half *)
        WordSize = 32;       (* Size of INTEGER in bits *)
  VAR frac, exp: INTEGER;  ans: LONGREAL;
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
    LOOPHOLE (ans, XX)[0] := r.integer ();(* Low-order fraction bits *)
    LOOPHOLE (ans, XX)[1] := Word.Or (
      Word.Shift (exp, FractionBits), 
      Word.Shift (Word.And (frac, 16_7fffffff), -(WordSize - 1 - FractionBits))
    );
    RETURN ans;
  END Longreal;

PROCEDURE Extended (r: Random.T): EXTENDED =
  BEGIN
    RETURN LOOPHOLE (Longreal (r), EXTENDED);
  END Extended;

BEGIN
END RandomReal.
