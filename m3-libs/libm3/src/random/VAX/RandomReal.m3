(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Fri Dec 17 13:08:50 PST 1993 by kalsow     *)
(*      modified on Tue Jan 30 10:57:41 1990 by muller         *)
(*      modified on Thu Jan 25 22:51:07 PST 1990 by stolfi     *)

UNSAFE MODULE RandomReal;

IMPORT Word, Random;

(*  
  Implements Random.Real and Random.LongReal for the VAX architecture,
  assuming the DFLOAT format for LONGREAL. *)

PROCEDURE Real (r: Random.T): REAL =
  CONST ExponentBias = 129;  (* Exponent bias for REAL *)
        FractionBits = 23;   (* Number of explicit fraction bits *)
        WordSize = 32;       (* Size of INTEGER in bits *)
  VAR frac, exp: INTEGER;
  BEGIN
    (* Generate a random fraction and get its first non-zero word: *)
    exp  := ExponentBias - 1;
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
    IF exp <= 0 THEN RETURN 0.0 END;  (* Store zero on underflow *)
    IF ((ExponentBias - 1 - exp) MOD WordSize) >  WordSize - FractionBits THEN
      (* Needs more random bits *)
      frac := r.integer ();
    END;

    (* Repack as REAL: *)
    RETURN LOOPHOLE (
      Word.Rotate(
        Word.Or (
          Word.Shift (exp, FractionBits), 
          Word.Shift (
            Word.And (frac, 16_7fffffff),
            -(WordSize - 1 - FractionBits)
          )
        ),
        16
      ),
      REAL
    );
  END Real;

PROCEDURE Longreal (r: Random.T): LONGREAL =
  TYPE XX = ARRAY [0..1] OF Word.T;
  VAR ans: LONGREAL;
  BEGIN
    LOOPHOLE (ans, XX)[0]
      := LOOPHOLE(Real(r), Word.T); (* Exp and high-order fraction bits *)
    LOOPHOLE (ans, XX)[1]
      := r.integer ();              (* Low-order fraction bits *)
    RETURN ans;
  END Longreal;

PROCEDURE Extended (r: Random.T): EXTENDED =
  BEGIN
    RETURN LOOPHOLE (Longreal (r), EXTENDED);
  END Extended;

BEGIN
END RandomReal.
