(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Jun  9 13:14:37 PDT 1994 by heydon                   *)

UNSAFE MODULE WriteVal;

IMPORT JunoMarshal;

PROCEDURE Grow(VAR (*IO*) code: Code; endLoc: CARDINAL) =
  BEGIN
    IF endLoc > LAST(code^) THEN
      VAR new := NEW(Code, 2 * NUMBER(code^)); BEGIN
        SUBARRAY(new^, 0, NUMBER(code^)) := code^;
        code := new
      END
    END
  END Grow;

PROCEDURE UShort(VAR (*IO*) code: Code;
    VAR (*IO*) loc: CARDINAL; v: CARDINAL) =
  BEGIN
    Grow(code, loc + JunoMarshal.UShortSize);
    VAR a: JunoMarshal.BytePtr := ADR(code[loc]); BEGIN
      JunoMarshal.WriteUShort(a, v);
      loc := a - ADR(code[0])
    END
  END UShort;

PROCEDURE Short(VAR (*IO*) code: Code; VAR (*IO*) loc: CARDINAL; v: INTEGER) =
  BEGIN
    Grow(code, loc + JunoMarshal.ShortSize);
    VAR a: JunoMarshal.BytePtr := ADR(code[loc]); BEGIN
      JunoMarshal.WriteShort(a, v);
      loc := a - ADR(code[0])
    END
  END Short;

PROCEDURE ULong(VAR (*IO*) code: Code;
    VAR (*IO*) loc: CARDINAL; v: CARDINAL) =
  BEGIN
    Grow(code, loc + JunoMarshal.ULongSize);
    VAR a: JunoMarshal.BytePtr := ADR(code[loc]); BEGIN
      JunoMarshal.WriteULong(a, v);
      loc := a - ADR(code[0])
    END
  END ULong;

PROCEDURE Real(VAR (*IO*) code: Code; VAR (*IO*) loc: CARDINAL; v: JVReal) =
  BEGIN
    Grow(code, loc + JunoMarshal.RealSize);
    VAR a: JunoMarshal.BytePtr := ADR(code[loc]); BEGIN
      JunoMarshal.WriteReal(a, v);
      loc := a - ADR(code[0])
    END
  END Real;

BEGIN
END WriteVal.
