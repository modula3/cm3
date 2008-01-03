(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Jun  9 13:13:39 PDT 1994 by heydon                   *)

UNSAFE MODULE JunoMarshal;

IMPORT JunoValue, JunoRT;

TYPE
  Byte = JunoRT.ByteCode;
  RefShort = UNTRACED REF ARRAY [0..ShortSize-1] OF Byte;
  RefUShort = UNTRACED REF ARRAY [0..UShortSize-1] OF Byte;
  RefULong = UNTRACED REF ARRAY [0..ULongSize-1] OF Byte;
  RefReal = UNTRACED REF ARRAY [0..RealSize-1] OF Byte;

PROCEDURE ReadShort(VAR a: BytePtr): Short =
  VAR res: Short; ra: RefShort := ADR(res); BEGIN
    FOR i := 0 TO ShortSize - 1 DO
      ra^[i] := a^; INC(a, ADRSIZE(a^))
    END;
    RETURN res
  END ReadShort;

PROCEDURE WriteShort(VAR a: BytePtr; sh: Short) =
  VAR ri: RefShort := ADR(sh); BEGIN
    FOR i := 0 TO ShortSize - 1 DO
      a^ := ri^[i]; INC(a, ADRSIZE(a^))
    END
  END WriteShort;

PROCEDURE ReadUShort(VAR a: BytePtr): UShort =
  VAR res: UShort; ra: RefUShort := ADR(res); BEGIN
    FOR i := 0 TO UShortSize - 1 DO
      ra^[i] := a^; INC(a, ADRSIZE(a^))
    END;
    RETURN res
  END ReadUShort;

PROCEDURE WriteUShort(VAR a: BytePtr; us: UShort) =
  VAR ri: RefUShort := ADR(us); BEGIN
    FOR i := 0 TO UShortSize - 1 DO
      a^ := ri^[i]; INC(a, ADRSIZE(a^))
    END
  END WriteUShort;

PROCEDURE ReadULong(VAR a: BytePtr): ULong =
  VAR res: ULong; ra: RefULong := ADR(res); BEGIN
    FOR i := 0 TO ULongSize - 1 DO
      ra^[i] := a^; INC(a, ADRSIZE(a^))
    END;
    RETURN res
  END ReadULong;

PROCEDURE WriteULong(VAR a: BytePtr; ul: ULong) =
  VAR ri: RefULong := ADR(ul); BEGIN
    FOR i := 0 TO ULongSize - 1 DO
      a^ := ri^[i]; INC(a, ADRSIZE(a^))
    END
  END WriteULong;

PROCEDURE ReadReal(VAR a: BytePtr): JunoValue.Real =
  VAR res: JunoValue.Real; ra: RefReal := ADR(res); BEGIN
    FOR i := 0 TO RealSize - 1 DO
      ra^[i] := a^; INC(a, ADRSIZE(a^))
    END;
    RETURN res
  END ReadReal;

PROCEDURE WriteReal(VAR a: BytePtr; r: JunoValue.Real) =
  VAR rr: RefReal := ADR(r); BEGIN
    FOR i := 0 TO RealSize - 1 DO
      a^ := rr^[i]; INC(a, ADRSIZE(a^))
    END
  END WriteReal;

BEGIN
END JunoMarshal.
