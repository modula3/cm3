(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Oct  3 16:49:04 PDT 1994 by heydon                   *)

UNSAFE MODULE RowOp;

IMPORT JunoValue;
IMPORT CRowOp;

PROCEDURE P(
  VAR target: Vector;
  READONLY src: Vector;
  factor: JunoValue.Real)
  : INTEGER =

  BEGIN
    RETURN CRowOp.P(NUMBER(target), ADR(target[0]), ADR(src[0]), factor)

(*
  VAR maxCol := -1; maxAbs := 0.0; i := 0; BEGIN
    WITH last = LAST(target) DO
(*
      WHILE i < last - 1 DO
        target[i] := target[i] - (factor * src[i]);
        WITH abs = ABS(target[i]) DO
          IF abs > maxAbs THEN
            maxAbs := abs;
            maxCol := i
          END
        END;
        VAR j := i + 1; BEGIN
          target[j] := target[j] - (factor * src[j]);
          WITH abs = ABS(target[j]) DO
            IF abs > maxAbs THEN
              maxAbs := abs;
              maxCol := j
            END
          END
        END;
        INC(i, 2)
      END;
*)
      FOR i := 0 TO last-1 DO
        target[i] := target[i] - (factor * src[i]);
        WITH abs = ABS(target[i]) DO
          IF abs > maxAbs THEN
            maxAbs := abs;
            maxCol := i
          END
        END
      END;
      target[last] := target[last] - (factor * src[last])
    END;
    RETURN maxCol
*)
  END P;

BEGIN
END RowOp.
