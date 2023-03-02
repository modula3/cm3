MODULE FFT;

IMPORT Word, Math, IO;
IMPORT LongRealComplex AS LC;
(* debug IMPORT LongRealComplexFmtLex AS LFC; *)

REVEAL
  T = Fft BRANDED OBJECT
      METHODS
        transform (data: FFTArr; inverse: BOOLEAN) := Transform;
        permute   (data: FFTArr)                   := Permute;
        scale     (data: FFTArr)                   := Scale;
      OVERRIDES
        forward := Forward;
        inverse := Inverse;
      END;

PROCEDURE Forward (self: T; data: FFTArr) =
  VAR n := NUMBER(data^);
  BEGIN
    IF Word.And(n, n - 1) # 0 THEN
      IO.Put("fourier data must be power of 2\n");
      RETURN;
    END;
    self.permute(data);
    self.transform(data, FALSE);
  END Forward;

PROCEDURE Inverse (self: T; data: FFTArr; scale := TRUE) =
  VAR n := NUMBER(data^);
  BEGIN
    IF Word.And(n, n - 1) # 0 THEN
      IO.Put("fourier data must be power of 2\n");
      RETURN;
    END;
    self.permute(data);
    self.transform(data, TRUE);
    IF scale THEN self.scale(data); END;
  END Inverse;

PROCEDURE Permute (<* UNUSED *> self: T; data: FFTArr) =
  VAR
    target, mask: Word.T := 0;
    initMask    : Word.T := NUMBER(data^);
    temp        : LC.T;
  BEGIN
    (* Process all positions of input signal *)
    FOR position := FIRST(data^) TO LAST(data^) DO
      IF target > position THEN
        (* Swap entries *)
        temp := data[target];
        data[target] := data[position];
        data[position] := temp;
      END;
      (* While bit is set *)
      mask := initMask DIV 2;
      WHILE Word.And(target, mask) > 0 DO
        (* Drop bit *)
        target := Word.And(target, Word.Not(mask));
        mask := mask DIV 2;
      END;
      (* The current bit is 0 - set it *)
      target := Word.Or(target, mask);
    END;
  END Permute;

PROCEDURE Transform
  (<* UNUSED *> self: T; data: FFTArr; inverse: BOOLEAN) =
  VAR
    step, jump, match, n       : INTEGER;
    delta, sine, pi            : LONGREAL;
    multiplier, factor, product: LC.T;
  BEGIN
    n := LAST(data^);
    pi := Math.Pi;
    IF NOT inverse THEN pi := -pi; END;
    (* Iteration through dyads, quadruples, octads and so on... *)
    step := 1;
    WHILE step < n DO
      (* Jump to the next entry of the same transform factor *)
      jump := step * 2;
      (* Angle increment *)
      delta := pi / FLOAT(step, LONGREAL);
      (* Auxiliary sin(delta / 2) *)
      sine := Math.sin(delta * 0.5D0);
      (* Multiplier for trigonometric recurrence *)
      multiplier := LC.T{-2.0D0 * sine * sine, Math.sin(delta)};
      (* Start value for transform factor, fi = 0 *)
      factor := LC.T{1.0D0, 0.0D0};
      (* Iteration through groups of different transform factor *)
      FOR group := 0 TO step - 1 DO
        (* Iteration within group *)
        FOR pair := group TO n - 1 BY jump DO
          match := pair + step;  (* Match position *)
          product := LC.Mul(factor, data[match]); (* Second term of
                                                     two-point transform *)
          data[match] :=
            LC.Sub(data[pair], product); (* Transform for fi + pi *)
          data[pair] := LC.Add(data[pair], product); (* Transform for fi *)
        END;
        (* Successive transform factor via trigonometric recurrence *)
        factor := LC.Add(LC.Mul(multiplier, factor), factor);
      END;
      step := step * 2;
    END;
  END Transform;

PROCEDURE Scale (<* UNUSED *> self: T; data: FFTArr) =
  VAR
    inv   : LONGREAL := 1.0D0 / FLOAT(NUMBER(data^), LONGREAL);
    factor           := LC.T{inv, 0.0D0};
  BEGIN
    FOR position := 0 TO LAST(data^) DO
      data[position] := LC.Mul(data[position], factor);
    END;
  END Scale;

BEGIN
END FFT.
