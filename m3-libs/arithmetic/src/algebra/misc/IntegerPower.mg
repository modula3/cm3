GENERIC MODULE IntegerPower(R);
(*Arithmetic for Modula-3, see doc for details *)

IMPORT Arithmetic AS Arith;

<* UNUSED *>
CONST
  Module = "IntegerPower.";
(*==========================*)
(*----------------------*)

PROCEDURE Power (x: T; y0: PositiveInteger): T
  RAISES {Arith.Error} <* NOWARN *> =
  (* for some datatypes no Error can occur *)
  VAR
    pow            := x;
    z   : T;
    init           := FALSE;
    y   : CARDINAL := y0;        (*0 will occur as final value*)
  BEGIN
    WHILE y > 0 DO
      IF y MOD 2 # 0 THEN
        IF init THEN z := R.Mul(z, pow); ELSE z := pow; init := TRUE; END;
      END;
      pow := R.Mul(pow, pow);
      y := y DIV 2;
    END;
    <* ASSERT init *>
    RETURN z;
  END Power;

PROCEDURE MulPower (x, y: T; z: CARDINAL): T
  RAISES {Arith.Error} <* NOWARN *> =
  (* for some datatypes no Error can occur *)
  BEGIN
    WHILE z > 0 DO
      IF z MOD 2 # 0 THEN x := R.Mul(x, y); END;
      y := R.Mul(y, y);
      z := z DIV 2;
    END;
    RETURN x;
  END MulPower;

(*==========================*)
BEGIN
END IntegerPower.
