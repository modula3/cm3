GENERIC MODULE IntegerPower(R);
(*Copyright (c) 1996, m3na project

   *)

FROM NADefinitions IMPORT Error, Err;

<*UNUSED*>
CONST Module = "IntegerPower.";
(*==========================*)
(*----------------------*)

PROCEDURE Power (x: T; y0: PositiveInteger): T =
  VAR
    pow            := x;
    z   : T;
    init           := FALSE;
    y   : CARDINAL := y0;        (*0 will occur as final value*)
  BEGIN
    TRY
      WHILE y > 0 DO
        IF y MOD 2 # 0 THEN
          IF init THEN
            z := R.Mul(z, pow);
          ELSE
            z := pow;
            init := TRUE;
          END;
        END;
        pow := R.Mul(pow, pow);
        y := y DIV 2;
      END;
    EXCEPT
      Error (err) =>             <*ASSERT err # Err.bad_size *>
    END;
    <*ASSERT init*>
    RETURN z;
  END Power;

PROCEDURE MulPower (x, y: T; z: CARDINAL): T =
  BEGIN
    TRY
      WHILE z > 0 DO
        IF z MOD 2 # 0 THEN x := R.Mul(x, y); END;
        y := R.Mul(y, y);
        z := z DIV 2;
      END;
    EXCEPT
      Error (err) =>
      <*ASSERT err # Err.bad_size *>
    END;
    RETURN x;
  END MulPower;

(*==========================*)
BEGIN
END IntegerPower.
