GENERIC MODULE IntegerPower(R);
(*Copyright (c) 1996, m3na project

Abstract: Integers

2/17/96  Harry George    Initial version
*)

FROM xUtils IMPORT Error, Err;

<*UNUSED*> CONST Module = "IntegerPower.";
(*==========================*)
(*----------------------*)

PROCEDURE Power(x:T; y0:PositiveInteger):T =
(*we cannot always count on the existence of general Zero and One,
  e.g. for matrices. This makes things a bit ugly*)
VAR
  pow  := x;
  z    :  T;
  init := FALSE;
  y    :  CARDINAL := y0;  (*0 will occur as final value*)
BEGIN
  TRY
    WHILE y>0 DO
      IF y MOD 2 # 0 THEN
        IF init THEN
          z := R.Mul(z,pow);
        ELSE
          z := pow;
          init := TRUE;
        END;
      END;
      pow := R.Mul(pow,pow);
      y   := y DIV 2;
    END;
  EXCEPT
    Error(err) => <*ASSERT err # Err.bad_size *>
  END;
  <*ASSERT init*>
  RETURN z;
END Power;

(*==========================*)
BEGIN
END IntegerPower.
