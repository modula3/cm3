GENERIC MODULE Combinatoric(R);
(*Copyright (c) 1996, m3na project

Abstract: Combinatoric operations

*)

FROM xUtils IMPORT Error;

<*UNUSED*> CONST Module = "Combinatoric.";
(*==========================*)

(* Fakultät von n *)
PROCEDURE Factorial (n : T) : T =
VAR
  num := R.One;

BEGIN
  WHILE NOT R.IsZero(n) DO
    num := R.Mul(num,n);
    n := R.Sub(n,R.One);
  END;
  RETURN num;
END Factorial;

(* Permutationen *)
PROCEDURE Permutations (READONLY n : ARRAY OF T) : T =
(*
possible optimizations:
1. start with the biggest number n[j],
   the computation time for the corresponding term can be saved completely
2. calculating binomial coefficients might be sped up by
   keeping a list of machine size integer factors
*)
VAR
  num:=R.One;
  k,div : T;

BEGIN
  TRY
    IF NUMBER(n)=0 THEN
      RETURN R.One;
    END;
    k:=n[0];
    FOR j:=1 TO LAST(n) DO
      div:=R.Zero;
      WHILE NOT R.Equal(div,n[j]) DO
        k  :=R.Add(k,  R.One);
        div:=R.Add(div,R.One);
        num:=R.Div(R.Mul(num,k),div);
      END;
    END;
  EXCEPT
    | Error(err) => EVAL err; <*ASSERT FALSE*>
  END;
  RETURN num;
END Permutations;

(* Variationen ohne Wiederholung von n zur Klasse k *)
PROCEDURE Arrangements (n, k : T) : T =
VAR
  num := R.One;

BEGIN
  WHILE NOT R.IsZero(k) DO
    num:=R.Mul(num,n);
    n:=R.Sub(n,R.One);
    k:=R.Sub(k,R.One);
  END;
  RETURN num;
END Arrangements;

(* Variationen mit Wiederholung von n zur Klasse k *)
(*similar to the routine IntegerPower*)
PROCEDURE ArrangementsR (n, k : T) : T =
VAR
  num := R.One;
  r   :  T;

BEGIN
  TRY
    WHILE NOT R.IsZero(k) DO
      k := R.DivMod(k,R.Two,r);
      IF NOT R.IsZero(r) THEN
        num := R.Mul(num,n);
      END;
      n := R.Mul(n,n);
    END;
  EXCEPT
    | Error(err) => EVAL err; <*ASSERT FALSE*>
  END;
  RETURN num;
END ArrangementsR;

(* Kombinationen ohne Wiederholung von n zur Klasse k *)
PROCEDURE Combinations (n, k : T) : T =
BEGIN
  RETURN Permutations (ARRAY OF T{k, R.Sub(n,k)}) ;
END Combinations;

(* Kombinationen mit Wiederholung von n zur Klasse k *)
PROCEDURE CombinationsR (n, k : T) : T =
BEGIN
  (*
  RETURN Combinations (n+k-1, k);
  RETURN Permutations (ARRAY OF T{n-1, k});
  *)
  RETURN Permutations (ARRAY OF T{R.Sub(n,R.One), k});
END CombinationsR;


(*==========================*)

BEGIN
END Combinatoric.
