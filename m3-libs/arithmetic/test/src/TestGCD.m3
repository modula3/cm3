MODULE TestGCD EXPORTS Test;
(*Copyright (c) 1996, m3na project
Abstract:  Tests for GCD module.

*)

IMPORT
  Integer32GCD               AS IG,
  Integer32ComplexGCD        AS ICG,
  Integer32ComplexBasic      AS IC,
  Integer32ComplexFmtLex     AS ICF,
  BigIntegerBasic            AS B,
  BigIntegerFmtLex           AS BF,
  BigIntegerComplexBasic     AS BC,
  BigIntegerComplexFmtLex    AS BCF,
  BigIntegerComplexGCD       AS BCG,
  BigIntegerPolynomialBasic  AS BP,
  BigIntegerPolynomialFmtLex AS BPF,
  BigIntegerPolynomialGCD    AS BPG,
  xWordEx AS Wx,
  Word AS W,
  Fmt AS F,
  Text;

(*=======================*)
CONST
  Module = "TestGCD.";

(*----------------------*)
PROCEDURE TestIntegerGCD():BOOLEAN=
CONST
  ftn = Module & "TestIntegerGCD";
  max = 22;
VAR
  gcd : IG.T;
  result := TRUE;
BEGIN
  Debug(1,ftn,"begin\n");

  FOR i:=0 TO max DO
    FOR j:=i TO max DO
      gcd := IG.GCD(i,j);
(*
      Msg(F.FN("gcd(%s,%s) = %s, lcm(%s,%s) = %s\n",
               ARRAY OF TEXT{F.Int(i), F.Int(j), F.Int(gcd),
                             F.Int(i), F.Int(j), F.Int(IG.LCM(i,j))}));
*)
      <*ASSERT gcd=IG.GCD(j,i)*>
      <*ASSERT i=0 OR i MOD gcd=0 AND j MOD gcd=0*>
    END;
  END;

  RETURN result;
END TestIntegerGCD;
(*----------------------*)
PROCEDURE TestComplexGCD():BOOLEAN=
CONST
  ftn = Module & "TestComplexGCD";
  max = 2;
VAR
  x,y,
  gcd0,
  gcd1 : ICG.T;
  result:=TRUE;
BEGIN
  Debug(1,ftn,"begin\n");
(* this is an example where the naive GCD algorithm run into an infinite loop
  x := IC.T{4,1};
  y := IC.T{4,-1};
  Msg(F.FN("gcd(%s,%s) ?\n",
           ARRAY OF TEXT{ICF.Fmt(x), ICF.Fmt(y)}));
  VAR q,r : ICG.T;
  BEGIN
    q := IC.DivMod(x,y,r);
    Msg(F.FN("q=%s,r=%s\t", ARRAY OF TEXT{ICF.Fmt(q), ICF.Fmt(r)}));
    q := IC.DivMod(y,x,r);
    Msg(F.FN("q=%s,r=%s\n", ARRAY OF TEXT{ICF.Fmt(q), ICF.Fmt(r)}));
  END;
  EVAL ICG.GCD(x,y);
*)

  FOR i:=-max TO max DO
    FOR j:=-max TO max DO
      FOR k:=i TO max DO
        FOR l:=-max TO max DO
          x := IC.T{i,j};
          y := IC.T{k,l};
(*
          Msg(F.FN("gcd(%s+%si,%s+%si) ?\n",
                   ARRAY OF TEXT{F.Int(i), F.Int(j),
                                 F.Int(k), F.Int(l)}));
          VAR q,r : ICG.T;
          BEGIN
            q := IC.DivMod(x,y,r);
            Msg(F.FN("q=%s,r=%s\t",
                     ARRAY OF TEXT{ICF.Fmt(q), ICF.Fmt(r)}));
          END;
*)
          gcd0 := ICG.GCD(x,y);
          gcd1 := ICG.GCD(y,x);
(*
          Msg(F.FN("gcd(%s+%si,%s+%si) = %s+%si\t",
                   ARRAY OF TEXT{F.Int(i), F.Int(j),
                                 F.Int(k), F.Int(l),
                                 F.Int(gcd0.re), F.Int(gcd0.im)}));
          Msg(F.FN("gcd(%s+%si,%s+%si) = %s+%si\n",
                   ARRAY OF TEXT{F.Int(k), F.Int(l),
                                 F.Int(i), F.Int(j),
                                 F.Int(gcd1.re), F.Int(gcd1.im)}));
*)
          <*ASSERT IC.IsZero(x) OR
                   IC.IsZero(y) OR
                   (*x and y must be divisible by gcd*)
                   IC.IsZero(IC.Mod(x,gcd0)) AND
                   IC.IsZero(IC.Mod(y,gcd0)) AND
                   (*gcd0 and gcd1 must be equal upto a unit*)
                   IC.IsZero(IC.Mod(gcd0,gcd1)) *>
        END;
      END;
    END;
  END;

  RETURN result;
END TestComplexGCD;
(*----------------------*)
<*UNUSED*>
PROCEDURE TestBigComplexGCD():BOOLEAN=
CONST
  ftn = Module & "TestBigComplexGCD";
VAR
  x,y,
  gcd : BCG.T;
  result:=TRUE;
BEGIN
  Debug(1,ftn,"begin\n");
  FOR i:=-10 TO -10 DO
    FOR j:=-10 TO -10 DO
      FOR k:=-10 TO -10 DO
        FOR l:=-10 TO -10 DO
          x := BC.T{B.FromInteger(i),B.FromInteger(j)};
          y := BC.T{B.FromInteger(k),B.FromInteger(l)};
          Msg(F.FN("x=%s,y=%s\n",
                   ARRAY OF TEXT{BCF.Fmt(x), BCF.Fmt(y)}));
          Msg(F.FN("q=%s,r=%s\n",
                   ARRAY OF TEXT{BCF.Fmt(BC.Div(x,y)), BCF.Fmt(BC.Mod(x,y))}));
          gcd := BCG.GCD(x,y);
          Msg(F.FN("gcd(%s+%si,%s+%si) = %s+%si\n",
                   ARRAY OF TEXT{F.Int(i), F.Int(j),
                                 F.Int(k), F.Int(l),
                                 BF.Fmt(gcd.re), BF.Fmt(gcd.im)}));
          <*ASSERT BC.Equal(gcd,BCG.GCD(y,x))*>
          <*ASSERT BC.IsZero(BC.Mod(x,gcd))*>
          <*ASSERT BC.IsZero(BC.Mod(y,gcd))*>
        END;
      END;
    END;
  END;

  RETURN result;
END TestBigComplexGCD;
(*----------------------*)
PROCEDURE TestPolynomialGCD():BOOLEAN=
CONST
  ftn = Module & "TestPolynomialGCD";
VAR
  fac0, fac1,
  poly0, poly1,
  gcd : BP.T;
  result:=TRUE;
BEGIN
  Debug(1,ftn,"begin\n");
  (*This is a typical example where the naive Euclidean algorithm fails
    due to the fact that none of the operands can be reduced by the other one.*)
  fac0   := BP.New(2);
  fac0^  := BP.TBody{B.FromInteger(1),B.FromInteger(2),B.FromInteger(3)};
  fac1   := BP.New(3);
  fac1^  := BP.TBody{B.FromInteger(1),B.FromInteger(2),B.FromInteger(3),B.FromInteger(1)};
  poly0  := BP.Mul(fac1,BP.Mul(fac0,fac0));
  poly0  := BP.Mul(fac0,fac0);
  Msg(F.FN("%s,%s\n",
           ARRAY OF TEXT{BPF.Fmt(fac0),BPF.Fmt(fac1)}));
  poly1  := BP.Derive(poly0);
  Msg(F.FN("gcd(%s,%s)\n",
           ARRAY OF TEXT{BPF.Fmt(poly0),BPF.Fmt(poly1)}));
  gcd    := BPG.GCD(poly0,poly1);
  Msg(F.FN("gcd(%s,%s) = %s\n",
           ARRAY OF TEXT{BPF.Fmt(poly0),BPF.Fmt(poly1),BPF.Fmt(gcd)}));
  <*ASSERT BP.Equal(gcd,fac0)*>
  RETURN result;
END TestPolynomialGCD;
(*-------------------------*)
PROCEDURE TestGCD():BOOLEAN=
CONST ftn = Module & "TestGCD";
VAR result:=TRUE;
BEGIN
  NewLine(); EVAL TestIntegerGCD();
  NewLine(); EVAL TestComplexGCD();
  (*NewLine(); EVAL TestBigComplexGCD();*)
  NewLine(); EVAL TestPolynomialGCD();

  RETURN result;
END TestGCD;
(*=======================*)
BEGIN
END TestGCD.
