MODULE TestBigInteger EXPORTS Test;
(*Copyright (c) 1996, m3na project
Abstract:  Tests for BigInteger module.

3/6/96    Harry George   Initial version

*)

IMPORT
  BigIntegerBasic        AS B,
  BigIntegerRep          AS BR,
  BigIntegerFmtLex       AS BF,
  BigIntegerMatrixBasic  AS BM,
  BigIntegerMatrixFmtLex AS BMF,
  NumberTheory AS NT,
  xWordEx AS Wx,
  Word AS W,
  Fmt AS F,
  Text,Wr,Thread,xUtils;
FROM BigIntegerMatrixIntegerPower IMPORT Power;
(*=======================*)
CONST
  Module = "TestBigInteger.";

CONST
  base2Style  = BF.FmtStyle{base:=2};
  base16Style = BF.FmtStyle{base:=16};
(*----------------------*)
<*FATAL xUtils.Error*>
PROCEDURE TestBasic():BOOLEAN=
CONST
  ftn = Module & "TestBasic";
VAR
  result:=TRUE;
  carry:=TRUE;
  hi,lo:W.T;
  x,y:B.T;
BEGIN
  Debug(1,ftn,"begin\n");
  Msg("W.Size " & F.Int(W.Size) & "\n");
  Msg("W.GT(2,3) " & F.Bool(W.GT(2,3)) & "\n");
  Msg("PlusWithCarry " & F.Int(Wx.PlusWithCarry(2,3,carry)) & "\n");
  Msg("MinusWithBorrow " & F.Int(Wx.MinusWithBorrow(2,3,carry)) & "\n");
  Msg("MinusWithBorrow " & F.Int(Wx.MinusWithBorrow(5,3,carry)) & "\n");
  Msg("MinusWithBorrow " & F.Int(Wx.MinusWithBorrow(5,3,carry)) & "\n");
  Wx.DoubleLengthMultiply(7,16_249,lo,hi);
  Msg("DoubleLengthMultiply " & F.Int(hi,16) & " " & F.Int(lo,16) & "\n");
  Msg("Plus " & F.Int(W.Plus(3,2)) & "\n");
(*
  x:=B.FromInteger(16_88d390);
  y:=B.Add(B.Mul(B.FromInteger(16_d6c5),B.FromInteger(16_10000)),B.FromInteger(16_61ad));
*)
  x:=B.FromInteger(16_800000);
  y:=B.Mul(B.FromInteger(16_8000),B.FromInteger(16_10000));
  <*ASSERT B.Equal(B.Mod(x,y),x) *>
  x:=B.Add(B.Mul(B.FromInteger(16_d6c5),B.FromInteger(16_10000)),
           B.FromInteger(16_61ad));
  y:=B.Add(B.Mul(B.FromInteger(16_d63c),B.FromInteger(16_10000)),
           B.FromInteger(16_8e1d));
  <*ASSERT B.Equal(B.Mod(x,y),B.Sub(x,y)) *>

  RETURN result;
END TestBasic;
(*----------------------*)
PROCEDURE TestPower():BOOLEAN=
CONST
  ftn = Module & "TestPower";
  cycles  = 52;
VAR
  x, y, z : B.T;
  fff     : TEXT;
  result  := TRUE;
BEGIN
  Debug(1,ftn,"begin\n");
  x := B.FromInteger(8);
  y := B.One;
  z := B.Zero;

  FOR j:=0 TO cycles-1 DO
    (*Msg(F.FN("%2s: 16_%s, 2_%s\n", ARRAY OF TEXT{F.Int(j), BF.Fmt(z,base16Style), BF.Fmt(z,base2Style)}));*)
    z := BR.AddU(z,y);
    y := BR.MulU(y,x);
  END;
  (*Msg(F.FN("%2s: 16_%s, 2_%s\n", ARRAY OF TEXT{F.Int(cycles), BF.Fmt(z,base16Style), BF.Fmt(z,base2Style)}));*)
  z := BR.MulU(z,B.FromInteger(7));
  fff := BF.Fmt(z,base16Style);
  Msg("multiply with 7: 16_" & fff & ": 2_" & BF.Fmt(z,base2Style) & "\n");
  <*ASSERT Text.Length(fff) = (cycles DIV 4)*3 *>
  FOR j:=0 TO Text.Length(fff)-1 DO
    <*ASSERT Text.GetChar(fff,j)='f'*>
  END;

  RETURN result;
END TestPower;
(*----------------------*)
PROCEDURE TestAddshift():BOOLEAN=
CONST
  ftn = Module & "TestAddshift";
  cycles  = 4*13;
VAR
  x, y    : B.T;
  q, r    : B.T;
  result  := TRUE;
  sh      := BR.BitPos{0,0};
BEGIN
  Debug(1,ftn,"begin\n");
  x.data := NEW(BR.Value,cycles*3 DIV W.Size +2);
  x.size := NUMBER(x.data^);
  BR.Clear(x.data);

  FOR j:=0 TO cycles DO
(*
    Msg(F.FN("%2s: bit %02s,%02s; 16_%s\n",
             ARRAY OF TEXT{F.Int(j), F.Int(sh.word), F.Int(sh.bit), BF.Fmt(x,base16Style)}));
*)
(*
    Msg(F.FN("%2s: bit %s,%s; 16_%s, 2_%s\n",
             ARRAY OF TEXT{F.Int(j), F.Int(sh.word), F.Int(sh.bit), BF.Fmt(x,base16Style), BF.Fmt(x,base2Style)}));
*)
    BR.AddShifted(x,j,sh);
    sh := BR.AddBitPos(sh,BR.BitPos{0,3});
  END;

  sh := BR.BitPos{0,0};
  FOR j:=0 TO cycles DO
(*
    Msg(F.FN("%2s: bit %02s,%02s; 16_%s\n",
             ARRAY OF TEXT{F.Int(j), F.Int(sh.word), F.Int(sh.bit), BF.Fmt(x,base16Style)}));
*)
    BR.AddShifted(x,cycles-j,sh);
    sh := BR.AddBitPos(sh,BR.BitPos{0,3});
  END;

(*
  sh := BR.BitPos{0,0};
  quotient := cycles * 2_1001001001;
  FOR j:=0 TO cycles DO
    Msg(F.FN("%2s: bit %02s,%02s; 16_%s\n",
             ARRAY OF TEXT{F.Int(j), F.Int(sh.word), F.Int(sh.bit), BF.Fmt(x,base16Style)}));
    BR.SubShiftedProd(x,cycles-j,sh);
    sh := BR.AddBitPos(sh,BR.BitPos{0,3});
  END;
*)
  x := B.One;
  FOR j:=0 TO 13 DO
    x := BR.AddU(B.One,BR.MulU(x,B.FromInteger(16_1000)));
  END;
  y := BR.MulU(B.FromInteger(16_1000001),B.FromInteger(16_1001));
(*
  x := B.FromInteger(16_1001001);
  FOR j:=0 TO 1 DO
    x := BR.MulU(x,x);
  END;
  x := BR.AddU(x,B.One);
  y := B.FromInteger(16_1001001);
  y := BR.MulU(y,y);
*)
  Msg(F.FN("x = 16_%s   y = 16_%s\n",
           ARRAY OF TEXT{BF.Fmt(x,base16Style), BF.Fmt(y,base16Style)}));
  q := BR.DivModU(x,y,r);
  Msg(F.FN("q = 16_%s   r = 16_%s\n",
           ARRAY OF TEXT{BF.Fmt(q,base16Style), BF.Fmt(r,base16Style)}));
  q.sign := FALSE;
  r.sign := FALSE;
  <*ASSERT B.Equal(x,BR.AddU(r,BR.MulU(q,y)))*>

  <*ASSERT NOT B.IsZero(B.Mod(B.FromInteger(16_4f7d3f), B.FromInteger(16_37))) *>

  RETURN result;
END TestAddshift;
(*----------------------*)
<*FATAL Wr.Failure, Thread.Alerted*>
PROCEDURE TestFibonacci():BOOLEAN=
CONST
  ftn = Module & "TestFibonacci";
TYPE
  RowBody    = ARRAY [0..1] OF B.T;
  MatrixBody = ARRAY [0..1] OF RowBody;
CONST
  cycles = 250;
VAR
  x, y, z   : B.T;
  base, pow : BM.T;
  result:=TRUE;
BEGIN
  Debug(1,ftn,"begin\n");
  x := B.Zero;
  y := B.One;

  base  := BM.New(2,2);
  base^ := MatrixBody{RowBody{B.Zero,B.One},RowBody{B.One,B.One}};

  FOR j:=0 TO cycles DO
(*
    Msg(F.FN("%2s: 16_%s, 10_%s\n",
      ARRAY OF TEXT {F.Int(j), BF.Fmt(x,base16Style), BF.Fmt(x)}));
*)
    z := BR.AddU(x,y);
    x := y;
    y := z;
    pow := Power(base,j+1);
(*
    Msg(F.FN("%2s: 16_%s = 16_%s\n",
      ARRAY OF TEXT {
        F.Int(j),
        BF.Fmt(y,base16Style),
        BF.Fmt(pow[1,1],base16Style)
      }
    ));
*)
    <*ASSERT B.Equal(y,pow[1,1])*>
  END;
  Msg(BF.Fmt(x,base16Style)); NewLine();

  pow := Power(base,cycles);
  Msg(BMF.Fmt(pow,BMF.FmtStyle{elemStyle:=base16Style})); NewLine();
  <*ASSERT B.Equal(x,pow[1,1])*>

  RETURN result;
END TestFibonacci;
(*-------------------------*)
PROCEDURE TestPseudoprime():BOOLEAN=
(*
This prime Test works for many numbers but not for all.
*)
CONST
  ftn = Module & "TestPseudoprime";
VAR
  x : ARRAY [0..3] OF B.T;
  mod : B.T;
  result:=TRUE;
  prime0, prime1 : BOOLEAN;
BEGIN
  Debug(1,ftn,"begin\n");
  x[0] := B.FromInteger(3);
  x[1] := B.Zero;
  x[2] := B.FromInteger(2);

  FOR j:=3 TO 1000 DO
    x[3] := B.Add (x[0], x[1]);
    (*Msg(F.FN("%s / %s\n", ARRAY OF TEXT {BF.Fmt(x[3],base16Style), F.Int(j,16)}));*)
    mod := B.Mod(x[3],B.FromInteger(j));
    prime0 := B.IsZero(mod);
    prime1 := NT.IsPrime(j);
    (*
    Msg(F.FN("%2s: %s, mod %s prime %s vs. %s\n",
      ARRAY OF TEXT {F.Int(j), BF.Fmt(x[3],10), BF.Fmt(mod,10), F.Bool(prime0), F.Bool(prime1)}));
    *)
    <*ASSERT prime0=prime1*>
    x[0] := x[1];
    x[1] := x[2];
    x[2] := x[3];
  END;
  RETURN result;
END TestPseudoprime;
(*-------------------------*)
PROCEDURE TestBigInteger():BOOLEAN=
<*UNUSED*>
CONST ftn = Module & "TestBigInteger";
VAR result:=TRUE;
BEGIN
  NewLine(); EVAL TestBasic();
  NewLine(); EVAL TestPower();
  NewLine(); EVAL TestAddshift();
  NewLine(); EVAL TestFibonacci();
  NewLine(); EVAL TestPseudoprime();

  RETURN result;
END TestBigInteger;
(*=======================*)
BEGIN
END TestBigInteger.
