MODULE tBigInteger EXPORTS test;
(*Copyright (c) 1996, m3na project
Abstract:  Tests for BigInteger module.

3/6/96    Harry George   Initial version

*)

IMPORT
  BigIntegerBasic  AS B,
  BigIntegerRep    AS Br,
  BigIntegerFmtLex AS FL,
  xInteger   AS I,
  xWordEx AS Wx,
  Word AS W,
  Fmt AS F,
  Text;
(*=======================*)
CONST
  Module = "tBigInteger.";
(*----------------------*)
PROCEDURE test_basic():BOOLEAN=
CONST
  ftn = Module & "test_basic";
VAR
  result:=TRUE;
  carry:=TRUE;
BEGIN
  debug(1,ftn,"begin\n");
  msg("W.Size " & F.Int(W.Size) & "\n");
  msg("W.GT(2,3) " & F.Bool(W.GT(2,3)) & "\n");
  msg("PlusWithCarry " & F.Int(Wx.PlusWithCarry(2,3,carry)) & "\n");
  msg("MinusWithBorrow " & F.Int(Wx.MinusWithBorrow(2,3,carry)) & "\n");
  msg("MinusWithBorrow " & F.Int(Wx.MinusWithBorrow(5,3,carry)) & "\n");
  msg("MinusWithBorrow " & F.Int(Wx.MinusWithBorrow(5,3,carry)) & "\n");
  msg("Plus " & F.Int(W.Plus(3,2)) & "\n");
  RETURN result;
END test_basic;
(*----------------------*)
PROCEDURE test_power():BOOLEAN=
CONST
  ftn = Module & "test_power";
  cycles  = 52;
VAR
  x, y, z : B.T;
  fff     : TEXT;
  result  := TRUE;
BEGIN
  debug(1,ftn,"begin\n");
  x := B.FromInteger(8);
  y := B.One;
  z := B.Zero;

  FOR j:=0 TO cycles DO
    msg(F.Pad(F.Int(j),2) & ": 16_" & FL.Fmt(z,16) & ": 2_" & FL.Fmt(z,2) & "\n");
    z := Br.AddU(z,y);
    y := Br.MulU(y,x);
  END;
  z := Br.MulU(z,B.FromInteger(7));
  fff := FL.Fmt(z,16)
  msg("multiply with 7: 16_" & fff & ": 2_" & FL.Fmt(z,2) & "\n");
  <*ASSERT Text.Length(fff) = (cycles DIV 4)*3 *>
  FOR j:=0 TO Text.Length(fff) DO
    <*ASSERT Text.GetChar(fff,j)='f'*>
  END;

  RETURN result;
END test_power;
(*----------------------*)
PROCEDURE test_fibonacci():BOOLEAN=
CONST
  ftn = Module & "test_fibonacci";
VAR
  x, y, z : B.T;
  result:=TRUE;
BEGIN
  debug(1,ftn,"begin\n");
  x := B.Zero;
  y := B.One;
  (*x := B.FromInteger(16_7FFFFFFF);*)

  FOR j:=0 TO 100 DO
    (*msg("size: " & F.Int(x.size) & "\n");*)
    msg(F.Pad(F.Int(j),2) & ": 16_" & FL.Fmt(x,16) & ": 2_" & FL.Fmt(x,2) & "\n");
    z := Br.AddU(x,y);
    x := y;
    y := z;
  END;

  (*!!! compare with explicit formula !!!*)
  RETURN result;
END test_fibonacci;
(*-------------------------*)
PROCEDURE test_pseudoprime():BOOLEAN=
CONST
  ftn = Module & "test_pseudoprime";
VAR
  x : ARRAY [0..3] OF B.T;
  result:=TRUE;
  prime0, prime1 : BOOLEAN;
BEGIN
  debug(1,ftn,"begin\n");
  x[0] := B.FromInteger(3);
  x[1] := B.Zero;
  x[2] := B.FromInteger(2);

  FOR j:=3 TO 100 DO
    x[3] := B.Add (x[0], x[1]);
    prime0 := B.Equal(B.Zero, B.Mod(x[3],B.FromInteger(j)));
    prime1 := I.isprime(j);
    msg(F.Pad(F.Int(j),2) & ": " & FL.Fmt(x[3],10) & ", prime " & F.Bool(prime0) & "vs. " & F.Bool(prime1) & "\n");
    <*ASSERT prime0=prime1*>
    x[0] := x[1];
    x[1] := x[2];
    x[2] := x[3];
  END;
  RETURN result;
END test_pseudoprime;
(*-------------------------*)
PROCEDURE test_BigInteger():BOOLEAN=
CONST ftn = Module & "test_BigInteger";
VAR result:=TRUE;
BEGIN
  newline(); EVAL test_basic();
  newline(); EVAL test_power();
  (*newline(); EVAL test_fibonacci();*)
  newline(); EVAL test_pseudoprime();

  RETURN result;
END test_BigInteger;
(*=======================*)
BEGIN
END tBigInteger.
