MODULE tBigInteger EXPORTS test;
(*Copyright (c) 1996, m3na project
Abstract:  Tests for BigInteger module.

3/6/96    Harry George   Initial version

*)

IMPORT BigIntegerBasic AS B;
IMPORT BigIntegerFmtLex AS FL;
(*=======================*)
CONST
  Module = "tBigInteger.";
(*----------------------*)
PROCEDURE test_basic():BOOLEAN=
CONST
  ftn = Module & "test_basic";
VAR
  x : B.T;
  result:=TRUE;
BEGIN
  x := B.Zero;
  FOR j:=0 TO 20 DO
    msg(FL.Fmt(x,16));
    x := B.Add(x,B.One);
  END;
  debug(1,ftn,"begin\n");
  RETURN result;   
END test_basic;
(*-------------------------*)
PROCEDURE test_BigInteger():BOOLEAN=
CONST ftn = Module & "test_BigInteger";
VAR result:=TRUE;
BEGIN
  newline(); EVAL test_basic();

  RETURN result;
END test_BigInteger;
(*=======================*)
BEGIN
END tBigInteger.
