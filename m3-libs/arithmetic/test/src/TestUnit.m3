MODULE TestUnit EXPORTS Test;
(*Copyright (c) 1996, m3na project
Abstract:  Tests for PhysicalUnit and related modules.

*)

IMPORT PhysicalUnit                AS U,
       LongRealPhysicalValue       AS PV,
       LongRealPhysicalValueFmtLex AS PVF,
       LongRealSIUnit              AS SI,
       Fmt,
       NADefinitions;

(*=======================*)
CONST
  Module = "TestUnit.";
(*----------------------*)
PROCEDURE TestCalc():BOOLEAN=
CONST
  ftn = Module & "TestCalc";
VAR
  result:=TRUE;
  x:=PV.T{1.0D0,U.FromArray(SI.voltage)};
  y:=PV.T{2.0D0,U.FromArray(SI.voltage)};
  z:PV.T;
<*FATAL NADefinitions.Error*>
BEGIN
  Debug(1,ftn,"begin\n");
  <*ASSERT U.Equal(x.unit,y.unit)*>
  z:=PV.Add(x,y);
  <*ASSERT U.Equal(x.unit,z.unit)*>
  <*ASSERT U.Equal(y.unit,z.unit)*>
  z:=PV.Sub(x,y);
  <*ASSERT U.Equal(x.unit,z.unit)*>
  <*ASSERT U.Equal(y.unit,z.unit)*>

  RETURN result;
END TestCalc;
(*----------------------*)
PROCEDURE TestFmt():BOOLEAN=
CONST
  ftn = Module & "TestFmt";
VAR
  result:=TRUE;
  x:=PV.T{1.0D0,U.FromArray(SI.voltage)};
  y:=PV.T{0.002D0,U.FromArray(SI.current)};
  style:=PVF.FmtStyle{SI.CreateDatabase()};
<*FATAL NADefinitions.Error*>
BEGIN
  Debug(1,ftn,"begin\n");

  Msg(Fmt.FN("%s, %s\n", ARRAY OF TEXT
      {PVF.Fmt(x,style), PVF.Fmt(y,style)}));

  x:=PV.T{1.0D0,U.FromArray(SI.mass)};
  y:=PV.T{1.0D0,U.FromArray(SI.speed)};
  FOR n:=0 TO 5 DO
    Msg(Fmt.FN("%s: %s\n", ARRAY OF TEXT
        {Fmt.Int(n), PVF.Fmt(x,style)}));
    x:=PV.Mul(x,y);
  END;

  x:=PV.T{10.0D0,U.FromArray(SI.force)};
  y:=PV.T{100.0D0,U.FromArray(SI.mass)};

  Msg(Fmt.FN("%s / %s = %s\n", ARRAY OF TEXT
      {PVF.Fmt(x,style), PVF.Fmt(y,style),
       PVF.Fmt(PV.Div(x,y),style)}));

  x:=PV.T{330.0D0,U.FromArray(SI.speed)};
  y:=PV.T{0.1D0,U.FromArray(SI.length)};

  Msg(Fmt.FN("%s / %s = %s\n", ARRAY OF TEXT
      {PVF.Fmt(x,style), PVF.Fmt(y,style),
       PVF.Fmt(PV.Div(x,y),style)}));

  x:=PV.T{1.0D-11,U.FromArray(SI.mass)};
  y:=PV.T{10.0D0,U.FromArray(SI.noUnit)};
  FOR n:=0 TO 20 DO
    Msg(Fmt.FN("%s: %s\n", ARRAY OF TEXT
        {Fmt.Int(n), PVF.Fmt(x,style)}));
    x:=PV.Mul(x,y);
  END;

  RETURN result;
END TestFmt;
(*-------------------------*)
PROCEDURE TestUnit():BOOLEAN=
CONST ftn = Module & "TestUnit";
VAR result:=TRUE;
BEGIN
  NewLine(); EVAL TestCalc();
  NewLine(); EVAL TestFmt();
  RETURN result;
END TestUnit;
(*=======================*)
BEGIN
END TestUnit.
