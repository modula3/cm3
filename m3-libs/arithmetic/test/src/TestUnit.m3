MODULE TestUnit EXPORTS Test;
(*Arithmetic for Modula-3, see doc for details Abstract:

   Tests for PhysicalUnit and related modules.

   *)

IMPORT PhysicalUnit                       AS U,
       LongRealPhysicalValue              AS PV,
       LongRealPhysicalValueFmtLex        AS PVF,
       LongRealFmtLex                     AS RF,
       LongRealSIUnit                     AS SI,
       LongRealComplex                    AS C,
       LongRealComplexFmtLex              AS CF,
       LongRealComplexPhysicalValue       AS CPV,
       LongRealComplexPhysicalValueFmtLex AS CPVF;

IMPORT Fmt;

IMPORT Arithmetic AS Arith;

(*=======================*)
CONST Module = "TestUnit.";
(*----------------------*)
PROCEDURE TestCalc (): BOOLEAN =
  CONST ftn = Module & "TestCalc";
  VAR
    result       := TRUE;
    x            := PV.T{1.0D0, U.FromArray(SI.voltage)};
    y            := PV.T{2.0D0, U.FromArray(SI.voltage)};
    z     : PV.T;
  <* FATAL Arith.Error *>
  BEGIN
    Debug(1, ftn, "begin\n");
    <* ASSERT U.Equal(x.unit, y.unit) *>
    z := PV.Add(x, y);
    <* ASSERT U.Equal(x.unit, z.unit) *>
    <* ASSERT U.Equal(y.unit, z.unit) *>
    z := PV.Sub(x, y);
    <* ASSERT U.Equal(x.unit, z.unit) *>
    <* ASSERT U.Equal(y.unit, z.unit) *>
    z := PV.Mul(x, y);
    <* ASSERT U.Equal(U.Scale(x.unit, 2), z.unit) *>
    z := PV.Div(x, y);
    <* ASSERT U.IsZero(z.unit) *>

    TRY
      y := PV.T{2.0D0, U.FromArray(SI.length)};
      z := PV.Add(x, y);
      <* ASSERT FALSE *>         (*the previous should throw an exception*)
    EXCEPT
    | Arith.Error (err) =>
        <* ASSERT NOT ISTYPE(err, Arith.ErrorUnitMismatch) *>
    END;

    RETURN result;
  END TestCalc;
(*----------------------*)
PROCEDURE TestFmt (): BOOLEAN =
  CONST ftn = Module & "TestFmt";
  VAR
    result    := TRUE;
    x         := PV.T{1.0D0, U.FromArray(SI.voltage)};
    y         := PV.T{0.002D0, U.FromArray(SI.current)};
    si        := SI.CreateDatabase();
    realStyle := RF.FmtStyle{prec := 5};
    style     := PVF.FmtStyle{si, elemStyle := realStyle};
    stylec := CPVF.FmtStyle{
                si, elemStyle := CF.FmtStyle{elemStyle := realStyle}};
  <* FATAL Arith.Error *>
  BEGIN
    Debug(1, ftn, "begin\n");

    Msg(Fmt.FN(
          "%s, %s\n", ARRAY OF TEXT{PVF.Fmt(x, style), PVF.Fmt(y, style)}));

    x := PV.T{1.0D0, U.FromArray(SI.mass)};
    y := PV.T{1.0D0, U.FromArray(SI.speed)};
    FOR n := 0 TO 5 DO
      Msg(Fmt.FN("%s: %s\n", ARRAY OF TEXT{Fmt.Int(n), PVF.Fmt(x, style)}));
      x := PV.Mul(x, y);
    END;

    x := PV.T{10.0D0, U.FromArray(SI.force)};
    y := PV.T{100.0D0, U.FromArray(SI.mass)};

    Msg(
      Fmt.FN("%s / %s = %s\n", ARRAY OF
                                 TEXT{PVF.Fmt(x, style), PVF.Fmt(y, style),
                                      PVF.Fmt(PV.Div(x, y), style)}));

    x := PV.T{330.0D0, U.FromArray(SI.speed)};
    y := PV.T{0.1D0, U.FromArray(SI.length)};

    Msg(
      Fmt.FN("%s / %s = %s\n", ARRAY OF
                                 TEXT{PVF.Fmt(x, style), PVF.Fmt(y, style),
                                      PVF.Fmt(PV.Div(x, y), style)}));

    VAR
      xc := CPV.T{C.T{1.0D-11, 0.0D0}, U.FromArray(SI.voltage)};
      yc := CPV.T{C.T{1.0D1, 1.0D1}, U.FromArray(SI.noUnit)};
    BEGIN
      x := PV.T{1.0D-11, U.FromArray(SI.mass)};
      y := PV.T{10.0D0, U.FromArray(SI.noUnit)};
      FOR n := 0 TO 20 DO
        Msg(Fmt.FN("%2s: %7s, %s\n", ARRAY OF
                                       TEXT{Fmt.Int(n), PVF.Fmt(x, style),
                                            CPVF.Fmt(xc, stylec)}));
        x := PV.Mul(x, y);
        xc := CPV.Mul(xc, yc);
      END;
    END;

    RETURN result;
  END TestFmt;
(*-------------------------*)
PROCEDURE TestUnit (): BOOLEAN =
  <* UNUSED *>
  CONST
    ftn = Module & "TestUnit";
  VAR result := TRUE;
  BEGIN
    NewLine();
    EVAL TestCalc();
    NewLine();
    EVAL TestFmt();
    RETURN result;
  END TestUnit;
(*=======================*)
BEGIN
END TestUnit.
