MODULE Main;

IMPORT FFTWLongReal AS FFT, IO, Fmt;

VAR
  x := NEW(REF ARRAY OF FFT.Complex, 23);
  y := NEW(REF ARRAY OF FFT.Complex, 23);

<* FATAL FFT.SizeMismatch *>
BEGIN
  FOR n := FIRST(x^) TO LAST(x^) DO x[n].r := 0.0D0; x[n].i := 0.0D0; END;
  x[1].r := 1.0D0;
  FFT.Execute(FFT.PlanDFT1D(x, y, FFT.Dir.Forward));
  FOR n := FIRST(y^) TO LAST(y^) DO
    IO.Put(Fmt.F("%03s: %s %s\n", Fmt.Int(n), Fmt.LongReal(y[n].r),
                 Fmt.LongReal(y[n].i)));
  END;
END Main.
