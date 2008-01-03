MODULE Main;

IMPORT LongRealFFTW AS FFT, IO, Fmt;

VAR
  x := NEW(REF ARRAY OF FFT.Complex, 23);
  y := NEW(REF ARRAY OF FFT.Complex, 23);

<* FATAL FFT.SizeMismatch *>
BEGIN
  FOR n := FIRST(x^) TO LAST(x^) DO x[n].re := 0.0D0; x[n].im := 0.0D0; END;
  x[1].re := 1.0D0;
  (* provoke garbage collector *)
  FOR i := 0 TO 1000 DO
    FFT.Execute(FFT.PlanDFT1D(x, y, FFT.Dir.Forward));
  END;
  FOR n := FIRST(y^) TO LAST(y^) DO
    IO.Put(Fmt.F("%03s: %s %s\n", Fmt.Int(n), Fmt.LongReal(y[n].re),
                 Fmt.LongReal(y[n].im)));
  END;
END Main.
