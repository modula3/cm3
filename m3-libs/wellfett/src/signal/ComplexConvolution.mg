GENERIC MODULE ComplexConvolution(CP, CVS, C, CV, FFT);


(* One can improve performance a lot here: Eliminate the monolithic Fourier
   transforms by using overlapping blocks for convolution.  Maybe FFTW
   supports that in a future version.  Store FFTW plans to accelerate
   repeated FFT application. *)


REVEAL
  HandleFourier = Handle BRANDED OBJECT
                    xFT   : CV.T;
                    number: CARDINAL;
                    width : Width;
                  OVERRIDES
                    init     := HandleFourierInit;
                    convolve := HandleFourierConvolve;
                  END;

PROCEDURE HandleFourierInit (h: HandleFourier; x: CP.T; width: Width; ):
  Handle =
  VAR
    xNumber := NUMBER(x^);
    number  := xNumber + width - 1;
    padded  := NEW(CP.T, number);
  BEGIN
    SUBARRAY(padded^, 0, xNumber) := x^;
    CVS.Clear(SUBARRAY(padded^, xNumber, number - xNumber));
    h.xFT := FFT.DFTC2C1D(padded^);
    h.number := number;
    h.width := width;
    RETURN h;
  END HandleFourierInit;

PROCEDURE HandleFourierConvolve (h: HandleFourier; y: CP.T; ): CP.T =
  VAR z := NEW(CP.T, h.number);
  BEGIN
    <* ASSERT NUMBER(y^) <= h.width,
                "y bigger than the width "
                  & "specified at initialization of convolution." *>
    SUBARRAY(z^, 0, NUMBER(y^)) := y^;
    CVS.Clear(SUBARRAY(z^, NUMBER(y^), NUMBER(z^) - NUMBER(y^)));
    WITH zFT = FFT.DFTC2C1D(z^)^ DO
      FOR i := FIRST(zFT) TO LAST(zFT) DO
        zFT[i] := C.Mul(zFT[i], h.xFT[i]);
      END;
      RETURN FFT.DFTC2C1D(zFT, FFT.Dir.Forward);
    END;
  END HandleFourierConvolve;


REVEAL
  HandleNaive = Handle BRANDED OBJECT
                  x: CP.T;
                OVERRIDES
                  init     := HandleNaiveInit;
                  convolve := HandleNaiveConvolve;
                END;

PROCEDURE HandleNaiveInit (             h    : HandleNaive;
                                        x    : CP.T;
                           <* UNUSED *> width: Width;       ): Handle =
  BEGIN
    h.x := x;
    RETURN h;
  END HandleNaiveInit;

PROCEDURE HandleNaiveConvolve (h: HandleNaive; y: CP.T; ): CP.T =
  BEGIN
    RETURN CP.Mul(h.x, y);
  END HandleNaiveConvolve;



BEGIN
END ComplexConvolution.
