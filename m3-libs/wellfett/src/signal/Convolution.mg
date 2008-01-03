GENERIC MODULE Convolution(R, P, VS, C, CV, FFT);


(* One can improve performance a lot here: Eliminate the monolithic Fourier
   transforms by using overlapping blocks for convolution.  Maybe FFTW
   supports that in a future version.  Store FFTW plans to accelerate
   repeated FFT application. *)


REVEAL
  Fourier = T BRANDED OBJECT
              xFT   : CV.T;
              number: CARDINAL;
              width : Width;
            OVERRIDES
              init     := FourierInit;
              exit     := FourierExit;
              convolve := FourierConvolve;
            END;

PROCEDURE FourierInit (h: Fourier; x: P.T; width: Width; ): T =
  VAR
    xNumber := NUMBER(x^);
    number  := xNumber + width - 1;
    padded  := NEW(P.T, number);
  BEGIN
    (* After two discrete Fourier transforms we have to scale the resulting
       signal by the size of the signal data.  We do the scaling now once
       instead of scaling after each synthesis transformation. *)
    SUBARRAY(padded^, 0, xNumber) :=
      P.Scale(x, R.One / R.FromInteger(number))^;
    VS.Clear(SUBARRAY(padded^, xNumber, number - xNumber));
    h.xFT := FFT.DFTR2C1D(padded^);
    h.number := number;
    h.width := width;
    RETURN h;
  END FourierInit;

PROCEDURE FourierExit (h: Fourier; ) =
  BEGIN
    h.xFT := NIL;
  END FourierExit;

PROCEDURE FourierConvolve (h: Fourier; y: P.T; ): P.T =
  VAR z := NEW(P.T, h.number);
  BEGIN
    <* ASSERT NUMBER(y^) <= h.width,
                "y bigger than the width "
                  & "specified at initialization of convolution." *>
    SUBARRAY(z^, 0, NUMBER(y^)) := y^;
    VS.Clear(SUBARRAY(z^, NUMBER(y^), NUMBER(z^) - NUMBER(y^)));
    WITH zFT = FFT.DFTR2C1D(z^)^ DO
      FOR i := FIRST(zFT) TO LAST(zFT) DO
        zFT[i] := C.Mul(zFT[i], h.xFT[i]);
      END;
      RETURN FFT.DFTC2R1D(zFT, h.number MOD 2);
    END;
  END FourierConvolve;


REVEAL
  Naive = T BRANDED OBJECT
            x: P.T;
          OVERRIDES
            init     := NaiveInit;
            exit     := NaiveExit;
            convolve := NaiveConvolve;
          END;

PROCEDURE NaiveInit (h: Naive; x: P.T; <* UNUSED *> width: Width; ): T =
  BEGIN
    h.x := x;
    RETURN h;
  END NaiveInit;

PROCEDURE NaiveExit (h: Naive; ) =
  BEGIN
    h.x := NIL;
  END NaiveExit;

PROCEDURE NaiveConvolve (h: Naive; y: P.T; ): P.T =
  BEGIN
    RETURN P.Mul(h.x, y);
  END NaiveConvolve;



BEGIN
END Convolution.
