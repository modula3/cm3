MODULE Main;

IMPORT FFT,Random,IO;
IMPORT LongRealComplex AS LC;
IMPORT LongRealComplexFmtLex AS LFC;

CONST N = 1024*4;

PROCEDURE Test() =
  VAR
    fft : FFT.T;
    in : FFT.FFTArr;
    r := NEW(Random.Default).init();
  BEGIN
    in := NEW(FFT.FFTArr,8);
    fft := NEW(FFT.T);
    FOR i := 0 TO 7 DO
      in[i] := LC.T{FLOAT(i,LONGREAL),0.0D0};
    END;
    (* debug *)
    FOR i := 0 TO 7 DO
      IO.Put(LFC.Fmt(in[i]) & "\n");
    END;

    IO.Put("forward\n");

    fft.forward(in);
    fft.inverse(in);

    FOR i := 0 TO 7 DO
      IO.Put(LFC.Fmt(in[i]) & "\n");
    END;

    (* random test *)
    in := NEW(FFT.FFTArr,N);
    FOR i := 0 TO N-1 DO
      in[i] := LC.T{r.longreal(),0.0D0};
    END;

    fft.forward(in);

    FOR i := 0 TO N - 1 DO
      IO.Put(LFC.Fmt(in[i]) & "\n");
    END;

  END Test;

BEGIN
  Test();
END Main.
