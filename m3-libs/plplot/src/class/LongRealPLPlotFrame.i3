INTERFACE LongRealPLPlotFrame;

IMPORT LongRealPLPlotFigure AS Figure;

TYPE
  T <: Public;
  Public = OBJECT METHODS draw (); END;

  Single <: SinglePublic;
  SinglePublic =
    T OBJECT
    METHODS
      init (READONLY figures: ARRAY OF Figure.T; color: CARDINAL := 1; ):
            Single;
      add       (figure: Figure.T; );
      setBox    (box: Figure.Box; );
      setRangeH (rng: Figure.Range; );
      setRangeV (rng: Figure.Range; );
      getBox    (): Figure.Box;
      (* setBoxAuto (spare: LONGREAL := 0.1D0; ); *)
      setAspect (aspect: LONGREAL; )
    END;

  Matrix <: MatrixPublic;
  MatrixPublic =
    T OBJECT
    METHODS
      init (READONLY frames: ARRAY OF ARRAY OF Single; ): Matrix;
    END;

END LongRealPLPlotFrame.
