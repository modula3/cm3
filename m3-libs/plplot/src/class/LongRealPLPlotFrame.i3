INTERFACE LongRealPLPlotFrame;

IMPORT LongRealPLPlotFigure AS Figure;

TYPE
  T <: Public;
  Public = OBJECT
           METHODS
           (* draw the frame with the left bottom corner at (x,y) *)
             draw     (x,y:LONGREAL;);
             
             (* get total size of the box and its labels in millimeter *)
             getSizeX (): LONGREAL;
             getSizeY (): LONGREAL;
           END;

  Single <: SinglePublic;
  SinglePublic =
    T OBJECT
    METHODS
      init (READONLY figures: ARRAY OF Figure.T; color: CARDINAL := 1; ):
            Single;
      add       (figure: Figure.T; );
      setRange  (range: Figure.Box; );
      setRangeX (range: Figure.Range; );
      setRangeY (range: Figure.Range; );
      getRange  (): Figure.Box;
      (* setRangeAuto (spare: LONGREAL := 0.1D0; ); *)

      (* set the size relative to the page width *)
      setRelSizeX (size: LONGREAL; );
      setRelSizeY (size: LONGREAL; );
    END;

  Matrix <: MatrixPublic;
  MatrixPublic = T OBJECT
                 METHODS
                   init (READONLY frames: ARRAY OF ARRAY OF T; ): Matrix;
                 END;

END LongRealPLPlotFrame.
