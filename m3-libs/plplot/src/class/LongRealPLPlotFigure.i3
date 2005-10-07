INTERFACE LongRealPLPlotFigure;

IMPORT LongRealPLPlot AS PL, LongRealPLPlotLineStyle AS LineStyle;

TYPE
  Range = RECORD min, max: LONGREAL;  END;
  Box = RECORD x, y: Range;  END;

  (* abstract base class *)
  T = Public;
  Public = OBJECT
           METHODS
             getBox (): Box;
             draw   ();
           END;

  PointSet <: T;

  Lines <: LinesPublic;
  LinesPublic =
    PointSet OBJECT
    METHODS
      init (x, y : REF PL.FloatVector;
            color: CARDINAL             := 2;
            style: LineStyle.T          := NIL; ): Lines;
      initEquidistant (y    : REF PL.FloatVector;
                       from                        := 0.0D0;
                       by                          := 1.0D0;
                       color: CARDINAL             := 2;
                       style: LineStyle.T          := NIL;   ): Lines;
    END;

  Points <: PointsPublic;
  PointsPublic = PointSet OBJECT
                 METHODS
                   init (x, y : REF PL.FloatVector;
                         code : INTEGER;
                         color: CARDINAL             := 2; ): Points;
                 END;


END LongRealPLPlotFigure.
