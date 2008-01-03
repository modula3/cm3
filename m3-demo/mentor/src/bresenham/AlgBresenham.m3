(* Copyright 1993 Digital Equipment Corporation. *)
(* Distributed only by permission. *)

(* Last modified on Tue Aug 17 12:12:43 PDT 1993 by comba    *)
(*      modified on Mon Aug 16 20:05:32 PDT 1993 by harrison *)
(*      modified on Sun Jul 11 20:57:37 PDT 1993 by mhb *)

<* PRAGMA LL *>

MODULE AlgBresenham;

IMPORT Algorithm, Thread, ZeusPanel;
IMPORT BresenhamAlgClass, BresenhamIE;
IMPORT FormsVBT, VBT ;

CONST
  CELLS_WIDTH  = 11;
  CELLS_HEIGHT = 7;

TYPE
  T = BresenhamAlgClass.T BRANDED OBJECT
      OVERRIDES
        run := Run;
      END;

PROCEDURE New (): Algorithm.T =
  BEGIN 
    RETURN 
      NEW(T, data := ZeusPanel.NewForm("bresenhaminput.fv")).init() 
  END New;

PROCEDURE DrawLine (alg: T; x1, y1, x2, y2: INTEGER)
  RAISES {Thread.Alerted} =
VAR
    dx                   := ABS(x1 - x2);
    dy                   := ABS(y1 - y2);
    c1                   := 2 * dy;
    p                    := c1 - dx;
    c2                   := p - dx;
    x, y, x_end: INTEGER;
    firstPixel           := TRUE ;
    old_p                : INTEGER ;
  BEGIN
    BresenhamIE.NewLine(alg, x1, y1, x2, y2);

    BresenhamIE.ErrorInit(alg);

    x := MIN(x1, x2);
    y := MIN(x1, x2);
    x_end := MAX(x1, x2);

    old_p := p ;

    LOOP
      BresenhamIE.ShowPixel(alg, x, y, old_p, p);
      IF firstPixel THEN
        firstPixel := FALSE
      ELSE
        BresenhamIE.Move(alg, old_p);
      END ;

      IF x >= x_end THEN EXIT; END;

      BresenhamIE.FindError(alg, p);
      BresenhamIE.CompareError(alg, p);
      BresenhamIE.ChangeError(alg, p);
      BresenhamIE.ShowNextPixel(alg, p);

      INC(x);

      old_p := p ;
      IF p < 0 THEN p := p + c1; ELSE INC(y); p := p + c2; END;
    END;
  END DrawLine;

PROCEDURE Run (alg: T) RAISES {Thread.Alerted} =
  <* FATAL FormsVBT.Error, FormsVBT.Unimplemented *>
  VAR
    showError : BOOLEAN ;
  BEGIN
    LOCK VBT.mu DO
      showError := FormsVBT.GetBoolean (alg.data, "E") ;
    END ;
    BresenhamIE.Setup(alg, CELLS_WIDTH, CELLS_HEIGHT, showError);

    DrawLine(alg, 0, 0, CELLS_WIDTH - 1, CELLS_HEIGHT - 1);
  END Run;

BEGIN
  ZeusPanel.RegisterAlg(New, "Bresenham Line", "Bresenham");
END AlgBresenham.
