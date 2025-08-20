MODULE NR4p4;
IMPORT LRFunction AS Function;
FROM PolInt IMPORT Interpolate;
IMPORT Math;
IMPORT MapError;
IMPORT Debug;
FROM Fmt IMPORT F, LongReal, Int;
<*FATAL MapError.E*>

CONST LR      = LongReal;
      DoDebug = FALSE;

PROCEDURE Qromo(func   : Function.T;
                a, b   : LONGREAL;
                choose : Chooser) : LONGREAL RAISES { TooManyIters } =
  CONST
    Jmax  = 14;
    Eps   = 1.0d-6;
    K     = 5;
  VAR
    h, s : ARRAY [ 0 .. Jmax ] OF LONGREAL; (* Jmax + 1 elements *)
    ss, dss : LONGREAL;
  BEGIN
    h[0] := 1.0d0;
    FOR j := 0 TO Jmax - 1 DO
      choose(func, a, b, s[j], j + 1);

      IF DoDebug THEN
        Debug.Out(F("j=%s s[j]=%s", Int(j), LR(s[j])))
      END;
                    
      IF j >= K - 1 THEN
        WITH ires = Interpolate(SUBARRAY(h, j - K + 1, K),
                                SUBARRAY(s, j - K + 1, K),
                                0.0d0) DO
          ss  := ires.y;
          dss := ires.dy;

          IF DoDebug THEN
            Debug.Out(F("ss=%s dss=%s", LR(ss), LR(dss)))
          END
        END;
        IF ABS(dss) <= Eps * ABS(ss) THEN
          RETURN ss
        END
      END;
      s[j + 1] := s[j];
      h[j + 1] := h[j] / 9.0d0;
      
      IF DoDebug THEN
        Debug.Out(F("s[j+1]=%s h[j+1]=%s", LR(ss), LR(dss)))
      END
    END;
    RAISE TooManyIters
  END Qromo;

PROCEDURE Midpoint(f     : Function.T;
                   a, b  : LONGREAL;
                   VAR s : LONGREAL;
                   n     : CARDINAL) =
  VAR
    tnm, del, ddel, x, sum : LONGREAL;
    it : CARDINAL;
    
  BEGIN
    IF n = 1 THEN
      s := (b - a) * f.eval(0.5d0 * (a + b))
    ELSE
      it   := ROUND(Math.pow(3.0d0, FLOAT(n - 2, LONGREAL)));
      tnm  := FLOAT(it, LONGREAL);
      del  := (b - a) / (3.0d0 * tnm);
      ddel := del + del;
      x    := a + 0.5d0 * del;
      sum  := 0.0d0;
      
      FOR j := 1 TO it DO
        sum := sum + f.eval(x);
        x   := x + ddel;
        sum := sum + f.eval(x);
        x   := x + del
      END;
      
      s := (s + (b - a) * sum / tnm) / 3.0d0
    END
  END Midpoint;

PROCEDURE QromoMidpoint(func   : Function.T;
                        a, b   : LONGREAL) : LONGREAL
  RAISES { TooManyIters } =
  BEGIN
    RETURN Qromo(func, a, b, Midpoint)
  END QromoMidpoint;

BEGIN END NR4p4.
