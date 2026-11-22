(* $Id: ConjGradient.m3,v 1.5 2002/11/12 02:04:31 kp Exp $ *)

MODULE ConjGradient;
IMPORT Matrix, Compress, Debug;
IMPORT Fmt; FROM Fmt IMPORT F;
IMPORT LRScalarField, LRVectorField;
IMPORT Wx;
IMPORT LRVector;
IMPORT Word;

CONST ItMax = 200;
CONST Eps = 1.0d-10;
CONST LR = Fmt.LongReal;

VAR doDebug := Debug.DebugThis("ConjGradient");

PROCEDURE FmtP(p : Matrix.Vector) : TEXT =
  VAR
    wx := Wx.New();
  BEGIN
    FOR i := FIRST(p^) TO LAST(p^) DO
      Wx.PutText(wx, Fmt.LongReal(p[i]));
      IF i # LAST(p^) THEN Wx.PutChar(wx, ' ') END
    END;
    RETURN Wx.ToText(wx)
  END FmtP;

TYPE
  SWrap = LRScalarField.T OBJECT
    base : LRScalarField.T;
  OVERRIDES
    eval := SWeval;
    evalHint := SWevalHint;
    hash := SWhash;
  END;

  VWrap = LRVectorField.T OBJECT
    base : LRVectorField.T;
  OVERRIDES
    eval := VWeval;
    evalHint := VWevalHint;
    hash := VWhash;
  END;

PROCEDURE SWeval(sw : SWrap; p : LRVector.T) : LONGREAL =
  BEGIN
    WITH res = sw.base.eval(p) DO
      Debug.Out(F("SWeval @ %s -> %s", FmtP(p), LR(res)));
      RETURN res
    END
  END SWeval;

PROCEDURE SWevalHint(sw : SWrap; p : LRVector.T) =
  BEGIN
    sw.base.evalHint(p)
  END SWevalHint;

PROCEDURE SWhash(sw : SWrap) : Word.T =
  BEGIN
    RETURN sw.base.hash()
  END SWhash;
  
PROCEDURE VWeval(sw : VWrap; p : LRVector.T) : LRVector.T =
  BEGIN
    WITH res = sw.base.eval(p) DO
      Debug.Out(F("VWeval @ %s -> %s", FmtP(p), FmtP(res)));
      RETURN res
    END
  END VWeval;

PROCEDURE VWevalHint(sw : VWrap; p : LRVector.T) =
  BEGIN
    sw.base.evalHint(p)
  END VWevalHint;

PROCEDURE VWhash(sw : VWrap) : Word.T =
  BEGIN
    RETURN sw.base.hash()
  END VWhash;

PROCEDURE Minimize(VAR p : Matrix.Vector;
                   ftol  : LONGREAL;
                   func  : LRScalarField.T;
                   dfunc : LRVectorField.T) : LONGREAL
  RAISES { TooManyIterations } =
  VAR 
    fret : LONGREAL;
    g, h, xi := NEW(Matrix.Vector, NUMBER(p^));
    fp, dgg, gg, gam : LONGREAL;
    its := 0;
    numberP := NUMBER(p^)+0; (* without this, CM3 crashes *)
  BEGIN
    IF doDebug THEN
      func := NEW(SWrap, base := func);
      dfunc := NEW(VWrap, base := dfunc)
    END;
    
    fp := func.eval(p);
    xi := dfunc.eval(p);

    IF doDebug THEN
      Debug.Out(Fmt.F("ConjGradient.Minimize : ftol=%s, starting at %s : f=%s grad=%s",
                      Fmt.LongReal(ftol), FmtP(p), Fmt.LongReal(fp), FmtP(xi)))
    END;
    
    FOR j := FIRST(xi^) TO LAST(xi^) DO
      g[j] := -xi[j];
      h[j] := g[j];
      xi[j] := h[j]
    END;

    IF doDebug THEN
      Debug.Out("Conjgradient.Minimize: setup complete ; p=" & FmtP(p))
    END;
    
    LOOP
      IF doDebug THEN
        Debug.Out(F("ConjGradient.Minimize: before LinMin p=%s ; xi=%s",
                    FmtP(p), FmtP(xi)))
      END;

      fret := Compress.LinMin(p,xi,func);

      IF doDebug THEN
        Debug.Out(F("ConjGradient.Minimize: after LinMin p=%s ; xi=%s",
                    FmtP(p), FmtP(xi)))
      END;


      (* quit if we are close enough... *)
      IF 2.0d0 * ABS(fret - fp) <= ftol * (ABS(fret) + ABS(fp) + Eps) THEN
        IF doDebug THEN
          Debug.Out("ConjGradient.Minimize: " & Fmt.Int(its) & " iterations.");
          Debug.Out(F("ConjGradient.Minimize close enough : fp=%s fret=%s p=%s",
                      LR(fp), LR(fret), FmtP(p)));
        END;
        
        RETURN fret
      END;
      
      fp := func.eval(p);
      xi := dfunc.eval(p);
      IF doDebug THEN
        Debug.Out(Fmt.F("Conjgradient.Minimize: p=%s : f=%s grad=%s",
                        FmtP(p), Fmt.LongReal(fp), FmtP(xi)))
      END;

      gg := 0.0d0; dgg := 0.0d0;
      FOR j := FIRST(g^) TO LAST(g^) DO
        gg := gg + g[j] * g[j];
        dgg := dgg + (xi[j]+g[j])*xi[j] (* Polak-Ribiere *)
      END;
      IF gg = 0.0d0 THEN
        IF doDebug THEN
          Debug.Out("Conjgradient.Minimize: " & Fmt.Int(its) & " iterations.")
        END;
        RETURN fret 
      END; (* hmm.. *)

      gam := dgg/gg;
      
      FOR j := FIRST(g^) TO LAST(g^) DO
        g[j] := -xi[j];
        h[j] := g[j] + gam*h[j];
        xi[j] := h[j]
      END;
      INC(its);

      (* how quickly is it supposed to converge? *)
      IF its >= ItMax + numberP DIV 5 THEN EXIT END;
    END;
    Debug.Warning("Too many iterations in ConjGradient.Minimize.\nBest so far = " & Fmt.LongReal(fp));
    RAISE TooManyIterations
  END Minimize;

BEGIN END ConjGradient.
