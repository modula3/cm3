MODULE TestTransition;

IMPORT LongRealBasic               AS R,
(*
       LongRealTrans               AS RT,
*)
       LongRealSignal              AS S,
       LongRealVectorFast          AS V,
(*
       LongRealComplexVectorTrans  AS CVT,
       LongRealMatrixFast          AS M,
*)
       LongRealEigenSystem         AS Eigen,

       LongRealRefinableFunc       AS Refn,

(*
       LongRealFmtLex              AS RF,
*)
       LongRealSignalFmtLex        AS SF,
(*
       LongRealVectorFmtLex        AS VF,
       LongRealComplexVectorFmtLex AS CVF,
       LongRealMatrixFmtLex        AS MF,
*)
       PLPlot AS PL,
       (*IO, Fmt, Wr,*) Thread,
       NADefinitions;

PROCEDURE PlotTransitionEV (mask : S.T) =
  <*FATAL NADefinitions.Error*>
  VAR
    ev := Eigen.EigenValuesGen(
            Refn.TransitionMatrix(
              mask.adjoint().convolve(mask)
            )
          );
    x  := NEW(V.T,NUMBER(ev.eigenvalues^));
    y  := NEW(V.T,NUMBER(ev.eigenvalues^));
  BEGIN
    (*IO.Put(CVF.Fmt(ev.eigenvalues));*)
    FOR i:=0 TO LAST(ev.eigenvalues^) DO
      x[i]:=ev.eigenvalues[i].re;
      y[i]:=ev.eigenvalues[i].im;
    END;
    (*IF PL.SetXORMode(TRUE) THEN*)
      PL.SetEnvironment(-10.0D0,10.0D0,-10.0D0,10.0D0);
      PL.PlotPoints(x^,y^,2);
      Thread.Pause(0.1D0);
      PL.PlotPoints(x^,y^,2);
(*
      EVAL PL.SetXORMode(FALSE);
    END;
*)
  END PlotTransitionEV;

PROCEDURE AnimateTransitionEV()=
  CONST
    frames = 20;
  VAR
    mask0 := NEW(S.T).fromArray(ARRAY OF R.T{-2.0D0,0.0D0,1.0D0});
    mask1 := NEW(S.T).fromArray(ARRAY OF R.T{ 2.0D0,0.0D0,1.0D0});
    delta := R.One/FLOAT(frames,R.T);
  BEGIN
    PL.Init();
    (*PL.SetColor0(1);*)
    FOR fr:=0 TO frames DO
      VAR
	t:=FLOAT(fr,R.T)*delta;
      BEGIN
	PlotTransitionEV(mask0.scale(R.One-t).superpose(mask1.scale(t)));
      END;
    END;
    PL.Exit();
  END AnimateTransitionEV;



PROCEDURE CurveTransitionEV(READONLY maskcoef0,maskcoef1:ARRAY OF R.T)=
  <*FATAL NADefinitions.Error*>
  CONST
    frames = 50;
  VAR
    mask0 := NEW(S.T).fromArray(maskcoef0);
    mask1 := NEW(S.T).fromArray(maskcoef1);
    delta := R.One/FLOAT(frames,R.T);
  BEGIN
    PL.SetColor0(1);
    PL.SetEnvironment(-0.2D0,1.2D0,-10.0D0,10.0D0);
    FOR fr:=0 TO frames DO
      VAR
	t:=FLOAT(fr,R.T)*delta;
	mask:=mask0.scale(R.One-t).superpose(mask1.scale(t));
        ev := Eigen.EigenValuesGen(
                Refn.TransitionMatrix(
        	  mask.adjoint().convolve(mask)
                )
              );
        x   := NEW(V.T,NUMBER(ev.eigenvalues^));
        yre := NEW(V.T,NUMBER(ev.eigenvalues^));
        yim := NEW(V.T,NUMBER(ev.eigenvalues^));
      BEGIN
        FOR i:=0 TO LAST(ev.eigenvalues^) DO
          x  [i]:=t;
          yre[i]:=ev.eigenvalues[i].re;
          yim[i]:=ev.eigenvalues[i].im;
        END;
        PL.SetColor0(2);
        PL.PlotPoints(x^,yre^,17);
        PL.SetColor0(3);
        PL.PlotPoints(x^,yim^,20);
      END;
    END;
  END CurveTransitionEV;

PROCEDURE CurveExamples()=
  BEGIN
    PL.Init();
    CurveTransitionEV(
      ARRAY OF R.T{-2.0D0, 1.0D0, 0.0D0, 1.0D0,-2.0D0},
      ARRAY OF R.T{ 2.0D0, 1.0D0, 0.0D0, 1.0D0, 2.0D0}
    );
    CurveTransitionEV(
      ARRAY OF R.T{-1.0D0, 0.0D0, 0.0D0, 1.0D0},
      ARRAY OF R.T{-1.0D0, 0.0D0, 0.0D0, 2.0D0}
    );
    CurveTransitionEV(
      ARRAY OF R.T{-2.0D0, 1.0D0, 1.0D0,-2.0D0},
      ARRAY OF R.T{ 2.0D0, 1.0D0, 1.0D0, 2.0D0}
    );
    CurveTransitionEV(
      ARRAY OF R.T{ 1.0D0, 1.0D0, 1.0D0, 3.0D0},
      ARRAY OF R.T{-1.0D0, 1.0D0, 1.0D0, 3.0D0}
    );
    CurveTransitionEV(
      ARRAY OF R.T{ 1.0D0, 1.0D0, 1.0D0, 3.0D0},
      ARRAY OF R.T{-1.0D0,-1.0D0,-1.0D0, 3.0D0}
    );
    CurveTransitionEV(
      ARRAY OF R.T{-1.0D0,-1.0D0,-1.0D0, 1.0D0},
      ARRAY OF R.T{-1.0D0, 1.0D0, 1.0D0, 1.0D0}
    );
    CurveTransitionEV(
      ARRAY OF R.T{-2.0D0,-1.0D0,1.0D0},
      ARRAY OF R.T{ 2.0D0,-1.0D0,1.0D0}
    );
    CurveTransitionEV(
      ARRAY OF R.T{-2.0D0,-0.5D0,1.0D0},
      ARRAY OF R.T{ 2.0D0,-0.5D0,1.0D0}
    );
    CurveTransitionEV(
      ARRAY OF R.T{-2.0D0,0.0D0,1.0D0},
      ARRAY OF R.T{ 2.0D0,0.0D0,1.0D0}
    );
    CurveTransitionEV(
      ARRAY OF R.T{-2.0D0,0.5D0,1.0D0},
      ARRAY OF R.T{ 2.0D0,0.5D0,1.0D0}
    );
    CurveTransitionEV(
      ARRAY OF R.T{-2.0D0,1.0D0,1.0D0},
      ARRAY OF R.T{ 2.0D0,1.0D0,1.0D0}
    );
    CurveTransitionEV(
      ARRAY OF R.T{ 1.0D0,-2.0D0,1.0D0},
      ARRAY OF R.T{ 1.0D0, 2.0D0,1.0D0}
    );
    CurveTransitionEV(
      ARRAY OF R.T{ 1.0D0, 1.0D0,1.0D0},
      ARRAY OF R.T{-1.0D0,-1.0D0,1.0D0}
    );
    CurveTransitionEV(
      ARRAY OF R.T{-2.0D0, 1.0D0,-2.0D0},
      ARRAY OF R.T{ 2.0D0, 1.0D0, 2.0D0}
    );
    PL.Exit();
  END CurveExamples;



PROCEDURE Test()=
  BEGIN
    CASE 1 OF
    | 0 => AnimateTransitionEV();
    | 1 => CurveExamples();
    END;
  END Test;



BEGIN
END TestTransition.
