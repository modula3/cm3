MODULE TestTransition;

IMPORT LongRealBasic               AS R,
       LongRealFmtLex              AS RF,
       LongRealTrans               AS RT,
       RandomDECSRC                AS Rnd,
       LongRealSignal              AS S,
       LongRealVectorFast          AS V,
(*
       LongRealVectorTrans         AS VT,
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
       IO, Fmt, Wr, Thread,
       NADefinitions AS NA;

CONST
  AThird = 1.0D0/3.0D0;

PROCEDURE PlotTransitionEV (mask : S.T) =
  <*FATAL NA.Error*>
  VAR
    ev := Refn.TransitionEV(mask);
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
    (*PL.SetFGColorDiscr(1);*)
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
  <*FATAL NA.Error*>
  CONST
    frames = 50;
  VAR
    mask0 := NEW(S.T).fromArray(maskcoef0);
    mask1 := NEW(S.T).fromArray(maskcoef1);
    delta := R.One/FLOAT(frames,R.T);
  BEGIN
    PL.SetFGColorDiscr(1);
    PL.SetEnvironment(-0.2D0,1.2D0,-10.0D0,10.0D0);
    FOR fr:=0 TO frames DO
      VAR
        t:=FLOAT(fr,R.T)*delta;
        mask:=mask0.scale(R.One-t).superpose(mask1.scale(t));
        ev := Eigen.EigenValues(
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
        PL.SetFGColorDiscr(2);
        PL.PlotPoints(x^,yre^,17);
        PL.SetFGColorDiscr(3);
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
    (* This example shows that the estimation can be arbitrarily bad. *)
    CurveTransitionEV(
      ARRAY OF R.T{ 0.3333D0, 0.3333D0, 0.3333D0, 0.0D0},
      ARRAY OF R.T{ 2.3333D0, 0.3333D0, 0.3333D0,-2.0D0}
    );
    CurveTransitionEV(
      ARRAY OF R.T{ 0.5D0, 0.0D0, 0.0D0, 0.5D0},
      ARRAY OF R.T{-3.0D0, 0.0D0, 0.0D0, 4.0D0}
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

PROCEDURE EstimateSpecRad(mask:S.T):R.T=
  VAR
    y:=mask.wrapCyclic(3).getData();
    p1,p2:=R.Zero;
  BEGIN
    FOR j:=0 TO 2 DO
      p1:=p1+y[j];
      p2:=p2+y[j]*y[j];
    END;
    VAR
      p12:=p1*p1;
      dif:=p2-p12/3.0D0;
    BEGIN
      RETURN RT.SqRt(1.5D0*dif*dif+p12*p12/3.0D0);
    END;
  END EstimateSpecRad;

PROCEDURE CompareEstimate(mask:S.T)=
  <*FATAL NA.Error, Thread.Alerted, Wr.Failure *>
  BEGIN
    IO.Put(Fmt.FN("%s: %s < %s ?\n",ARRAY OF TEXT
      {SF.Fmt(mask),
       RF.Fmt(Refn.TransitionSpecRad(mask)),
       RF.Fmt(EstimateSpecRad(mask))
      }));
  END CompareEstimate;

PROCEDURE RandomMaskWithLeastEstimate():S.T=
  <*FATAL NA.Error *>
  VAR
    rnd    := NEW(Rnd.T).init();
    rndArr : ARRAY [0..4] OF R.T;
  BEGIN
    FOR j:=FIRST(rndArr) TO LAST(rndArr) DO
      rndArr[j]:=rnd.uniform(-1.0D0,1.0D0);
    END;
    VAR
      slice:=NEW(S.T).fromArray(rndArr).slice(3);
    BEGIN
      FOR j:=FIRST(slice^) TO LAST(slice^) DO
        slice[j] := slice[j].raise((AThird-slice[j].sum())/FLOAT(slice[j].getNumber(),R.T),
	                           slice[j].getFirst(),slice[j].getNumber());
      END;
      RETURN NEW(S.T).interleave(slice^);
    END;
  END RandomMaskWithLeastEstimate;

PROCEDURE RandomMask(size:CARDINAL):S.T=
  <*FATAL NA.Error *>
  VAR
    rnd    := NEW(Rnd.T).init();
    rndArr := NEW(REF ARRAY OF R.T, size);
    mask   := NEW(S.T);
  BEGIN
    FOR j:=FIRST(rndArr^) TO LAST(rndArr^) DO
      rndArr[j]:=rnd.uniform(-1.0D0,1.0D0);
    END;
    mask := mask.fromArray(rndArr^);
    RETURN mask.raise((1.0D0-mask.sum())/FLOAT(size,R.T),mask.getFirst(),mask.getNumber());
  END RandomMask;

PROCEDURE CompareTranslatedMasks(mask0,mask1:S.T) =
  <*FATAL NA.Error *>
  BEGIN
    IO.Put(Fmt.FN("estimate: %s\n",ARRAY OF TEXT
      {RF.Fmt(EstimateSpecRad(mask0.superpose(mask1)))
      }));

    FOR n:=-10 TO 10 DO
      IO.Put(Fmt.FN("%s, ",ARRAY OF TEXT
        {RF.Fmt(Refn.TransitionSpecRad(mask1.translate(3*n).superpose(mask0)))
        }));
    END;
    IO.Put("\n");
  END CompareTranslatedMasks;

PROCEDURE EstimateChecks()=
  BEGIN
    CASE 2 OF
    | 0 =>
      FOR n:=0 TO 10 DO
        CompareEstimate(RandomMaskWithLeastEstimate());
      END;
    | 1 =>
      (* This example shows that the estimation can be arbitrarily bad. *)
      VAR
        maskArr := ARRAY [0..3] OF R.T{0.0D0,AThird,AThird,0.0D0};
      BEGIN
        FOR n:=0 TO 10 DO
          maskArr[0]:=FLOAT(n,R.T);
          maskArr[3]:=AThird-maskArr[0];
          CompareEstimate(NEW(S.T).fromArray(maskArr));
        END;
      END;
    | 2 =>
      (* Check the same coefficients in different positions.
         The estimation will remain constant,
         what do the real eigenvalues? *)
      VAR
        mask0Arr := ARRAY [0..1] OF R.T{AThird,AThird};
        mask1Arr := ARRAY [0..0] OF R.T{AThird};
      BEGIN
        CompareTranslatedMasks(
          NEW(S.T).fromArray(mask0Arr,0),
          NEW(S.T).fromArray(mask1Arr,2)
        );
      END;
      VAR
        mask0Arr := ARRAY [0..2] OF R.T{AThird,R.Zero,AThird};
        mask1Arr := ARRAY [0..0] OF R.T{AThird};
      BEGIN
        CompareTranslatedMasks(
          NEW(S.T).fromArray(mask0Arr,0),
          NEW(S.T).fromArray(mask1Arr,2)
        );
      END;
      FOR n:=0 TO 9 DO
        VAR
          mask  := RandomMask(4);
          mask0 := NEW(S.T).fromArray(SUBARRAY(mask.getData()^,0,1));
          mask1 := NEW(S.T).fromArray(SUBARRAY(mask.getData()^,1,mask.getNumber()-1));
        BEGIN
          CompareTranslatedMasks(mask0,mask1);
        END;
      END;
    ELSE <*ASSERT FALSE*>
    END;
  END EstimateChecks;


PROCEDURE Test()=
  BEGIN
    CASE 2 OF
    | 0 => AnimateTransitionEV();
    | 1 => CurveExamples();
    | 2 => EstimateChecks();
    ELSE <*ASSERT FALSE*>
    END;
  END Test;



BEGIN
END TestTransition.
