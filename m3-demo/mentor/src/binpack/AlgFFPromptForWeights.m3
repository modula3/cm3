(* Copyright 1992 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
<* PRAGMA LL *>

MODULE AlgFFPromptForWeights;

IMPORT Algorithm, BinpackAlgClass, BinpackIE, FloatMode, FormsVBT,
       Lex, Scan, Split, Thread, TSplit, VBT, ZeusPanel;

<* FATAL FormsVBT.Error, FormsVBT.Unimplemented *>

CONST
  DataForm = "binpackprompt.fv";
  Opts = "opts";
  Prompt = "ch0";
  GetMaxes = "ch1";
  GetWeight = "ch2";
  GotMaxes = "gotMaxes";
  MaxBins = "B";
  MaxWts = "N";
  Wt = "new";

TYPE
  T = BinpackAlgClass.T BRANDED OBJECT
        B  : INTEGER;           (* number of bins *)
        N  : INTEGER;           (* number of weights *)
        amt: REAL;              (* value of current weight *)
      OVERRIDES
        reactivity := Reactivity;
        run        := Run;
      END;

PROCEDURE New (): Algorithm.T =
  <*FATAL Split.NotAChild*>
  VAR
    fv  := ZeusPanel.NewForm(DataForm);
    alg := NEW(T, data := fv).init();
  BEGIN
    TSplit.SetCurrent(
      FormsVBT.GetVBT(fv, Opts), FormsVBT.GetVBT(fv, Prompt));
    FormsVBT.AttachProc(fv, GotMaxes, GotMaxesProc, alg);
    FormsVBT.AttachProc(fv, Wt, WtProc, alg);
    RETURN alg;
  END New;

PROCEDURE GotMaxesProc (             fv   : FormsVBT.T;
                        <* UNUSED *> event: TEXT;
                                     cl   : REFANY;
                        <* UNUSED *> time : VBT.TimeStamp) =
  <* LL=VBT.mu *>
  <* FATAL Split.NotAChild, Thread.Alerted *>
  BEGIN
    WITH alg = NARROW(cl, T) DO
      alg.N := FormsVBT.GetInteger(fv, MaxWts);
      alg.B := FormsVBT.GetInteger(fv, MaxBins);
      TSplit.SetCurrent(FormsVBT.GetVBT(fv, Opts),
                        FormsVBT.GetVBT(fv, GetWeight));
      ZeusPanel.EndFeedback(alg)
    END
  END GotMaxesProc;


PROCEDURE WtProc (             fv   : FormsVBT.T;
                  <* UNUSED *> event: TEXT;
                               cl   : REFANY;
                  <* UNUSED *> time : VBT.TimeStamp) =
  <* LL=VBT.mu *>
  <* FATAL FloatMode.Trap, Lex.Error, Thread.Alerted *>
  BEGIN
    WITH alg  = NARROW(cl, T),
         text = FormsVBT.GetText(fv, Wt) DO
      alg.amt := Scan.Real(text);
      IF alg.amt >= 0.0 AND alg.amt <= 1.0 THEN
        ZeusPanel.EndFeedback(alg);
        RETURN;
      END;
      FormsVBT.PutText(fv, Wt, "");
    END;
  END WtProc;


PROCEDURE Run (alg: T) RAISES {Thread.Alerted} =
  <*FATAL Split.NotAChild*>
  VAR
    bin: INTEGER;               (* index into array of bins *)
    totals: REF ARRAY OF REAL;  (* b'th bin has totals[b] *)
  BEGIN
    LOCK VBT.mu DO
      TSplit.SetCurrent(
        NARROW(FormsVBT.GetVBT(alg.data, Opts), TSplit.T),
        FormsVBT.GetVBT(alg.data, GetMaxes));
    END;
    ZeusPanel.StartFeedback(alg);
    BinpackIE.Setup(alg, alg.B, alg.N);
    totals := NEW(REF ARRAY OF REAL, alg.B);
    FOR b := 0 TO alg.B - 1 DO totals[b] := 0.0 END;
    FOR w := 0 TO alg.N - 1 DO
      LOCK VBT.mu DO FormsVBT.PutText(alg.data, Wt, "") END;
      ZeusPanel.StartFeedback(alg);
      BinpackIE.NewWeight(alg, w, alg.amt);
      bin := 0;
      BinpackIE.Probe (alg, bin, totals[bin]);
      WHILE (bin < alg.B) AND (totals[bin] + alg.amt > 1.0) DO
        INC(bin);
        IF bin < alg.B THEN BinpackIE.Probe (alg, bin, totals[bin]) END;
      END;
      IF bin = alg.B THEN
        BinpackIE.Ignore(alg);
      ELSE
        totals[bin] := totals[bin] + alg.amt;
        BinpackIE.Pack(alg, bin, totals[bin])
      END
    END
  END Run;


PROCEDURE Reactivity (alg: T; enable: BOOLEAN) =
  <* LL=VBT.mu *>
  BEGIN
    IF enable THEN
      FormsVBT.MakeActive(alg.data, "ch1");
      FormsVBT.MakeActive(alg.data, "ch2")
    ELSE
      FormsVBT.MakePassive(alg.data, "ch1");
      FormsVBT.MakePassive(alg.data, "ch2")
    END
  END Reactivity;

BEGIN
  ZeusPanel.RegisterAlg(New, "First-Fit Prompt-for-weights", "Binpack");
END AlgFFPromptForWeights.

