(* Copyright 1992 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Last modified on Fri Jul  9 01:18:49 PDT 1993 by mhb      *)
(*      modified on Mon Jul 27  2:44:37 PDT 1992 by sclafani *)

MODULE AlgFFCodeView;

IMPORT Algorithm, BinpackAlgClass, BinpackIE, FormsVBT,
       Random, RefList, Thread, VBT, ZeusCodeView, ZeusPanel;

<* FATAL FormsVBT.Error, FormsVBT.Unimplemented *>

TYPE 
  T = BinpackAlgClass.T BRANDED OBJECT 
    OVERRIDES 
      run := Run; 
    END;

TYPE
  Bins = REF ARRAY OF REAL;

PROCEDURE Run (alg: T) RAISES {Thread.Alerted} =
  PROCEDURE At (line: INTEGER) RAISES {Thread.Alerted} =
    BEGIN ZeusCodeView.Event(alg, line); END At;
  VAR
    B: INTEGER <* TRACE alg.varView.setIntegerL *>;
        (* number of bins *)
    N: INTEGER <* TRACE alg.varView.setIntegerL *>;
        (* number of weights *)
    wt: REAL <* TRACE alg.varView.setReal *>;
        (* current to try packing; between [0..1] *)
    bin: INTEGER <* TRACE alg.varView.setInteger *>;
        (* index into array of bins *)
    bins: Bins;  (* array of bins *)
    lost: REAL := 0.0 <* TRACE alg.varView.setReal *>;
        (* sum of weights that wouldn't fit *)
    rand := NEW(Random.Default).init();
  BEGIN
            ZeusCodeView.Event(alg, procedureName := "FirstFit");
            LOCK VBT.mu DO
              N := FormsVBT.GetInteger(alg.data, "N");
              B := FormsVBT.GetInteger(alg.data, "B");
            END;
            BinpackIE.Setup(alg, B, N);
At(1);      bins := NEW(Bins, B);
At(2);      FOR b := 0 TO B-1 DO bins[b] := 0.0 END;
At(3);      FOR i := 1 TO N DO 
At(4);        wt := rand.real()/2.0;
              BinpackIE.NewWeight (alg, i-1, wt);
At(5);        bin := 0; 
              BinpackIE.Probe(alg, bin, bins[bin]);
              WHILE (bin < B) AND (bins[bin] + wt > 1.0) DO
At(6);
At(7);          INC(bin);
                IF bin < B THEN BinpackIE.Probe(alg, bin, bins[bin]) END;
              END;
At(8);        IF bin = B THEN 
At(9);          lost := lost + wt;
                BinpackIE.Ignore(alg);
              ELSE  
At(10);         bins[bin] := bins[bin] + wt;
                BinpackIE.Pack(alg, bin, bins[bin]) 
              END
            END
  END Run;

PROCEDURE New (): Algorithm.T =
  VAR fv := ZeusPanel.NewForm("binpackinput.fv");
  BEGIN
    RETURN
      NEW(
        T, data := fv, varRsrc := "binpackFFvar.fv",
        codeViews :=
          RefList.List1(RefList.List2("C Code View", "alg.c"))).init()
  END New;

BEGIN
  ZeusPanel.RegisterAlg(New, "First-Fit CodeView", "Binpack");
END AlgFFCodeView.
