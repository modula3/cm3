(* Copyright 1992 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
<* PRAGMA LL *>

MODULE AlgFFSimple;

IMPORT Algorithm, BinpackAlgClass, BinpackIE, FormsVBT, Random, Thread,
       VBT, ZeusPanel;

<* FATAL FormsVBT.Error, FormsVBT.Unimplemented *>

REVEAL
  T = BinpackAlgClass.T BRANDED OBJECT
      OVERRIDES
        run := Run;
      END;

PROCEDURE New (): Algorithm.T =
  VAR fv := ZeusPanel.NewForm("binpackinput.fv");
  BEGIN
    RETURN NEW(T, data := fv).init()
  END New;

PROCEDURE Run (alg: T) RAISES {Thread.Alerted} =
  VAR
    B  : INTEGER;               (* number of bins *)
    N  : INTEGER;               (* number of weights *)
    bin: INTEGER;               (* index into array of bins *)
    amt: REAL;                  (* current weight *)
    totals: REF ARRAY OF REAL;  (* b'th bin has totals[b] *)
    rand := NEW(Random.Default).init();
  BEGIN
    LOCK VBT.mu DO
      N := FormsVBT.GetInteger(alg.data, "N");
      B := FormsVBT.GetInteger(alg.data, "B");
    END;
    BinpackIE.Setup(alg, B, N);
    totals := NEW(REF ARRAY OF REAL, B);
    FOR b := 0 TO B - 1 DO totals[b] := 0.0 END;
    FOR w := 0 TO N - 1 DO
      amt := rand.real()/2.0;
      BinpackIE.NewWeight(alg, w, amt);
      bin := 0;
      WHILE (bin < B) AND (totals[bin] + amt > 1.0) DO
        INC(bin)
      END;
      IF bin = B THEN
        BinpackIE.Ignore(alg);
      ELSE
        totals[bin] := totals[bin] + amt;
        BinpackIE.Pack(alg, bin, totals[bin])
      END
    END
  END Run;

BEGIN
  ZeusPanel.RegisterAlg(New, "First-Fit Simple", "Binpack");
END AlgFFSimple.
