(* Copyright 1992 Digital Equipment Corporation.          *)
(* Distributed only by permission.                        *)
(* Last modified on Fri Jul 24 22:29:30 1992 by kalsow    *)

MODULE A_LL;

IMPORT Algorithm, FormsVBT, Thread, ZeusPanel, Rd;
IMPORT Parse, ParseIE, ParseAlgClass;

<*FATAL FormsVBT.Error, FormsVBT.Unimplemented *>

TYPE 
  T = BinpackAlgClass.T BRANDED OBJECT OVERRIDES run := Run END;

PROCEDURE Run (alg: T) RAISES {Thread.Alerted} =
  VAR
    wts    : REF ARRAY OF REAL;
    bins   : REF ARRAY OF REAL;
    n_bins : INTEGER;
    wt     : REAL;    (* current weight to try packing; between [0..1] *)
    bin    : INTEGER; (* index into array of bins *)
    x      : INTEGER;
    map    : REF ARRAY OF INTEGER;
  BEGIN
    BinPack.GetConfig (alg.data, n_bins, wts);
    BinpackIE.Setup (alg, n_bins, wts);

    (* create empty bins *)
    bins := NEW (REF ARRAY OF REAL, n_bins);
    FOR b := 0 TO n_bins - 1 DO bins[b] := 0.0 END;

    (* build the identity map *)
    map := NEW (REF ARRAY OF INTEGER, NUMBER (wts^));
    FOR i := FIRST (map^) TO LAST (map^) DO map[i] := i END;

    (* insertion sort the pieces:  wts[map [i]] > wts [map[j]] <=> i < j *)
    FOR i := 1 TO LAST (wts^) DO
      wt := wts [map [i]];
      x := i - 1;
      WHILE (x >= 0) AND (wt > wts [map [x]]) DO
        map [x+1] := map [x];
        DEC (x);
      END;
      map [x+1] := i;
    END;

    (* pack the bins *)
    FOR i := 0 TO LAST (wts^) DO
      x  := map [i];
      wt := wts [x];

      (* find the first bin that will hold this piece *)
      bin := 0;
      LOOP
        IF (bin >= n_bins) THEN
          BinpackIE.Ignore (alg, x);
          EXIT;
        ELSIF (bins[bin] + wt <= 1.0) THEN
          BinpackIE.Pack (alg, bin, x, bins[bin]);
          bins[bin] := bins[bin] + wt;
          EXIT;
        ELSE
          BinpackIE.Probe (alg, bin, x, bins[bin]);
        END;
        INC (bin);
      END;

    END;
  END Run;

PROCEDURE New (): Algorithm.T =
  <*FATAL Rd.Failure, Thread.Alerted*>
  VAR fv := FormsVBT.NewFromFile("binpackinput.fv");
  BEGIN
    RETURN NEW (T, data := fv).init ()
  END New;

BEGIN
  ZeusPanel.RegisterAlg(New, "First-Fit, decreasing", "Binpack");
END A_LL.

