(* Copyright 1992 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* Last modified on Fri Jan  6 00:40:46 PST 1995 by najork         *)
(*      modified on Thu Sep 24 13:16:42 PDT 1992 by mhb            *)

MODULE AlgFn;

IMPORT Algorithm, FormsVBT, ZPaperAlgClass, ZPaperIE, Thread, VBT, ZeusPanel;

TYPE T = ZPaperAlgClass.T BRANDED OBJECT
  OVERRIDES
    run := Run;
  END;

PROCEDURE Run (alg: T) RAISES {Thread.Alerted} =
  <* FATAL FormsVBT.Error, FormsVBT.Unimplemented *>
  VAR N, k: INTEGER; fv: FormsVBT.T;
  BEGIN
    fv := alg.data;
    LOCK VBT.mu DO
      N := FormsVBT.GetInteger(fv, "N");
      k := FormsVBT.GetInteger(fv, "k");
    END;
    ZPaperIE.Init(alg, N, k);
    FOR y := 0 TO N - 1 DO
      FOR x := 0 TO N - 1 DO
        ZPaperIE.Set(alg, x, y, (x * x + y * y) MOD k)
      END
    END
  END Run;

PROCEDURE FnNew (): Algorithm.T =
  BEGIN
    RETURN NEW(T, data := ZeusPanel.NewForm ("zdata.fv")).init()
  END FnNew;

BEGIN
  ZeusPanel.RegisterAlg(FnNew, "(x*x + y*y) MOD k", "ZPaper");
END AlgFn.

