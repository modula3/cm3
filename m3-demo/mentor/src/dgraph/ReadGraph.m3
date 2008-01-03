(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

MODULE ReadGraph;

IMPORT Algorithm, AdjMatrix, FileRd, FormsVBT, OSError, Rd, Rsrc,
       Thread, VBT, ZeusPanel;

<* FATAL FormsVBT.Error, FormsVBT.Unimplemented *>

PROCEDURE In (alg: Algorithm.T): AdjMatrix.T
  RAISES {Thread.Alerted} =
  VAR rd: Rd.T; f: TEXT;
  BEGIN
    TRY
      LOCK VBT.mu DO
        IF FormsVBT.GetBoolean(alg.data, "fromSample") THEN
          rd := Rsrc.Open("dgraph.data", ZeusPanel.GetPath());
        ELSE
          f := FormsVBT.GetText(alg.data, "graphdata");
          rd := FileRd.Open(f);
        END
      END;
      RETURN NEW(AdjMatrix.T).initFromRd(rd)
    EXCEPT
    | Rsrc.NotFound =>           
        <* ASSERT FALSE *>
    | OSError.E =>
        ZeusPanel.ReportError("Could not open" & f);
        RETURN NIL;
    END;
  END In;

BEGIN
END ReadGraph.
