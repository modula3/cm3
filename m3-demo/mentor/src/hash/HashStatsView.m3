(* Copyright 1992 Digital Equipment Corporation.           *)
(* Distributed only by permission.                         *)
(*      modified on Wed Jul 22 00:37:01 1992 by mhb        *)

MODULE HashStatsView;

IMPORT FormsVBT, HashViewClass, VBT, View, Rd, Rsrc, Thread, ZeusPanel;

<* FATAL FormsVBT.Error, FormsVBT.Unimplemented *>


TYPE
  T = HashViewClass.T BRANDED OBJECT
        fv: FormsVBT.T;
        operations: CARDINAL;
        runtime: CARDINAL;
      OVERRIDES
        oeSetup := Setup;
        oeInsert := Operation;
        oeDelete := Operation;
        oeFind := Operation;
        oeCompare := Run1;
        oeAddToBucket := AddToBucket;
        oeDeleteFromBucket := DeleteFromBucket;
        oeCheckDeletable := Run1;
        oeCheckHashPosition := Run1;
      END;

PROCEDURE Setup (             view:     T; 
                 <* UNUSED *> data:     FormsVBT.T; 
                 <* UNUSED *> nBuckets: INTEGER) =
  BEGIN
    view.operations := 0;
    view.runtime := 0;
    LOCK VBT.mu DO
      FormsVBT.PutInteger(view.fv, "Operations", 0);
      FormsVBT.PutInteger(view.fv, "Runtime", 0);
    END;
  END Setup;

PROCEDURE Operation (view: T; <* UNUSED *> item: TEXT) =
  BEGIN
    LOCK VBT.mu DO
      INC(view.operations);
      FormsVBT.PutInteger(view.fv, "Operations", view.operations);
    END;
  END Operation;

PROCEDURE Run1 (view: T; <* UNUSED *> bucket: INTEGER) =
  BEGIN
    LOCK VBT.mu DO
      INC(view.runtime);
      FormsVBT.PutInteger(view.fv, "Runtime", view.runtime);
    END;
  END Run1;


PROCEDURE AddToBucket (             view:   T; 
                       <* UNUSED *> key:    TEXT; 
                       <* UNUSED *> bucket: INTEGER) =
  BEGIN
    LOCK VBT.mu DO
      INC(view.runtime);
      FormsVBT.PutInteger(view.fv, "Runtime", view.runtime);
    END;
  END AddToBucket;

PROCEDURE DeleteFromBucket (             view     : T;
                            <* UNUSED *> key      : TEXT;
                            <* UNUSED *> bucket   : INTEGER;
                            <* UNUSED *> markEmpty: BOOLEAN  ) =
  BEGIN
    LOCK VBT.mu DO
      INC(view.runtime);
      FormsVBT.PutInteger(view.fv, "Runtime", view.runtime);
    END;
  END DeleteFromBucket;

PROCEDURE New (): View.T =
  <* FATAL Rd.Failure, Rsrc.NotFound, Thread.Alerted *>
  BEGIN
    WITH view = NEW(T, fv := NEW(FormsVBT.T).initFromRsrc(
                               "hashstats.fv", ZeusPanel.GetPath())) DO
      EVAL view.init(view.fv);
      RETURN view;
    END;
  END New;

BEGIN
  ZeusPanel.RegisterView (New, "Stats", "Hash");
END HashStatsView.
