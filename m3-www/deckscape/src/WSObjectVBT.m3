(* Copyright 1996 Digital Equipment Corporation.              *)
(* Distributed only by permission.                            *)
(*                                                            *)
(* Last modified on Mon Aug 19 22:14:00 PDT 1996 by mhb       *)

MODULE WSObjectVBT;

IMPORT FormsVBT, Rect, Split, VBT, ZSplit;

REVEAL 
  T = Public BRANDED OBJECT
    workspace: VBT.T;
  OVERRIDES
    getWorkspace := GetWorkspace;
    setWorkspace := SetWorkspace;
    mouse := Mouse;
  END;

PROCEDURE Mouse (v: T; READONLY cd: VBT.MouseRec) =
  <* FATAL FormsVBT.Error *>
  BEGIN
    IF cd.whatChanged = VBT.Modifier.MouseR THEN
      IF cd.clickType = VBT.ClickType.FirstDown THEN
        VAR zSplit := FormsVBT.GetVBT(v.getWorkspace(), "zSplit"); BEGIN
          IF FullyVisible (zSplit, v) THEN
            ZSplit.Lift(v, ZSplit.Altitude.Bot);
          ELSE
            ZSplit.Lift(v, ZSplit.Altitude.Top);
          END
        END
      END
    END;
    Public.mouse(v, cd);
  END Mouse;


PROCEDURE FullyVisible (z: ZSplit.T; ch: VBT.T): BOOLEAN =
  <* FATAL Split.NotAChild *>
  VAR
    dom := ZSplit.GetDomain(ch);
  BEGIN
    ch  := Split.Pred(z, ch);
    WHILE ch # NIL DO
      IF ZSplit.IsMapped(ch)
           AND Rect.Overlap(dom, ZSplit.GetDomain(ch)) THEN
        RETURN FALSE
      END;
      ch := Split.Pred(z, ch);
    END;
    RETURN TRUE
  END FullyVisible;

PROCEDURE SetWorkspace (v: T; workspace: VBT.T) =
  BEGIN
    v.workspace := workspace;
  END SetWorkspace;

PROCEDURE GetWorkspace (v: T): VBT.T =
  BEGIN
    RETURN v.workspace
  END GetWorkspace;

BEGIN
END WSObjectVBT.
