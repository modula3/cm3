(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Sat Feb  6 18:05:14 PST 1993 by meehan     *)
(*      modified on Mon Feb  1 13:24:01 PST 1993 by mhb        *)
(*      modified on Tue Jun 16 13:08:12 PDT 1992 by muller     *)
(*      modified on Mon Jun 18 16:52:39 PDT 1990 by birrell    *)

MODULE SplitterVBT;

IMPORT Axis, HVBar, HVSplit, MultiClass, MultiSplit, PaintOp, Pixmap,
         Split, VBT, VBTKitResources;

TYPE
  MC = MultiClass.Split BRANDED OBJECT
       OVERRIDES
         insert  := Insert;
         replace := Replace;
       END;

REVEAL
  T = Public BRANDED OBJECT
        size: REAL;
        op:   PaintOp.T;
        txt:  Pixmap.T;
      OVERRIDES
        init := Init
      END;

VAR
  barPixmap: Pixmap.T;

PROCEDURE Init (v       : T;
                hv      : Axis.T;
                size               := DefaultSize;
                op                 := PaintOp.BgFg;
                txt                := Pixmap.Gray;
                saveBits           := FALSE;
                parlim             := -1            ): T =
  BEGIN
    GetResources();
    EVAL HVSplit.T.init(v, hv, saveBits, parlim, TRUE);
    v.size := size;
    v.op := op;
    v.txt := txt;
    MultiClass.Be(v, NEW(MC));
    RETURN v
  END Init;

PROCEDURE Insert (m: MC; pred, ch: VBT.T) = 
  <* FATAL MultiSplit.NotAChild *>
  <* FATAL Split.NotAChild *>
  VAR
    neighbor: VBT.T;
    v       : T     := m.vbt;
  BEGIN
    IF pred = NIL THEN
      neighbor := MultiSplit.Succ (v, NIL)
    ELSE
      neighbor := pred
    END;
    IF neighbor = NIL THEN
      (* new child is only child *)
      Split.Insert (v, NIL, ch);
    ELSE
      Split.Insert (v, pred, ch);
      IF pred = NIL THEN
        Split.Insert (v, ch, NewBar (v));
      ELSE
        Split.Insert (v, pred, NewBar (v))
      END
    END
  END Insert;

PROCEDURE Replace (m: MC; ch, new: VBT.T) =
  <* FATAL Split.NotAChild *>
  VAR hvBar: VBT.T;
  BEGIN
    WITH v = NARROW (m.vbt, T) DO
      IF new # NIL THEN         (* a true replace of a child *)
        Split.Replace (v, ch, new);
      ELSE                      (* a deletion *)
        hvBar := Split.Succ (v, ch);
        IF hvBar = NIL THEN hvBar := Split.Pred (v, ch) END;
        Split.Delete (v, ch);
        IF hvBar # NIL THEN Split.Delete (v, hvBar) END;
      END
    END
  END Replace;


(*

PROCEDURE InsertT (m: MCTargets; pred, ch: VBT.T) =
  <* FATAL Split.NotAChild *>
  VAR
    (* tc := SourceVBT.TargetClassOf (ch); *)
    v    : T := m.vbt;
    hvBar    := NewBar(v);
  BEGIN
    (*
      SourceVBT.BeTarget (hvBar, SourceVBT.NewInserterTarget());
    *)
    Split.Insert(v, pred, hvBar);
    (*
      IF tc = NIL THEN tc := SourceVBT.NewSwapTarget() END;
      SourceVBT.BeTarget(ch, tc);
    *)
    Split.Insert(v, hvBar, ch);
  END InsertT;

PROCEDURE ReplaceT (m: MCTargets; ch, new: VBT.T) =
  <* FATAL Split.NotAChild *>
  VAR
    (* tc := SourceVBT.TargetClassOf (ch); *)
    v: T := m.vbt;
  BEGIN
    IF new # NIL THEN            (* a true replace of a child *)
      (*
      IF tc = NIL THEN tc := SourceVBT.NewSwapTarget() END;
      SourceVBT.BeTarget(new, tc);
      *)
      Split.Replace(v, ch, new);
    ELSE                         (* a deletion *)
      Split.Delete(v, Split.Succ(v, ch));
      Split.Delete(v, ch);
    END
  END ReplaceT;
        
*)

PROCEDURE NewBar (v: T): HVBar.T =
  BEGIN
    RETURN HVBar.New(v.size, v.op, v.txt)
  END NewBar;

VAR
  rsrcMu := NEW (MUTEX);
  rsrcInit := FALSE;

PROCEDURE GetResources () =
  BEGIN
    LOCK rsrcMu DO
      IF rsrcInit THEN RETURN END;
      barPixmap := VBTKitResources.GetPixmap ("NWDiagonal");
      rsrcInit := TRUE;
    END
  END GetResources;

BEGIN
END SplitterVBT.
