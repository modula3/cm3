(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: PSReaderPaneVBT.m3,v 1.1.1.1 2001-09-19 11:40:10 wagner Exp $ *)

MODULE PSReaderPaneVBT;
IMPORT Transform;
IMPORT VBTDrawContext;
IMPORT PaintOp;
IMPORT Point;
IMPORT VBT;
IMPORT Region;
IMPORT Figure;
IMPORT PaneVBT;
IMPORT Rd;
IMPORT DrawContext;
IMPORT PaneManOp;
IMPORT TextSubs;

(*
IMPORT Stroker;
FROM Debug IMPORT S;
IMPORT Fmt;

CONST
  DebugLevel = 0;
*)

REVEAL
  T = Public BRANDED "PSReaderPaneVBT" OBJECT
    m: MUTEX;
    pm: PaneManOp.T;
    fig: Figure.T := NIL;
    subs: TextSubs.T := NIL;
  OVERRIDES
    init := Init;
    clone := Clone;
    paint := Paint;
    setSubs := SetSubs;
  END;

PROCEDURE Init(self: T; rd: Rd.T; pm: PaneManOp.T): PaneVBT.T =
  BEGIN
    self.pm := pm;
    self.m := NEW(MUTEX);
    IF rd = NIL THEN
      IF self.fig = NIL THEN
        self.fig := Figure.FromText("PS file not found.");
      END;
    ELSE
      self.pm.print("reading postscript...");
      self.fig := Figure.FromPS(rd);
      self.pm.print("done.");
    END;
    (* self.center(); *)
    RETURN Public.init(self, rd, pm);
  END Init;

PROCEDURE SetSubs(self: T; subs: TextSubs.T) =
  VAR
    rgn: Region.T;
    transform: Transform.T;
    dc: VBTDrawContext.T;
  BEGIN
    LOCK self.m DO
      (* VBT.ForceRepaint(self, Region.Full); *)
      rgn := Region.FromRect(VBT.Domain(self));
      transform := self.getTransform();
      dc := NEW(VBTDrawContext.T).init(self, PaintOp.Bg);
      dc.setClip(rgn);
      dc.setTransform(transform);

      (* no erase here! (the point of the exercise) *)

      Figure.ToCache(self.fig).diffRecall(dc, Point.Origin, self.subs, subs);
      self.subs := subs;
      dc.setClip(rgn);
      Figure.Plot(self.fig, dc, subs := self.subs); (* just to be safe *)
      VBT.Unmark(self);
      VBT.Sync(self);
    END;
  END SetSubs;

PROCEDURE Clone(self: T): PaneVBT.T =
  VAR
    result: T;
  BEGIN
    LOCK self.m DO
      result := NEW(T, fig := self.fig,
                    subs := self.subs).init(NIL, self.pm);
      result.getTransformFrom(self);
    END;
    RETURN result;
  END Clone;

PROCEDURE Paint(self: T; dc: DrawContext.T) =
  BEGIN
    LOCK self.m DO
      Figure.Plot(self.fig, dc, subs := self.subs);
      (* NEW(Stroker.T).init(dc).frameRect(Figure.BoundRect(self.fig, dc)); *)
    END;
  END Paint;

BEGIN
END PSReaderPaneVBT. 
