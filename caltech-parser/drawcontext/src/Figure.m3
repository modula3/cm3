(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: Figure.m3,v 1.2 2001-09-19 15:30:31 wagner Exp $ *)

MODULE Figure;
IMPORT LinoText;
IMPORT VBT;
IMPORT VBTDrawContext;
IMPORT VBTTextBounder;
IMPORT Rect;
IMPORT TextBounder;
IMPORT Rd;
IMPORT PSReader;
IMPORT Axis;
IMPORT DrawContext;
IMPORT DrawContextClass;
IMPORT CacheDrawContext;
IMPORT RegionDrawContext;
IMPORT TextSubs;
IMPORT Point;
IMPORT Region;
IMPORT TransformOther;

TYPE
  AttributeCache = RECORD
    tb: TextBounder.T := NIL;
    region: Region.T;
  END;

  VBTCache = RECORD
    vbt: VBT.T := NIL;
    dc: VBTDrawContext.T;
  END;

REVEAL
  T = ROOT BRANDED "Figure" OBJECT
    data: CacheDrawContext.T;
    ac: AttributeCache;
    vc: VBTCache;
  END;

PROCEDURE ToCache(fig: T; static := TRUE): CacheDrawContext.T =
  BEGIN
    IF static THEN
      RETURN fig.data;
    ELSE
      RETURN FromCache(fig.data).data;
    END;
  END ToCache;

PROCEDURE FromPS(rd: Rd.T; captureResDPI := 720): T =
  BEGIN
    RETURN FromCache(PSReader.New(rd, captureResDPI));
  END FromPS;

PROCEDURE FromCache(cache: CacheDrawContext.T; static := TRUE): T =
  VAR
    result := NEW(T, data := cache);
  BEGIN
    IF NOT static THEN
      result := FromFigure(result);
    END;
    RETURN result;
  END FromCache;

PROCEDURE FromFigure(fig: T; where := Point.Origin;
                     subs: TextSubs.T := NIL): T =
  VAR
    result := NEW(T, data := NEW(CacheDrawContext.T,
                                 resDPI := fig.data.resDPI));
  BEGIN
    Plot(fig, result.data, where, subs);
    RETURN result;
  END FromFigure;

PROCEDURE FromText(text: TEXT := NIL; size := 12): T =
  VAR
    result := NEW(T, data := NEW(CacheDrawContext.T));
  BEGIN
    IF text # NIL THEN
      result.data.text(LinoText.T{Point.Origin, text, size});
    END;
    RETURN result;
  END FromText;

PROCEDURE Plot(fig: T; in: DrawContext.T;
               where := Point.Origin; subs: TextSubs.T := NIL) =
  BEGIN
    fig.data.recall(in, where, subs);
  END Plot;

PROCEDURE Size(fig: T; in: DrawContext.T;
               axis := Axis.T.Hor;
               subs: TextSubs.T := NIL): CARDINAL =
  BEGIN
    RETURN Rect.Size(axis, BoundRect(fig, in, Point.Origin, subs));
  END Size;

PROCEDURE VBTSize(fig: T; vbt: VBT.T;
                  axis := Axis.T.Hor;
                  subs: TextSubs.T := NIL): CARDINAL =
  BEGIN
    IF fig.vc.vbt # vbt THEN
      fig.vc.dc := NEW(VBTDrawContext.T,
                        textBounder := NEW(VBTTextBounder.T).init(vbt));
      fig.vc.vbt := vbt;
    END;
    RETURN Size(fig, fig.vc.dc, axis, subs);
  END VBTSize;

PROCEDURE BoundRect(fig: T; in: DrawContext.T;
                    where := Point.Origin;
                    subs: TextSubs.T := NIL): Rect.T =
  BEGIN
    RETURN BoundRegion(fig, in, where, subs).r;
  END BoundRect;

PROCEDURE BoundRegion(fig: T; in: DrawContext.T;
                      where := Point.Origin;
                      subs: TextSubs.T := NIL): Region.T =
  BEGIN
    IF subs # NIL OR fig.ac.tb # in.textBounder THEN
      VAR
        region := NEW(RegionDrawContext.T).init(in.textBounder);
        inv := TransformOther.Inverse(in.transform);
      BEGIN
        region.setTransform(in.transform);
        fig.data.recall(region, where, subs);
        fig.ac.tb := in.textBounder;
        fig.ac.region := region.toRegion();
        fig.ac.region := TransformOther.ApplyToRegion(inv, fig.ac.region);
      END;
    END;
    RETURN fig.ac.region;
  END BoundRegion;

BEGIN
END Figure.
