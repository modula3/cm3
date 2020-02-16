(* $Id$ *)

MODULE MagicStuff;
FROM EndPointStatus IMPORT Dir;
IMPORT MagPointList, GridPoint, RectSet;
IMPORT MagRect, MagLayerRect AS LayerRect, MagPoint, Conf;
FROM SimpleGrid IMPORT PointType;
IMPORT MagRectList, MagRouteLayer AS RouteLayer, SimpleGrid;
IMPORT MagCell, MagCellExtendable;
IMPORT MagLabel;

(***************************************************************************)
(***************************************************************************)
(***************************************************************************)
(*************************                     *****************************)
(*************************  LAYOUT GENERATION  *****************************)
(*************************                     *****************************)
(***************************************************************************)
(***************************************************************************)
(***************************************************************************)

PROCEDURE DrawATarget(c : MagCell.Labelled;
                      at : GridPoint.T;
                      labelled : TEXT ) : MagLabel.T =
  <* FATAL Conf.LayerNotFound *>
  VAR
    layer : RouteLayer.T := Conf.LayerLookup(at.l);
    xbase := at.x * GridStep();
    ybase := at.y * GridStep();
    rect := MagRect.T { MagPoint.T { xbase, ybase },
                        MagPoint.T { xbase + ViaSize(), ybase + ViaSize() } };
    labPt := MagPoint.T { xbase + ViaSize() DIV 2, ybase + ViaSize() DIV 2 };
    label := MagLabel.T {
                 MagRect.T { labPt, labPt },
                 layer.name,
                 FIRST(MagLabel.Position),
                 labelled };
  BEGIN
    c.addRect(rect, layer);
    c.addLabel(label);
    RETURN label
  END DrawATarget;

(* establish a square of metal at ll of GridPoint, connected to RectSet *)
(* Hmm.. this isn't quite right.. *)
PROCEDURE ConnectTo(entry : MagPointList.T; 
                    READONLY p : GridPoint.T;
                    dir : Dir) : RectSet.T =
  <* FATAL Conf.LayerNotFound *>
  CONST
    Tgt = PointType.Target;
    N = SimpleGrid.Dir.N;
    W = SimpleGrid.Dir.W;

    (* remember that the wires can go a bit outside... only the actual
       routed path needs to stay inside *)
    
  VAR
    GridClip := MagRect.T { MagPoint.T { -GridStep() - WireWidth() + 1 , -GridStep() - WireWidth() + 1 } , 
                           MagPoint.T { 2*GridStep() + WireWidth() - 1, 
                                        2*GridStep() + WireWidth() - 1 } };
    s := NEW(RectSet.T).init();
    g := NEW(SimpleGrid.ArrImpl).init(GridClip);
    l : MagRectList.T;
    layer := Conf.LayerLookup(p.l);
    xbase := p.x * GridStep();
    ybase := p.y * GridStep();
    shift := MagPoint.T { -xbase, -ybase };
  BEGIN
    g.drawPointList(Tgt, entry);
    
    (* bloat entry *)
    g.bloat(Tgt, N, WireWidth() - 1);
    g.bloat(Tgt, W, WireWidth() - 1);
    
    l := g.rectify(Tgt);

    WHILE l # NIL DO
      EVAL s.insert(LayerRect.T { MagRect.Shift(l.head, shift) , layer });
      l := l.tail
    END;

    (* add up and down vias, if applicable *)
    IF dir = Dir.U OR dir = Dir.D THEN
      VAR 
        otherL : RouteLayer.T;
      BEGIN
        IF dir = Dir.U THEN 
          <* ASSERT p.l # LAST(GridPoint.Layer) *>
          otherL := Conf.LayerLookup(p.l + 1)
        ELSE
          <* ASSERT p.l # FIRST(GridPoint.Layer) *>
          otherL := Conf.LayerLookup(p.l - 1)
        END;

        s := s.union(ViaAt(MagPoint.T { xbase, ybase }, layer, otherL))
      END
    END;

    (* if entered from south or west, then other special cases apply.. sigh *)
    (* we need a rect to connect to the layout in the previous GridPoint. *)
    IF dir = Dir.S THEN
      VAR 
        endRect := MagRect.T { MagPoint.T { 0, -GridStep() + WireWidth() },
                               MagPoint.T { WireWidth(), 0 } };
      BEGIN
        EVAL s.insert(LayerRect.T { MagRect.Shift(endRect,shift), layer })
      END
    ELSIF dir = Dir.W THEN
      VAR 
        endRect := MagRect.T { MagPoint.T { -GridStep() + WireWidth(), 0 },
                               MagPoint.T { 0, WireWidth() } };
      BEGIN
        EVAL s.insert(LayerRect.T { MagRect.Shift(endRect,shift), layer })
      END
    END;

    RETURN s
  END ConnectTo;

(* get the corner coordinates of rects, considering the wire width *)
PROCEDURE Lo(a : INTEGER) : INTEGER = 
  BEGIN RETURN a * GridStep() END Lo;

PROCEDURE Hi(a : INTEGER) : INTEGER = 
  BEGIN RETURN a * GridStep() + WireWidth() END Hi;

(* ViaAt returns the set of rects necessary to put a via with its
   lower left corner at ll and between layers l1 and l2 *)
PROCEDURE ViaAt(READONLY ll : MagPoint.T; l1, l2 : RouteLayer.T) : RectSet.T =
  VAR
    (* this stuff deals with the gv's *)
    gvRect := MagRect.T { MagPoint.T { ll.x + ViaOverlap(), ll.y + ViaOverlap() } , 
                         MagPoint.T { ll.x + ViaSize() - ViaOverlap() - ViaNEOverlap(), 
                                      ll.y + ViaSize() - ViaOverlap() - ViaNEOverlap()} };

    (* overlapping metal *)
    viaRect := MagRect.T { MagPoint.T { ll.x , ll.y } , 
                             MagPoint.T { ll.x + ViaSize() ,  ll.y + ViaSize() } };
    set := NEW(RectSet.T).init();
  BEGIN
    EVAL set.insert(LayerRect.T { gvRect, Conf.ViaLookup(l1,l2) });
    EVAL set.insert(LayerRect.T { viaRect, l1 });
    EVAL set.insert(LayerRect.T { viaRect, l2 });
    RETURN set
  END ViaAt;

PROCEDURE EndPadRects(READONLY a : GridPoint.T) : RectSet.T =
  <* FATAL Conf.LayerNotFound *>
  VAR
    set := NEW(RectSet.T).init();
    res : LayerRect.T;
  BEGIN
    res.rect.ll := MagPoint.T { Lo(a.x), Lo(a.y) };
    res.rect.ur := MagPoint.T { Lo(a.x) + ViaSize(), Lo(a.y) + ViaSize() };
    res.layer := Conf.LayerLookup(a.l);
    EVAL set.insert(res);
    RETURN set
  END EndPadRects;

PROCEDURE ConnRects(READONLY a, b : GridPoint.T) : RectSet.T =
  <* FATAL Conf.LayerNotFound *>
  VAR
    set := NEW(RectSet.T).init();
    res : LayerRect.T;
    from, to : INTEGER;
  BEGIN
    IF    a.x # b.x THEN
      <* ASSERT a.y = b.y AND a.l = b.l *>
      from := Lo(MIN(a.x, b.x));
      to   := Hi(MAX(a.x, b.x));
      res.rect.ll := MagPoint.T { from, Lo(a.y) };
      res.rect.ur := MagPoint.T { to  , Hi(a.y) };
      res.layer := Conf.LayerLookup(a.l);
      EVAL set.insert(res)
    ELSIF a.y # b.y THEN
      <* ASSERT a.x = b.x AND a.l = b.l *>
      from := Lo(MIN(a.y, b.y));
      to   := Hi(MAX(a.y, b.y));
      res.rect.ll := MagPoint.T { Lo(a.x), from  };
      res.rect.ur := MagPoint.T { Hi(a.x), to    };
      res.layer := Conf.LayerLookup(a.l);
      EVAL set.insert(res)
    ELSIF a.l # b.l THEN
      <* ASSERT a.x = b.x AND a.y = b.y *>
      from := MIN(a.l, b.l);
      to   := MAX(a.l, b.l);
      set := ViaAt(MagPoint.T { Lo(a.x), Lo(a.y) }, 
                   Conf.LayerLookup(a.l), 
                   Conf.LayerLookup(b.l) );
    ELSE
      <* ASSERT FALSE *>
    END;
    RETURN set
  END ConnRects;

PROCEDURE WireWidth() : CARDINAL = BEGIN RETURN 3 END WireWidth;
PROCEDURE ViaSize() : CARDINAL = BEGIN RETURN 4 + viaNEOverlap END ViaSize;
PROCEDURE ViaOverlap() : CARDINAL = BEGIN RETURN 1 END ViaOverlap;
PROCEDURE ViaNEOverlap() : CARDINAL = BEGIN RETURN viaNEOverlap END ViaNEOverlap;
PROCEDURE MetalSpacing() : CARDINAL = BEGIN RETURN 3 END MetalSpacing;
PROCEDURE GridStep() : CARDINAL = 
  BEGIN RETURN MAX(WireWidth(), ViaSize()) + MetalSpacing() END GridStep;

PROCEDURE SetViaNEOverlap(to : CARDINAL) = BEGIN viaNEOverlap := to END SetViaNEOverlap;

VAR viaNEOverlap := 1;

BEGIN END MagicStuff.
