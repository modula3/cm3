(* Copyright 1991 Digital Equipment Corporation.             *)
(* Distributed only by permission.                           *)
(* by Steve Glassman, Mark Manasse and Greg Nelson           *)
(* Last modified on Wed Apr 19 09:50:37 PDT 1995 by najork   *)
(*      modified on Mon Dec 21 13:46:48 PST 1992 by mhb      *)
(*      modified on Fri Apr 24 08:51:31 1992 by msm      *)
(*      modified on Sat Apr  4 19:45:04 PST 1992 by gnelson  *)

<*PRAGMA LL*>

MODULE Test21 EXPORTS Main;

IMPORT VBT, Trestle, TextVBT, Fmt, Region, Path, Rect;

TYPE
  TrackVBT = TextVBT.T OBJECT 
  METHODS
    init(): TrackVBT := Init
  OVERRIDES
    redisplay := Redisplay;
    repaint   := Repaint;
    position  := Position;
    mouse     := Mouse;
  END;
  
PROCEDURE Init(v: TrackVBT): TrackVBT =
  BEGIN
    EVAL TextVBT.T.init(v, "Cursor gone");
    RETURN v
  END Init;

PROCEDURE Out (v: TrackVBT; msg: TEXT) =
  BEGIN
    TextVBT.Put(v, msg);
    (* Wr.PutText(Stdio.stderr, msg & "\n") *)
  END Out;


CONST rect = Rect.T {50,100,50,100};

PROCEDURE DrawRect (v: VBT.Leaf; READONLY badrect: Rect.T) =
  BEGIN
    WITH path = NEW (Path.T) DO
      Path.MoveTo (path, Rect.NorthWest (rect));
      Path.LineTo (path, Rect.NorthEast (rect));
      Path.LineTo (path, Rect.SouthEast (rect));
      Path.LineTo (path, Rect.SouthWest (rect));
      Path.Close (path);
      VBT.Stroke (v, badrect, path);
    END;
  END DrawRect;


PROCEDURE Redisplay (v: VBT.Leaf) =
  BEGIN
    TextVBT.T.redisplay (v);
    DrawRect (v, Rect.Full);
  END Redisplay;


PROCEDURE Repaint (v: VBT.Leaf; READONLY bad: Region.T) =
  BEGIN
    TextVBT.T.repaint (v, bad);
    DrawRect (v, bad.r);
  END Repaint;


PROCEDURE Mouse (v: TrackVBT; READONLY cd: VBT.MouseRec) =
  BEGIN
    IF cd.clickType = VBT.ClickType.FirstDown THEN
      Out(v, "FirstDown")
    ELSIF cd.clickType = VBT.ClickType.LastUp THEN
      Out(v, "LastUp");
      VBT.SetCage(v, VBT.CageFromRect(rect, cd.cp));
    ELSE
      Out(v, "Mouse")
    END;
    TextVBT.T.mouse(v, cd)
  END Mouse;

PROCEDURE Position (v: TrackVBT; READONLY cd: VBT.PositionRec) =
  BEGIN
    IF cd.cp.gone THEN
      Out(v, Fmt.F("GONE ... Cursor (h, v) = (%s, %s)",
                   Fmt.Int(cd.cp.pt.h), Fmt.Int(cd.cp.pt.v)));
    ELSE
      Out(v, Fmt.F("Cursor (h, v) = (%s, %s)",
                   Fmt.Int(cd.cp.pt.h), Fmt.Int(cd.cp.pt.v)));
    END;
    VBT.SetCage(v, VBT.CageFromRect(rect, cd.cp));
  END Position;

VAR v := NEW(TrackVBT).init(); BEGIN
  Trestle.Install(v);
  Trestle.AwaitDelete(v)
END Test21.
