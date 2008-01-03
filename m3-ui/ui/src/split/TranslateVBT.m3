(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman, Mark Manasse and Greg Nelson           *)
(* Last modified on Thu Feb 10 17:57:54 PST 1994 by msm      *)
(*      modified on Thu May  7 11:45:44 PDT 1992 by harrison *)
(*      modified on Tue Mar 10 19:06:09 1992 by steveg   *)
(*      modified on Mon Feb 24 13:55:18 PST 1992 by muller   *)
(*      modified on Sun Nov 10 19:22:25 PST 1991 by gnelson  *)
<*PRAGMA LL*>

MODULE TranslateVBT;

IMPORT Filter, FilterClass, Point, VBTClass, VBT, Rect, 
  Region, Trestle, Batch, BatchUtil, ScrnPixmap, MouseSplit;

REVEAL 
  T = Filter.T BRANDED OBJECT 
    delta: Point.T;
    (* child coord + delta = parent coord. *)
    (* v.delta is protected both by v and by VBT.mu.
       (If v is disconnected, these locks are not necessary). *)
    screen: VBT.ScreenID := -1
  OVERRIDES
    (* VBT down methods *)
    reshape := Reshape;
    repaint := Repaint;
    rescreen := Rescreen;
    mouse := Mouse;
    position := Position;

    (* VBT up methods *)
    setcage := SetCage;
    setcursor := SetCursor;
    paintbatch := PaintBatch;
    capture := Capture;
    screenOf := ScreenOf;

    (* split methods *)
    beChild := BeChild;
    init := Be
  END;

PROCEDURE Be(v: T; ch: VBT.T): Filter.T =
  BEGIN
    LOCK v DO
      v.delta := Rect.NorthWest(v.domain)
    END;
    EVAL Filter.Replace(v, ch);
    RETURN v
  END Be;

PROCEDURE New(ch: VBT.T): T =
  BEGIN RETURN Be(NEW(T), ch) END New;

PROCEDURE BeChild(v: T; ch: VBT.T) RAISES {} =
  BEGIN
    Filter.T.beChild(v, ch); 
    VBTClass.ClearShortCircuit(ch)
  END BeChild;
    
PROCEDURE Mouse(prnt: T; READONLY cd: VBT.MouseRec) RAISES {} =
  VAR cdP: VBT.MouseRec; child := prnt.ch;
  BEGIN (* LL = VBT.mu *)
    IF child # NIL THEN
      cdP := cd;
      IF NOT cdP.cp.gone THEN
        cdP.cp.pt := Point.Sub(cdP.cp.pt, prnt.delta)
      END;
      VBTClass.Mouse(child, cdP)
    END
  END Mouse;

PROCEDURE Position(prnt: T; READONLY cd: VBT.PositionRec) RAISES {} =
  VAR cdP: VBT.PositionRec; child := prnt.ch;
  BEGIN (* LL = VBT.mu *)
    IF child # NIL THEN
      cdP := cd;
      IF NOT cdP.cp.offScreen THEN
        cdP.cp.pt := Point.Sub(cd.cp.pt, prnt.delta)
      END;
      VBTClass.Position(child, cdP)
    END
  END Position;

PROCEDURE Repaint(prnt: T; READONLY rgn: Region.T) RAISES {} =
  BEGIN (* LL = VBT.mu *)
    IF prnt.ch # NIL THEN
      VBTClass.Repaint(prnt.ch, Region.Sub(rgn, prnt.delta))
    END
  END Repaint;

PROCEDURE Rescreen(prnt: T; READONLY cd: VBT.RescreenRec) =
  VAR screen := Trestle.ScreenOf(prnt, Point.Origin).id; BEGIN
    LOCK prnt DO prnt.screen := screen END;
    Filter.T.rescreen(prnt, cd)
  END Rescreen;

PROCEDURE Reshape(prnt: T; READONLY cd: VBT.ReshapeRec) RAISES {} =
  VAR
    deltaP: Point.T;
    saved, newchdom, chsaved: Rect.T;
    chbad: Region.T;
    child := prnt.ch;
  BEGIN (* LL = VBT.mu *)
    IF child # NIL THEN
      deltaP :=
        Point.Sub(Rect.NorthWest(cd.new), Rect.NorthWest(cd.prev));
      saved := Rect.Meet(Rect.Move(cd.saved, deltaP), cd.new);
      LOCK child DO
        VBT.Scroll(prnt, saved, deltaP);
        prnt.delta := Rect.NorthWest(cd.new)
      END;
      newchdom := Rect.Sub(cd.new, prnt.delta);
      chsaved := Rect.Sub(saved, prnt.delta);
      (* preserved region, in child's coordinates *)
      IF Rect.Equal(newchdom, VBT.Domain(child)) THEN
        (* Child's domain is unchanged; only need to repaint it *)
        chbad := Region.Difference(Region.FromRect(newchdom),
           Region.FromRect(chsaved));
        VBTClass.Repaint(child, chbad)
      ELSE
        VBTClass.Reshape(child, newchdom, chsaved)
      END
    END
  END Reshape;

PROCEDURE ScreenOf(
    prnt: T;
    <*UNUSED*> ch: VBT.T;
    READONLY pt: Point.T):
    Trestle.ScreenOfRec
    RAISES {} =
  BEGIN (* LL=ch *)
    RETURN Trestle.ScreenOf(prnt, Point.Add(pt, prnt.delta));
  END ScreenOf;

PROCEDURE SetCage(prnt: T; ch: VBT.T) RAISES {} =
  VAR cg := VBTClass.Cage(ch); 
  BEGIN (* LL=ch *)
    LOCK prnt DO
      IF cg.rect # Rect.Full AND prnt.screen = cg.screen THEN 
        cg.rect := Rect.Move(cg.rect, prnt.delta)
      END;
      VBTClass.SetCage(prnt, cg)
    END
  END SetCage;

PROCEDURE SetCursor(prnt: T; ch: VBT.T) RAISES {} =
  VAR cs := ch.getcursor();
  BEGIN (* LL=ch *)
    LOCK prnt DO
      IF cs # prnt.effectiveCursor THEN
        prnt.effectiveCursor := cs;
        IF prnt.parent # NIL THEN prnt.parent.setcursor(prnt) END
      END
    END
  END SetCursor;

PROCEDURE Capture(
    prnt: T; 
    <*UNUSED*> ch: VBT.T; 
    READONLY rect: Rect.T;
    VAR (*out*) br: Region.T)
    : ScrnPixmap.T RAISES {} =
  VAR res: ScrnPixmap.T;
  BEGIN (* LL = ch *)
    res := VBT.Capture(prnt, Rect.Add(rect, prnt.delta), br);
    br := Region.Sub(br, prnt.delta);
    RETURN res
  END Capture;

PROCEDURE PaintBatch(prnt: T; <*UNUSED*> ch: VBT.T; ba: Batch.T) RAISES {} =
  BEGIN
    BatchUtil.Clip(ba);
    BatchUtil.Translate(ba, prnt.delta);
    VBTClass.PaintBatch(prnt, ba)
  END PaintBatch;

BEGIN
END TranslateVBT.
