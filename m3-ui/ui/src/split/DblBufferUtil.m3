(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Wed Apr  6 13:08:40 PDT 1994 by heydon                   *)

UNSAFE MODULE DblBufferUtil;

IMPORT VBT, Point, Rect, Path, PathPrivate, Batch, BatchUtil, BatchRep,
  PaintPrivate, PaintExt, Word, Math;

TYPE Com = PaintPrivate.PaintCommand;

CONST
  MiterLimit = 11.0d0;	    (* in degrees *)
  ToRadians = 3.14159265358979324d0 / 180.0d0;

VAR (* CONST *)
  MiterFactor: LONGREAL;    (* 1.0 / sin((ToRadians * MiterLimit) / 2.0) *)

PROCEDURE Tighten(ba: Batch.T) =
  VAR
    cptr: PaintPrivate.CommandPtr := BatchUtil.Succ(ba, NIL);
    doClip := ba.clipped = BatchUtil.ClipState.Unclipped;
    baClip := ba.clip;
    res := Rect.Empty;
  BEGIN
    WHILE cptr # NIL DO
      IF doClip AND NOT Rect.Subset(cptr.clip, baClip) THEN
        (* clip the command's "clip" to the batch's "clip" *)
        IF cptr.command = Com.TextCom THEN
          VAR tptr := LOOPHOLE(cptr, PaintPrivate.TextPtr); BEGIN
            tptr.props := tptr.props +
              PaintPrivate.Props{PaintPrivate.Prop.Clipped}
          END
        END;
        cptr.clip := Rect.Meet(cptr.clip, baClip)
      END;
      IF cptr.command = Com.ExtensionCom THEN
        VAR
          eptr := LOOPHOLE(cptr, PaintPrivate.ExtensionPtr);
          rect := ExtensionBB(eptr);
        BEGIN
          cptr.clip := Rect.Meet(cptr.clip, rect);
          res := Rect.Join(res, cptr.clip);
          (* clip any following "RepeatCom"'s by "rect" *)
          LOOP
            cptr := BatchUtil.Succ(ba, cptr);
            IF cptr = NIL OR cptr.command # Com.RepeatCom THEN EXIT END;
            IF doClip THEN cptr.clip := Rect.Meet(cptr.clip, baClip) END;
            cptr.clip := Rect.Meet(cptr.clip, rect);
            res := Rect.Join(res, cptr.clip)
          END
        END
      ELSE
        res := Rect.Join(res, cptr.clip);
        cptr := BatchUtil.Succ(ba, cptr)
      END;
    END;
    ba.clip := res;
    ba.clipped := BatchUtil.ClipState.Tight
  END Tighten;

PROCEDURE ExtensionBB(eptr: PaintPrivate.ExtensionPtr): Rect.T =
  VAR res: Rect.T; BEGIN
    CASE eptr.subCommand OF
      PaintExt.FillCommand =>
        VAR fptr := LOOPHOLE(eptr, PaintExt.FillPtr); BEGIN
          res := PathBB(eptr, ADR(fptr.path))
        END
    | PaintExt.StrokeCommand =>
        VAR sptr := LOOPHOLE(eptr, PaintExt.StrokePtr); grow: INTEGER; BEGIN
          res := PathBB(eptr, ADR(sptr.path));
          IF sptr.width # 0 THEN
            grow := (sptr.width + 1) DIV 2;
            IF sptr.join = VBT.JoinStyle.Miter THEN
              grow := CEILING(FLOAT(grow, LONGREAL) * MiterFactor)
            END;
            res := Rect.Inset(res, -grow)
          END
        END
    | PaintExt.LineCommand =>
        VAR lptr := LOOPHOLE(eptr, PaintExt.LinePtr); BEGIN
          res := RectHull(lptr.p, lptr.q);
          IF lptr.width # 0 THEN
            res := Rect.Inset(res, -((lptr.width + 1) DIV 2))
          END
        END
    ELSE
        RETURN Rect.Full
    END;
    RETURN Rect.Add(res, eptr.delta)
  END ExtensionBB;

PROCEDURE RectHull(READONLY p, q: Point.T): Rect.T =
  BEGIN
    RETURN Rect.T{
      MIN(p.h, q.h), MAX(p.h, q.h) + 1,
      MIN(p.v, q.v), MAX(p.v, q.v) + 1}
  END RectHull;

PROCEDURE PathBB(
    eptr: PaintPrivate.ExtensionPtr;
    pptr: PaintExt.PathPtr)
  : Rect.T =
  <* FATAL Path.Malformed *>
  VAR path := NEW(Path.T); BEGIN
    path.curveCount := pptr.curveCount;
    path.start := pptr + ADRSIZE(pptr^); (* skip "curveCount" field *)
    VAR end := eptr + (eptr.szOfRec * ADRSIZE(Word.T)); BEGIN
      path.next := end;
      path.end := end;
      path.current := end
    END;
    RETURN Path.BoundingBox(path)
  END PathBB;

BEGIN
  (* initialize MiterFactor *)
  MiterFactor := 1.0d0 / Math.sin((ToRadians * MiterLimit) / 2.0d0);
END DblBufferUtil.
