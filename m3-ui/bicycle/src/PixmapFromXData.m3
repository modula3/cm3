(* Copyright (C) 1991, Digital Equipment Corporation                    *)
(* All rights reserved.                                                 *)
(* See the file COPYRIGHT for a full description.                       *)

(* Last modified on Wed Sep 18 01:24:59 1991 by kalsow  *)
(*      modified on Thu Sep 12 17:59:51 PDT 1991 by msm     *)

MODULE PixmapFromXData;

IMPORT Pixmap, ScrnPixmap, TextRd, Rd, Word, Rect, Point, Palette,
  ScreenType, Thread;

<* FATAL Rd.Failure, Rd.EndOfFile, Thread.Alerted *>

PROCEDURE NextByte(rd: Rd.T): Word.T =
  VAR res := 0; ch := Rd.GetChar(rd); BEGIN
    WHILE ch = ' ' OR ch = '\n' OR ch = '\t' OR ch = ',' DO
      ch := Rd.GetChar(rd)
    END;
    CASE ch OF
      '0','1','2','3','4','5','6','7','8','9' =>
        res := ORD(ch) - ORD('0')
    | 'a','b','c','d','e','f' =>
        res := ORD(ch) - ORD('a') + 10
    | 'A','B','C','D','E','F' =>
        res := ORD(ch) - ORD('A') + 10
    ELSE <* ASSERT FALSE *>
    END;
    TRY
      LOOP
        ch := Rd.GetChar(rd);
        CASE ch OF
          '0','1','2','3','4','5','6','7','8','9' =>
            res := 16 * res + ORD(ch) - ORD('0')
        | 'a','b','c','d','e','f' =>
            res := 16 * res + ORD(ch) - ORD('a') + 10
        | 'A','B','C','D','E','F' =>
            res := 16 * res + ORD(ch) - ORD('A') + 10
        | 'x','X'=> res := 0
        ELSE EXIT
        END
      END;
    EXCEPT Rd.EndOfFile =>
    END;
    RETURN res
  END NextByte;

PROCEDURE P(t: T; halftone: BOOLEAN): Pixmap.T =
  VAR 
    r := ScrnPixmap.NewRaw(1, Rect.FromSize(t.width, t.height));
    rd := TextRd.New(t.t);
    word, mask: Word.T;
    res: Pixmap.T;
  BEGIN
    FOR v := 0 TO t.height - 1 DO
      IF halftone THEN 
        IF v MOD 2 = 0 THEN 
          mask := 16_EE
        ELSE 
          mask := 16_BB
        END
      END;
      FOR h := 0 TO t.width - 1 DO
        IF h MOD 8 = 0 THEN
          word := NextByte(rd);
          IF halftone THEN word := Word.And(word, mask) END
        END;
        r.set(Point.T{h, v}, Word.And(word, 1));
        word := Word.RightShift(word, 1)
      END
    END;
    Rd.Close(rd);
    res := Pixmap.FromBitmap(r);
    IF halftone THEN
      RETURN TwoTone(res, P(t, FALSE))
    ELSE
      RETURN res
    END
  END P;

PROCEDURE Flip(t: T; halftone: BOOLEAN): Pixmap.T =
  VAR 
    r := ScrnPixmap.NewRaw(1, Rect.FromSize(t.width, t.height));
    rd := TextRd.New(t.t);
    word, mask: Word.T;
    res: Pixmap.T;
  BEGIN
    FOR v := 0 TO t.height - 1 DO
      IF halftone THEN 
        IF v MOD 2 = 0 THEN 
          mask := 16_EE
        ELSE 
          mask := 16_BB
        END
      END;
      FOR h := 0 TO t.width - 1 DO
        IF h MOD 8 = 0 THEN
          word := NextByte(rd);
          IF halftone THEN word := Word.And(word, mask) END
        END;
        r.set(Point.T{t.width - 1 - h, t.height - 1 - v}, Word.And(word, 1));
        word := Word.RightShift(word, 1)
      END
    END;
    Rd.Close(rd);
    res := Pixmap.FromBitmap(r);
    IF halftone THEN
      RETURN TwoTone(res, Flip(t, FALSE))
    ELSE
      RETURN res
    END
  END Flip;

PROCEDURE TwoTone(bw, color: Pixmap.T): Pixmap.T =
(* Return the pixmap which is "pm" on a black-and-white display,
   and "Pixmap.Solid" otherwise. *)
   BEGIN
    RETURN Palette.FromPixmapClosure(NEW(TTClosure, bw := bw, color := color))
   END TwoTone;

TYPE TTClosure = Palette.PixmapClosure OBJECT
    bw, color: Pixmap.T
  OVERRIDES
    apply := TTApply
  END;

PROCEDURE TTApply(cl: TTClosure; st: ScreenType.T): ScrnPixmap.T =
  BEGIN
    IF st.depth = 1 THEN 
      RETURN Palette.ResolvePixmap(st, cl.bw)
    ELSE
      RETURN Palette.ResolvePixmap(st, cl.color)
    END
  END TTApply;

BEGIN END PixmapFromXData.
