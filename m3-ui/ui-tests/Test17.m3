MODULE Test17 EXPORTS Main;

IMPORT Axis, PaintOp, Pixmap, Point, Rect, Region, ScrnPixmap, Text, Trestle, 
       Word, VBT;

PROCEDURE Pmap(): Pixmap.T =

  PROCEDURE CharToInt(ch: CHAR): INTEGER =
    BEGIN
      CASE ch OF
      | '0'..'9' => RETURN ORD(ch) - ORD('0')
      | 'a'..'f' => RETURN ORD(ch) - ORD('a') + 10
      ELSE <* ASSERT FALSE *>
      END;
    END CharToInt;
    
  PROCEDURE NextByte(): Word.T =
    BEGIN
      WITH hi = Text.GetChar (text[i], 0), lo = Text.GetChar (text[i], 1) DO
        INC(i);
        RETURN CharToInt(hi) * 16 + CharToInt(lo);
      END;
    END NextByte;

  CONST
    width  = 47;
    height = 46;
    text = ARRAY OF TEXT {
           "00","b0","20","08","82","06","00","e0","20","08","82","03",
           "00","c0","db","b6","ed","01","00","80","21","08","c2","00",
           "00","00","ff","ff","7f","00","00","00","56","55","35","00",
           "00","00","ac","aa","1a","00","00","00","fc","ff","0f","00",
           "00","00","fc","ff","0f","00","00","00","04","a0","0a","08",
           "00","00","3c","ae","0a","1c","00","00","74","a6","0a","3e",
           "00","00","7c","be","0a","3a","00","00","24","a0","0a","2a",
           "00","00","24","a0","0a","3a","00","00","e4","a1","0a","2a",
           "00","00","44","a0","0a","3a","00","00","b4","a1","3a","2a",
           "00","00","47","a0","2e","3a","00","80","e4","b8","2b","2a",
           "00","80","06","a7","3a","3a","00","e0","e0","ac","1e","2a",
           "00","f8","f1","21","32","3a","00","fe","9f","ff","6b","2a",
           "80","ff","3f","22","e2","3a","e0","ff","bf","88","f8","2b",
           "f8","df","7f","22","be","3b","fe","bf","fe","88","cf","2b",
           "ff","f7","f5","e3","63","3a","7f","36","eb","ff","31","2b",
           "3e","36","f6","3e","78","3a","be","f7","ea","00","1c","6b",
           "de","e3","d5","38","ce","7b","fe","9c","ef","38","9e","6b",
           "7e","7f","d7","d6","c6","7b","be","eb","ae","ee","f3","6b",
           "de","dd","d5","d6","e3","7b","ff","aa","ab","10","f7","6b",
           "7b","77","d7","38","f3","7b","7d","2a","eb","87","f9","6b",
           "fc","c9","9d","8c","f9","7b","f8","ff","86","90","fb","0f",
           "fc","67","b6","b6","f9","78","fe","d7","32","a6","fd","0e",
           "cf","cf","82","a0","f9","7e","d8","ef","de","bd","fb","0d"};
  VAR 
    r := ScrnPixmap.NewRaw(1, Rect.FromSize(width, height));
    i := 0;
    word: Word.T;
    res: Pixmap.T;
  BEGIN
    FOR v := 0 TO height - 1 DO
      FOR h := 0 TO width - 1 DO
        IF h MOD 8 = 0 THEN
          word := NextByte();
        END;
        r.set(Point.T{h, v}, Word.And(word, 1));
        word := Word.RightShift(word, 1)
      END
    END;
    res := Pixmap.FromBitmap(r);
    RETURN res
  END Pmap;

VAR p := Pmap();

PROCEDURE Shape (<*UNUSED*> ch: VBT.T;
                 <*UNUSED*> ax: Axis.T;
                 <*UNUSED*> n : CARDINAL): VBT.SizeRange =
  BEGIN
    RETURN VBT.SizeRange{lo := 240, pref := 240, hi := 241}
  END Shape;


PROCEDURE Repaint (v: VBT.Leaf; <*UNUSED*> READONLY bad: Region.T) =
  VAR
    r := Rect.Full;
  BEGIN
    VBT.PaintTint (v, Rect.Full,                PaintOp.FromRGB(1.0,0.0,0.0));
    VBT.PaintTint (v, Rect.T {  0, 30, 0, 240}, PaintOp.FromRGB(0.0,1.0,0.0));
    VBT.PaintTint (v, Rect.T { 60, 90, 0, 240}, PaintOp.FromRGB(0.0,1.0,0.0));
    VBT.PaintTint (v, Rect.T {120,150, 0, 240}, PaintOp.FromRGB(0.0,1.0,0.0));
    VBT.PaintTint (v, Rect.T {180,210, 0, 240}, PaintOp.FromRGB(0.0,1.0,0.0));
    VBT.PaintPixmap(v, r, PaintOp.BgBg,                   p, Point.T{  5,  5});
    VBT.PaintPixmap(v, r, PaintOp.BgFg,                   p, Point.T{  5, 65});
    VBT.PaintPixmap(v, r, PaintOp.BgTransparent,          p, Point.T{  5,125});
    VBT.PaintPixmap(v, r, PaintOp.BgSwap,                 p, Point.T{  5,185});
    VBT.PaintPixmap(v, r, PaintOp.FgBg,                   p, Point.T{ 65,  5});
    VBT.PaintPixmap(v, r, PaintOp.FgFg,                   p, Point.T{ 65, 65});
    VBT.PaintPixmap(v, r, PaintOp.FgTransparent,          p, Point.T{ 65,125});
    VBT.PaintPixmap(v, r, PaintOp.FgSwap,                 p, Point.T{ 65,185});
    VBT.PaintPixmap(v, r, PaintOp.TransparentBg,          p, Point.T{125,  5});
    VBT.PaintPixmap(v, r, PaintOp.TransparentFg,          p, Point.T{125, 65});
    VBT.PaintPixmap(v, r, PaintOp.TransparentTransparent, p, Point.T{125,125});
    VBT.PaintPixmap(v, r, PaintOp.TransparentSwap,        p, Point.T{125,185});
    VBT.PaintPixmap(v, r, PaintOp.SwapBg,                 p, Point.T{185,  5});
    VBT.PaintPixmap(v, r, PaintOp.SwapFg,                 p, Point.T{185, 65});
    VBT.PaintPixmap(v, r, PaintOp.SwapTransparent,        p, Point.T{185,125});
    VBT.PaintPixmap(v, r, PaintOp.SwapSwap,               p, Point.T{185,185});
  END Repaint;


BEGIN
  WITH v = NEW (VBT.Leaf, repaint := Repaint, shape := Shape) DO
    Trestle.Install(v);
    Trestle.AwaitDelete(v);
  END;
END Test17.
