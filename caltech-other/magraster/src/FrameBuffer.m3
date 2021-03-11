(* $Id$ *)

MODULE FrameBuffer EXPORTS FrameBuffer, FrameBufferOutput;
IMPORT FrameBufferOutput;
IMPORT Word, MagLayer AS Layer, PaintLayer, PaintLayerClass, BaseLayerList, BaseLayer, TextMagLayerTbl AS TextLayerTbl, LayerIntTbl;
IMPORT MagRect, MagPoint;
IMPORT Debug, Fmt, TextUtils;
IMPORT FramePixel;
IMPORT AlphaBlend, Tint;

TYPE
  Pixel = FramePixel.T;
  Buffer = UNTRACED REF ARRAY OF ARRAY OF Pixel; (* XXX will leak *)

REVEAL
  T = FrameBufferOutput.Output BRANDED Brand OBJECT
    pens : LayerIntTbl.T; (* used for color conversion *)
    buf : Buffer;
    origin : MagPoint.T := MagPoint.T { 0, 0 };
    zeroValue : REF Pixel;
  OVERRIDES
    init := Init;
    clear := Clear;
    setOffset := SetOffset;
  END;

(* produce a zero value with the right dimension for the given buffer *)
PROCEDURE ZeroValue(buf : Buffer) : REF Pixel =
  VAR
    res := NEW(REF Pixel, NUMBER(buf[0,0]));
  BEGIN
    FOR i := FIRST(res^) TO LAST(res^) DO res[i] := 0 END;
    RETURN res
  END ZeroValue;

PROCEDURE GetTint(self : T; READONLY pt : MagPoint.T) : Tint.T =
  VAR
    x := pt.x - self.origin.x;
    y := pt.y - self.origin.y;
  BEGIN
    
    (* 
       XXX only one type of out-of-bound access should be possible:
       accessing an element overrunning in x, but only by one image pixel 
    *)

    <* ASSERT NOT ( x < 0 OR y < 0 OR y > LAST(self.buf[0]) ) *>

    IF x > LAST(self.buf^) THEN
      RETURN AlphaBlend.Blend(self.zeroValue^,self.pens)
    ELSE
      RETURN AlphaBlend.Blend(self.buf[x,y], self.pens)
    END
  END GetTint;

PROCEDURE SetOffset(self : T ; ll : MagPoint.T) = 
  BEGIN self.origin := ll END SetOffset;

PROCEDURE Init(self : T; db : TextLayerTbl.T; maxBytes , width : CARDINAL;
               VAR height : CARDINAL; cellHeight : CARDINAL) : T =

  PROCEDURE InitPenTbl() =
    VAR
      tbl := NEW(LayerIntTbl.Default).init();
      iter := db.iterate();
      layer : Layer.T;
      dummy : TEXT;
      i := 0;
    BEGIN
      WHILE iter.next(dummy, layer) DO
        TYPECASE layer OF
        | BaseLayer.T => EVAL tbl.put(layer, i); INC(i)
        ELSE 
          (* skip *)
        END
      END;
      self.pens := tbl
    END InitPenTbl;

  VAR
    pixelWords, pixelBytes : CARDINAL;  
  BEGIN
    InitPenTbl();
    pixelWords := self.pens.size() DIV Word.Size + 1;
    pixelBytes := pixelWords * BYTESIZE(Word.T);

    <* ASSERT width > 0 *>

    (* 2 is for fudging the edges (?) *)
    height := MIN(maxBytes DIV (width * pixelBytes),cellHeight + 2);

    Debug.Out("FrameBuffer.Init: " &
      TextUtils.Pluralize("pen",self.pens.size()) & ", " &
      TextUtils.Pluralize("word",pixelWords) & " per pixel, " & 
      Fmt.Int(pixelBytes) & " bytes per pixel" & 
      "\nFrameBuffer.Init: " &
      Fmt.Int(width) & " lambda wide, " & 
      Fmt.Int(height) & " lambda high.");
      
    self.buf := NEW(Buffer, width, height, pixelWords);
    self.zeroValue := ZeroValue(self.buf);
    self.clear();
    RETURN self
  END Init;

PROCEDURE Clear(self : T) =
  BEGIN WITH buf = self.buf^ DO
    FOR i := FIRST(buf) TO LAST(buf) DO
      FOR j := FIRST(buf[0]) TO LAST(buf[0]) DO
        FOR k := FIRST(buf[0,0]) TO LAST(buf[0,0]) DO
          buf[i,j,k] := 0
        END
      END
    END
  END END Clear;

PROCEDURE DrawRect(frameBuffer : T; 
                   READONLY at : MagRect.T; READONLY player : PaintLayer.T) =

  BEGIN WITH buf = frameBuffer.buf^ DO

    PROCEDURE DrawBaseRect() =
      VAR
        pen: INTEGER;
        penOffset, penBit : CARDINAL;
        x : BOOLEAN;
      BEGIN
        x := frameBuffer.pens.get(bl.head, pen);
        <* ASSERT x *>
        penOffset := pen DIV Word.Size;
        penBit := pen MOD Word.Size;
        FOR i := MAX(0, drawRect.ll.x) TO MIN(maxX, drawRect.ur.x - 1) DO
          FOR j := MAX(0, drawRect.ll.y) TO MIN(maxY, drawRect.ur.y - 1) DO
            WITH word = buf[i,j,penOffset] DO
              word := Word.Insert(word, 1, penBit, 1)
            END
          END
        END
      END DrawBaseRect;
      
    VAR
      maxX, maxY : CARDINAL;
      drawRect := MagRect.Shift(at, frameBuffer.origin);
      bl : BaseLayerList.T := player.baseLayers;
    BEGIN 
      maxX := LAST(buf); maxY := LAST(buf[0]); 
      WHILE bl # NIL DO DrawBaseRect(); bl := bl.tail END 
    END
  END END DrawRect;

BEGIN END FrameBuffer.
