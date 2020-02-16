(* $Id$ *)

MODULE AlphaBlend;
IMPORT FramePixel, Tint, LayerIntTbl, BaseLayerList, BaseLayerListSort, Word;
FROM Tint IMPORT Channel;
IMPORT Debug;

PROCEDURE Blend(READONLY pixel : FramePixel.T; tbl : LayerIntTbl.Default) : Tint.T =
  VAR
    tint : Tint.T;
  BEGIN
    IF tbl.cachedKey # NIL AND FramePixel.Equal(tbl.cachedKey^,pixel) THEN 
      RETURN tbl.cachedVal 
    END;

    IF NOT tbl.pixelTintTbl.get(pixel, tint) THEN 
      tint := ComputeTint(pixel, tbl);
      EVAL tbl.pixelTintTbl.put(pixel, tint)
    END;

    (* cache the last value instead of looking it up in the hash table *)

    IF tbl.cachedKey = NIL THEN 
      tbl.cachedKey := NEW(REF FramePixel.T, NUMBER(pixel))
    END;
    
    tbl.cachedKey^ := pixel;
    tbl.cachedVal := tint;

    RETURN tint
  END Blend;

  (* blend in each layer *)
  (* this isn't REALLY alpha blending *)
  (* 
     The model is:
     White background, bright light.
     Monochromatic color mixtures, RGB.
     
     Each layer has transparency for other layers (1 - alpha) plus 
     "own light" (alpha)
  *)

PROCEDURE ComputeTint(READONLY pixel : FramePixel.T; 
                      tbl : LayerIntTbl.Default) : Tint.T =
  VAR
    layerList : BaseLayerList.T := NIL;
    tint := BaseTint;
  BEGIN
    FOR i := FIRST(pixel) TO LAST(pixel) DO
      FOR b := 0 TO Word.Size - 1 DO
        IF Word.Extract(pixel[i], b, 1) # 0 THEN
          layerList := BaseLayerList.Cons(tbl.getLayer(i * Word.Size + b),
                                          layerList)
        END
      END
    END;

    (* sort in ascending order *)
    layerList := BaseLayerListSort.Sort(layerList);
    VAR
      cur := layerList;
    BEGIN
      WHILE cur # NIL DO
        WITH c = cur.head.tint DO
          FOR i := Channel.R TO Channel.B DO
            tint[i] := tint[i] *        (1.0d0 - c[Channel.alpha]) +
                                 c[i] *          c[Channel.alpha]
          END
        END;
        cur := cur.tail
      END
    END;
    Debug.Out("Computing tint for " & FramePixel.Format(pixel), 100);
    RETURN tint
  END ComputeTint;

BEGIN END AlphaBlend.
