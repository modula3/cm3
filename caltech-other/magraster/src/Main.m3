(* $Id$ *)

MODULE Main;
IMPORT Params;
IMPORT Scan, Text;
IMPORT MagPath, FloatMode, Lex;
IMPORT Debug;
IMPORT TextMagLayerTbl AS TextLayerTbl, MagCell;
IMPORT MagTransform, MagCellExtendable, MagSubCell AS SubCell;
IMPORT Rd, Thread;
IMPORT FrameBuffer, FrameBufferOutput, Configuration;
IMPORT FileRd, OSError;
IMPORT MagRect, MagLayer AS Layer, MagPoint;
IMPORT PaintLayer, PaintLayerClass;
IMPORT Fmt;
IMPORT TextUtils;
IMPORT Wr, FileWr;
FROM Tint IMPORT Channel;
IMPORT Tint;
IMPORT SIsuffix;
IMPORT RTCollectorSRC;

EXCEPTION 
  ParamErr;

(* parameter things should be broken out as a "database" *)

PROCEDURE GetParam(n : CARDINAL) : TEXT RAISES { ParamErr } = 
  BEGIN
    IF n < Params.Count THEN RETURN Params.Get(n) ELSE RAISE ParamErr END
  END GetParam;

<*NOWARN*>PROCEDURE DumpRect(rect : MagRect.T; 
                   layer : Layer.T; <*UNUSED*> args : REFANY) =
  BEGIN 
    Debug.Out("Rect at " & MagRect.Format(rect) & 
      " : " & NARROW(layer, PaintLayer.T).format() ) 
  END DumpRect;

PROCEDURE DrawRect(rect : MagRect.T; 
                   layer : Layer.T; args : REFANY) =
  BEGIN FrameBuffer.DrawRect(args, rect, layer) END DrawRect;

  (* this routine integrates a bunch of object-precision pixels into
     a single image-precision pixel *)

PROCEDURE ComputePixel( READONLY x, y : CARDINAL;
                        buf : FrameBuffer.T;
                        lpp : LONGREAL ) : Tint.T =
  VAR
    (* compute object-precision boundaries *)
    llxR := lpp * FLOAT(x,LONGREAL);  urxR := llxR + lpp;
    llyR := lpp * FLOAT(y,LONGREAL);  uryR := llyR + lpp;

    (* lower left object pixel needed *)
    llx := FLOOR(llxR); lly := FLOOR(llyR);
    
    (* upper right *)
    urx := CEILING(urxR); ury := CEILING(uryR);
    
    xcov, xcovOverArea, ycov : LONGREAL;
    sum := Tint.Black;
    pixelArea := lpp * lpp;

  BEGIN
    FOR i := llx TO urx - 1 DO
      xcov := MIN(urxR, FLOAT(i + 1,LONGREAL)) - 
              MAX(llxR, FLOAT(i,LONGREAL));
      xcovOverArea := xcov / pixelArea;

      FOR j := lly TO ury - 1 DO
        ycov := MIN(uryR, FLOAT(j + 1,LONGREAL)) - 
                MAX(llyR, FLOAT(j,LONGREAL));
        VAR 
          oTint := FrameBufferOutput.GetTint(buf, MagPoint.T { i, j });
        BEGIN
          FOR i := Channel.R TO Channel.B DO
            sum[i] := sum[i] + oTint[i] * xcovOverArea * ycov
          END
        END
      END
    END;
    RETURN sum
  END ComputePixel;

VAR 
  topCell : TEXT := NIL;
  layout : MagCell.T;
  memUse := 1024 * 1024 * 16; (* 16 megs *)
  xres : CARDINAL := 1024;
  yres : CARDINAL := 768;
  magHeight, magWidth : CARDINAL;
  frameBuffer : FrameBuffer.T;
  layerDB : TextLayerTbl.T;
  configFile := "magraster.conf";
  outFile : Wr.T;
  gamma := 2.0d0;
  ppmFileName := "magraster.ppm";
BEGIN
  Debug.RaiseLevel(10);

  (* tweak gc *)
  RTCollectorSRC.incremental := FALSE;
  RTCollectorSRC.gcRatio := 0.2;

  (* parse command-line args *)
  VAR 
    i := 1;
    flag : TEXT;
  BEGIN
    TRY
      WHILE i < Params.Count DO
        flag := GetParam(i);
        IF Text.Equal(flag,"-path") THEN (* set magic path *)
          INC(i);
          MagPath.Set(GetParam(i))
        ELSIF Text.Equal(flag, "-conf") THEN (* set config file *)
          INC(i);
          configFile := GetParam(i)
        ELSIF Text.Equal(flag, "-x") THEN (* set x width in pixels *)
          INC(i);
          xres := Scan.Int(GetParam(i)) 
        ELSIF Text.Equal(flag, "-y") THEN (* set y height in pixels *)
          INC(i);
          yres := Scan.Int(GetParam(i))
        ELSIF Text.Equal(flag, "-top") THEN (* top cell (design) *)
          INC(i);
          topCell := GetParam(i)
        ELSIF Text.Equal(flag, "-mem") THEN (* memory use *)
          INC(i);
          memUse := SIsuffix.Int(GetParam(i), mode := SIsuffix.Mode.Base2) 
        ELSIF Text.Equal(flag, "-gamma") THEN (* gamma exponent *)
          INC(i);
          gamma := Scan.LongReal(GetParam(i))
        ELSIF Text.Equal(flag, "-o") THEN
          INC(i);
          ppmFileName := GetParam(i)
        ELSE
          Debug.Error("Unknown command-line flag \"" & GetParam(i) & "\"")
        END;
        INC(i)
      END
    EXCEPT
      FloatMode.Trap , Lex.Error , ParamErr, SIsuffix.OutOfRange, 
      SIsuffix.UnknownSuffix => 
      Debug.Error("Command-line parameter syntax error.");
    END
  END;

  TRY
    layerDB := Configuration.ReadConf(FileRd.Open(configFile));
  EXCEPT
    OSError.E => Debug.Error("Couldn't open conf file: ")
  END;

  Debug.Out("Output file is \"" & ppmFileName & "\"");
  TRY
    outFile := FileWr.Open(ppmFileName);
  EXCEPT
    OSError.E => Debug.Error("Couldn't open output file: " & ppmFileName)
  END;


  IF topCell = NIL THEN
    Debug.Error("No top-level cell specified.")
  END;

  Debug.Out("Max memory use for frame buffer is " & Fmt.Int(memUse) &
    " bytes.");

  Debug.Out("Reading in layout:");
  TRY
    layout := NEW(MagCell.T).lookup(topCell, layerDB);
  EXCEPT
  | MagCell.NotFound => Debug.Error("Cell not found: " & topCell)
  | Rd.Failure, Thread.Alerted => Debug.Error("I/O Error opening " & topCell)
  END;

  (* this is a good time to garbage collect... *)
  RTCollectorSRC.StartCollection();
  RTCollectorSRC.FinishCollection();

  (* get width of layout in lambdas *)
  WITH bbox = layout.getBBox() DO
    magWidth := bbox.ur.x - bbox.ll.x;
    (* shift layout so that its origin is at 0,0 *)
    VAR
      sform := MagTransform.T { 1, 0, -bbox.ll.x, 
                                0, 1, -bbox.ll.y };
      xform := 
          MagTransform.Compose( MagTransform.T { 1, 0, 0, 0, -1, 
                                                 bbox.ur.y - bbox.ll.y } , 
                                sform );
      subCell := SubCell.T { layout, xform, "TOP", box := bbox };
    BEGIN
      layout := NEW(MagCell.T).init("toptop");
      layout.addSub(subCell)
    END;

    (* adjust resolution to give proper aspect ratio *)
    VAR
      cellW := magWidth;
      cellH := bbox.ur.y - bbox.ll.y;
    BEGIN
      IF xres * cellH > cellW * yres THEN
        xres := (cellW * yres) DIV cellH
      ELSE
        yres := (cellH * xres) DIV cellW
      END;

      Debug.Out("\nAllocating buffer.");
      frameBuffer := NEW(FrameBuffer.T).init(layerDB,
                                             memUse, 
                                             magWidth,
                                             magHeight,
                                             cellH)
    END
  END;

  Debug.Out("");

  TRY
    VAR
      bbox := layout.getBBox();
      cellHeight := bbox.ur.y - bbox.ll.y;
      lambdasPerPixel := FLOAT(cellHeight,LONGREAL) / FLOAT(yres,LONGREAL);
      
      (* - 1 here is a fudge factor.. *)
      pixelsPerStrip := FLOOR(FLOAT(magHeight - 1,LONGREAL) / lambdasPerPixel);

      strips := yres DIV pixelsPerStrip + 1;
    BEGIN
      Debug.Out("Resolution is " & Fmt.Int(xres) & "x" & Fmt.Int(yres));
      Debug.Out("bbox at " & MagRect.Format( layout.getBBox() ));
      Debug.Out("Will use " & TextUtils.Pluralize(" strip",strips) & "  of " & 
        Fmt.Int(pixelsPerStrip) & " each.");

      Wr.PutText(outFile, "P6\n" & 
        Fmt.Int(xres) & " " & 
        Fmt.Int(yres) & 
        "\n255\n");

      FOR s := 0 TO strips - 1 DO
        (* 
           N.B. to render a pixel at position (x,y), we need to have
           all data from (x,y) to (x+1, y+1).  In other words,
           we need to integrate over data from all object-precision 
           pixels that intersect the image-precision square
           {(x,y),(x+1, y+1)}.
        *)
        
        VAR
          strtPix := s * pixelsPerStrip;
          stopPix := MIN((s + 1) * pixelsPerStrip - 1, yres - 1);
          strtLam := FLOOR(lambdasPerPixel * FLOAT(strtPix,LONGREAL));
          stopLam := CEILING(lambdasPerPixel * FLOAT(stopPix + 1,LONGREAL));
          clipBox := NEW(REF MagRect.T);
        BEGIN
          clipBox.ll.y := strtLam;
          clipBox.ur.y := stopLam;
          clipBox.ll.x := 0;
          clipBox.ur.x := bbox.ur.x - bbox.ll.x;

          <* ASSERT stopLam - strtLam <= magHeight *>

          Debug.Out("Strip " & Fmt.Pad(Fmt.Int(s),5) & ": pixel " & 
            Fmt.Pad(Fmt.Int(strtPix),3) &
            " to pixel " & Fmt.Pad(Fmt.Int(stopPix),5) & "; clip to " & 
            MagRect.Format(clipBox^));

          (* Clear contents of frame buffer *)
          frameBuffer.clear();

          (* set up coordinates of this pass *)
          frameBuffer.setOffset(MagPoint.T { 0, strtLam });

          (* draw rectangles into framebuffer *)
          layout.flatClipMap(DrawRect, frameBuffer, clipBox);

          FOR j := strtPix TO stopPix DO
            FOR i := 0 TO xres - 1 DO
              VAR
                tint := Tint.Gamma(ComputePixel(i,j, 
                                                frameBuffer, lambdasPerPixel),
                                   gamma);
                r := ROUND(255.0d0*tint[Channel.R]);
                g := ROUND(255.0d0*tint[Channel.G]);
                b := ROUND(255.0d0*tint[Channel.B]);
              BEGIN
                Wr.PutChar(outFile, VAL(r,CHAR));
                Wr.PutChar(outFile, VAL(g,CHAR));
                Wr.PutChar(outFile, VAL(b,CHAR))
              END
            END
          END

        END
      END (* FOR *)
    END; (* BEGIN *)
    Wr.Flush(outFile); Wr.Close(outFile)
  EXCEPT
    Thread.Alerted, Wr.Failure => Debug.Error("I/O Error writing output file.")
  END;
  Debug.Out("")
END Main.


