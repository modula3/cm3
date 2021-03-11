(* $Id$ *)

MODULE Main;
IMPORT Fmt, Scan, Char, Text, Params, FloatMode, Lex;
IMPORT Wr, Rd, IntPair, IntPairList;
IMPORT OSError, FileRd, Stdio, Process, TextReader;
IMPORT Math;
IMPORT Thread;
IMPORT Time, FileWr;
IMPORT Debug;
IMPORT Point;

EXCEPTION ParamErr;

CONST GridColor = Color { 0.0, 0.0, 0.0 };

CONST Usage =
  "conflictdensity [-grid <grid step>] [-scale <post-scaling factor>] [-dumpinterval <number of points added per dump>] [-decay <decay rate>] [-boxes <cell outline file>] [-corners <llx> <lly> <urx> <ury>] [-dumpforhistogram <out-file>] [-maxdensity <max density>] [-influenceradius <influence radius>] <input file>";


(* 

(gdb) show args
Argument list to give program being debugged when it is started is 
    "-boxes control0.gpl -scale 2 -dumpinterval 10 -decay 0.001 -maxdensity 20 -grid 8 -corners -500 0 3000 4500 r".

*)

(* fix these later *)
<* FATAL ParamErr, TextReader.NoMore, Thread.Alerted, Wr.Failure, Rd.Failure *>
<* FATAL OSError.E *>
<* FATAL FloatMode.Trap, Lex.Error *>
        
PROCEDURE GetParam(n : CARDINAL) : TEXT RAISES { ParamErr } =
  BEGIN   
    IF n < Params.Count THEN RETURN Params.Get(n) ELSE RAISE ParamErr END
  END GetParam;

CONST WhiteSpace = " \t";
TYPE 
  GridArr = REF ARRAY OF ARRAY OF REAL;

  Grid = RECORD
    g : GridArr;
    origin : Point.T;
    step : CARDINAL;
  END;

(* we want something with zero influence outside a certain radius.

   idea: add a cone with radius R... and height... 

   V (cone) = 1/3 pi R^2 h = 1

   h = 3/(pi R^2)

   better method: use FFT.
*)

VAR Radius := 80.0;
VAR MaxDensity := 100.0d0;

PROCEDURE DrawLine(buf : Framebuffer; grid : Grid;
                   p, q : Point.T) =
  VAR
    scale := FLOAT(buf.scale) / FLOAT(grid.step);
    fx := FLOAT(p.h-grid.origin.h) * scale;
    fy := FLOAT(p.v-grid.origin.v) * scale;
    gx := FLOAT(q.h-grid.origin.h) * scale;
    gy := FLOAT(q.v-grid.origin.v) * scale;

    dx := gx-fx;
    dy := gy-fy;
    order : [-1..1];
  BEGIN
    IF ABS(dy) < ABS(dx) THEN
      IF fx < gx THEN order := 1 ELSE order := -1 END;
      VAR
        y := fy;
        m := dy/dx;
      BEGIN
        FOR i := ROUND(fx) TO ROUND(gx) BY order DO
          WritePixel(buf.b, Point.T { i, ROUND(y) });
          y := y + m
        END
      END
    ELSE
      IF fy < gy THEN order := 1 ELSE order := -1 END;
      VAR
        x := fx;
        m := dx/dy;
      BEGIN
        FOR j := ROUND(fy) TO ROUND(gy) BY order DO
          WritePixel(buf.b, Point.T { ROUND(x), j });
          x := x + m
        END
      END
    END
  END DrawLine;

PROCEDURE WritePixel(arr : FrameArr; p : Point.T) =
  BEGIN
    IF p.h >= FIRST(arr^) AND p.h <= LAST(arr^) AND 
      p.v >= FIRST(arr[0]) AND p.v <= LAST(arr[0]) THEN
      arr[p.h,p.v] := GridColor
    END
  END WritePixel;

PROCEDURE AddPoint(READONLY p : IntPair.T; 
                   grid : Grid) =
  VAR
    infl := (TRUNC(Radius)) DIV grid.step + 2;
    x := FLOAT(p.k1-grid.origin.h) / FLOAT(grid.step);
    y := FLOAT(p.k2-grid.origin.v) / FLOAT(grid.step);

    xlo := MAX(TRUNC(x) - infl, FIRST(grid.g^));
    ylo := MAX(TRUNC(y) - infl, FIRST(grid.g[0]));
    xhi := MIN(TRUNC(x) + infl, LAST(grid.g^));
    yhi := MIN(TRUNC(y) + infl, LAST(grid.g[0]));
  BEGIN
    (* clip *)
    xlo := MIN(xlo, LAST(grid.g^));
    xlo := MAX(xlo, FIRST(grid.g^));
    xhi := MIN(xhi, LAST(grid.g^));
    xhi := MAX(xhi, FIRST(grid.g^));

    ylo := MIN(ylo, LAST(grid.g[0]));
    ylo := MAX(ylo, FIRST(grid.g[0]));
    yhi := MIN(yhi, LAST(grid.g[0]));
    yhi := MAX(yhi, FIRST(grid.g[0]));

    FOR i := xlo TO xhi DO
      FOR j := ylo TO yhi DO
        VAR
          dx := FLOAT(p.k1 - (i * grid.step + grid.origin.h),LONGREAL);
          dy := FLOAT(p.k2 - (j * grid.step + grid.origin.v),LONGREAL);
          dist := FLOAT(Math.sqrt(dx*dx + dy*dy));
          height := MAX((1.0 - dist/Radius), 0.0);
        BEGIN
          grid.g[i,j] := grid.g[i,j] + height
        END
      END
    END
  END AddPoint;

TYPE Color = RECORD r, g, b : REAL; END;

TYPE 
  FrameArr = REF ARRAY OF ARRAY OF Color;

  Framebuffer = RECORD 
    b : FrameArr;
    scale : CARDINAL
  END;

PROCEDURE ValueToColor(val : REAL) : Color =
  VAR
    scaledD := 
        Math.log(FLOAT(val + 1.0, LONGREAL)) / Math.log(MaxDensity);
    scaled := FLOAT(scaledD);
  CONST
    Step = 1.0/4.0;
  BEGIN
    IF scaled >= 1.0 THEN scaled := 1.0 END;

    IF scaled < Step THEN
      RETURN Color { scaled / Step, Step-scaled, Step-scaled }
    ELSIF scaled < 2.0*Step THEN
      RETURN Color { 1.0, (scaled - Step) / Step, 0.0 }
    ELSIF scaled < 3.0*Step THEN
      RETURN Color { 1.0, 1.0, (scaled - 2.0*Step) / Step }
    ELSIF scaled <= 4.0 * Step THEN
      RETURN Color { 1.0-(scaled-3.0*Step) / Step, 
                     1.0-(scaled-3.0*Step) / Step, 
                     1.0 }
    ELSE
      <* ASSERT FALSE *>
    END
  END ValueToColor;

PROCEDURE DumpHist(grid : Grid; wr : Wr.T) =
  BEGIN
    FOR i := FIRST(grid.g^) TO LAST(grid.g^) DO
      FOR j := FIRST(grid.g[0]) TO LAST(grid.g[0]) DO
        Wr.PutText(wr,Fmt.Real(grid.g[i,j]));
        Wr.PutChar(wr,'\n')
      END
    END
  END DumpHist;

PROCEDURE Decay(grid : Grid; rate : LONGREAL) =
  VAR
    mult := FLOAT(1.0d0 - rate);
    g := grid.g;
  BEGIN
    FOR i := FIRST(g^) TO LAST(g^) DO
      FOR j := FIRST(g[0]) TO LAST(g[0]) DO
        g[i,j] := mult * g[i,j]
      END
    END
  END Decay;
  
PROCEDURE Dump(grid : Grid; fn : TEXT; boxesFn : TEXT) =
  CONST
    MaxPrintColor = 255;
  VAR
    fnn := fn & ".ppm";
    wr := FileWr.Open(fnn);
    gridMax := 0.0;
  BEGIN
    Debug.Out("Dumping grid to \"" & fnn & "\"\n");

    Wr.PutText(wr, "P3\n# " & fnn & "\n# " & Fmt.LongReal(Time.Now()) & "\n");
    
    VAR
      framebuffer := Framebuffer { NEW(FrameArr,
                                       NUMBER(grid.g^) * scale,
                                       NUMBER(grid.g[0]) * scale),
                                   scale };
    BEGIN
      FOR j := LAST(grid.g[0]) TO FIRST(grid.g[0]) BY -1 DO
        FOR i := FIRST(grid.g^) TO LAST(grid.g^) DO
          VAR
            color := ValueToColor(grid.g[i,j]);
          BEGIN
            gridMax := MAX(grid.g[i,j],gridMax);

            FOR k := 0 TO scale-1 DO
              FOR l := 0 TO scale-1 DO
                framebuffer.b[i*scale+k, j*scale+l] := color
              END
            END
          END
        END
      END;
      
      Debug.Out("gridMax = " & Fmt.Real(gridMax));

      (* add the polygons... *)

      IF boxesFn # NIL THEN
        VAR 
          rd := FileRd.Open(boxesFn);
          px, py : INTEGER;
          drawing := FALSE;
        BEGIN
          TRY
            LOOP
              VAR
                line := Rd.GetLine(rd);
              BEGIN
                IF Text.Equal(line,"") THEN 
                  drawing := FALSE 
                ELSE
                  VAR
                    reader := NEW(TextReader.T).init(line);
                    qx := Scan.Int(reader.nextE(" ", skipNulls := TRUE));
                    qy := Scan.Int(reader.nextE(" ", skipNulls := TRUE));
                  BEGIN
                    IF drawing THEN
                      DrawLine(framebuffer, grid,
                               Point.T { px, py } , 
                               Point.T { qx, qy })
                    END;
                    px := qx;
                    py := qy;
                    drawing := TRUE
                  END
                END;
              END
              
            END
          EXCEPT
            Rd.EndOfFile => Rd.Close(rd)
          END
        END
      END;

      Wr.PutText(wr, Fmt.Int(NUMBER(framebuffer.b^)) & " " & Fmt.Int(NUMBER(framebuffer.b[0])) &"\n");
      Wr.PutText(wr, Fmt.Int(MaxPrintColor) & "\n");
      FOR j := LAST(framebuffer.b[0]) TO FIRST(framebuffer.b[0]) BY -1 DO
        FOR i := FIRST(framebuffer.b^) TO LAST(framebuffer.b^) DO
          VAR
            color := framebuffer.b[i,j];
          BEGIN
            Wr.PutText(wr, Fmt.Int(ROUND(color.r * FLOAT(MaxPrintColor))));
            Wr.PutChar(wr, ' ');
            Wr.PutText(wr, Fmt.Int(ROUND(color.g * FLOAT(MaxPrintColor))));
            Wr.PutChar(wr, ' ');
            Wr.PutText(wr, Fmt.Int(ROUND(color.b * FLOAT(MaxPrintColor))));
            Wr.PutChar(wr, '\n')
          END
        END;
        Wr.PutChar(wr, '\n')
      END
    END;
    Wr.Close(wr)
  END Dump;

TYPE
  Corners = OBJECT
    minx, maxx, miny, maxy : INTEGER;
  END;

VAR
  gridStep : CARDINAL := 1;
  scale : CARDINAL := 1;
  newparam : TEXT;
  lNo := 1;
  rd : Rd.T;
  fileName : TEXT;
  i := 1;
  dumpInterval := 0;
  histWr : Wr.T := NIL;
  boxesFn : TEXT := NIL;
  corners : Corners := NIL;
  decay := 0.0d0;
BEGIN
  TRY
    WHILE Char.Equal(Text.GetChar(Params.Get(i),0), '-') DO
      newparam := GetParam(i);
      IF Text.Equal(newparam, "-grid") THEN 
        gridStep := Scan.Int(GetParam(i+1)); i:=i+2
      ELSIF Text.Equal(newparam, "-scale") THEN
        scale := Scan.Int(GetParam(i+1)); i:=i+2
      ELSIF Text.Equal(newparam, "-decay") THEN
        decay := Scan.LongReal(GetParam(i+1)); i:=i+2
      ELSIF Text.Equal(newparam, "-dumpinterval") THEN
        dumpInterval := Scan.Int(GetParam(i+1)); i:=i+2
      ELSIF Text.Equal(newparam, "-boxes") THEN
        boxesFn := GetParam(i+1); i:=i+2
      ELSIF Text.Equal(newparam, "-corners") THEN
        corners := NEW(Corners, 
                       minx := Scan.Int(GetParam(i+1)),
                       miny := Scan.Int(GetParam(i+2)),
                       maxx := Scan.Int(GetParam(i+3)),
                       maxy := Scan.Int(GetParam(i+4)));
        i:=i+5
      ELSIF Text.Equal(newparam, "-dumpforhistogram") THEN
        histWr := FileWr.Open(GetParam(i+1)); i:=i+2
      ELSIF Text.Equal(newparam, "-maxdensity") THEN
        MaxDensity := Scan.LongReal(GetParam(i+1)); i:=i+2
      ELSIF Text.Equal(newparam, "-influenceradius") THEN
        Radius := Scan.Real(GetParam(i+1)); i:=i+2
      ELSE 
        Process.Crash("Unknown command-line flag \"" & newparam & "\"!\n" & Usage)
      END
    END
  EXCEPT
    FloatMode.Trap, Lex.Error => 
      Process.Crash("Trouble parsing command-line argument \"" &
        GetParam(i+1) & "\"!\n" & Usage)
  END;

  fileName := GetParam(i);
  IF Text.Equal(fileName, "-") THEN
    rd := Stdio.stdin
  ELSE
    TRY
      rd := FileRd.Open(fileName)
    EXCEPT
      OSError.E => 
        Process.Crash("Trouble opening \"" & fileName & "\" for reading.")
    END
  END;

  VAR
    data : IntPairList.T;
    minx, miny := LAST(INTEGER);
    maxx, maxy := FIRST(INTEGER);
    dump := 0;
  BEGIN
    TRY
      LOOP
        VAR
          line := Rd.GetLine(rd);
          r := NEW(TextReader.T).init(line);
          x := Scan.Int(r.nextE(WhiteSpace, skipNulls := TRUE));
          y := Scan.Int(r.nextE(WhiteSpace, skipNulls := TRUE));
        BEGIN
          minx := MIN(minx,x);
          miny := MIN(miny,y);
          maxx := MAX(maxx,x);
          maxy := MAX(maxy,y);

          data := IntPairList.Cons(IntPair.T { x, y }, data);
          INC(lNo)
        END
      END
    EXCEPT
      Rd.EndOfFile => TRY Rd.Close(rd) EXCEPT ELSE END
    |
      Lex.Error, FloatMode.Trap =>
        Process.Crash("Garbage input on line " & Fmt.Int(lNo))
    END;
    
    Wr.PutText(Stdio.stdout, "corners (" & Fmt.Int(minx) & "," & Fmt.Int(miny)&
      ") (" & Fmt.Int(maxx) & "," & Fmt.Int(maxy) & ")\n");

    IF corners # NIL THEN
      minx := corners.minx;
      miny := corners.miny;

      maxx := corners.maxx;
      maxy := corners.maxy;
    END;

    data := IntPairList.Reverse(data);

    VAR
      xsize := (maxx-minx) DIV gridStep;
      ysize := (maxy-miny) DIV gridStep;

      grid := Grid { NEW(GridArr, xsize, ysize), 
                     Point.T { minx, miny }, 
                     gridStep };
    BEGIN
      FOR i := FIRST(grid.g^) TO LAST(grid.g^) DO
        FOR j := FIRST(grid.g[i]) TO LAST(grid.g[i]) DO
          grid.g[i,j] := 0.0
        END
      END;

      (* convolve *)
      VAR
        p := data;
        c := 0;
      BEGIN
        WHILE p # NIL DO
          AddPoint(p.head, grid);
          INC(c);

          IF dumpInterval # 0 AND c MOD dumpInterval = 0 THEN
            Dump(grid, "grid.dump." & Fmt.Pad(Fmt.Int(dump), length := 5, padChar := '0'), boxesFn);
            IF decay # 0.0d0 THEN
              Decay(grid, decay * FLOAT(dumpInterval, LONGREAL))
            END;
            INC(dump)
          END;
          p := p.tail
        END;

        IF histWr # NIL THEN DumpHist(grid, histWr) END;

        Dump(grid, "grid.dump", boxesFn)
      END
    END

  END;

  IF histWr # NIL THEN Wr.Close(histWr) END
END Main.
