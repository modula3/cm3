(* Copyright (C) 1996, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Fri Oct 18 13:55:00 PDT 1996 by najork                   *)
(*       Created on Tue Oct 15 13:38:19 PDT 1996 by najork                   *)

MODULE Main;

IMPORT FormsVBT, MyBundle, PaintOp, Path, Pixmap, Point, 
       Rect, Region, Rsrc, ScrnPixmap, Text, Trestle, VBT;


TYPE
  Form = FormsVBT.T BRANDED OBJECT
    v: Leaf;
  END;

  Leaf = VBT.Leaf BRANDED OBJECT
    rect : Rect.T;
    delta: Point.T;
    op   : PaintOp.T;
    src  : Pixmap.T;
    bgPat: BOOLEAN;
  METHODS
    init (): Leaf := Init;
  OVERRIDES
    repaint := Repaint;
  END;

CONST
  PxmpRect = Rect.T {15, 65, 10, 60};
  ClipRect = Rect.T {50, 100, 350, 400};


PROCEDURE Init (v: Leaf): Leaf =
  BEGIN
    WITH raw = ScrnPixmap.NewRaw (1, PxmpRect) DO
      (* First, set all pixels in the raw to 0 *)
      FOR x := raw.bounds.west TO raw.bounds.east-1 DO
        FOR y := raw.bounds.north TO raw.bounds.south-1 DO
          raw.set (Point.T{x, y}, 0);
        END;
      END;

      (* This chunk of code assumes that the pixmap is 50 by 50! *)
      FOR i := 5 TO 44 DO
        WITH j = ABS (((i-5) MOD 10) - 5) DO
          FOR k := j TO j+3 DO
            raw.set (Point.T{raw.bounds.west + k, raw.bounds.north + i}, 1);
            raw.set (Point.T{raw.bounds.east-1 - k, raw.bounds.north + i}, 1);
            raw.set (Point.T{raw.bounds.west + i, raw.bounds.north + k}, 1);
            raw.set (Point.T{raw.bounds.west + i, raw.bounds.south-1 - k}, 1);
          END;
        END;
      END;

      v.src := Pixmap.FromBitmap (raw);
    END;
          
    v.rect := ClipRect;
    v.delta := Rect.NorthWest (ClipRect);
    v.op := PaintOp.BgFg;
    RETURN v;
  END Init;


PROCEDURE Repaint (v: Leaf; <*UNUSED*> READONLY rgn: Region.T) =
  CONST
    ts = 25; (* Background tile size *)
  VAR
    part: Rect.Partition;
    path := NEW (Path.T);
  BEGIN
    (* Paint the background *)
    VBT.PaintTint (v, Rect.Full, PaintOp.Bg);

    (* Paint background pattern if desired *)
    IF v.bgPat THEN
      WITH dom = VBT.Domain (v) DO
        FOR x := dom.west TO dom.east - 1 BY ts DO
          FOR y := dom.north TO dom.south - 1 BY ts DO
            IF (x - dom.west + y - dom.north) MOD (2 * ts) = 0 THEN
              VBT.PaintTint (v, Rect.T {x, x + ts, y, y + ts}, PaintOp.Fg);
            END;
          END;
        END;
      END;
    END;

    (* Paint the texture *)
    VBT.PaintTexture (v, v.rect, v.op, v.src, v.delta);

    (* Paint a green outline of the clipping region *)
    Rect.Factor (Rect.Inset (v.rect, -1), v.rect, part, 0, 0);
    part[2] := Rect.Empty;
    VBT.PolyTint (v, part, PaintOp.FromRGB (0.0, 1.0, 0.0));

    (* Paint a blue grid marking the texture tiling *)
    WITH vdom = VBT.Domain (v),
         pdom = Rect.Add (PxmpRect, v.delta),
         width = Rect.HorSize (pdom),
         height = Rect.VerSize (pdom),
         nwRect = Rect.Add (Rect.T{0, width, 0, height}, Rect.NorthWest(vdom)),
         init = Rect.Mod (Rect.NorthWest (pdom), nwRect) DO
      Path.Reset (path);
      FOR x := init.h TO vdom.east-1 BY width DO
        Path.MoveTo (path, Point.T {x, vdom.north});
        Path.LineTo (path, Point.T {x, vdom.south - 1});
      END;
      FOR y := init.v TO vdom.south-1 BY height DO
        Path.MoveTo (path, Point.T {vdom.west, y});
        Path.LineTo (path, Point.T {vdom.east-1, y});
      END;
      VBT.Stroke(v, Rect.Full, path, 1, op := PaintOp.FromRGB (0.0, 0.0, 1.0));
    END;

    (* Paint a red crosshair marking delta *)
    Path.Reset (path);
    Path.MoveTo (path, Point.T {v.delta.h - 3, v.delta.v});
    Path.LineTo (path, Point.T {v.delta.h + 3, v.delta.v});
    Path.MoveTo (path, Point.T {v.delta.h, v.delta.v - 3});
    Path.LineTo (path, Point.T {v.delta.h, v.delta.v + 3});
    VBT.Stroke (v, Rect.Full, path, 1, op := PaintOp.FromRGB (1.0, 0.0, 0.0));
  END Repaint;


PROCEDURE FormUpdated (           fv       : FormsVBT.T;
                       <*UNUSED*> name     : TEXT;
                       <*UNUSED*> eventData: REFANY;
                       <*UNUSED*> time     : VBT.TimeStamp) =
  <* FATAL FormsVBT.Error, FormsVBT.Unimplemented *>
  BEGIN
    WITH form = NARROW (fv, Form) DO
      form.v.op := MakePaintOp (FormsVBT.GetChoice (form, "Op1"),
                                FormsVBT.GetChoice (form, "Op2"));
      form.v.bgPat := FormsVBT.GetBoolean (form, "bgPat");
      form.v.delta.h := FormsVBT.GetInteger (form, "deltax");
      form.v.delta.v := FormsVBT.GetInteger (form, "deltay");
      form.v.rect.west := FormsVBT.GetInteger (form, "clipwest");
      form.v.rect.east := FormsVBT.GetInteger (form, "clipeast");
      form.v.rect.north := FormsVBT.GetInteger (form, "clipnorth");
      form.v.rect.south := FormsVBT.GetInteger (form, "clipsouth");
      VBT.Mark (form.v);
    END;      
  END FormUpdated;


PROCEDURE MakePaintOp (choice1, choice2: TEXT): PaintOp.T =
  CONST op = ARRAY OF PaintOp.T {
                 PaintOp.BgBg,
                 PaintOp.BgFg,
                 PaintOp.BgTransparent,
                 PaintOp.BgSwap,
                 PaintOp.FgBg,
                 PaintOp.FgFg,
                 PaintOp.FgTransparent,
                 PaintOp.FgSwap,
                 PaintOp.TransparentBg,
                 PaintOp.TransparentFg,
                 PaintOp.TransparentTransparent,
                 PaintOp.TransparentSwap,
                 PaintOp.SwapBg,
                 PaintOp.SwapFg,
                 PaintOp.SwapTransparent,
                 PaintOp.SwapSwap};
  BEGIN
    WITH a = ORD (Text.GetChar (choice1, 1)) - ORD ('0'),
         b = ORD (Text.GetChar (choice2, 1)) - ORD ('0') DO
      RETURN op[a*4+b];
    END;
  END MakePaintOp;


PROCEDURE Install () =
  <* FATAL FormsVBT.Error, FormsVBT.Unimplemented *>
  VAR
    v  := NEW (Leaf).init ();
    fv := NEW (Form, v := v).initFromRsrc ("form.fv", 
                                           Rsrc.BuildPath (MyBundle.Get ()));
  BEGIN
    FormsVBT.PutChoice (fv, "Op1", "X0");
    FormsVBT.PutChoice (fv, "Op2", "Y1");
    FormsVBT.PutBoolean (fv, "bgPat", v.bgPat);
    FormsVBT.PutInteger (fv, "deltax", v.delta.h);
    FormsVBT.PutInteger (fv, "deltay", v.delta.v);
    FormsVBT.PutInteger (fv, "clipwest", v.rect.west);
    FormsVBT.PutInteger (fv, "clipeast", v.rect.east);
    FormsVBT.PutInteger (fv, "clipnorth", v.rect.north);
    FormsVBT.PutInteger (fv, "clipsouth", v.rect.south);

    FormsVBT.AttachProc (fv, "bgPat", FormUpdated);
    FormsVBT.AttachProc (fv, "Op1", FormUpdated);
    FormsVBT.AttachProc (fv, "Op2", FormUpdated);
    FormsVBT.AttachProc (fv, "deltax", FormUpdated);
    FormsVBT.AttachProc (fv, "deltay", FormUpdated);
    FormsVBT.AttachProc (fv, "clipwest", FormUpdated);
    FormsVBT.AttachProc (fv, "clipeast", FormUpdated);
    FormsVBT.AttachProc (fv, "clipnorth", FormUpdated);
    FormsVBT.AttachProc (fv, "clipsouth", FormUpdated);

    FormsVBT.PutGeneric (fv, "canvas", v);
    Trestle.Install (fv);
    Trestle.AwaitDelete (fv);
  END Install;


BEGIN
  Install ();
END Main.
