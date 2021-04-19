(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman and Stephen Harrison                                    *)
(* Last modified on Tue Jun 22 10:25:24 PDT 1993 by steveg   *)

MODULE MGPaintOp;

IMPORT IntRefTbl, Palette, PaintOp, PlttFrnds, ScrnColorMap, ScrnPaintOp,
       TrestleComm, VBT, VBTRep;

TYPE
  Closure = Palette.OpClosure OBJECT
    rgb: RGB;
  OVERRIDES
    apply := Apply;
  END;

VAR
  mu := NEW(MUTEX);
  (* protects table *)
  table := NEW(IntRefTbl.Default).init();

PROCEDURE New (rgb: RGB): PaintOp.T =
  VAR
    cl := NEW(Closure, rgb := rgb);
    op := Palette.FromOpClosure(cl);
  BEGIN
    LOCK mu DO EVAL table.put(op.op, cl); END;
    RETURN op
  END New;

PROCEDURE RGBTo24BitPixel (rgb: RGB): INTEGER =
  BEGIN
    RETURN ROUND(rgb.r * 255.0) * 256 * 256
             + ROUND(rgb.g * 255.0) * 256 + ROUND(rgb.b * 255.0)
  END RGBTo24BitPixel;

<* FATAL TrestleComm.Failure, ScrnPaintOp.Failure *>
PROCEDURE Apply (cl: Closure; st: VBT.ScreenType): ScrnPaintOp.T =
  VAR
    cmap: ScrnColorMap.T;
    pix : INTEGER;
    t   : ScrnPaintOp.T;
    xrgb : ScrnColorMap.XRGB;
  BEGIN
    IF st.depth = 24 THEN
      t := st.op.opaque(RGBTo24BitPixel(cl.rgb));
    ELSIF st.cmap = NIL THEN
      t := st.op.opaque(1);
    ELSE
      TRY
        cmap := st.cmap.standard();
        pix := cmap.new(1).lo;
      EXCEPT
      | ScrnColorMap.Failure => RETURN st.op.opaque(1);
      END;
      t := st.op.opaque(pix);
      TRY
        cmap.write(ARRAY [0 .. 0] OF
                     ScrnColorMap.Entry{ScrnColorMap.Entry{pix, cl.rgb, xrgb}});
      EXCEPT
      | ScrnColorMap.Failure =>
      END;
    END;
    RETURN t
  END Apply;

PROCEDURE Set (st: VBT.ScreenType; op: PaintOp.T; rgb: RGB) =
  VAR
    cl: Closure;
    ra: REFANY;
    xrgb : ScrnColorMap.XRGB;
  BEGIN
    LOCK mu DO
      EVAL table.get(op.op, ra);
      cl := NARROW(ra, Closure);
    END;
    cl.rgb := rgb;
    IF st.depth = 24 THEN
(*
      VAR po := st.ops[op.op];
      BEGIN
        po.pix := RGBTo24BitPixel(rgb);
      END;
*)
    ELSE
      IF st.cmap # NIL THEN
        VAR
          cmap := st.cmap.standard();
          po   := st.ops[op.op];
        BEGIN
          IF po = NIL OR po = PlttFrnds.noOp THEN
            po := Palette.ResolveOp(st, op)
          END;
          TRY
            cmap.write(
              ARRAY [0 .. 0] OF
                ScrnColorMap.Entry{ScrnColorMap.Entry{po.pix, rgb, xrgb}});
          EXCEPT
          | ScrnColorMap.Failure =>
          END;
        END;
      END;
    END;
  END Set;

PROCEDURE Get (op: PaintOp.T): RGB =
  VAR
    ra: REFANY;
  BEGIN
    LOCK mu DO
      EVAL table.get(op.op, ra);
      RETURN NARROW(ra, Closure).rgb;
    END;
  END Get;

BEGIN
END MGPaintOp.
