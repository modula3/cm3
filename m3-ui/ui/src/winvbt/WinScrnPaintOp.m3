(* Copyright (C) 1995, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Tue Aug  6 11:39:03 PDT 1996 by najork                   *)
(*       Created on Tue Jan 17 16:48:42 PST 1995 by najork                   *)


MODULE WinScrnPaintOp;

IMPORT PaintOp, ScreenType, ScrnPaintOp, TrestleClass, VBTClass, WinDef, 
       WinGDI, WinScreenType, WinScreenTypePrivate, Word;


PROCEDURE NewOracle (st: WinScreenType.T): ScrnPaintOp.Oracle =
  BEGIN
    RETURN NEW(Oracle, st := st)
  END NewOracle;


TYPE
  Oracle = ScrnPaintOp.Oracle BRANDED OBJECT
    st: WinScreenType.T;
  OVERRIDES
    opaque      := Opaque;
    bgfg        := BgFg;
    swap        := Swap;
    transparent := Transparent;
    copy        := Copy;
    builtIn     := BuiltIn;    
  END;


PROCEDURE Opaque (self: Oracle; pix: ScrnPaintOp.Pixel): ScrnPaintOp.T =
  BEGIN
    RETURN NewPaintOp (self.st, Op {Mode.Opaq, pix}, Op {Mode.Opaq, pix}, pix);
  END Opaque;


PROCEDURE Transparent (self: Oracle): ScrnPaintOp.T =
  BEGIN
    RETURN NewPaintOp (self.st, Op {Mode.Tran, 0}, Op {Mode.Tran, 0});
  END Transparent;


PROCEDURE Copy (self: Oracle): ScrnPaintOp.T =
  BEGIN
    RETURN NewPaintOp (self.st, Op {Mode.Copy, 0}, Op {Mode.Copy, 0});
  END Copy;


PROCEDURE Swap (self: Oracle; p, q: ScrnPaintOp.Pixel): ScrnPaintOp.T =
  VAR 
    pix := Word.Xor(p, q);
(*    pix := 16_00000000; *) (* expected behavior - no-op *)
(*    pix := 16_00FFFFFF; *) (* white to black, red to green (but not cyan) *)
(*    pix := 16_02FFFFFF; *) (* white turns green; red turns blue *)
  BEGIN
    (* "p = q" is a special case, which can be handled 
       much more efficiently by calling "Transparent". *)
    IF p = q THEN 
      RETURN Transparent(self);  
    END;
    RETURN NewPaintOp (self.st, Op {Mode.Swap, pix}, Op {Mode.Swap, pix});
  END Swap;


PROCEDURE BgFg (self: Oracle; bg, fg: ScrnPaintOp.T): ScrnPaintOp.T
    RAISES {ScrnPaintOp.Failure} =

  PROCEDURE IsTint (rec: OpRecord): BOOLEAN =
    BEGIN
      RETURN rec.bop = rec.fop AND NOT rec.bop.mode = Mode.Copy;
    END IsTint;

  VAR
    st := self.st;
  BEGIN
    LOCK st.trsl DO
      IF bg.id < 0 OR bg.id >= st.opcount OR NOT IsTint(st.optable[bg.id]) OR 
         fg.id < 0 OR fg.id >= st.opcount OR NOT IsTint(st.optable[fg.id]) THEN
        RAISE ScrnPaintOp.Failure;
      END;
    END;
    RETURN NewPaintOp(st, st.optable[bg.id].bop, st.optable[fg.id].fop);
  END BgFg;


PROCEDURE BuiltIn (self: Oracle; op: PaintOp.Predefined): ScrnPaintOp.T =
  VAR 
    b    := self.st.bg;
    f    := self.st.fg;
    back := Op {Mode.Opaq, b};
    fore := Op {Mode.Opaq, f};
    swap := Op {Mode.Swap, Word.Xor (b, f)};
    tran := Op {Mode.Tran, 0};
  BEGIN
    CASE op OF
    | PaintOp.Bg.op              => RETURN NewPaintOp (self.st, back, back, b);
    | PaintOp.BgFg.op            => RETURN NewPaintOp (self.st, back, fore);
    | PaintOp.BgTransparent.op   => RETURN NewPaintOp (self.st, back, tran);
    | PaintOp.BgSwap.op          => RETURN NewPaintOp (self.st, back, swap);
    | PaintOp.FgBg.op            => RETURN NewPaintOp (self.st, fore, back);
    | PaintOp.Fg.op              => RETURN NewPaintOp (self.st, fore, fore, f);
    | PaintOp.FgTransparent.op   => RETURN NewPaintOp (self.st, fore, tran);
    | PaintOp.FgSwap.op          => RETURN NewPaintOp (self.st, fore, swap);
    | PaintOp.TransparentBg.op   => RETURN NewPaintOp (self.st, tran, back);
    | PaintOp.TransparentFg.op   => RETURN NewPaintOp (self.st, tran, fore);
    | PaintOp.Transparent.op     => RETURN NewPaintOp (self.st, tran, tran);
    | PaintOp.TransparentSwap.op => RETURN NewPaintOp (self.st, tran, swap);
    | PaintOp.SwapBg.op          => RETURN NewPaintOp (self.st, swap, back);
    | PaintOp.SwapFg.op          => RETURN NewPaintOp (self.st, swap, fore);
    | PaintOp.SwapTransparent.op => RETURN NewPaintOp (self.st, swap, tran);
    | PaintOp.Swap.op            => RETURN NewPaintOp (self.st, swap, swap);
    | PaintOp.Copy.op            => RETURN Copy(self);
    END;
  END BuiltIn;


PROCEDURE ToBinaryRasterOp (bop, fop: Op): INTEGER =
  BEGIN
    CASE bop.mode OF
    | Mode.Copy =>
      CASE fop.mode OF
      | Mode.Copy => RETURN WinGDI.R2_COPYPEN;
      | Mode.Tran => RETURN WinGDI.R2_NOP;  (* illegal combination *)
      | Mode.Opaq => RETURN WinGDI.R2_NOP;  (* illegal combination *)
      | Mode.Swap => RETURN WinGDI.R2_NOP;  (* illegal combination *)
      END;
    | Mode.Tran =>
      CASE fop.mode OF
      | Mode.Copy => RETURN WinGDI.R2_NOP;  (* illegal combination *)
      | Mode.Tran => RETURN WinGDI.R2_NOP;
      | Mode.Opaq => RETURN WinGDI.R2_COPYPEN;
      | Mode.Swap => RETURN WinGDI.R2_XORPEN;
      END;
    | Mode.Opaq =>
      CASE fop.mode OF
      | Mode.Copy => RETURN WinGDI.R2_NOP;  (* illegal combination *)
      | Mode.Tran => RETURN WinGDI.R2_NOP;  
      | Mode.Opaq => RETURN WinGDI.R2_COPYPEN;
      | Mode.Swap => RETURN WinGDI.R2_XORPEN;
      END;
    | Mode.Swap =>
      CASE fop.mode OF
      | Mode.Copy => RETURN WinGDI.R2_NOP;  (* illegal combination *)
      | Mode.Tran => RETURN WinGDI.R2_NOP;  
      | Mode.Opaq => RETURN WinGDI.R2_COPYPEN;
      | Mode.Swap => RETURN WinGDI.R2_XORPEN;
      END;
    END;
  END ToBinaryRasterOp;

(******
CONST
  R3_NOOP = 16_00AA0029;
*****)

PROCEDURE ToBgTernaryRasterOp (op: Op): WinDef.DWORD =
  BEGIN
    CASE op.mode OF
    | Mode.Opaq => RETURN 16_00B8074A;
    | Mode.Tran => RETURN 0;
    | Mode.Swap => RETURN 16_009A0709;
    | Mode.Copy => RETURN WinGDI.SRCCOPY;
    END;
  END ToBgTernaryRasterOp;


PROCEDURE ToFgTernaryRasterOp (op: Op): WinDef.DWORD =
  BEGIN
    CASE op.mode OF
    | Mode.Opaq => RETURN 16_00E20746;
    | Mode.Tran => RETURN 0;
    | Mode.Swap => RETURN 16_006A01E9;
    | Mode.Copy => RETURN 0;
    END;
  END ToFgTernaryRasterOp;


PROCEDURE NewPaintOp (VAR      st  : WinScreenType.T;
                      READONLY bop : Op;
                      READONLY fop : Op;
                               pix := -1): ScrnPaintOp.T =
  VAR 
    res := NEW(ScrnPaintOp.T, pix := pix);
    rec : OpRecord;
  BEGIN
    rec.bop   := bop;
    rec.fop   := fop;
    rec.rop2  := ToBinaryRasterOp  (bop, fop);
    rec.brop3 := ToBgTernaryRasterOp (bop);
    rec.frop3 := ToFgTernaryRasterOp (fop);

    LOCK st.trsl DO
      WITH n = NUMBER(st.optable^) DO
        IF n = st.opcount THEN
          WITH new = NEW(REF ARRAY OF OpRecord, 2 * n) DO
            SUBARRAY (new^, 0, n) := st.optable^;
            st.optable := new;
          END;
        END;
      END;
      res.id := st.opcount;
      st.optable[res.id] := rec;
      INC(st.opcount);
    END;
    RETURN res
  END NewPaintOp;


BEGIN
END WinScrnPaintOp.
