(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Jan 31 09:42:02 PST 1995 by kalsow  *)
(*      modified on Fri Dec 11 10:26:05 PST 1992 by msm     *)
<*PRAGMA LL*>

MODULE JoinPaintOp;

IMPORT ScrnPaintOp, JoinScreen, Palette, PaintOp, VBTRep, PlttFrnds;

PROCEDURE New(st: JoinScreen.T): Oracle =
  BEGIN
    RETURN NEW(Oracle, st := st)
  END New;

PROCEDURE Resolve (st: JoinScreen.T; op: PaintOp.T) =
  VAR i: INTEGER; t := st.succ(NIL, i);
  BEGIN
    WHILE t # NIL DO EVAL Palette.ResolveOp(t, op); t := st.succ(t, i) END
  END Resolve;

PROCEDURE Apply (           st: JoinScreen.T;
                 <*UNUSED*> cl: Palette.OpClosure;
                            op: PaintOp.T          ): ScrnPaintOp.T =
  VAR res := st.ops[op.op];
  BEGIN
    Resolve(st, op);
    IF res # NIL AND res # PlttFrnds.noOp THEN RETURN res END;
    RETURN NEW(T, id := 2 * op.op + 1, st := st);
  END Apply;

REVEAL
  Oracle = ScrnPaintOp.Oracle BRANDED OBJECT
             st: JoinScreen.T;
           (*
           OVERRIDES
             opaque      := Opaque;
             bgfg        := Bgfg;
             swap        := Swap;
             transparent := Transparent;
             copy        := Copy;
             builtIn     := OpBuiltIn;
           *)
           END;

TYPE T = ScrnPaintOp.T OBJECT st: JoinScreen.T;  END;

(*
PROCEDURE OpBuiltIn(orc: OpOracle;
                    op: PaintOp.Predefined) : ScrnPaintOp.T =
  BEGIN
    IF orc.st.sts = NIL THEN Crash(); END;
    IF orc.st.sts[0].ops[op] = PlttFrnds.noOp THEN
       EVAL Palette.ResolveOp(orc.st.sts[0], PaintOp.T{op});
    END; (* if *)
    RETURN orc.st.sts[0].ops[op];
  END OpBuiltIn;


PROCEDURE Bgfg(orc: OpOracle;
               bg, fg: ScrnPaintOp.T): ScrnPaintOp.T
    RAISES {ScrnPaintOp.Failure} =
  BEGIN
    IF orc.st.sts = NIL THEN Crash(); END;
    RETURN orc.st.sts[0].op.bgfg(bg, fg);
  END Bgfg;


PROCEDURE Copy(orc: OpOracle): ScrnPaintOp.T RAISES {} =
  BEGIN
    IF orc.st.sts = NIL THEN Crash(); END;
    RETURN orc.st.sts[0].op.copy();
  END Copy;


PROCEDURE Opaque(orc: OpOracle; pix: ScrnPaintOp.Pixel)
  : ScrnPaintOp.T RAISES {} =
  BEGIN
    IF orc.st.sts = NIL THEN Crash(); END;
    RETURN orc.st.sts[0].op.opaque(pix);
  END Opaque;

PROCEDURE Swap(orc: OpOracle; p,q: ScrnPaintOp.Pixel)
    : ScrnPaintOp.T RAISES {} =
  BEGIN
    IF orc.st.sts = NIL THEN Crash(); END;
    RETURN orc.st.sts[0].op.swap(p, q);
  END Swap;

PROCEDURE Transparent(orc: OpOracle): ScrnPaintOp.T RAISES {}=
  BEGIN
    IF orc.st.sts = NIL THEN Crash(); END;
    RETURN orc.st.sts[0].op.transparent();
  END Transparent;

EXCEPTION FatalError;

PROCEDURE Crash () =
  <* FATAL FatalError *>
  BEGIN
    RAISE FatalError;
  END Crash;
*)

BEGIN END JoinPaintOp.
