(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Wed Mar 27 19:45:20 PST 1996 by heydon                   *)
<* PRAGMA SPEC *>

MODULE JunoZeus;

IMPORT JunoConfig, CurrCmd, Drawing, FVFilter, RemoteView, View, PSImpl;
IMPORT JunoScope, JunoAST;
IMPORT JunoRT, JunoArgs, JunoValue, RTVal;
IMPORT FormsVBT;
IMPORT VBT, Filter, Rect, Region, PaintOp, DblBufferVBT;
IMPORT Atom, RefList, Sx, Thread, TextRd, Rd;

REVEAL
  T = Public BRANDED "JunoZeus.T" OBJECT
    fv: FormsVBT.T;
    rt: View.Root;
    tFactor := -1.0;
    tFactorSlot: CARDINAL
  OVERRIDES
    init := Init;
    startrun := StartRun;
    endrun := EndRun;
    event := Event
  END;

TYPE
  AnimChild = Drawing.ChildWriteOnly OBJECT OVERRIDES
    repaint := RepaintAnimChild;
  END;

<* SPEC RepaintAnimChild REQUIRES sup(LL) = VBT.mu.v *>

PROCEDURE RepaintAnimChild(ch: AnimChild; READONLY rgn: Region.T) =
  BEGIN
    VBT.PaintRegion(ch, rgn, PaintOp.Bg);
    Drawing.Sync(ch)
  END RepaintAnimChild;

PROCEDURE Init(
    jz: T;
    fv: FormsVBT.T;
    rt: View.Root;
    origin: JunoConfig.Origin)
  : T =
  BEGIN
    jz.fv := fv;
    jz.rt := rt;
    rt.animView := NEW(PSImpl.T).init(NEW(AnimChild).init(origin), rt); 
    jz.w := NEW(DblBufferVBT.T).init(rt.animView);
    RETURN jz
  END Init;

PROCEDURE StartRun(jz: T) =
  VAR d := jz.rt.animView; BEGIN
    jz.rt.currView := d;
    FVFilter.MakePassive(jz.fv, "background");
    JunoRT.Interrupt();
    PSImpl.Reset(d, inExec := FALSE);
    VBT.PaintTint(Filter.Child(d), Rect.Full, PaintOp.Bg)
  END StartRun;

PROCEDURE EndRun(jz: T) =
  BEGIN
    FVFilter.MakeActive(jz.fv, "background");
    jz.rt.currView := jz.rt.drawing;
  END EndRun;

PROCEDURE Event(jz: T; tFactor: REAL; nm, args: TEXT)
    RAISES {RemoteView.Error, Thread.Alerted} =
  VAR
    ent := JunoScope.Lookup(CurrCmd.GetScope(jz.rt.ccmd), Atom.FromText(nm));
  BEGIN
    TYPECASE ent OF
      NULL => RAISE RemoteView.Error("Unknown event procedure \"" & nm & "\"")
    | JunoScope.Proc (p) => 
        VAR argList: RefList.T; len: CARDINAL; BEGIN
          TRY argList := Sx.Read(TextRd.New(args)) EXCEPT
            Sx.ReadError, Rd.EndOfFile =>
              RAISE RemoteView.Error("Invalid Sx argument to event")
          END;
          len := RefList.Length(argList);
          (* check signature *)
          IF p.out_cnt # 0 OR p.inout_cnt # 0 THEN
            RAISE RemoteView.Error("Event procedure \"" & nm &
              "\" is not allowed to have OUT or INOUT parameters")
          ELSIF p.in_cnt # len THEN
            RAISE RemoteView.Error("Wrong number of IN parameters to \""
              & nm & "\" event procedure")
          ELSIF p.external THEN
            RAISE RemoteView.Error("Event procedure \"" & nm
              & "\" is not user-defined")
          END;
          (* simulate call *)
          JunoRT.ResetMachine();
          IF tFactor # jz.tFactor THEN
            IF jz.tFactor = -1.0 THEN
              VAR ent: JunoScope.Proc; dummy: JunoScope.Entity; BEGIN
                ent := JunoScope.LookupQId(CurrCmd.GetScope(jz.rt.ccmd),
                  NEW(JunoAST.QId, id0 := Atom.FromText("Anim"),
                    id1 := Atom.FromText("SetTFactor")), dummy);
                jz.tFactorSlot := ent.index
              END
            END;
            jz.tFactor := tFactor;
            JunoArgs.PushValue(RTVal.FromReal(tFactor));
            RunSlot(jz.tFactorSlot)
          END;
          WHILE argList # NIL DO
            JunoArgs.PushValue(SxToRTVal(argList.head));
            argList := argList.tail
          END;
          RunSlot(p.index)
        END
    ELSE RAISE RemoteView.Error("Event \"" & nm & "\" is not a procedure")
    END
  END Event;

PROCEDURE RunSlot(slot: CARDINAL) RAISES {RemoteView.Error} =
  VAR execRes := JunoRT.ExecFromSlot(slot, reset := FALSE); BEGIN
    JunoRT.ResetMachine();
    IF execRes.trapCode # JunoRT.TrapCode.NormalHalt THEN
      RAISE RemoteView.Error(JunoRT.TrapMessage(execRes))
    END
  END RunSlot;

PROCEDURE SxToRTVal(sx: Sx.T): RTVal.T RAISES {RemoteView.Error} =
  BEGIN
    TYPECASE sx OF
      NULL => RETURN RTVal.nil
    | TEXT (t) => RETURN RTVal.FromText(t)
    | REF INTEGER (i) =>  RETURN RTVal.FromInt(i^)
    | REF REAL (r) =>     RETURN RTVal.FromReal(r^)
    | REF LONGREAL (r) => RETURN RTVal.FromReal(FLOAT(r^, JunoValue.Real))
    | REF EXTENDED (r) => RETURN RTVal.FromReal(FLOAT(r^, JunoValue.Real))
    | RefList.T (p) =>
        RETURN RTVal.FromPair(SxToRTVal(p.head), SxToRTVal(p.tail))
    ELSE
      RAISE RemoteView.Error("Illegal Juno value in event argument")
    END
  END SxToRTVal;

BEGIN
END JunoZeus.
