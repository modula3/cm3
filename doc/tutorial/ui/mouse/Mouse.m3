MODULE Mouse EXPORTS Main;
IMPORT Trestle, VBT, TextVBT, RigidVBT, HVSplit, BorderedVBTASBdVBT;
IMPORT Axis, Region, PaintOp, Fmt;
FROM VBT IMPORT Modifier, ClickType; (* used in MouseEvent *)

TYPE
  MouseVBT =
    VBT.Leaf OBJECT
      whatChanged, time, cp, modifiers, clickType, clickCount: TextVBT.T;
    METHODS
      init (): MouseVBT := Init;
    OVERRIDES
      mouse   := MouseEvent;
      repaint := Repaint;        (* Leaf repaint does nothing *)
    END;

PROCEDURE Init (v: MouseVBT): MouseVBT =
  BEGIN
    v.whatChanged := TextVBT.New(" ");
    v.time := TextVBT.New(" ");
    v.cp := TextVBT.New(" ");    (* cursor postion *)
    v.modifiers := TextVBT.New(" ");
    v.clickType := TextVBT.New(" ");
    v.clickCount := TextVBT.New(" "); (* "double click", etc *)
    RETURN v;
  END Init;

PROCEDURE MouseEvent (v: MouseVBT; READONLY cd: VBT.MouseRec) =
  VAR
    point := cd.cp.pt;
    mods  := cd.modifiers;
    text  := "";
  BEGIN
    CASE cd.whatChanged OF
    | Modifier.MouseL => TextVBT.Put(v.whatChanged, "Left Button");
    | Modifier.MouseM => TextVBT.Put(v.whatChanged, "Middle Button");
    | Modifier.MouseR => TextVBT.Put(v.whatChanged, "Right Button");
    END;
    TextVBT.Put(v.time, Fmt.Unsigned(cd.time)); (* really Word.T *)
    TextVBT.Put(
      v.cp, "(" & Fmt.Int(point.h) & "," & Fmt.Int(point.v) & ")");
    IF (Modifier.Shift IN mods) THEN text := text & "+Shift"; END;
    IF (Modifier.Lock IN mods) THEN text := text & "+Lock"; END;
    IF (Modifier.Control IN mods) THEN text := text & "+Control"; END;
    IF (Modifier.Option IN mods) THEN text := text & "+Option"; END;
    TextVBT.Put(v.modifiers, text);
    CASE cd.clickType OF
    | ClickType.FirstDown => TextVBT.Put(v.clickType, "FirstDown");
    | ClickType.OtherDown => TextVBT.Put(v.clickType, "OtherDown");
    | ClickType.OtherUp => TextVBT.Put(v.clickType, "OtherUp");
    | ClickType.LastUp => TextVBT.Put(v.clickType, "LastUp");
    END;
    TextVBT.Put(v.clickCount, Fmt.Int(cd.clickCount));
  END MouseEvent;

PROCEDURE Repaint (v: MouseVBT; READONLY rgn: Region.T) =
  BEGIN
    VBT.PaintRegion(v, rgn, PaintOp.Bg);
  END Repaint;

CONST
  height = 35.0;                 (* Height of vbt1, vbt2, vbt3 (in
                                    millimeters). *)
VAR
  mousevbt := NEW(MouseVBT).init();
  vbt1     := RigidVBT.FromHV(mousevbt, 20.0, 35.0);
  split1 := HVSplit.Cons(
              Axis.T.Ver, BdVBT.New(TextVBT.New("what changed")),
              BdVBT.New(TextVBT.New("time")),
              BdVBT.New(TextVBT.New("position")),
              BdVBT.New(TextVBT.New("modifiers")),
              BdVBT.New(TextVBT.New("click type")),
              BdVBT.New(TextVBT.New("click count")));
  vbt2 := RigidVBT.FromHV(split1, 20.0, 35.0);
  split2 := HVSplit.Cons(
              Axis.T.Ver, BdVBT.New(mousevbt.whatChanged),
              BdVBT.New(mousevbt.time), BdVBT.New(mousevbt.cp),
              BdVBT.New(mousevbt.modifiers), BdVBT.New(mousevbt.clickType),
              BdVBT.New(mousevbt.clickCount));
  vbt3 := RigidVBT.FromHV(split2, 35.0, 35.0);
  main := HVSplit.Cons(Axis.T.Hor, vbt1, vbt2, vbt3);
BEGIN
  Trestle.Install(main);
  Trestle.AwaitDelete(main);
END Mouse.
