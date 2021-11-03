MODULE Main;
IMPORT Trestle, VBT, TextVBT, ListVBT, HVSplit, RigidVBT, PaintOp, Axis,
       Font, HVBar;

TYPE
  T = ListVBT.UniSelector OBJECT
      OVERRIDES
        insideClick  := InsideClick;
        outsideClick := OutsideClick;
        insideDrag   := InsideDrag;
        outsideDrag  := OutsideDrag;
      END;

PROCEDURE InsideClick (v: T; cd: VBT.MouseRec; this: ListVBT.Cell) =
  BEGIN
    list.selectOnly(this);
    TextVBT.Put(txt1, "inside click");
    TextVBT.Put(txt2, list.getValue(this));
  END InsideClick;

PROCEDURE InsideDrag (v: T; cd: VBT.PositionRec; this: ListVBT.Cell) =
  BEGIN
    list.selectOnly(this);
    TextVBT.Put(txt1, "inside drag");
    TextVBT.Put(txt2, list.getValue(this));
  END InsideDrag;

PROCEDURE OutsideClick (v: T; cd: VBT.MouseRec) =
  BEGIN
    TextVBT.Put(txt1, "outside click");
  END OutsideClick;

PROCEDURE OutsideDrag (v: T; cd: VBT.PositionRec) =
  BEGIN
    TextVBT.Put(txt1, "outside drag");
  END OutsideDrag;

CONST
  times14 = "-*-times-*-r-*-*-14-*-*-*-*-*-*-*";
  strings = ARRAY [1 .. 7] OF
              TEXT{"Africa", "Antartica", "Asia", "Australia", "Europe",
                   "North America", "South America"};
VAR
  font   := Font.FromName(ARRAY OF TEXT{times14});
  colors := PaintOp.MakeColorQuad(PaintOp.Bg, PaintOp.Fg);
  list   := NEW(ListVBT.T, selector := NEW(T).init(NIL)).init(colors);
  txt1   := TextVBT.New("", fnt := font);
  txt2   := TextVBT.New("", fnt := font);
  main := HVSplit.Cons(Axis.T.Ver, RigidVBT.FromHV(list, 30.0, 35.0),
                       HVBar.New(), txt1, HVBar.New(), txt2);
BEGIN
  list.insertCells(at := 0, n := NUMBER(strings)); (* Create 7 new cells *)
  FOR i := FIRST(strings) TO LAST(strings) DO
    list.setValue(i - FIRST(strings), strings[i]); (* Set cell value *)
  END;
  ListVBT.TextPainter.setFont(list.painter, list, font);
  Trestle.Install(main);
  Trestle.AwaitDelete(main)
END Main.
