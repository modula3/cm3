MODULE Menu EXPORTS Main;
IMPORT Trestle, VBT, TextVBT, RigidVBT, HVSplit;
IMPORT ButtonVBT, MenuBtnVBT, AnchorBtnVBT, BorderedVBT;
IMPORT PaintOp, Axis, Wr, Stdio;

PROCEDURE OpenAction (v: ButtonVBT.T; READONLY cd: VBT.MouseRec) =
  BEGIN
    Wr.PutText(Stdio.stdout, "Open\n");
  END OpenAction;

PROCEDURE CloseAction (v: ButtonVBT.T; READONLY cd: VBT.MouseRec) =
  BEGIN
    Wr.PutText(Stdio.stdout, "Close\n");
  END CloseAction;

PROCEDURE CutAction (v: ButtonVBT.T; READONLY cd: VBT.MouseRec) =
  BEGIN
    Wr.PutText(Stdio.stdout, "Cut\n");
  END CutAction;

PROCEDURE PasteAction (v: ButtonVBT.T; READONLY cd: VBT.MouseRec) =
  BEGIN
    Wr.PutText(Stdio.stdout, "Paste\n");
  END PasteAction;

CONST
  horz = 30.0;
  vert = 10.0;                   (* size of main window in millimeters *)
VAR
  (* build up the menu items for pull down menu1 *)
  open  := MenuBtnVBT.TextItem("Open", OpenAction);
  close := MenuBtnVBT.TextItem("Close", CloseAction);
  (* build up the pull down menu1 *)
  menu1 := BorderedVBT.New(HVSplit.Cons(Axis.T.Ver, open, close));
  a1    := AnchorBtnVBT.New(ch := TextVBT.New("File"), menu := menu1);
  (* build up the menu items for pull down menu2 *)
  cut   := MenuBtnVBT.TextItem("Cut", CutAction);
  paste := MenuBtnVBT.TextItem("Paste", PasteAction);
  (* build up the pulldown menu2 *)
  menu2 := BorderedVBT.New(HVSplit.Cons(Axis.T.Ver, cut, paste));
  a2    := AnchorBtnVBT.New(ch := TextVBT.New("Edit"), menu := menu2);
  (* Build up the menu bar containing the two anchor buttons *)
  blue := PaintOp.FromRGB(0.0, 0.0, 1.0);
  bar  := ButtonVBT.MenuBar(a1, a2, op := blue);
  text := RigidVBT.FromHV(TextVBT.New("Menu demo"), horz, vert);
  main := HVSplit.Cons(Axis.T.Ver, bar, text);
BEGIN
  Trestle.Install(main);
  Trestle.AwaitDelete(main);
END Menu.

