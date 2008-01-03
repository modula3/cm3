MODULE Confirm EXPORTS Main;

IMPORT Trestle, VBT, TextVBT, RigidVBT, ButtonVBT, BorderedVBT, HVSplit;
IMPORT PaintOp, Pixmap, Axis;
IMPORT ConfirmVBT;

PROCEDURE Yes (self: ButtonVBT.T; READONLY cd: VBT.MouseRec) =
  BEGIN
    Trestle.Delete(main);
  END Yes;

PROCEDURE QuitAction (self: ButtonVBT.T; READONLY cd: VBT.MouseRec) =
  VAR
    msg     := "Do you really wish to quit?";
    confirm := ConfirmVBT.New(msg, Yes);
  BEGIN
    Trestle.Install(confirm)
  END QuitAction;

CONST
  (* sizes for the RigidVBT *)
  horz = 30.0;
  vert = 10.0;                   (* size in millimeters *)
  (* border size for text and button *)
  bsize = 1.5;                   (* size in millimeters *)

VAR
  blue  := PaintOp.FromRGB(0.0, 0.0, 1.0);
  green := PaintOp.FromRGB(0.0, 1.0, 0.0);
  text  := RigidVBT.FromHV(TextVBT.New("Confirm demo"), horz, vert);
  top := BorderedVBT.New(
           text, size := bsize, op := blue, txt := Pixmap.Solid);
  button := ButtonVBT.New(TextVBT.New("Quit"), QuitAction);
  bottom := BorderedVBT.New(
              button, size := bsize, op := green, txt := Pixmap.Gray);
  main := HVSplit.Cons(Axis.T.Ver, top, bottom);

BEGIN
  Trestle.Install(main);
  Trestle.AwaitDelete(main)
END Confirm.
