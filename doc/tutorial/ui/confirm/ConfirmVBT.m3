MODULE ConfirmVBT;

IMPORT Trestle, VBT, TextVBT, ButtonVBT, BorderedVBT, HVSplit, Split;
IMPORT PaintOp, Pixmap, Axis;

CONST
  bsize1 = 5.0;                  (* Border Size in millimeters *)
  bsize2 = 1.5;                  (* Border Size in millimeters *)

(* Delete the dialog box under the assumption it has been installed. *)
PROCEDURE DeleteDialog (v: ButtonVBT.T; READONLY cd: VBT.MouseRec) =
  BEGIN
    Trestle.Delete(VBT.Parent(VBT.Parent(VBT.Parent(VBT.Parent(v)))));
  END DeleteDialog;

PROCEDURE Init (self   : T;
                message: TEXT;
                yaction         := DeleteDialog;
                caction         := DeleteDialog  ): T =
  VAR
    red := PaintOp.FromRGB(1.0, 0.0, 0.0);
    ch0 := BorderedVBT.New(TextVBT.New(message), size := bsize1,
                           txt := Pixmap.Solid, op := PaintOp.Bg);
    split := HVSplit.Cons(
               Axis.T.Hor,
               BorderedVBT.New(ButtonVBT.New(TextVBT.New("Yes"), yaction)),
               BorderedVBT.New(
                 ButtonVBT.New(TextVBT.New("Cancel"), caction)));
    ch1 := BorderedVBT.New(
             split, size := bsize2, op := red, txt := Pixmap.Solid);
  BEGIN
    EVAL HVSplit.T.init(self, Axis.T.Ver);
    Split.AddChild(self, ch0, ch1);
    RETURN self;
  END Init;

PROCEDURE New (message: TEXT;
               yaction         := DeleteDialog;
               caction         := DeleteDialog  ): T =
  BEGIN
    RETURN Init(NEW(T), message, yaction, caction)
  END New;

BEGIN
END ConfirmVBT.
