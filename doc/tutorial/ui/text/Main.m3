MODULE Main;
IMPORT Trestle, TextVBT, RigidVBT, Font, PaintOp;
IMPORT Latin1Key, Text;          (* Standard Modula-3 library modules. *)

CONST
  ha = 0.0;                      (* horizonal alignment; left justified *)
  va = 0.4;                      (* vertical alignment; 4/10 toward the
                                    bottom *)
  hm = 3.0;                      (* horizonal margin (millimeters) *)
  vm = 5.0;                      (* vertical margin (millimeters) *)
  courier18 = "-*-courier-bold-r-*-*-18-*-*-*-*-*-*-*";
VAR
  white := PaintOp.FromRGB(1.0, 1.0, 1.0);
  blue  := PaintOp.FromRGB(0.0, 0.0, 0.8);
  quad  := PaintOp.MakeColorQuad(bg := blue, fg := white);
  qd    := Text.FromChar(VAL(Latin1Key.questiondown, CHAR));
  ea    := Text.FromChar(VAL(Latin1Key.eacute, CHAR));
  text  := qd & "Qu" & ea & " pasa?";
  font  := Font.FromName(ARRAY OF TEXT{courier18});
  main := TextVBT.New(
            txt := text, halign := ha, valign := va, hmargin := hm,
            vmargin := vm, fnt := font, bgFg := quad);
BEGIN
  Trestle.Install(main);
  Trestle.AwaitDelete(main);
END Main.
