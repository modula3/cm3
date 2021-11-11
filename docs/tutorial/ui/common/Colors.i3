INTERFACE Colors;

IMPORT Color, PaintOp;

PROCEDURE FromName (name: TEXT): PaintOp.T;
PROCEDURE FromRec (color: Color.T): PaintOp.T;

VAR
  white, royal, navy: PaintOp.T;
  red, lred, dred   : PaintOp.T;
  blue, lblue, dblue: PaintOp.T;
  grey, lgrey, dgrey: PaintOp.T;

END Colors.
