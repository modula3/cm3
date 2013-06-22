MODULE QtRgb;

IMPORT Word;


PROCEDURE qRed (rgb: CARDINAL;
): INTEGER =
BEGIN
RETURN Word.And(Word.RightShift(rgb,16),16_FF);
END qRed;

PROCEDURE qGreen (rgb: CARDINAL;
): INTEGER =
BEGIN
RETURN Word.And(Word.RightShift(rgb,8),16_FF);
END qGreen;

PROCEDURE qBlue (rgb: CARDINAL;
): INTEGER =
BEGIN
RETURN Word.And(rgb,16_FF);
END qBlue;

PROCEDURE qAlpha (rgb: CARDINAL;
): INTEGER =
BEGIN
RETURN Word.And(Word.RightShift(rgb,24),16_FF);
END qAlpha;

PROCEDURE qRgb (r, g, b: INTEGER;
): CARDINAL =
VAR
 t1,t2,t3,t4 : Word.T;
BEGIN
  t1 := Word.LeftShift(16_FF,24);
  t2 := Word.LeftShift(Word.And(r,16_FF),16);
  t3 := Word.LeftShift(Word.And(g,16_FF),8);
  t4 := Word.And(b,16_FF);
RETURN Word.Or(Word.Or(Word.Or(t3,t4),t2),t1);
END qRgb;

PROCEDURE qRgba (r, g, b, a: INTEGER;
): CARDINAL =
VAR
 t1,t2,t3,t4 : Word.T;
BEGIN
  t1 := Word.LeftShift(Word.And(a,16_FF),24);
  t2 := Word.LeftShift(Word.And(r,16_FF),16);
  t3 := Word.LeftShift(Word.And(g,16_FF),8);
  t4 := Word.And(b,16_FF);
RETURN Word.Or(Word.Or(Word.Or(t3,t4),t2),t1);
END qRgba;

PROCEDURE qGray (r, g, b: INTEGER;
): INTEGER =
BEGIN
RETURN (r*11+g*16+b*5) DIV 32;
END qGray;

PROCEDURE qGray1 (rgb: CARDINAL;
): INTEGER =
BEGIN
RETURN qGray(qRed(rgb), qGreen(rgb), qBlue(rgb));
END qGray1;

PROCEDURE qIsGray (rgb: CARDINAL;
): BOOLEAN =
BEGIN
RETURN (qRed(rgb) = qGreen(rgb)) AND (qRed(rgb) = qBlue(rgb));
END qIsGray;


BEGIN
END QtRgb.
