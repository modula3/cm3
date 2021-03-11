MODULE WxDefault EXPORTS Wx;
IMPORT Fmt;
IMPORT TextWr;
IMPORT Wr;
REVEAL
  T = TextWr.T BRANDED "WxDefault" OBJECT END;

PROCEDURE New (): T =
  BEGIN RETURN NEW(T).init(); END New;
PROCEDURE PutChar (t: T;  ch: CHAR) =
  BEGIN Wr.PutChar(t, ch); END PutChar;
PROCEDURE PutText (t: T;  txt: TEXT) =
  BEGIN Wr.PutText(t, txt); END PutText;
PROCEDURE PutInt  (t: T;  i: INTEGER) =
  BEGIN Wr.PutText(t, Fmt.Int(i)); END PutInt;
PROCEDURE PutStr  (t: T;  READONLY x: ARRAY OF CHAR) =
  BEGIN Wr.PutString(t, x); END PutStr;

PROCEDURE ToText   (t: T): TEXT =
  BEGIN RETURN TextWr.ToText(t); END ToText;

BEGIN
END WxDefault.
