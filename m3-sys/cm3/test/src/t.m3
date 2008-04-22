MODULE t EXPORTS Main;
IMPORT RTIO, Text, M3Path;

PROCEDURE T(text: TEXT) =
VAR length := Text.Length(text);
    buf := NEW (REF ARRAY OF CHAR, length);
    t2 := "";
BEGIN
  FOR i := 0 TO 1 DO
     M3Path.SetOS(ARRAY OF M3Path.OSKind {M3Path.OSKind.Unix, M3Path.OSKind.Win32}[i], host := TRUE);
     Text.SetChars(buf^, text);
     t2 := M3Path.FixPath(buf^);
     RTIO.PutText(ARRAY OF TEXT {"Unix", "Win32"}[i] & " " &  text & " => " & t2 & "\n");
  END;
END T;

CONST Letters1 = SET OF CHAR {'a' .. 'z', 'A' .. 'Z'};
CONST Letters2 = SET OF CHAR {'A' .. 'Z', 'a' .. 'z'};

PROCEDURE BoolToText(a: BOOLEAN) : TEXT =
  BEGIN
    RETURN ARRAY OF TEXT{"FALSE", "TRUE"}[ORD(a)];
  END BoolToText;

BEGIN
  T("");
  T("a");
  T("/");
  T("/foo");
  T("/foo/bar");
  T("c:\\");
  T("c:\\foo");
  T("c:\\foo//bar");
  T("c:\\foo//bar\\\\///");
  T("c:\\foo//\\\\bar\\\\///");
  T("c:///\\foo//\\\\bar\\\\///");
  T("/cygdrive/c/foo");
  T("/cygdrive/1/foo");
  T("\\cygdrive\\c\\foo");
  T("\\cygdrive\\1\\foo");
  T("///");
  T("//");
  T("\\\\");
  T("\\\\\\");
  T("//foo//bar////");

  RTIO.PutText("x in Letters1:" & BoolToText('x' IN Letters1) & "\n");
  RTIO.PutText("X in Letters1:" & BoolToText('X' IN Letters1) & "\n");
  RTIO.PutText("1 in Letters1:" & BoolToText('1' IN Letters1) & "\n");

  RTIO.PutText("x in Letters2:" & BoolToText('x' IN Letters2) & "\n");
  RTIO.PutText("X in Letters2:" & BoolToText('X' IN Letters2) & "\n");
  RTIO.PutText("1 in Letters2:" & BoolToText('1' IN Letters2) & "\n");

  RTIO.Flush();
END t.
