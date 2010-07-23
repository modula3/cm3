MODULE t EXPORTS Main;
IMPORT IO, Text, M3Path, Wr, TextUtils;

PROCEDURE T(text: TEXT) =
VAR length := Text.Length(text);
    buf := NEW (REF ARRAY OF CHAR, length);
    t2 := "";
BEGIN
  Text.SetChars(buf^, text);
  t2 := M3Path.FixPath(buf^);
  (* Backward slashes confuse the test harness,
   * so print them as tildes instead.
   *)
  text := TextUtils.SubstChar(text, '\\', '~');
  t2 := TextUtils.SubstChar(t2, '\\', '~');
  IO.Put(text & " => " & t2 & Wr.EOL);
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
  T("a/");
  T("a//");
  T("a/.");
  T("a//.");

  T("abc");
  T("abc/");
  T("abc//");
  T("abc/.");
  T("abc//.");

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

  IO.Put("x in Letters1:" & BoolToText('x' IN Letters1) & Wr.EOL);
  IO.Put("X in Letters1:" & BoolToText('X' IN Letters1) & Wr.EOL);
  IO.Put("1 in Letters1:" & BoolToText('1' IN Letters1) & Wr.EOL);

  IO.Put("x in Letters2:" & BoolToText('x' IN Letters2) & Wr.EOL);
  IO.Put("X in Letters2:" & BoolToText('X' IN Letters2) & Wr.EOL);
  IO.Put("1 in Letters2:" & BoolToText('1' IN Letters2) & Wr.EOL);
END t.
