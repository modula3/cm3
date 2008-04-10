MODULE t EXPORTS Main;
IMPORT RTIO, Text, M3Path, Process;

PROCEDURE T(text: TEXT) =
VAR length := Text.Length(text);
    buf := NEW (REF ARRAY OF CHAR, length);
    t1 := "";
    t2 := "";
BEGIN
  FOR i := 0 TO 1 DO
     M3Path.SetOS(ARRAY OF M3Path.OSKind {M3Path.OSKind.Unix, M3Path.OSKind.Win32}[i], host := TRUE);
     t1 := M3Path.PathLooselyConvertUserInputToHost_TextToText(text);
     Text.SetChars(buf^, text);
     t2 := M3Path.PathLooselyConvertUserInputToHost_ArrayToText(buf^);
     RTIO.PutText(ARRAY OF TEXT {"Unix", "Win32"}[i] & " " &  text & " => " & t1 & "\n");
     RTIO.PutText(ARRAY OF TEXT {"Unix", "Win32"}[i] & " " &  text & " => " & t2 & "\n");
     IF NOT Text.Equal (t1, t2) THEN
       RTIO.PutText("ERROR " & t1 & " " & t2 & "\n");
       RTIO.Flush();
       Process.Exit (1);
     END;
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

  FOR i := 0 TO NUMBER(M3Path.PathVariableNames) - 1 DO
    WITH a = M3Path.PathVariableNames[i],
         b = M3Path.IsPathVariableName(a) DO
      IF NOT b THEN
         RTIO.PutText("ERROR IsPathVariableName " & a & "\n");
         RTIO.Flush();
         Process.Exit (1);
      ELSE
        RTIO.PutText("IsPathVariableName(" & a & ") => " & BoolToText(b) & "\n");
      END;
    END;
  END;
  CONST x = ARRAY OF TEXT {
  "",
  "1",
  "12",
  "123",
  "1324",
  "1245",
  "123456",
  "1234567",
  "12345678",
  "123456789",
  "1234567890",
  "12345678901",
  "123456789012",
  "1234567890123",
  "12345678901234",
  "123456789012345",
  "1234567890123456",
  "12345678901234567",
  "CM3_ROOT"
  };
  BEGIN
    FOR i := 0 TO NUMBER(x) - 1 DO
      WITH a = M3Path.IsPathVariableName(x[i]) DO
        RTIO.PutText("IsPathVariableName(" & x[i] & ") => " & BoolToText(a) & "\n");
      END;
    END;
  END;

  RTIO.PutText("x in Letters1:" & BoolToText('x' IN Letters1) & "\n");
  RTIO.PutText("X in Letters1:" & BoolToText('X' IN Letters1) & "\n");
  RTIO.PutText("1 in Letters1:" & BoolToText('1' IN Letters1) & "\n");

  RTIO.PutText("x in Letters2:" & BoolToText('x' IN Letters2) & "\n");
  RTIO.PutText("X in Letters2:" & BoolToText('X' IN Letters2) & "\n");
  RTIO.PutText("1 in Letters2:" & BoolToText('1' IN Letters2) & "\n");

  RTIO.Flush();
END t.
