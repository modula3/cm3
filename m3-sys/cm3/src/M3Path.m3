(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Mon Sep 26 09:07:50 PDT 1994 by kalsow     *)
(*      modified on Mon Oct 25 10:31:06 PDT 1993 by mcjones    *)
(*      modified on Wed May 12 16:56:05 PDT 1993 by meehan     *)
(*      modified on Mon May 10 20:58:46 PDT 1993 by mjordan    *)

MODULE M3Path;

IMPORT Pathname, Text;
IMPORT RTIO, Process;

CONST
  Null      = '\000';
  Colon     = ':';
  Slash     = '/';
  BackSlash = '\\';
  DirSep = ARRAY OSKind OF CHAR { Slash,  Slash,  BackSlash };
  VolSep = ARRAY OSKind OF CHAR { Null,   Null,   Colon  };
  DirSepText = ARRAY OSKind OF TEXT { "/",  "/",  "\\" };

TYPE
  SMap = ARRAY Kind OF TEXT;

CONST
  Suffix = ARRAY OSKind OF SMap {
  (* Unix *)       SMap { "", ".i3", ".ic", ".is", ".io",
                          ".m3", ".mc", ".ms", ".mo",
                          ".ig", ".mg", ".c", ".h", ".s",
                          ".o", ".a", ".a", ".m3x", "", ".mx", ".tmpl" },
  (* GrumpyUnix *) SMap { "", ".i3", ".ic", ".is", "_i.o",
                          ".m3", ".mc", ".ms", "_m.o",
                          ".ig", ".mg", ".c", ".h", ".s",
                          ".o", ".a", ".a", ".m3x", "", ".mx", ".tmpl" },
  (* Win32 *)      SMap { "", ".i3", ".ic", ".is", ".io",
                          ".m3", ".mc", ".ms", ".mo",
                          ".ig", ".mg", ".c", ".h", ".s",
                          ".obj",".lib",".lib",".m3x",".exe",".mx",".tmpl" }
  };

  Prefix = ARRAY OSKind OF SMap {
  (* Unix *)       SMap { "", "", "", "", "",
                          "", "", "", "",
                          "", "", "", "", "",
                          "", "lib", "lib", "lib", "", "","" },
  (* GrumpyUnix *) SMap { "", "", "", "", "",
                          "", "", "", "",
                          "", "", "", "", "",
                          "", "lib", "lib", "lib", "", "","" },
  (* Win32 *)      SMap { "", "", "", "", "",
                          "", "", "", "",
                          "", "", "", "", "",
                          "", "", "", "","", "","" }
  };

  Default_pgm = ARRAY OSKind OF TEXT { "a.out", "a.out", "NONAME.EXE" };

VAR
  host_os := OSKind.Unix;
  target_os := OSKind.Unix;
  lcase  : ARRAY CHAR OF CHAR;

PROCEDURE SetOS (kind: OSKind;  host: BOOLEAN) =
  BEGIN
    IF host THEN
      host_os := kind;
    ELSE
      target_os := kind;
    END;
  END SetOS;

PROCEDURE New (a, b, c, d: TEXT := NIL): TEXT =
  VAR len: INTEGER;  buf: ARRAY [0..255] OF CHAR;  ref: REF ARRAY OF CHAR;
  BEGIN
    IF (b # NIL) THEN
      IF Pathname.Absolute (b) THEN
        a := b;
      ELSE
        a := Pathname.Join (a, b, NIL);
      END;
    END;
    IF (c # NIL) THEN
      IF Pathname.Absolute (c) THEN
        a := c;
      ELSE
        a := Pathname.Join (a, c, NIL);
      END;
    END;
    IF (d # NIL) THEN
      IF Pathname.Absolute (d) THEN
        a := d;
      ELSE
        a := Pathname.Join (a, d, NIL);
      END;
    END;
    len := Text.Length (a);
    IF (len <= NUMBER (buf)) THEN
      Text.SetChars (buf, a);
      RETURN FixPath (SUBARRAY (buf, 0, len));
    ELSE
      ref := NEW (REF ARRAY OF CHAR, len);
      Text.SetChars (ref^, a);
      RETURN FixPath (ref^);
    END;
  END New;

PROCEDURE Join (dir, base: TEXT;  k: Kind;  <*UNUSED*>host: BOOLEAN): TEXT =
  VAR
    pre      := Prefix [target_os][k];
    ext      := Suffix [target_os][k];
    d_sep    := DirSep [host_os];
    v_sep    := VolSep [host_os];
    ch       : CHAR;
    buf      : ARRAY [0..255] OF CHAR;
    dir_len  := 0;
    pre_len  := Text.Length (pre);
    base_len := Text.Length (base);
    ext_len  := Text.Length (ext);
    add_sep  := FALSE;
    len      := (pre_len + base_len + ext_len);

    PROCEDURE Append (VAR a: ARRAY OF CHAR;  start: INTEGER;  b: TEXT; len: INTEGER): INTEGER =
      BEGIN
        Text.SetChars (SUBARRAY (a, start, len), b);
        RETURN start + len;
      END Append;

    PROCEDURE DoJoin (VAR buf: ARRAY OF CHAR): TEXT =
      VAR
        len := 0;
      BEGIN
        IF dir_len # 0 THEN
          len := Append (buf, len, dir, dir_len);
          IF add_sep THEN
            buf[len] := d_sep;
            INC (len);
          END;
        END;
        len := Append (buf, len, pre, pre_len);
        len := Append (buf, len, base, base_len);
        len := Append (buf, len, ext, ext_len);
        RETURN FixPath (SUBARRAY (buf, 0, len));
      END DoJoin;

  BEGIN (* Join *)

    (* find out how much space we need *)
    IF (dir # NIL) THEN
      dir_len := Text.Length (dir);
      INC (len, dir_len);
      IF dir_len # 0 THEN
        ch := Text.GetChar (dir, dir_len-1);
        (* ensure there is a slash after dir *)
        IF (NOT IsDirSep(ch, d_sep)) AND (ch # v_sep) THEN
          add_sep := TRUE;
          INC (len);
        END;
      END;
    END;

    (* allocate it and fill it in *)
    IF (len <= NUMBER (buf)) THEN
      RETURN DoJoin (buf);
    ELSE
      RETURN DoJoin (NEW (REF ARRAY OF CHAR, len)^);
    END;
  END Join;

PROCEDURE Parse (nm: TEXT;  <*UNUSED*>host: BOOLEAN): T =
  VAR len := Text.Length (nm);   buf: ARRAY [0..255] OF CHAR;
  BEGIN
    IF (len <= NUMBER (buf))
      THEN RETURN DoParse (nm, SUBARRAY (buf, 0, len));
      ELSE RETURN DoParse (nm, NEW (REF ARRAY OF CHAR, len)^);
    END;
  END Parse;

PROCEDURE DoParse (nm_txt: TEXT;  VAR nm: ARRAY OF CHAR): T =
  VAR
    t       : T;
    len     := NUMBER (nm);
    base_len:= 0;
    d_index := -1;
    v_index := -1;
    start   := 0;
    d_sep   := DirSep [host_os];
    v_sep   := VolSep [host_os];
    ext     : TEXT;
    ext_len : INTEGER;
    pre     : TEXT;
    ch      : CHAR;
  BEGIN
    Text.SetChars (nm, nm_txt);

    (* find the last instance of each separator *)
    FOR i := 0 TO len-1 DO
      ch := nm[i];
      IF (ch = v_sep) THEN
        v_index := i;
      END;
      IF IsDirSep (ch, d_sep) THEN
        d_index := i;
      END;
    END;

    (* extract the prefix *)
    IF (v_index = -1) AND (d_index = -1) THEN
      (* no separators *)
      t.dir := NIL;
      start := 0;
    ELSIF (d_index = -1) THEN
      (* no directory separator, only a volume separator *)
      t.dir := Text.FromChars (SUBARRAY (nm, 0, v_index+1));
      start := v_index + 1;
    ELSIF (d_index = 0) THEN
      t.dir := DirSepText [host_os];
      start := 1;
    ELSE
      t.dir := Text.FromChars (SUBARRAY (nm, 0, d_index));
      start := d_index+1;
    END;
    base_len := len - start;

    (* search for a matching suffix *)
    t.kind := Kind.Unknown;
    ext_len := 0;
    FOR k := FIRST (Kind) TO LAST (Kind) DO
      ext := Suffix [target_os][k];
      IF ExtMatch (nm_txt, ext, host_os) THEN
        ext_len := Text.Length (ext);
        t.kind := k;
        EXIT;
      END;
    END;

    (* extract the base component *)
    t.base := Text.FromChars (SUBARRAY (nm, start, base_len - ext_len));

    pre := Prefix[target_os][t.kind];
    IF (Text.Length (pre) > 0) AND PrefixMatch (t.base, pre, host_os) THEN
      t.base := Text.Sub (t.base, Text.Length (pre));
    END;

    RETURN t;
  END DoParse;

PROCEDURE IsEqual (a, b: TEXT): BOOLEAN =
  BEGIN
    RETURN RegionMatch (a, 0, b, 0, MAX (Text.Length (a), Text.Length (b)),
                        ignore_case := (host_os = OSKind.Win32));
  END IsEqual;

PROCEDURE ExtMatch (nm, ext: TEXT;  os: OSKind): BOOLEAN =
  VAR nm_len := Text.Length (nm);  ext_len := Text.Length (ext);
  BEGIN
    RETURN (ext_len > 0)
       AND RegionMatch (nm, nm_len - ext_len, ext, 0, ext_len,
                        ignore_case := (os = OSKind.Win32));
  END ExtMatch;

PROCEDURE PrefixMatch (nm, pre: TEXT;  os: OSKind): BOOLEAN =
  BEGIN
    RETURN RegionMatch (nm, 0, pre, 0, Text.Length (pre),
                        ignore_case := (os = OSKind.Win32));
  END PrefixMatch;

PROCEDURE RegionMatch (a: TEXT;  start_a: INTEGER;
                       b: TEXT;  start_b: INTEGER;
                       len: INTEGER;  ignore_case: BOOLEAN): BOOLEAN =
  CONST N = 128;
  VAR
    len_a : INTEGER;
    len_b : INTEGER;
    buf_a, buf_b : ARRAY [0..N-1] OF CHAR;
    cha : CHAR;
    chb : CHAR;
  BEGIN
    IF (start_a < 0) OR (start_b < 0) THEN RETURN FALSE; END;

    len_a := Text.Length (a);
    IF (start_a + len > len_a) THEN RETURN FALSE; END;

    len_b := Text.Length (b);
    IF (start_b + len > len_b) THEN RETURN FALSE; END;

    WHILE (len > 0) DO
      Text.SetChars (buf_a, a, start_a);
      Text.SetChars (buf_b, b, start_b);
      IF ignore_case THEN
        FOR i := 0 TO MIN (N, len) - 1 DO
          cha := buf_a[i];
          chb := buf_b[i];
          IF (cha # chb) AND (lcase [cha] # lcase [chb]) THEN
            RETURN FALSE;
          END;
        END;
      ELSE
        FOR i := 0 TO MIN (N, len) - 1 DO
          IF buf_a[i] # buf_b[i] THEN RETURN FALSE; END;
        END;
      END;
      DEC (len, N);  INC (start_a, N);  INC (start_a, N);
    END;
    RETURN TRUE;
  END RegionMatch;

PROCEDURE EndOfArc (path: TEXT;  xx: CARDINAL;  d_sep: CHAR): BOOLEAN =
  VAR len := Text.Length (path);
  BEGIN
    RETURN (len = xx) OR ((len > xx) AND IsDirSep (Text.GetChar (path, xx), d_sep));
  END EndOfArc;

PROCEDURE DefaultProgram (<*UNUSED*>host: BOOLEAN): TEXT =
  BEGIN
    RETURN Default_pgm [target_os];
  END DefaultProgram;

PROCEDURE ProgramName (base: TEXT;  <*UNUSED*>host: BOOLEAN): TEXT =
  BEGIN
    RETURN base & Suffix[target_os][Kind.PGM];
  END ProgramName;

PROCEDURE LibraryName (base: TEXT;  <*UNUSED*>host: BOOLEAN): TEXT =
  VAR os := target_os;
  BEGIN
    RETURN Prefix[os][Kind.LIB] & base & Suffix[os][Kind.LIB];
  END LibraryName;

PROCEDURE Convert (nm: TEXT; <*UNUSED*>host: BOOLEAN): TEXT =
  BEGIN
    RETURN nm;
  END Convert;

PROCEDURE Escape (nm: TEXT): TEXT =
  VAR len: INTEGER;   buf: ARRAY [0..255] OF CHAR;
  BEGIN
    IF (nm = NIL) THEN RETURN NIL; END;
    len := Text.Length (nm);
    IF (len + len <= NUMBER (buf))
      THEN RETURN DoEscape (nm, len, buf);
      ELSE RETURN DoEscape (nm, len, NEW (REF ARRAY OF CHAR, len + len)^);
    END;
  END Escape;

PROCEDURE DoEscape (nm: TEXT;  len: INTEGER;  VAR buf: ARRAY OF CHAR): TEXT =
  VAR n_escapes := 0;  src, dest: INTEGER;  c: CHAR;
  BEGIN
    Text.SetChars (buf, nm);
    FOR i := 0 TO len-1 DO
      IF (buf[i] = BackSlash) THEN INC (n_escapes); END;
    END;
    IF (n_escapes = 0) THEN RETURN nm; END;
    src  := len - 1;
    dest := src + n_escapes;
    WHILE (src > 0) DO
      c := buf[src];  DEC (src);
      buf[dest] := c;  DEC (dest);
      IF (c = BackSlash) THEN  buf[dest] := BackSlash;  DEC (dest);  END;
    END;
    RETURN Text.FromChars (SUBARRAY (buf, 0, len + n_escapes));
  END DoEscape;

PROCEDURE IsDirSep (ch: CHAR; d_sep: CHAR): BOOLEAN =
  BEGIN
    RETURN (ch = Slash) OR (ch = d_sep);
  END IsDirSep;

PROCEDURE MakeRelative (VAR path: TEXT;  full, rel: TEXT): BOOLEAN =
  VAR
    os := host_os;
    d_sep := DirSep[os];
  BEGIN
    IF PrefixMatch (path, full, os)
      AND EndOfArc (path, Text.Length (full), d_sep) THEN
      VAR
        p := Text.Length(full);
        n := Text.Length(path);
      BEGIN
        WHILE (p < n) AND IsDirSep (Text.GetChar (path, p), d_sep) DO
          INC(p) 
        END;
        path := New (rel, Text.Sub (path, p)); 
      END;
      RETURN TRUE;
    ELSE
      RETURN FALSE;
    END;
  END MakeRelative;

PROCEDURE Reverse (VAR p: ARRAY OF CHAR) =
  VAR
    len := NUMBER (p);
    ch: CHAR;
    i : INTEGER;
    j : INTEGER;
  BEGIN
    IF len > 1 THEN
      i := 0;
      j := len - 1;
      WHILE i < j DO
        ch := p[i];
        p[i] := p[j];
        p[j] := ch;
        INC (i);
        DEC (j);
      END;
    END;
  END Reverse;

PROCEDURE PathAnyDots (READONLY p: ARRAY OF CHAR; READONLY start: INTEGER; READONLY len: CARDINAL): BOOLEAN =
  BEGIN
    FOR i := start TO (start + len - 1) DO
      IF p[i] = '.' THEN
        RETURN TRUE;
      END;
    END;
    RETURN FALSE;
  END PathAnyDots;

PROCEDURE PathRemoveDots (VAR p: ARRAY OF CHAR; READONLY start: INTEGER; VAR len: CARDINAL) =
  (* remove redundant "/arc/../" and "/./" segments *)
  VAR
    os    := host_os;
    d_sep := DirSep [os];
    v_sep := VolSep [os];
    level := 0;
    end   := (start + len);
    from  := start;
    to    := start;
    ch    := Null;
  BEGIN

    IF len < 2 THEN
      RETURN;
    END;

    (* first check for any dots in order to avoid being slower than the old version *)
    IF NOT PathAnyDots (p, start, len) THEN
      RETURN;
    END;

    Reverse (SUBARRAY (p, start, len));

    WHILE from # end DO
      ch := p[from];
      IF (ch = '.')
          AND (((from + 1) = end) OR (p[from + 1] = v_sep) OR IsDirSep (p[from + 1], d_sep))
          AND ((from = start) OR IsDirSep (p[from - 1], d_sep)) THEN

        (* change \.: to : probably v_sep should be allowed in fewer places *)
        IF (v_sep # Null)
            AND ((from + 1) # end) AND (p[from + 1] = v_sep)
            AND (from # start) AND IsDirSep (p[from - 1], d_sep)
            AND (to # start) AND IsDirSep (p[to - 1], d_sep) THEN
          p[to - 1] := v_sep;
        END;

        INC (from);
        IF from = end THEN
          DEC (from);
        END;
      ELSE
        IF (ch = '.')
            AND ((from + 1) # end)
            AND (p[from + 1] = '.')
            AND (((from + 2) = end) OR (p[from + 2] = v_sep) OR IsDirSep (p[from + 2], d_sep))
            (* probably v_sep should be allowed in fewer places *)
            AND ((from = start) OR IsDirSep (p[from - 1], d_sep)) THEN
          INC (level);
          INC (from, 2);
          (* remove the slash we already wrote; we will write the one at the end, if there is one *)
          IF (to # start) AND IsDirSep (p[to - 1], d_sep) THEN
            DEC (to);
          END;
          (* counteract the implicit slash at end *)
          IF from = end THEN
            INC (level);
            DEC (from);
          END;
        ELSE
          DEC (level, ORD ((level # 0) AND ((ch = '/') OR (ch = d_sep))));
          IF level = 0 THEN
            p[to] := ch;
            INC (to);
          END;
        END;
      END;
      INC (from);
    END;

    (* implicit slash at end *)
    DEC (level, ORD ((level # 0) AND NOT IsDirSep (p[end - 1], d_sep)));

    (* if there were more ".."s than preceding elements, add back some ".."s *)
    WHILE level # 0 DO
      IF (to # start) AND (NOT IsDirSep (p[to - 1], d_sep)) THEN
        p[to] := d_sep;
        INC (to);
      END;
      p[to] := '.';
      INC (to);
      p[to] := '.';
      INC (to);
      DEC (level);
    END;

    end := to;
    len := (end - start);

    (* if input started with a separator or two, then so must output *)
    IF IsDirSep (p[from - 1], d_sep) AND (len = 0 OR NOT IsDirSep (p[to - 1], d_sep)) THEN
      p[to] := d_sep;
      INC (to);
      INC (end);
      INC (len);
      IF IsDirSep (p[from - 2], d_sep) AND (len = 1 OR NOT IsDirSep (p[to - 2], d_sep)) THEN
        p[to] := d_sep;
        INC (to);
        INC (end);
        INC (len);
      END;
    END;

    Reverse (SUBARRAY (p, start, len));

  END PathRemoveDots;

PROCEDURE FixPath (VAR p: ARRAY OF CHAR): TEXT =
  (* remove redundant "/arc/../" and "/./" segments
  VAR
    d_sep := DirSep [host_os];
    start := 0;
    len := NUMBER (p);
  BEGIN
    (* remove trailing slashes, leaving at most one *)
    WHILE (len > 1) AND IsDirSep (p[start + len - 1], d_sep) DO
      DEC (len);
    END;
    (* remove leading slashes, leaving at most two *)
    WHILE (len > 2) AND IsDirSep (p[start], d_sep) AND IsDirSep (p[start + 1], d_sep) AND IsDirSep (p[start + 2], d_sep) DO
      INC (start);
      DEC (len);
    END;
    PathRemoveDots (p, start, len);
    IF len = 0 THEN
      RETURN ".";
    END;
    RETURN Text.FromChars (SUBARRAY (p, start, len));
  END FixPath;

PROCEDURE Test () =
  CONST
    data = ARRAY OF TEXT {
        "abc",
        "def",
        "////c../..b/a..////",
        "////..c/..b/a..////",
        "////../..b/a..////",
        "////../b../a..////",
        "////../../a..////",
        "////../../..////",
        "../../..////",
        "../../../a///",
        "../../a/..////",
        ".",
        "./",
        "/.",
        "/./",
        "/./.",
        "/././",
        "././.",
        "./././",
        "./.",
        "a/./b",
        "a/../b",
        "a/b/../c/d",
        "a/b/../c.",
        "a/b/../../c.",
        "a/b/../../../c.",
        ".a/b/../../../c.",
        "/.a/b/../../../c.",
        "a/..",
        "a/../",
        "a/../../..",
        "/../",
        "/../a",
        "c:\\b",
        "c:.\\b",
        "c:.\\b\\c",
        "c:.\\b\\..\\c",
        "c:\\b\\..\\c"
        };
  VAR
    buf : ARRAY [0..255] OF CHAR;
    t1 : TEXT;
  BEGIN

    SetOS (OSKind.Win32, TRUE);
    SetOS (OSKind.Unix, FALSE);

    FOR i := 0 TO LAST(data) DO
      Text.SetChars (buf, data[i]);
      t1 := Convert (FixPath (SUBARRAY (buf, 0, Text.Length (data[i]))), TRUE);
      Text.SetChars (buf, data[i]);
      RTIO.PutInt (i);
      RTIO.PutText (" result for " & data[i] & " => " & t1 & "\n");
    END;
    RTIO.Flush ();
    Process.Exit (1);
  END Test;

BEGIN
  FOR i := FIRST (lcase) TO LAST (lcase) DO lcase[i] := i; END;
  FOR i := 'A' TO 'Z' DO
    lcase[i] := VAL (ORD (i) - ORD ('A') + ORD ('a'), CHAR);
  END;

 (* Probe the host for what slash it uses, and default host and
    target "naming conventions" based on that.
  *)
  IF Text.GetChar (Pathname.Join ("a", "b"), 1) = BackSlash THEN

    SlashText := "\\";
    SetOS (OSKind.Win32, TRUE);
    SetOS (OSKind.Win32, FALSE);

    (* forward and backward slash compare equal *)

    lcase [Slash] := BackSlash;

  END;

  (* Test (); *)

END M3Path.
