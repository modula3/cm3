(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Mon Sep 26 09:07:50 PDT 1994 by kalsow     *)
(*      modified on Mon Oct 25 10:31:06 PDT 1993 by mcjones    *)
(*      modified on Wed May 12 16:56:05 PDT 1993 by meehan     *)
(*      modified on Mon May 10 20:58:46 PDT 1993 by mjordan    *)

MODULE M3Path;

IMPORT Pathname, Text, ASCII, Compiler;

CONST
  Null      = '\000';
  Colon     = ':';
  Slash     = '/';
  BackSlash = '\\';

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

  Default_pgm = ARRAY OSKind OF TEXT { "a.out", "a.out", "noname.exe" };

VAR target_os := ARRAY Compiler.OS OF OSKind{OSKind.Unix, OSKind.Win32}[Compiler.ThisOS];
CONST d_sep = ARRAY Compiler.OS OF CHAR{Slash, BackSlash}[Compiler.ThisOS];
CONST v_sep = ARRAY Compiler.OS OF CHAR{Null, Colon}[Compiler.ThisOS];
CONST DirSepText = ARRAY Compiler.OS OF TEXT{"/", "\\"}[Compiler.ThisOS];

PROCEDURE SetTargetOS (kind: OSKind) =
  BEGIN
    target_os := kind;
  END SetTargetOS;

PROCEDURE New (a, b, c, d: TEXT := NIL): TEXT =
  VAR len: CARDINAL;  buf: ARRAY [0..255] OF CHAR;  ref: REF ARRAY OF CHAR;
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

PROCEDURE Join (dir, base: TEXT;  k: Kind): TEXT =
  VAR
    pre      := Prefix [target_os][k];
    ext      := Suffix [target_os][k];
    ch       : CHAR;
    buf      : ARRAY [0..255] OF CHAR;
    dir_len  : CARDINAL := 0;
    pre_len  := Text.Length (pre);
    base_len := Text.Length (base);
    ext_len  := Text.Length (ext);
    add_sep  := FALSE;
    len      := (pre_len + base_len + ext_len);

    PROCEDURE Append (VAR a: ARRAY OF CHAR;  start: CARDINAL;  b: TEXT; len: CARDINAL): CARDINAL =
      BEGIN
        Text.SetChars (SUBARRAY (a, start, len), b);
        RETURN start + len;
      END Append;

    PROCEDURE DoJoin (VAR buf: ARRAY OF CHAR): TEXT =
      VAR
        len : CARDINAL := 0;
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
        IF (NOT IsDirSep(ch)) AND (ch # v_sep) THEN
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

PROCEDURE Parse (nm: TEXT): T =
  VAR len := Text.Length (nm);   buf: ARRAY [0..255] OF CHAR;
  BEGIN
    IF (len <= NUMBER (buf))
      THEN RETURN DoParse (nm, len, SUBARRAY (buf, 0, len));
      ELSE RETURN DoParse (nm, len, NEW (REF ARRAY OF CHAR, len)^);
    END;
  END Parse;

PROCEDURE DoParse (nm_txt: TEXT; len: CARDINAL; VAR nm: ARRAY OF CHAR): T =
  VAR
    t       : T;
    base_len: CARDINAL;
    d_index : INTEGER;
    start   : CARDINAL;
    ext     : TEXT;
    ext_len : CARDINAL;
    pre     : TEXT;
  BEGIN
    Text.SetChars (nm, nm_txt);

    (* find the last directory separator *)
    d_index := Text.FindCharR (nm_txt, '/');
    IF d_sep # '/' THEN
      d_index := MAX (d_index, Text.FindCharR (nm_txt, d_sep));
    END;

    (* extract the prefix *)
    IF d_index = -1 THEN
      (* no separators *)
      t.dir := NIL;
      start := 0;
    ELSIF (d_index = 0) THEN
      t.dir := DirSepText;
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
      ext_len := Text.Length (ext);
      IF (ext_len # 0)
          AND (len >= ext_len)
          AND RegionMatch (nm_txt, (len - ext_len), ext, 0, ext_len) THEN
        t.kind := k;
        EXIT;
      ELSE
        ext_len := 0;
      END;
    END;

    (* extract the base component *)
    t.base := Text.FromChars (SUBARRAY (nm, start, base_len - ext_len));

    pre := Prefix[target_os][t.kind];
    IF (Text.Length (pre) > 0) AND PrefixMatch (t.base, pre) THEN
      t.base := Text.Sub (t.base, Text.Length (pre));
    END;

    RETURN t;
  END DoParse;

PROCEDURE IsEqual (a, b: TEXT): BOOLEAN =
  BEGIN
    RETURN RegionMatch (a, 0, b, 0, MAX (Text.Length (a), Text.Length (b)));
  END IsEqual;

PROCEDURE PrefixMatch (nm, pre: TEXT): BOOLEAN =
  BEGIN
    RETURN RegionMatch (nm, 0, pre, 0, Text.Length (pre));
  END PrefixMatch;

PROCEDURE RegionMatch (a: TEXT;  start_a: CARDINAL;
                       b: TEXT;  start_b: CARDINAL;
                       len: CARDINAL): BOOLEAN =
  CONST N = 128;
        ignore_case = (Compiler.ThisOS = Compiler.OS.WIN32);
  VAR
    len_a : CARDINAL;
    len_b : CARDINAL;
    buf_a, buf_b : ARRAY [0..N-1] OF CHAR;
    cha : CHAR;
    chb : CHAR;
    j : CARDINAL;
  BEGIN
    len_a := Text.Length (a);
    IF (start_a + len > len_a) THEN RETURN FALSE; END;

    len_b := Text.Length (b);
    IF (start_b + len > len_b) THEN RETURN FALSE; END;

    WHILE len # 0 DO
      Text.SetChars (buf_a, a, start_a);
      Text.SetChars (buf_b, b, start_b);
      j := MIN (N, len);
      IF ignore_case THEN
        FOR i := 0 TO j - 1 DO
          cha := buf_a[i];
          chb := buf_b[i];
          IF cha # chb THEN
            IF cha = '/' THEN
              cha := '\\';
            END;
            IF chb = '/' THEN
              chb := '\\';
            END;
            IF (cha # chb) AND (ASCII.Lower [cha] # ASCII.Lower [chb]) THEN
              RETURN FALSE;
            END;
          END;
        END;
      ELSE
        FOR i := 0 TO j - 1 DO
          IF buf_a[i] # buf_b[i] THEN RETURN FALSE; END;
        END;
      END;
      DEC (len, j);
      INC (start_a, j);
      INC (start_a, j);
    END;
    RETURN TRUE;
  END RegionMatch;

PROCEDURE EndOfArc (path: TEXT;  xx: CARDINAL): BOOLEAN =
  VAR len := Text.Length (path);
  BEGIN
    RETURN (len = xx) OR ((len > xx) AND IsDirSep (Text.GetChar (path, xx)));
  END EndOfArc;

PROCEDURE DefaultProgram (): TEXT =
  BEGIN
    RETURN Default_pgm [target_os];
  END DefaultProgram;

PROCEDURE ProgramName (base: TEXT): TEXT =
  BEGIN
    RETURN base & Suffix[target_os][Kind.PGM];
  END ProgramName;

PROCEDURE LibraryName (base: TEXT): TEXT =
  VAR os := target_os;
  BEGIN
    RETURN Prefix[os][Kind.LIB] & base & Suffix[os][Kind.LIB];
  END LibraryName;

PROCEDURE Convert (nm: TEXT): TEXT =
  VAR len: CARDINAL := 0; buf: ARRAY [0..255] OF CHAR;
  BEGIN
    IF nm # NIL THEN
      len := Text.Length (nm);
    END;
    IF len = 0 THEN
      RETURN nm;
    END;
    IF (len <= NUMBER (buf))
      THEN RETURN DoConvert (nm, len, buf);
      ELSE RETURN DoConvert (nm, len, NEW (REF ARRAY OF CHAR, len)^);
    END;
  END Convert;

PROCEDURE DoConvert (nm: TEXT;  len: CARDINAL;  VAR buf: ARRAY OF CHAR): TEXT =
  VAR changed := FALSE;
      c: CHAR;
  BEGIN
    Text.SetChars (buf, nm);
    FOR i := 0 TO len-1 DO
      c := buf[i];
      IF c = BackSlash THEN
        changed := TRUE;
        buf[i] := Slash;
      END;
    END;
    IF changed THEN
      RETURN Text.FromChars (SUBARRAY (buf, 0, len));
    ELSE
      RETURN nm;
    END;
  END DoConvert;

PROCEDURE IsDirSep (ch: CHAR): BOOLEAN =
  BEGIN
    RETURN (ch = Slash) OR (ch = d_sep);
  END IsDirSep;

PROCEDURE MakeRelative (VAR path: TEXT;  full, rel: TEXT): BOOLEAN =
  BEGIN
    IF PrefixMatch (path, full)
      AND EndOfArc (path, Text.Length (full)) THEN
      VAR
        p := Text.Length(full);
        n := Text.Length(path);
      BEGIN
        WHILE (p < n) AND IsDirSep (Text.GetChar (path, p)) DO
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
    i : CARDINAL;
    j : CARDINAL;
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

PROCEDURE PathAnyDots (READONLY p: ARRAY OF CHAR; READONLY start: CARDINAL; READONLY len: CARDINAL): BOOLEAN =
  BEGIN
    IF len = 0 THEN
      RETURN FALSE;
    END;
    FOR i := start TO (start + len - 1) DO
      IF p[i] = '.' THEN
        RETURN TRUE;
      END;
    END;
    RETURN FALSE;
  END PathAnyDots;

PROCEDURE PathRemoveDots (VAR p: ARRAY OF CHAR; READONLY start: CARDINAL; VAR len: CARDINAL) =
  (* remove redundant "/arc/../" and "/./" segments
  The algorithm here is:
    Move through the string while copying on to itself in place.
    Maintain a separate source and destination index, since
      we sometimes skip characters.
    If we encounter /./, skip it.
    If we counter /../ increment a counter.
     As long as that counter is not zero, skip elements.
     If we see a non-/../ element while counter is not zero, decrement counter.
     Reverse the string initially so that we are skipping "in the right direction".
     And reverse it again once we are done.
    There are a few extra details, such as if there are "extra" .. elements, keep them
      that is a/.. => empty
              a/../.. => ..
    It is possible that the strings we get will be further concated to other strings.
    Turning a/../../ into empty is not correct if it is then going to be appended
    to b/c/d.

  This algorithm works with arbitrarily long strings with only fixed small additional memory used.
  The previous algorithm used here had several deficiencies. It used an amount of memory correlated
  to the number of slashes -- longer strings required more memory. Every time it removed an element,
  it would copy the whole rest of the string down and rescan for all the slashes.
  *)
  VAR
    level : CARDINAL := 0;
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
          AND (((from + 1) = end) OR (p[from + 1] = v_sep) OR IsDirSep (p[from + 1]))
          AND ((from = start) OR IsDirSep (p[from - 1])) THEN

        (* change \.: to : probably v_sep should be allowed in fewer places *)
        IF (v_sep # Null)
            AND ((from + 1) # end) AND (p[from + 1] = v_sep)
            AND (from # start) AND IsDirSep (p[from - 1])
            AND (to # start) AND IsDirSep (p[to - 1]) THEN
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
            AND (((from + 2) = end) OR (p[from + 2] = v_sep) OR IsDirSep (p[from + 2]))
            (* probably v_sep should be allowed in fewer places *)
            AND ((from = start) OR IsDirSep (p[from - 1])) THEN
          INC (level);
          INC (from, 2);
          (* remove the slash we already wrote; we will write the one at the end, if there is one *)
          IF (to # start) AND IsDirSep (p[to - 1]) THEN
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
    DEC (level, ORD ((level # 0) AND NOT IsDirSep (p[end - 1])));

    (* if there were more ".."s than preceding elements, add back some ".."s *)
    WHILE level # 0 DO
      IF (to # start) AND (NOT IsDirSep (p[to - 1])) THEN
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
    IF IsDirSep (p[from - 1]) AND (len = 0 OR NOT IsDirSep (p[to - 1])) THEN
      p[to] := d_sep;
      INC (to);
      INC (end);
      INC (len);
      IF IsDirSep (p[from - 2]) AND (len = 1 OR NOT IsDirSep (p[to - 2])) THEN
        p[to] := d_sep;
        INC (to);
        INC (end);
        INC (len);
      END;
    END;

    Reverse (SUBARRAY (p, start, len));

  END PathRemoveDots;

PROCEDURE FixPath (VAR p: ARRAY OF CHAR): TEXT =
  (* remove redundant "/arc/../" and "/./" segments *)
  VAR
    start : CARDINAL := 0;
    len := NUMBER (p);
    j: CARDINAL := 0;

  BEGIN

    (* remove trailing slashes, leaving at most one *)
    (* check for length 3 here so we don't encroach on the leading slashes *)
    (* Trailing slashes break some code so for now don't do this.
    WHILE (len >= 3) AND IsDirSep (p[start + len - 1]) AND IsDirSep (p[start + len - 2]) DO
      DEC (len);
    END;
    *)

    (* remove all trailing slashes *)
    (* check for length 3 here so we do not encroach on any leading slashes *)

    WHILE (len >= 3) AND IsDirSep (p[start + len - 1]) DO
      DEC (len);
    END;

    (* remove trailing slash in 2 char string if first char is not slash,
    otherwise ab/ => ab, but a/ => a/ which does not make sense *)

    IF (len = 2) AND (NOT IsDirSep(p[start])) AND (IsDirSep(p[start + 1])) THEN
      len := 1;
    END;

    (* remove leading slashes, leaving at most two *)

    WHILE (len > 2) AND IsDirSep (p[start]) AND IsDirSep (p[start + 1]) AND IsDirSep (p[start + 2]) DO
      INC (start);
      DEC (len);
    END;

    (* Change runs of separators to single separators, except at the start. *)

    IF len > 2 THEN
        j := start + 1;
        FOR i := start + 1 TO start + len - 2 DO
            IF NOT (IsDirSep(p[i]) AND IsDirSep(p[i + 1])) THEN
                p[j] := p[i];
                INC(j);
            END;
        END;
        p[j] := p[start + len - 1];
        INC(j);
        len := j - start;
    END;

    PathRemoveDots (p, start, len);

    IF len = 0 THEN
      RETURN ".";
    END;

    RETURN Text.FromChars (SUBARRAY (p, start, len));

  END FixPath;

BEGIN
END M3Path.
