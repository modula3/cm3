(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Mon Sep 26 09:07:50 PDT 1994 by kalsow     *)
(*      modified on Mon Oct 25 10:31:06 PDT 1993 by mcjones    *)
(*      modified on Wed May 12 16:56:05 PDT 1993 by meehan     *)
(*      modified on Mon May 10 20:58:46 PDT 1993 by mjordan    *)

MODULE M3Path;

IMPORT Pathname, Text, ASCII, QMachine;

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

PROCEDURE SetOS (kind: OSKind;  host: BOOLEAN) =
  BEGIN
    IF host THEN
      host_os := kind;
    ELSE
      target_os := kind;
    END;
  END SetOS;

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
    d_sep    := DirSep [host_os];
    v_sep    := VolSep [host_os];
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
    d_sep   := DirSep [host_os];
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
  VAR
    ignore_case := (host_os = OSKind.Win32);
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

PROCEDURE EndOfArc (path: TEXT;  xx: CARDINAL;  d_sep: CHAR): BOOLEAN =
  VAR len := Text.Length (path);
  BEGIN
    RETURN (len = xx) OR ((len > xx) AND IsDirSep (Text.GetChar (path, xx), d_sep));
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

PROCEDURE Escape (nm: TEXT): TEXT =
  VAR len: CARDINAL := 0; buf: ARRAY [0..255] OF CHAR;
  BEGIN
    IF nm # NIL THEN
      len := Text.Length (nm);
    END;
    IF len = 0 THEN
      RETURN nm;
    END;
    IF (len + len <= NUMBER (buf))
      THEN RETURN DoEscape (nm, len, buf);
      ELSE RETURN DoEscape (nm, len, NEW (REF ARRAY OF CHAR, len + len)^);
    END;
  END Escape;

PROCEDURE DoEscape (nm: TEXT;  len: CARDINAL;  VAR buf: ARRAY OF CHAR): TEXT =
  VAR n_escapes : CARDINAL := 0;  src, dest: CARDINAL;  c: CHAR;
  BEGIN
    Text.SetChars (buf, nm);
    FOR i := 0 TO len-1 DO
      IF (buf[i] = BackSlash) THEN INC (n_escapes); END;
    END;
    IF (n_escapes = 0) THEN RETURN nm; END;
    src  := len;
    dest := src + n_escapes;
    WHILE src # 0 DO
      DEC (src);
      DEC (dest);
      c := buf[src];
      buf[dest] := c;
      IF (c = BackSlash) THEN
        DEC (dest);
        buf[dest] := BackSlash;
      END;
    END;
    RETURN Text.FromChars (SUBARRAY (buf, 0, len + n_escapes));
  END DoEscape;

PROCEDURE IsDirSep (ch: CHAR; d_sep: CHAR): BOOLEAN =
  BEGIN
    RETURN (ch = Slash) OR (ch = d_sep);
  END IsDirSep;

PROCEDURE MakeRelative (VAR path: TEXT;  full, rel: TEXT): BOOLEAN =
  VAR
    d_sep := DirSep[host_os];
  BEGIN
    IF PrefixMatch (path, full)
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
    os    := host_os;
    d_sep := DirSep [os];
    v_sep := VolSep [os];
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
  (* remove redundant "/arc/../" and "/./" segments *)
  VAR
    d_sep := DirSep [host_os];
    start : CARDINAL := 0;
    len := NUMBER (p);
  BEGIN
    (* remove trailing slashes, leaving at most one *)
    (* check for length 3 here so we don't encroach on the leading slashes *)
    (* Trailing slashes break some code so for now don't do this.
    WHILE (len >= 3) AND IsDirSep (p[start + len - 1], d_sep) AND IsDirSep (p[start + len - 2], d_sep) DO
      DEC (len);
    END;
    *)
    (* remove all trailing slashes *)
    (* check for length 3 here so we don't encroach on the leading slashes *)
    WHILE (len >= 3) AND IsDirSep (p[start + len - 1], d_sep) DO
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

PROCEDURE PathLooselyConvertUserInputToHost_TextToText (text: TEXT) : TEXT =
  VAR length : CARDINAL := 0;
      smallbuf: ARRAY [0..63] OF CHAR;
      bigbuf: REF ARRAY OF CHAR;
  BEGIN
    IF text # NIL THEN
      length := Text.Length (text);
    END;
    IF length = 0 THEN
      RETURN text;
    END;
    IF length <= NUMBER (smallbuf) THEN
      Text.SetChars (smallbuf, text);
      text := PathLooselyConvertUserInputToHost_ArrayToText (SUBARRAY (smallbuf, 0, length));
    ELSE
      bigbuf := NEW (REF ARRAY OF CHAR, length);
      Text.SetChars (bigbuf^, text);
      text := PathLooselyConvertUserInputToHost_ArrayToText (bigbuf^);
    END;
  RETURN text;
  END PathLooselyConvertUserInputToHost_TextToText;

PROCEDURE PathLooselyConvertUserInputToHost_ArrayToText (VAR p: ARRAY OF CHAR) : TEXT =
  (* Loosely convert Win32 or Posix to native; this is so that the environment
  variables and quake variables M3CONFIG, ROOT, INSTALLROOT can be
  specified either way. There is no clear spec for this function and users
  of are it strongly cautioned (cautioned what? nothing in particular) *)
  CONST
    Letters = SET OF CHAR {'a' .. 'z', 'A' .. 'Z'};
    root = ARRAY OF CHAR { '/', 'c', 'y', 'g', 'd', 'r', 'i', 'v', 'e' };
  VAR
    rootlen : CARDINAL := 0;
    d_sep := DirSep [host_os];
    start : CARDINAL := 0;
    len := NUMBER (p);
    ch := '\000';
    q : REF ARRAY OF CHAR;
    to : CARDINAL := 0;
  BEGIN

    IF len = 0 THEN RETURN "" END;

    (* convert all slashes to native *)
    FOR i := start TO (start + len - 1) DO
      ch := p[i];
      IF (ch # d_sep) AND (ch = '/' OR ch = '\\') THEN
        p[i] := d_sep;
      END;
    END;

    (* remove trailing slashes, leaving at most one *)
    (* check for length 3 here so we don't encroach on the leading slashes *)
    (* Trailing slashes break some code so for now don't do this.
    WHILE (len >= 3) AND (p[start + len - 1] = d_sep) AND (p[start + len - 2] = d_sep) DO
      DEC (len);
    END;
    *)
    (* remove all trailing slashes *)
    (* check for length 3 here so we don't encroach on the leading slashes *)
    WHILE (len >= 3) AND (p[start + len - 1] = d_sep) DO
      DEC (len);
    END;

    IF len = 0 THEN RETURN "" END;

    (* remove leading slashes, leaving at most two *)
    WHILE (len >= 3) AND (p[start] = d_sep) AND (p[start + 1] = d_sep) AND (p[start + 2] = d_sep) DO
      INC (start);
      DEC (len);
    END;

    IF len = 0 THEN RETURN "" END;

    (* change all runs of slashes to single slashes, except again allowing two at the start *)

    ch := '\000';
    to := 1;
    FOR from := 1 TO len - 1 DO
      IF (ch # d_sep) OR (p[from + start] # d_sep) THEN
        ch := p[from + start];
        p[to + start] := ch;
        INC(to);
      ELSE
      END;
    END;
    len := to;

    IF ((host_os = OSKind.Unix) OR (host_os = OSKind.GrumpyUnix))
        AND (len > 2)
        AND (p[start + 1] = ':')
        AND (p[start + 2] = '/')
        AND (p[start] IN Letters) THEN
      (* c:\foo => /cygdrive + /c/foo *)
      p[start + 1] := p[start];
      p[start] := '/';
      rootlen := NUMBER (root);
    END;

    (* NOTE Cygwin paths do NOT necessarily start /cygdrive -- e.g. /bin/sh.
    There IS a Win32 equivalent of such paths, e.g. c:\cygwin\bin\sh, but we don't
    provide it. *)

    IF (host_os = OSKind.Win32)
        AND (len > 11)
        AND (p[start] = '\\')
        AND (ASCII.Lower[p[start + 1]] = 'c')
        AND (ASCII.Lower[p[start + 2]] = 'y')
        AND (ASCII.Lower[p[start + 3]] = 'g')
        AND (ASCII.Lower[p[start + 4]] = 'd')
        AND (ASCII.Lower[p[start + 5]] = 'r')
        AND (ASCII.Lower[p[start + 6]] = 'i')
        AND (ASCII.Lower[p[start + 7]] = 'v')
        AND (ASCII.Lower[p[start + 8]] = 'e')
        AND (p[start + 9] = '\\')
        AND (p[start + 10] IN Letters)
        AND (p[start + 11] = '\\')
        THEN
      (* 012345678901 *)
      (* /cygdrive/c/ => c:\ *)
      p[start + 9] := p[start + 10];
      p[start + 10] := ':';
      p[start + 11] := '\\';
      INC (start, 9);
      DEC (len, 9);
    END;

    IF rootlen = 0 THEN
      RETURN FixPath (SUBARRAY (p, start, len));
    ELSE
      q := NEW(REF ARRAY OF CHAR, (rootlen + len));
      SUBARRAY(q^, 0, rootlen) := root;
      SUBARRAY(q^, rootlen, len) := SUBARRAY (p, start, len);
      RETURN FixPath (q^);
    END;
  END PathLooselyConvertUserInputToHost_ArrayToText;

PROCEDURE IsPathVariableName(a: TEXT): BOOLEAN =
VAR length : CARDINAL := 0;
    ch4: CHAR;
BEGIN
 (* future: write a tool to generate code like this *)
  IF a # NIL THEN
    length := Text.Length (a);
  END;
  IF (length < 8) OR (length > 16) THEN
    RETURN FALSE;
  END;
  ch4 := Text.GetChar (a, 4);
  CASE (length - 8) OF <* NOWARN *> (* all cases are handled, but the compiler can't tell *)
    |  0 =>
      CASE ch4 OF
        | 'R' =>
          RETURN Text.Equal (a, "CM3_ROOT");
        | 'N' =>
          RETURN Text.Equal (a, "M3CONFIG");
        ELSE
          RETURN FALSE;
      END;
    | 1 =>
      RETURN FALSE;
    | 2 =>
      RETURN (ch4 = 'C') AND Text.Equal (a, "CM3_CONFIG");
    | 3 =>
      CASE ch4 OF
        | 'I' =>
          RETURN Text.Equal (a, "CM3_INSTALL");
        | 'A' =>
          RETURN Text.Equal (a, "INSTALLROOT");
        ELSE
          RETURN FALSE;
        END;
    | 4 =>
        RETURN (ch4 = 'A') AND Text.Equal (a, "INSTALL_ROOT");
    | 5 =>
      RETURN FALSE;
    | 6 =>
        RETURN (ch4 = 'S') AND Text.Equal (a, "CM3_SOURCEROOT");
    | 7 =>
      CASE ch4 OF
        | 'I' =>
          RETURN Text.Equal (a, "CM3_INSTALLROOT");
        | 'S' =>
          RETURN Text.Equal (a, "CM3_SOURCE_ROOT");
        ELSE
          RETURN FALSE;
        END;
    | 8 =>
        RETURN (ch4 = 'I') AND Text.Equal (a, "CM3_INSTALL_ROOT");
  END;
END IsPathVariableName;

BEGIN
  (* Probe the host for what slash it uses, and default host and
    target "naming conventions" based on that.
  *)
  IF Text.GetChar (Pathname.Join ("a", "b"), 1) = BackSlash THEN
    SlashText := "\\";
    SetOS (OSKind.Win32, TRUE);
    SetOS (OSKind.Win32, FALSE);
  END;

  (* set callbacks until M3Path is moved to QPath *)

  QMachine.PathLooselyConvertUserInputToHost_TextToText := PathLooselyConvertUserInputToHost_TextToText;
  QMachine.IsPathVariableName := IsPathVariableName;

END M3Path.
