(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Mon Sep 26 09:07:50 PDT 1994 by kalsow     *)
(*      modified on Mon Oct 25 10:31:06 PDT 1993 by mcjones    *)
(*      modified on Wed May 12 16:56:05 PDT 1993 by meehan     *)
(*      modified on Mon May 10 20:58:46 PDT 1993 by mjordan    *)

MODULE M3Path;

IMPORT Pathname, Text, TextF;

CONST
  Null      = '\000';
  Colon     = ':';
  Slash     = '/';
  BackSlash = '\\';

CONST
  DirSep = ARRAY OSKind OF CHAR { Slash,  Slash,  BackSlash };
  VolSep = ARRAY OSKind OF CHAR { Null,   Null,   Colon  };

CONST
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

CONST
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

CONST
  Default_pgm = ARRAY OSKind OF TEXT { "a.out", "a.out", "NONAME.EXE" };

VAR
  os_map := ARRAY BOOLEAN OF OSKind { OSKind.Unix, OSKind.Unix };
  lcase  : ARRAY CHAR OF CHAR;

PROCEDURE SetOS (kind: OSKind;  host: BOOLEAN) =
  BEGIN
    os_map [host] := kind;
  END SetOS;

PROCEDURE New (a, b, c, d: TEXT := NIL): TEXT =
  BEGIN
    IF (b # NIL) THEN a := Pathname.Join (a, b, NIL); END;
    IF (c # NIL) THEN a := Pathname.Join (a, c, NIL); END;
    IF (d # NIL) THEN a := Pathname.Join (a, d, NIL); END;
    RETURN FixPath (a, host := TRUE);
  END New;

PROCEDURE Join (dir, base: TEXT;  k: Kind;  host: BOOLEAN): TEXT =
  VAR
    len    := 0;
    os     := os_map [host];
    pre    := Prefix [os][k];
    ext    := Suffix [os][k];
    d_sep  := DirSep [os];
    v_sep  := VolSep [os];
    result : TEXT;
  BEGIN
    (* find out how much space we need *)
    IF (dir # NIL) THEN
      len := LAST (dir^);
      IF (dir[len-1] # d_sep) AND (dir[len-1] # v_sep) THEN INC (len); END;
    END;
    INC (len, LAST (pre^));
    INC (len, LAST (base^));
    INC (len, LAST (ext^));

    (* allocate it and fill it in *)
    result := TextF.New (len);
    len := 0;
    IF (dir # NIL) THEN
      len := Append (result, dir, 0);
      IF (dir[len-1] # d_sep) AND (dir[len-1] # v_sep) THEN
        result [len] := d_sep; INC (len);
      END;
    END;
    len := Append (result, pre, len);
    len := Append (result, base, len);
    len := Append (result, ext, len);
    RETURN FixPath (result, host);
  END Join;

PROCEDURE Append (a, b: TEXT;  start: INTEGER): INTEGER =
  BEGIN
    SUBARRAY (a^, start, NUMBER (b^)) := b^;
    RETURN start + LAST (b^);
  END Append;

PROCEDURE Parse (nm: TEXT;  host: BOOLEAN): T =
  VAR
    t       : T;
    len     := LAST (nm^);
    base_len:= 0;
    d_index := -1;
    v_index := -1;
    start   := 0;
    os      := os_map [host];
    d_sep   := DirSep [os];
    v_sep   := VolSep [os];
    ext     : TEXT;
    ext_len : INTEGER;
    pre     : TEXT;
  BEGIN
    (* find the last instance of each separator *)
    FOR i := 0 TO len-1 DO IF (nm[i] = v_sep) THEN v_index := i; END; END;
    FOR i := 0 TO len-1 DO IF (nm[i] = d_sep) THEN d_index := i; END; END;

    (* extract the prefix *)
    IF (v_index = -1) AND (d_index = -1) THEN
      (* no separators *)
      t.dir := NIL;
      start := 0;
    ELSIF (d_index = -1) THEN
      (* no directory separator, only a volume separator *)
      t.dir := Text.FromChars (SUBARRAY (nm^, 0, v_index+1));
      start := v_index + 1;
    ELSIF (d_index = 0) THEN
      t.dir := DirSepText [os];
      start := 1;
    ELSE
      t.dir := Text.FromChars (SUBARRAY (nm^, 0, d_index));
      start := d_index+1;
    END;
    base_len := len - start;

    (* search for a matching suffix *)
    t.kind := Kind.Unknown;
    ext_len := 0;
    FOR k := FIRST (Kind) TO LAST (Kind) DO
      ext := Suffix [os][k];
      IF ExtMatch (nm, ext, os) THEN
        ext_len := Text.Length (ext);
        t.kind := k;
        EXIT;
      END;
    END;

    (* extract the base component *)
    t.base := Text.FromChars (SUBARRAY (nm^, start, base_len - ext_len));

    pre := Prefix[os][t.kind];
    IF (Text.Length (pre) > 0) AND PrefixMatch (t.base, pre, os) THEN
      t.base := Text.Sub (t.base, Text.Length (pre));
    END;

    RETURN t;
  END Parse;

PROCEDURE IsEqual (a, b: TEXT): BOOLEAN =
  VAR len := LAST (a^);
  BEGIN
    IF (len # LAST (b^)) THEN RETURN FALSE; END;
    IF os_map [TRUE] = OSKind.Win32 THEN
      FOR i := 0 TO len - 1 DO
        IF lcase[a[i]] # lcase[b[i]] THEN RETURN FALSE; END;
      END;
    ELSE
      FOR i := 0 TO len - 1 DO
        IF a[i] # b[i] THEN RETURN FALSE; END;
      END;
    END;
    RETURN TRUE;
  END IsEqual;

PROCEDURE ExtMatch (nm, ext: TEXT;  os: OSKind): BOOLEAN =
  VAR
    nm_len  := LAST (nm^);
    ext_len := LAST (ext^);
    j := 0;
  BEGIN
    IF (nm_len < ext_len) THEN RETURN FALSE END;
    IF (ext_len <= 0) THEN RETURN FALSE END;
    IF (os = OSKind.Win32) THEN
      (* ignore case *)
      FOR i := nm_len-ext_len TO nm_len-1 DO
        IF lcase [nm[i]] # ext[j] THEN RETURN FALSE; END;
        INC (j);
      END;
    ELSE
      FOR i := nm_len-ext_len TO nm_len-1 DO
        IF nm[i] # ext[j] THEN RETURN FALSE; END;
        INC (j);
      END;
    END;
    RETURN TRUE;
  END ExtMatch;

PROCEDURE PrefixMatch (nm, pre: TEXT;  os: OSKind): BOOLEAN =
  VAR
    nm_len  := LAST (nm^);
    pre_len := LAST (pre^);
  BEGIN
    IF (nm_len < pre_len) THEN RETURN FALSE END;
    IF (os = OSKind.Win32) THEN
      (* ignore case *)
      FOR i := 0 TO pre_len-1 DO
        IF lcase [nm[i]] # pre[i] THEN RETURN FALSE; END;
      END;
    ELSE
      FOR i := 0 TO pre_len-1 DO
        IF nm[i] # pre[i] THEN RETURN FALSE; END;
      END;
    END;
    RETURN TRUE;
  END PrefixMatch;

PROCEDURE EndOfArc (path: TEXT;  xx: CARDINAL;  os: OSKind): BOOLEAN =
  VAR len := LAST (path^);
  BEGIN
    RETURN (len = xx) OR ((len > xx) AND (path[xx] = DirSep[os]));
  END EndOfArc;

PROCEDURE DefaultProgram (host: BOOLEAN): TEXT =
  BEGIN
    RETURN Default_pgm [os_map [host]];
  END DefaultProgram;

PROCEDURE ProgramName (base: TEXT;  host: BOOLEAN): TEXT =
  VAR os := os_map [host];
  BEGIN
    RETURN base & Suffix[os][Kind.PGM];
  END ProgramName;

PROCEDURE LibraryName (base: TEXT;  host: BOOLEAN): TEXT =
  VAR os := os_map [host];
  BEGIN
    RETURN Prefix[os][Kind.LIB] & base & Suffix[os][Kind.LIB];
  END LibraryName;

PROCEDURE Convert (nm: TEXT;  host: BOOLEAN): TEXT =
  VAR
    len  := LAST (nm^);
    res  := TextF.New (len);
    good := DirSep [os_map [host]];
    bad  := DirSep [os_map [NOT host]];
  BEGIN
    FOR i := 0 TO len-1 DO
      IF (nm[i] = bad)
        THEN res[i] := good;
        ELSE res[i] := nm[i];
      END;
    END;
    RETURN res;
  END Convert;

PROCEDURE Escape (nm: TEXT): TEXT =
  VAR n_escapes := 0;  len: INTEGER;
  BEGIN
    IF (nm = NIL) THEN RETURN NIL; END;
    len := LAST (nm^);
    FOR i := 0 TO len-1 DO
      IF (nm[i] = BackSlash) THEN INC (n_escapes); END;
    END;
    IF (n_escapes = 0) THEN RETURN nm; END;
    VAR res := TextF.New (len + n_escapes);  j := 0;  BEGIN
      FOR i := 0 TO len-1 DO
        res[j] := nm[i];  INC (j);
        IF (nm[i] = BackSlash) THEN res[j] := BackSlash;  INC (j); END;
      END;
      RETURN res;
    END;
  END Escape;

PROCEDURE MakeRelative (VAR path: TEXT;  full, rel: TEXT): BOOLEAN =
  BEGIN
    IF PrefixMatch (path, full, os_map[TRUE])
      AND EndOfArc (path, Text.Length (full), os_map[TRUE]) THEN
      path := New (rel, Text.Sub (path, Text.Length (full)));
      RETURN TRUE;
    ELSE
      RETURN FALSE;
    END;
  END MakeRelative;

(******
PROCEDURE FixPath (p: TEXT;  host: BOOLEAN): TEXT =
  VAR p0 := FixPathX (p, host);
  BEGIN
    IF (p # p0) THEN
      Msg.Debug ("FIX: ", p, " -> ", p0);
      Msg.Debug ("\n\r");
    END;
    RETURN p0;
  END FixPath;
********)

TYPE
  SepInfo = RECORD
    d_sep : CHAR;
    v_sep : CHAR;
    dots  : BOOLEAN;
    cnt   : INTEGER;
    loc   : ARRAY [0..31] OF INTEGER;
  END;

PROCEDURE FixPath (p: TEXT;  host: BOOLEAN): TEXT =
  (* remove redundant "/arc/../" and "/./" segments *)
  VAR os := os_map [host];  x, s0, s1, s2: INTEGER;  info: SepInfo;
  BEGIN
    info.d_sep := DirSep [os];
    info.v_sep := VolSep [os];

    FindSeps (p, info);  x := 1;
    WHILE (info.dots) AND (x < info.cnt-1) DO
      s0 := info.loc[x-1];
      s1 := info.loc[x];
      s2 := info.loc[x+1];
      IF (s1 - s0 = 2) AND (p[s0+1] = '.') AND (p[s1] = info.d_sep) THEN
        (* found a /./ arc  => remove it *)
        p := CutSection (p, s0+1, s1);
        FindSeps (p, info);  x := 1;  (* restart the scan *)
      ELSIF (s2 - s1 = 3)
        AND (p[s1+1] = '.') AND (p[s1+2] = '.')
        AND (p[s1] = info.d_sep)
        AND ((p[s1-1] # '.') OR (p[s1-2] # '.')) THEN
        (* found a /<foo>/../ segment => remove it *)
        p := CutSection (p, s0+1, s2);
        FindSeps (p, info);  x := 1;  (* restart the scan *)
      ELSE
        (* found nothing... *)
        INC (x);
      END;
    END;

    (* remove trailing slashs *)
    x := LAST (p^);
    WHILE (x > 1) AND (p[x-1] = info.d_sep) DO
      p := Text.Sub (p, 0, x-1);
      x := LAST (p^);
    END;

    IF x <= 0 THEN RETURN "."; END;
    RETURN p;
  END FixPath;

PROCEDURE FindSeps (txt: TEXT;  VAR info: SepInfo) =
  VAR len := LAST (txt^);  c: CHAR;
  BEGIN
    info.dots := FALSE;
    info.loc[0] := -1;  info.cnt := 1;  (* initial marker *)
    FOR i := 0 TO len-1 DO
      c := txt[i];
      IF (c = info.d_sep) OR (c = info.v_sep) THEN
        IF (info.cnt >= LAST (info.loc)) THEN EXIT; (*give up*) END;
        info.loc[info.cnt] := i;  INC (info.cnt);
      ELSIF (c = '.') THEN
        info.dots := TRUE;
      END;
    END;
    info.loc[info.cnt] := len;  INC (info.cnt);  (* final marker *)
  END FindSeps;

PROCEDURE CutSection (txt: TEXT;  start, stop: INTEGER): TEXT =
  VAR len := LAST (txt^);  chop: INTEGER;   x: TEXT;
  BEGIN
    start := MAX (0, MIN (start, len));
    stop := MAX (0, MIN (stop+1, len));
    chop := stop - start;
    x := TextF.New (len - chop);
    IF (start > 0) THEN
      SUBARRAY (x^, 0, start) := SUBARRAY (txt^, 0, start);
    END;
    chop := len - chop - start;
    IF (chop > 0) THEN
      SUBARRAY (x^, start, chop) := SUBARRAY (txt^, stop, chop);
    END;
    RETURN x;
  END CutSection;

BEGIN
  FOR i := FIRST (lcase) TO LAST (lcase) DO lcase[i] := i; END;
  FOR i := 'A' TO 'Z' DO
    lcase[i] := VAL (ORD (i) - ORD ('A') + ORD ('a'), CHAR);
  END;

  (* guess some reasonable defaults for this platform *)
  CONST XX = ARRAY BOOLEAN OF OSKind { OSKind.Win32, OSKind.Unix };
  VAR   k := XX [Text.Equal (Pathname.Join ("a", "b", NIL), "a/b")];
  BEGIN
    os_map [TRUE]  := k;
    os_map [FALSE] := k;
  END;
END M3Path.
