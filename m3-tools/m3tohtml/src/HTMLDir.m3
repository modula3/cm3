(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Jun  1 10:21:26 PDT 1995 by kalsow                   *)

MODULE HTMLDir;

IMPORT Pathname, Fmt, Text, Wr, FileWr, FilePath;

TYPE
  Node = RECORD
    key    : TEXT    := NIL;
    name   : TEXT    := NIL;
    count  : INTEGER := 0;
    start  : INTEGER := 0;
  END;

TYPE
  State = RECORD
    base_wr : Wr.T;
    file    : TEXT;
    title   : TEXT;
    limit   : INTEGER;
    next_id : INTEGER;
    max_len : INTEGER;
    n_elts  : INTEGER;
    elts    : REF ARRAY OF Node;
  END;

PROCEDURE GenDir (READONLY names: ARRAY OF TEXT;
                  wr: Wr.T;
                  file, title: TEXT;
                  limit: INTEGER) =
  VAR s: State;
  BEGIN
    s.base_wr := wr;
    s.file    := file;
    s.title   := title;
    s.limit   := limit;
    s.next_id := 1;

    (* build the initial sorted list of nodes *)
    s.elts := NEW (REF ARRAY OF Node, NUMBER (names));
    FOR i := 0 TO LAST (names) DO
      WITH x = s.elts[i] DO
        x.name  := names[i];
        x.key   := Pathname.LastBase (names[i]);
        x.count := 0;
        x.start := 0;
      END;
    END;
    Sort (s.elts^);

    (* Find and remove entries with duplicate keys *)
    s.n_elts := 0;
    VAR i := 0; BEGIN
      WHILE (i < NUMBER (s.elts^)) DO
        VAR j := i+1; BEGIN
          WHILE (j < NUMBER (s.elts^))
            AND Text.Equal (s.elts[i].key, s.elts[j].key) DO
            INC (j);
          END;
          IF (j - i > 1) THEN RemoveDuplicates (s, i, j-i); END;
          IF (i # s.n_elts) THEN s.elts[s.n_elts] := s.elts[i]; END;
          INC (s.n_elts);
          i := j;
        END;
      END;
    END;

    (* find the length of the longest key *)
    s.max_len := 0;
    FOR i := 0 TO s.n_elts-1 DO
      s.max_len := MAX (s.max_len, Text.Length (s.elts[i].key));
    END;

    EVAL Generate (s, 0, 0, s.n_elts, 0);

    s.elts := NIL;
  END GenDir;

PROCEDURE RemoveDuplicates (VAR s: State;  start, len: INTEGER) =
  <*FATAL ANY*>
  VAR
    file  := FName (s, s.next_id);
    wr    := FileWr.Open (file & ".html");
    key   := s.elts[start].key;
  BEGIN
    INC (s.next_id);
    Out (wr, "<HTML>\n<HEAD>\n<TITLE>", key, " locations</TITLE>\n</HEAD>\n");
    Out (wr, "<BODY>\n<H2>", key, " is located in:<H2>\n<UL>\n");
    FOR i := start TO start + len - 1 DO
      VAR nm := s.elts[i].name; BEGIN
        Out (wr, "<LI><A HREF=\"", FilePath.Normalize (nm, file), ".html\">");
        Out (wr, nm, "</A>\n");
      END;
    END;
    Out (wr, "</UL>\n</BODY>\n</HTML>\n");
    Wr.Close (wr);
    s.elts[start].name := file;
  END RemoveDuplicates;

PROCEDURE Generate (VAR s: State;  id, start, cnt, prefix: INTEGER): TEXT =
  VAR cnt0, cnt1, len: INTEGER;  file := FName (s, id);
  BEGIN
    (* count the elements with the specified prefix length *)
    len  := prefix;
    cnt1 := CntPrefixes (s, start, cnt, len);

    (* find a prefix that generates a non-trivial choice *)
    WHILE (len <= s.max_len) AND (cnt1 < 2) DO
      INC (len);
      cnt1 := CntPrefixes (s, start, cnt, len);
    END;

    (* find the largest prefix that's got fewer than limit classes *)
    REPEAT
      INC (len);
      cnt0 := cnt1;
      cnt1 := CntPrefixes (s, start, cnt, len);
    UNTIL (len >= s.max_len) OR (cnt1 > s.limit);

    (* pick the best size *)
    IF (s.limit - cnt0 <= cnt1 - s.limit) THEN
      (* use the shorter prefix *)
      DEC (len);
      cnt1 := CntPrefixes (s, start, cnt, len);
    END;

    (* generate the smaller, non-trivial partitions *)
    FOR i := start TO start+cnt-1 DO
      IF s.elts[i].count > 1 THEN
        VAR
          id    := s.next_id;
          start := s.elts[i].start;
          count := s.elts[i].count;
          name  : TEXT;
        BEGIN
          INC (s.next_id);
          name := Generate (s, id, start, count, len+1);
          ResetElts (s, start, count, i, name);
        END;
      END;
    END;

    (* generate my elements *)
    WriteDir (s, file, start, cnt, len);

    RETURN file;
  END Generate;

PROCEDURE CntPrefixes (VAR s: State;  start, cnt, len: INTEGER): INTEGER =
  VAR
    n_classes := 1;
    class     := start;
    c_len     := Text.Length (s.elts[start].key);
  BEGIN
    s.elts[start].count := 1;
    s.elts[start].start := start;
    FOR i := start + 1 TO start + cnt - 1 DO
      IF PrefixMatch (s.elts[class].key, s.elts[i].key, len) THEN
        INC (s.elts[class].count);
        IF (c_len < len) AND (c_len < Text.Length (s.elts[i].key)) THEN
          (* move the class rep *)
          s.elts[i].count := s.elts[class].count;
          s.elts[i].start := s.elts[class].start;
          s.elts[class].count := 0;
          class := i;
        END;
      ELSE
        INC (n_classes);
        class := i;
        c_len := Text.Length (s.elts[i].key);
        s.elts[i].count := 1;
        s.elts[i].start := i;
      END;
    END;
    RETURN n_classes;
  END CntPrefixes;

PROCEDURE ResetElts (VAR s: State;  start, count, base: INTEGER;  file: TEXT) =
  BEGIN
    FOR i := start TO start + count -1 DO
      s.elts[i].count := 0;
    END;
    s.elts[base].count := count;
    s.elts[base].start := start;
    s.elts[base].name  := file;
  END ResetElts;

PROCEDURE WriteDir (VAR s: State;  file: TEXT;  start, cnt, len: INTEGER) =
  <*FATAL ANY*>
  CONST Dir_width = 78; (* max # characters per line *)
  CONST Max_cols  = 6;  (* max # columns per line *)
  CONST Gap       = 2;  (* inter-column gap *)
  CONST Gap_text  = "  ";
  VAR
    max_len    := 0;
    n_cols     := 1;
    width      : CARDINAL;
    n_rows     : CARDINAL;
    j          : CARDINAL;
    nm         : TEXT;
    nm_len     : INTEGER;
    elts       := NEW (REF ARRAY OF INTEGER, cnt);
    n_elts     := 0;
    wr         : Wr.T;
    base_file  : TEXT;
  BEGIN

    (* find the longest name and pack the elements *)
    FOR i := start TO start+cnt-1 DO
      VAR n := s.elts[i].count; BEGIN
        IF n > 0 THEN
          elts[n_elts] := i;  INC (n_elts);
          IF n = 1
            THEN max_len := MAX (max_len, Text.Length (s.elts[i].key));
            ELSE max_len := MAX (max_len, len+3);
          END;
        END;
      END;
    END;

    (* compute an approriate layout *)
    max_len := MAX (5, max_len);
    INC (max_len, Gap);
    n_cols := MAX (1, MIN (Dir_width DIV max_len, Max_cols));
    n_rows := (n_elts + n_cols - 1) DIV n_cols;
    width  := Dir_width DIV n_cols - Gap;

    IF (file = NIL) THEN
      base_file := "";
      wr := s.base_wr;
    ELSE
      base_file := file;
      wr := FileWr.Open (file & ".html");
      Out (wr, "<HTML>\n<HEAD>\n<TITLE>", s.title,
               "</TITLE>\n</HEAD>\n<BODY>\n");
    END;
    Out (wr, "<PRE>\n");
    FOR row := 0 TO n_rows-1 DO
      FOR col := 0 TO n_cols-1 DO
        j := col * n_rows + row;
        IF (j < n_elts) THEN
          WITH x = s.elts[elts[j]] DO
            Out (wr, "<A HREF=\"", FilePath.Normalize (x.name, base_file), ".html\">");
            nm := x.key;
            nm_len := Text.Length (nm);
            IF (x.count > 1) THEN
              VAR xxx := MIN (FindMaxLen (s, elts[j], len), width-4); BEGIN
                IF (nm_len > xxx) THEN
                  nm := Text.Sub (nm, 0, xxx);
                  nm_len := xxx;
                END;
              END;
              Out (wr, nm, "...");
              INC (nm_len, 3);
            ELSE
              Out (wr, nm);
            END;
          END;

          Out (wr, "</A>");
          IF (col # n_cols-1) THEN
            (* pad to the next column *)
            FOR x := 1 TO width - nm_len DO Out (wr, " "); END;
          END;
          Out (wr, Gap_text);
        END;
      END;
      Out (wr, "\n");
    END;
    Out (wr, "</PRE>\n");
    IF (file # NIL) THEN
      Out (wr, "</BODY>\n</HTML>\n");
      Wr.Close (wr);
    END;
  END WriteDir;

PROCEDURE FindMaxLen (VAR s: State;  base, len: INTEGER): INTEGER =
  VAR
    start := s.elts[base].start;
    cnt   := s.elts[base].count;
    key   := s.elts[base].key;
    max   := Text.Length (key);
  BEGIN
    WHILE (len < max) DO
      INC (len);
      FOR i := start TO start + cnt - 1 DO
        IF NOT PrefixMatch (s.elts[i].key, key, len) THEN RETURN len-1 END;
      END;
    END;
    RETURN len;
  END FindMaxLen;

PROCEDURE PrefixMatch (a, b: TEXT;  len: INTEGER): BOOLEAN =
  VAR min := MIN (MIN (Text.Length (a), Text.Length (b)), len);
  BEGIN
    FOR i := 0 TO min-1 DO
      IF Text.GetChar (a, i) # Text.GetChar (b, i) THEN RETURN FALSE END;
    END;
    RETURN (min = len);
  END PrefixMatch;

PROCEDURE Out (wr: Wr.T;  a, b, c, d: TEXT := NIL) =
  <*FATAL ANY*>
  BEGIN
    IF (a # NIL) THEN Wr.PutText (wr, a); END;
    IF (b # NIL) THEN Wr.PutText (wr, b); END;
    IF (c # NIL) THEN Wr.PutText (wr, c); END;
    IF (d # NIL) THEN Wr.PutText (wr, d); END;
  END Out;

(*--------------------------------------------------------------- sorting ---*)

TYPE  Elem_T = Node;

PROCEDURE Elem_Compare (a, b: Node): [-1..1] =
  BEGIN
    RETURN Text.Compare (a.key, b.key);
  END Elem_Compare;

PROCEDURE Sort (VAR a: ARRAY OF Elem_T;  cmp := Elem_Compare) =
  BEGIN
    QuickSort (a, 0, NUMBER (a), cmp);
    InsertionSort (a, 0, NUMBER (a), cmp);
  END Sort;

PROCEDURE QuickSort (VAR a: ARRAY OF Elem_T;  lo, hi: INTEGER;
                     cmp := Elem_Compare) =
  CONST CutOff = 9;
  VAR i, j: INTEGER;  key, tmp: Elem_T;
  BEGIN
    WHILE (hi - lo > CutOff) DO (* sort a[lo..hi) *)

      (* use median-of-3 to select a key *)
      i := (hi + lo) DIV 2;
      IF cmp (a[lo], a[i]) < 0 THEN
        IF cmp (a[i], a[hi-1]) < 0 THEN
          key := a[i];
        ELSIF cmp (a[lo], a[hi-1]) < 0 THEN
          key := a[hi-1];  a[hi-1] := a[i];  a[i] := key;
        ELSE
          key := a[lo];  a[lo] := a[hi-1];  a[hi-1] := a[i];  a[i] := key;
        END;
      ELSE (* a[lo] >= a[i] *)
        IF cmp (a[hi-1], a[i]) < 0 THEN
          key := a[i];  tmp := a[hi-1];  a[hi-1] := a[lo];  a[lo] := tmp;
        ELSIF cmp (a[lo], a[hi-1]) < 0 THEN
          key := a[lo];  a[lo] := a[i];  a[i] := key;
        ELSE
          key := a[hi-1];  a[hi-1] := a[lo];  a[lo] := a[i];  a[i] := key;
        END;
      END;

      (* partition the array *)
      i := lo+1;  j := hi-2;

      (* find the first hole *)
      WHILE cmp (a[j], key) > 0 DO DEC (j) END;
      tmp := a[j];
      DEC (j);

      LOOP
        IF (i > j) THEN EXIT END;

        WHILE cmp (a[i], key) < 0 DO INC (i) END;
        IF (i > j) THEN EXIT END;
        a[j+1] := a[i];
        INC (i);

        WHILE cmp (a[j], key) > 0 DO DEC (j) END;
        IF (i > j) THEN  IF (j = i-1) THEN  DEC (j)  END;  EXIT  END;
        a[i-1] := a[j];
        DEC (j);
      END;

      (* fill in the last hole *)
      a[j+1] := tmp;
      i := j+2;

      (* then, recursively sort the smaller subfile *)
      IF (i - lo < hi - i)
        THEN  QuickSort (a, lo, i-1);   lo := i;
        ELSE  QuickSort (a, i, hi);     hi := i-1;
      END;

    END; (* WHILE (hi-lo > CutOff) *)
  END QuickSort;


PROCEDURE InsertionSort (VAR a: ARRAY OF Elem_T;  lo, hi: INTEGER;
                         cmp := Elem_Compare) =
  VAR j: INTEGER;  key: Elem_T;
  BEGIN
    FOR i := lo+1 TO hi-1 DO
      key := a[i];
      j := i-1;
      WHILE (j >= lo) AND cmp (key, a[j]) < 0 DO
        a[j+1] := a[j];
        DEC (j);
      END;
      a[j+1] := key;
    END;
  END InsertionSort;

PROCEDURE FName (VAR s: State;  id: INTEGER): TEXT =
  BEGIN
    IF (id = 0) THEN RETURN NIL; END;
    RETURN Fmt.F ("%s_%s", s.file, Fmt.Int (id));
  END FName;

BEGIN
END HTMLDir.
