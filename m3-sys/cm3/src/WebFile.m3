(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Mon Apr 17 09:00:54 PDT 1995 by kalsow     *)

MODULE WebFile;

IMPORT IntIntTbl, ETimer, File, Text, OSError;
IMPORT M3Buf, M3ID, M3File, Target;
IMPORT M3Path, Utils, Msg;

CONST
  InfoFile = ".M3WEB";

TYPE
  InfoEntry = REF RECORD
    next  : InfoEntry;
    file  : M3ID.T;
    txt   : TEXT;
    chars : CharList;
    start : INTEGER;
    len   : INTEGER;
  END;

TYPE
  CharList = REF ARRAY OF CHAR;

VAR
  new_info : InfoEntry := NIL;

PROCEDURE Update (file, info: TEXT) =
  VAR
    x  := NEW (InfoEntry);
    nm := M3Path.Parse (file, host := TRUE);
    id := M3ID.Add (M3Path.Join (NIL, nm.base, nm.kind, host := FALSE));
  BEGIN
    x.next  := new_info;  new_info := x;
    x.file  := id;
    x.txt   := info;
    x.chars := NIL;
    x.start := 0;
    x.len   := Text.Length (info);
  END Update;

PROCEDURE Dump () =
  VAR x: InfoEntry;
  BEGIN
    IF (new_info = NIL) THEN RETURN END;
    x := new_info;
    new_info := NIL;  (* in case we die while updating *)

    ETimer.Push (ETimer.New ("updating web info"));
      DumpFile (x, ParseFile (Inhale (), x));
    ETimer.Pop ();
  END Dump;

PROCEDURE Inhale (): CharList =
  VAR
    rd  : File.T;
    buf : CharList := NIL;
    len : INTEGER;
    xxx : INTEGER;
  BEGIN
    IF Utils.LocalModTime (InfoFile) = Utils.NO_TIME THEN RETURN NIL END;
    rd  := Utils.OpenReader (InfoFile, fatal := FALSE);
    IF (rd = NIL) THEN RETURN NIL END;
    TRY
      len := rd.status().size;
      IF (len > 0) THEN
        buf := NEW (CharList, len);
        xxx := M3File.Read (rd, buf^, len);
      END;
    EXCEPT OSError.E(ec) =>
      Msg.Error (ec, "unable to read ", InfoFile);
      buf := NIL;
    END;
    Utils.CloseReader (rd, InfoFile);
    IF (buf # NIL) AND (len # NUMBER(buf^)) THEN
      Msg.Error (NIL, "unable to read ", InfoFile);
      buf := NIL;
    END;
    RETURN buf;
  END Inhale;

PROCEDURE ParseFile (txt: CharList;  updates: InfoEntry): InfoEntry =
  VAR
    cursor := 0;
    name_start, name_len: INTEGER;
    len, entry_len, body_cursor: INTEGER;
    updated: IntIntTbl.T;
    key, val: INTEGER;
    survivors, x: InfoEntry := NIL;
    ch: CHAR;
  BEGIN
    IF (txt = NIL) THEN RETURN NIL; END;
    len := NUMBER (txt^);
    IF (len <= 0) THEN RETURN NIL END;

    (* build a table of the updated entries *)
    updated := NEW (IntIntTbl.Default).init ();
    WHILE (updates # NIL) DO
      EVAL updated.put (updates.file, 0);
      updates := updates.next;
    END;

    (* read the table of contents *)
    body_cursor := 0;
    LOOP
      IF (cursor >= len) THEN RETURN BadFile("eof in toc"); END;

      (* read a file name *)
      name_start := cursor;
      WHILE (cursor < len) AND (txt[cursor] # ' ') DO INC (cursor); END;
      name_len := cursor - name_start;
      IF (cursor >= len) THEN RETURN BadFile ("eof in name"); END;
      INC (cursor);

      (* read a length *)
      entry_len := 0;  ch := '0';
      LOOP
        IF (cursor >= len) THEN RETURN BadFile ("eof in len"); END;
        ch := txt[cursor]; INC (cursor);
        IF (ch < '0') OR ('9' < ch) THEN EXIT END;
        entry_len := 10 * entry_len + ORD (ch) - ORD ('0');
      END;
      DEC (cursor);

      (* skip to next line *)
      WHILE (cursor < len) AND (txt[cursor] # '\n') DO INC (cursor) END;
      INC (cursor);

      IF (name_len = 1) AND (txt[name_start] = '$') THEN EXIT END;

      key := M3ID.FromStr (SUBARRAY (txt^, name_start, name_len));
      IF NOT updated.get (key, val) THEN
        (* save this entry *)
        survivors := NEW (InfoEntry, next := survivors, file := key,
                          txt := NIL, chars := txt,
                          start := body_cursor, len := entry_len);
                           
      END;
      INC (body_cursor, entry_len);
    END;

    (* put the list back in file order *)
    survivors := ReverseList (survivors);

    (* and patch the entries to compensate for the table of contents length *)
    x := survivors;
    WHILE (x # NIL) DO
      INC (x.start, cursor);
      IF (x.start > len) THEN RETURN BadFile ("eof in body"); END;
      x := x.next;
    END;

    RETURN survivors;
  END ParseFile;

PROCEDURE ReverseList (a: InfoEntry): InfoEntry =
  VAR b, c: InfoEntry;
  BEGIN
    b := NIL;
    WHILE (a # NIL) DO
      c := a.next;
      a.next := b;
      b := a;
      a := c;
    END;
    RETURN b;
  END ReverseList;

PROCEDURE BadFile (msg: TEXT): InfoEntry =
  BEGIN
    Msg.Error (NIL, "web info file \"" & InfoFile & "\" is damaged: ", msg);
    RETURN NIL;
  END BadFile;

PROCEDURE DumpFile (a, b: InfoEntry) =
  VAR
    wr := Utils.OpenWriter (InfoFile, fatal := TRUE);
    buf := M3Buf.New ();
  BEGIN
    M3Buf.AttachDrain (buf, wr);
    DumpHeaders (buf, a);
    DumpHeaders (buf, b);
    M3Buf.PutText (buf, "$ 0");
    M3Buf.PutText (buf, Target.EOL);
    DumpBodies (buf, a);
    DumpBodies (buf, b);
    M3Buf.Flush (buf, wr);
    Utils.CloseWriter (wr, InfoFile);
  END DumpFile;

PROCEDURE DumpHeaders (buf: M3Buf.T;  e: InfoEntry) =
  BEGIN
    WHILE (e # NIL) DO
      M3ID.Put      (buf, e.file);
      M3Buf.PutChar (buf, ' ');
      M3Buf.PutInt  (buf, e.len);
      M3Buf.PutText (buf, Target.EOL);
      e := e.next;
    END;
  END DumpHeaders;

PROCEDURE DumpBodies (buf: M3Buf.T;  e: InfoEntry) =
  BEGIN
    WHILE (e # NIL) DO
      IF (e.txt # NIL)
        THEN M3Buf.PutSubText (buf, e.txt, e.start, e.len);
        ELSE M3Buf.PutSub (buf, SUBARRAY (e.chars^, e.start, e.len));
      END;
      e := e.next;
    END;
  END DumpBodies;

BEGIN
END WebFile.
