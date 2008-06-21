(* Copyright 1996, Critical Mass, Inc.  All rights reserved. *)

(* This module maintains a per-user, persistent table of key-value
   pairs. *)

MODULE UserState;

IMPORT IntArraySort, IntRefTbl, FileWr, Fmt, FS, M3Scanner;
IMPORT OS, OSError, Pathname, Process, Text, Thread, Word, Wr;
IMPORT Buf, ErrLog, ID, LexMisc;

CONST
  Escape = '\134'; (* back slash *)

VAR
  mu      : MUTEX;
  changed : Thread.Condition;
  tbl     : IntRefTbl.T;
  version : INTEGER := 0;
  save_dir: TEXT := NIL;
  dirty   : BOOLEAN := FALSE;
  fname   := ARRAY [0..1] OF TEXT { "CM3_IDE.cfg0", "CM3_IDE.cfg1" };

PROCEDURE Get (key_nm: TEXT): TEXT =
  VAR key := ID.Add (key_nm);  ref: REFANY;
  BEGIN
    LOCK mu DO
      IF NOT tbl.get (key, ref) THEN ref := NIL; END;
    END;
    RETURN ref;
  END Get;

PROCEDURE Put (key_nm, value: TEXT) =
  VAR key := ID.Add (key_nm);  ref: REFANY;
  BEGIN
    IF (value = NIL) THEN value := ""; END;
    LOCK mu DO
      IF tbl.get (key, ref) THEN
        IF NOT Text.Equal (ref, value) THEN
          EVAL tbl.put (key, value);
          dirty := TRUE;
          Thread.Signal (changed);
        END;
      ELSE
        EVAL tbl.put (key, value);
        dirty := TRUE;
        Thread.Signal (changed);
      END;
    END;
  END Put;

(*-------------------------------------------------------------- updating ---*)

TYPE UpdateClosure = Thread.Closure OBJECT OVERRIDES apply := DoUpdates; END;

PROCEDURE DoUpdates (<*UNUSED*> cl: UpdateClosure):  REFANY =
  VAR x: [0..1];
  BEGIN
    LOOP
      LOCK mu DO
        WHILE NOT dirty DO Thread.Wait (mu, changed); END;
        x := Word.And (version+1, 1);
        IF DumpTable (fname [x]) THEN RemoveTable (fname [1-x]); END;
        dirty := FALSE;
      END;
      Thread.Pause (10.0d0);  (* give everybody a break... *)
    END;
  END DoUpdates;

PROCEDURE DumpState () =
  (* called hastily during process shutdown... *)
  BEGIN
    IF dirty AND (save_dir # NIL) THEN
      INC (version);
      EVAL DumpTable (fname [Word.And (version, 1)]);
    END;
  END DumpState;

PROCEDURE DumpTable (fn: TEXT): BOOLEAN =
  VAR
    key: INTEGER;
    ref: REFANY;
    wr: Wr.T;
    iter: IntRefTbl.Iterator;
    t1, t2: TEXT;
    keys := NEW (REF ARRAY OF INTEGER, tbl.size ());

  PROCEDURE CmpKey (a, b: INTEGER): [-1 .. +1] =
    BEGIN
      IF    (a = b)        THEN RETURN  0;
      ELSIF (a = ID.NoID)  THEN RETURN +1;
      ELSIF (b = ID.NoID)  THEN RETURN -1;
      ELSIF ID.IsLT (a, b) THEN RETURN -1;
      ELSE                      RETURN +1;
      END;
    END CmpKey;

  BEGIN
    iter := tbl.iterate ();
    FOR i := FIRST (keys^) TO LAST (keys^) DO
      IF NOT iter.next (key, ref) THEN key := ID.NoID; END;
      keys[i] := key;
    END;
    IntArraySort.Sort (keys^, CmpKey);
    TRY
      wr := FileWr.Open (fn);
      TRY
        FOR i := FIRST (keys^) TO LAST (keys^) DO
          key := keys[i];
          IF (key # ID.NoID) AND tbl.get (key, ref) THEN
            t1 := ID.ToText (key);
            t2 := ref;
            IF t2 = NIL THEN t2 := ""; END;
            DumpText (wr, t1);
            IF Text.Length (t1) + Text.Length (t2) > 70 THEN
              Wr.PutText (wr, Wr.EOL);
              Wr.PutText (wr, "  ");
            END;
            Wr.PutText (wr, "  ");
            DumpText (wr, t2);
            Wr.PutText (wr, Wr.EOL);
            Wr.PutText (wr, Wr.EOL);
          END;
        END;
        Wr.PutText (wr, Fmt.Int (version));
        Wr.PutText (wr, " (* sequence number *)");
        Wr.PutText (wr, Wr.EOL);
      FINALLY
        Wr.Close (wr);
      END;
      INC (version);
    EXCEPT
    | Thread.Alerted =>
        ErrLog.Msg ("interrupted while updating user state file: ", fn);
        RETURN FALSE;
    | OSError.E, Wr.Failure (ec) =>
        ErrLog.Msg ("trouble updating user state file: ", fn, OS.Err (ec));
        RETURN FALSE;
    END;
    RETURN TRUE;
  END DumpTable;

PROCEDURE DumpText (wr: Wr.T;  txt: TEXT)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR x0, x1, x2: INTEGER;  c: CHAR;
  BEGIN
    Wr.PutChar (wr, '\"');
    FOR i := 0 TO Text.Length (txt) - 1 DO
      c := Text.GetChar (txt, i);
      IF    (c = '\n')   THEN    Wr.PutChar (wr, Escape); Wr.PutChar (wr, 'n');
      ELSIF (c = '\r')   THEN    Wr.PutChar (wr, Escape); Wr.PutChar (wr, 'r');
      ELSIF (c = '\t')   THEN    Wr.PutChar (wr, Escape); Wr.PutChar (wr, 't');
      ELSIF (c = '\f')   THEN    Wr.PutChar (wr, Escape); Wr.PutChar (wr, 'f');
      ELSIF (c = '\'')   THEN    Wr.PutChar (wr, Escape); Wr.PutChar (wr, '\'');
      ELSIF (c = '\"')   THEN    Wr.PutChar (wr, Escape); Wr.PutChar (wr, '\"');
      ELSIF (c = Escape) THEN    Wr.PutChar (wr, Escape); Wr.PutChar (wr, Escape);
      ELSIF (' ' <= c) AND (c <= '}') THEN    Wr.PutChar (wr, c);
      ELSE
        x2 :=                  Word.And (ORD (c), 8_007)     + ORD ('0');
        x1 := Word.RightShift (Word.And (ORD (c), 8_070), 3) + ORD ('0');
        x0 := Word.RightShift (Word.And (ORD (c), 8_700), 6) + ORD ('0');
        Wr.PutChar (wr, Escape);
        Wr.PutChar (wr, VAL (x0, CHAR));
        Wr.PutChar (wr, VAL (x1, CHAR));
        Wr.PutChar (wr, VAL (x2, CHAR));
      END;
    END;
    Wr.PutChar (wr, '\"');
  END DumpText;

PROCEDURE RemoveTable (fn: TEXT) =
  BEGIN
    TRY
      FS.DeleteFile (fn);
    EXCEPT OSError.E =>
      (* darn *)
    END;
  END RemoveTable;

(*----------------------------------------------------------- initialization ---*)

PROCEDURE Init (dir: TEXT) =
  VAR
    buf0, buf1: Buf.T;
    tbl0, tbl1: IntRefTbl.T;
    v0,   v1  : INTEGER;
  BEGIN
    mu := NEW (MUTEX);
    changed := NEW (Thread.Condition);

    save_dir := dir;
    IF (dir = NIL) THEN
      tbl := DefaultTable ();
      RETURN;
    END;

    fname[0] := Pathname.Join (dir, fname[0], NIL);
    fname[1] := Pathname.Join (dir, fname[1], NIL);

    buf0 := Buf.FromFile (fname[0], pad := 1);
    buf1 := Buf.FromFile (fname[1], pad := 1);

    IF (buf0 = NIL) AND (buf1 = NIL) THEN
      tbl := DefaultTable ();
    ELSIF (buf0 = NIL) THEN
      ErrLog.Msg ("Recovering user state from ", fname[1]);
      tbl := ReadBuf (buf1, version);
    ELSIF (buf1 = NIL) THEN
      ErrLog.Msg ("Recovering user state from ", fname[0]);
      tbl := ReadBuf (buf0, version);
    ELSE
      tbl0 := ReadBuf (buf0, v0);
      tbl1 := ReadBuf (buf1, v1);
      IF (v0 > v1) THEN
        ErrLog.Msg ("Recovering user state from ", fname[0]);
        version := v0;
        tbl := tbl0;
      ELSIF (v1 >= 0) THEN
        ErrLog.Msg ("Recovering user state from ", fname[1]);
        version := v1;
        tbl := tbl1;
      ELSE
        tbl := DefaultTable ();
      END;
    END;

    (* start the lazy update thread & cleanup routine *)
    EVAL Thread.Fork (NEW (UpdateClosure));
    Process.RegisterExitor (DumpState);
  END Init;

(*--------------------------------------------------------------- reading ---*)

PROCEDURE ReadBuf (buf: Buf.T;  VAR(*OUT*) vers: INTEGER): IntRefTbl.T =
  VAR
    tbl := NewTable ();
    cnt := 0;
    key, txt : TEXT;
    cursor: INTEGER;
    lex := NEW (M3Scanner.Default).initFromBuf (buf, skip_comments := TRUE,
                                                split_pragmas := FALSE);
  BEGIN
    vers := FIRST (INTEGER);
    LOOP
      CASE lex.token OF
      | M3Scanner.TK_EOF, M3Scanner.TK_Error =>
          EXIT;
      | M3Scanner.TK_Text_const =>
          txt := LexMisc.ReadString (SUBARRAY (buf^, lex.offset+1, lex.length-2));
          IF (cnt = 0) THEN
            key := txt;
            INC (cnt);
          ELSIF (cnt = 1) THEN
            EVAL tbl.put (ID.Add (key), txt);
            cnt := 0;
          END;
      | M3Scanner.TK_Card_const =>
          cursor := 0;
          vers := LexMisc.ReadInt (SUBARRAY (buf^, lex.offset, lex.length), cursor);
      ELSE (* ignore the bogus tokens *)
      END;
      lex.next ();
    END;
    RETURN tbl;
  END ReadBuf;

PROCEDURE DefaultTable (): IntRefTbl.T =
  BEGIN
    ErrLog.Msg ("Using default configuration.");
    RETURN NewTable ();
  END DefaultTable;

PROCEDURE NewTable (): IntRefTbl.T =
  BEGIN
    RETURN NEW (IntRefTbl.Default).init ();
  END NewTable;

(*--------------------------------------------------------------- writing ---*)

BEGIN
END UserState.



