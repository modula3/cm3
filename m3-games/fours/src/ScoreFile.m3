(* Copyright (C) 1995, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Fri Jun 16 08:20:51 PDT 1995 by kalsow     *)

MODULE ScoreFile;

IMPORT Text, Wr, FileWr, FS, OSError, File, Thread;
IMPORT Buf, Date, RegularFile, Time, Fmt, AtomList, Atom;


PROCEDURE Get (f: FileName; p: Player; VAR(*OUT*) s: Score): BOOLEAN
  RAISES {Error} =
  VAR
    player := FixName (p);
    buf    := Inhale (f);
    cursor := FindEntry (buf, player);
  BEGIN
    IF (cursor < 0) THEN RETURN FALSE; END;
    ScanScore (buf, cursor + Text.Length (player), s);
    RETURN TRUE;
  END Get;

PROCEDURE Enumerate (f: FileName;  enum: EnumProc) RAISES {Error} =
  VAR
    buf     := Inhale (f);
    buf_len := NUMBER (buf^);
    cursor  := 0;
    score   : Score;
    player  : Player;
  BEGIN
    WHILE (cursor < buf_len) DO
      player := ScanName (buf, cursor);
      IF (player # NIL) THEN
        ScanScore (buf, cursor, score);
        enum (player, score);
      END;
      cursor := NextLine (buf, cursor);
    END;
  END Enumerate;

CONST
  NoScore = Score { 0.0d0, 0, 0, 0, 0, 0.0d0 };

PROCEDURE Put (f: FileName;  p: Player;  READONLY s: Score) RAISES {Error} =
  VAR
    player := FixName (p);
    fd     : RegularFile.T;
    wr     : Wr.T;
    buf    : Buf.T;
    x0, x1 : INTEGER;
    ss     : Score;
  BEGIN
    TRY
      TRY
        (* open and read the file *)
        fd := FS.OpenFile (f, truncate := FALSE, create := FS.CreateOption.Ok);
        LockFile (fd);
        buf := Inhale (f, fd);

        (* find the matching entry *)
        x0 := FindEntry (buf, player);
        IF (x0 >= 0) THEN
          ScanScore (buf, x0 + Text.Length (player), ss);
          x1 := NextLine (buf, x0);
        ELSE
          ss := NoScore;
          x0 := NUMBER (buf^);
          x1 := x0;
        END;

        (* merge the old and new scores *)
        INC (ss.n_games, s.n_games);
        ss.n_seconds := ss.n_seconds + s.n_seconds;
        IF (s.best_score > ss.best_score) THEN
          ss.best_score := s.best_score;
          ss.best_rows  := s.best_rows;
          ss.best_level := s.best_level;
          ss.best_date  := s.best_date;
        END;

        wr := NEW (FileWr.T).init (fd);
	Wr.Seek (wr, 0);

        (* write the new score *)
        Wr.PutText (wr, player);
        Wr.PutChar (wr, ' ');
        Wr.PutText (wr, Fmt.Int (ROUND (ss.best_date - Epoch ())));
        Wr.PutChar (wr, ' ');
        Wr.PutText (wr, Fmt.Int (ss.best_level));
        Wr.PutChar (wr, ' ');
        Wr.PutText (wr, Fmt.Int (ss.best_rows));
        Wr.PutChar (wr, ' ');
        Wr.PutText (wr, Fmt.Int (ss.best_score));
        Wr.PutChar (wr, ' ');
        Wr.PutText (wr, Fmt.Int (ss.n_games));
        Wr.PutChar (wr, ' ');
        Wr.PutText (wr, Fmt.Int (ROUND (ss.n_seconds * 10.0d0)));
        Wr.PutText (wr, Wr.EOL);

        (* append the existing scores *)
        IF (x0 > 0) THEN
          Wr.PutString (wr, SUBARRAY (buf^, 0, x0));
        END;
        IF (x1 < NUMBER (buf^)) THEN
          Wr.PutString (wr, SUBARRAY (buf^, x1, NUMBER (buf^) - x1));
        END;

        Wr.Flush (wr);
        fd.unlock ();
      EXCEPT
      | OSError.E(ec)  =>  FileProblem (ec);
      | Thread.Alerted =>  RAISE Error ("interrupted");
      | Wr.Failure(ec) =>  FileProblem (ec);
      END;
    FINALLY
      Close (wr);
      Close (fd);
    END;
  END Put;

(*-------------------------------------------------------------- internal ---*)

PROCEDURE Inhale (path: TEXT;  src: File.T := NIL): Buf.T RAISES {Error} =
  VAR buf: Buf.T;
  BEGIN
    TRY
      buf := Buf.FromFile (path, src);
    EXCEPT
      OSError.E (ec) => FileProblem (ec);
    END;
    RETURN buf;
  END Inhale;

PROCEDURE FindEntry (buf: Buf.T;  p: Player): INTEGER =
  VAR
    buf_len := NUMBER (buf^);
    nm_len  := Text.Length (p);
    nm      := NEW (Buf.T, nm_len);
    cursor  := 0;
  BEGIN
    Text.SetChars (nm^, p);
    WHILE (cursor <= buf_len - nm_len) DO
      IF SUBARRAY (buf^, cursor, nm_len) = nm^ THEN
        RETURN cursor;
      END;
      cursor := NextLine (buf, cursor);
    END;
    RETURN -1;
  END FindEntry;

PROCEDURE NextLine (buf: Buf.T;  cursor: INTEGER): INTEGER =
  VAR len := NUMBER (buf^);
  BEGIN
    WHILE (cursor < len) AND (buf[cursor] # '\n') DO INC (cursor); END;
    RETURN cursor + 1;
  END NextLine;

PROCEDURE ScanScore (buf: Buf.T;  cursor: INTEGER;  VAR s: Score)
  RAISES {Error} =
  BEGIN
    s.best_date  := FLOAT (GetInt (buf, cursor), Time.T) + Epoch ();
    s.best_level := GetInt (buf, cursor);
    s.best_rows  := GetInt (buf, cursor);
    s.best_score := GetInt (buf, cursor);
    s.n_games    := GetInt (buf, cursor);
    s.n_seconds  := FLOAT (GetInt (buf, cursor), Time.T) / 10.0d0;
  END ScanScore;

PROCEDURE FixName (p: Player): Player =
  VAR
    plen := Text.Length (p);
    buf  := NEW (REF ARRAY OF CHAR, plen + 6);
    blen := 0;
    num  := Fmt.Int (plen);
  BEGIN
    (* prepend the length *)
    Text.SetChars (buf^, num);
    blen := Text.Length (num);

    buf[blen] := ' ';  INC (blen);

    (* add the name *)
    Text.SetChars (SUBARRAY (buf^, blen, NUMBER (buf^) - blen), p);
    INC (blen, plen);

    (* replace any nasty characters *)    
    FOR i := 0 TO blen-1 DO
      IF (buf[i] = '\n') THEN buf[i] := ' '; END;
    END;

    RETURN Text.FromChars (SUBARRAY (buf^, 0, blen));
  END FixName;

PROCEDURE ScanName (buf: Buf.T;  VAR cursor: INTEGER): Player =
  VAR val := 0;  z := cursor;  len := NUMBER (buf^);  player: Player;
  BEGIN
    WHILE (cursor < len) AND (buf[cursor] = ' ') DO INC (cursor); END;
    z := cursor;
    WHILE (z < len) AND ('0' <= buf[z]) AND (buf[z] <= '9') DO
      val := 10 * val + ORD (buf[z]) - ORD ('0');
      INC (z);
    END;
    IF (z = cursor) THEN RETURN NIL END;
    cursor := z + 1;  (* skip the blank *)
    val := MIN (val, NUMBER (buf^) - cursor);
    player := Text.FromChars (SUBARRAY (buf^, cursor, val));
    INC (cursor, val);
    RETURN player;
  END ScanName;

PROCEDURE GetInt (buf: Buf.T;  VAR cursor: INTEGER): INTEGER RAISES {Error} =
  VAR val := 0;  z := cursor;  len := NUMBER (buf^);
  BEGIN
    WHILE (cursor < len) AND (buf[cursor] = ' ') DO INC (cursor); END;
    z := cursor;
    WHILE (z < len) AND ('0' <= buf[z]) AND (buf[z] <= '9') DO
      val := 10 * val + ORD (buf[z]) - ORD ('0');
      INC (z);
    END;
    IF (z = cursor) THEN
      RAISE Error ("bad file format: missing integer value");
    END;
    cursor := z;
    RETURN val;
  END GetInt;

PROCEDURE LockFile (fd: RegularFile.T) RAISES {OSError.E, Error} =
  VAR delay := 1.0d0; (* seconds *)
  BEGIN
    FOR i := 0 TO 3 DO
      IF fd.lock () THEN RETURN END;
      Thread.Pause (delay);
      delay := delay + delay;
    END;
    IF fd.lock () THEN RETURN END;
    RAISE Error ("unable to lock the score file");
  END LockFile;

PROCEDURE Close (r: REFANY) =
  BEGIN
    TYPECASE r OF
    | NULL =>        (* ignore *)
    | Wr.T(wr) =>    TRY Wr.Close (wr) EXCEPT Wr.Failure, Thread.Alerted=> END;
    | File.T(fd) =>  TRY fd.close()    EXCEPT OSError.E => END;
    ELSE             (* ignore *)
    END;
  END Close;

VAR
  epoch: Time.T := 0.0d0;

PROCEDURE Epoch (): Time.T =
  <*FATAL Date.Error*>
  CONST
    T0 = Date.T{1995, Date.Month.Jan, 1, 0,0,0,0, "", Date.WeekDay.Sun};
  BEGIN
    IF (epoch = 0.0d0) THEN epoch := Date.ToTime (T0); END;
    RETURN epoch;
  END Epoch;

PROCEDURE FileProblem (ec: AtomList.T) RAISES {Error} =
  VAR msg := "file system problem";
  BEGIN
    WHILE (ec # NIL) DO
      msg := msg & " ** " & Atom.ToText (ec.head);
      ec := ec.tail;
    END;
    RAISE Error (msg);
  END FileProblem;

BEGIN
END ScoreFile.
