(* Copyright (C) 1994, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(*                                                            *)
(* Last modified on Fri Nov  4 11:11:52 PST 1994 by kalsow    *)

MODULE SilRd;

IMPORT Text, FS, File, OSError;
IMPORT SilError;

CONST
  EOF = '\000';

REVEAL
  T = Tx BRANDED "SilRd" OBJECT
    name   : TEXT    := NIL;
    file   : File.T  := NIL;
    cursor : INTEGER := 0;
    len    : INTEGER := 0;
    ch     : CHAR;
    buf    : ARRAY [0..1023] OF File.Byte;
  END;

PROCEDURE Open (filename: TEXT): T =
  VAR t := NEW (T);
  BEGIN
    TRY
      t.ch   := ' ';
      t.name := filename;
      t.file := FS.OpenFileReadonly (filename);
    EXCEPT OSError.E(info) =>
      SilError.Put (info, "unable to open \"", filename, "\" for reading");
      RETURN NIL;
    END;
    RETURN t;
  END Open;

PROCEDURE Close (t: T) =
  BEGIN
    IF (t # NIL) AND (t.file # NIL) THEN
      TRY
        t.file.close ();
      EXCEPT OSError.E(info) => (* ignore *)
        SilError.Put (info, "problem closing \"", t.name, "\"");
      END;
      t.file := NIL;
      t.name := NIL;
    END;
  END Close;

PROCEDURE ParseNextLine (t: T): BOOLEAN =
  BEGIN
    t.n_strs := 0;
    t.n_ints := 0;

    (* skip white space *)
    WHILE (t.ch = ' ') OR (t.ch = '\n') OR (t.ch = '\t') OR (t.ch = '\r') DO
      GetCh (t);
    END;

    IF (t.ch = EOF) THEN RETURN FALSE; END;

    (* grab the command *)    
    t.cmd := t.ch;  GetCh (t);

    LOOP
      CASE t.ch OF
      | '-', '0' .. '9' => (* number *)
          IF (t.n_ints > LAST (t.ints)) THEN
            SilError.Put (NIL, "too many integers on a line");
            RETURN FALSE;
          END;
          t.ints [t.n_ints] := ReadInt (t);
          INC (t.n_ints);

      | '"' => (* string *)
          IF (t.n_strs > LAST (t.strs)) THEN
            SilError.Put (NIL, "too many strings on a line");
            RETURN FALSE;
          END;
          t.strs [t.n_strs] := ReadString (t);
          INC (t.n_strs);

      | '\n', EOF => (* end of line *)
          EXIT;

      | ' ', '\t', '\r' => (* white space *)
          (* ignore *)
          GetCh (t);

      ELSE
          SilError.Put (NIL, "unexpected input character '",
                               Text.FromChar (t.ch), "'");
      END;
    END;
    
    RETURN TRUE;
  END ParseNextLine;
  
PROCEDURE ReadString (t: T): TEXT =
  VAR txt := "";  len := 0;  buf: ARRAY [0..99] OF CHAR;
  BEGIN
    GetCh (t); (* skip the opening quote *)
    WHILE (t.ch # '"') AND (t.ch # EOF) DO
      IF (len >= NUMBER (buf)) THEN
        txt := txt & Text.FromChars (buf);
        len := 0;
      END;
      buf [len] := t.ch;
      INC (len);
      GetCh (t);
    END;
    GetCh (t); (* skip the closing quote or EOF *)
    RETURN txt & Text.FromChars (SUBARRAY (buf, 0, len));
  END ReadString;
  
PROCEDURE ReadInt (t: T): INTEGER =
  VAR neg := FALSE;  val := 0;
  BEGIN
    IF t.ch = '-' THEN  neg := TRUE;  GetCh (t);  END;
    WHILE ('0' <= t.ch) AND (t.ch <= '9') DO
      val := (10*val) + ORD(t.ch) - ORD('0');
      GetCh (t);
    END;
    IF neg THEN val := -val; END;
    RETURN val;
  END ReadInt;

PROCEDURE GetCh (t: T) =
  VAR x := t.cursor;
  BEGIN
    IF (x >= t.len) THEN Refill (t);  x := 0;  END;
    t.ch := VAL (t.buf [x], CHAR);
    INC (t.cursor);
  END GetCh;

PROCEDURE Refill (t: T) =
  BEGIN
    TRY
      t.len := t.file.read (t.buf);
    EXCEPT OSError.E (info) =>
      SilError.Put (info, "problem reading \"", t.name, "\"");
      t.len := 0;
    END;
    IF (t.len <= 0) THEN
      t.len := 1;
      t.buf[0] := ORD (EOF);
    END;
  END Refill;

BEGIN
END SilRd.
