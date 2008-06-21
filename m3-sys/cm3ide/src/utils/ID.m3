(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Thu Dec  8 15:26:38 PST 1994 by kalsow     *)
(*      modified on Wed Nov 28 02:23:29 1990 by muller         *)

UNSAFE MODULE ID;

IMPORT Wx, Wr, Thread, Text, Word, Cstring, Ctypes;

CONST
  MaxLength   = 8192 - BYTESIZE(ADDRESS) (* allocator goo *);
  NullChar    = '\000';

TYPE
  StrPtr     = UNTRACED REF CHAR;
  CharBuffer = UNTRACED REF ARRAY [0 .. MaxLength - 1] OF CHAR;
  DescBuffer = REF ARRAY OF Desc;

TYPE
  Desc = RECORD
    start  : StrPtr  := NIL;
    hash   : INTEGER := 0;
    text   : TEXT    := NIL;
  END;

VAR
  mu        := NEW (MUTEX);

  next_char : CARDINAL := 0;
  chars     := NEW (CharBuffer);

  next_t    : T := 1;
  ids       := NEW (DescBuffer, 2000);

  hashMask  : INTEGER := 2047; (* == 2^11-1 == 11 bits on *)
  hashTable := NEW (REF ARRAY OF T, 2048);

(*-------------------------------------------------------------- exported ---*)

PROCEDURE Add (x: TEXT): T =
  VAR
    t   : T;
    len := Text.Length (x);
    buf : ARRAY [0..255] OF CHAR;
  BEGIN
    <*ASSERT len <= NUMBER (buf) *>
    Text.SetChars (buf, x);
    t := FromStr (buf, len);
    IF (ids[t].text = NIL) THEN ids[t].text := x; END;
    RETURN t;
  END Add;

PROCEDURE FromStr (READONLY buf: ARRAY OF CHAR;  length: INTEGER): T =
  VAR hash, n: INTEGER;  bucket: CARDINAL;  t: T;  p0, p1: StrPtr;
  BEGIN
    length := MIN (length, NUMBER (buf));
    hash := 0;
    FOR i := 0 TO length - 1 DO
      hash := Word.Plus (Word.Times (17, hash), ORD (buf[i]));
    END;

    LOCK mu DO

      bucket := Word.And (hash, hashMask);
      LOOP
        t := hashTable[bucket];
        IF (t = NoID) THEN (* empty! *) EXIT; END;
        IF (ids[t].hash = hash) THEN
          IF (length > 0) THEN p0 := ADR (buf[0]); END;
          p1 := ids[t].start;
          n  := length;
          WHILE (n > 0) AND (p0^ = p1^) DO
            DEC (n);
            INC (p0, ADRSIZE (p0^));
            INC (p1, ADRSIZE (p1^));
          END;
          IF (n = 0) AND (p1^ = NullChar) THEN RETURN t; END;
        END;
        INC (bucket);
        IF (bucket >= NUMBER (hashTable^)) THEN bucket := 0; END;
      END;

      (* we didn't find a match => build a new one *)
      t := next_t;  INC (next_t);
      IF (t >= NUMBER (ids^)) THEN ExpandIDs (); END;
      hashTable[bucket] := t;

      (* make sure we've got room to stuff the characters *)
      IF (next_char + length >= LAST (chars^)) THEN ExpandChars (); END;

      (* initialize the descriptor and stuff the characters *)
      WITH z = ids[t] DO
        z.start := ADR (chars[next_char]);
        z.hash  := hash;
        z.text  := NIL;
        SUBARRAY (chars^, next_char, length) := SUBARRAY (buf, 0, length);
        chars [next_char + length] := NullChar;
        INC (next_char, length + 1);
      END;

      (* make sure we're not overloading the hash table *)
      IF (2 * next_t > NUMBER (hashTable^)) THEN ExpandHashTable (); END;

    END; (*LOCK*)

    RETURN t;
  END FromStr;

PROCEDURE ToText (t: T): TEXT =
  VAR ptr: StrPtr;  len: INTEGER;  x: TEXT;
  BEGIN
    <*ASSERT t < next_t*>
    IF (t = NoID) THEN RETURN NIL END;
    LOCK mu DO
      x := ids[t].text;
      IF (x = NIL) THEN
        ptr := ids[t].start;
        len := Cstring.strlen (LOOPHOLE (ptr, Ctypes.char_star));
        x   := Text.FromChars (SUBARRAY (LOOPHOLE (ptr, CharBuffer)^, 0, len));
        ids[t].text := x;
      END;
    END;
    RETURN x;
  END ToText;

PROCEDURE Put (wr: Wx.T;  t: T)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR ptr := LOOPHOLE (ids[t].start, CharBuffer);  len: INTEGER;
  BEGIN
    <*ASSERT t < next_t*>
    IF (t = NoID) THEN RETURN END;
    len := Cstring.strlen (LOOPHOLE (ptr, Ctypes.char_star));
    wr.putStr (SUBARRAY (ptr^, 0, len));
  END Put;

PROCEDURE Hash (t: T): INTEGER =
  BEGIN
    <*ASSERT t < next_t*>
    RETURN ids[t].hash;
  END Hash;

PROCEDURE IsLT (a, b: T): BOOLEAN =
  VAR pa, pb: ADDRESS;
  BEGIN
    <*ASSERT a < next_t AND b < next_t *>
    pa := ids[a].start;
    pb := ids[b].start;
    RETURN Cstring.strcmp (pa, pb) < 0;
  END IsLT;

PROCEDURE Compare (a, b: T): [-1 .. +1] =
  BEGIN
    IF    (a = b)     THEN RETURN  0;
    ELSIF IsLT (a, b) THEN RETURN -1;
    ELSE                   RETURN +1;
    END;
  END Compare;

(*-------------------------------------------------------------- internal ---*)

PROCEDURE ExpandChars () =
  BEGIN
    chars := NEW (CharBuffer);
    next_char := 0;
  END ExpandChars;

PROCEDURE ExpandIDs () =
  VAR n := NUMBER (ids^);  new := NEW (DescBuffer, n+n);
  BEGIN
    SUBARRAY (new^, 0, n) := ids^;
    ids := new;
  END ExpandIDs;

PROCEDURE ExpandHashTable () =
  VAR
    n_old   := NUMBER (hashTable^);
    n_new   := n_old + n_old;
    new     := NEW (REF ARRAY OF T, n_new);
    newMask := hashMask + hashMask + 1;
    t       : T;
    bucket  : INTEGER;
  BEGIN
    FOR i := 0 TO n_new - 1 DO new[i] := NoID END;

    FOR i := 0 TO n_old - 1 DO
      t := hashTable [i];
      IF (t # NoID) THEN
        bucket := Word.And (ids[t].hash, newMask);
        WHILE (new[bucket] # NoID) DO
          INC (bucket);
          IF (bucket >= n_new) THEN bucket := 0; END;
        END;
        new[bucket] := t;
      END;
    END;

    hashMask := newMask;
    hashTable := new;
  END ExpandHashTable;

BEGIN
END ID.
