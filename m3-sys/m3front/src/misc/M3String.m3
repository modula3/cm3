(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: M3String.m3                                           *)
(* Last modified on Fri Sep 16 14:14:22 PDT 1994 by kalsow     *)
(*      modified on Wed Nov 28 02:23:29 1990 by muller         *)

MODULE M3String;

IMPORT M3Buf, Text, Word, CG, Target;

TYPE
  Buf       = ARRAY OF CHAR;
  HashTable = REF ARRAY OF T;

REVEAL
  T = BRANDED REF RECORD
    prefix : T         := NIL;
    suffix : T         := NIL;
    body   : TEXT      := NIL;
    length : INTEGER   := 0;
    hash   : INTEGER   := 0;
    uid    : INTEGER   := 0;
    next   : T         := NIL;
  END;
  (* There are two variants of a String.T:
       (body # NIL)  => the characters are in body
       ELSE          => prefix & suffix
  *)

CONST
  Digits = ARRAY [0..9] OF CHAR {'0','1','2','3','4','5','6','7','8','9'};

VAR
  hashMask  : INTEGER   := 511; (* == 2^9-1 == 9 bits on *)
  hashTable : HashTable := NIL;
  next_t    : T         := NIL;
  nStrings  : INTEGER   := 0;

(*-------------------------------------------------------------- exported ---*)

PROCEDURE Add (x: TEXT): T =
  VAR buf: ARRAY [0..255] OF CHAR;  ref: REF ARRAY OF CHAR;
  BEGIN
    IF (next_t = NIL) THEN next_t := NEW (T) END;
    next_t.prefix := NIL;
    next_t.suffix := NIL;
    next_t.body   := x;
    next_t.length := Text.Length (x);
    next_t.uid    := NO_UID;
    IF (next_t.length <= NUMBER (buf)) THEN
      Text.SetChars (buf, x);
      RETURN Intern (SUBARRAY (buf, 0, next_t.length));
    ELSE
      ref := NEW (REF ARRAY OF CHAR, next_t.length);
      Text.SetChars (ref^, x);
      RETURN Intern (ref^);
    END;
  END Add;

PROCEDURE FromStr (READONLY buf: Buf;  length: INTEGER): T =
  VAR t: T;
  BEGIN
    IF (next_t = NIL) THEN next_t := NEW (T) END;
    next_t.prefix := NIL;
    next_t.suffix := NIL;
    next_t.body   := NIL; (* for now *)
    next_t.length := MIN (length, NUMBER (buf));
    next_t.uid    := NO_UID;
    t := Intern (SUBARRAY (buf, 0, next_t.length));
    RETURN t;
  END FromStr;

PROCEDURE Concat (a, b: T): T =
  VAR buf: ARRAY [0..3] OF CHAR;
  BEGIN
    IF (a = NIL) OR (a.length = 0) THEN RETURN b END;
    IF (b = NIL) OR (b.length = 0) THEN RETURN a END;
    IF (next_t = NIL) THEN next_t := NEW (T) END;
    next_t.prefix := a;
    next_t.suffix := b;
    next_t.body   := NIL;
    next_t.length := a.length + b.length;
    next_t.uid    := NO_UID;
    RETURN Intern (buf);
  END Concat;

PROCEDURE ToText (t: T): TEXT =
  VAR
    buf : ARRAY [0..255] OF CHAR;
    ref : REF ARRAY OF CHAR;
  BEGIN
    IF (t = NIL) THEN
      RETURN NIL;
    ELSIF (t.body # NIL) THEN
      (* already done. *)
    ELSIF (t.length <= NUMBER (buf)) THEN
      Flatten (t, buf, 0);
      t.body := Text.FromChars (SUBARRAY (buf, 0, t.length));
    ELSE
      ref := NEW (REF ARRAY OF CHAR, t.length);
      Flatten (t, ref^, 0);
      t.body := Text.FromChars (ref^);
    END;
    RETURN t.body;
  END ToText;

PROCEDURE Put (wr: M3Buf.T;  t: T) =
  BEGIN
    IF (t = NIL) THEN
      (* done *)
    ELSIF (t.body # NIL) THEN
      FOR i := 0 TO t.length-1 DO EmitChar (wr, Text.GetChar (t.body, i)) END;
    ELSE
      Put (wr, t.prefix);
      Put (wr, t.suffix);
    END;
  END Put;

PROCEDURE Init_chars (offset: INTEGER;  t: T;  is_const: BOOLEAN) =
  BEGIN
    IF (t = NIL) THEN
      (* done *)
    ELSIF (t.body # NIL) THEN
      CG.Init_chars (offset, t.body, is_const);
    ELSE
      Init_chars (offset, t.prefix, is_const);
      Init_chars (offset + t.prefix.length * Target.Char.size, t.suffix, is_const);
    END;
  END Init_chars;

PROCEDURE Length (t: T): INTEGER =
  BEGIN
    IF (t = NIL)
      THEN RETURN 0;
      ELSE RETURN t.length;
    END;
  END Length;

PROCEDURE GetUID (t: T): INTEGER =
  BEGIN
    RETURN t.uid;
  END GetUID;

PROCEDURE SetUID (t: T;  uid: INTEGER) =
  BEGIN
    t.uid := uid;
  END SetUID;

PROCEDURE Hash (t: T): INTEGER =
  BEGIN
    IF (t = NIL)
      THEN RETURN 953;
      ELSE RETURN t.hash;
    END;
  END Hash;

(*-------------------------------------------------------------- internal ---*)

PROCEDURE Intern (READONLY buf: Buf): T =
  VAR hash, bucket: INTEGER;  t: T;
  BEGIN
    (* search the hash table *)
    next_t.hash := 0;
    hash := InternHash (next_t, 0, buf);
    bucket := Word.And (hash, hashMask);
    t := hashTable[bucket];
    WHILE (t # NIL) DO
      IF (t.hash = hash) AND Equal (t, next_t, buf) THEN RETURN t; END;
      t := t.next;
    END;

    (* we didn't find the string => add it to the hash table *)
    t := next_t;
    t.hash := hash;
    t.next := hashTable [bucket];
    hashTable [bucket] := t;
    next_t := NIL; (* since we've used it! *)

    IF (t.prefix = NIL) AND (t.body = NIL) THEN
      t.body := Text.FromChars (buf);
    END;

    INC (nStrings);
    IF (nStrings > 2 * NUMBER (hashTable^)) THEN ExpandHashTable () END;
    RETURN t;
  END Intern;

PROCEDURE ExpandHashTable () =
  VAR
    n_old   := NUMBER (hashTable^);
    n_new   := n_old + n_old;
    new     := NEW (HashTable, n_new);
    newMask := hashMask + hashMask + 1;
    t, u    : T;
    x       : INTEGER;
  BEGIN
    FOR i := 0 TO n_new - 1 DO new[i] := NIL END;

    FOR i := 0 TO n_old - 1 DO
      t := hashTable [i];
      WHILE (t # NIL) DO
        u := t.next;
        x := Word.And (t.hash, newMask);
        t.next := new [x];
        new [x] := t;
        t := u;
      END;
    END;

    hashMask := newMask;
    hashTable := new;
  END ExpandHashTable;

PROCEDURE InternHash (t: T;  hash: INTEGER;  READONLY buf: Buf): INTEGER =
  BEGIN
    IF (t = NIL) THEN RETURN 0 END;
    IF (hash = 0) AND (t.hash # 0) THEN RETURN t.hash END;

    IF (t.body # NIL) THEN
      FOR i := 0 TO t.length - 1 DO
        hash := Word.Plus (Word.Times (2, hash), ORD (Text.GetChar (t.body, i)));
      END;
    ELSIF (t.prefix # NIL) THEN
      (* a concatentation *)
      hash := InternHash (t.prefix, hash, buf);
      hash := InternHash (t.suffix, hash, buf);
    ELSE (* use the buffer *)
      FOR i := 0 TO t.length - 1 DO
        hash := Word.Plus (Word.Times (2, hash), ORD (buf[i]));
      END;
    END;

    RETURN hash;
  END InternHash;

PROCEDURE Equal (a, b: T;  READONLY buf: Buf): BOOLEAN =
  BEGIN
    IF (a.length # b.length) THEN RETURN FALSE END;
    FOR i := 0 TO a.length - 1 DO
      IF GetCh (a, buf, i) # GetCh (b, buf, i) THEN RETURN FALSE; END;
    END;
    RETURN TRUE;
  END Equal;

PROCEDURE GetCh (t: T;  READONLY buf: Buf;  i: INTEGER): CHAR =
  VAR u: T;
  BEGIN
    (* walk the tree to find the right segment *)
    WHILE (t.prefix # NIL) DO
      u := t.prefix;
      IF (u.length > i)
        THEN t := t.prefix;
        ELSE t := t.suffix;  DEC (i, u.length);
      END;
    END;

    IF (t.body # NIL)
      THEN RETURN Text.GetChar (t.body, i);
      ELSE RETURN buf[i];
    END;
  END GetCh;

PROCEDURE Flatten (t: T;  VAR buf: Buf;  start: INTEGER) =
  BEGIN
    IF (t = NIL) THEN
      (* done *)
    ELSIF (t.body # NIL) THEN
      Text.SetChars (SUBARRAY (buf, start, t.length), t.body);
    ELSE
      WHILE (t # NIL) AND (t.body = NIL) DO
        Flatten (t.suffix, buf, start + Length (t.prefix));
        t := t.prefix;
      END;
      Flatten (t, buf, start);
    END;
  END Flatten;

PROCEDURE EmitChar (wr: M3Buf.T;  c: CHAR) =
  VAR i: INTEGER;
  BEGIN
    IF (c < ' ') OR (c = '\"') OR (c = '\'') OR ('~' < c) OR (c = '\\') THEN
      i := Word.And (ORD (c), 255);
      M3Buf.PutChar (wr, '\\');
      M3Buf.PutChar (wr, Digits[i DIV 64]);  i := Word.And (i, 63);
      M3Buf.PutChar (wr, Digits[i DIV 8]);   i := Word.And (i, 7);
      M3Buf.PutChar (wr, Digits[i]);
    ELSE (* simple graphic character *)
      M3Buf.PutChar (wr, c);
    END;
  END EmitChar;

(*-------------------------------------------------------- initialization ---*)

PROCEDURE Initialize () =
  BEGIN
    <*ASSERT hashTable = NIL*>
    hashTable := NEW (HashTable, hashMask+1);
    FOR i := 0 TO LAST (hashTable^) DO hashTable[i] := NIL; END;
  END Initialize;

PROCEDURE Reset () =
  VAR t: T;
  BEGIN
    FOR i := FIRST (hashTable^) TO LAST (hashTable^) DO
      t := hashTable[i];
      WHILE (t # NIL) DO t.uid := NO_UID;  t := t.next END;
    END;
  END Reset;

BEGIN
END M3String.
