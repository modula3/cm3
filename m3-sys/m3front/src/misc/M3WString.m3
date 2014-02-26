(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved.  *)
(* See file COPYRIGHT-CMASS for details. *)

MODULE M3WString;

IMPORT M3Buf, Text, Word, CG, WCharr;

CONST
  NO_UID = -1;

TYPE
  HashTable = REF ARRAY OF T;
  WText_T   = REF Buf; 

REVEAL
  T = BRANDED REF RECORD
    prefix : T         := NIL;
    suffix : T         := NIL;
    body   : WText_T   := NIL;
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
  Digits = ARRAY [0..15] OF CHAR {'0','1','2','3','4','5','6','7',
                                  '8','9','A','B','C','D','E','F'};

VAR
  hashMask  : INTEGER   := 511; (* == 2^9-1 == 9 bits on *)
  hashTable : HashTable := NIL;
  next_t    : T         := NIL;
  nStrings  : INTEGER   := 0;

(*-------------------------------------------------------------- exported ---*)

PROCEDURE Add (x: TEXT): T =
  VAR
    len  := Text.Length (x);
    body := NEW (WText_T, len + 1);
  BEGIN
    body[len-1] := 0;
    FOR i := 0 TO len-1 DO
      body[i] := ORD (Text.GetWideChar (x, i));
    END;
    IF (next_t = NIL) THEN next_t := NEW (T) END;
    next_t.prefix := NIL;
    next_t.suffix := NIL;
    next_t.body   := body;
    next_t.length := len;
    next_t.uid    := NO_UID;
    RETURN Intern (body^);
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
  VAR buf: ARRAY [0..3] OF Char;
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

PROCEDURE ToLiteral (t: T): TEXT =
  VAR
    len := LiteralLength (t) + 3;
    buf : ARRAY [0..255] OF CHAR;
    ref : REF ARRAY OF CHAR;
  BEGIN
    IF (t = NIL) THEN RETURN NIL END;
    IF (len <= NUMBER (buf)) THEN
      buf[0] := 'W';
      buf[1] := '"';
      Flatten (t, buf, 2);
      buf[len-1] := '"';
      RETURN Text.FromChars (SUBARRAY (buf, 0, len));
    ELSE
      ref := NEW (REF ARRAY OF CHAR, len);
      ref[0] := 'W';
      ref[1] := '"';
      Flatten (t, ref^, 2);
      ref[len-1] := '"';
      RETURN Text.FromChars (ref^);
    END;
  END ToLiteral;

PROCEDURE PutLiteral (wr: M3Buf.T;  t: T) =
  BEGIN
    M3Buf.PutChar (wr, 'W');
    M3Buf.PutChar (wr, '"');
    PutChars (wr, t);
    M3Buf.PutChar (wr, '"');
  END PutLiteral;

PROCEDURE Init_chars (offset: INTEGER;  t: T;  is_const: BOOLEAN) =
  VAR TargetWCBitsize: INTEGER;
  BEGIN 
    IF (t = NIL) THEN
      (* done *)
    ELSE
      IF WCharr.IsUnicode
      THEN TargetWCBitsize := 32
      ELSE TargetWCBitsize := 16
      END; 
      IF (t.body # NIL) THEN
        FOR i := 0 TO t.length-1 DO
          CG.Init_intt (offset, TargetWCBitsize, t.body[i], is_const);
          INC (offset, TargetWCBitsize);
        END;
      ELSE
        Init_chars (offset, t.prefix, is_const);
        Init_chars (offset + t.prefix.length * TargetWCBitsize, 
                    t.suffix, is_const);
      END;
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
      t.body := NEW (WText_T, NUMBER (buf) + 1);
      SUBARRAY (t.body^, 0, NUMBER (buf)) := buf;
      t.body[LAST(t.body^)] := 0;
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
        hash := Word.Plus (Word.Times (2, hash), t.body[i]);
      END;
    ELSIF (t.prefix # NIL) THEN
      (* a concatentation *)
      hash := InternHash (t.prefix, hash, buf);
      hash := InternHash (t.suffix, hash, buf);
    ELSE (* use the buffer *)
      FOR i := 0 TO t.length - 1 DO
        hash := Word.Plus (Word.Times (2, hash), buf[i]);
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

PROCEDURE GetCh (t: T;  READONLY buf: Buf;  i: INTEGER): Char =
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
      THEN RETURN t.body[i];
      ELSE RETURN buf[i];
    END;
  END GetCh;

PROCEDURE LiteralLength (t: T): INTEGER =
  BEGIN
    IF (t = NIL) THEN
      RETURN 0;
    ELSIF (t.body # NIL) THEN
      VAR len := 0;  lit: CharBuf;  BEGIN
        FOR i := 0 TO t.length-1 DO
          INC (len, CharLiteral (t.body[i], (*OUT, Unused*) lit));
        END;
        RETURN len;
      END;
    ELSE
      RETURN LiteralLength (t.prefix) + LiteralLength (t.suffix);
    END;
  END LiteralLength;

PROCEDURE Flatten (t: T;  VAR buf: ARRAY OF CHAR;  start: INTEGER) =
  BEGIN
    IF (t = NIL) THEN
      (* done *)
    ELSIF (t.body # NIL) THEN
      VAR x: INTEGER;  lit: CharBuf;  BEGIN
        FOR i := 0 TO t.length-1 DO
          x := CharLiteral (t.body[i], (*OUT*) lit);
          FOR j := 0 TO x-1 DO  buf[start] := lit[j];  INC (start);  END;
        END;
      END;
    ELSE
      WHILE (t # NIL) AND (t.body = NIL) DO
        Flatten (t.suffix, buf, start + LiteralLength (t.prefix));
        t := t.prefix;
      END;
      Flatten (t, buf, start);
    END;
  END Flatten;

PROCEDURE PutChars (wr: M3Buf.T;  t: T) =
  BEGIN
    WHILE (t # NIL) DO
      IF (t.body # NIL) THEN
        FOR i := 0 TO t.length-1 DO EmitChar (wr, t.body[i]) END;
        t := NIL;
      ELSE
        PutChars (wr, t.prefix);
        t := t.suffix;
      END;
    END;
  END PutChars;

PROCEDURE EmitChar (wr: M3Buf.T;  c: Char) =
  VAR lit: CharBuf;
  BEGIN
    FOR i := 0 TO CharLiteral (c, (*OUT*) lit) - 1 DO
      M3Buf.PutChar (wr, lit[i]);
    END;
  END EmitChar;

TYPE
  CharBuf = ARRAY [0..7] OF CHAR;

PROCEDURE CharLiteral (ch: Char;  VAR(*OUT*) lit: CharBuf): [1..8] =
  BEGIN
    IF ch > 16_FFFF THEN 
      lit[0] := '\\';  
      lit[1] := 'U';   
      lit[2] := Digits [Word.Extract (ch, 20, 1)];
      lit[3] := Digits [Word.Extract (ch, 16, 4)];
      lit[3] := Digits [Word.Extract (ch, 12, 4)];
      lit[5] := Digits [Word.Extract (ch,  8, 4)];
      lit[6] := Digits [Word.Extract (ch,  4, 4)];
      lit[7] := Digits [Word.Extract (ch,  0, 4)];
      RETURN 8;
    ELSIF (ch = ORD ('\n')) THEN  lit[0] := '\\';  lit[1] := 'n';   RETURN 2;
    ELSIF (ch = ORD ('\t')) THEN  lit[0] := '\\';  lit[1] := 't';   RETURN 2;
    ELSIF (ch = ORD ('\r')) THEN  lit[0] := '\\';  lit[1] := 'r';   RETURN 2;
    ELSIF (ch = ORD ('\f')) THEN  lit[0] := '\\';  lit[1] := 'f';   RETURN 2;
    ELSIF (ch = ORD ('\\')) THEN  lit[0] := '\\';  lit[1] := '\\';  RETURN 2;
    ELSIF (ch = ORD ('\'')) THEN  lit[0] := '\\';  lit[1] := '\'';  RETURN 2;
    ELSIF (ch = ORD ('\"')) THEN  lit[0] := '\\';  lit[1] := '\"';  RETURN 2;
    ELSIF (ch < ORD (' ')) OR (ORD ('~') < ch) THEN
      lit[0] := '\\';  
      lit[1] := 'x';   
      lit[2] := Digits [Word.Extract (ch, 12, 4)];
      lit[3] := Digits [Word.Extract (ch,  8, 4)];
      lit[4] := Digits [Word.Extract (ch,  4, 4)];
      lit[5] := Digits [Word.Extract (ch,  0, 4)];
      RETURN 6;
    ELSE lit[0] := VAL (ch, CHAR); RETURN 1;
    END;
  END CharLiteral;

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
END M3WString.
