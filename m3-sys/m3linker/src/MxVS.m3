(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: MxVS.m3                                               *)
(* Last Modified On Mon Aug  1 15:41:51 PDT 1994 By kalsow     *)

MODULE MxVS;

IMPORT Word;

TYPE
  InfoBuffer = REF ARRAY OF Info;
  HashTable  = REF ARRAY OF T;

VAR
  next_t    : T := NoVS + 1;
  info      := NEW (InfoBuffer, 2000);
  hashMask  : INTEGER := 2047; (* == 2^11-1 == 11 bits on *)
  hashTable := NEW (HashTable, 2048);

PROCEDURE Get (t: T;  VAR(*OUT*) i: Info) =
  BEGIN
    <*ASSERT 0 < t AND t < next_t*>
    i:= info[t];
  END Get;

PROCEDURE Put (READONLY i: Info): T =
  VAR
    t      : T;
    hash   : INTEGER  := Hash (i);
    bucket : CARDINAL := Word.And (hash, hashMask);
  BEGIN
    (* search the table *)
    LOOP
      t := hashTable[bucket];
      IF (t = NoVS) THEN (* empty! *) EXIT; END;
      IF (info[t] = i) THEN RETURN t; END;
      INC (bucket);
      IF (bucket >= NUMBER (hashTable^)) THEN bucket := 0; END;
    END;

    (* we didn't find a match => build a new one *)
    t := next_t;  INC (next_t);
    IF (t >= NUMBER (info^)) THEN ExpandInfo (); END;
    hashTable[bucket] := t;
    info[t] := i;

    (* make sure we're not overloading the hash table *)
    IF (next_t + next_t > NUMBER (hashTable^)) THEN ExpandHashTable (); END;

    RETURN t;
  END Put;

(*-------------------------------------------------------------- internal ---*)

PROCEDURE ExpandInfo () =
  VAR n := NUMBER (info^);  new := NEW (InfoBuffer, n+n);
  BEGIN
    SUBARRAY (new^, 0, n) := info^;
    info := new;
  END ExpandInfo;

PROCEDURE ExpandHashTable () =
  VAR
    n_old   := NUMBER (hashTable^);
    n_new   := n_old + n_old;
    new     := NEW (REF ARRAY OF T, n_new);
    newMask := hashMask + hashMask + 1;
    t       : T;
    bucket  : INTEGER;
  BEGIN
    FOR i := 0 TO n_new - 1 DO new[i] := NoVS END;

    FOR i := 0 TO n_old - 1 DO
      t := hashTable [i];
      IF (t # NoVS) THEN
        bucket := Word.And (Hash (info[t]), newMask);
        WHILE (new[bucket] # NoVS) DO
          INC (bucket);
          IF (bucket >= n_new) THEN bucket := 0; END;
        END;
        new[bucket] := t;
      END;
    END;

    hashMask := newMask;
    hashTable := new;
  END ExpandHashTable;

PROCEDURE Hash (READONLY i: Info): INTEGER =
  BEGIN
    RETURN Word.Plus (Word.Plus (Word.Times (i.source, 37),
                                 Word.Times (i.symbol, 17)),
                      Word.Plus (Word.Times (i.stamp.byte[0], 73),
                                 Word.Times (i.stamp.byte[1], 91)));
  END Hash;

BEGIN
END MxVS.
