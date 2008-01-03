(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Apr  8 15:54:13 PDT 1994 by kalsow                   *)

MODULE TextDB;

IMPORT TextList, RefList, TextIntTbl, Text;
IMPORT DBRd, DBWr;

(*-------------------------------------------------------------- database ---*)

REVEAL
  T = T_ BRANDED OBJECT
    words     : WordSet;
    relations : RefList.T;
  OVERRIDES
    init            := InitDB;
    all_relations   := AllRelations;
    get_relation    := GetRelation;
    create_relation := CreateRelation;
    load            := LoadDB;
    dump            := DumpDB;
  END;

PROCEDURE InitDB (self: T): T =
  BEGIN
    self.words     := NEW (WordSet).init ();
    self.relations := NIL;
    RETURN self;
  END InitDB;

PROCEDURE AllRelations (self: T): RefList.T =
  VAR x: RefList.T := NIL;  r := self.relations;
  BEGIN
    WHILE (r # NIL) DO
      x := RefList.Cons (r.head, x);
      r := r.tail;
    END;
    RETURN x;
  END AllRelations;

PROCEDURE GetRelation (self: T;  nm: TEXT): Relation =
  VAR x := self.relations;  r: Relation;
  BEGIN
    WHILE (x # NIL) DO
      r := x.head;
      IF Text.Equal (r.name(), nm) THEN RETURN r; END;
      x := x.tail;
    END;
    RETURN NIL;
  END GetRelation;

PROCEDURE CreateRelation (self: T;  nm: TEXT): Relation =
  VAR r := GetRelation (self, nm);
  BEGIN
    IF (r = NIL) THEN
      r := NEW (Relation).init (nm, self.words);
      self.relations := RefList.Cons (r, self.relations);
    END;
    RETURN r;
  END CreateRelation;

PROCEDURE LoadDB (self: T;  path: TEXT) =
  VAR rd := NEW (DBRd.T).init (path);  r: Relation;
  BEGIN
    self.words.load (rd);
    FOR i := 1 TO rd.get_int () DO
      r := CreateRelation (self, rd.get_line ());
      r.load (rd);
    END;
    rd.close ();
  END LoadDB;

PROCEDURE DumpDB (self: T;  path: TEXT) =
  VAR wr := NEW (DBWr.T).init (path);
  BEGIN
    self.words.dump (wr);
    wr.put_int (RefList.Length (self.relations));
    VAR x := self.relations; BEGIN
      WHILE (x # NIL) DO
        VAR r: Relation := x.head;  BEGIN
          wr.put_line (r.name ());
          r.dump (wr);
        END;
        x := x.tail;
      END;
    END;
    wr.close ();
  END DumpDB;

(*------------------------------------------------------------- relations ---*)

TYPE
   Pair = RECORD key, value: INTEGER;  END;
   PairList = REF ARRAY OF Pair;
   PairSet = RECORD
     cnt  : CARDINAL;
     elts : PairList;
   END;

(* value = MissingWord ==> deleted entry *)

REVEAL
  Relation = R_ BRANDED OBJECT
    id    : TEXT;
    words : WordSet;
    old   : PairSet;
    new   : PairSet;
  METHODS
    init (nm: TEXT;  words: WordSet): Relation := InitRelation;
    load (rd: DBRd.T) := LoadRelation;
    dump (wr: DBWr.T) := DumpRelation;
  OVERRIDES
    name     := RelationName;
    getValue := GetValue;
    getKey   := GetKey;
    insert   := Insert;
    delete   := Delete;
    deleteValue := DeleteValue;
  END;

PROCEDURE InitRelation (r: Relation;  nm: TEXT;  words: WordSet): Relation =
  BEGIN
    r.id       := nm;
    r.words    := words;
    r.old.cnt  := 0;
    r.old.elts := NEW (PairList, 100);
    r.new.cnt  := 0;
    r.new.elts := NEW (PairList, 100);
    RETURN r;
  END InitRelation;

PROCEDURE RelationName (r: Relation): TEXT =
  BEGIN
    RETURN r.id;
  END RelationName;

PROCEDURE GetValue (r: Relation;  key: TEXT): TextList.T =
  VAR w := r.words.get (key);  x: TextList.T := NIL;
  BEGIN
    IF (w = MissingWord) THEN RETURN NIL; END;

    FOR i := GetKeyIndex (r.old, w) TO r.old.cnt-1 DO
      WITH p = r.old.elts[i] DO
        IF (p.key # w) THEN EXIT; END;
        IF (p.value # MissingWord) THEN
          x := TextList.Cons (r.words.text (p.value), x);
        END;
      END;
    END;

    FOR i := GetKeyIndex (r.new, w) TO r.new.cnt-1 DO
      WITH p = r.new.elts[i] DO
        IF (p.key # w) THEN EXIT; END;
        IF (p.value # MissingWord) THEN
          x := TextList.Cons (r.words.text (p.value), x);
        END;
      END;
    END;

    RETURN x;
  END GetValue;

PROCEDURE GetKey (r: Relation;  value: TEXT): TextList.T =
  VAR w := r.words.get (value);  x: TextList.T := NIL;
  BEGIN
    IF (w = MissingWord) THEN RETURN NIL; END;

    (* linear search of both sets *)
    FOR i := 0 TO r.old.cnt-1 DO
      WITH p = r.old.elts[i] DO
        IF (p.value = w) THEN
          x := TextList.Cons (r.words.text (p.key), x);
        END;
      END;
    END;
    FOR i := 0 TO r.new.cnt-1 DO
      WITH p = r.new.elts[i] DO
        IF (p.value = w) THEN
          x := TextList.Cons (r.words.text (p.key), x);
        END;
      END;
    END;

    RETURN x;
  END GetKey;

PROCEDURE Delete (r: Relation;  key, value: TEXT) =
  VAR k := r.words.get (key);  v := r.words.get (value);
  BEGIN
    IF (k = MissingWord) OR (v = MissingWord) THEN RETURN; END;

    FOR i := GetKeyIndex (r.old, k) TO r.old.cnt-1 DO
      WITH p = r.old.elts[i] DO
        IF (p.key # k) THEN EXIT; END;
        IF (p.value = v) THEN  p.value := MissingWord; END;
      END;
    END;

    FOR i := GetKeyIndex (r.new, k) TO r.new.cnt-1 DO
      WITH p = r.new.elts[i] DO
        IF (p.key # k) THEN EXIT; END;
        IF (p.value = v) THEN  p.value := MissingWord; END;
      END;
    END;
  END Delete;

PROCEDURE DeleteValue (r: Relation;  value: TEXT) =
  VAR w := r.words.get (value);
  BEGIN
    IF (w = MissingWord) THEN RETURN; END;

    (* linear search of both sets *)
    FOR i := 0 TO r.old.cnt-1 DO
      WITH p = r.old.elts[i] DO
        IF (p.value = w) THEN p.value := MissingWord; END;
      END;
    END;
    FOR i := 0 TO r.new.cnt-1 DO
      WITH p = r.new.elts[i] DO
        IF (p.value = w) THEN p.value := MissingWord; END;
      END;
    END;
  END DeleteValue;

PROCEDURE Insert (r: Relation;  key, value: TEXT) =
  (* keep the new set sorted, by inserting each pair in place *)
  VAR k := r.words.add (key);  v := r.words.add (value);
  BEGIN
    IF (r.new.cnt >= NUMBER (r.new.elts^)) THEN MergeUpdates (r); END;
    VAR i : INTEGER := r.new.cnt-1; BEGIN
      WHILE (i >= 0) AND (r.new.elts[i].key > k) DO
        r.new.elts[i+1] := r.new.elts[i];  DEC (i);
      END;
      WITH p = r.new.elts[i+1] DO  p.key := k;  p.value := v;  END;
    END;
    INC (r.new.cnt);
  END Insert;

PROCEDURE MergeUpdates (r: Relation) =
  VAR n, o: CARDINAL;
  BEGIN
    (* make sure we have enough room in "old" *)
    IF (r.old.cnt + r.new.cnt >= NUMBER (r.old.elts^)) THEN
      VAR new := NEW (PairList, 2 * NUMBER (r.old.elts^)); BEGIN
        SUBARRAY (new^, 0, NUMBER (r.old.elts^)) := r.old.elts^;
        r.old.elts := new;
      END;
    END;

    (* move the old elements out of the way *)
    VAR i := LAST (r.old.elts^);  BEGIN
      FOR j := r.old.cnt-1 TO 0 BY -1 DO
        WITH p = r.old.elts[j] DO
          IF (p.value # MissingWord) THEN
            r.old.elts[i] := p;  DEC (i);
          END;
        END;
      END;
      o := i+1;
    END;

    (* merge the two lists *)
    n := 0;  r.old.cnt := 0;
    WHILE (n < r.new.cnt) AND (o < NUMBER (r.old.elts^)) DO
      (* both lists are non-empty *)
      WITH nx = r.new.elts[n],  ox = r.old.elts[o]  DO
        IF (nx.value = MissingWord) THEN  INC (n);
        ELSIF (nx.key < ox.key)
          THEN r.old.elts[r.old.cnt] := nx;  INC (r.old.cnt);  INC (n);
          ELSE r.old.elts[r.old.cnt] := ox;  INC (r.old.cnt);  INC (o);
        END;
      END;
    END;

    (* add the remaining old elements *)
    WHILE (o < NUMBER (r.old.elts^)) DO
      r.old.elts[r.old.cnt] := r.old.elts[o];  INC (r.old.cnt);  INC (o);
    END;

    (* add the remaining new elements *)
    WHILE (n < r.new.cnt) DO
      WITH nx = r.new.elts[n] DO
        IF (nx.value # MissingWord) THEN
          r.old.elts[r.old.cnt] := nx;  INC (r.old.cnt);
        END;
        INC (n);
      END;
    END;

    r.new.cnt := 0;
  END MergeUpdates;

PROCEDURE GetKeyIndex (READONLY p: PairSet;  key: INTEGER): CARDINAL =
  (* Returns the smallest i such that (p.elts[i].key = key).  If
     no such pair exists, return p.cnt. *)
  VAR
    lo   : CARDINAL := 0;
    hi   : CARDINAL := p.cnt;
    mid  : CARDINAL;
 BEGIN
    WHILE (lo < hi) DO
      mid := (lo + hi) DIV 2;
      IF (key < p.elts[mid].key)
        THEN hi := mid;
        ELSE lo := mid + 1;
      END;
    END;
    IF (lo > 0) THEN DEC (lo) END;
    IF (p.elts[lo].key # key) THEN RETURN p.cnt; END;
    WHILE (lo > 0) AND (p.elts[lo-1].key = key) DO DEC (lo); END;
    RETURN lo;
  END GetKeyIndex;

PROCEDURE LoadRelation (r: Relation;  rd: DBRd.T) =
  VAR n := rd.get_int ();
  BEGIN
    IF (n > NUMBER (r.old.elts^)) THEN
      r.old.elts := NEW (PairList, 3 * n DIV 2);
    END;
    FOR i := 0 TO n-1 DO
      WITH p = r.old.elts[i] DO
        p.key   := rd.get_int ();
        p.value := rd.get_int ();
      END;
    END;
    r.old.cnt := n;
    r.new.cnt := 0;
  END LoadRelation;

PROCEDURE DumpRelation (r: Relation;  wr: DBWr.T) =
  BEGIN
    MergeUpdates (r);
    wr.put_int (r.old.cnt);
    FOR i := 0 TO r.old.cnt-1 DO
      WITH p = r.old.elts[i] DO
        wr.put_int (p.key);
        wr.put_int (p.value);
      END;
    END;
  END DumpRelation;

(*----------------------------------------------------------------- words ---*)

CONST
  MissingWord = FIRST(INTEGER);

TYPE
  WordSet = OBJECT
    map : TextIntTbl.T;
    cnt : INTEGER;
    word: REF ARRAY OF TEXT;
  METHODS
    init (): WordSet := InitWordSet;
    add  (word: TEXT): INTEGER := AddWord;
    get  (word: TEXT): INTEGER := GetWord;
    text (id: INTEGER): TEXT   := WordText;
    load (rd: DBRd.T)          := LoadWords;
    dump (wr: DBWr.T)          := DumpWords;
  END;

PROCEDURE InitWordSet (w: WordSet): WordSet =
  BEGIN
    w.map := NEW (TextIntTbl.Default).init ();
    w.cnt := 0;
    w.word := NEW (REF ARRAY OF TEXT, 100);
    RETURN w;
  END InitWordSet;

PROCEDURE AddWord (w: WordSet;  word: TEXT): INTEGER =
  VAR id: INTEGER;
  BEGIN
    IF NOT w.map.get (word, id) THEN
      id := w.cnt;  INC (w.cnt);
      EVAL w.map.put (word, id);
      IF (id >= NUMBER (w.word^)) THEN ExpandWords (w); END;
      w.word [id] := word;
    END;
    RETURN id;
  END AddWord;

PROCEDURE ExpandWords (w: WordSet) =
  VAR
    n := NUMBER (w.word^);
    new := NEW (REF ARRAY OF TEXT, n + n);
  BEGIN
    SUBARRAY (new^, 0, n) := w.word^;
    w.word := new;
  END ExpandWords;

PROCEDURE GetWord (w: WordSet;  word: TEXT): INTEGER =
  VAR id: INTEGER;
  BEGIN
    IF w.map.get (word, id)
      THEN RETURN id;
      ELSE RETURN MissingWord;
    END;
  END GetWord;

PROCEDURE WordText (w: WordSet;  id: INTEGER): TEXT =
  BEGIN
    IF (0 < id) AND (id < w.cnt)
      THEN RETURN w.word[id];
      ELSE RETURN NIL;
    END;
  END WordText;

PROCEDURE LoadWords (w: WordSet;  rd: DBRd.T) =
  BEGIN
    FOR i := 1 TO rd.get_int () DO
      EVAL w.add (rd.get_line ());
    END;
  END LoadWords;

PROCEDURE DumpWords (w: WordSet;  wr: DBWr.T) =
  BEGIN
    wr.put_int (w.cnt);
    FOR i := 0 TO w.cnt-1 DO
      wr.put_line (w.word[i]);
    END;
  END DumpWords;

BEGIN
END TextDB.

