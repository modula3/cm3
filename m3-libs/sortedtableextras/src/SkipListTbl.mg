(* Created on Sat Nov 22 11:57:02 PST 1997 by heydon       *)
(* Last modified on Sat Nov 22 20:02:35 PST 1997 by heydon *)
(* Copyright (C) 1997, Digital Equipment Corporation       *)

GENERIC MODULE SkipListTbl(Key, Value, Tbl, SortedTbl);

IMPORT Random, Word;

TYPE
  ForwardPtrs = REF ARRAY OF Node;
  Node = REF RECORD
    k: Key.T;
    v: Value.T;
    back: Node := NIL;
    forward: ForwardPtrs := NIL;
  END;

REVEAL
  T = Public BRANDED Brand OBJECT
    num: CARDINAL;         (* number of elements in the table *)
    maxLevel: CARDINAL;    (* READONLY after "init" *)
    level: CARDINAL;       (* current list level *)
    root: Node := NIL;     (* root node; its "k" and "v" are ignored *)
    update: ForwardPtrs;   (* scratch area used in "put", "delete" *)
    rand: Random.T := NIL; (* random number generator *)
    randBits: INTEGER;     (* cache of random bits *)
    bitsRem: CARDINAL;     (* number of random bits remaining in "lastRand" *)
  OVERRIDES
    keyCompare := KeyCompare;
    init := Init;
    size := Size;
    get := Get;
    put := Put;
    delete := Delete;
    iterate := Iterate;
    iterateOrdered := IterateOrdered;
  END;

(* The node pointed to by "root" stores "level" forward pointers, and
   its "back" field points to the last element of the list if the list
   is non-empty.

   Invariants that hold after "init":

|  I0. root # NIL
|  I1. (root.back = NIL) = (num = 0)
|  I1. level <= maxLevel
|  I2. NUMBER(update^) = NUMBER(root.forward^) = maxLevel
|  I3. (forall i in [level, maxLevel): root.forward[i] = NIL)
|  I4. rand # NIL AND bitsRem = Word.Size
*)

CONST
  IterBrand = "(Iterator " & Brand & ")";

REVEAL
  Iterator = IteratorPublic BRANDED IterBrand OBJECT
    t: T;       (* corresponding tree *)
    curr: Node; (* current node in iteration *)
  END;

TYPE
  IteratorUp = Iterator OBJECT OVERRIDES
    reset := ResetUp;
    next := NextUp;
    seek := SeekUp;
  END;
  IteratorDown = Iterator OBJECT OVERRIDES
    reset := ResetDown;
    next := NextDown;
    seek := SeekDown;
  END;

(* Utility procedures ------------------------------------------------------ *)

PROCEDURE FlipOneFourthCoin(t: T): INTEGER =
  VAR res := Word.And(t.randBits, 2_11); BEGIN
    IF t.bitsRem > 2 THEN
      t.randBits := Word.RightShift(t.randBits, 2);
      DEC(t.bitsRem, 2)
    ELSE
      t.randBits := t.rand.integer();
      t.bitsRem := Word.Size
    END;
    RETURN res;
  END FlipOneFourthCoin;

PROCEDURE RandLevel(t: T): CARDINAL =
  VAR level: CARDINAL := 1; BEGIN
    WHILE level < t.maxLevel AND FlipOneFourthCoin(t) = 0 DO
      INC(level);
    END;
    RETURN level
  END RandLevel;

PROCEDURE NewNode(l: CARDINAL; READONLY k: Key.T; READONLY v: Value.T): Node =
(* Return a new node with level "l", key "k", and value "v". *)
  BEGIN
    RETURN NEW(Node, k := k, v := v, forward := NEW(ForwardPtrs, l))
  END NewNode;

PROCEDURE Seek(t: T; READONLY k: Key.T): Node =
(* Return the highest node in "t" with key less than "key". *)
  VAR alreadyChecked: Node := NIL; n := t.root; BEGIN
    FOR i := t.level - 1 TO 0 BY -1 DO
      VAR next := n.forward[i]; BEGIN
        WHILE next # alreadyChecked AND next # NIL AND
              t.keyCompare(next.k, k) < 0 DO
          n := next; next := n.forward[i]
        END;
        alreadyChecked := next;
      END
    END;
    RETURN n
  END Seek;

(* Tree method implementations --------------------------------------------- *)

PROCEDURE KeyCompare(<*UNUSED*> t: T; READONLY k1, k2: Key.T): [-1..1] =
  BEGIN RETURN Key.Compare(k1, k2) END KeyCompare;

PROCEDURE Init(t: T; maxSizeHint: CARDINAL; fixedSeed: BOOLEAN): T =
  BEGIN
    (* compute "t.maxLevel" as "ceiling(log_4(maxSizeHint))" *)
    t.maxLevel := 0;
    WHILE maxSizeHint > 0 DO
      maxSizeHint := Word.RightShift(maxSizeHint, 2);
      INC(t.maxLevel)
    END;
    t.maxLevel := MAX(t.maxLevel, 1);

    (* initialized "t.root" *)
    IF t.root = NIL THEN t.root := NEW(Node) END;
    IF t.root.forward = NIL OR NUMBER(t.root.forward^) # t.maxLevel THEN
      t.root.forward := NEW(ForwardPtrs, t.maxLevel);
      t.update := NEW(ForwardPtrs, t.maxLevel);
    END;
    t.root.back := NIL;
    FOR i := 0 TO t.maxLevel - 1 DO t.root.forward[i] := NIL END;

    (* initialize random number generator *)
    IF fixedSeed OR t.rand = NIL THEN
      t.rand := NEW(Random.Default).init(fixedSeed)
    END;
    t.randBits := t.rand.integer();
    t.bitsRem := Word.Size;
    <* ASSERT (t.bitsRem MOD 4) = 0 *>

    (* initialize other fields *)
    t.num := 0;
    t.level := 0;
    RETURN t
  END Init;

PROCEDURE Size(t: T): CARDINAL =
  BEGIN RETURN t.num END Size;

PROCEDURE Get(t: T; READONLY k: Key.T; VAR (*OUT*) v: Value.T): BOOLEAN =
  VAR alreadyChecked: Node := NIL; n := t.root; BEGIN
    FOR i := t.level - 1 TO 0 BY -1 DO
      VAR next := n.forward[i]; BEGIN
        WHILE next # alreadyChecked AND next # NIL AND
              t.keyCompare(next.k, k) < 0 DO
          n := next; next := n.forward[i]
        END;
        alreadyChecked := next
      END
    END;
    n := n.forward[0];
    IF n # NIL AND t.keyCompare(n.k, k) = 0 THEN
      v := n.v;
      RETURN TRUE
    END;
    RETURN FALSE
  END Get;

PROCEDURE Put(t: T; READONLY k: Key.T; READONLY v: Value.T): BOOLEAN =
  VAR alreadyChecked: Node := NIL; n := t.root; BEGIN
    FOR i := t.level - 1 TO 0 BY -1 DO
      VAR next := n.forward[i]; BEGIN
        WHILE next # alreadyChecked AND next # NIL AND
              t.keyCompare(next.k, k) < 0 DO
          n := next; next := n.forward[i]
        END;
        alreadyChecked := next;
        t.update[i] := n
      END
    END;
    n := n.forward[0];
    IF n # NIL AND t.keyCompare(n.k, k) = 0 THEN
      (* key already in table; set its value *)
      n.v := v;
      RETURN TRUE
    END;
    VAR newLevel := RandLevel(t); newNode := NewNode(newLevel, k, v); BEGIN
      (* correct "update" array if new level is new max *)
      IF newLevel > t.level THEN
        FOR i := t.level TO newLevel - 1 DO t.update[i] := t.root END;
        t.level := newLevel
      END;
      (* update forward pointers *)
      FOR i := 0 TO newLevel - 1 DO
        WITH prev = t.update[i].forward[i] DO
          newNode.forward[i] := prev;
          prev := newNode
        END
      END;
      (* update back pointers *)
      newNode.back := t.update[0];
      VAR nextNode := newNode.forward[0]; BEGIN
      	IF nextNode = NIL
      	  THEN t.root.back := newNode
      	  ELSE nextNode.back := newNode
      	END
      END
    END;
    INC(t.num);
    RETURN FALSE
  END Put;

PROCEDURE Delete(t: T; READONLY k: Key.T; VAR (*OUT*) v: Value.T): BOOLEAN =
  VAR alreadyChecked: Node := NIL; n := t.root; BEGIN
    FOR i := t.level - 1 TO 0 BY -1 DO
      VAR next := n.forward[i]; BEGIN
        WHILE next # alreadyChecked AND next # NIL AND
              t.keyCompare(next.k, k) < 0 DO
          n := next; next := n.forward[i]
        END;
        alreadyChecked := next;
        t.update[i] := n
      END
    END;
    n := n.forward[0];
    IF n = NIL OR t.keyCompare(n.k, k) # 0 THEN
      (* key not in table *)
      RETURN FALSE
    END;
    v := n.v;
    (* update forward pointers *)
    FOR i := 0 TO t.level - 1 DO
      WITH prevPtr = t.update[i].forward[i] DO
        IF prevPtr # n THEN EXIT END;
        prevPtr := n.forward[i]
      END
    END;
    (* update back pointers *)
    VAR nextNode := n.forward[0]; BEGIN
      IF nextNode = NIL
        THEN t.root.back := n.back
        ELSE nextNode.back := n.back
      END
    END;
    DEC(t.num);
    RETURN TRUE
  END Delete;

PROCEDURE Iterate(t: T): Tbl.Iterator =
  BEGIN RETURN IterateOrdered(t, TRUE) END Iterate;

PROCEDURE IterateOrdered(t: T; up: BOOLEAN): SortedTbl.Iterator =
  VAR res: Iterator; BEGIN
    IF up
      THEN res := NEW(IteratorUp);
      ELSE res := NEW(IteratorDown);
    END;
    res.t := t;
    res.reset();
    RETURN res
  END IterateOrdered;
 
(* Iterator method implementations ---------------------------------------- *)

PROCEDURE ResetUp(it: Iterator) =
  BEGIN it.curr := it.t.root.forward[0] END ResetUp;

PROCEDURE ResetDown(it: Iterator) =
  BEGIN it.curr := it.t.root.back END ResetDown;

PROCEDURE NextUp(it: Iterator; VAR (*OUT*) k: Key.T; VAR (*OUT*) v: Value.T):
    BOOLEAN =
  VAR curr := it.curr; BEGIN
    IF curr = NIL THEN RETURN FALSE END;
    k := curr.k; v := curr.v;
    it.curr := curr.forward[0];
    RETURN TRUE;
  END NextUp;

PROCEDURE NextDown(it: Iterator; VAR (*OUT*) k: Key.T; VAR (*OUT*) v: Value.T):
    BOOLEAN =
  VAR curr := it.curr; BEGIN
    IF curr = it.t.root THEN RETURN FALSE END;
    k := curr.k; v := curr.v;
    it.curr := curr.back;
    RETURN TRUE;
  END NextDown;

PROCEDURE SeekUp(it: Iterator; READONLY key: Key.T) =
  VAR n := Seek(it.t, key); BEGIN
    it.curr := n.forward[0]
  END SeekUp;

PROCEDURE SeekDown(it: Iterator; READONLY key: Key.T) =
  VAR n := Seek(it.t, key); next := n.forward[0]; BEGIN
    IF next # NIL AND it.t.keyCompare(next.k, key) = 0
      THEN it.curr := next
      ELSE it.curr := n
    END
  END SeekDown;

BEGIN
END SkipListTbl.
