(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Revelation.m3                                         *)
(* Last modified on Tue Dec 20 15:23:30 PST 1994 by kalsow     *)
(*      modified on Sat Aug 25 02:55:44 1990 by muller         *)

MODULE Revelation;

IMPORT M3ID, Value, Type, Error, OpaqueType, Scope, Decl, Host;
IMPORT ObjectType, RefType, Scanner, Token, Module, ValueRep, CG;
IMPORT M3RT, Target, Reff;
IMPORT PersistentRevelation, PersistentRevelationArraySort, PersistentRevelationSeq, PersistentRevelationSeqRep;
FROM Scanner IMPORT GetToken, Fail, Match, MatchID, cur;
FROM M3 IMPORT QID;

TYPE
  T = BRANDED "Revelation.T" REF RECORD
        home    : Value.T; (* the containing interface or module *)
        env     : Scope.T;
        qid     : QID;
        obj     : Value.T; (* value named by 'qid' in scope 'env' *)
        rhs     : Type.T;  (* REVEAL qid (<:|=) rhs *)
        lhs     : Type.T;  (* == type that corresponds to qid *)
        equal   : BOOLEAN; (* TRUE => lhs = rhs, FALSE => lhs <: rhs *)
        checked : BOOLEAN;
        origin  : INTEGER;
      END;

TYPE
  List = BRANDED "Revelation.List" REF RECORD
           next  : List;
           ident : T;
           local : BOOLEAN; (* as opposed to inherited *)
           used  : BOOLEAN;
           home  : Value.T; (* External.T that caused the import *)
         END;

TYPE
  Node = BRANDED "Revelation.Node" REF RECORD
           next     : Node;
           key      : Type.T;
           best     : List     := NIL;
           contents : List     := NIL;
           reducing : BOOLEAN  := FALSE;
         END;

TYPE
  HashTable = REF ARRAY OF Node;

REVEAL
  Set = BRANDED "Revelation.Set" REF RECORD
          home   : Value.T    := NIL;
          count  : INTEGER    := 0;
          idents : List       := NIL;
          hash   : HashTable  := NIL;
          seq    : PersistentRevelationSeq.T := NIL;
          (* The visible revelations are in the union of idents and hash
           * seq is a temporary, placed here to reduce heap allocations.
           *)
        END;

TYPE
  Iterator = RECORD
    cur       : List;
    set       : Set;
    next_list : List;
    next_node : Node;
    next_hash : INTEGER;
    max_hash  : INTEGER;
  END;

VAR top := NewSet (NIL);

PROCEDURE NewSet (module: Value.T): Set =
  BEGIN
    RETURN NEW (Set, home := module,
                     seq := NEW(PersistentRevelationSeq.T).init());
  END NewSet;

PROCEDURE Push (s: Set): Set =
  VAR old := top;
  BEGIN
    <* ASSERT s # NIL *>
    top := s;
    RETURN old;
  END Push;

PROCEDURE Pop (s: Set) =
  BEGIN
    top := s;
  END Pop;

PROCEDURE Parse (<*UNUSED*> READONLY att: Decl.Attributes) =
  TYPE TK = Token.T;
  VAR id, id2: M3ID.T;  loc: INTEGER;
  BEGIN
    Match (TK.tREVEAL);
    WHILE (cur.token = TK.tIDENT) DO
      id2 := M3ID.NoID;
      id  := MatchID ();
      IF (cur.token = TK.tDOT) THEN
        GetToken (); (* . *)
        id2 := id;
        id := MatchID ();
      END;
      loc := Scanner.offset;
      CASE cur.token OF
      | TK.tEQUAL =>
          GetToken (); (* = *)
          New (id2, id, Type.Parse (), TRUE, loc);
      | TK.tSUBTYPE =>
          GetToken (); (* <: *)
          New (id2, id, Type.Parse (), FALSE, loc);
      ELSE Fail ("missing \'=\' or \'<:\'");
      END;
      Match (TK.tSEMI);
    END;
  END Parse;

PROCEDURE New (module, name: M3ID.T;  rhs: Type.T;  eq: BOOLEAN; loc: INTEGER)=
  VAR t: T;
  BEGIN
    <* ASSERT top.home # NIL *>
    t := NEW (T);
    t.home       := top.home;
    t.qid.module := module;
    t.qid.item   := name;
    t.obj        := NIL;
    t.rhs        := rhs;
    t.lhs        := NIL;
    t.equal      := eq;
    t.checked    := FALSE;
    t.origin     := loc;
    t.env        := Scope.Top ();
    AddOne (t, TRUE, NIL);
  END New;

PROCEDURE Inherit (s: Set;  import: Value.T) =
  VAR iter: Iterator;
  BEGIN
    InitIterator (s, iter);
    WHILE Iterate (iter) DO
      IF (iter.cur.local) THEN AddOne (iter.cur.ident, FALSE, import) END;
    END;
  END Inherit;

PROCEDURE AddOne (t: T;  isLocal: BOOLEAN;  import: Value.T) =
  VAR y := NEW (List, ident := t, local := isLocal,
                         used := FALSE, home := import);
  BEGIN
    y.next := top.idents;
    top.idents := y;
    INC (top.count);
  END AddOne;

PROCEDURE TypeCheck (s: Set) =
  VAR l: List;  save, n_buckets: INTEGER;  n: Node;
  BEGIN
    IF (s.count <= 0) THEN RETURN END;

    save := Scanner.offset;

    (* allocate and initialize the hash table *)
    <*ASSERT s.hash = NIL*> (* otherwise we've been checked twice!? *)
    n_buckets := 2 * s.count;
    s.hash := NEW (HashTable, n_buckets);
    FOR i := 0 TO n_buckets - 1 DO s.hash[i] := NIL END;

    (* bind the lhs qid's to types & map them into the hash table *)
    WHILE (s.idents # NIL) DO
      l := s.idents;
      s.idents := l.next;
      IF DoBind (l.ident) THEN HashInsert (s, l) END; 
    END;

    (* type check the lhs and rhs *)
    FOR i := 0 TO n_buckets-1 DO
      n := s.hash[i];
      WHILE (n # NIL) DO
        l := n.contents;
        WHILE (l # NIL) DO DoCheck0 (l.ident); l := l.next;  END;
        n := n.next;
      END;
    END;

    (* type check the lhs and rhs *)
    FOR i := 0 TO n_buckets-1 DO
      n := s.hash[i];
      WHILE (n # NIL) DO
        l := n.contents;
        WHILE (l # NIL) DO DoCheck (l.ident); l := l.next;  END;
        n := n.next;
      END;
    END;

    (* find the strongest revelation for each type *)
    FOR i := 0 TO n_buckets-1 DO
      n := s.hash [i];
      WHILE (n # NIL) DO
        Reduce (s, n.key);
        n := n.next;
      END;
    END;

    Scanner.offset := save;
  END TypeCheck;

PROCEDURE DoBind (t: T) : BOOLEAN (*Success*) =
  VAR obj: Value.T;
  BEGIN
    IF (t.checked) THEN RETURN TRUE END;
    Scanner.offset := t.origin;
    obj := Scope.LookUpQID (t.env, t.qid);
    t.obj := obj;
    IF (obj = NIL) THEN
      Error.QID (t.qid, "undefined");
      RETURN FALSE;
    ELSIF (Value.ClassOf (obj) # Value.Class.Type) THEN
      Error.QID (t.qid, "is not a type");
      RETURN FALSE;
    ELSE
      t.lhs := Value.ToType (obj);
    END;
    t.lhs := Type.Strip (t.lhs);
    RETURN TRUE; 
  END DoBind;

PROCEDURE HashInsert (s: Set;  l: List) =
  VAR lhs := l.ident.lhs;
  VAR hsh := OpaqueType.UID (lhs) MOD NUMBER (s.hash^);
  VAR n   := s.hash [hsh];
  VAR x: List;
  BEGIN
    (* look for the node that contains l's revelations *)
    LOOP
      IF (n = NIL) THEN
        (* we didn't find a node for this type *)
        n := NEW (Node, next := s.hash[hsh], key := lhs);
        s.hash [hsh] := n;
        EXIT;
      END;
      IF Type.IsEqual (n.key, lhs, NIL) THEN EXIT END;
      n := n.next;
    END;

    (* check for a duplicate revelation (possible because both "IMPORT X"
       and "FROM X IMPORT" are allowed in a single unit.  They cause X's
       revelations to be inherited twice.  sigh. *)
    x := n.contents;
    WHILE (x # NIL) DO
      IF (x.ident = l.ident) THEN (* drop 'l' on the floor *) RETURN END;
      x := x.next;
    END;

    (* add 'l' to the list *)
    l.next := n.contents;
    n.contents := l;
  END HashInsert;

PROCEDURE DoCheck0 (t: T) =
  BEGIN
    Scanner.offset := t.origin;
    EVAL Type.Check (t.rhs);
    (** t.rhs := Type.Check (t.rhs);  -- we don't want to save the
      checked RHS, otherwise if the RHS is a named type we'll miss
      the check below which requires a full revelation to have a
      branded constructor. **)
    t.lhs := Type.Check (t.lhs);
  END DoCheck0;

PROCEDURE DoCheck (t: T) =
  VAR  xx: Type.T;  name: TEXT;
  BEGIN
    IF (t.checked) THEN RETURN END;
    Scanner.offset := t.origin;

    IF (NOT OpaqueType.Is (t.lhs)) THEN
      Error.QID (t.qid, "is not an opaque type");
    ELSIF NOT Type.IsSubtype (t.rhs, OpaqueType.Super (t.lhs)) THEN
      Error.QID (t.qid, "identification is not to a legal subtype");
    END;

    IF (t.equal) THEN
      xx := Type.Strip (t.rhs);
      IF (xx # t.rhs)
        OR xx = Reff.T 
           (* Reff.T is REFANY, and it is initialized with a brand field. *) 
        OR NOT (RefType.IsBranded (xx) OR ObjectType.IsBranded (xx)) THEN
        Error.QID (t.qid, "right-hand side must be a branded type expression");
        t.rhs := xx;
      END;

      name := Value.GlobalName (t.obj);
      IF RefType.Is (t.rhs)
        THEN RefType.NoteRefName (t.rhs, name);
        ELSE ObjectType.NoteRefName (t.rhs, name);
      END;
    END;

    t.checked := TRUE;
  END DoCheck;

PROCEDURE Reduce (s: Set;  key: Type.T) =
  VAR x: INTEGER;  n: Node;  best, l: List;
  BEGIN
    IF (key = NIL) THEN RETURN END;
    key := Type.Strip (key);

    x := OpaqueType.UID (key);
    IF (x = 0) THEN
      (* it's not an opaque type *)
      Reduce (s, ObjectType.Super (key));
      RETURN;
    END;
    Reduce (s, OpaqueType.Super (key));

    (* find the hash table node *)
    x := x MOD NUMBER (s.hash^);
    n := s.hash [x];
    LOOP
      IF (n = NIL) THEN RETURN END;
      IF Type.IsEqual (n.key, key, NIL) THEN EXIT END;
      n := n.next;
    END;

    IF (n.reducing) THEN (*recursive call*) RETURN END;
    IF (n.best # NIL) THEN (*done*) RETURN END;
    n.reducing := TRUE;

    (* first, reduce the rhs's *)
    l := n.contents;
    WHILE (l # NIL) DO
      Reduce (s, l.ident.rhs);
      l := l.next;
    END;

    (* finally, search for the best candidate *)
    best := n.contents;
    l := best.next;
    WHILE (l # NIL) DO

      best.ident.rhs := Type.Check (best.ident.rhs);
      l.ident.rhs := Type.Check (l.ident.rhs);
      (* We need these checks since Reduce can be called by
         LookUpAll during an active call to  TypeCheck. *)

      IF Type.IsSubtype (best.ident.rhs, l.ident.rhs) THEN
        (* best is better than l *)
        IF (l.ident.equal) THEN TooStrong (l, best) END;
      ELSIF Type.IsSubtype (l.ident.rhs, best.ident.rhs) THEN
        (* l is better than best *)
        IF (best.ident.equal) THEN TooStrong (best, l) END;
        best := l;
      ELSE (* unrelated revelations! *)
        Scanner.offset := best.ident.origin;
        Error.QID (best.ident.qid, "non-comparable revelation");
        Scanner.offset := l.ident.origin;
        Error.QID (l.ident.qid, "non-comparable revelation");
      END;
      l := l.next;
    END;

    n.best := best;
  END Reduce;

PROCEDURE TooStrong (xa, xb: List) =
  (* a.ident.equal *)
  VAR a := xa.ident;  b := xb.ident;
  BEGIN
    <*ASSERT a.equal *>
    IF (b.equal) THEN
      Scanner.offset := a.origin;
      Error.QID (a.qid, "multiple full revelations");
      Scanner.offset := b.origin;
      Error.QID (b.qid, "multiple full revelations");
    ELSE
      Scanner.offset := b.origin;
      Error.QID (b.qid, "partial revelation is stronger than full revelation");
    END;
  END TooStrong;

PROCEDURE LookUp (key: Type.T): Type.T =
  VAR h: INTEGER;  x: Type.T;
  BEGIN
    key := Type.Strip (key);
    IF (top.hash # NIL) THEN
      h := OpaqueType.UID (key) MOD NUMBER (top.hash^);
      x := SearchEQ (top.hash [h], key);
      IF (x # NIL) THEN RETURN x END;
    END;
    RETURN SearchListEQ (top.idents, key);
  END LookUp;

PROCEDURE SearchEQ (n: Node;  key: Type.T): Type.T =
  VAR l: List;  t: T;
  BEGIN
    (* look for the chain header *)
    LOOP
      IF (n = NIL) THEN RETURN NIL END;
      IF Type.IsEqual (n.key, key, NIL) THEN EXIT END;
      n := n.next;
    END;

    (* has it already been reduced? *)
    l := n.best;
    IF (l # NIL) THEN
      t := l.ident;
      IF NOT t.equal THEN RETURN NIL END;
      IF (NOT t.checked) THEN CheckRHS (t) END;
      NoteUse (l);
      RETURN t.rhs;
    END;

    (* no, then search the full list for a match *)
    RETURN SearchListEQ (n.contents, key);
  END SearchEQ;

PROCEDURE SearchListEQ (l: List;  key: Type.T): Type.T =
  VAR t: T;
  BEGIN
    WHILE (l # NIL) DO
      t := l.ident;
      <* ASSERT t.lhs # NIL OR t.rhs = NIL *> (* => LHS is bound *)
      IF (t.equal) AND Type.IsEqual (t.lhs, key, NIL) THEN
        IF (NOT t.checked) THEN CheckRHS (t) END;
        NoteUse (l);
        RETURN t.rhs;
      END;
      l := l.next;
    END;
    RETURN NIL; (* didn't find a full revelation *)
  END SearchListEQ;

PROCEDURE LookUpAll (key: Type.T;  VAR(*OUT*) x: TypeSet) =
  VAR h: INTEGER;
  BEGIN
    x.cnt := 0;
    x.others := NIL;
    key := Type.Strip (key);
    IF (top.idents # NIL) THEN
      SearchListAll (top.idents, key, x);
    END;
    IF (top.hash # NIL) THEN
      Reduce (top, key);
      h := OpaqueType.UID (key) MOD NUMBER (top.hash^);
      SearchAll (top.hash [h], key, x);
    END;
  END LookUpAll;

PROCEDURE SearchAll (n: Node;  key: Type.T;  VAR x: TypeSet) =
  VAR t: T;
  BEGIN
    (* search the list for a matching node *)
    LOOP
      IF (n = NIL) THEN RETURN END;
      IF Type.IsEqual (n.key, key, NIL) THEN EXIT END;
      n := n.next;
    END;

    IF (n.best # NIL) THEN
      NoteUse (n.best);
      t := n.best.ident;
      <* ASSERT Type.IsEqual (t.lhs, key, NIL) *>
      IF (NOT t.checked) THEN CheckRHS (t) END;
      AddType (x, t.rhs);
    ELSE
      (* we haven't reduced this node yet => return all possible nodes *)
      SearchListAll (n.contents, key, x);
    END;
  END SearchAll;

PROCEDURE SearchListAll (l: List;  key: Type.T;  VAR x: TypeSet) =
  VAR t: T;
  BEGIN
    WHILE (l # NIL) DO
      t := l.ident;
      <* ASSERT t.lhs # NIL OR t.rhs = NIL *> (* => LHS is bound *)
      IF Type.IsEqual (t.lhs, key, NIL) THEN
        NoteUse (l);
        IF (NOT t.checked) THEN CheckRHS (t) END;
        AddType (x, t.rhs);
      END;
      l := l.next;
    END;
  END SearchListAll;

PROCEDURE AddType (VAR x: TypeSet;  t: Type.T) =
  BEGIN
    IF (x.cnt < NUMBER (x.types)) THEN
      x.types [x.cnt] := t;
      INC (x.cnt);
    ELSE
      x.others := NEW (TypeList, next := x.others, type := t);
    END;
  END AddType;

PROCEDURE NoteUse (l: List) =
  BEGIN
    IF (Scanner.in_main) THEN
      l.used := TRUE;
      IF (l.home # NIL) THEN l.home.used := TRUE END;
    END;
  END NoteUse;

PROCEDURE CheckRHS (t: T) =
  (* we're doing a lookup while the revelations are being checked... *)
  VAR save := Scanner.offset;
  BEGIN
    t.rhs := Type.Check (t.rhs);
    Scanner.offset := save;
  END CheckRHS;

PROCEDURE Reuse (s: Set) =
  VAR iter: Iterator;
  BEGIN
    InitIterator (s, iter);
    WHILE Iterate (iter) DO
      iter.cur.used := FALSE;
    END;
  END Reuse;

PROCEDURE Declare (s: Set;  VAR full_info, partial_info: INTEGER) =
  VAR l: List;  n_full, n_partial := 0;  iter: Iterator;
  BEGIN
    (* generate the link info for the revelations defined or used here
       and count the exported revelations *)
    InitIterator (s, iter);
    WHILE Iterate (iter) DO
      l := iter.cur;
      IF (l.used) OR (l.local) THEN
        DeclareRevelation (l.ident, l.local, n_full, n_partial);
      END;
    END;

    full_info := -1;
    partial_info := -1;
    IF (n_full > 0) THEN
      full_info := GenList (s, n_full, TRUE);
    END;
    IF (n_partial > 0) THEN
      partial_info := GenList (s, n_partial, FALSE);
    END;
  END Declare;

PROCEDURE GenList (s: Set;  cnt: INTEGER;  eq: BOOLEAN): INTEGER =
  VAR
    base := Module.Allocate (cnt * M3RT.RV_SIZE + Target.Integer.size,
                             Target.Integer.align, TRUE, "revelations");
    offs := base;
    iter : Iterator;
    l    : List;
    seq   := s.seq;
    array : REF ARRAY OF PersistentRevelation.T;
  BEGIN

    (* Collect into array in hash order, sort, and output. *)

    seq.sz := 0;
    <*ASSERT seq.st = 0*>

    InitIterator (s, iter);
    WHILE Iterate (iter) DO
      l := iter.cur;
      IF (l.local) AND (l.ident.equal = eq) THEN
        seq.addhi (PersistentRevelation.T {
                    Type.GlobalUID (l.ident.lhs),
                    Type.GlobalUID (l.ident.rhs)});
      END;
    END;

    array := seq.elem;

    PersistentRevelationArraySort.Sort(SUBARRAY(array^, 0, seq.sz));

    FOR i := 0 TO seq.sz - 1 DO
      CG.Init_intt (offs + M3RT.RV_lhs_id, Target.Integer.size, array[i].lhs_id, TRUE);
      CG.Init_intt (offs + M3RT.RV_rhs_id, Target.Integer.size, array[i].rhs_id, TRUE);
      INC (offs, M3RT.RV_SIZE);
    END;

    RETURN base;
  END GenList;

PROCEDURE DeclareRevelation (t: T;  exported: BOOLEAN;
                             VAR full, partial: INTEGER) =
  VAR
    interface := (NOT exported) OR (Module.IsInterface ());
    lhs_uid   := Type.GlobalUID (t.lhs);
    rhs_uid   := Type.GlobalUID (t.rhs);
  BEGIN
    Value.Declare (t.obj);
    Type.Compile (t.rhs);
    Type.Compile (t.lhs);
    Host.env.note_revelation (t.home.name, interface,
                             lhs_uid, rhs_uid, t.equal, NOT exported);
    IF (exported) THEN
      IF (t.equal) THEN
        CG.Reveal_opaque (lhs_uid, rhs_uid);
        ObjectType.NoteOffsets (t.lhs, t.rhs);
        INC (full);
      ELSE
        INC (partial);
      END;
    END;
  END DeclareRevelation;

PROCEDURE InitIterator (s: Set;  VAR i: Iterator) =
  BEGIN
    i.cur       := NIL;
    i.set       := s;
    i.next_list := s.idents;
    i.next_node := NIL;
    i.next_hash := 0;
    i.max_hash  := 0;
    IF (s.hash # NIL) THEN i.max_hash := NUMBER (s.hash^); END;
  END InitIterator;

PROCEDURE Iterate (VAR i: Iterator): BOOLEAN =
  BEGIN
    IF i.next_list # NIL THEN
      i.cur       := i.next_list;
      i.next_list := i.next_list.next;
      RETURN TRUE;
    END;
    IF (i.next_node # NIL) THEN
      i.next_list := i.next_node.contents;
      i.next_node := i.next_node.next;
      RETURN Iterate (i);
    END;
    WHILE (i.next_hash < i.max_hash) AND (i.next_node = NIL) DO
      i.next_node := i.set.hash[i.next_hash];
      INC (i.next_hash);
    END;
    IF (i.next_node # NIL) THEN RETURN Iterate (i); END;
    i.cur := NIL;
    RETURN FALSE;
  END Iterate;

BEGIN
END Revelation.
