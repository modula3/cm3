(* $Id$ *)
(* revelation of BDD.T *)
MODULE BDDImpl EXPORTS BDD, BDDDepends, BDDImpl, BDDSystemState, BDDCleaner;
IMPORT BDDPair;
IMPORT BDDTripleHash;
IMPORT Word;
IMPORT Debug;
IMPORT BDDSet, BDDSetDef, BDDTextTbl, BDDBDDTbl;
(*IMPORT IO;*)

IMPORT Fmt; FROM Fmt IMPORT Int;

TYPE
  Op = { And, Not, Or, MakeTrue, MakeFalse };
  Pair = BDDPair.T;

REVEAL
  T = BRANDED Brand OBJECT
    l , r : T;
    root  : Root;
    tag   : CARDINAL; (* for hashing *)
    name  : TEXT;
  METHODS
    init() : T := Init;
  END;

PROCEDURE Right(a : T) : T = BEGIN RETURN a.r END Right;

PROCEDURE Left(a : T) : T = BEGIN RETURN a.l END Left;

PROCEDURE NodeVar(v : T) : T = 
  VAR b : T; BEGIN
    <*ASSERT v # false*>
    <*ASSERT v # true*>
(*
    IF v.root.tab = NIL THEN
      <*ASSERT v.root = true OR v.root = false*>
      IO.Put(v.name & "\n")
    END;
*)

    IF v.root.tab = NIL THEN InitTab(v.root) END;

    <*ASSERT v.root.tab # NIL*>
    IF BDDTripleHash.Get(v.root.tab, Pair { true, false } , b) THEN
      RETURN b
    ELSE
      (* can this happen?, and is this right?? *)
      RETURN v.root
    END
  END NodeVar;

(* this special object is used as a literal. *)
(* in order to allow garbage collection, the caches and the lookup table *)
(* are stored in this object instead of as static data structures *)
(* N.B. for a multithreaded application, this structure will need to be *)
(* monitored. *)
TYPE
  Root = T OBJECT
    (*mu : MUTEX;*) (* as yet unused *)
    id : CARDINAL;
    tab : BDDTripleHash.T := NIL;
    cache := ARRAY Op OF BDDTripleHash.T { NIL, .. };
  END;

VAR
  mu := NEW(MUTEX);
  
PROCEDURE Init(self : T) : T = 
  BEGIN LOCK mu DO self.tag := nextTag; INC(nextTag) END; RETURN self END Init;

PROCEDURE Order(VAR b1, b2 : T) = 
  BEGIN
    IF b1.root.id > b2.root.id THEN VAR x := b1; BEGIN b1 := b2; b2 := x END END
  END Order;
    
(* hmm *)

PROCEDURE InitTripleHash(VAR cache : BDDTripleHash.T) =
  CONST
    DefSiz = 2;
  BEGIN
    <*ASSERT cache = NIL*>
    cache := NEW(BDDTripleHash.Default).init(DefSiz)
  END InitTripleHash;

PROCEDURE And(b1, b2 : T) : T =
  VAR 
    tripleHash : BDDTripleHash.T;
    l, r : T; 
    b : T;
  BEGIN
    IF b1 = b2 THEN RETURN b1
    ELSIF b1 = false OR b2 = false THEN RETURN false
    ELSIF b2 = true THEN RETURN b1
    ELSIF b1 = true THEN RETURN b2
    END;

    Order(b1,b2);

    WITH op = Op.And DO
      IF b1.root.cache[op] = NIL THEN
        InitTripleHash(b1.root.cache[op])
      END;
      tripleHash := b1.root.cache[op]
    END;

    IF BDDTripleHash.Get(tripleHash, Pair { b1, b2 } , b) THEN
      RETURN b
    END;
    
    IF b1.root.id = b2.root.id THEN
      l := And(b1.l, b2.l);
      r := And(b1.r, b2.r)
    ELSE
      l := And(b1.l, b2);
      r := And(b1.r, b2)
    END;

    IF l = r THEN
      EVAL BDDTripleHash.Put(tripleHash, Pair { b1, b2 }, (l));
      RETURN l
    END;

    IF b1.root.tab = NIL THEN InitTab(b1.root) END;
    IF NOT BDDTripleHash.Get(b1.root.tab, Pair { l, r } , b) THEN
      b := NEW(T).init();
      b.root := b1.root;
      b.root.id := b1.root.id;
      b.l := l;
      b.r := r;
      IF b.root.tab = NIL THEN InitTab(b.root) END;
      EVAL BDDTripleHash.Put(b.root.tab, Pair { l, r }, (b));
    END;

    EVAL BDDTripleHash.Put(tripleHash, Pair { b1, b2 } , (b));
    RETURN b

  END And;

PROCEDURE Or(b1, b2 : T) : T =
  VAR 
    tripleHash : BDDTripleHash.T;
    l, r : T; 
    b : T;
  BEGIN
    IF b1 = b2 THEN RETURN b1
    ELSIF b1 = true OR b2 = true THEN RETURN true
    ELSIF b2 = false THEN RETURN b1
    ELSIF b1 = false THEN RETURN b2
    END;

    Order(b1,b2);

    WITH op = Op.Or DO
      IF b1.root.cache[op] = NIL THEN
        InitTripleHash(b1.root.cache[op])
      END;
      tripleHash := b1.root.cache[op]
    END;

    IF BDDTripleHash.Get(tripleHash, Pair { b1, b2 } , b) THEN
      RETURN b
    END;
    
    IF b1.root.id = b2.root.id THEN
      l := Or(b1.l, b2.l);
      r := Or(b1.r, b2.r)
    ELSE
      l := Or(b1.l, b2);
      r := Or(b1.r, b2)
    END;

    IF l = r THEN
      EVAL BDDTripleHash.Put(tripleHash, Pair { b1, b2 }, (l));
      RETURN l
    END;

    IF b1.root.tab = NIL THEN InitTab(b1.root) END;
    IF NOT BDDTripleHash.Get(b1.root.tab, Pair { l, r } , b) THEN
      b := NEW(T).init();
      b.root := b1.root;
      b.root.id := b1.root.id;
      b.l := l;
      b.r := r;
      IF b.root.tab = NIL THEN InitTab(b.root) END;
      EVAL BDDTripleHash.Put(b.root.tab, Pair { l, r }, (b));
    END;

    EVAL BDDTripleHash.Put(tripleHash, Pair { b1, b2 } , (b));
    RETURN b

  END Or;

PROCEDURE Not(b1 : T) : T = 
  VAR 
    tripleHash : BDDTripleHash.T;
    b, l, r : T;
  BEGIN
    IF    b1 = true THEN RETURN false
    ELSIF b1 = false THEN RETURN true
    END;

    WITH op = Op.Not DO
      IF b1.root.cache[op] = NIL THEN
        InitTripleHash(b1.root.cache[op])
      END;
      tripleHash := b1.root.cache[op]
    END;

    <*ASSERT tripleHash # NIL*>
    IF BDDTripleHash.Get(tripleHash, Pair { b1, true }, b) THEN
      RETURN b
    END;

    l := Not(b1.l);
    r := Not(b1.r);
    
    IF b1.root.tab = NIL THEN InitTab(b1.root) END;
    IF NOT BDDTripleHash.Get(b1.root.tab, Pair { l, r }, b) THEN
      b := NEW(T).init();
      b.root := b1.root;
      b.l := l;
      b.r := r;
      EVAL BDDTripleHash.Put(b1.root.tab, Pair { l, r }, (b))
    END;

    EVAL BDDTripleHash.Put(tripleHash, Pair { b1, true }, (b));
    RETURN b

  END Not;

(**********************************************************************)

PROCEDURE MakeTrue(b, v : T) : T =
  VAR
    tripleHash : BDDTripleHash.T;
    l, r, b1 : T;
  BEGIN
    IF b = true OR b = false THEN
      RETURN b 
    END;
    IF    b.root.id > v.root.id THEN 
      RETURN b 
    ELSIF b.root.id = v.root.id THEN
      RETURN b.l
    END;

    (* { b.root.id < v.root.id } *)
    
    WITH op = Op.MakeTrue DO
      IF b.root.cache[op] = NIL THEN
        InitTripleHash(b.root.cache[op])
      END;
      tripleHash := b.root.cache[op]
    END;

    IF BDDTripleHash.Get(tripleHash, Pair { b, v }, b1) THEN
      RETURN b1
    END;

    IF NOT BDDTripleHash.Get(tripleHash, Pair { b.l, v }, l) THEN
      l := MakeTrue(b.l, v)
    END;

    IF NOT BDDTripleHash.Get(tripleHash, Pair { b.r, v }, r ) THEN
      r := MakeTrue(b.r, v)
    END;

    IF l = r THEN
      EVAL BDDTripleHash.Put(tripleHash, Pair { b, v }, l);
      RETURN l
    END;

    IF b.root.tab = NIL THEN InitTab(b.root) END;
    IF NOT BDDTripleHash.Get(b.root.tab, Pair { l, r }, b1) THEN
      b1 := NEW(T).init();
      b1.root := b.root;
      b1.l := l;
      b1.r := r;
      EVAL BDDTripleHash.Put(b.root.tab, Pair { l, r } , b1)
    END;
    
    EVAL BDDTripleHash.Put(tripleHash, Pair { b, v }, b1);
    RETURN b1

  END MakeTrue;

PROCEDURE MakeFalse(b, v : T) : T =
  VAR
    tripleHash : BDDTripleHash.T;
    l, r, b1 : T;
  BEGIN
    IF b = true OR b = false THEN
      RETURN b 
    END;
    IF    b.root.id > v.root.id THEN 
      RETURN b 
    ELSIF b.root.id = v.root.id THEN
      RETURN b.r
    END;

    (* { b.root.id < v.root.id } *)
    
    WITH op = Op.MakeFalse DO
      IF b.root.cache[op] = NIL THEN
        InitTripleHash(b.root.cache[op])
      END;
      tripleHash := b.root.cache[op]
    END;

    IF BDDTripleHash.Get(tripleHash, Pair { b, v }, b1) THEN
      RETURN b1
    END;

    IF NOT BDDTripleHash.Get(tripleHash, Pair { b.l, v }, l) THEN
      l := MakeFalse(b.l, v)
    END;

    IF NOT BDDTripleHash.Get(tripleHash, Pair { b.r, v }, r ) THEN
      r := MakeFalse(b.r, v)
    END;

    IF l = r THEN
      EVAL BDDTripleHash.Put(tripleHash, Pair { b, v }, l);
      RETURN l
    END;

    IF b.root.tab = NIL THEN InitTab(b.root) END;
    IF NOT BDDTripleHash.Get(b.root.tab, Pair { l, r }, b1) THEN
      b1 := NEW(T).init();
      b1.root := b.root;
      b1.l := l;
      b1.r := r;
      EVAL BDDTripleHash.Put(b.root.tab, Pair { l, r } , b1)
    END;
    
    EVAL BDDTripleHash.Put(tripleHash, Pair { b, v }, b1);
    RETURN b1

  END MakeFalse;

(**********************************************************************)

PROCEDURE True() : T = BEGIN RETURN true END True;

PROCEDURE False() : T = BEGIN RETURN false END False;

PROCEDURE InitTab(r : Root) =
  BEGIN
    r.tab := NEW(BDDTripleHash.Default).init(1);
    <*ASSERT r.l = true*>
    <*ASSERT r.r = false*>
    EVAL BDDTripleHash.Put(r.tab, Pair { true, false }, (r));
  END InitTab;

PROCEDURE New(name : TEXT) : T = 
  VAR res : Root := NEW(Root).init(); BEGIN 
    res.name := name;
    res.root := res;
    res.l := true; res.r := false; 
    <*ASSERT nextId >= 2*>
    res.id := nextId;
(*
    res.tab := NEW(BDDTripleHash.Default).init(1);
    EVAL BDDTripleHash.Put(res.tab, Pair { res.l, res.r }, (res));
*)
(*
    FOR i := FIRST(res.cache) TO LAST(res.cache) DO
      res.cache[i] := NEW(BDDTripleHash.Default).init(64)
    END;
*)
    INC(nextId);
    RETURN res
  END New;

PROCEDURE Format(x : T; symtab : REFANY := NIL; pfx : TEXT := "") : TEXT =
  VAR nm : TEXT;
  BEGIN
    IF symtab # NIL AND NARROW(symtab, BDDTextTbl.T).get(x, nm) THEN
      RETURN pfx & nm
    END;
    TYPECASE x OF
      Root(r) =>
        IF    r.id = 1 THEN RETURN "TRUE"
        ELSIF r.id = 0 THEN RETURN "FALSE"
        END
    ELSE
      (* skip *)
    END;

    IF x.name # NIL THEN RETURN pfx & x.name END;

    RETURN Fmt.Int(x.root.id) & " && (" & Format(x.l) & ") || (" & Format(x.r) &
           ") && ~" & Fmt.Int(x.root.id)
  END Format;
  
<*INLINE*>PROCEDURE Hash(a : T) : Word.T = 
  BEGIN RETURN a.tag END Hash;

VAR
  true, false : Root;
  nextId : CARDINAL;

PROCEDURE Size(b1 : T) : CARDINAL =
  VAR seen := NEW(BDDSetDef.T).init();
      res  := 0;

  PROCEDURE Recurse(b : T) =
    BEGIN
      IF seen.insert(b) THEN RETURN END;

      IF b = true OR b = false OR ISTYPE(b, Root) THEN
        INC(res) (* leaf *)
      ELSE
        INC(res); (* tree node *)
        Recurse(b.root); Recurse(b.r); Recurse(b.l)
      END
    END Recurse;

  BEGIN
    Recurse(b1);
    RETURN res
  END Size;

PROCEDURE Depends(b1 : T) : BDDSet.T =
  VAR seen, res := NEW(BDDSetDef.T).init();
      (* use BDDSetDef instead for seen or maybe for both *)

  PROCEDURE Recurse(b : T) =
    BEGIN
      IF seen.insert(b) THEN RETURN END;

      IF b = true OR b = false THEN 
      ELSIF ISTYPE(b, Root) THEN
        EVAL res.insert(b)
      ELSE
        Recurse(b.root); Recurse(b.r); Recurse(b.l)
      END
    END Recurse;

  BEGIN
    Recurse(b1);
    RETURN res
  END Depends;

PROCEDURE GetId(a : T) : INTEGER = BEGIN RETURN a.root.id END GetId;

REVEAL
  SystemState = BRANDED "BDD SystemState" OBJECT
    true, false : Root;
    nextTag     : CARDINAL;
    nextId      : CARDINAL;
  END;

VAR
  nextTag : CARDINAL;

PROCEDURE SetSystemState(s : SystemState) =
  BEGIN
    true    := s.true;   <*ASSERT true.id = 1*>
    false   := s.false;  <*ASSERT false.id = 0*>
    nextTag := s.nextTag;

    <*ASSERT s.nextId >= 2*>
    nextId  := s.nextId;
  END SetSystemState;

PROCEDURE GetSystemState() : SystemState =
  BEGIN
    RETURN NEW(SystemState, 
               true    := true, 
               false   := false, 
               nextTag := nextTag,
               nextId  := nextId)
  END GetSystemState;

PROCEDURE NewDefaultSystemState() : SystemState =
  VAR 
    t, f : Root;
  BEGIN
    t := NEW(Root).init();
    t.root := t;
    t.id := 1;
    t.r := t;
    t.l := t;
    t.name := "TRUE";
    
    f := NEW(Root).init();
    f.root := f;
    f.r := f;
    f.l := f;
    f.id := 0;
    f.name := "FALSE";

    WITH sys = NEW(SystemState, 
                   true    := t, 
                   false   := f, 
                   nextTag := 0, 
                   nextId  := 2) 
     DO
      RETURN sys
    END
  END NewDefaultSystemState;

REVEAL
  (* this is really a major hack

     point is to save memory on small, independent BDDs and to be able
     to still use those in pickles *)
  Cleaner = PublicCleaner BRANDED "BDD Cleaner" OBJECT
    s   : SystemState;
    map : BDDBDDTbl.T;
  OVERRIDES
    init  := InitC;
    state := StateC;
    clean := CleanC;
  END;

PROCEDURE InitC(c : Cleaner) : Cleaner =
  BEGIN
    c.s := NewDefaultSystemState();
    c.map := NEW(BDDBDDTbl.Default).init();
    EVAL c.map.put(false, c.s.false);
    EVAL c.map.put(true,  c.s.true);
    RETURN c
  END InitC;

PROCEDURE StateC(c : Cleaner) : SystemState =
  BEGIN RETURN c.s END StateC;

PROCEDURE CleanC(c : Cleaner; b : T) : T =
  VAR
    z : T;
    saveState := GetSystemState();
  BEGIN
    IF NOT c.map.get(b, z) THEN
      IF ISTYPE(b, Root) THEN
        <*ASSERT c.s # saveState *>

        <*ASSERT NARROW(b,Root).id >= 2*>

        SetSystemState(c.s);
        z := New(NARROW(b, Root).name);
        
        Debug.Out("made new z " & Format(z));
        Debug.Out("z.id = " & Int(NARROW(z,Root).id));
        <*ASSERT NARROW(z,Root).id >= 2*>
      ELSE
        WITH oo        = c.clean(b.root),
             ll        = c.clean(b.l),
             rr        = c.clean(b.r) DO
          <*ASSERT c.s # saveState *>
          SetSystemState(c.s);
          z := Or(And(oo,ll),
                  And(rr,Not(oo)))
        END
      END;

      c.s := GetSystemState();
      <*ASSERT c.s # saveState *>
      SetSystemState(saveState);

      EVAL c.map.put(b, z)
    END;

    Debug.Out("---");
    Debug.Out("b pre-clean: " & Format(b)) ;
    IF b.root.tab # NIL THEN
      Debug.Out("b.root.tab.size: " & Int(b.root.tab.size()));
    END;
    Debug.Out("z post-clean: " & Format(z));
    IF z.root.tab # NIL THEN
      Debug.Out("z.root.tab.size: " & Int(z.root.tab.size()));
    END;

    RETURN z
  END CleanC;

BEGIN 
  SetSystemState(NewDefaultSystemState())
END BDDImpl.









