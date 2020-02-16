(* $Id$ *)

MODULE Bool EXPORTS Bool, BoolImpl, BoolRemapImpl;
IMPORT Cbool, CboolImpl;
IMPORT CPtr;
IMPORT Word;
IMPORT Debug;
IMPORT BoolSet, BoolSetDef;
IMPORT BoolTextTbl, BoolBoolTbl;
IMPORT Fmt;

VAR B : Cbool.T := NIL;

CONST
  gcRate = 100;

(* locking order:
   FIRST mu, THEN CPtr.mu *)

VAR
  mu := NEW(MUTEX); (* to protect Rajits code *)

PROCEDURE Init() =
  BEGIN LOCK mu DO
    IF B # NIL THEN 
      Debug.Error("B already initialized") 
    ELSE 
      B := Cbool.init() 
    END
  END END Init;

(* CPtr.Wrap may lock CPtr.mu *)
PROCEDURE Wrap(cbool : Cbool.t) : T =
  VAR
    hadIt : BOOLEAN;
    res := CPtr.Wrap(cbool, Destroy, mu, hadIt);
  BEGIN 
    (* this is gross.  we want to maintain the invariant that we *)
    (* have exactly ONE reference to a Cbool.t from the library  *)

    (* I wonder if this is really right.... *)
    (* commenting this out could lead to a memory leak.. sigh *)
    IF hadIt THEN Cbool.free(B,cbool) END;
    <* ASSERT res # NIL *>
    <* ASSERT Cbool.refs(B,cbool) >= 1 *>
    RETURN res 
  END Wrap;

PROCEDURE Destroy(cbool : Cbool.t) =
  (* this routine is entered with mu AND CPtr.mu LOCKed (in that order) *)
  BEGIN 
    frees := frees + 1;
    Cbool.free(B,cbool);
    IF frees MOD gcRate = 0 THEN Cbool.gc(B) END
  END Destroy;

(* for all these functions, we LOCK mu to protect against messing around *)
(* at the same time as the garbage collector.  The Destroy routine above *)
(* is protected by a LOCK mu LOCK CPtr.mu inside the WeakRef cleaner routine *)
(* in CPtr.  We have to do it this way to ensure that the LOCKs are always *)
(* acquired in the same order! *)

(* in other words: the Bool data structure is a monitor *)

PROCEDURE New() : T = 
  BEGIN LOCK mu DO RETURN Wrap(Cbool.newvar(B)) END END New;

PROCEDURE And(a , b : T) : T = 
  BEGIN LOCK mu DO RETURN Wrap(Cbool.and(B, a.get(), b.get())) END END And;

PROCEDURE Or(a , b : T) : T = 
  BEGIN LOCK mu DO RETURN Wrap(Cbool.or(B, a.get(), b.get())) END END Or;

PROCEDURE Xor(a , b : T) : T = 
  BEGIN LOCK mu DO RETURN Wrap(Cbool.xor(B, a.get(), b.get())) END END Xor;

PROCEDURE Equivalent(a, b : T) : T = BEGIN RETURN Not(Xor(a,b)) END Equivalent;

PROCEDURE Not(a : T) : T = 
  BEGIN LOCK mu DO RETURN Wrap(Cbool.not(B, a.get())) END END Not;

PROCEDURE Copy(a : T) : T = 
  BEGIN LOCK mu DO RETURN Wrap(Cbool.copy(B, a.get())) END END Copy;

PROCEDURE Implies(a , b : T) : T = 
  BEGIN LOCK mu DO RETURN Wrap(Cbool.implies(B, a.get(), b.get())) END END Implies;

PROCEDURE MakeTrue(a , b : T) : T = 
  BEGIN LOCK mu DO RETURN Wrap(Cbool.maketrue(B, a.get(), b.get())) END END MakeTrue;

PROCEDURE MakeFalse(a , b : T) : T = 
  BEGIN LOCK mu DO RETURN Wrap(Cbool.makefalse(B, a.get(), b.get())) END END MakeFalse;

PROCEDURE Print(a : T) = BEGIN Cbool.print(a.get()) END Print;

PROCEDURE True() : T =
  BEGIN LOCK mu DO RETURN Wrap(Cbool.true(B)) END END True;

PROCEDURE False() : T =
  BEGIN LOCK mu DO RETURN Wrap(Cbool.false(B)) END END False;

(* here we need to increase the ref count first so that we may wrap later *)
PROCEDURE FromC(cbool : Cbool.t) : T =
  BEGIN LOCK mu DO RETURN Wrap(CboolImpl.copy(B,cbool)) END END FromC;

PROCEDURE GetId(a : T) : INTEGER = BEGIN RETURN Cbool.getid(a.get()) END GetId;

PROCEDURE GC() = BEGIN LOCK mu DO Cbool.gc(B) END END GC;

PROCEDURE Equal(a, b : T) : BOOLEAN = BEGIN RETURN a = b END Equal;

PROCEDURE Choose(c : T; it, if : T) : T =
  BEGIN RETURN Or(And(c,it), And(Not(c),if)) END Choose;

(* this is really no good! --- we can probably hash on the pointer *)
PROCEDURE Hash(<*UNUSED*>a : T) : Word.T = BEGIN RETURN 1 END Hash;

(* Tricky things that are exported by BoolImpl follow. *)
(* These functions allow access to Rajit's internal data structures *)

(* XXX NOTE CLEARLY: Wrap() should only be used to wrap routines that *)
(* INCREASE the ref count in Rajit's code. DO NOT CALL OTHERWISE.     *)
PROCEDURE IsLeaf( a : T ) : BOOLEAN =
  BEGIN LOCK mu DO RETURN CboolImpl.isleaf(a.get()) # 0 END END IsLeaf;

PROCEDURE Right(a : T) : T =
  BEGIN LOCK mu DO RETURN Wrap(CboolImpl.get_r(a.get())) END END Right;

PROCEDURE Left(a : T) : T =
  BEGIN LOCK mu DO RETURN Wrap(CboolImpl.get_l(a.get())) END END Left;

PROCEDURE NodeVar(a : T) : T =
  BEGIN LOCK mu DO RETURN Wrap(CboolImpl.node_var(B,a.get())) END END NodeVar;

PROCEDURE Vars(a : T) : BoolSet.T =

  PROCEDURE VarsRecurse(a : T) =
    VAR
      r, l : T; 
    BEGIN
      IF a = True() OR a = False() THEN RETURN END;
      r := Right(a);
      l := Left(a);
      EVAL set.insert(NodeVar(a));
      IF r # NIL THEN VarsRecurse(r) END;
      IF l # NIL THEN VarsRecurse(l) END
    END VarsRecurse;

  VAR
    set := NEW(BoolSetDef.T).init();
  BEGIN
    VarsRecurse(a);
    RETURN set
  END Vars;

PROCEDURE Remap(map : BoolBoolTbl.T; e : T; check : BOOLEAN) : T =
  
  PROCEDURE Recurse(e : T) : T =
    VAR
      o : T;
      n : T;
      gotIt : BOOLEAN;
    BEGIN
      IF    e = False() THEN 
        RETURN False()
      ELSIF e = True() THEN
        RETURN True()
      END;

      o := NodeVar(e);
      gotIt := map.get(o,n);

      (* rename current *)
      IF NOT gotIt  AND check AND NOT o = False() AND NOT o = True() THEN
        Debug.Error("No mapping for Bool.")
      END;
      
      IF NOT gotIt THEN n := o END;

      (* prepare to recurse *)
      VAR
        r := Right(e);
        l := Left(e);
        nr, nl : T;
      BEGIN
        IF r = NIL THEN nr := False() ELSE nr := Recurse(r) END;
        IF l = NIL THEN nl := True()  ELSE nl := Recurse(l) END;

        RETURN Or(And(n,nl),And(Not(n),nr))
      END
    END Recurse;

  BEGIN RETURN Recurse(e) END Remap;

PROCEDURE Format(b : T; symTab : BoolTextTbl.T; pfx : TEXT) : TEXT =
  VAR 
    name : TEXT; 
  BEGIN 
    IF symTab # NIL AND symTab.get(b,name) THEN 
      RETURN pfx & name (* & "(??" & Fmt.Int(GetId(b)) & ")" *)
    ELSE 
      RETURN pfx & "??(??" & Fmt.Int(GetId(b)) &")"
    END
  END Format; 

PROCEDURE Difference(f, x : T) : T =
  BEGIN RETURN Xor(MakeTrue(f,x),MakeFalse(f,x)) END Difference;

PROCEDURE Smooth(f, x : T) : T =
  BEGIN RETURN Or(MakeTrue(f,x),MakeFalse(f,x)) END Smooth;

BEGIN 
  Init();
  frees := 0 
END Bool.
