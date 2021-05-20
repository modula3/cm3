(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: ESet.m3                                               *)
(* Last modified on Wed Feb  1 14:03:26 PST 1995 by kalsow     *)

MODULE ESet;

IMPORT M3, M3ID, CG, Value, Token, Scope, Scanner, M3Buf;
IMPORT Error, Word, Exceptionz, Target, Module;
FROM Scanner IMPORT Match, MatchID, GetToken, cur;
FROM M3CG IMPORT QID;

TYPE TList = M3.ExSetList;
REVEAL
  M3.ExSetList = BRANDED "ESet.List" REF RECORD
    set : T     := NIL;
    next: TList := NIL;
  END;

REVEAL
  M3.ExSet = M3.Node BRANDED "ESet.T" OBJECT
    elts     : Elt     := NIL;
    env      : Scope.T := NIL;
    any      : BOOLEAN := FALSE;
    resolved : BOOLEAN := FALSE;
    used_any : BOOLEAN := FALSE;
    hash     : INTEGER := 0;
    age      : INTEGER := 0;
    offset   : INTEGER := 0;
  END;

TYPE
  Elt = BRANDED "ESet.Elt" REF RECORD
    origin : INTEGER;
    name   : QID;
    except : Value.T;
    next   : Elt;
    used   : BOOLEAN;
  END;

VAR AnySet     := NEW (T, any := TRUE,  resolved := TRUE);
VAR NoneSet    := NEW (T, any := FALSE, resolved := TRUE);
VAR DefaultSet := NoneSet;

TYPE HashTable = REF ARRAY OF T;

VAR thisAge  : INTEGER   := 1;
VAR hashTbl  : HashTable := NIL;
VAR n_hashed : INTEGER   := 0;

PROCEDURE Reset () =
  BEGIN
    INC (thisAge);
    hashTbl := NIL;
  END Reset;

PROCEDURE ParseRaises (): T =
  TYPE  TK = Token.T;
  VAR t: T;  elt: Elt;  here := Scanner.offset;
  BEGIN
    Match (TK.tRAISES);
    IF cur.token = TK.tANY THEN
      GetToken (); (* ANY *)
      t := AnySet;
    ELSE
      elt := NIL;
      Match (TK.tLBRACE);
      WHILE (cur.token = TK.tIDENT) DO
        elt := NEW (Elt, next := elt);
        elt.origin      := Scanner.offset;
        elt.name.module := M3ID.NoID;
        elt.name.item   := MatchID ();
        elt.except      := NIL;
        IF (cur.token = TK.tDOT) THEN
          GetToken (); (* . *)
          elt.name.module := elt.name.item;
          elt.name.item   := MatchID ();
        END;
        IF (cur.token # TK.tCOMMA) THEN EXIT END;
        GetToken (); (* , *)
      END;
      Match (TK.tRBRACE);
      IF (elt = NIL)
        THEN t := NoneSet;
        ELSE t := NEW (T, origin := here, env := Scope.Top (), elts := elt);
      END;
    END;
    RETURN t;
  END ParseRaises;

PROCEDURE ParseFails  (existing: T): T =
  TYPE  TK = Token.T;
  VAR t: T := existing;  elt: Elt;
  BEGIN
    IF (t = NIL) THEN  t := NEW (T);  t.origin := Scanner.offset;  END;
    Match (TK.tFATAL);
    LOOP
      IF (cur.token = TK.tANY) THEN
        GetToken (); (* ANY *)
        t.any := TRUE;
      ELSIF (cur.token = TK.tIDENT) THEN
        IF (t.env = NIL) THEN t.env := Scope.Top () END;
        elt := NEW (Elt, next := t.elts);  t.elts := elt;
        elt.origin      := Scanner.offset;
        elt.name.module := M3ID.NoID;
        elt.name.item   := MatchID ();
        elt.except      := NIL;
        IF (cur.token = TK.tDOT) THEN
          GetToken (); (* . *)
          elt.name.module := elt.name.item;
          elt.name.item   := MatchID ();
        END;
      ELSE
        EXIT;
      END;
      IF (cur.token # TK.tCOMMA) THEN EXIT END;
      GetToken (); (* , *)
    END;
    Match (TK.tENDPRAGMA);
    RETURN t;
  END ParseFails;

PROCEDURE Hash (t: T): INTEGER =
  VAR hash := 691;  e: Elt;  o: Value.T;  oname: M3ID.T;
  BEGIN
    IF (t # NIL) THEN
      IF (t.hash = 0) THEN
        <*ASSERT t.resolved*>
        e := t.elts;
        WHILE (e # NIL) DO
          o := e.except;
          IF (o # NIL) THEN
            oname := Value.CName (o);
            hash := Word.Plus (Word.Times (hash, 89), M3ID.Hash (oname));
          END;
          e := e.next;
        END;
        t.hash := hash;
      END;
      hash := t.hash;
    END;
    RETURN hash;
  END Hash;

PROCEDURE LookUp (t: T): T =
  VAR hx := Hash (t);   i: INTEGER;   x: T;
  BEGIN
    <*ASSERT t.resolved*>
    IF (hashTbl = NIL) THEN hashTbl := NEW (HashTable, 100) END;
    i := hx MOD NUMBER (hashTbl^);
    LOOP
      x := hashTbl[i];
      IF (x = NIL) THEN (* a new entry! *)
        hashTbl[i] := t;
        INC (n_hashed);
        IF (2 * n_hashed > NUMBER (hashTbl^)) THEN ExpandHashTable () END;
        RETURN t;
      END;
      IF (x = t) OR IsEqual (x, t) THEN RETURN x END;
      INC (i);
      IF (i > LAST (hashTbl^)) THEN i := 0 END;
    END;
  END LookUp;

PROCEDURE ExpandHashTable () =
  VAR old := hashTbl;  t: T;
  BEGIN
    hashTbl  := NEW (HashTable, 2 * NUMBER (old^));
    n_hashed := 0;
    FOR i := FIRST (old^) TO LAST (old^) DO
      t := old[i];
      IF (t # NIL) THEN EVAL LookUp (t) END;
    END;
  END ExpandHashTable;

PROCEDURE Declare (t: T) =
  VAR e: Elt;  n: INTEGER;
  BEGIN
    IF (t = NIL) OR (t.any) OR (t.elts = NIL) THEN RETURN END;
    t := LookUp (t);
    IF (t.age >= thisAge) THEN RETURN END;

    (* declare each of the exceptions and size the list *)
    e := t.elts; n := 1;
    WHILE (e # NIL) DO
      IF (e.except # NIL) THEN
        Value.Declare (e.except);
        INC (n);
      END;
      e := e.next;
    END;

    (* allocate the space *)
    t.offset := Module.Allocate (n * Target.Integer.pack, Target.Integer.align,
                                 TRUE, "*exception list*");

    (* initialize the list *)
    e := t.elts;  n := t.offset;
    WHILE (e # NIL) DO
      IF (e.except # NIL) THEN
        CG.Init_intt (n, Target.Integer.size, Exceptionz.UID (e.except), TRUE);
        INC (n, Target.Integer.pack);
      END;
      e := e.next;
    END;

    t.age := thisAge;
  END Declare;

PROCEDURE GetAddress (t: T;  VAR base: CG.Var;  VAR offset: INTEGER) =
  BEGIN
    IF (t = NIL) THEN
      base := NIL;
      offset := 0;
    ELSIF (t.any) THEN
      Error.Msg ("INTERNAL ERROR: need address of RAISES ANY list");
      base := NIL;
      offset := 0;
    ELSIF (t.elts = NIL) THEN
      base := NIL;
      offset := 0;
    ELSE
      t := LookUp (t);
      Declare (t);
      base := Module.GlobalData (is_const := TRUE);
      offset := t.offset;
    END;
  END GetAddress;

PROCEDURE RaisesAny (t: T): BOOLEAN =
  BEGIN
    RETURN (t # NIL) AND (t.any);
  END RaisesAny;

PROCEDURE RaisesNone (t: T): BOOLEAN =
  BEGIN
    RETURN (t = NIL) OR ((NOT t.any) AND (t.elts = NIL));
  END RaisesNone;

PROCEDURE NewAny (): T =
  BEGIN
    RETURN NEW (T, any := TRUE, origin := Scanner.offset);
  END NewAny;

PROCEDURE NewEmpty (env: Scope.T): T =
  BEGIN
    RETURN NEW (T, env := env, origin := Scanner.offset);
  END NewEmpty;

PROCEDURE Add (t: T;  READONLY name: QID;   ex: Value.T) =
  VAR e: Elt;
  BEGIN
    ex := Value.Base (ex);

    (* check for a duplicate *)
    e := t.elts;
    WHILE (e # NIL) DO
      IF (e.except = ex) AND (ex # NIL) THEN
        Error.QID (name, "repeated exception in handler list");
        RETURN;
      END;
      e := e.next;
    END;

    t.elts := NEW (Elt, origin := Scanner.offset, next := t.elts,
                   name := name, except := ex);
  END Add;

PROCEDURE IsEqual (a, b: T): BOOLEAN =
  BEGIN
    IF (a = NIL) THEN a := DefaultSet END;
    IF (b = NIL) THEN b := DefaultSet END;
    IF (a.any # b.any) THEN RETURN FALSE END;
    Resolve (a);
    Resolve (b);
    RETURN EltSubset (a.elts, b.elts) AND EltSubset (b.elts, a.elts);
  END IsEqual;

PROCEDURE IsSubset (a, b: T): BOOLEAN =
(* TRUE iff a is a subset of b *)
  BEGIN
    IF (a = NIL) THEN a := DefaultSet END;
    IF (b = NIL) THEN b := DefaultSet END;
    IF (b.any) THEN RETURN TRUE END;
    IF (a.any) THEN RETURN FALSE END;
    Resolve (a);
    Resolve (b);
    RETURN EltSubset (a.elts, b.elts);
  END IsSubset;

PROCEDURE TypeCheck (t: T) =
  BEGIN
    IF (t # NIL) THEN Resolve (t) END;
  END TypeCheck;

PROCEDURE Push (VAR cs: M3.CheckState;  ok_to_raise, no_error: T;
                                                               stop: BOOLEAN) =
  VAR x: TList;
  BEGIN
    IF (stop) THEN
      (* this is a nested procedure => truncate the "ok_to_raise" list *)
      cs.ok_to_raise := NEW (TList, set := NIL, next := cs.ok_to_raise);
    END;

    IF (ok_to_raise # NIL) THEN
      ResetUsed (ok_to_raise);
      x := NEW (TList, set := ok_to_raise);
      x.next := cs.ok_to_raise;
      cs.ok_to_raise := x;
    END;
    IF (no_error # NIL) THEN
      ResetUsed (no_error);
      x := NEW (TList, set := no_error);
      x.next := cs.no_error;
      cs.no_error := x;
    END;
  END Push;

PROCEDURE ResetUsed (t: T) =
  VAR e := t.elts;
  BEGIN
    t.used_any := FALSE;
    WHILE (e # NIL) DO e.used := FALSE;  e := e.next; END;
  END ResetUsed;

PROCEDURE Pop (VAR cs: M3.CheckState;  ok_to_raise, no_error: T;
                                                               stop: BOOLEAN) =
  VAR u: TList;
  BEGIN
    IF (ok_to_raise # NIL) THEN
      <*ASSERT ok_to_raise = cs.ok_to_raise.set *>
      CheckUnused (ok_to_raise);
      cs.ok_to_raise := cs.ok_to_raise.next;
    END;
    IF (no_error # NIL) THEN
      <*ASSERT no_error = cs.no_error.set *>
      CheckUnused (no_error);
      cs.no_error := cs.no_error.next;
    END;
    IF (stop) THEN
      u := cs.ok_to_raise;
      cs.ok_to_raise := u.next;
      <* ASSERT u.set = NIL *>
    END;
  END Pop;

PROCEDURE CheckUnused (t: T) =
  VAR save := Scanner.offset;  e := t.elts;
  BEGIN
    WHILE (e # NIL) DO
      IF (e.except # NIL) AND (NOT e.used)
        AND (NOT Exceptionz.IsImplicit (e.except)) THEN
        Scanner.offset := e.origin;
        Error.Warn (1, "exception is never raised: "
          & Value.GlobalName (e.except));
      END;
      e := e.next;
    END;
    IF (t.any) AND (NOT t.used_any) THEN
      IF (t.origin = 0)
        THEN Scanner.offset := save;
        ELSE Scanner.offset := t.origin;
      END;
      Error.Warn (1, "exception is never raised: <ANY>");
    END;
    Scanner.offset := save;
  END CheckUnused;

PROCEDURE MarkEverythingUsed (u: TList;  handler: BOOLEAN) =
  VAR t: T;  e: Elt;
  BEGIN
    WHILE (u # NIL) DO
      t := u.set;
      IF (t = NIL) THEN EXIT END;
      IF (t.any) THEN
        t.used_any := TRUE;
        IF (handler) THEN EXIT END;
      END;
      e := t.elts;
      WHILE (e # NIL) DO
        e.used := TRUE;
        e := e.next;
      END;
      u := u.next;
    END;
  END MarkEverythingUsed;

PROCEDURE NoteExceptions (VAR cs: M3.CheckState;  t: T) =
  VAR u: TList;  e: Elt;  n_bad: INTEGER;  name, bad: TEXT;
  BEGIN
    IF (t = NIL) THEN RETURN END;

    IF (t.any) THEN
      MarkEverythingUsed (cs.ok_to_raise, TRUE);
      MarkEverythingUsed (cs.no_error, FALSE);
      u := cs.ok_to_raise;
      WHILE (u # NIL) AND (u.set # NIL) DO
        IF (u.set.any) THEN u.set.used_any := TRUE;  RETURN END;
        u := u.next;
      END;
      cs.raises_others := TRUE;
      u := cs.no_error;
      WHILE (u # NIL) DO
        IF (u.set.any) THEN u.set.used_any := TRUE;  RETURN END;
        u := u.next;
      END;
      Error.Warn (1, "potentially unhandled exception: <ANY>");
      RETURN;
    END;

    e := t.elts;  n_bad := 0;
    WHILE (e # NIL) DO
      name := CheckRaise (cs, e.except);
      IF (name # NIL) THEN
        IF (n_bad = 0)
          THEN bad := name;
          ELSE bad := bad & ", " & name;
        END;
        INC (n_bad);
      END;
      e := e.next;
    END;

    IF (n_bad > 0) THEN
      IF (n_bad = 1)
        THEN Error.Warn (1, "potentially unhandled exception: " & bad);
        ELSE Error.Warn (1, "potentially unhandled exceptions: " & bad );
      END;
    END;

  END NoteExceptions;

PROCEDURE NoteException (VAR cs: M3.CheckState;  v: Value.T) =
  VAR name := CheckRaise (cs, v);
  BEGIN
    IF (name # NIL) THEN
      Error.Warn (1, "potentially unhandled exception: " & name);
    END;
  END NoteException;

PROCEDURE CheckRaise (VAR cs: M3.CheckState;  v: Value.T): TEXT =
(* Returns NIL if it's ok to raise the exception 'v', otherwise
   returns the fully qualified name of the exception. *)
  BEGIN
    IF (v = NIL) THEN (* there's already been an error *) RETURN NIL END;
    v := Value.Base (v);
    IF CheckTList (cs.ok_to_raise, v) THEN RETURN NIL END;
    cs.raises_others := TRUE;
    IF CheckTList (cs.no_error, v) THEN RETURN NIL END;
    IF Exceptionz.IsImplicit (v) THEN RETURN NIL END;
    RETURN Value.GlobalName (v);
  END CheckRaise;

PROCEDURE CheckTList (u: TList;  v: Value.T): BOOLEAN =
(* Return TRUE if exception 'v' occurs in the list of sets 'u'. *)
  VAR e: Elt;  t: T;
  BEGIN
    WHILE (u # NIL) AND (u.set # NIL) DO
      t := u.set;
      e := t.elts;
      WHILE (e # NIL) DO
        IF ExceptionEQ (e.except, v) THEN e.used := TRUE; RETURN TRUE END;
        e := e.next;
      END;
      IF (t.any) THEN t.used_any := TRUE; RETURN TRUE END;
      u := u.next;
    END;
    RETURN FALSE;
  END CheckTList;

(*------------------------------------------------------------- debugging ---*)

PROCEDURE EmitTypes (t: T): INTEGER =
  VAR e: Elt;  n: INTEGER;
  BEGIN
    IF (t = NIL) THEN RETURN 0 END;
    t := LookUp (t);
    IF (t.any) THEN RETURN -1 END;
    
    e := t.elts;  n := 0;
    WHILE (e # NIL) DO
      IF (e.except # NIL) THEN
        Value.Declare (e.except);
        INC (n);
      END;
      e := e.next;
    END;

    RETURN n;
  END EmitTypes;

PROCEDURE EmitNames (t: T) =
  VAR e: Elt;
  BEGIN
    IF (t = NIL) THEN RETURN END;
    t := LookUp (t);
    IF (t.any) THEN RETURN END;
    e := t.elts;
    WHILE (e # NIL) DO
      IF (e.except # NIL) THEN
        CG.Declare_raises (M3ID.Add (Value.GlobalName (e.except)));
      END;
      e := e.next;
    END;
  END EmitNames;

(*---------------------------------------------------------- fingerprints ---*)

PROCEDURE AddFPTag (t: T;  VAR x: M3.FPInfo): CARDINAL =
  VAR e: Elt;  n: INTEGER;
  BEGIN
    IF (t = NIL) THEN RETURN 0 END;
    t := LookUp (t);
    IF (t.any) THEN
      M3Buf.PutText (x.buf, "RAISES-ANY");
      RETURN 0;
    ELSIF (t.elts = NIL) THEN
      RETURN 0;
    ELSE
      <*ASSERT t.resolved*>
      M3Buf.PutText (x.buf, "RAISES{");
      e := t.elts;  n := 0;
      WHILE (e # NIL) DO
        INC (n, Exceptionz.AddFPSetTag (e.except, x));
        e := e.next;
      END;
      M3Buf.PutChar (x.buf, '}');
      RETURN n;
    END;
  END AddFPTag;

PROCEDURE AddFPEdges (t: T;  VAR x: M3.FPInfo;  n: CARDINAL): CARDINAL =
  VAR e: Elt;
  BEGIN
    IF (t = NIL) THEN t := DefaultSet END;
    t := LookUp (t);
    IF (NOT t.any) THEN
      e := t.elts;
      WHILE (e # NIL) DO
        n := Value.AddFPEdges (e.except, x, n);
        e := e.next;
      END;
    END;
    RETURN n;
  END AddFPEdges;

(*-------------------------------------------------------------- internal ---*)

PROCEDURE Resolve (t: T) =
  (* look up each of the named exceptions *)
  VAR e: Elt;  o: Value.T;  save: INTEGER;
  BEGIN
    IF (t.resolved) THEN RETURN END;
    save := Scanner.offset;
    Scanner.offset := t.origin;
    e := t.elts;
    WHILE (e # NIL) DO
      IF (e.except = NIL)
        THEN o := Scope.LookUpQID (t.env, e.name);
        ELSE o := e.except;
      END;
      IF (o = NIL) THEN
        Error.QID (e.name, "undefined");
      ELSIF (Value.ClassOf (o) # Value.Class.Exception) THEN
        Error.QID (e.name, "not an exception");
      ELSE
        e.except := Value.Base (o);
      END;
      e := e.next;
    END;
    Scanner.offset := save;
    t.elts := SortElts (t.elts);
    t.resolved := TRUE;
  END Resolve;

(************* assume the lists are sorted ***************************)
PROCEDURE EltSubset (a, b: Elt): BOOLEAN =
  BEGIN
    WHILE (a # NIL) DO
      LOOP
        IF (b = NIL) THEN RETURN FALSE END;
        IF ExceptionEQ (b.except, a.except) THEN EXIT END;
        b := b.next;
      END;
      (* this element of a is in b *)
      a := a.next;
    END;
    RETURN TRUE;
  END EltSubset;

(***** assumes that the lists are not sorted *************************
PROCEDURE EltSubset (a, b: Elt): BOOLEAN =
  VAR ar, br: Elt;
  BEGIN
    ar := a;
    WHILE (ar # NIL) DO
      br := b;
      LOOP
        IF (br = NIL) THEN RETURN FALSE END;
        IF ExceptionEQ (br.except, ar.except) THEN EXIT END;
        br := br.next;
      END;
      (* this element of a is in b *)
      ar := ar.next;
    END;
    RETURN TRUE;
  END EltSubset;
*********************************************************************)

PROCEDURE SortElts (e: Elt): Elt =
  VAR i: INTEGER;  x: Elt;  tmp: ARRAY [0..9] OF Elt;
  BEGIN
    IF (e = NIL) THEN RETURN NIL END;
    IF (e.next = NIL) THEN RETURN e END;

    (* unpack the list *)
    i := 0;  x := e;
    WHILE (x # NIL) DO
      IF (i <= LAST (tmp)) THEN tmp[i] := x END;
      x := x.next;
      INC (i);
    END;

    IF (i <= NUMBER (tmp)) THEN
      RETURN DoSort (tmp, i);
    ELSE
      WITH ref = NEW (REF ARRAY OF Elt, i) DO
        i := 0;  x := e;
        WHILE (x # NIL) DO  ref[i] := x;  x := x.next; INC (i)  END;
        RETURN DoSort (ref^, i);
      END;
    END;
  END SortElts;

PROCEDURE DoSort (VAR e: ARRAY OF Elt;  n: INTEGER): Elt =
  VAR x: Elt;  j: INTEGER;
  BEGIN
    (* insertion sort the list *)
    FOR i := 1 TO n-1 DO
      x := e[i];
      j := i-1;
      WHILE (j >= 0) AND EltLT (x, e[j]) DO  e[j+1] := e[j]; DEC (j)  END;
      e[j+1] := x;
    END;

    (* build the new linked list *)
    FOR i := 0 TO n-2 DO  e[i].next := e[i+1]  END;
    e[n-1].next := NIL;

    RETURN e[0];
  END DoSort;

PROCEDURE EltLT (a, b: Elt): BOOLEAN =
  VAR aName, bName: Scope.IDStack;
  BEGIN
    IF (a = b)               THEN RETURN FALSE END;
    IF (a = NIL)             THEN RETURN TRUE  END;
    IF (b = NIL)             THEN RETURN FALSE END;
    IF (a.except = b.except) THEN RETURN FALSE END;
    IF (a.except = NIL)      THEN RETURN TRUE  END;
    IF (b.except = NIL)      THEN RETURN FALSE END;
    aName.top := 0;  bName.top := 0;
    Scope.NameToPrefix (a.except, aName);
    Scope.NameToPrefix (b.except, bName);
    FOR i := 0 TO MIN (aName.top, bName.top) - 1 DO
      WITH ax = aName.stk[i], bx = bName.stk[i] DO
        IF (ax # bx) THEN RETURN M3ID.IsLT (ax, bx) END;
      END;
    END;
    RETURN (aName.top < bName.top);
  END EltLT;

PROCEDURE ExceptionEQ (a, b: Value.T): BOOLEAN =
  VAR aName, bName: Scope.IDStack;
  BEGIN
    IF (a = b)                THEN RETURN TRUE  END;
    IF (a = NIL) OR (b = NIL) THEN RETURN FALSE END;

    aName.top := 0;  bName.top := 0;
    Scope.NameToPrefix (a, aName);
    Scope.NameToPrefix (b, bName);
    IF (aName.top # bName.top) THEN RETURN FALSE END;
    FOR i := 0 TO aName.top - 1 DO
      IF (aName.stk[i] # bName.stk[i]) THEN RETURN FALSE END;
    END;

    RETURN TRUE;
  END ExceptionEQ;

BEGIN
END ESet.
