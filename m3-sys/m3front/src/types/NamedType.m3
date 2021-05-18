(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: NamedType.m3                                          *)
(* Last modified on Tue Jul 19 10:05:49 PDT 1994 by kalsow     *)
(*      modified on Fri Dec 21 01:25:25 1990 by muller         *)

MODULE NamedType;

IMPORT M3, M3ID, Token, Type, TypeRep, Scanner, ObjectType;
IMPORT Error, Scope, Brand, Value, ErrType;

TYPE
  P = Type.T BRANDED "NamedType.T" OBJECT
        scope      : Scope.T := NIL;
        module     := M3ID.NoID; (* QID along with type.info.name *)
        type       : Type.T := NIL;
        obj        : Value.T := NIL;
      OVERRIDES
        check      := Check;
        no_straddle:= NoStraddle;
        isEqual    := TypeRep.NeverEqual;
        isSubtype  := TypeRep.NoSubtypes;
        compile    := Compiler;
        initCost   := InitCoster;
        initValue  := GenInit;
        mapper     := GenMap;
        gen_desc   := GenDesc;
        fprint     := FPrinter;
      END;

PROCEDURE Parse (): Type.T =
  TYPE TK = Token.T;
  VAR p: P;  t: Type.T;
  BEGIN
    IF (Scanner.cur.token = TK.tIDENT)
      AND (Scanner.cur.defn # NIL)
      AND (Value.ClassOf (Scanner.cur.defn) = Value.Class.Type) THEN
      (* this identifier is reserved! *)
      t := Value.ToType (Scanner.cur.defn);
      Scanner.GetToken (); (* IDENT *)
    ELSE
      (* this is a non-reserved ID *)
      p := NEW (P);
      TypeRep.Init (p, Type.Class.Named);
      p.scope      := Scope.Top ();
      p.info.name  := Scanner.MatchID ();
      IF (Scanner.cur.token = TK.tDOT) THEN
        Scanner.GetToken (); (* . *)
        p.module     := p.info.name;
        p.info.name  := Scanner.MatchID ();
      END;
      t := p;
    END;
 
    IF (Scanner.cur.token = TK.tBRANDED) THEN
      t := ObjectType.Parse (t, FALSE, Brand.Parse ());
    ELSIF (Scanner.cur.token = TK.tOBJECT) THEN
      t := ObjectType.Parse (t, FALSE, NIL);
    END;
    RETURN t;
  END Parse;

PROCEDURE New (t: Type.T): Type.T =
  VAR p: P;
  BEGIN
    p := NEW (P);
    TypeRep.Init (p, Type.Class.Named);
    p.type       := t;
    RETURN p;
  END New;

PROCEDURE Create (m, n: M3ID.T): Type.T =
  VAR p: P;
  BEGIN
    p := NEW (P);
    TypeRep.Init (p, Type.Class.Named);
    p.scope      := Scope.Top ();
    p.module     := m;
    p.info.name  := n;
    RETURN p;
  END Create;

PROCEDURE Reduce (t: Type.T): P =
  BEGIN
    IF (t = NIL) THEN RETURN NIL END;
    IF (t.info.class # Type.Class.Named) THEN RETURN NIL END;
    RETURN t;
  END Reduce;

PROCEDURE Split (t: Type.T;  VAR name: M3.QID): BOOLEAN =
  VAR p := Reduce (t);
  BEGIN
    IF (p = NIL) THEN RETURN FALSE END;
    Resolve (p);
    name.module := p.module;
    name.item := p.info.name;
    RETURN TRUE;
  END Split;

PROCEDURE SplitV (t: Type.T;  VAR v: Value.T): BOOLEAN =
(* return V for Value *)
  VAR p := Reduce (t);
  BEGIN
    IF (p = NIL) THEN RETURN FALSE END;
    Resolve (p);
    v := p.obj;
    RETURN TRUE;
  END SplitV;

PROCEDURE Resolve (p: P) =
  VAR o: Value.T;  t: Type.T;  save: INTEGER;
      typename := M3.NoQID;
  BEGIN
    IF (p.type = NIL) THEN
      typename.module := p.module;
      typename.item   := p.info.name;
      o := Scope.LookUpQID (p.scope, typename);
      p.module := typename.module;
      p.obj := o;
      IF (o = NIL) THEN
        save := Scanner.offset;
        Scanner.offset := p.origin;
        Error.QID (typename, "undefined");
        Scanner.offset := save;
        t := ErrType.T;
      ELSIF (Value.ClassOf (o) = Value.Class.Type) THEN
        t := Value.ToType (o);
      ELSE
        save := Scanner.offset;
        Scanner.offset := p.origin;
        Error.QID (typename, "name isn\'t bound to a type");
        Scanner.offset := save;
        t := ErrType.T;
      END;
      p.type := t;
    END;
  END Resolve;

PROCEDURE Strip (t: Type.T): Type.T =
  VAR p: P := t;
  BEGIN
    Resolve (p);
    RETURN p.type;
  END Strip;

PROCEDURE Check (p: P) =
  VAR cs := M3.OuterCheckState;  nErrs, nWarns, nErrsB: INTEGER;
      name := p.info.name;
  BEGIN
    Resolve (p);
    nErrs := 0;  nErrsB := 0;
    IF (p.obj # NIL) THEN
      Error.Count (nErrs, nWarns);
      Value.TypeCheck (p.obj, cs);
      Error.Count (nErrsB, nWarns);
    END;
    IF (nErrs = nErrsB) THEN
      (* no errors yet... *)
      p.type := Type.CheckInfo (p.type, p.info);
    ELSE (* some sort of error (probably illegal recursion...) *)
      EVAL Type.CheckInfo (ErrType.T, p.info);
    END;
    p.info.class := Type.Class.Named; (* this node is still a Named node *)
    p.info.name  := name;
  END Check;

PROCEDURE
  NoStraddle (p: P;  offset: INTEGER; IsEltOrField: BOOLEAN): BOOLEAN =
  BEGIN
    Resolve (p);
    RETURN Type.StraddleFreeScalars (p.type, offset, IsEltOrField);
  END NoStraddle;

PROCEDURE Compiler (p: P) =
  BEGIN
    Resolve (p);
    (*** Type.Compile (p.type);  ***)
    IF (p.type # NIL) THEN
      Scanner.offset := p.type.origin;
      p.type.compile ();
    END;
  END Compiler;

PROCEDURE InitCoster (p: P;  zeroed: BOOLEAN): INTEGER =
  BEGIN
    Resolve (p);
    RETURN Type.InitCost (p.type, zeroed);
  END InitCoster;

PROCEDURE GenInit (p: P;  zeroed: BOOLEAN) =
  BEGIN
    Resolve (p);
    Type.InitValue (p.type, zeroed);
  END GenInit;

PROCEDURE GenMap (p: P;  offset, size: INTEGER;  refs_only: BOOLEAN) =
  BEGIN
    Resolve (p);
    Type.GenMap (p.type, offset, size, refs_only);
  END GenMap;

PROCEDURE GenDesc (p: P) =
  BEGIN
    Resolve (p);
    Type.GenDesc (p.type);
  END GenDesc;

PROCEDURE FPrinter (p: P;  VAR x: M3.FPInfo) =
  VAR typename := M3.QID {p.module, p.info.name};
  BEGIN
    Error.QID (typename, "INTERNAL ERROR: fingerprint of named type");
    Resolve (p);
    IF (p.type # NIL) THEN p.type.fprint (x); END;
  END FPrinter;

BEGIN
END NamedType.
