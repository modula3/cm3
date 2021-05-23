(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: External.m3                                           *)
(* Last Modified On Wed Mar  1 08:42:34 PST 1995 By kalsow     *)

MODULE External;

IMPORT M3ID, Value, ValueRep, Token, Scope, Module, Error;
IMPORT Type, Expr, Variable, Ident, Scanner, RunTyme, CG, Host;
FROM Scanner IMPORT GetToken, Match, MatchID, cur;
FROM M3 IMPORT QID;

TYPE TK = Token.T;

REVEAL
  Set = BRANDED "Import.Set" REF RECORD
    exports    : Port;
    imports    : Port;
    importObjs : T;
    last_obj   : T;
  END;

TYPE
  Port = REF RECORD
    next   : Port;
    module : Module.T;
    origin : INTEGER;
    source : T;
    name   : M3ID.T;
    direct : BOOLEAN;
    export : BOOLEAN;
  END;

TYPE
  T = Value.T BRANDED "Import.T" OBJECT
    next : T;
    obj  : Value.T;
    home : Port;
  OVERRIDES
    typeCheck   := Check;
    set_globals := ValueRep.NoInit;
    load        := Load;
    declare     := Declare;
    const_init  := ConstInit;
    need_init   := NeedInit;
    lang_init   := LangInit;
    user_init   := UserInit;
    toExpr      := ToExpr;
    toType      := ToType;
    typeOf      := TypeOf;
    repTypeOf   := RepTypeOf;
    base        := Base;
  END;

PROCEDURE NewSet (): Set =
  VAR s := NEW (Set);
  BEGIN
    s.exports    := NIL;
    s.imports    := NIL;
    s.importObjs := NIL;
    s.last_obj   := NIL;
    RETURN s;
  END NewSet;

PROCEDURE NoteExport (s: Set;  name: M3ID.T) =
  VAR ex: Module.T;  p: Port;
  BEGIN
    ex := Module.LookUp (name, internal := FALSE);
    IF (ex = NIL) THEN RETURN END;
    p := Push (s.exports, ex, name);
    p.direct := TRUE;
    p.export := TRUE;
  END NoteExport;

PROCEDURE NoteImport (s: Set;  im: Module.T;  name: M3ID.T) =
  VAR p: Port;
  BEGIN
    IF (im = NIL) THEN RETURN END;
    p := Push (s.imports, im, name);
    p.source := ImportObj (s, im, name, cur.offset, p);
    p.direct := TRUE;
  END NoteImport;

PROCEDURE ParseImports (s: Set;  self: Module.T) =
  VAR runtime: Module.T;  name: M3ID.T;
  BEGIN
    (* parse the "magic" runtime import *)
    RunTyme.Import ();
    RunTyme.Bind (self, runtime, name);
    IF (runtime # NIL) THEN
      NoteImport (s, runtime, name);
      s.last_obj.used := TRUE; (* so the user doesn't get any warnings *)
    END;

    (* parse the explicit imports *)
    LOOP
      IF    (cur.token = TK.tIMPORT) THEN ParseImport (s);
      ELSIF (cur.token = TK.tFROM)   THEN ParseFromImport (s);
      ELSE  EXIT;
      END;
    END;

    ResolveImports (s, self);
  END ParseImports;

PROCEDURE ParseImport (s: Set) =
  VAR id, alias: M3ID.T;  im: Module.T;
  BEGIN
    Match (TK.tIMPORT);
    LOOP
      id := MatchID ();
      alias := id;

      IF (cur.token = TK.tAS) THEN
        GetToken (); (* AS *)
        alias := MatchID ();
      END;

      im := Module.LookUp (id, internal := FALSE);
      NoteImport (s, im, alias);

      IF (cur.token # TK.tCOMMA) THEN EXIT END;
      GetToken (); (* , *)
    END;
    Match (TK.tSEMI);
  END ParseImport;

PROCEDURE ParseFromImport (s: Set) =
  VAR id: M3ID.T;  j, n: INTEGER;   p: Port;
  BEGIN
    Match (TK.tFROM);
    id := MatchID ();
    Match (TK.tIMPORT);
    n := Ident.ParseList ();
    Match (TK.tSEMI);

    p := Push (s.imports, NIL, id);

    j := Ident.top - n;
    FOR i := 0 TO n - 1 DO
      EVAL ImportObj (s, NIL, Ident.stack[j + i], Ident.offset[j + i], p);
    END;
    DEC (Ident.top, n);
  END ParseFromImport;

PROCEDURE Push (VAR list: Port;  m: Module.T;  name: M3ID.T): Port =
  VAR p: Port;
  BEGIN
    (* search for a match *)
    p := list;
    WHILE (p # NIL) DO
      IF (p.name = name) THEN
        IF (m = NIL) OR (p.module = m) THEN (* ok *)
        ELSIF (p.module = NIL) THEN p.module := m;
        ELSE Error.ID (name, "inconsistent imports");
        END;
        RETURN p;
      END;
      p := p.next;
    END;

    (* build a new entry *)
    p := NEW (Port);
    p.next   := list;  list := p;
    p.module := m;
    p.name   := name;
    p.origin := Scanner.offset;
    p.source := NIL;
    p.direct := FALSE;
    p.export := FALSE;
    RETURN p;
  END Push;

PROCEDURE ImportObj (s: Set;  obj: Value.T;  name: M3ID.T;
                      offset: INTEGER;  port: Port): T =
  VAR t := NEW (T);  c := Value.Class.Error;
  BEGIN
    IF (s = NIL) THEN RETURN NIL END;
    IF (obj # NIL) THEN c := obj.class; END;
    ValueRep.Init (t, name, c);
    t.origin   := offset;
    t.next     := NIL;
    t.obj      := obj;
    t.home     := port;
    t.imported := TRUE;
    t.exported := FALSE;
    IF (port.export) THEN t.exportable := TRUE END;
    IF (s.importObjs = NIL)
      THEN s.importObjs := t;
      ELSE s.last_obj.next := t;
    END;
    s.last_obj := t;
    RETURN t;
  END ImportObj;

PROCEDURE ResolveImports (s: Set;  self: Module.T) =
  VAR
    p     : Port;
    t     : T;
    m     : Module.T;
    v     : Value.T;
    syms  : Scope.T;
    save  : INTEGER;
  BEGIN
    save := Scanner.offset;

    (* import the exported symbols *)
    p := s.exports;
    WHILE (p # NIL) DO
      m := p.module;
      IF (m # NIL) AND (m # self) THEN
        v := Scope.ToList (Module.ExportScope (m));
        WHILE (v # NIL) DO
          EVAL ImportObj (s, v, v.name, p.origin, p);
          v := v.next;
        END;
      END;
      p := p.next;
    END;

    (* resolve the deferred "FROM x IMPORT" modules *)
    p := s.imports;
    WHILE (p # NIL) DO
      IF (p.module = NIL) THEN
        Scanner.offset := p.origin;
        p.module := LookUpInList (p.name, s.imports);
      END;
      p := p.next;
    END;

    (* resolve the deferred "FROM x IMPORT y" imports *)
    t := s.importObjs;
    WHILE (t # NIL) DO
      IF (t.obj = NIL) THEN
        (* this item is from a "FROM x IMPORT" => look up that was deferred *)
        Scanner.offset := t.origin;
        p := t.home;
        IF (p.source # NIL) THEN p.source.used := TRUE END;
        syms := Module.ExportScope (p.module);
        IF (syms # NIL)
          THEN v := Scope.LookUp (syms, t.name, TRUE);
          ELSE v := NIL; (* probably a circular import! *)
        END;
        IF (v # NIL) THEN
          t.obj   := v;
          t.class := v.class;
        ELSE
          Error.QID (QID {module := p.name, item := t.name},
                      "symbol not exported")
        END;
      END;
      t := t.next;
    END;

    Scanner.offset := save;
  END ResolveImports;

PROCEDURE LookUpInList (name: M3ID.T;  local: Port): Module.T =
  BEGIN
    WHILE (local # NIL) DO
      IF (local.name = name) AND (local.module # NIL) THEN
        RETURN local.module;
      END;
      local := local.next;
    END;
    RETURN Module.LookUp (name, internal := FALSE);
  END LookUpInList;

PROCEDURE LoadImports (s: Set;  self: Module.T) =
  VAR p: Port;  t: T;  m: Module.T;  save: INTEGER;
  BEGIN
    save := Scanner.offset;

    (* load the imported symbols *)
    t := s.importObjs;
    WHILE (t # NIL) DO
      Scanner.offset := t.origin;
      IF (t.obj # NIL) THEN Scope.Insert (t) END;
      t := t.next;
    END;

    (* get the revelations in imported interfaces *)
    p := s.imports;
    WHILE (p # NIL) DO
      IF (p.direct) THEN
        m := p.module;
        Scanner.offset := p.origin;
        IF (m # NIL) AND (m # self) THEN
          Module.ImportRevelations (m, p.source);
        END;
      END;
      p := p.next;
    END;

    (* get the revelations in exported interfaces *)
    p := s.exports;
    WHILE (p # NIL) DO
      IF (p.direct) THEN
        m := p.module;
        Scanner.offset := p.origin;
        IF (m # NIL) AND (m # self) THEN
          Module.ImportRevelations (m, p.source);
        END;
      END;
      p := p.next;
    END;

    Scanner.offset := save;
  END LoadImports;

PROCEDURE IsExportable (v: Value.T): BOOLEAN =
  BEGIN
    TYPECASE v OF
    | NULL => RETURN FALSE;
    | T(t) => RETURN t.home.export;
    ELSE      RETURN FALSE;
    END;
  END IsExportable;

PROCEDURE Redirect (intf, impl: Value.T) =
  VAR t: T := intf;
  BEGIN
    t.obj := impl;
  END Redirect;

PROCEDURE GenLinkInfo (s: Set) =
  BEGIN
    GenInitLinks (s.exports, FALSE, CG.Export_unit);
    GenInitLinks (s.imports, TRUE,  CG.Import_unit);
  END GenLinkInfo;

PROCEDURE GenInitLinks (p: Port;  imported: BOOLEAN;
                        note: PROCEDURE (n: CG.Name)) =
  VAR x, y: Port;
  BEGIN
    x := p;
    WHILE (x # NIL) DO
      y := p;
      LOOP
        IF (x = y) THEN
          Module.ImportInterface (x.module);
          Host.env.note_interface_use (x.module.name, imported);
          note (x.module.name);
          EXIT;
        END;
        IF (x.module = y.module) THEN (* duplicate *) EXIT  END;
        y := y.next;
      END;
      x := x.next;
    END;
  END GenInitLinks;

PROCEDURE GenImports (s: Set) =
  VAR p: Port;
  BEGIN
    p := s.imports;
    WHILE (p # NIL) DO
      Scope.Enter (Module.ExportScope (p.module));
      p := p.next;
    END;
  END GenImports;

(*---------------------------------------------------------------------------*)

PROCEDURE InitGlobals (s: Set) =
  VAR x := s.exports;
  BEGIN
    WHILE (x # NIL) DO InitExports (x.module); x := x.next;  END;
  END InitGlobals;

PROCEDURE InitExports (interface: Module.T) =
  VAR o: Value.T;
  BEGIN
    o := Scope.ToList (Module.ExportScope (interface));
    WHILE (o # NIL) DO
      IF (o.exported) AND (Value.ClassOf (o) = Value.Class.Var) THEN
        Variable.InitGlobal (o);
      END;
      o := o.next;
    END;
  END InitExports;

PROCEDURE Visit (s: Set;  v: Module.Visitor) =
  VAR p := s.exports;
  BEGIN
    WHILE (p # NIL) DO
      v (p.module);
      p := p.next;
    END;
    p := s.imports;
    WHILE (p # NIL) DO
      v (p.module);
      p := p.next;
    END;
  END Visit;

(*---------------------------------------------------------------------------*)

PROCEDURE Check (t: T;  VAR cs: Value.CheckState) =
  BEGIN
    Value.TypeCheck (t.obj, cs);
  END Check;

PROCEDURE Load (t: T) =
  BEGIN
    Value.Load (t.obj);
  END Load;

PROCEDURE Declare (t: T): BOOLEAN =
  VAR i, e, u: BOOLEAN;  o: Value.T;
  BEGIN
    o := t.obj;
    IF (o # NIL) THEN
      i := o.imported;           e := o.exported;           u := o.used;
      o.imported := t.imported;  o.exported := t.exported;  o.used := t.used;
      Value.Declare (t.obj);
      o.imported := i;           o.exported := e;           o.used := u;
    END;
    RETURN FALSE;
  END Declare;

PROCEDURE ConstInit (t: T) =
  BEGIN
    Value.ConstInit (t.obj);
  END ConstInit;

PROCEDURE NeedInit (t: T): BOOLEAN =
  BEGIN
    RETURN Value.NeedsInit (t.obj);
  END NeedInit;

PROCEDURE LangInit (t: T) =
  BEGIN
    Value.LangInit (t.obj);
  END LangInit;

PROCEDURE UserInit  (t: T) =
  BEGIN
    Value.UserInit (t.obj);
  END UserInit;

PROCEDURE ToExpr (t: T): Expr.T =
  BEGIN
    RETURN Value.ToExpr (t.obj);
  END ToExpr;

PROCEDURE ToType (t: T): Type.T =
  BEGIN
    RETURN Value.ToType (t.obj);
  END ToType;

PROCEDURE TypeOf (t: T): Type.T =
  BEGIN
    RETURN Value.TypeOf (t.obj);
  END TypeOf;

PROCEDURE RepTypeOf (t: T): Type.T =
  BEGIN
    RETURN Value.RepTypeOf (t.obj);
  END RepTypeOf;

PROCEDURE Base (t: T): Value.T =
  BEGIN
    RETURN Value.Base (t.obj);
  END Base;

BEGIN
END External.
