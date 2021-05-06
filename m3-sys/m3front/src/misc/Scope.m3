(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Scope.m3                                              *)
(* Last modified on Tue Feb 28 17:01:39 PST 1995 by kalsow     *)
(*      modified on Sat Feb 16 02:55:11 1991 by muller         *)

MODULE Scope;

IMPORT M3, M3ID, M3Buf, CG, Value, Module, Error, Procedure;
IMPORT Scanner, ValueRep, Variable, Tracer, Convert;

CONST
  MinHashed = 30;

REVEAL
  M3.Scope = M3.Node BRANDED "Scope.T" OBJECT
    next       : T;
    parent     : T;
    sname      : M3ID.T;
    children   : INTEGER  := 0;
    head       : Value.T  := NIL;
    tail       : Value.T  := NIL;
    open       : BOOLEAN  := FALSE;  (* => lookups can see parent *)
    module     : BOOLEAN  := FALSE;  (* => is an outer module scope *)
    nested     : BOOLEAN  := FALSE;  (* => is a nested block *)
    proc_frame : BOOLEAN  := FALSE;
    home       : Module.T := NIL;
    n_elts     : INTEGER  := 0;
    hash_size  : INTEGER  := 0;
    hash       : REF ARRAY OF Value.T := NIL;
  END;

VAR (* explicit reset *)
  top       : T;
  allScopes : T;

VAR (* string "constants" *)
  emptyStr    : M3ID.T;
  Dot         : M3ID.T;
  DUnderscore : M3ID.T;

VAR (* never reset *)
  stack_buf : M3Buf.T := NIL;
  int_cache := ARRAY [0..15] OF M3ID.T {M3ID.NoID, ..};

PROCEDURE PopNew () =
  BEGIN
    top := top.parent;
  END PopNew;

PROCEDURE PushNew (open: BOOLEAN;  name: M3ID.T;  module := FALSE;
                   nested := FALSE): T =
  VAR t := NEW (T);
  BEGIN
    t.origin     := Scanner.offset;
    t.next       := allScopes;  allScopes := t;
    t.parent     := top;
    t.sname      := name;
    t.open       := open;
    t.module     := module;
    t.nested     := nested;
    t.proc_frame := (open) AND (NOT module) AND (NOT nested);
    t.home       := Module.Current ();
    top := t;
    EVAL ScopeName(t); (* Force every anonymous block to have a number, even
                          if not used in a prefix of a procedure name.
                          without this, m3gdb can't figure out what block
                          a nested procedure is inside of. *)
    RETURN t;
  END PushNew;

PROCEDURE New1 (obj: Value.T): T =
  VAR t := PushNew (open:=TRUE, name:=M3ID.NoID, module:=FALSE, nested:=TRUE);
  BEGIN
    Insert (obj);
    RETURN t;
  END New1;

PROCEDURE Push (t: T): T =
  VAR old := top;
  BEGIN
    <* ASSERT t # NIL *>
    top := t;
    RETURN old;
  END Push;

PROCEDURE Pop (old: T) =
  BEGIN
    <* ASSERT old # NIL *>
    top := old;
  END Pop;

PROCEDURE Top (): T =
  (* return the top "open" scope *)
  VAR t: T;
  BEGIN
    t := top;
    WHILE (t # NIL) AND (NOT t.open) DO t := t.parent END;
    RETURN t;
  END Top;

PROCEDURE OuterMost (t: T) : BOOLEAN =
  BEGIN
    RETURN (t # NIL) AND (t.module);
  END OuterMost;

PROCEDURE LookUpQID (t: T;  VAR q: M3.QID): Value.T =
  VAR v: Value.T := NIL;
  BEGIN
    IF (q.module = M3ID.NoID) THEN
      v := LookUp (t, q.item, FALSE);
      IF v # NIL THEN
        q.module := ModuleName (v);
        <* ASSERT q.module # M3ID.NoID *>
      END;
    ELSE
      TYPECASE Value.Base (LookUp (t, q.module, FALSE)) OF
      | Module.T (m) => v := LookUp (Module.ExportScope (m), q.item, TRUE);
      ELSE
      END;
    END;
    RETURN v;
  END LookUpQID;

PROCEDURE LookUp (t: T;  name: M3ID.T;  strict: BOOLEAN): Value.T =
  VAR o: Value.T;  up_level := FALSE;  x, y: CARDINAL;
  BEGIN
    LOOP
      IF (t = NIL) THEN RETURN NIL END;
      IF (t.n_elts < MinHashed) THEN
        o := t.head;
        WHILE (o # NIL) AND (o.name # name) DO o := o.next END;
      ELSE
        IF (t.n_elts # t.hash_size) THEN Rehash (t) END;
        y := NUMBER (t.hash^);
        x := M3ID.Hash (name) MOD y;
        LOOP
          o := t.hash [x];
          IF (o = NIL) OR (o.name = name) THEN EXIT END;
          INC (x);  IF (x >= y) THEN x := 0 END;
        END;
      END;
      IF (o # NIL) THEN EXIT END;
      IF (strict) OR (NOT t.open) THEN RETURN NIL END;
      up_level := up_level OR t.proc_frame;
      t := t.parent;
    END;
    o.up_level := o.up_level OR up_level;

    IF (Scanner.in_main) THEN
      (* this is a top-level use of the symbol! *)
      o.used := TRUE;
      IF o.obsolete THEN
        Error.WarnID (2, name, "<*OBSOLETE*> symbol used");
      ELSIF o.unused THEN
        Error.WarnID (2, name, "<*UNUSED*> symbol used");
      END;
    END;

    RETURN o;
  END LookUp;

PROCEDURE Rehash (t: T) =
  VAR
    max  := 2 * t.n_elts;
    x    := NEW (REF ARRAY OF Value.T, max);
    v    := t.head;
    hash : INTEGER;
  BEGIN
    WHILE (v # NIL) DO
      hash := M3ID.Hash (v.name) MOD max;
      WHILE (x[hash] # NIL) DO
        INC (hash);
        IF (hash >= max) THEN hash := 0 END;
      END;
      x[hash] := v;
      v := v.next;
    END;
    t.hash_size := t.n_elts;
    t.hash := x;
  END Rehash;

PROCEDURE Insert (o: Value.T) =
  VAR t := top;
  BEGIN
    (* check for a reserved word *)
    IF (t # Initial) AND (LookUp (Initial, o.name, TRUE) # NIL) THEN
      Error.ID (o.name, "Reserved identifier redefined");
    END;

    IF (o.next # NIL) THEN
      Error.ID (o.name, "INTERNAL ERROR: value reinserted in scope");
      <*ASSERT FALSE*>
    END;

    (* finally, do the insertion *)
    IF (o.scope = NIL) THEN o.scope := t END;
    IF (t.tail # NIL)
      THEN t.tail.next := o;
      ELSE t.head := o;
    END;
    t.tail := o;
    INC (t.n_elts);
  END Insert;

PROCEDURE ToList (t: T): Value.T =
  BEGIN
    IF (t = NIL)
      THEN  RETURN NIL;
      ELSE  RETURN t.head;
    END;
  END ToList;

PROCEDURE TypeCheck (t: T;  VAR cs: Value.CheckState) =
  (* note: we separate the type checking of procedures heads and bodies
     in an attempt to keep the error messages sorted in a rational order *)
  VAR v: Value.T;
  BEGIN
    IF (t = NIL) THEN RETURN END;
    CheckDuplicates (t);
    v := t.head;
    WHILE (v # NIL) DO  Value.TypeCheck (v, cs);  v := v.next;  END;
    v := t.head;
    WHILE (v # NIL) DO
      TYPECASE Value.Base (v) OF
      | NULL            => (* ignore *)
      | Procedure.T (p) => Procedure.CheckBody (p, cs);
      ELSE                 (* ignore *)
      END;
      v := v.next;
    END;
  END TypeCheck;

PROCEDURE Enter (t: T) =
  VAR v: Value.T;  c: Value.Class;
  BEGIN
    IF (t = NIL) THEN RETURN END;
    IF (t.nested) THEN CG.Begin_block () END;
    v := t.head;
    WHILE (v #  NIL) DO
      c := Value.ClassOf (v);
      IF (c # Value.Class.Procedure) THEN Value.Declare (v) END;
      v := v.next;
    END;
    v := t.head;
    WHILE (v #  NIL) DO
      c := Value.ClassOf (v);
      IF (c = Value.Class.Procedure) THEN Value.Declare (v) END;
      v := v.next;
    END;
    v := t.head;
    WHILE (v #  NIL) DO
      Value.ConstInit (v);
      v := v.next;
    END;
  END Enter;

PROCEDURE InitValues (t: T) =
  VAR v: Value.T;
  BEGIN
    IF t = NIL THEN RETURN; END;
    v := t.head;
    WHILE (v #  NIL) DO  Value.LangInit (v);  v := v.next;  END;
    Tracer.EmitPending ();
    v := t.head;
    WHILE (v #  NIL) DO  Value.UserInit (v);  v := v.next;  END;
    Tracer.EmitPending ();
  END InitValues;

PROCEDURE Exit (t: T) =
  BEGIN
    IF (t.nested) THEN CG.End_block () END;
  END Exit;

PROCEDURE WarnUnused (t: T) =
  VAR save, level: INTEGER;  vc: Value.Class;  v: Value.T;
  BEGIN
    IF (t = NIL) THEN RETURN END;
    save := Scanner.offset;
    v := t.head;
    WHILE (v # NIL) DO
      IF (NOT v.used) AND (NOT v.exportable) THEN
        IF NOT (v.unused OR v.obsolete) THEN
          level := 2;
          vc := Value.ClassOf (v);
          IF (vc = Value.Class.Formal) OR
            ((vc = Value.Class.Var) AND Variable.IsFormal (Value.Base(v))) THEN
            level := 1;
          END;
          Scanner.offset := v.origin;
          Error.WarnID (level, v.name, "not used");
        END;
      END;
      v := v.next;
    END;
    Scanner.offset := save;
  END WarnUnused;

PROCEDURE CheckDuplicates (t: T) =
  VAR v: Value.T;
  BEGIN
    M3ID.AdvanceMarks ();

    (* check each name *)
    v := t.head;
    WHILE (v # NIL) DO
      IF M3ID.SetMark (v.name) THEN ReportDuplicate (t, v) END;
      v := v.next;
    END;
 END CheckDuplicates;

PROCEDURE ReportDuplicate (t: T;  v1: Value.T) =
  VAR save := Scanner.offset;  v2: Value.T;
  BEGIN
    (* verify the duplicate *)
    v2 := t.head;
    WHILE (v2 # NIL) AND ((v2.name # v1.name) OR (v2 = v1)) DO
      v2 := v2.next;
    END;
    IF (v2 = NIL) THEN RETURN END;

    (* report the error *)
    Scanner.offset := v1.origin;
    IF (Value.Base (v1) = Value.Base (v2)) THEN
      (* same item duplicated => kill 2nd one *)
      Error.ID (v1.name, "duplicate import");
    ELSE (* different items with the same name! *)
      Error.ID (v1.name, "symbol redefined");
    END;
    Scanner.offset := save;
  END ReportDuplicate;

PROCEDURE ModuleName (v: Value.T): M3ID.T =
  BEGIN
    v := Value.Base (v);
    RETURN ScopeName (v.scope);
  END ModuleName;

PROCEDURE PutStack (mbuf: M3Buf.T;  READONLY s: IDStack) =
  BEGIN
    FOR i := 0 TO s.top-1 DO M3ID.Put (mbuf, s.stk[i]); END;
  END PutStack;

PROCEDURE StackToText (READONLY s: IDStack): TEXT =
  BEGIN
    IF (stack_buf = NIL) THEN stack_buf := M3Buf.New (); END;
    FOR i := 0 TO s.top-1 DO M3ID.Put (stack_buf, s.stk[i]); END;
    RETURN M3Buf.ToText (stack_buf);
  END StackToText;

PROCEDURE NameToPrefix (v: Value.T;
                    VAR p: IDStack;
                        considerExternal := TRUE;
                        dots := FALSE;
                        with_module := TRUE) =
  VAR t: T; count: INTEGER := 0;  dot, scope_name: M3ID.T;  save: INTEGER;
  BEGIN
    v := Value.Base (v);
    IF (dots)
      THEN dot := Dot;
      ELSE dot := DUnderscore;
    END;

    IF considerExternal AND v.external THEN
      (* simple external name:  foo  *)
      p.stk [p.top] := v.extName;
      INC (p.top);

    ELSIF v.exported OR v.imported OR (v.scope # NIL AND v.scope.module) THEN
      (* global names:   foo   module.foo  *)
      scope_name := ScopeName (v.scope);
      IF (scope_name = emptyStr) OR (NOT with_module) THEN
        p.stk [p.top] := v.name;
        INC (p.top, 1);
      ELSE
        p.stk [p.top]   := scope_name;
        p.stk [p.top+1] := dot;
        p.stk [p.top+2] := v.name;
        INC (p.top, 3);
      END;

    ELSIF with_module
      OR Value.ClassOf (v) = Value.Class.Procedure
      OR Value.ClassOf (v) = Value.Class.Expr THEN
      (* procedure or constant => fully qualified name:  module.p1.p2.p *)

      (* count how may strings we'll produce *)
      save := p.top;
      count := p.top;
      t := v.scope;
      LOOP
        IF t = NIL THEN EXIT END;
        IF (with_module) OR (NOT t.module) THEN
          scope_name := ScopeName (v.scope);
          IF scope_name # emptyStr THEN INC (count, 2); END;
        END;
        IF (NOT t.open) THEN EXIT END;
        IF (t.module) THEN EXIT END;
        t := t.parent;
      END;
      INC (count);

      p.top := count; 
      DEC (count);
      p.stk [count] := v.name;
      t := v.scope;
      LOOP
        IF t = NIL THEN EXIT END;
        IF (with_module) OR (NOT t.module) THEN
          scope_name := ScopeName (t);
          IF scope_name # emptyStr THEN 
            DEC (count, 2);
            p.stk [count + 1] := dot;
            p.stk [count]     := scope_name;
          END;
        END;
        IF (NOT t.open) THEN EXIT END;
        IF (t.module) THEN EXIT END;
        t := t.parent;
      END;
      <* ASSERT count = save *>

    ELSE
      (* variable => simple name:  foo *)
      p.stk [p.top] := v.name;
      INC (p.top);
    END;
  END NameToPrefix;

PROCEDURE ScopeName (t: T): M3ID.T =
  BEGIN
    IF t = NIL THEN RETURN emptyStr END;
    IF t.sname = M3ID.NoID THEN
      IF (t.parent = NIL) THEN
        t.sname := emptyStr;
      ELSE
        INC (t.parent.children);
        t.sname := InternalName (t.parent.children);
      END;
    END;
    RETURN t.sname;
  END ScopeName;

PROCEDURE InternalName (n: INTEGER): M3ID.T =
  BEGIN
    IF (n < 0) OR (LAST (int_cache) < n) THEN RETURN NewInt (n) END;
    IF int_cache [n] = M3ID.NoID THEN int_cache [n] := NewInt (n) END;
    RETURN int_cache [n];
  END InternalName;

PROCEDURE NewInt (i: INTEGER): M3ID.T =
  <*FATAL Convert.Failed*>
  VAR
    buf : ARRAY [0..BITSIZE(INTEGER)-1] OF CHAR;
    len := Convert.FromInt (buf, i);
  BEGIN
    RETURN M3ID.FromStr (buf, len);
  END NewInt;

PROCEDURE ToUnit (v: Value.T): M3.Value =
  VAR t, t0: T;
  BEGIN
    v := Value.Base (v);

    (* look for the containing module scope *)
    t := v.scope;  t0 := t;
    WHILE (t # NIL) AND (t.home = NIL) DO t := t.parent END;

    IF (t = NIL) THEN RETURN NIL END;

    (* mark any intervening scopes with the same "home" *)
    WHILE (t0 # t) DO t0.home := t.home;  t0 := t0.parent END;

    RETURN t.home;
  END ToUnit;

PROCEDURE Initialize () =
  BEGIN
    emptyStr    := M3ID.Add ("");
    Dot         := M3ID.Add (".");
    DUnderscore := M3ID.Add ("__");

    Initial := PushNew (FALSE, M3ID.Add ("M3_BUILTIN")(*emptyStr*));
  END Initialize;

PROCEDURE Reset () =
  BEGIN
    allScopes := NIL;
    top       := Initial;
  END Reset;

BEGIN
END Scope.
