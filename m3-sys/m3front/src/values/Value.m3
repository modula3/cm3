(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Value.m3                                              *)
(* Last modified on Wed Mar  1 08:51:59 PST 1995 by kalsow     *)
(*      modified on Wed Mar 27 03:00:56 1991 by muller         *)

MODULE Value EXPORTS Value, ValueRep;

IMPORT M3, M3ID, Type, Expr, Error, M3Buf, Scope, M3FP, CG;
IMPORT Scanner, Host, ErrType, TypeFP, Procedure;

CONST NOT_CHECKED = -1;
CONST CHECKED     = 0;

PROCEDURE TypeCheck (t: T;  VAR cs: CheckState) =
  VAR save: INTEGER;
  BEGIN
    IF (t = NIL) THEN RETURN END;
    IF (t.checked) THEN RETURN END;
    IF (t.checkDepth = NOT_CHECKED) THEN
      (* this node is not currently being checked *)
      save := Scanner.offset;
      Scanner.offset := t.origin;
        t.checkDepth := Type.recursionDepth;
        t.typeCheck (cs);
        t.checkDepth := CHECKED;
        t.checked := TRUE;
      Scanner.offset := save;
    ELSIF (t.checkDepth # Type.recursionDepth) THEN
      (* this is a legal recursion, just return *)
    ELSE
      IllegalRecursion (t);
    END;
  END TypeCheck;

PROCEDURE TypeOf (t: T): Type.T =
  VAR x: Type.T;
  BEGIN
    IF (t = NIL) THEN RETURN ErrType.T END;
    IF (t.inTypeOf) THEN IllegalRecursion (t);  RETURN ErrType.T  END;
    t.inTypeOf := TRUE;
    x := t.typeOf ();
    t.inTypeOf := FALSE;
    RETURN x;
  END TypeOf;

PROCEDURE SetGlobals (t: T) =
  BEGIN
    IF (t = NIL ) THEN RETURN END;
    <*ASSERT t.checked *>
    t.set_globals ();
  END SetGlobals;

PROCEDURE Load (t: T) =
  BEGIN
    IF (t = NIL) THEN RETURN END;
    <* ASSERT t.checked *>
    t.used := TRUE;
    t.load ();
  END Load;

PROCEDURE ToExpr (t: T): Expr.T =
  VAR e: Expr.T;
  BEGIN
    IF (t = NIL) THEN RETURN NIL END;
    IF (t.inToExpr) THEN IllegalRecursion (t); RETURN NIL END;
    t.inToExpr := TRUE;
    e := t.toExpr ();
    t.inToExpr := FALSE;
    RETURN e;
  END ToExpr;

PROCEDURE ToType (t: T): Type.T =
  VAR x: Type.T;
  BEGIN
    IF (t = NIL) THEN RETURN NIL END;
    IF (t.inToType) THEN IllegalRecursion (t); RETURN NIL END;
    t.inToType := TRUE;
    x := t.toType ();
    t.inToType := FALSE;
    RETURN x;
  END ToType;

PROCEDURE Base (t: T): T =
  BEGIN
    IF (t = NIL) THEN RETURN NIL END;
    RETURN t.base ();
  END Base;

PROCEDURE IllegalRecursion (t: T) =
  BEGIN
    IF (NOT t.error) THEN
      Error.ID (t.name, "illegal recursive declaration");
      t.error := TRUE;
    END;
  END IllegalRecursion;

PROCEDURE ClassOf (t: T): Class =
  BEGIN
    IF (t = NIL) THEN RETURN Class.Error END;
    RETURN t.class;
  END ClassOf;

VAR mbuf: M3Buf.T := NIL;

PROCEDURE ToFP (t: T): M3FP.T =
  VAR n: CARDINAL;  x: M3.FPInfo;  fp: M3FP.T;
  BEGIN
    IF (t = NIL) THEN RETURN M3FP.Zero END;
    t := Base (t);
    IF (mbuf = NIL) THEN mbuf := M3Buf.New () END;

    (* build the tag *)
    x.tag     := NIL;
    x.buf     := mbuf;  mbuf := NIL;
    x.n_nodes := 0;
    x.others  := NIL;
    n := AddFPTag (t, x);
    IF (x.tag # NIL)
      THEN fp := TypeFP.FromText (x.tag);
      ELSE fp := TypeFP.FromBuf (x.buf);
    END;
    mbuf := x.buf;  x.buf := NIL;  x.tag := NIL;

    (* add any type information *)
    IF (n > 0) THEN
      <*ASSERT n = 1*>
      EVAL AddFPEdges (t, x, 0);
      fp := TypeFP.FromPair (fp, TypeFP.FromType (x.nodes[0]));
    END;

    RETURN fp;
  END ToFP;

PROCEDURE AddFPTag (t: T;  VAR x: M3.FPInfo): CARDINAL =
  VAR n: CARDINAL;
  BEGIN
    IF (t = NIL) THEN RETURN 0 END;
    t := t.base ();
    M3Buf.PutChar (x.buf, '<');
    n := t.add_fp_tag (x);
    M3Buf.PutChar (x.buf, '>');
    RETURN n;
  END AddFPTag;

PROCEDURE AddFPEdges (t: T;  VAR x: M3.FPInfo;  n: CARDINAL): CARDINAL =
  VAR u: T;  type: Type.T;
  BEGIN
    IF (t = NIL) THEN RETURN n END;
    u := t.base ();
    type := u.fp_type ();
    IF (type # NIL) THEN
      IF (x.others = NIL)
        THEN x.nodes [n] := type;
        ELSE x.others [n] := type;
      END;
      INC (n);
    END;
    RETURN n;
  END AddFPEdges;

PROCEDURE FPStart (t: T;  VAR x: M3.FPInfo;  tag: TEXT;
                   offset: INTEGER;  global: BOOLEAN) =
  VAR s: Scope.IDStack;
  BEGIN
    M3Buf.PutText (x.buf, tag);
    IF (global) THEN
      s.top := 0;
      Scope.NameToPrefix (t, s, dots := TRUE);
      Scope.PutStack (x.buf, s);
    ELSE
      M3ID.Put (x.buf, t.name);
    END;
    IF (offset # 0) THEN
      M3Buf.PutChar (x.buf, '@');
      M3Buf.PutInt  (x.buf, offset);
    END;
    IF (t.external) THEN
      M3Buf.PutChar (x.buf, '$');
      M3ID.Put      (x.buf, t.extName);
    END;
  END FPStart;

TYPE
  VSFlags = RECORD
    need_vs     : BOOLEAN;
    imported    : BOOLEAN;
    implemented : BOOLEAN;
  END;

PROCEDURE Declare (t: T) =
  VAR f: VSFlags;  fp: M3FP.T;
  BEGIN
    IF (t = NIL) THEN RETURN END;
    IF (t.declared) THEN RETURN END;
    IF (NOT t.used) AND (t.imported) THEN RETURN END;
    t.declared := TRUE;
    GetVSFlags (t, f);
    IF t.declare () AND (Host.versionStamps) AND (f.need_vs) THEN
      fp := ToFP (t);
      Host.env.note_version_stamp (Scope.ModuleName (t), t.name,
                                   fp, f.imported, f.implemented);
    END;
  END Declare;

PROCEDURE GetVSFlags (t: T;  VAR f: VSFlags) =
  BEGIN
    f.need_vs := t.imported OR t.exported;

    IF (t.external)
      THEN f.imported := NOT t.exported;
      ELSE f.imported := t.imported;
    END;

    f.implemented := NOT f.imported;
    IF (NOT t.external) AND (t.class = Class.Procedure) THEN
      f.implemented := Procedure.HasBody (Base (t));
    END;

  END GetVSFlags;

PROCEDURE ConstInit (t: T) =
  VAR save: INTEGER;
  BEGIN
    IF (t = NIL) THEN RETURN END;
    IF (t.inited) THEN RETURN END;
    <* ASSERT t.checked *>
    t.inited := TRUE;
    save := Scanner.offset;
    Scanner.offset := t.origin;
    t.const_init ();
    Scanner.offset := save;
  END ConstInit;

PROCEDURE NeedsInit (t: T): BOOLEAN =
  BEGIN
    IF (t = NIL) THEN RETURN FALSE END;
    RETURN t.need_init ();
  END NeedsInit;

PROCEDURE LangInit (t: T) =
  VAR save: INTEGER;
  BEGIN
    IF (t = NIL) THEN RETURN END;
    IF (t.compiled) THEN RETURN END;
    <* ASSERT t.checked *>
    t.compiled := TRUE;
    save := Scanner.offset;
    Scanner.offset := t.origin;
    t.lang_init ();
    CG.Free_temps ();
    Scanner.offset := save;
  END LangInit;

PROCEDURE UserInit (t: T) =
  VAR save: INTEGER;
  BEGIN
    IF (t = NIL) THEN RETURN; END;
    save := Scanner.offset;
    Scanner.offset := t.origin;
    t.user_init ();
    CG.Free_temps ();
    Scanner.offset := save;
  END UserInit;

VAR all: T;  (* all values in the current module *)

PROCEDURE Init (t: T;  name: M3ID.T;  c: Class) =
  BEGIN
    t.origin     := Scanner.offset;
    t.name       := name;
    t.extName    := M3ID.NoID;
    t.scope      := NIL;
    t.checkDepth := NOT_CHECKED;
    t.class      := c;
    t.checked    := FALSE;
    t.readonly   := FALSE;
    t.external   := FALSE;
    t.unused     := FALSE;
    t.obsolete   := FALSE;
    t.up_level   := FALSE;

    IF (c # Class.Module) THEN
      t.vnext := all;  all := t;
    END;

    t.error      := FALSE;
    t.used       := FALSE;
    t.declared   := FALSE;
    t.inited     := FALSE;
    t.compiled   := FALSE;
    t.imported   := NOT Scanner.in_main;
    t.exported   := FALSE;
    t.exportable := FALSE;
    t.inTypeOf   := FALSE;
    t.inToExpr   := FALSE;
    t.inToType   := FALSE;
  END Init;

PROCEDURE NoExpr (<*UNUSED*> t: T): Expr.T =
  BEGIN
    <* ASSERT FALSE *>
  END NoExpr;

PROCEDURE NoType (<*UNUSED*> t: T): Type.T =
  BEGIN
    <* ASSERT FALSE *>
  END NoType;

PROCEDURE NoLoader (<*UNUSED*> t: T) =
  BEGIN
    <* ASSERT FALSE *>
  END NoLoader;

PROCEDURE Never (<*UNUSED*> t: T): BOOLEAN =
  BEGIN
    RETURN FALSE;
  END Never;

PROCEDURE NoInit (<*UNUSED*> t: T) =
  BEGIN
  END NoInit;

PROCEDURE Always (<*UNUSED*> t: T): BOOLEAN =
  BEGIN
    RETURN TRUE;
  END Always;

PROCEDURE TypeVoid (<*UNUSED*> t: T): Type.T =
  BEGIN
    RETURN NIL;
  END TypeVoid;

PROCEDURE Self (t: T): T =
  BEGIN
    RETURN t;
  END Self;

PROCEDURE Reset () =
  BEGIN
    all := NIL;
  END Reset;

PROCEDURE SetModule (new: T): T =
  VAR old := all;
  BEGIN
    all := new;
    RETURN old;
  END SetModule;

PROCEDURE Reuse (t: T) =
  BEGIN
    WHILE (t # NIL) DO
      t.used       := FALSE;
      t.error      := FALSE;
      t.declared   := FALSE;
      t.inited     := FALSE;
      t.compiled   := FALSE;
      t.imported   := (NOT Host.emitBuiltins);
      t.exported   := FALSE;
      t.exportable := FALSE;
      t.inTypeOf   := FALSE;
      t.inToExpr   := FALSE;
      t.inToType   := FALSE;
      t := t.vnext;
    END;
  END Reuse;

PROCEDURE IsExternal (t: T): BOOLEAN =
  BEGIN
    RETURN (t.external);
  END IsExternal;

PROCEDURE IsImported (t: T): BOOLEAN =
  BEGIN
    RETURN (t # NIL) AND (t.imported);
  END IsImported;

PROCEDURE IsWritable (t: T): BOOLEAN =
  BEGIN
    RETURN NOT t.readonly;
  END IsWritable;

PROCEDURE CName (t: T): M3ID.T =
  BEGIN
    IF (t = NIL) THEN RETURN M3ID.NoID END;
    RETURN t.base().name;
  END CName;

PROCEDURE GlobalName (t: T;  dots, with_module: BOOLEAN): TEXT =
  VAR ss: Scope.IDStack;
  BEGIN
    IF (t = NIL) THEN RETURN NIL END;
    ss.top := 0;
    Scope.NameToPrefix (t, ss, NOT dots, dots, with_module);
    RETURN Scope.StackToText (ss);
  END GlobalName;

BEGIN
END Value.
