(* $Id$ *)

MODULE TypeTranslator;

(* 
 * Copyright (c) 2009, Generation Capital Ltd.  All rights reserved.
 * Author : Mika Nystrom <mika@alum.mit.edu> 
 *)

IMPORT Type, SchemeObject;
IMPORT SchemeLongReal, Atom, SchemePair, SchemeSymbol;
IMPORT SchemeBoolean;
FROM Type IMPORT Qid, Field, Method, Signature, Formal, Exception, Mode;
FROM Type IMPORT MethodDefault, MethodDefault1, MethodDefault2;
FROM Type IMPORT Override;
FROM SchemeUtils IMPORT Cons, MakeList, List1, Reverse;
IMPORT ValueTranslator;

PROCEDURE F(name : TEXT;
            what : SchemeObject.T) : SchemeObject.T =
  BEGIN 
    RETURN NEW(SchemePair.T,
               first := SchemeSymbol.FromText(name),
               rest := what) 
  END F;

PROCEDURE B(b : BOOLEAN) : SchemeObject.T =
  BEGIN RETURN SchemeBoolean.Truth(b) END B;

PROCEDURE LRI(i : INTEGER) : SchemeLongReal.T =
  BEGIN RETURN SchemeLongReal.FromI(i) END LRI;

TYPE 
  Rec = REF RECORD
    t : Type.T;
    u : SchemeObject.T;
    n : Rec := NIL;
  END;

VAR recs : Rec := NIL;

PROCEDURE GotIt(t : Type.T; VAR o : SchemeObject.T) : BOOLEAN =
  VAR p := recs;
  BEGIN
    WHILE p # NIL DO
      IF p.t = t THEN o := p.u; RETURN TRUE END;
      p := p.n
    END;
    RETURN FALSE
  END GotIt;

PROCEDURE Insert(t : Type.T; o : SchemeObject.T) =
  BEGIN
    recs := NEW(Rec, t := t, u := o, n := recs)
  END Insert;

PROCEDURE Translate(t : Type.T) : SchemePair.T =    
  BEGIN RETURN Trans(t) END Translate;

PROCEDURE Trans(t : Type.T) : SchemeObject.T =

  PROCEDURE M(named : TEXT;
              w0, w1, w2, w3, w4, w5, w6, w7 : SchemeObject.T := NIL) : SchemeObject.T =

    PROCEDURE Cons(what : SchemeObject.T) =
      BEGIN
        res := NEW(SchemePair.T,
                   first := what, rest := res)
      END Cons;

    VAR
      res : SchemePair.T := NIL;
    BEGIN
      head.first := SchemeSymbol.FromText(named);

(*
      IF qid # NIL THEN 
        Cons(F("alias",TranslateQid(qid))) 
      END;
*)

      Cons(F("name",TranslateQid(t.name)));
      Cons(F("visited",B(t.visited)));
      Cons(F("brandsOK",B(t.brandsOK)));
      IF w0 # NIL THEN 
        Cons(w0);
        IF w1 # NIL THEN
          Cons(w1);
          IF w2 # NIL THEN
            Cons(w2);
            IF w3 # NIL THEN
              Cons(w3);
              IF w4 # NIL THEN
                Cons(w4);
                IF w5 # NIL THEN
                  Cons(w5);
                  IF w6 # NIL THEN
                    Cons(w6);
                    IF w7 # NIL THEN
                      Cons(w7);
                    END
                  END
                END
              END
            END
          END
        END
      END;

      head.rest := Reverse(res);

      RETURN head
    END M;

  VAR 
    r : SchemeObject.T;
    head : SchemePair.T;
  BEGIN
    IF t = NIL THEN RETURN NIL END;

    IF GotIt(t, r) THEN RETURN r END;

    head := NEW(SchemePair.T);
    
    Insert(t, head);

    TYPECASE t OF
      Type.Procedure(p) => RETURN M("Procedure", 
                                    F("sig",TranslateSignature(p.sig)))
    |
      Type.Set(s)       => RETURN M("Set", F("range", Trans(s.range)))
    |
      Type.Record(r)    => RETURN M("Record", F("fields",
                                                TranslateFieldArray(r.fields^)))
    |
      Type.Packed(p)    => RETURN M("Packed", 
                                    F("size",LRI(p.size)), 
                                    F("base",Trans(p.base)))
    |
      Type.Object(o)    => RETURN M("Object",
                                    F("traced",B(o.traced)),
                                    F("branded", B(o.branded)),
                                    F("brand", o.brand),
                                    F("revIntf", o.revIntf),
                                    F("super", Trans(o.super)),
                                    F("fields", TranslateFieldArray(o.fields^)),
                                    F("methods", TranslateMethodArray(o.methods^)),
                                    F("overrides", TranslateOverrideArray(o.overrides^))
)
    |
      Type.Ref(o)       => RETURN M("Ref",
                                    F("traced",B(o.traced)),
                                    F("branded", B(o.branded)),
                                    F("brand", o.brand),
                                    F("revIntf", o.revIntf),
                                    F("target", Trans(o.target)))
    |
      Type.Opaque(o) => RETURN M("Opaque",
                                    F("traced",B(o.traced)),
                                    F("branded", B(o.branded)),
                                    F("brand", o.brand),
                                    F("revIntf", o.revIntf),
                                    F("revealedSuperType", Trans(o.revealedSuperType)))
    | 
      Type.OpenArray(a) => RETURN M("OpenArray",
                                    F("index", Trans(a.index)),
                                    F("element", Trans(a.element)),
                                    F("refArray", Trans(a.refArray)),
                                    F("openDimensions", LRI(a.openDimensions)))
      
    | 
      Type.Array(a) => RETURN M("Array",
                                    F("index", Trans(a.index)),
                                    F("element", Trans(a.element)))
      
    |
      Type.Reference(o) => RETURN M("Reference",
                                    F("traced",B(o.traced)),
                                    F("branded", B(o.branded)),
                                    F("brand", o.brand),
                                    F("revIntf", o.revIntf))
    |
      Type.Extended     => RETURN M("Extended")
    |
      Type.LongReal     => RETURN M("LongReal")
    |
      Type.Real         => RETURN M("Real")
    |
      Type.Subrange(s)  => RETURN M("Subrange",
                                    F("base", Trans(s.base)),
                                    F("min", ValueTranslator.Translate(s.min)),
                                    F("max", ValueTranslator.Translate(s.max)))
    |
      Type.UserDefined(u)=>RETURN M("UserDefined",
                                    F("elts", TranslateAtomArray(u.elts^)))
    |
      Type.WideChar     => RETURN M("WideChar")
    |
      Type.Char         => RETURN M("Char")
    |
      Type.Enumeration  => RETURN M("Enumeration")
    |
      Type.Ordinal      => RETURN M("Ordinal")
    |
      Type.T            => RETURN M("T")
    END
  END Trans;

PROCEDURE TranslateQid(q : Qid) : SchemePair.T =
  BEGIN
    IF q = NIL THEN RETURN NIL END;
    RETURN MakeList(
               ARRAY OF SchemeObject.T {
    SchemeSymbol.FromText("Qid"),
    F("intf", q.intf),
    F("item", q.item) })
  END TranslateQid;

PROCEDURE TranslateFieldArray(READONLY f : ARRAY OF Field) : SchemeObject.T =
  VAR res : SchemePair.T := NIL;
  BEGIN 
    FOR i := LAST(f) TO FIRST(f) BY -1 DO
      res := NEW(SchemePair.T,
                 first := MakeList(ARRAY OF SchemeObject.T {
      SchemeSymbol.FromText("Field"),
      F("name",f[i].name),
      F("type",Trans(f[i].type)),
      F("default", ValueTranslator.Translate(f[i].default)) }),
                 rest := res)
    END;
    RETURN res
  END TranslateFieldArray;

PROCEDURE TranslateOverrideArray(READONLY f : ARRAY OF Override) : SchemeObject.T =
  VAR res : SchemePair.T := NIL;
  BEGIN 
    FOR i := LAST(f) TO FIRST(f) BY -1 DO
      res := NEW(SchemePair.T,
                 first := MakeList(ARRAY OF SchemeObject.T {
      SchemeSymbol.FromText("Override"),
      F("name",f[i].name),
      F("default", TranslateMethodDefault(f[i].default)) }),
                 rest := res)
    END;
    RETURN res
  END TranslateOverrideArray;

PROCEDURE TranslateMethodArray(READONLY f : ARRAY OF Method) : SchemeObject.T =
  VAR res : SchemePair.T := NIL;
  BEGIN 
    FOR i := LAST(f) TO FIRST(f) BY -1 DO
      res := NEW(SchemePair.T,
                 first := MakeList(ARRAY OF SchemeObject.T {
      SchemeSymbol.FromText("Method"),
      F("name",f[i].name),
      F("sig",TranslateSignature(f[i].sig)),
      F("default", TranslateMethodDefault(f[i].default)) }),
                 rest := res)
    END;
    RETURN res
  END TranslateMethodArray;

PROCEDURE TranslateMethodDefault(d : MethodDefault) : SchemeObject.T =
  BEGIN 
    TYPECASE d OF
      NULL => RETURN NIL
    |
      MethodDefault1(d1) =>
      RETURN MakeList( ARRAY OF SchemeObject.T {
      SchemeSymbol.FromText("MethodDefault1"),
      F("qid", TranslateQid(d1.qid)) })
    |
      MethodDefault2(d2) =>
      RETURN MakeList( ARRAY OF SchemeObject.T {
      SchemeSymbol.FromText("MethodDefault2"),
      F("obType", Trans(d2.obType)),
      F("method", d2.method) })
    ELSE
      <*ASSERT FALSE*>
    END
  END TranslateMethodDefault;

PROCEDURE TranslateAtomArray(READONLY f : ARRAY OF Atom.T) : SchemeObject.T =
  VAR res : SchemePair.T := NIL;
  BEGIN
    FOR i := LAST(f) TO FIRST(f) BY -1 DO
      res := NEW(SchemePair.T,
                 first := f[i],
                 rest := res)
    END;
    RETURN res
  END TranslateAtomArray;

PROCEDURE TranslateSignature(sig : Signature) : SchemeObject.T =
  BEGIN 
    RETURN MakeList( ARRAY OF SchemeObject.T {
    SchemeSymbol.FromText("Signature"),
    F("formals", TranslateFormalArray(sig.formals^)),
    F("result", Trans(sig.result)),
    F("raises", TranslateExceptionArray(sig.raises (* sic! *)) 
    (* careful here! -- distinguish between RAISES {} and RAISES ANY*)) })
  END TranslateSignature;

PROCEDURE TranslateFormalArray(READONLY f : ARRAY OF Formal) : SchemeObject.T=
  VAR res : SchemePair.T := NIL;
  BEGIN 
    FOR i := LAST(f) TO FIRST(f) BY -1 DO
      res := NEW(SchemePair.T,
                 first := MakeList(ARRAY OF SchemeObject.T {
      SchemeSymbol.FromText("Formal"),
      F("mode",TranslateMode(f[i].mode)),
      F("outOnly",B(f[i].outOnly)),
      F("name", f[i].name),
      F("type", Trans(f[i].type)),
      F("default", ValueTranslator.Translate(f[i].default)) }),
      rest := res)
    END;
    RETURN res
  END TranslateFormalArray;

PROCEDURE TranslateException(x : Exception) : SchemePair.T =
  BEGIN
    RETURN MakeList(ARRAY OF SchemeObject.T {
                       SchemeSymbol.FromText("Exception"),
                       F("qid",TranslateQid(x.qid)),
                       F("arg",Trans(x.arg)) })
  END TranslateException;

PROCEDURE TranslateExceptionArray(f : REF ARRAY OF Exception) : SchemeObject.T=
  VAR
    res : SchemePair.T := NIL;
  BEGIN
    IF f = NIL THEN RETURN NIL END;
    FOR i := LAST(f^) TO FIRST(f^) BY -1 DO
      res := NEW(SchemePair.T,
                 first := TranslateException(f[i]),
                 rest := res)
    END;
    RETURN List1(res)
  END TranslateExceptionArray;

PROCEDURE TranslateMode(m : Mode) : SchemeObject.T =
  BEGIN
    CASE m OF 
      Mode.Value => RETURN SchemeSymbol.FromText("Mode.Value")
    | Mode.Var => RETURN SchemeSymbol.FromText("Mode.Var")
    | Mode.Readonly => RETURN SchemeSymbol.FromText("Mode.Readonly")
    END
  END TranslateMode;

PROCEDURE AddProtos() =

  PROCEDURE Add(name : TEXT; what : SchemeObject.T) =
    BEGIN
      protoList := Cons(Cons(SchemeSymbol.FromText(name),
                             Translate(what)),
                        protoList);
    END Add;

  BEGIN
    (*
    Add("T",                    Translate(NEW(Type.T)));

    Add("Ordinal",              Translate(NEW(Type.Ordinal)));
    Add("Enumeration",              Translate(NEW(Type.Ordinal)));
    Add("Char",              Translate(NEW(Type.Ordinal)));
    Add("WideChar",              Translate(NEW(Type.Ordinal)));
    Add("UserDefined",              Translate(NEW(Type.Ordinal)));
    Add("Subrange",              Translate(NEW(Type.Ordinal)));
    Add("Real",               Translate(NEW(Type.Ordinal)));
    Add("LongReal",               Translate(NEW(Type.Ordinal)));
    Add("Extended",               Translate(NEW(Type.Ordinal)));
    Add("Reference",               Translate(NEW(Type.Ordinal)));
    Add("Ref",               Translate(NEW(Type.Ordinal)));
    Add("Opaque",               Translate(NEW(Type.Ordinal)));
    Add("Array",               Translate(NEW(Type.Ordinal)));

    Add("OpenArray",               Translate(NEW(Type.Ordinal)));
    Add("Packed",               Translate(NEW(Type.Ordinal)));
    Add("Record",               Translate(NEW(Type.Ordinal)));
    Add("Set",               Translate(NEW(Type.Ordinal)));
    Add("Procedure",               Translate(NEW(Type.Ordinal)));
    (************************************************************)

    Add("Field",               Translate(NEW(Type.Ordinal)));
    *)

  END AddProtos;

PROCEDURE AddBasetypes() =

  PROCEDURE Add(name : TEXT; what : Type.T) =
    BEGIN
      basetypeList := Cons(Cons(SchemeSymbol.FromText(name),
                                Translate(what)),
                           basetypeList);
    END Add;

  BEGIN
    Add("INTEGER",      Type.integer);
    Add("LONGINT",      Type.longint);
    Add("CARDINAL",     Type.cardinal);
    Add("BOOLEAN",      Type.boolean);
    Add("CHAR",         Type.char);
    Add("WIDECHAR",     Type.widechar);
    Add("REAL",         Type.real);
    Add("LONGREAL",     Type.longreal);
    Add("EXTENDED",     Type.extended);
    Add("REFANY",       Type.refany);
    Add("ADDRESS",      Type.address);
    Add("ROOT",         Type.root);

    Add("UNTRACEDROOT", Type.untracedRoot);
    (*
    Add("UNTRACED_ROOT",Type.untracedRoot);
    Add("UNTRACED ROOT",Type.untracedRoot);
    *)

    Add("NULL",         Type.null);
    Add("TEXT",         Type.text);
    Add("MUTEX",        Type.mutex)
  END AddBasetypes;

BEGIN
  AddProtos();
  AddBasetypes()
END TypeTranslator.

