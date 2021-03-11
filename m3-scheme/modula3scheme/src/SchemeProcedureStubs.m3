(* $Id$ *)

MODULE SchemeProcedureStubs;

IMPORT Scheme;
IMPORT SchemeProcedure;
FROM Scheme IMPORT E, Object;
FROM SchemeUtils IMPORT Cons, First, Second, Third, Rest, Stringify;
IMPORT SchemePrimitive;
IMPORT SchemePair;
IMPORT Debug;
IMPORT RT0;
IMPORT CardRefTbl, Atom, SchemeObject, SchemeLongReal, Fmt;
IMPORT AtomCardTbl;
IMPORT SchemeBoolean;
IMPORT Pickle, SchemeVector, TextRd, TextWr, Text;
IMPORT Thread, Rd, Wr;
IMPORT AtomRefTbl, IntRefTbl;
IMPORT SchemeSymbol;
IMPORT RuntimeError;

TYPE
  T = OBJECT
    name : Qid;
    next : T;
  END;

  Rec = T OBJECT
    proc : P;
  END;

VAR rec : Rec := NIL;

PROCEDURE Register(name : Qid; p : P) =
  BEGIN
    rec := NEW(Rec, name := name, proc := p, next := rec)
  END Register;

PROCEDURE ListStubApply(<*UNUSED*>proc : SchemeProcedure.T; 
                        <*UNUSED*>interpA : Scheme.T; 
                        <*UNUSED*>args : Object) : Object =
  VAR
    res : SchemePair.T := NIL;
    p := rec;
  BEGIN
    WHILE p # NIL DO
      res := Cons(Cons(p.name.intf,p.name.item),res);
      p := p.next
    END;
    RETURN res
  END ListStubApply;

PROCEDURE CallStubApply(<*UNUSED*>proc : SchemeProcedure.T; 
                        interpA : Scheme.T; 
                        args : Object) : Object RAISES { E } =
  VAR 
    n := ConditionallyConvertSymbolToPair(First(args));

    intf := First(n);
    item := Rest(n);

    a := Second(args);

    x := Third(args);
    p := rec;
  BEGIN
    WHILE p # NIL DO
      IF intf = p.name.intf AND item = p.name.item THEN
        RETURN p.proc(interpA, a, x)
      END;
      p := p.next
    END;
    RAISE E("SchemeProcedureStubs.CallStubApply : cant find " & Stringify(n))
  END CallStubApply;

(**********************************************************************)

TYPE
  NewRec = T OBJECT
    newProc : NewProc;
  END;

VAR
  newMu := NEW(MUTEX);
  new : NewRec := NIL;

PROCEDURE RegisterNew(typeName : Qid; newProc : NewProc) =
  BEGIN
    LOCK newMu DO
      new := NEW(NewRec, name := typeName, newProc := newProc, next := new)
    END
  END RegisterNew;

PROCEDURE NewModulaApply(<*UNUSED*>proc : SchemeProcedure.T; 
                         interp : Scheme.T; 
                         args : Object) : Object RAISES { E } =
  VAR
    n := ConditionallyConvertSymbolToPair(First(args));

    intf := First(n);
    item := Rest(n);

    p := new;
  BEGIN
    WHILE p # NIL DO
      IF intf = p.name.intf AND item = p.name.item THEN
        RETURN p.newProc(interp, Rest(args))
      END;
      p := p.next
    END;
    RAISE E("SchemeProcedureStubs.NewModulaApply : cant find " & Stringify(n))
  END NewModulaApply;

PROCEDURE NewableTypesApply(<*UNUSED*>proc : SchemeProcedure.T; 
                        <*UNUSED*>interpA : Scheme.T; 
                        <*UNUSED*>args : Object) : Object =
  VAR
    res : SchemePair.T := NIL;
    p := new;
  BEGIN
    WHILE p # NIL DO
      res := Cons(Cons(p.name.intf,p.name.item),res);
      p := p.next
    END;
    RETURN res
  END NewableTypesApply;

(**********************************************************************)

PROCEDURE Extend(prims : SchemePrimitive.ExtDefiner) : SchemePrimitive.ExtDefiner =
  BEGIN
    prims.addPrim("scheme-procedure-stubs-call",
                  NEW(SchemeProcedure.T, apply := CallStubApply),
                  2, 3);
    prims.addPrim("scheme-procedure-stubs-list",
                  NEW(SchemeProcedure.T, apply := ListStubApply),
                  0, 0);

    prims.addPrim("new-modula-object",
                  NEW(SchemeProcedure.T, apply := NewModulaApply),
                  1, LAST(CARDINAL));
    prims.addPrim("newable-types",
                  NEW(SchemeProcedure.T, apply := NewableTypesApply),
                  0, 0);

    prims.addPrim("modula-type-op",
                  NEW(SchemeProcedure.T, apply := ModulaTypeOpApply),
                  3, LAST(CARDINAL));

    prims.addPrim("list-modula-type-ops",
                  NEW(SchemeProcedure.T, apply := ListTypeOpsApply),
                  1, 1);

    prims.addPrim("typename->typecode",
                  NEW(SchemeProcedure.T, apply := LookupTCApply),
                  1, 1);

    prims.addPrim("list-modula-types",
                  NEW(SchemeProcedure.T, apply := ListTypesApply),
                  0, 0);

    prims.addPrim("encode-pickle-vector",
                  NEW(SchemeProcedure.T, apply := WritePickleApply),
                  1, 1);
    prims.addPrim("decode-pickle-vector",
                  NEW(SchemeProcedure.T, apply := ReadPickleApply),
                  1, 1);

    prims.addPrim("modula-type-class",
                  NEW(SchemeProcedure.T, apply := ModulaTypeClassApply),
                  1, 1);

    RETURN prims
  END Extend;

PROCEDURE CallScheme(interp : Scheme.T;
                     proc : Object; 
                     args : Object) : Object =
  BEGIN
    TRY
      WITH p = SchemeProcedure.Proc(proc) DO
        RETURN p.apply(interp,args)
      END
    EXCEPT
      Scheme.E(x) =>

      TRY
        Debug.Warning("SchemeProcedureStubs.CallScheme: caught Scheme.E("&x
        &") calling: " & Stringify(proc) & ", with args : " & Stringify(args))
      EXCEPT ELSE END;

      RETURN NIL
    END
  END CallScheme;

VAR opMu := NEW(MUTEX);
VAR op := NEW(CardRefTbl.Default).init();

TYPE 
  OpRec = OBJECT
    nam : Atom.T;
    proc : OpProc;
    next : OpRec := NIL;
  END;

PROCEDURE RegisterOp(typeId : RT0.Typecode;
                     nameT : TEXT;
                     opProc : OpProc) =
  VAR 
    name := Atom.FromText(nameT);
    ops : OpRec := NIL;
  BEGIN
    LOCK opMu DO
      VAR r : REFANY; BEGIN
        EVAL op.get(typeId, r);  ops := r
      END;

      ops := NEW(OpRec, nam := name, proc := opProc, next := ops);
      EVAL op.put(typeId, ops)
    END
  END RegisterOp;

PROCEDURE GetRest(lst : SchemeObject.T; skip : CARDINAL) : SchemeObject.T 
  RAISES { Scheme.E } =
  VAR
    i := 0;
  BEGIN
    LOOP
      IF i = skip THEN RETURN lst END;
      WITH res = SchemePair.Pair(lst) DO
        IF res = NIL THEN RETURN NIL END;
        lst := res.rest
      END;
      INC(i)
    END
  END GetRest;

PROCEDURE ModulaTypeOpApply(<*UNUSED*>proc : SchemeProcedure.T; 
                            interp : Scheme.T; 
                            args : Object) : Object RAISES { E } =
  (* modula-type-op <typecode> <name> <object> <args> *)
  VAR
    tci   : INTEGER;
    name := Scheme.SymbolCheck(Second(args));
    obj  := Third(args);
    rest := GetRest(args, 3);

    r : REFANY := NIL;
    ops : OpRec;
  BEGIN
    
    IF ISTYPE(First(args),SchemeLongReal.T) THEN
      tci := SchemeLongReal.Int(First(args))
    ELSE
      (* allow type to be symbol too *)
      VAR tca : CARDINAL; BEGIN
        EVAL tc.get(Scheme.SymbolCheck(First(args)), tca);
        tci := tca
      END
    END;

    EVAL op.get(tci, r);

    ops := r;
    
    WHILE ops # NIL DO
      IF ops.nam = name THEN
        IF mapRuntimeErrors THEN
          TRY
            RETURN ops.proc(interp, obj, rest)
          EXCEPT
            <*NOWARN*>RuntimeError.E(err) =>
            RAISE E("EXCEPTION! RuntimeError! calling out to Modula-3 code, op=" & SchemeSymbol.ToText(name) & " " &  RuntimeError.Tag(err) & "\n")
          END
        ELSE
          RETURN ops.proc(interp, obj, rest)
        END
      END;
      ops := ops.next
    END;
    RAISE Scheme.E("Unknown op " & Atom.ToText(name) & " for tc " & Fmt.Int(tci))
  END ModulaTypeOpApply;

VAR tcMu := NEW(MUTEX);
    tc := NEW(AtomCardTbl.Default).init();

PROCEDURE RegisterTC(tca : RT0.Typecode; name : TEXT) =
  BEGIN  
    LOCK tcMu DO
      EVAL tc.put(Atom.FromText(name),tca)
    END
  END RegisterTC;

PROCEDURE LookupTCApply(<*UNUSED*>proc : SchemeProcedure.T; 
                        <*UNUSED*>interp : Scheme.T; 
                        args : Object) : Object RAISES { E } =
  VAR
    nam := Scheme.SymbolCheck(First(args));
    tca : CARDINAL;
  BEGIN
    IF NOT tc.get(nam, tca) THEN 
      RETURN SchemeBoolean.False() 
    ELSE
      RETURN SchemeLongReal.FromI(tca)
    END
  END LookupTCApply;

PROCEDURE ListTypesApply(<*UNUSED*>proc : SchemeProcedure.T; 
                        <*UNUSED*>interp : Scheme.T; 
                        <*UNUSED*>args : Object) : Object =
  VAR
    res : SchemePair.T := NIL;
    iter := tc.iterate();
    tca : CARDINAL;
    a : Atom.T;
  BEGIN
    WHILE iter.next(a, tca) DO
      res := Cons(Cons(a, SchemeLongReal.FromI(tca)), res)
    END;
    RETURN res
  END ListTypesApply;

PROCEDURE ListTypeOpsApply(<*UNUSED*>proc : SchemeProcedure.T; 
                           <*UNUSED*>interp : Scheme.T; 
                            args : Object) : Object RAISES { E } =
  (* list-type-ops <typecode> *)
  VAR
    tc   := SchemeLongReal.Int(First(args));
    r : REFANY;
    lst : OpRec;
    res : SchemePair.T := NIL;
  BEGIN
    EVAL op.get(tc,r);
    lst := r;
    
    WHILE lst # NIL DO
      res := Cons(lst.nam, res);
      lst := lst.next
    END;
    RETURN res
  END ListTypeOpsApply;

(**********************************************************************)

PROCEDURE WritePickleApply(<*UNUSED*>proc : SchemeProcedure.T; 
                           <*UNUSED*>interp : Scheme.T; 
                           args : Object) : Object RAISES { E } =
  <*FATAL Thread.Alerted, Wr.Failure *>
  VAR
    wr := NEW(TextWr.T).init();
  BEGIN
    TRY
      Pickle.Write(wr, First(args));
    EXCEPT
      Pickle.Error(e) => RAISE E("Pickle error : " & e)
    END;
    WITH txt = TextWr.ToText(wr),
         vec = NEW(SchemeVector.T, Text.Length(txt)) DO
      FOR i := 0 TO Text.Length(txt)-1 DO
        vec[i] := SchemeLongReal.FromI(ORD(Text.GetChar(txt,i)))
      END;
      RETURN vec
    END
  END WritePickleApply;

PROCEDURE ReadPickleApply(<*UNUSED*>proc : SchemeProcedure.T; 
                           <*UNUSED*>interp : Scheme.T; 
                           args : Object) : Object RAISES { E } =
  <*FATAL Thread.Alerted, Rd.Failure, Rd.EndOfFile *>
  VAR
    vec := Scheme.VectorCheck(First(args));
    chars := NEW(REF ARRAY OF CHAR, NUMBER(vec^));
  BEGIN
    FOR i := FIRST(chars^) TO LAST(chars^) DO
      WITH c = SchemeLongReal.Int(vec[i]) DO
        IF c < ORD(FIRST(CHAR)) OR c > ORD(LAST(CHAR)) THEN
          RAISE E("Vector entry out of range : " & Stringify(First(args)))
        END;
        chars[i] := VAL(c,CHAR)
      END
    END;

    TRY
      RETURN NEW(Pickle.Reader, 
                 rd := NEW(TextRd.T).init(Text.FromChars(chars^))).read()
    EXCEPT
      Pickle.Error(e) => RAISE E("Pickle error : " & e)
    END
  END ReadPickleApply;

VAR typesByName := NEW(AtomRefTbl.Default).init();
    typesByTC   := NEW(IntRefTbl.Default).init();

PROCEDURE RegisterTypePickle(READONLY typeCodes : ARRAY OF [-1..LAST(RT0.Typecode)];
                             READONLY names : ARRAY OF TEXT;
                             READONLY pickle : ARRAY OF CHAR) =

  PROCEDURE Error(msg : TEXT) =
    BEGIN
      Debug.Error("SchemeProcedureStubs.RegisterTypePickle : couldnt initialize MScheme runtime type stubs : " & msg)
    END Error;

  VAR
    theTypes : SchemePair.T ;
  BEGIN
    TRY
      theTypes := Pickle.Read(NEW(TextRd.T).init(Text.FromChars(pickle)));

      IF NUMBER(typeCodes) # NUMBER(names) THEN Error("type count mismatch") END;
      FOR i := FIRST(names) TO LAST(names) DO
        EVAL typesByName.put(Atom.FromText(names[i]),theTypes.first);
        EVAL typesByTC.put(typeCodes[i],theTypes.first);
        theTypes := theTypes.rest
      END;
      IF theTypes # NIL THEN Error("too many types") END;
    EXCEPT
      Rd.EndOfFile, Rd.Failure, Thread.Alerted, Pickle.Error =>
      Error("pickle problem")
    END
  END RegisterTypePickle;

PROCEDURE ModulaTypeClassApply(<*UNUSED*>proc : SchemeProcedure.T; 
                           <*UNUSED*>interp : Scheme.T; 
                           args : Object) : Object RAISES { E } =
  VAR
    type := First(args);
    res : REFANY := SchemeBoolean.False();
  BEGIN
    TYPECASE type OF
      SchemeSymbol.T(sym) =>
      EVAL typesByName.get(sym,res)
    |
      SchemeLongReal.T(lr) =>
      EVAL typesByTC.get(SchemeLongReal.Card(lr),res)
    ELSE
      RAISE E("expected symbol or integer : " & Stringify(type))
    END;
    RETURN res
  END ModulaTypeClassApply;

PROCEDURE ConditionallyConvertSymbolToPair(obj : Object) : SchemePair.T 
  RAISES { E } =
  BEGIN
    TYPECASE obj OF 
      SchemePair.T(p) => RETURN p
    |
      SchemeSymbol.T(sym) =>
      WITH str = SchemeSymbol.ToText(sym),
           idx = Text.FindChar(str, '.') DO
        IF idx > 0 THEN
          RETURN NEW(SchemePair.T,
                     first := SchemeSymbol.FromText(Text.Sub(str,0,idx)),
                     rest  := SchemeSymbol.FromText(Text.Sub(str,idx+1)))
        ELSE
          RAISE E("No dot in symbol : \"" & str & "\"")
        END
      END
    ELSE
      RAISE E("Can't convert to symbol-pair : " & Stringify(obj))
    END
  END ConditionallyConvertSymbolToPair;

VAR mapRuntimeErrors := TRUE;

PROCEDURE GetMapRuntimeErrors() : BOOLEAN = 
  BEGIN RETURN mapRuntimeErrors END GetMapRuntimeErrors;

PROCEDURE SetMapRuntimeErrors(to : BOOLEAN) =
  BEGIN mapRuntimeErrors := to END SetMapRuntimeErrors;

BEGIN END SchemeProcedureStubs.
