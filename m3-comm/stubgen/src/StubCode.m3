(* Copyright (C) 1991, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Created by Susan Owicki                                     *)
(* Last modified on Mon Feb 28 17:01:36 PST 1994 by wobber     *)
(*      modified on Mon May 17 14:59:49 PDT 1993 by mjordan    *)
(*      modified on Thu Apr 22 10:32:22 PDT 1993 by owicki     *)

MODULE StubCode;

IMPORT Atom, CodeForType, FileWr, Fmt, Formatter, IntfStubCode, RefList,
       ModuleStubCode, OSError, Protocol, AtomRefTbl, StubUtils, Type, Wr;

<* FATAL OSError.E, Wr.Failure *>

VAR netobjName:= NEW(Type.Qid);

PROCEDURE BrandsOK(t: Type.T;
                    <*UNUSED*> existingSuper: Type.T := NIL): BOOLEAN =
  VAR OKSoFar := TRUE;
  BEGIN
    IF t = NIL THEN RETURN TRUE END;    
    IF t.visited THEN RETURN t.brandsOK END;
    t.visited := TRUE;
    TYPECASE t OF
    | Type.Reference (ref) =>
      IF ref.branded AND ref.brand = NIL THEN
        StubUtils.Message("Error: Branded type with no explicit brand -- "
          & CodeForType.ToText(ref));
        OKSoFar := FALSE;
      END;
      TYPECASE ref OF
      | Type.Ref (r) => t.brandsOK := BrandsOK(r.target) AND OKSoFar
      | Type.Object (obj) => 
          OKSoFar := BrandsOK(obj.super) AND OKSoFar;
          FOR i := 0 TO LAST(obj.fields^) DO
            OKSoFar := BrandsOK(obj.fields[i].type) AND OKSoFar;
          END;
          FOR i := 0 TO LAST(obj.methods^) DO
            OKSoFar := SigBrandsOK(obj.methods[i].sig) AND OKSoFar;
          END;
          t.brandsOK := OKSoFar
      | Type.Opaque (opq) => 
          t.brandsOK := BrandsOK(opq.revealedSuperType) AND OKSoFar
      ELSE t.brandsOK := OKSoFar
      END;
    | Type.Array (arr) => t.brandsOK := BrandsOK(arr.element) AND OKSoFar
    | Type.Packed (p) => t.brandsOK := BrandsOK(p.base)AND OKSoFar
    | Type.Record (rec) =>
        FOR i := 0 TO LAST(rec.fields^) DO
          OKSoFar := BrandsOK(rec.fields[i].type) AND OKSoFar;
        END;
        t.brandsOK := OKSoFar;
    | Type.Procedure (proc) =>
        t.brandsOK := SigBrandsOK(proc.sig);
    ELSE t.brandsOK := TRUE
    END;
    RETURN t.brandsOK;
  END BrandsOK;

PROCEDURE SigBrandsOK(sig: Type.Signature): BOOLEAN =
  VAR OKSoFar := TRUE;
  BEGIN
    FOR i := 0 TO LAST(sig.formals^) DO
      OKSoFar := BrandsOK(sig.formals[i].type) AND OKSoFar
    END;
    RETURN BrandsOK(sig.result) AND OKSoFar;
  END SigBrandsOK;


  PROCEDURE GenStub(t: Type.Object; 
                    objName: Type.Qid;
                    existingSuper: Type.T := NIL;
                    existingSuperName: Type.Qid := NIL) =
    VAR methods: MethodList;
        intWr, modWr: Formatter.T;
        typeName := StubName(objName);
        fileName := StubUtils.FileName(typeName);
        superName: Atom.T;
        imports := NEW(AtomRefTbl.Default).init();
        returnCodes: RefList.T;
        lastNewMethod: INTEGER;
    BEGIN
      IF existingSuper # NIL THEN 
        superName  := StubName(existingSuperName);
      END;
      TRY
        Initialize(t, intWr, modWr, typeName, fileName, methods, 
                   lastNewMethod, imports, existingSuper, superName);
        IntfStubCode.Header(t, intWr, typeName, objName, methods, 
                            lastNewMethod, imports);
        CodeForType.AddModuleImports(imports, methods, lastNewMethod);
        ModuleStubCode.Header(modWr, t, typeName, objName, methods, 
                              lastNewMethod, returnCodes, imports);
        Body(t, modWr, typeName, methods, lastNewMethod, returnCodes);
        CloseUp(intWr, modWr, fileName);
      EXCEPT
        StubUtils.Failure => 
          Formatter.Close(intWr);
          Formatter.Close(modWr)
      END;
    END GenStub;

  PROCEDURE StubName(objName: Type.Qid): Atom.T =
    BEGIN
      RETURN Atom.FromText(Atom.ToText(objName.intf) & "_" & Atom.ToText(objName.item));
    END StubName;

  PROCEDURE Initialize(t: Type.Object; VAR intWr, modWr: Formatter.T;
                       typeName: Atom.T;
                       fileName: TEXT;
                       VAR methods: MethodList;
                       VAR lastNewMethod: INTEGER;
                       imports: AtomRefTbl.T; 
                       existingSuper: Type.T;
                       superName: Atom.T )=
    BEGIN
      intWr := Formatter.New(FileWr.Open(fileName & ".i3"));
      modWr := Formatter.New(FileWr.Open(fileName & ".m3"));
      lastNewMethod := -1;
      BuildMethods(t, typeName, methods, lastNewMethod, 
                        existingSuper, superName, 0);
      CodeForType.ImportList(t, imports, methods, lastNewMethod,
                             byName := FALSE);
      (* CodeForType.ImportRevelations(t, imports); *)
      CodeForType.AugmentImportList(imports, netObjImports);
    END Initialize;

  VAR netObjImports:= ARRAY {NetObj, StubLib, Rd, Thread, Wr} 
                        OF Atom.T
                     {Atom.FromText("NetObj"),
                      Atom.FromText("StubLib"), Atom.FromText("Rd"), 
                      Atom.FromText("Thread"), Atom.FromText("Wr")};

  PROCEDURE BuildMethods(t: Type.Reference; 
                         typeName: Atom.T;
                         VAR methods: MethodList;
                         VAR lastNewMethod: INTEGER;
                         existingSuper: Type.T;
                         superName: Atom.T;
                         n: INTEGER) =
    VAR debug: TEXT;
    BEGIN
      IF t.name # NIL AND t.name.intf = netobjName.intf 
          AND t.name.item = netobjName.item THEN
        methods := NEW(MethodList, n);
        IF lastNewMethod < 0 THEN lastNewMethod := n-1; END;
      ELSE
        TYPECASE t OF
        | Type.Object (ot) => 
          IF ot = existingSuper THEN lastNewMethod := n-1; END;
          BuildMethods(ot.super, typeName, methods, lastNewMethod,
                            existingSuper, superName, n + NUMBER(ot.methods^));
          FOR i := 0 TO LAST(ot.methods^) DO
            debug := Atom.ToText(ot.name.intf);
            methods[n].name := ot.methods[i].name;
            IF n <= lastNewMethod THEN
              methods[n].intf := typeName;
            ELSE
              methods[n].intf := superName;
            END;
            methods[n].sig := ot.methods[i].sig;
            INC(n);
          END;
        ELSE
          StubUtils.Die("StubCode.BuildMethods: non-object has methods");
        END;
      END;
    END BuildMethods;


  PROCEDURE Body(t: Type.Object; 
                 modWr: Formatter.T; 
                 typeName: Atom.T;
                 methods: MethodList;
                 lastNewMethod: INTEGER;
                 returnCodes: RefList.T) RAISES {StubUtils.Failure}  = 
    BEGIN
     ModuleStubCode.Surrogates(modWr, t, methods, lastNewMethod);
     ModuleStubCode.Dispatcher(modWr, t, typeName, methods, 
                               returnCodes);
     ModuleStubCode.OwnerStubs(modWr, t, methods, lastNewMethod);
     Formatter.PutText(modWr, "BEGIN");
     Formatter.PutText(modWr, Wr.EOL);
     Formatter.PutText(modWr, "  StubLib.Register(TYPECODE(");
     Formatter.PutText(modWr, 
                CodeForType.ToText(t) & "), " &
                Fmt.Int(Protocol.version) & ", " &
                "TYPECODE(Surrogate_" & Atom.ToText(typeName));
     Formatter.PutText(modWr, "), Invoke);");
     Formatter.PutText(modWr, Wr.EOL);

    END Body;

PROCEDURE CloseUp(intWr, modWr: Formatter.T; fileName: TEXT) = 
  BEGIN
    Formatter.PutText(intWr, "END " & fileName & ".");
    Formatter.PutText(modWr, "END " & fileName & ".");
    Formatter.NewLine(intWr);
    Formatter.NewLine(modWr);
    Formatter.Close(intWr);
    Formatter.Close(modWr);
  END CloseUp;

PROCEDURE PragmasForStub(): REF ARRAY OF TEXT =
  BEGIN
    RETURN stubFormalPrags;
  END PragmasForStub;

PROCEDURE SigForStub(m: Type.Signature): Type.Signature =
  VAR sig:= Type.Signature{formals := stubFormals, result := NIL,
                           raises := NIL};
      useException: REF ARRAY OF BOOLEAN;
      c := 0;
      found: BOOLEAN;
  BEGIN
    IF m.raises = NIL THEN
      sig.raises := netObjExceptions;
    ELSE
      useException := NEW(REF ARRAY OF BOOLEAN, NUMBER(m.raises^));
      FOR i := 0 TO LAST(m.raises^) DO
        useException[i] := FALSE;
        found := FALSE;
        FOR j := 0 TO LAST(netObjExceptions^) DO
          IF m.raises[i].qid.intf = netObjExceptions[j].qid.intf AND
             m.raises[i].qid.item = netObjExceptions[j].qid.item THEN
             found := TRUE
          END;
        END;
        IF NOT found THEN 
          useException[i] := TRUE;
          INC(c);
        END;
      END;
      sig.raises := NEW(REF ARRAY OF Type.Exception,
                         NUMBER(netObjExceptions^) + c);
      FOR i := 0 TO LAST(netObjExceptions^) DO
        sig.raises[i] := netObjExceptions[i]
      END;
      c := NUMBER(netObjExceptions^);
      FOR i := 0 TO LAST(m.raises^) DO
        IF useException[i] THEN 
          sig.raises[c] := m.raises[i];
          INC(c)
        END;
      END;
    END;
    RETURN sig;
  END SigForStub;

VAR stubFormals: REF ARRAY OF Type.Formal;
    stubFormalPrags: REF ARRAY OF TEXT;
    netObjExceptions: REF ARRAY OF Type.Exception;

BEGIN
  netobjName.intf := Atom.FromText("NetObj");
  netobjName.item := Atom.FromText("T");
  netObjExceptions := NEW (REF ARRAY OF Type.Exception, 4);
  FOR i := 0 TO LAST(netObjExceptions^) DO
    netObjExceptions[i] := NEW(Type.Exception);
  END;
  netObjExceptions[0].qid := 
      NEW(Type.Qid, intf := Atom.FromText("NetObj"), item := Atom.FromText("Error"));
  (* The arg field for NetObj.Error is not filled in; this field isn't
     needed, since NetObj.Error is not marshalled or unmarshalled by
     stubs *)
  netObjExceptions[1].qid := 
      NEW(Type.Qid, intf := Atom.FromText("Rd"), item := Atom.FromText("Failure"));
  netObjExceptions[2].qid := 
      NEW(Type.Qid, intf := Atom.FromText("Wr"), item := Atom.FromText("Failure"));
  netObjExceptions[3].qid := 
      NEW(Type.Qid, intf := Atom.FromText("Thread"), item := Atom.FromText("Alerted"));
  stubFormals := NEW(REF ARRAY OF Type.Formal, 2);
  stubFormalPrags := NEW(REF ARRAY OF TEXT, 2);
  stubFormals[0] := NEW(Type.Formal, mode := Type.Mode.Value, 
                        name := Atom.FromText("c"),
                        default := NIL,
                        type := NEW(Type.Object, 
                            name := NEW(Type.Qid, 
                                        intf:= Atom.FromText("StubLib"),
                                        item := Atom.FromText("Conn"))));
  stubFormalPrags[0] := "";
  stubFormals[1] := NEW(Type.Formal, mode := Type.Mode.Value, 
                        name := Atom.FromText("rep"),
                        default := NIL,
                        type := NEW(Type.Object, 
                            name := NEW(Type.Qid, 
                                        intf:= Atom.FromText("StubLib"),
                                        item := Atom.FromText("DataRep"))));
  stubFormalPrags[1] := "<* NOWARN *> ";
    
END StubCode.
