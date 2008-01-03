(* Copyright (C) 1991, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Created by Susan Owicki                                     *)
(* Last modified on Mon Feb 28 17:01:36 PST 1994 by wobber     *)
(*      modified on Mon May 17 14:59:49 PDT 1993 by mjordan    *)
(*      modified on Thu Apr 22 10:32:22 PDT 1993 by owicki     *)

MODULE StubCode;

IMPORT Atom, CodeForType, FileWr, Formatter, SOxCodeFiles,
       OSError, ImportList, SOxCodeUtils,
       SOxCodeGenError, Type, Wr, UpdateMethodsTbl,
       AtomList, AtomAtomListTbl;

<* FATAL OSError.E, Wr.Failure *>

PROCEDURE BrandsOK(t: Type.T): BOOLEAN =
  VAR OKSoFar := TRUE;
  BEGIN
    IF t = NIL THEN RETURN TRUE END;    
    IF t.visited THEN RETURN t.brandsOK END;
    t.visited := TRUE;
    TYPECASE t OF
    | Type.Reference (ref) =>
      IF ref.branded AND ref.brand = NIL THEN
        SOxCodeUtils.Message("Error: Branded type with no explicit brand -- "
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


PROCEDURE GenCode(basename: TEXT;
                  t_array: REF ARRAY OF Type.Object; 
                  qid_array: REF ARRAY OF Type.Qid;
                  meth_array: REF ARRAY OF ImportList.MethodList;
                  umethodsTbl: UpdateMethodsTbl.T) =
    VAR 
      wr: Formatter.T; 
      t : Type.Qid;
      o : Type.Object;
      m : ImportList.MethodList;
      fileName, basefname : TEXT;
      umethods : AtomList.T;
      umTbl : AtomAtomListTbl.T;
      imports: ImportList.T;
    BEGIN
      TRY
        FOR ftyp := FIRST(SOxCodeFiles.T) TO LAST(SOxCodeFiles.T) DO 
          IF SOxCodeUtils.DoFile(ftyp) THEN
            basefname := SOxCodeUtils.FileName(basename, ftyp);
            fileName := basefname & SOxCodeUtils.FileExtension(ftyp); 
            wr := Formatter.New(FileWr.Open(fileName));

            imports := ImportList.New();
            SOxCodeFiles.coderTable[ftyp].InitImports(basename, imports);
            FOR i := 0 TO LAST(qid_array^) DO
              t := qid_array[i];
              o := t_array[i];
              m := meth_array[i];
              IF t # NIL THEN
                EVAL umethodsTbl.get(t.intf, umTbl);
                EVAL umTbl.get(t.item, umethods);
                SOxCodeFiles.coderTable[ftyp].Import(o, m, umethods, imports);
                (* CodeForType.ImportRevelations(t, imports); *)
              END;
            END;

            SOxCodeFiles.coderTable[ftyp].Head(wr, fileName, 
                                               basename, imports);
            FOR i := 0 TO LAST(qid_array^) DO
              t := qid_array[i]; 
              IF t # NIL THEN
                EVAL umethodsTbl.get(t.intf, umTbl);
                EVAL umTbl.get(t.item, umethods);
                SOxCodeFiles.coderTable[ftyp].Decls(wr, t,
                                                    t_array[i].name,
                                                    basename,
                                                    meth_array[i],
                                                    umethods); 
              END;
            END;

            FOR i := 0 TO LAST(qid_array^) DO
              IF t#NIL THEN
                t := qid_array[i]; 
                o := t_array[i];
                EVAL umethodsTbl.get(t.intf, umTbl);
                EVAL umTbl.get(t.item, umethods);
                SOxCodeFiles.coderTable[ftyp].Main(wr, t, o,
                                                   t_array[i].name,
                                                   basename,
                                                   meth_array[i],
                                                   umethods);
              END;
            END;

            SOxCodeFiles.coderTable[ftyp].Bottom(wr, basename);
            Formatter.Close(wr);
          END;
        END;          

        (* CodeForType.AddModuleImports(imports, methods,
           lastNewMethod); *)

      EXCEPT
        SOxCodeGenError.E => 
          Formatter.Close(wr);
      END;
    END GenCode;

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
    IF NUMBER(m.formals^) <= 0 THEN
      sig.raises := m.raises;
    ELSIF m.raises = NIL THEN
      sig.raises := stubExceptions;
    ELSE
      useException := NEW(REF ARRAY OF BOOLEAN, NUMBER(m.raises^));
      FOR i := 0 TO LAST(m.raises^) DO
        useException[i] := FALSE;
        found := FALSE;
        FOR j := 0 TO LAST(stubExceptions^) DO
          IF m.raises[i].qid.intf = stubExceptions[j].qid.intf AND
             m.raises[i].qid.item = stubExceptions[j].qid.item THEN
             found := TRUE
          END;
        END;
        IF NOT found THEN 
          useException[i] := TRUE;
          INC(c);
        END;
      END;
      sig.raises := NEW(REF ARRAY OF Type.Exception,
                         NUMBER(stubExceptions^) + c);
      FOR i := 0 TO LAST(stubExceptions^) DO
        sig.raises[i] := stubExceptions[i]
      END;
      c := NUMBER(stubExceptions^);
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
    stubExceptions: REF ARRAY OF Type.Exception;

BEGIN
  stubExceptions := NEW (REF ARRAY OF Type.Exception, 3);
  FOR i := 0 TO LAST(stubExceptions^) DO
    stubExceptions[i] := NEW(Type.Exception);
  END;
  stubExceptions[0].qid := NEW(Type.Qid, intf := Atom.FromText("SharedObj"), 
                               item := Atom.FromText("Error"));
  stubExceptions[1].qid := NEW(Type.Qid, intf := Atom.FromText("Rd"), 
                               item := Atom.FromText("Failure"));
  stubExceptions[2].qid := NEW(Type.Qid, intf := Atom.FromText("Thread"), 
                               item := Atom.FromText("Alerted"));
  stubFormals := NEW(REF ARRAY OF Type.Formal, 1);
  stubFormalPrags := NEW(REF ARRAY OF TEXT, 1);
  stubFormals[0] := NEW(Type.Formal, mode := Type.Mode.Value, 
                        name := Atom.FromText("in"),
                        default := NIL,
                        type := NEW(Type.Object, 
                            name := NEW(Type.Qid, 
                                        intf:= Atom.FromText("EventStubLib"),
                                        item := Atom.FromText("Handle"))));
  stubFormalPrags[0] := "<* NOWARN *> ";
  (*
  stubFormals[1] := NEW(Type.Formal, mode := Type.Mode.Value, 
                        name := Atom.FromText("rep"),
                        default := NIL,
                        type := NEW(Type.Object, 
                            name := NEW(Type.Qid, 
                                        intf:= Atom.FromText("StubLib"),
                                        item := Atom.FromText("DataRep"))));
  stubFormalPrags[0] := "";
  *)
END StubCode.
