(* Copyright (C) 1991, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(* Created by Susan Owicki, rewritten by Carsten Weich               *)
(* Last modified on Tue Jan  3 17:12:15 PST 1995 by chaiken    *)
(*      modified on Wed Sep 28 11:25:05 PDT 1994 by weich      *)

(* This module mainly contains main procedures to generate Modula-3
   code and checking procedures.

   "OneStub()" produces the implementation of a stable subtype of its
   parameter type.

   "CheckStableObj()" checks for validy for stable subtyping.

   "CheckBrands()" checks for explicit brands and prints warnings if
   where not present. 
*)
MODULE GenCode;

IMPORT Atom, AtomList, Thread, Lex, Wr, Rd, FileWr,
       TextRd, Formatter, OSError;
IMPORT ImportList, GenTypeCode, AstToType, GenModuleCode,
       StablegenError, Type;

(* Imports needed for the toolkit-stuff in "OneStub()" *)
IMPORT ASTWalk, M3AST_AS, M3ASTScope, M3CId, M3CPragma;
IMPORT M3AST_AS_F, M3AST_SM_F, M3AST_TM_F,
       M3AST_TL_F, M3Context, M3CConcTypeSpec, M3CUnit;


(* \subsection{List of additional Imports}
   The (readonly) variable "implImports" contains all interface
   names necessary for the code beeing produced by the generator.
   The elements of this array must be compatible with "ImportList.T"
   elements.
*)
VAR
  implImports := ARRAY [0 .. 2] OF
                 Atom.T{Atom.FromText("StableError"),
                        Atom.FromText("StableLog"),
                        Atom.FromText("Rd")};

(* \subsection{Procedure Do}
   Main control procedure that takes one typename and produces the
   implementation of a stable subtype.

   \paragraph{Parameters}
   "c" is the complete compilation context. "qid" is the name of the
   type (interface name and "T") from which a stable subtype shall be
   produced. "implName" and "repName" are the names of the
   generated module and the instantiated "StableRep" resp.

   \paragraph{Algorithm}
   First get the compilation unit AST of "qid"
   ("M3Context.FindExact()").  Then we look for pragmas starting with
   the keyword "STABLE". It is interpreted by "ParsePragma()" (see
   below) which generates a list of updatemethods (they will be
   checked later in "BuildMethods()". The call to
   "M3CConcTypeSpec.SetCurrentReveal()" set the "cu_reveal" id to the
   compilation unit containing the most specific revealation of "qid"
   (it must be given by the user from the command line, see
   "StablegenArgs").  Then the definition of "qid" is looked up
   ("M3ASTScope.Lookup()"). It is checked to be an identifier of a
   type definition (in the typecase stmt).  We use
   "AstToType.Convert()" to convert the type specification to a more
   handy "Type.T". The "Type.T" is handled in "TypeDo()" (see there).
*)
PROCEDURE Do (c                        : M3Context.T;
              qid                      : Type.Qid;
              reveal, implName, repName: TEXT         )
  RAISES {StablegenError.E} =
  VAR cu: M3AST_AS.Compilation_Unit;
  BEGIN
    IF NOT M3Context.FindExact(c, Atom.ToText(qid.intf),
                               M3CUnit.Type.Interface, cu) THEN
      RAISE StablegenError.E("Can not find interface "
                               & Atom.ToText(qid.intf))
    END;
    VAR
      pragIter := M3CPragma.NewIter(cu.lx_pragmas);
      prag    : M3CPragma.T;
      umethods: AtomList.T  := NIL;
    BEGIN
      WHILE M3CPragma.Next(pragIter, prag) DO
        VAR pragText: TEXT;
        BEGIN
          IF M3CPragma.Match(prag, "STABLE", pragText) THEN
            ParsePragma(pragText, umethods)
          END (*IF match*)
        END
      END; (*WHILE*)
      IF umethods = NIL THEN
        RAISE
          StablegenError.E(
            "can not find STABLE UPDATE METHODS pragma");
      END;
      VAR
        used_id: M3AST_AS.USED_ID := NEW(
                                       M3AST_AS.USED_ID).init();
        def_id   : M3AST_AS.DEF_ID;
        cu_reveal: M3AST_AS.Compilation_Unit;
      BEGIN
        used_id.lx_symrep :=
          M3CId.Enter(Atom.ToText(qid.item));
        IF NOT M3Context.FindExact(
                 c, reveal, M3CUnit.Type.Interface,
                 cu_reveal) THEN
          RAISE StablegenError.E(
                  "Can not find interface " & reveal)
        END;
        M3CConcTypeSpec.SetCurrentReveal(
          cu_reveal, ASTWalk.VisitMode.Entry);
        def_id := M3ASTScope.Lookup(
                    cu.as_root.as_id.vSCOPE, used_id);
        IF def_id = NIL THEN
          RAISE StablegenError.E(Atom.ToText(qid.intf) & "."
                                   & Atom.ToText(qid.item)
                                   & " not defined.");
        END;
        TYPECASE def_id OF
          M3AST_AS.TYPED_ID (typed_id) =>
            TypeDo(AstToType.Convert(typed_id.sm_type_spec),
                   qid, implName, repName, umethods)
        ELSE
          RAISE
            StablegenError.E(
              Atom.ToText(qid.item) & " is not a type in"
                & " interface " & Atom.ToText(qid.intf));
        END;  (*TYPECASE*)
        M3CConcTypeSpec.SetCurrentReveal(cu_reveal,
           ASTWalk.VisitMode.Exit);
      END
    END
  END Do;

(* \subsubsection*{TypeDo}
   If it is an opaque type we proceed to the revealation of that type
   (which must be visible). The check procedure "CheckStableObj()" is
   then called.  If "qid" is valid for stable subtyping, we check for
   explicit brands (since object used outside of the program that
   generated it should have an explicit brand). Finally we call
   "GenStableImpl()" which will generate the Modula-3 code for the
   stable object implementation.
*)
PROCEDURE TypeDo (type             : Type.T;
                  qid              : Type.Qid;
                  implName, repName: TEXT;
                  umethods         : AtomList.T)
  RAISES {StablegenError.E} =
  BEGIN
    WHILE ISTYPE(type, Type.Opaque) DO
      type := NARROW(type, Type.Opaque).revealedSuperType
    END;
    IF CheckStableType(type) THEN
      IF NOT CheckBrands(type) THEN
        StablegenError.Warning(
          "you should use explicitly branded type");
      END;
      GenStableImpl(qid, type, implName, repName, umethods);
    ELSE
      RAISE StablegenError.E(
        Atom.ToText(qid.intf) & "." & Atom.ToText(qid.item)
          & " can not be made stable (not an object type or has"
          & " procedure parameters in update methods)");
    END
  END TypeDo;

(* \subsubsection*{ParsePragma}
   Parse the pragma 
| <*STABLE UPDATE METHODS meth1, meth2, ...*>
   The pragma may appear more than once. "txt" is set to the string
   that starts with "UPDATE...". "methods" will contains the accumulated
   list of methods. They are separated by blanks. 

   The pragma has a second form (only method stated is the keyword
   "ANY") which is not checked in here.
*)
PROCEDURE ParsePragma (txt: TEXT; VAR methods: AtomList.T)
  RAISES {StablegenError.E} =
  <*FATAL Rd.Failure, Thread.Alerted*>

  PROCEDURE Add (VAR methods: AtomList.T; methname: TEXT)
    RAISES {StablegenError.E} =
    BEGIN
      IF methods = NIL THEN
        methods := AtomList.List1(Atom.FromText(methname))
      ELSE
        IF AtomList.Member(methods, Atom.FromText(methname)) THEN
          RAISE StablegenError.E(
                  "duplicate entry " & methname
                    & " in STABLE UPDATE METHODS pragma");
        END;
        methods :=
          AtomList.Cons(Atom.FromText(methname), methods)
      END
    END Add;

  CONST IdChars = SET OF CHAR{'_', 'A'..'Z', 'a'..'z', '0'..'9'};
  VAR
    rd             := TextRd.New(txt);
    methname: TEXT;
  BEGIN
    TRY
      Lex.Skip(rd);
      Lex.Match(rd, "UPDATE");
      Lex.Skip(rd);
      Lex.Match(rd, "METHODS");
      Lex.Skip(rd);

      (* First one is special: Has no ``,'' in front and
         "methods" may be "NIL" *)
      IF Rd.EOF(rd) THEN
        RAISE StablegenError.E(
                "empty STABLE UPDATE METHODS pragma");
      ELSE
        methname := Lex.Scan(rd, IdChars);
        Add(methods, methname);
      END; (*IF*)

      (* Consume comma and read method names *)
      Lex.Skip(rd);
      WHILE NOT Rd.EOF(rd) DO
        Lex.Match(rd, ",");
        Lex.Skip(rd);
        methname := Lex.Scan(rd, IdChars);
        Add(methods, methname);
        Lex.Skip(rd);
      END;
    EXCEPT
      Lex.Error =>
        RAISE StablegenError.E(
                "error in pragma: STABLE " & txt);
    END
  END ParsePragma;


(* \subsection{Procedure CheckStableType}
   A object suitable for the stable subtype generator must:
   \begin{enumerate}
   \item  Be an object type
   \item  (NOT CHECKED YET) Not update method can have a procedure type
          as parameter
   \end{enumerate}
   If both holds for the the type specification in parameter "o", the
   procedure will return "TRUE".
*)
PROCEDURE CheckStableType (type: Type.T): BOOLEAN =
  BEGIN
    TYPECASE type OF
      Type.Object=> RETURN TRUE;
      ELSE RETURN FALSE;
    END;
  END CheckStableType;

(* \subsection{Procedure CheckBrands}
   Take a type "t" an check if it is a branded type with an explicit
   brand, or if it contains fields with branded types with explicit
   brands. Return "TRUE" if all brands are explicit. If not, warning
   messages are printed and "FALSE" is returned.
*)
PROCEDURE CheckBrands (t: Type.T): BOOLEAN =
  VAR ok := TRUE;
  BEGIN
    IF t = NIL THEN RETURN TRUE END;
    IF t.visited THEN RETURN t.brandsOK END;
    t.visited := TRUE;
    TYPECASE t OF
    | Type.Reference (ref) =>
        IF ref.branded AND ref.brand = NIL THEN
          StablegenError.Warning(
              "Branded type with no explicit brand -- "
              & GenTypeCode.ToText(ref));
          ok := FALSE;
        END;
        TYPECASE ref OF
        | Type.Ref (r) => t.brandsOK := CheckBrands(r.target) AND ok
        | Type.Object (obj) =>
            ok := CheckBrands(obj.super) AND ok;
            FOR i := 0 TO LAST(obj.fields^) DO
              ok := CheckBrands(obj.fields[i].type) AND ok;
            END;
            FOR i := 0 TO LAST(obj.methods^) DO
              ok := CheckSigBrands(obj.methods[i].sig) AND ok;
            END;
            t.brandsOK := ok
        | Type.Opaque (opq) =>
            t.brandsOK := CheckBrands(opq.revealedSuperType) AND ok
        ELSE
          t.brandsOK := ok
        END;
    | Type.Array (arr) => t.brandsOK := CheckBrands(arr.element) AND ok
    | Type.Packed (p) => t.brandsOK := CheckBrands(p.base) AND ok
    | Type.Record (rec) =>
        FOR i := 0 TO LAST(rec.fields^) DO
          ok := CheckBrands(rec.fields[i].type) AND ok;
        END;
        t.brandsOK := ok;
    | Type.Procedure (proc) => t.brandsOK := CheckSigBrands(proc.sig);
    ELSE
      t.brandsOK := TRUE
    END;
    RETURN t.brandsOK;
  END CheckBrands;

PROCEDURE CheckSigBrands (sig: Type.Signature): BOOLEAN =
  VAR ok := TRUE;
  BEGIN
    FOR i := 0 TO LAST(sig.formals^) DO
      ok := CheckBrands(sig.formals[i].type) AND ok
    END;
    RETURN CheckBrands(sig.result) AND ok;
  END CheckSigBrands;


(* \subsection{Procedure GenStableImpl}
   Take a "Type.T" structure "type" representing an object type
   and generate an implementation of a stable subtype 
   of "type" in module "implName".

   The writer to put the implementation Modula-3 code is a "Formatter.T".
   The method list of "type" and its supertypes is produced by
   "BuildMethods()" (see there).
   "ImportList.FromType()" takes the type structure and the a list of
   its methods to look up all necessary imports to compile the
   type and the methods. We add the global "stableObjImports", the
   list of interfaces needed by generated code, to this list.
   Finally generate the logging overrides, the respool procedure,
   the replay stubs and finish up.
*)
PROCEDURE GenStableImpl (name             : Type.Qid;
                         type             : Type.Object;
                         implName, repName: TEXT;
                         umethods         : AtomList.T   )
  RAISES {StablegenError.E} =
  <*FATAL OSError.E, Wr.Failure*>
  VAR
    methods: ImportList.MethodList;
    modWr  : Formatter.T;
  BEGIN
    TRY
      modWr := Formatter.New(FileWr.Open(implName & ".m3"));
      methods := BuildMethods(type, umethods);
      VAR imports := ImportList.FromType(type, methods);
      BEGIN
        FOR i := FIRST(implImports) TO LAST(implImports) DO
          ImportList.Add(imports, implImports[i]);
        END;
        ImportList.Add(imports, name.intf);
        ImportList.Add(imports, Atom.FromText(repName));
        GenModuleCode.Header(
          modWr, implName, methods, imports);
      END;
      GenModuleCode.Revealation(modWr, repName, methods);
      GenModuleCode.Surrogates(
        modWr, name, repName, methods);
      GenModuleCode.Dispatcher(modWr, methods);
      GenModuleCode.ReplayStubs(modWr, name, methods);
      GenModuleCode.Checkpoint(modWr, repName);
      Formatter.PutText(modWr, "BEGIN\n");
      Formatter.PutText(modWr, "END " & implName & ".");
      Formatter.NewLine(modWr);
    FINALLY
      Formatter.Close(modWr);
    END;
  END GenStableImpl;


(* \subsection{Procedure BuildMethods}
   Take a type "type" and build a list of all its methods by scanning
   the methods declared by "t" and all its supertypes. Copy only those
   methods in the result list that are listed in "umethods".

   "Umethods" is checked: If it contains "ANY" it must be the only
   element. All methods listed in "umethods" must be declared in "type"
   otherwise.

   The procedure recursivly walks through all supertypes of "type".
   If the variable "count" is zero or negativ, the methods declared
   for "type" will be counted ("ANY" was used), otherwise the number
   of methods found in "type" that are contained in "umethods" must
   be the same as the length of "umethods" (i.e.\ no undeclared
   methods are in "umethods").

   Non of the method names listed in "reserved" may appear in the
   as name of an update method.
*)
VAR reserved:= NEW(AtomList.T, head:= Atom.FromText("init"), tail:=
               NEW(AtomList.T, head:= Atom.FromText("dispose"), tail:= 
               NEW(AtomList.T, head:= Atom.FromText("flushLog"), tail:= 
               NEW(AtomList.T, head:= Atom.FromText("freeLog"), tail:= 
               NEW(AtomList.T, head:= Atom.FromText("writeCheckpoint"), tail:= 
               NEW(AtomList.T, head:= Atom.FromText("readCheckpoint"), tail:= 
               NEW(AtomList.T, head:= Atom.FromText("replayLog"),
                   tail:= NIL)))))));

PROCEDURE BuildMethods (type    : Type.Object;
                        umethods: AtomList.T   ):
  ImportList.MethodList RAISES {StablegenError.E} =

  PROCEDURE Search (    type    : Type.Reference;
                    VAR count   : INTEGER;
                    VAR top     : CARDINAL;
                        umethods: AtomList.T   ):
    ImportList.MethodList RAISES {StablegenError.E} =
    VAR methods: ImportList.MethodList;
    BEGIN
      IF (type = Type.root) OR (type = NIL) THEN (* base of
                                                    recursion *)
        RETURN NEW(ImportList.MethodList, ABS(count))
      ELSE
	TYPECASE type OF
          Type.Object (ob) => 
          IF count <= 0 THEN
            count:= count - NUMBER(ob.methods^)
          END;
          methods := Search(ob.super, count, top, umethods);
          FOR i := 0 TO LAST(ob.methods^) DO
            IF umethods = NIL
              OR AtomList.Member(
                     umethods, ob.methods[i].name) THEN
              IF AtomList.Member(reserved, ob.methods[i].name) THEN
                RAISE StablegenError.E(Atom.ToText(ob.methods[i].name)
                &" is a reserved method name in stable objects. "
                &"Must not be an update method.")
              END;
              methods[top].name := ob.methods[i].name;
              methods[top].sig := ob.methods[i].sig;
              INC(top)
            END
          END;
          RETURN methods
        | Type.Opaque (op) =>
          RETURN Search(op.revealedSuperType, count, top, umethods)
        | Type.Reference => <*ASSERT FALSE*>
        END
      END
    END Search;

  VAR count: INTEGER;
  BEGIN
    IF AtomList.Member(umethods, Atom.FromText("ANY")) THEN
      IF AtomList.Length(umethods) # 1 THEN  
        RAISE
          StablegenError.E(
              "STABLE UPDATE METHODS ANY used with other methods")
      END;
      umethods := NIL;            (* take all *)
      count:= 0;                      (* and count them *)
    ELSE
      count:= AtomList.Length(umethods);
    END;
    VAR
      top : CARDINAL := 0;
      methList := Search(type, count, top, umethods);
    BEGIN
      IF ABS(count) # top THEN
        RAISE
          StablegenError.E(
              "method listed in STABLE UPDATE METHODS pragma not declared");
      END;
      RETURN methList
    END
  END BuildMethods;

BEGIN
END GenCode.

