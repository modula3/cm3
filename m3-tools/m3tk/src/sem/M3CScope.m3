MODULE M3CScope EXPORTS M3CScope, M3CScope_priv;

(***************************************************************************)
(*                      Copyright (C) Olivetti 1989                        *)
(*                          All Rights reserved                            *)
(*                                                                         *)
(* Use and copy of this software and preparation of derivative works based *)
(* upon this software are permitted to any person, provided this same      *)
(* copyright notice and the following Olivetti warranty disclaimer are     *) 
(* included in any copy of the software or any modification thereof or     *)
(* derivative work therefrom made by any person.                           *)
(*                                                                         *)
(* This software is made available AS IS and Olivetti disclaims all        *)
(* warranties with respect to this software, whether expressed or implied  *)
(* under any law, including all implied warranties of merchantibility and  *)
(* fitness for any purpose. In no event shall Olivetti be liable for any   *)
(* damages whatsoever resulting from loss of use, data or profits or       *)
(* otherwise arising out of or in connection with the use or performance   *)
(* of this software.                                                       *)
(***************************************************************************)

IMPORT M3AST, M3AST_AS, M3AST_SM, M3ASTNext;

IMPORT M3AST_AS_F, M3AST_SM_F, M3AST_TM_F;

IMPORT SeqM3AST_AS_Const_decl, SeqM3AST_AS_TYPE_DECL, SeqM3AST_AS_Var_decl,
    SeqM3AST_AS_Var_id, SeqM3AST_AS_IMPORTED, SeqM3AST_AS_Used_def_id, 
    SeqM3AST_AS_Used_interface_id, SeqM3AST_AS_DECL_REVL, SeqM3AST_AS_Exc_decl,
    SeqM3AST_AS_Import_item, SeqM3AST_AS_DEF_ID;
IMPORT M3CId, M3Error, M3Assert, M3CSearch, M3CRecursive;


(* Scope types and variables *)

(* Modified to build the SCOPE class. Should be modified to
   use this info instead of the Definitions data structure. *)

TYPE
  Scope = OBJECT
    next: Scope;
    defs: Definitions;
    vSCOPE: M3AST_SM.SCOPE := NIL;
  END;

  UnitScope = Scope OBJECT
    cu: M3AST_AS.Compilation_Unit;
  END;

  NormalUnitScope = UnitScope BRANDED OBJECT END;

  InitialScope = UnitScope BRANDED OBJECT END;
  (* In the initial scope 'cu' is the standard interface *)

  ProcedureScope = Scope OBJECT
    proc: M3AST_AS.Proc_decl;
  END;

  MethodScope = Scope OBJECT
    meth: M3AST_AS.Method;
  END;

  BlockScope = Scope OBJECT
    block: M3AST_AS.Block;
  END;

  DefIdScope = Scope OBJECT
    defId: M3AST_AS.DEF_ID;
  END;


VAR
  scopeNumber_g := -1;
  scope_g: Scope := NIL;


(* Basic routines: pushing and popping scopes, low level routine for adding
 definitions *)

PROCEDURE PushScope(scope: Scope) RAISES {}=
  BEGIN
    scope.next := scope_g;
    scope.defs := NIL;
    scope_g := scope;
    INC(scopeNumber_g);
  END PushScope;


PROCEDURE PopScope() RAISES {}=
  VAR
    d: Definitions;
  BEGIN
    d := scope_g.defs;
    IF d # NIL THEN
      TYPECASE scope_g OF
      | NormalUnitScope, ProcedureScope, BlockScope =>
          M3CRecursive.CheckDeclarations(d);
      ELSE
      END;
    END;
    WHILE d # NIL DO
      M3Assert.Check(d.defId.lx_symrep = d.id);
      IF d.id.defs # d THEN M3Assert.Fail() END;
      d.id.defs := d.enclosing;
      d := d.next;
    END; (* while *)
    scope_g := scope_g.next;
    DEC(scopeNumber_g);
  END PopScope;


PROCEDURE Add(
    defId: M3AST_AS.DEF_ID;
    enclosing: Definitions;
    decl: M3AST.NODE)
    RAISES {}=
  VAR
    new := NEW(Definitions);
  BEGIN
    new.next := scope_g.defs;
    scope_g.defs := new;
    new.enclosing := enclosing;
    new.id := defId.lx_symrep;
    new.defId := defId;
    new.scope := scopeNumber_g;
    new.id.defs := new;
    new.hook := decl;
    WITH s = scope_g.vSCOPE DO
      IF s # NIL THEN
        SeqM3AST_AS_DEF_ID.AddFront(s.sm_def_id_s, defId);
      END;
    END;
  END Add;


(* Layer on the basic add routine - provides add procedure which checks for
name clashes *)

PROCEDURE ExportsInterface(
    m: M3AST_AS.Module;
    i: M3AST_AS.Interface)
    : BOOLEAN
    RAISES {}=
  VAR
    iterExports := SeqM3AST_AS_Used_interface_id.NewIter(m.sm_export_s);
    export: M3AST_AS.Used_interface_id;
    def_id := i.as_id;
  BEGIN
    WHILE SeqM3AST_AS_Used_interface_id.Next(iterExports, export) DO
      IF export.sm_def = def_id THEN
        RETURN TRUE;
      ELSE
        (* continue loop *)
      END; (* if *)
    END; (* while *)
    RETURN FALSE;
  END ExportsInterface;


PROCEDURE ProcedureRedeclaration(
    new, old: M3AST_AS.DEF_ID)
    : BOOLEAN
    RAISES {}=
  VAR
    oldUnit, newUnit: M3AST_AS.UNIT;
  BEGIN
    IF (ISTYPE(new, M3AST_AS.Proc_id)) AND
          (ISTYPE(old, M3AST_AS.Proc_id)) THEN
      oldUnit := old.tmp_unit_id.sm_spec;
      newUnit := new.tmp_unit_id.sm_spec;
      RETURN (ISTYPE(oldUnit, M3AST_AS.Interface)) AND
          (ISTYPE(newUnit, M3AST_AS.Module)) AND
          ExportsInterface(newUnit, oldUnit);
    ELSE
      RETURN FALSE;
    END; (* if *)
  END ProcedureRedeclaration;


PROCEDURE BadRedefinition(id: M3AST_AS.ID) RAISES {}=
  BEGIN
    M3Error.ReportWithId(id,
        "Illegal redefinition of identifier \'%s\'", id.lx_symrep);
  END BadRedefinition;


PROCEDURE AddDefId(defId: M3AST_AS.DEF_ID; decl: M3AST.NODE := NIL;
    used_id: M3AST_AS.USED_ID := NIL) RAISES {}=
  VAR
    d: Definitions;
    old: M3AST_AS.DEF_ID;
  BEGIN
    IF defId.lx_symrep = NIL THEN RETURN END;
    d := defId.lx_symrep.defs;
    IF (d # NIL) AND ((d.scope = scopeNumber_g) OR (d.scope = 0)) THEN
      old := d.defId;
      IF ProcedureRedeclaration(defId, old) THEN
        (* use latest definition and set up 'sm_int_def/sm_concrete_proc_id' *)
        d.defId := defId;
        NARROW(defId, M3AST_AS.Proc_id).vREDEF_ID.sm_int_def := old;
        NARROW(old, M3AST_AS.Proc_id).sm_concrete_proc_id := defId;
      ELSE
        IF used_id # NIL THEN
          BadRedefinition(used_id);
        ELSE
          BadRedefinition(defId);
        END;
      END; (* if *)
    END; (* if *)
    Add(defId, d, decl);
  END AddDefId;


(* Secondary routines - form an intermediate layer between the basic routines
 and the exported routines *)

PROCEDURE AddBlock(block: M3AST_AS.Block) RAISES {}=
  VAR
    iter := SeqM3AST_AS_DECL_REVL.NewIter(block.as_decl_s);
    decl: M3AST_AS.DECL_REVL;
  BEGIN
    WHILE SeqM3AST_AS_DECL_REVL.Next(iter, decl) DO
      TYPECASE decl OF
      | M3AST_AS.Const_decl_s(constDeclS) =>
          VAR
            iter :=
                SeqM3AST_AS_Const_decl.NewIter(constDeclS.as_const_decl_s);
            constDecl: M3AST_AS.Const_decl;
          BEGIN
            WHILE SeqM3AST_AS_Const_decl.Next(iter, constDecl) DO
              AddDefId(constDecl.as_id, constDecl);
            END;
          END;
      | M3AST_AS.Type_decl_s(typeDeclS) =>
          VAR
            iter := SeqM3AST_AS_TYPE_DECL.NewIter(typeDeclS.as_type_decl_s);
            typeDecl: M3AST_AS.TYPE_DECL;
          BEGIN
            WHILE SeqM3AST_AS_TYPE_DECL.Next(iter, typeDecl) DO
              AddDefId(typeDecl.as_id, typeDecl);
            END;
          END;
      | M3AST_AS.Var_decl_s(varDeclS) =>
          VAR
            iter := SeqM3AST_AS_Var_decl.NewIter(varDeclS.as_var_decl_s);
            varDecl: M3AST_AS.Var_decl;
          BEGIN
            WHILE SeqM3AST_AS_Var_decl.Next(iter, varDecl) DO
              VAR
                iter := SeqM3AST_AS_Var_id.NewIter(varDecl.as_id_s);
                varId: M3AST_AS.Var_id;
              BEGIN
                WHILE SeqM3AST_AS_Var_id.Next(iter, varId) DO
                  AddDefId(varId, varDecl);
                END;
              END;
            END;
          END;
      | M3AST_AS.Exc_decl_s(excDeclS) =>
          VAR
            iter := SeqM3AST_AS_Exc_decl.NewIter(excDeclS.as_exc_decl_s);
            excDecl: M3AST_AS.Exc_decl;
          BEGIN
            WHILE SeqM3AST_AS_Exc_decl.Next(iter, excDecl) DO
              AddDefId(excDecl.as_id, excDecl);
            END;
          END;
      | M3AST_AS.Proc_decl(procDecl) =>
          AddDefId(procDecl.as_id, procDecl);
      ELSE
      END;
    END;
  END AddBlock;


PROCEDURE CheckFrom(
    fromImport: M3AST_AS.From_import;
    VAR (*out*) from: M3AST_AS.Interface)
    : BOOLEAN
    RAISES {}=
  VAR
    usedIntId: M3AST_AS.Used_interface_id;
    cu: M3AST_AS.Compilation_Unit;
    intf_id: M3AST_AS.Interface_id;
  BEGIN
    (* returns TRUE unless FROM Self IMPORT ... *)
    usedIntId := fromImport.as_intf_id;
    TYPECASE usedIntId.sm_def OF
    | NULL => RETURN FALSE;
    | M3AST_AS.Interface_AS_id(intf_as_id) =>
        intf_id := NARROW(intf_as_id.tmp_used_id.sm_def,
                              M3AST_AS.Interface_id);
    | M3AST_AS.Interface_id(t_intf_id) =>
        intf_id := t_intf_id;
    ELSE
      M3Assert.Fail();
    END;
    cu := intf_id.sm_spec.sm_comp_unit;

    TYPECASE scope_g OF
    | NormalUnitScope(scope) =>
        IF scope.cu # cu THEN
          from := cu.as_root;
          RETURN TRUE;
        ELSE
          (* Note: 'usedIntId.lx_symrep' must be non NIL or 'sm_def' would
           be NIL *)
          M3Error.ReportWithId(usedIntId,
              "Cannot import from self (\'%s\')", usedIntId.lx_symrep);
          RETURN FALSE;
        END;
    ELSE
      M3Assert.Fail(); <*NOWARN*>
    END;
  END CheckFrom;


<*INLINE*> PROCEDURE AddImport(used_id: M3AST_AS.USED_ID) RAISES {}=
  BEGIN
    IF used_id.sm_def # NIL THEN AddDefId(used_id.sm_def, used_id := used_id)
    END; (* if *)
  END AddImport;


PROCEDURE AddImports(seqImported: SeqM3AST_AS_IMPORTED.T) RAISES {}=
  VAR
    iterImported := SeqM3AST_AS_IMPORTED.NewIter(seqImported);
    imported: M3AST_AS.IMPORTED;
    from: M3AST_AS.Interface;
    iterUsedIds: SeqM3AST_AS_Used_def_id.Iter;
    usedDefId: M3AST_AS.Used_def_id;
    iterImport_item: SeqM3AST_AS_Import_item.Iter;
    import_item: M3AST_AS.Import_item;
  BEGIN
    WHILE SeqM3AST_AS_IMPORTED.Next(iterImported, imported) DO
      TYPECASE imported OF <*NOWARN*>
      | M3AST_AS.From_import(fromImport) =>
          IF CheckFrom(fromImport, from) THEN
            iterUsedIds :=
                SeqM3AST_AS_Used_def_id.NewIter(fromImport.as_id_s);
            WHILE SeqM3AST_AS_Used_def_id.Next(iterUsedIds, usedDefId) DO
              M3CSearch.Export(from, usedDefId);
              AddImport(usedDefId);
            END; (* while *)
          END;
      | M3AST_AS.Simple_import(simpleImport) =>
          iterImport_item :=
              SeqM3AST_AS_Import_item.NewIter(simpleImport.as_import_item_s);
	  WHILE SeqM3AST_AS_Import_item.Next(
              iterImport_item, import_item) DO
            IF import_item.as_id # NIL THEN
              AddDefId(import_item.as_id);
            ELSE
              AddImport(import_item.as_intf_id);
            END;
	  END; (* while *)
      END; (* typecase *)
    END; (* while *)
  END AddImports;


PROCEDURE AddExport(export: M3AST_AS.Used_interface_id) RAISES {}=
  BEGIN
    TYPECASE export.sm_def OF
    | NULL =>
    | M3AST_AS.Interface_id(iId) =>
        WITH unit = NARROW(iId.sm_spec.sm_comp_unit.as_root, M3AST_AS.UNIT_NORMAL) DO
          M3Assert.Check(iId = unit.as_id);
          AddBlock(unit.as_block);
        END;
    ELSE
      M3Assert.Fail();
    END; (* if *)
  END AddExport;


PROCEDURE AddExports(cu: M3AST_AS.Compilation_Unit) RAISES {}=
  VAR
    iterExports: SeqM3AST_AS_Used_interface_id.Iter;
    export: M3AST_AS.Used_interface_id;
  BEGIN
    TYPECASE cu.as_root OF
    | M3AST_AS.Module(module) =>
        iterExports :=
            SeqM3AST_AS_Used_interface_id.NewIter(module.sm_export_s);
        WHILE SeqM3AST_AS_Used_interface_id.Next(iterExports, export) DO
          AddExport(export);
        END; (* while *)
    ELSE
      (* interface - does not have any exports *)
    END; (* if *)
  END AddExports;


(* The main exported routines *)

PROCEDURE Standard(standard: M3AST_AS.Compilation_Unit) RAISES {}=
  BEGIN
    TYPECASE scope_g OF
    | NULL =>
    | InitialScope(scope) =>
        IF scope.cu = NIL THEN
          scope.cu := standard;
          scope.vSCOPE := standard.as_root.as_id.vSCOPE;
          WITH s = scope.vSCOPE DO
            s.sm_level := scopeNumber_g;
            WITH un = NARROW(standard.as_root, M3AST_AS.UNIT_NORMAL) DO
              AddBlock(un.as_block);
              WITH bs = un.as_block.vSCOPE DO
                bs.sm_level := s.sm_level;
                bs.sm_enc_scope := s;
              END;
            END;
          END;
        ELSE
          (* already done *)
        END; (* if *)
        RETURN;
    ELSE
    END;
    M3Assert.Fail();
  END Standard;


PROCEDURE CompilationUnit(
    cu: M3AST_AS.Compilation_Unit;
    change: Change)
    RAISES {}=
  BEGIN
    TYPECASE scope_g OF
    | NULL =>
    | InitialScope(scope) =>
        IF change = Change.Enter AND scope.cu # NIL THEN
          VAR
            new := NEW(NormalUnitScope, vSCOPE := cu.as_root.as_id.vSCOPE);
          BEGIN
            WITH s = new.vSCOPE DO
              s.sm_level := scopeNumber_g+1;
              s.sm_enc_scope := scope_g.vSCOPE;
              new.cu := cu;
              PushScope(new);
              AddExports(cu);
              WITH un = NARROW(cu.as_root, M3AST_AS.UNIT_NORMAL) DO
                AddImports(un.as_import_s);
                AddBlock(un.as_block);
                WITH bs = un.as_block.vSCOPE DO
                  bs.sm_level := s.sm_level;
                  bs.sm_enc_scope := s;
                END;
              END;
            END;
          END;
          RETURN;
        END;
    | NormalUnitScope(scope) =>
        IF change = Change.Exit AND scope.cu = cu THEN
          PopScope();
          RETURN;
        END;
    ELSE
    END; (* typecase *)
    M3Assert.Fail();
  END CompilationUnit;


PROCEDURE Procedure(proc: M3AST_AS.Proc_decl; change: Change) RAISES {}=
  BEGIN
    IF change = Change.Enter THEN
      VAR
        new := NEW(ProcedureScope, vSCOPE := proc.as_id.vSCOPE);
        iter := M3ASTNext.NewIterFormal(proc.as_type.as_formal_param_s);
        formal: M3AST_AS.Formal_param;
        formalId: M3AST_AS.FORMAL_ID;
      BEGIN
        WITH s = new.vSCOPE DO
          s.sm_level := scopeNumber_g+1;
          s.sm_enc_scope := scope_g.vSCOPE;
          new.proc := proc;
          PushScope(new);
          WHILE M3ASTNext.Formal(iter, formal, formalId) DO
            AddDefId(formalId);
          END;
          IF proc.as_body # NIL THEN
            AddBlock(proc.as_body);
            WITH bs = proc.as_body.vSCOPE DO
              bs.sm_level := s.sm_level;
              bs.sm_enc_scope := s;
            END;
          END;
        END;
      END;
    ELSE
      TYPECASE scope_g OF
      | NULL =>
      | ProcedureScope(scope) =>
          IF scope.proc = proc THEN PopScope(); RETURN END;
      ELSE
      END;
      M3Assert.Fail();
    END; (* if *)
  END Procedure;


PROCEDURE Method(meth: M3AST_AS.Method; change: Change) RAISES {}=
  BEGIN
    IF change = Change.Enter THEN
      VAR
        new := NEW(MethodScope,
                   vSCOPE := NARROW(meth.as_id, M3AST_AS.Method_id).vSCOPE);
        iter := M3ASTNext.NewIterFormal(meth.as_type.as_formal_param_s);
        formal: M3AST_AS.Formal_param;
        formalId: M3AST_AS.FORMAL_ID;
      BEGIN
        WITH s = new.vSCOPE DO
          s.sm_level := scopeNumber_g+1;
          s.sm_enc_scope := scope_g.vSCOPE;
          new.meth := meth;
          PushScope(new);
          WHILE M3ASTNext.Formal(iter, formal, formalId) DO
            AddDefId(formalId);
          END;
        END;
      END;
    ELSE
      TYPECASE scope_g OF
      | NULL =>
      | MethodScope(scope) =>
          IF scope.meth = meth THEN PopScope(); RETURN END;
      ELSE
      END;
      M3Assert.Fail();
    END; (* if *)
  END Method;


PROCEDURE UnitOrProcedureBody(block: M3AST_AS.Block): BOOLEAN RAISES {}=
  BEGIN
    TYPECASE scope_g OF
    | NULL =>
        M3Assert.Fail();
        <*ASSERT FALSE*>
    | UnitScope(unitScope) =>
        RETURN
          NARROW(unitScope.cu.as_root, M3AST_AS.UNIT_NORMAL).as_block = block;
    | ProcedureScope(procScope) =>
        RETURN procScope.proc.as_body = block;
    ELSE
      RETURN FALSE;
    END; (* case *)    
  END UnitOrProcedureBody;


PROCEDURE Block(block: M3AST_AS.Block; change: Change) RAISES {}=
  BEGIN
    IF  UnitOrProcedureBody(block) THEN RETURN END;
    IF change = Change.Enter THEN
      VAR
        new := NEW(BlockScope, vSCOPE := block.vSCOPE);
      BEGIN
        WITH s = new.vSCOPE DO
          s.sm_level := scopeNumber_g+1;
          s.sm_enc_scope := scope_g.vSCOPE;
        END;
        new.block := block;
        PushScope(new);
        AddBlock(block);
      END;
    ELSE
      TYPECASE scope_g OF
      | NULL =>
      | BlockScope(scope) =>
          IF scope.block = block THEN PopScope(); RETURN END;
      ELSE
      END; (* typecase *)
      M3Assert.Fail();
    END; (* if *)
  END Block;


PROCEDURE DefId(defId: M3AST_AS.DEF_ID; change: Change) RAISES {}=

  PROCEDURE ScopeForDefId(): M3AST_SM.SCOPE=
    VAR r: M3AST_SM.SCOPE := NIL;
    BEGIN
      IF defId.IsA_SCOPE(r) THEN END;
      RETURN r;
    END ScopeForDefId;

  BEGIN
    IF change = Change.Enter THEN
      VAR
        new := NEW(DefIdScope, vSCOPE := ScopeForDefId());
      BEGIN
        WITH s = new.vSCOPE DO
          s.sm_level := scopeNumber_g+1;
          s.sm_enc_scope := scope_g.vSCOPE;
        END;
        new.defId := defId;
        PushScope(new);
        AddDefId(defId);
      END;
    ELSE
      TYPECASE scope_g OF
      | NULL =>
      | DefIdScope(scope) =>
          IF scope.defId = defId THEN PopScope(); RETURN END;
      ELSE
      END;
      M3Assert.Fail();
    END; (* if *)
  END DefId;


PROCEDURE Lookup(usedId: M3AST_AS.USED_ID) RAISES {}=
  VAR
    symrep := usedId.lx_symrep;
  BEGIN
    IF symrep # NIL THEN
      VAR
        d := symrep.defs;
      BEGIN
        IF d = NIL THEN
          M3Error.ReportWithId(usedId,
              "Identifier \'%s\' not declared", symrep);
        ELSE
          usedId.sm_def := d.defId;
        END; (* if *)
      END;
    END;
  END Lookup;


PROCEDURE PushInitialScope() RAISES {}=
  VAR
    new := NEW(InitialScope);
  BEGIN
    new.cu := NIL;
    PushScope(new);
  END PushInitialScope;


BEGIN
  PushInitialScope();
END M3CScope.
