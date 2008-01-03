(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

MODULE M3ASTPickle;

IMPORT Rd, Wr, Thread, Pickle;
IMPORT AST, ASTWalk;
IMPORT M3AST_LX, M3AST_AS, M3AST_SC, M3Context, M3CUnit, M3CId, M3CLiteral;
IMPORT SeqM3AST_AS_Used_interface_id, SeqM3AST_SC_Exported_node,
       SeqM3AST_SC_Unit_stub;
IMPORT M3AST_AS_F, M3AST_SM_F, M3AST_TM_F, M3AST_SC_F;

<*FATAL Wr.Failure, Rd.EndOfFile, Rd.Failure, Thread.Alerted, Pickle.Error *>

CONST
  Internal = '0'; External = '1';

TYPE 
  Reader = Pickle.Reader OBJECT
    cu: M3AST_AS.Compilation_Unit := NIL;
    context: M3Context.T;
    importProc: ImportedUnitProc;
  END;

  Writer = Pickle.Writer OBJECT
    cu: M3AST_AS.Compilation_Unit;
  END;   

PROCEDURE Read(context: M3Context.T; rd: Rd.T; p: ImportedUnitProc; 
    VAR cu: M3AST_AS.Compilation_Unit
    ) RAISES {Rd.Failure, Thread.Alerted, Pickle.Error}=
  BEGIN
    cu := NEW(Reader, importProc := p, context := context,
              rd := rd).read();
  END Read;

PROCEDURE Write(cu: M3AST_AS.Compilation_Unit; wr: Wr.T
  ) RAISES {Wr.Failure, Thread.Alerted, Pickle.Error}=
  VAR
    iter := SeqM3AST_AS_Used_interface_id.NewIter(
                NARROW(cu.as_root, M3AST_AS.UNIT_WITH_BODY).sm_import_s);
    used_intf_id: M3AST_AS.Used_interface_id;
  BEGIN
    WHILE SeqM3AST_AS_Used_interface_id.Next(iter, used_intf_id) DO
      BuildExportedNodeTable(
        NARROW(used_intf_id.sm_def, M3AST_AS.UNIT_ID).sm_spec.sm_comp_unit);
    END;
    NEW(Writer, cu := cu, wr := wr).write(cu);
  END Write;

PROCEDURE BuildExportedNodeTable(cu: M3AST_AS.Compilation_Unit)=
  <*FATAL ANY*>
  BEGIN
    IF SeqM3AST_SC_Exported_node.Empty(cu.sc_exported_node_s) THEN
      ASTWalk.VisitNodes(cu, NEW(BuildClosure, cu := cu));
    END;
  END BuildExportedNodeTable;

TYPE BuildClosure = ASTWalk.Closure OBJECT
    cu: M3AST_AS.Compilation_Unit;
  OVERRIDES
    callback := BuildNode
  END;

PROCEDURE BuildNode(cl: BuildClosure; n: AST.NODE;
                    <*UNUSED*> vm: ASTWalk.VisitMode)=
  BEGIN
    TYPECASE n OF
    | M3AST_AS.DEF_ID, M3AST_AS.TYPE_SPEC =>
        SeqM3AST_SC_Exported_node.AddRear(cl.cu.sc_exported_node_s,
            NEW(M3AST_SC.Exported_node, sc_actual_node := n));
    ELSE
    END;
  END BuildNode;

PROCEDURE Special_read_TYPE_SPEC(sp: Pickle.Special; reader: Pickle.Reader;
                                 id: Pickle.RefID): REFANY=
  VAR
    kind := Rd.GetChar(reader.rd);
    type_spec: M3AST_AS.TYPE_SPEC := NIL;
  BEGIN
    IF kind = Internal THEN
      type_spec := Pickle.Special.read(sp, reader, id)
    ELSE
      VAR imported_node: M3AST_SC.IMPORTED_NODE := reader.read();
        a: M3AST_AS.SRC_NODE;
      BEGIN
        IF imported_node # NIL THEN
          IF FindActualFromIMPORTED_NODE(reader, imported_node, a) THEN
            type_spec := a
          END;
        END;
        reader.noteRef(type_spec, id);
      END
    END;
    RETURN type_spec;
  END Special_read_TYPE_SPEC;

PROCEDURE Special_write_TYPE_SPEC(sp: Pickle.Special; r: REFANY;
                                 writer: Pickle.Writer)=

  VAR 
    type_spec := NARROW(r, M3AST_AS.TYPE_SPEC);
    ext_unit_id: M3AST_AS.UNIT_ID;
    this_cu := NARROW(writer, Writer).cu;
  BEGIN
    IF ThisUnit(this_cu, type_spec.tmp_unit_id, ext_unit_id) THEN
      Wr.PutChar(writer.wr, Internal);
      Pickle.Special.write(sp, r, writer);
    ELSE
      VAR imported_node: M3AST_SC.IMPORTED_NODE := NIL;
      BEGIN
        IF ext_unit_id # NIL THEN
          imported_node := NewIMPORTED_NODE(this_cu, ext_unit_id, type_spec);
        END;
        Wr.PutChar(writer.wr, External);
        writer.write(imported_node);
      END
    END
  END Special_write_TYPE_SPEC;

PROCEDURE Special_read_DEF_ID(sp: Pickle.Special; reader: Pickle.Reader;
                              id: Pickle.RefID): REFANY=
  VAR
    kind := Rd.GetChar(reader.rd);
    def_id: M3AST_AS.DEF_ID := NIL;
  BEGIN
    IF kind = Internal THEN
      def_id := Pickle.Special.read(sp, reader, id)
    ELSE
      VAR imported_node: M3AST_SC.IMPORTED_NODE := reader.read();
        a: M3AST_AS.SRC_NODE;
      BEGIN
        IF imported_node # NIL THEN
          IF FindActualFromIMPORTED_NODE(reader, imported_node, a) THEN
            def_id := a
          END;
        END;
        reader.noteRef(def_id, id);
      END
    END;
    RETURN def_id;
  END Special_read_DEF_ID;

PROCEDURE Special_write_DEF_ID(sp: Pickle.Special; r: REFANY;
                                 writer: Pickle.Writer)=

  VAR 
    def_id := NARROW(r, M3AST_AS.DEF_ID);
    ext_unit_id: M3AST_AS.UNIT_ID;
    this_cu := NARROW(writer, Writer).cu;
  BEGIN
    IF ThisUnit(this_cu, def_id.tmp_unit_id, ext_unit_id) THEN
      Wr.PutChar(writer.wr, Internal);
      Pickle.Special.write(sp, r, writer);
    ELSE
      VAR imported_node: M3AST_SC.IMPORTED_NODE := NIL;
      BEGIN
        IF ext_unit_id # NIL THEN
          imported_node := NewIMPORTED_NODE(this_cu, ext_unit_id, def_id);
        END;
        Wr.PutChar(writer.wr, External);
        writer.write(imported_node);
      END
    END
  END Special_write_DEF_ID;

PROCEDURE NewIMPORTED_NODE(this_cu: M3AST_AS.Compilation_Unit;
                           ext_unit_id: M3AST_AS.UNIT_ID;
                           n: M3AST_AS.SRC_NODE): M3AST_SC.IMPORTED_NODE=
  BEGIN
    WITH ext_cu = ext_unit_id.sm_spec.sm_comp_unit DO
      RETURN NEW(M3AST_SC.IMPORTED_NODE,
          sc_unit_stub := FindOrGenerateUnit_stub(this_cu, ext_cu),
          sc_eoi := FindExportedNodeIndexFor(n, ext_cu));
    END
  END NewIMPORTED_NODE;

PROCEDURE FindActualFromIMPORTED_NODE(
    reader: Reader;
    imported_node: M3AST_SC.IMPORTED_NODE;
    VAR (*out*) actual_node: M3AST_AS.SRC_NODE): BOOLEAN=
  <*FATAL ANY*>
  VAR imp_cu: M3AST_AS.Compilation_Unit;
  BEGIN
    WITH usb = imported_node.sc_unit_stub DO
      IF reader.importProc(
           M3CId.ToText(usb.sc_unit_symrep),
           usb.sc_unit_type,
           usb.sc_unit_uid,
           reader.context,
           imp_cu) THEN
          BuildExportedNodeTable(imp_cu);
          actual_node := FindFromExportedNodeIndex(imp_cu,
                                                   imported_node.sc_eoi);
          RETURN TRUE;
      ELSE
        RETURN FALSE;
      END
    END
  END FindActualFromIMPORTED_NODE;
    
PROCEDURE ThisUnit(cu: M3AST_AS.Compilation_Unit; unit_id: M3AST_AS.UNIT_ID;
    VAR (*out*) ext_unit_id: M3AST_AS.UNIT_ID): BOOLEAN=
  BEGIN
    IF unit_id = NIL OR unit_id = cu.as_root.as_id THEN RETURN TRUE
    ELSE
      IF ISTYPE(unit_id, M3AST_AS.Module_id) THEN ext_unit_id := NIL
      ELSE ext_unit_id := unit_id
      END;
      RETURN FALSE;
    END
  END ThisUnit;

PROCEDURE FindExportedNodeIndexFor(t: M3AST_AS.SRC_NODE; 
    cu: M3AST_AS.Compilation_Unit): INTEGER RAISES{}=
  VAR
    en: M3AST_SC.Exported_node;
    iter := SeqM3AST_SC_Exported_node.NewIter(cu.sc_exported_node_s);
    eoi_uid: INTEGER := 0;
  BEGIN
    WHILE SeqM3AST_SC_Exported_node.Next(iter, en) DO
      IF en.sc_actual_node = t THEN
      	RETURN eoi_uid;
      END; (* if *)
      INC(eoi_uid);
    END; (* while *)
    <*ASSERT FALSE*>
  END FindExportedNodeIndexFor;

PROCEDURE FindFromExportedNodeIndex(
    cu: M3AST_AS.Compilation_Unit;
    this_eoi_uid: INTEGER): M3AST_AS.SRC_NODE=
  VAR
    en: M3AST_SC.Exported_node;
    iter := SeqM3AST_SC_Exported_node.NewIter(cu.sc_exported_node_s);
    eoi_uid: INTEGER := 0;
  BEGIN
    WHILE SeqM3AST_SC_Exported_node.Next(iter, en) DO
      IF eoi_uid = this_eoi_uid THEN
        RETURN en.sc_actual_node;
      END; (* if *)
      INC(eoi_uid);
    END; (* while *)
    <*ASSERT FALSE*>
  END FindFromExportedNodeIndex;

PROCEDURE FindOrGenerateUnit_stub(
    this_cu, ext_cu: M3AST_AS.Compilation_Unit): M3AST_SC.Unit_stub RAISES {}=
  VAR
    iter := SeqM3AST_SC_Unit_stub.NewIter(this_cu.sc_unit_stub_s);
    unit_stub: M3AST_SC.Unit_stub;
  BEGIN
    WHILE SeqM3AST_SC_Unit_stub.Next(iter, unit_stub) DO
      IF ext_cu.as_root.as_id.lx_symrep = unit_stub.sc_unit_symrep THEN
        (* assume same type (interface) *)
        RETURN unit_stub
      END; (* if *)
    END; (* while *)
    unit_stub := NEW(M3AST_SC.Unit_stub, 
        sc_unit_symrep := ext_cu.as_root.as_id.lx_symrep,
        sc_unit_uid := ext_cu.fe_uid,
        sc_unit_type := M3CUnit.ToType(ext_cu.as_root));
    SeqM3AST_SC_Unit_stub.AddRear(this_cu.sc_unit_stub_s, unit_stub);
    RETURN unit_stub;
  END FindOrGenerateUnit_stub;

PROCEDURE Special_read_Symbol_rep(<*UNUSED*> sp: Pickle.Special;
                                  reader: Pickle.Reader;
                                  id: Pickle.RefID;): REFANY=
  VAR text: TEXT := reader.read();
  BEGIN
    WITH atom = M3CId.Enter(text) DO
      reader.noteRef(atom, id);
      RETURN atom;
    END
  END Special_read_Symbol_rep;

PROCEDURE Special_write_Symbol_rep(<*UNUSED*> sp: Pickle.Special; r: REFANY;
                                 writer: Pickle.Writer)=
  BEGIN
    writer.write(M3CId.ToText(r));
  END Special_write_Symbol_rep;

PROCEDURE Special_read_Literal_rep(<*UNUSED*> sp: Pickle.Special;
                                   reader: Pickle.Reader;
                                   id: Pickle.RefID): REFANY=
  VAR text: TEXT := reader.read();
  BEGIN
    WITH lit = M3CLiteral.Enter(text) DO
      reader.noteRef(lit, id);
      RETURN lit;
    END
  END Special_read_Literal_rep;

PROCEDURE Special_write_Literal_rep(<*UNUSED*> sp: Pickle.Special; r: REFANY;
                                 writer: Pickle.Writer)=
  BEGIN
    writer.write(M3CLiteral.ToText(r));
  END Special_write_Literal_rep;

BEGIN
  Pickle.RegisterSpecial(
      NEW(Pickle.Special, sc := TYPECODE(M3AST_AS.TYPE_SPEC),
          read := Special_read_TYPE_SPEC,
          write := Special_write_TYPE_SPEC));

  Pickle.RegisterSpecial(
      NEW(Pickle.Special, sc := TYPECODE(M3AST_AS.DEF_ID),
          read := Special_read_DEF_ID,
          write := Special_write_DEF_ID));

  Pickle.RegisterSpecial(
      NEW(Pickle.Special, sc := TYPECODE(M3AST_LX.Symbol_rep),
          read := Special_read_Symbol_rep,
          write := Special_write_Symbol_rep));

  Pickle.RegisterSpecial(
      NEW(Pickle.Special, sc := TYPECODE(M3AST_LX.Literal_rep),
          read := Special_read_Literal_rep,
          write := Special_write_Literal_rep));
END M3ASTPickle.
