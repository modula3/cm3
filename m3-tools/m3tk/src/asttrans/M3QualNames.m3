MODULE M3QualNames;

(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)


IMPORT AST, M3AST_AS;
IMPORT M3Context;
IMPORT AST_Iter;
IMPORT SeqM3AST_AS_Import_item;
IMPORT M3AST_AS_F, M3AST_SM_F, M3AST_TM_F;

PROCEDURE Set(cu: M3AST_AS.Compilation_Unit) RAISES {}=
  BEGIN
    Qualify(cu.as_root, cu.as_root.as_id, NIL, NIL,
      M3Context.Standard().as_root.as_id);
  END Set;

PROCEDURE Qualify(n: AST.NODE; unit_id: M3AST_AS.UNIT_ID; 
    parent: AST.NODE; iter: AST_Iter.T;
    standard_id: M3AST_AS.UNIT_ID) RAISES {}=
  BEGIN
    TYPECASE n OF
    | M3AST_AS.Used_def_id(u) =>
        IF IsImported(u.sm_def, unit_id, standard_id) THEN
	  VAR q: M3AST_AS.Qual_used_id :=
              NEW(M3AST_AS.Qual_used_id).init();
          BEGIN
	    q.as_id := u;
	    q.as_intf_id := NEW(M3AST_AS.Used_interface_id).init();
	    q.as_intf_id.lx_symrep := u.sm_def.tmp_unit_id.lx_symrep;
	    q.as_intf_id.sm_def := u.sm_def.tmp_unit_id;
	    parent.update(iter, q);
	  END;
	END; (* if *)

    | M3AST_AS.Exp_used_id(e) =>
      	IF IsImported(e.vUSED_ID.sm_def, unit_id, standard_id) THEN
	  VAR
	    s: M3AST_AS.Binary := NEW(M3AST_AS.Binary).init();
	    int_e: M3AST_AS.Exp_used_id := NEW(M3AST_AS.Exp_used_id).init();
          BEGIN
	    s.as_binary_op := NEW(M3AST_AS.Select).init();
	    s.as_exp2 := e; s.as_exp1 := int_e;
	    int_e.vUSED_ID.lx_symrep :=
	        e.vUSED_ID.sm_def.tmp_unit_id.lx_symrep;
            int_e.vUSED_ID.sm_def := e.vUSED_ID.sm_def.tmp_unit_id;
	    parent.update(iter, s);
	  END;
	END; (* if *)

    | M3AST_AS.Qual_used_id(q) =>
      	IF q.as_intf_id = NIL THEN
	  IF IsImported(q.as_id.sm_def, unit_id, standard_id) THEN
            q.as_intf_id := NEW(M3AST_AS.Used_interface_id).init();
	    q.as_intf_id.lx_symrep := q.as_id.sm_def.tmp_unit_id.lx_symrep;
	    q.as_intf_id.sm_def := q.as_id.sm_def.tmp_unit_id;
	  END; (* if *)
	END;
	RETURN;

    | M3AST_AS.Binary(b) =>
        TYPECASE b.as_binary_op OF
	| M3AST_AS.Select =>
	    TYPECASE b.as_exp1 OF
	    | M3AST_AS.Exp_used_id(e) =>
                TYPECASE e.vUSED_ID.sm_def OF
                | NULL =>
                | M3AST_AS.Interface_id, M3AST_AS.Interface_AS_id =>
		    RETURN
                ELSE
		END;
	    ELSE
	    END; (* typecase *)
	ELSE
	END; (* typecase *)

    | M3AST_AS.From_import(f) =>
      	VAR
	  s: M3AST_AS.Simple_import := NEW(M3AST_AS.Simple_import).init();
	  i: M3AST_AS.Import_item := NEW(M3AST_AS.Import_item).init();
          u: M3AST_AS.Used_interface_id :=
              NEW(M3AST_AS.Used_interface_id).init();
	BEGIN
	  s.as_import_item_s := SeqM3AST_AS_Import_item.Null;
	  SeqM3AST_AS_Import_item.AddFront(s.as_import_item_s, i);
          i.as_intf_id := u;
	  u.lx_symrep := f.as_intf_id.sm_def.lx_symrep;
	  u.sm_def := f.as_intf_id.sm_def.tmp_unit_id;
	  parent.update(iter, s);
	  RETURN
	END;

    ELSE (* ignore *) 	
    END; (* typecase *)

    (* iterate children *)
    VAR
      iter := n.newIter();
      parent := n.newIter();
      child, void: AST.NODE;
    BEGIN
      WHILE iter.next(child) DO
      	IF child # NIL THEN
	  Qualify(child, unit_id, n, parent, standard_id);
	END; (* if *)
	EVAL parent.next(void);
      END; (* while *)
    END;
  END Qualify;

PROCEDURE IsImported(def_id: M3AST_AS.DEF_ID;
    this_unit_id, standard_id: M3AST_AS.UNIT_ID
    ): BOOLEAN RAISES {}=
  VAR unit_id := def_id.tmp_unit_id;
  BEGIN
    RETURN unit_id # this_unit_id AND unit_id # standard_id AND
        TopLevel(def_id);
  END IsImported;

PROCEDURE TopLevel(def_id: M3AST_AS.DEF_ID): BOOLEAN RAISES {}=
  BEGIN
    TYPECASE def_id OF
    | M3AST_AS.Const_id, M3AST_AS.Var_id, M3AST_AS.Proc_id, M3AST_AS.Type_id, 
      M3AST_AS.Exc_id =>
      	RETURN TRUE
    ELSE RETURN FALSE;
    END; (* typecase *)
  END TopLevel;


BEGIN

END M3QualNames.
