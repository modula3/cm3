(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

MODULE M3AST_AS_Name;

IMPORT M3AST_AS_F;



PROCEDURE Whitespace(<*UNUSED*> n: M3AST_AS_F.Whitespace): TEXT=
  BEGIN
    RETURN "Whitespace";
  END Whitespace;

PROCEDURE Comment(<*UNUSED*> n: M3AST_AS_F.Comment): TEXT=
  BEGIN
    RETURN "Comment";
  END Comment;

PROCEDURE Pragma(<*UNUSED*> n: M3AST_AS_F.Pragma): TEXT=
  BEGIN
    RETURN "Pragma";
  END Pragma;

PROCEDURE BadChar(<*UNUSED*> n: M3AST_AS_F.BadChar): TEXT=
  BEGIN
    RETURN "BadChar";
  END BadChar;

PROCEDURE Token(<*UNUSED*> n: M3AST_AS_F.Token): TEXT=
  BEGIN
    RETURN "Token";
  END Token;

PROCEDURE Module_id(<*UNUSED*> n: M3AST_AS_F.Module_id): TEXT RAISES {}=
  BEGIN
    RETURN "Module_id";
  END Module_id;


PROCEDURE Interface_id(<*UNUSED*> n: M3AST_AS_F.Interface_id): TEXT RAISES {}=
  BEGIN
    RETURN "Interface_id";
  END Interface_id;


PROCEDURE F_Interface_id(<*UNUSED*> n: M3AST_AS_F.F_Interface_id): TEXT RAISES {}=
  BEGIN
    RETURN "F_Interface_id";
  END F_Interface_id;


PROCEDURE Interface_AS_id(<*UNUSED*> n: M3AST_AS_F.Interface_AS_id): TEXT RAISES {}=
  BEGIN
    RETURN "Interface_AS_id";
  END Interface_AS_id;


PROCEDURE F_Value_id(<*UNUSED*> n: M3AST_AS_F.F_Value_id): TEXT RAISES {}=
  BEGIN
    RETURN "F_Value_id";
  END F_Value_id;


PROCEDURE F_Var_id(<*UNUSED*> n: M3AST_AS_F.F_Var_id): TEXT RAISES {}=
  BEGIN
    RETURN "F_Var_id";
  END F_Var_id;


PROCEDURE F_Readonly_id(<*UNUSED*> n: M3AST_AS_F.F_Readonly_id): TEXT RAISES {}=
  BEGIN
    RETURN "F_Readonly_id";
  END F_Readonly_id;


PROCEDURE Type_id(<*UNUSED*> n: M3AST_AS_F.Type_id): TEXT RAISES {}=
  BEGIN
    RETURN "Type_id";
  END Type_id;


PROCEDURE Const_id(<*UNUSED*> n: M3AST_AS_F.Const_id): TEXT RAISES {}=
  BEGIN
    RETURN "Const_id";
  END Const_id;


PROCEDURE Var_id(<*UNUSED*> n: M3AST_AS_F.Var_id): TEXT RAISES {}=
  BEGIN
    RETURN "Var_id";
  END Var_id;


PROCEDURE Proc_id(<*UNUSED*> n: M3AST_AS_F.Proc_id): TEXT RAISES {}=
  BEGIN
    RETURN "Proc_id";
  END Proc_id;


PROCEDURE Enum_id(<*UNUSED*> n: M3AST_AS_F.Enum_id): TEXT RAISES {}=
  BEGIN
    RETURN "Enum_id";
  END Enum_id;


PROCEDURE Method_id(<*UNUSED*> n: M3AST_AS_F.Method_id): TEXT RAISES {}=
  BEGIN
    RETURN "Method_id";
  END Method_id;


PROCEDURE Override_id(<*UNUSED*> n: M3AST_AS_F.Override_id): TEXT RAISES {}=
  BEGIN
    RETURN "Override_id";
  END Override_id;

PROCEDURE Field_id(<*UNUSED*> n: M3AST_AS_F.Field_id): TEXT RAISES {}=
  BEGIN
    RETURN "Field_id";
  END Field_id;


PROCEDURE For_id(<*UNUSED*> n: M3AST_AS_F.For_id): TEXT RAISES {}=
  BEGIN
    RETURN "For_id";
  END For_id;


PROCEDURE Handler_id(<*UNUSED*> n: M3AST_AS_F.Handler_id): TEXT RAISES {}=
  BEGIN
    RETURN "Handler_id";
  END Handler_id;


PROCEDURE Tcase_id(<*UNUSED*> n: M3AST_AS_F.Tcase_id): TEXT RAISES {}=
  BEGIN
    RETURN "Tcase_id";
  END Tcase_id;


PROCEDURE With_id(<*UNUSED*> n: M3AST_AS_F.With_id): TEXT RAISES {}=
  BEGIN
    RETURN "With_id";
  END With_id;


PROCEDURE Exc_id(<*UNUSED*> n: M3AST_AS_F.Exc_id): TEXT RAISES {}=
  BEGIN
    RETURN "Exc_id";
  END Exc_id;


PROCEDURE Used_interface_id(<*UNUSED*> n: M3AST_AS_F.Used_interface_id): TEXT RAISES {}=
  BEGIN
    RETURN "Used_interface_id";
  END Used_interface_id;


PROCEDURE Used_def_id(<*UNUSED*> n: M3AST_AS_F.Used_def_id): TEXT RAISES {}=
  BEGIN
    RETURN "Used_def_id";
  END Used_def_id;


PROCEDURE Qual_used_id(<*UNUSED*> n: M3AST_AS_F.Qual_used_id): TEXT RAISES {}=
  BEGIN
    RETURN "Qual_used_id";
  END Qual_used_id;


PROCEDURE Compilation_Unit(<*UNUSED*> n: M3AST_AS_F.Compilation_Unit): TEXT RAISES {}=
  BEGIN
    RETURN "Compilation_Unit";
  END Compilation_Unit;


PROCEDURE Interface(<*UNUSED*> n: M3AST_AS_F.Interface): TEXT RAISES {}=
  BEGIN
    RETURN "Interface";
  END Interface;


PROCEDURE Module(<*UNUSED*> n: M3AST_AS_F.Module): TEXT RAISES {}=
  BEGIN
    RETURN "Module";
  END Module;


PROCEDURE Interface_gen_def(<*UNUSED*> n: M3AST_AS_F.Interface_gen_def): TEXT RAISES {}=
  BEGIN
    RETURN "Interface_gen_def";
  END Interface_gen_def;


PROCEDURE Module_gen_def(<*UNUSED*> n: M3AST_AS_F.Module_gen_def): TEXT RAISES {}=
  BEGIN
    RETURN "Module_gen_def";
  END Module_gen_def;


PROCEDURE Interface_gen_ins(<*UNUSED*> n: M3AST_AS_F.Interface_gen_ins): TEXT RAISES {}=
  BEGIN
    RETURN "Interface_gen_ins";
  END Interface_gen_ins;


PROCEDURE Module_gen_ins(<*UNUSED*> n: M3AST_AS_F.Module_gen_ins): TEXT RAISES {}=
  BEGIN
    RETURN "Module_gen_ins";
  END Module_gen_ins;


PROCEDURE Unsafe(<*UNUSED*> n: M3AST_AS_F.Unsafe): TEXT RAISES {}=
  BEGIN
    RETURN "Unsafe";
  END Unsafe;


PROCEDURE Import_item(<*UNUSED*> n: M3AST_AS_F.Import_item): TEXT RAISES {}=
  BEGIN
    RETURN "Import_item";
  END Import_item;


PROCEDURE Simple_import(<*UNUSED*> n: M3AST_AS_F.Simple_import): TEXT RAISES {}=
  BEGIN
    RETURN "Simple_import";
  END Simple_import;


PROCEDURE From_import(<*UNUSED*> n: M3AST_AS_F.From_import): TEXT RAISES {}=
  BEGIN
    RETURN "From_import";
  END From_import;


PROCEDURE Revelation_s(<*UNUSED*> n: M3AST_AS_F.Revelation_s): TEXT RAISES {}=
  BEGIN
    RETURN "Revelation_s";
  END Revelation_s;


PROCEDURE Const_decl_s(<*UNUSED*> n: M3AST_AS_F.Const_decl_s): TEXT RAISES {}=
  BEGIN
    RETURN "Const_decl_s";
  END Const_decl_s;


PROCEDURE Type_decl_s(<*UNUSED*> n: M3AST_AS_F.Type_decl_s): TEXT RAISES {}=
  BEGIN
    RETURN "Type_decl_s";
  END Type_decl_s;


PROCEDURE Var_decl_s(<*UNUSED*> n: M3AST_AS_F.Var_decl_s): TEXT RAISES {}=
  BEGIN
    RETURN "Var_decl_s";
  END Var_decl_s;


PROCEDURE Exc_decl_s(<*UNUSED*> n: M3AST_AS_F.Exc_decl_s): TEXT RAISES {}=
  BEGIN
    RETURN "Exc_decl_s";
  END Exc_decl_s;


PROCEDURE Proc_decl(<*UNUSED*> n: M3AST_AS_F.Proc_decl): TEXT RAISES {}=
  BEGIN
    RETURN "Proc_decl";
  END Proc_decl;


PROCEDURE Const_decl(<*UNUSED*> n: M3AST_AS_F.Const_decl): TEXT RAISES {}=
  BEGIN
    RETURN "Const_decl";
  END Const_decl;


PROCEDURE Var_decl(<*UNUSED*> n: M3AST_AS_F.Var_decl): TEXT RAISES {}=
  BEGIN
    RETURN "Var_decl";
  END Var_decl;


PROCEDURE Exc_decl(<*UNUSED*> n: M3AST_AS_F.Exc_decl): TEXT RAISES {}=
  BEGIN
    RETURN "Exc_decl";
  END Exc_decl;


PROCEDURE Subtype_decl(<*UNUSED*> n: M3AST_AS_F.Subtype_decl): TEXT RAISES {}=
  BEGIN
    RETURN "Subtype_decl";
  END Subtype_decl;


PROCEDURE Concrete_decl(<*UNUSED*> n: M3AST_AS_F.Concrete_decl): TEXT RAISES {}=
  BEGIN
    RETURN "Concrete_decl";
  END Concrete_decl;


PROCEDURE Subtype_reveal(<*UNUSED*> n: M3AST_AS_F.Subtype_reveal): TEXT RAISES {}=
  BEGIN
    RETURN "Subtype_reveal";
  END Subtype_reveal;


PROCEDURE Concrete_reveal(<*UNUSED*> n: M3AST_AS_F.Concrete_reveal): TEXT RAISES {}=
  BEGIN
    RETURN "Concrete_reveal";
  END Concrete_reveal;


PROCEDURE Named_type(<*UNUSED*> n: M3AST_AS_F.Named_type): TEXT RAISES {}=
  BEGIN
    RETURN "Named_type";
  END Named_type;


PROCEDURE Integer_type(<*UNUSED*> n: M3AST_AS_F.Integer_type): TEXT RAISES {}=
  BEGIN
    RETURN "Integer_type";
  END Integer_type;


PROCEDURE Real_type(<*UNUSED*> n: M3AST_AS_F.Real_type): TEXT RAISES {}=
  BEGIN
    RETURN "Real_type";
  END Real_type;


PROCEDURE LongReal_type(<*UNUSED*> n: M3AST_AS_F.LongReal_type): TEXT RAISES {}=
  BEGIN
    RETURN "LongReal_type";
  END LongReal_type;


PROCEDURE Extended_type(<*UNUSED*> n: M3AST_AS_F.Extended_type): TEXT RAISES {}=
  BEGIN
    RETURN "Extended_type";
  END Extended_type;


PROCEDURE Null_type(<*UNUSED*> n: M3AST_AS_F.Null_type): TEXT RAISES {}=
  BEGIN
    RETURN "Null_type";
  END Null_type;


PROCEDURE RefAny_type(<*UNUSED*> n: M3AST_AS_F.RefAny_type): TEXT RAISES {}=
  BEGIN
    RETURN "RefAny_type";
  END RefAny_type;


PROCEDURE Address_type(<*UNUSED*> n: M3AST_AS_F.Address_type): TEXT RAISES {}=
  BEGIN
    RETURN "Address_type";
  END Address_type;


PROCEDURE Root_type(<*UNUSED*> n: M3AST_AS_F.Root_type): TEXT RAISES {}=
  BEGIN
    RETURN "Root_type";
  END Root_type;


PROCEDURE Enumeration_type(<*UNUSED*> n: M3AST_AS_F.Enumeration_type): TEXT RAISES {}=
  BEGIN
    RETURN "Enumeration_type";
  END Enumeration_type;


PROCEDURE Subrange_type(<*UNUSED*> n: M3AST_AS_F.Subrange_type): TEXT RAISES {}=
  BEGIN
    RETURN "Subrange_type";
  END Subrange_type;


PROCEDURE Array_type(<*UNUSED*> n: M3AST_AS_F.Array_type): TEXT RAISES {}=
  BEGIN
    RETURN "Array_type";
  END Array_type;


PROCEDURE Record_type(<*UNUSED*> n: M3AST_AS_F.Record_type): TEXT RAISES {}=
  BEGIN
    RETURN "Record_type";
  END Record_type;


PROCEDURE Object_type(<*UNUSED*> n: M3AST_AS_F.Object_type): TEXT RAISES {}=
  BEGIN
    RETURN "Object_type";
  END Object_type;


PROCEDURE Set_type(<*UNUSED*> n: M3AST_AS_F.Set_type): TEXT RAISES {}=
  BEGIN
    RETURN "Set_type";
  END Set_type;


PROCEDURE Procedure_type(<*UNUSED*> n: M3AST_AS_F.Procedure_type): TEXT RAISES {}=
  BEGIN
    RETURN "Procedure_type";
  END Procedure_type;


PROCEDURE Ref_type(<*UNUSED*> n: M3AST_AS_F.Ref_type): TEXT RAISES {}=
  BEGIN
    RETURN "Ref_type";
  END Ref_type;


PROCEDURE Packed_type(<*UNUSED*> n: M3AST_AS_F.Packed_type): TEXT RAISES {}=
  BEGIN
    RETURN "Packed_type";
  END Packed_type;


PROCEDURE Opaque_type(<*UNUSED*> n: M3AST_AS_F.Opaque_type): TEXT RAISES {}=
  BEGIN
    RETURN "Opaque_type";
  END Opaque_type;


PROCEDURE Brand(<*UNUSED*> n: M3AST_AS_F.Brand): TEXT RAISES {}=
  BEGIN
    RETURN "Brand";
  END Brand;


PROCEDURE Untraced(<*UNUSED*> n: M3AST_AS_F.Untraced): TEXT RAISES {}=
  BEGIN
    RETURN "Untraced";
  END Untraced;


PROCEDURE Fields(<*UNUSED*> n: M3AST_AS_F.Fields): TEXT RAISES {}=
  BEGIN
    RETURN "Fields";
  END Fields;


PROCEDURE Method(<*UNUSED*> n: M3AST_AS_F.Method): TEXT RAISES {}=
  BEGIN
    RETURN "Method";
  END Method;


PROCEDURE Override(<*UNUSED*> n: M3AST_AS_F.Override): TEXT RAISES {}=
  BEGIN
    RETURN "Override";
  END Override;


PROCEDURE Formal_param(<*UNUSED*> n: M3AST_AS_F.Formal_param): TEXT RAISES {}=
  BEGIN
    RETURN "Formal_param";
  END Formal_param;


PROCEDURE Raisees_some(<*UNUSED*> n: M3AST_AS_F.Raisees_some): TEXT RAISES {}=
  BEGIN
    RETURN "Raisees_some";
  END Raisees_some;


PROCEDURE Raisees_any(<*UNUSED*> n: M3AST_AS_F.Raisees_any): TEXT RAISES {}=
  BEGIN
    RETURN "Raisees_any";
  END Raisees_any;


PROCEDURE Range_EXP(<*UNUSED*> n: M3AST_AS_F.Range_EXP): TEXT RAISES {}=
  BEGIN
    RETURN "Range_EXP";
  END Range_EXP;


PROCEDURE Range(<*UNUSED*> n: M3AST_AS_F.Range): TEXT RAISES {}=
  BEGIN
    RETURN "Range";
  END Range;


PROCEDURE Integer_literal(<*UNUSED*> n: M3AST_AS_F.Integer_literal): TEXT RAISES {}=
  BEGIN
    RETURN "Integer_literal";
  END Integer_literal;


PROCEDURE Real_literal(<*UNUSED*> n: M3AST_AS_F.Real_literal): TEXT RAISES {}=
  BEGIN
    RETURN "Real_literal";
  END Real_literal;


PROCEDURE LongReal_literal(<*UNUSED*> n: M3AST_AS_F.LongReal_literal): TEXT RAISES {}=
  BEGIN
    RETURN "LongReal_literal";
  END LongReal_literal;


PROCEDURE Extended_literal(<*UNUSED*> n: M3AST_AS_F.Extended_literal): TEXT RAISES {}=
  BEGIN
    RETURN "Extended_literal";
  END Extended_literal;


PROCEDURE Text_literal(<*UNUSED*> n: M3AST_AS_F.Text_literal): TEXT RAISES {}=
  BEGIN
    RETURN "Text_literal";
  END Text_literal;


PROCEDURE Char_literal(<*UNUSED*> n: M3AST_AS_F.Char_literal): TEXT RAISES {}=
  BEGIN
    RETURN "Char_literal";
  END Char_literal;


PROCEDURE Nil_literal(<*UNUSED*> n: M3AST_AS_F.Nil_literal): TEXT RAISES {}=
  BEGIN
    RETURN "Nil_literal";
  END Nil_literal;


PROCEDURE Exp_used_id(<*UNUSED*> n: M3AST_AS_F.Exp_used_id): TEXT RAISES {}=
  BEGIN
    RETURN "Exp_used_id";
  END Exp_used_id;


PROCEDURE Constructor(<*UNUSED*> n: M3AST_AS_F.Constructor): TEXT RAISES {}=
  BEGIN
    RETURN "Constructor";
  END Constructor;


PROCEDURE RANGE_EXP_elem(<*UNUSED*> n: M3AST_AS_F.RANGE_EXP_elem): TEXT RAISES {}=
  BEGIN
    RETURN "RANGE_EXP_elem";
  END RANGE_EXP_elem;


PROCEDURE Actual_elem(<*UNUSED*> n: M3AST_AS_F.Actual_elem): TEXT RAISES {}=
  BEGIN
    RETURN "Actual_elem";
  END Actual_elem;


PROCEDURE Propagate(<*UNUSED*> n: M3AST_AS_F.Propagate): TEXT RAISES {}=
  BEGIN
    RETURN "Propagate";
  END Propagate;


PROCEDURE Plus(<*UNUSED*> n: M3AST_AS_F.Plus): TEXT RAISES {}=
  BEGIN
    RETURN "Plus";
  END Plus;


PROCEDURE Minus(<*UNUSED*> n: M3AST_AS_F.Minus): TEXT RAISES {}=
  BEGIN
    RETURN "Minus";
  END Minus;


PROCEDURE Times(<*UNUSED*> n: M3AST_AS_F.Times): TEXT RAISES {}=
  BEGIN
    RETURN "Times";
  END Times;


PROCEDURE Rdiv(<*UNUSED*> n: M3AST_AS_F.Rdiv): TEXT RAISES {}=
  BEGIN
    RETURN "Rdiv";
  END Rdiv;


PROCEDURE Textcat(<*UNUSED*> n: M3AST_AS_F.Textcat): TEXT RAISES {}=
  BEGIN
    RETURN "Textcat";
  END Textcat;


PROCEDURE Div(<*UNUSED*> n: M3AST_AS_F.Div): TEXT RAISES {}=
  BEGIN
    RETURN "Div";
  END Div;


PROCEDURE Mod(<*UNUSED*> n: M3AST_AS_F.Mod): TEXT RAISES {}=
  BEGIN
    RETURN "Mod";
  END Mod;


PROCEDURE Eq(<*UNUSED*> n: M3AST_AS_F.Eq): TEXT RAISES {}=
  BEGIN
    RETURN "Eq";
  END Eq;


PROCEDURE Ne(<*UNUSED*> n: M3AST_AS_F.Ne): TEXT RAISES {}=
  BEGIN
    RETURN "Ne";
  END Ne;


PROCEDURE Gt(<*UNUSED*> n: M3AST_AS_F.Gt): TEXT RAISES {}=
  BEGIN
    RETURN "Gt";
  END Gt;


PROCEDURE Lt(<*UNUSED*> n: M3AST_AS_F.Lt): TEXT RAISES {}=
  BEGIN
    RETURN "Lt";
  END Lt;


PROCEDURE Ge(<*UNUSED*> n: M3AST_AS_F.Ge): TEXT RAISES {}=
  BEGIN
    RETURN "Ge";
  END Ge;


PROCEDURE Le(<*UNUSED*> n: M3AST_AS_F.Le): TEXT RAISES {}=
  BEGIN
    RETURN "Le";
  END Le;


PROCEDURE And(<*UNUSED*> n: M3AST_AS_F.And): TEXT RAISES {}=
  BEGIN
    RETURN "And";
  END And;


PROCEDURE Or(<*UNUSED*> n: M3AST_AS_F.Or): TEXT RAISES {}=
  BEGIN
    RETURN "Or";
  END Or;


PROCEDURE In(<*UNUSED*> n: M3AST_AS_F.In): TEXT RAISES {}=
  BEGIN
    RETURN "In";
  END In;


PROCEDURE Select(<*UNUSED*> n: M3AST_AS_F.Select): TEXT RAISES {}=
  BEGIN
    RETURN "Select";
  END Select;


PROCEDURE Not(<*UNUSED*> n: M3AST_AS_F.Not): TEXT RAISES {}=
  BEGIN
    RETURN "Not";
  END Not;


PROCEDURE Unaryplus(<*UNUSED*> n: M3AST_AS_F.Unaryplus): TEXT RAISES {}=
  BEGIN
    RETURN "Unaryplus";
  END Unaryplus;


PROCEDURE Unaryminus(<*UNUSED*> n: M3AST_AS_F.Unaryminus): TEXT RAISES {}=
  BEGIN
    RETURN "Unaryminus";
  END Unaryminus;


PROCEDURE Deref(<*UNUSED*> n: M3AST_AS_F.Deref): TEXT RAISES {}=
  BEGIN
    RETURN "Deref";
  END Deref;


PROCEDURE Call(<*UNUSED*> n: M3AST_AS_F.Call): TEXT RAISES {}=
  BEGIN
    RETURN "Call";
  END Call;


PROCEDURE NEWCall(<*UNUSED*> n: M3AST_AS_F.NEWCall): TEXT RAISES {}=
  BEGIN
    RETURN "NEWCall";
  END NEWCall;


PROCEDURE Index(<*UNUSED*> n: M3AST_AS_F.Index): TEXT RAISES {}=
  BEGIN
    RETURN "Index";
  END Index;


PROCEDURE Actual(<*UNUSED*> n: M3AST_AS_F.Actual): TEXT RAISES {}=
  BEGIN
    RETURN "Actual";
  END Actual;


PROCEDURE Assign_st(<*UNUSED*> n: M3AST_AS_F.Assign_st): TEXT RAISES {}=
  BEGIN
    RETURN "Assign_st";
  END Assign_st;


PROCEDURE Call_st(<*UNUSED*> n: M3AST_AS_F.Call_st): TEXT RAISES {}=
  BEGIN
    RETURN "Call_st";
  END Call_st;


PROCEDURE Case_st(<*UNUSED*> n: M3AST_AS_F.Case_st): TEXT RAISES {}=
  BEGIN
    RETURN "Case_st";
  END Case_st;


PROCEDURE Eval_st(<*UNUSED*> n: M3AST_AS_F.Eval_st): TEXT RAISES {}=
  BEGIN
    RETURN "Eval_st";
  END Eval_st;


PROCEDURE Exit_st(<*UNUSED*> n: M3AST_AS_F.Exit_st): TEXT RAISES {}=
  BEGIN
    RETURN "Exit_st";
  END Exit_st;


PROCEDURE For_st(<*UNUSED*> n: M3AST_AS_F.For_st): TEXT RAISES {}=
  BEGIN
    RETURN "For_st";
  END For_st;


PROCEDURE If_st(<*UNUSED*> n: M3AST_AS_F.If_st): TEXT RAISES {}=
  BEGIN
    RETURN "If_st";
  END If_st;


PROCEDURE Lock_st(<*UNUSED*> n: M3AST_AS_F.Lock_st): TEXT RAISES {}=
  BEGIN
    RETURN "Lock_st";
  END Lock_st;


PROCEDURE Loop_st(<*UNUSED*> n: M3AST_AS_F.Loop_st): TEXT RAISES {}=
  BEGIN
    RETURN "Loop_st";
  END Loop_st;


PROCEDURE Raise_st(<*UNUSED*> n: M3AST_AS_F.Raise_st): TEXT RAISES {}=
  BEGIN
    RETURN "Raise_st";
  END Raise_st;


PROCEDURE Repeat_st(<*UNUSED*> n: M3AST_AS_F.Repeat_st): TEXT RAISES {}=
  BEGIN
    RETURN "Repeat_st";
  END Repeat_st;


PROCEDURE Return_st(<*UNUSED*> n: M3AST_AS_F.Return_st): TEXT RAISES {}=
  BEGIN
    RETURN "Return_st";
  END Return_st;


PROCEDURE Try_st(<*UNUSED*> n: M3AST_AS_F.Try_st): TEXT RAISES {}=
  BEGIN
    RETURN "Try_st";
  END Try_st;


PROCEDURE Typecase_st(<*UNUSED*> n: M3AST_AS_F.Typecase_st): TEXT RAISES {}=
  BEGIN
    RETURN "Typecase_st";
  END Typecase_st;


PROCEDURE While_st(<*UNUSED*> n: M3AST_AS_F.While_st): TEXT RAISES {}=
  BEGIN
    RETURN "While_st";
  END While_st;


PROCEDURE With_st(<*UNUSED*> n: M3AST_AS_F.With_st): TEXT RAISES {}=
  BEGIN
    RETURN "With_st";
  END With_st;


PROCEDURE Block(<*UNUSED*> n: M3AST_AS_F.Block): TEXT RAISES {}=
  BEGIN
    RETURN "Block";
  END Block;


PROCEDURE Case(<*UNUSED*> n: M3AST_AS_F.Case): TEXT RAISES {}=
  BEGIN
    RETURN "Case";
  END Case;


PROCEDURE Else_stm(<*UNUSED*> n: M3AST_AS_F.Else_stm): TEXT RAISES {}=
  BEGIN
    RETURN "Else_stm";
  END Else_stm;


PROCEDURE Elsif(<*UNUSED*> n: M3AST_AS_F.Elsif): TEXT RAISES {}=
  BEGIN
    RETURN "Elsif";
  END Elsif;


PROCEDURE Try_except(<*UNUSED*> n: M3AST_AS_F.Try_except): TEXT RAISES {}=
  BEGIN
    RETURN "Try_except";
  END Try_except;


PROCEDURE Try_finally(<*UNUSED*> n: M3AST_AS_F.Try_finally): TEXT RAISES {}=
  BEGIN
    RETURN "Try_finally";
  END Try_finally;


PROCEDURE Tcase(<*UNUSED*> n: M3AST_AS_F.Tcase): TEXT RAISES {}=
  BEGIN
    RETURN "Tcase";
  END Tcase;


PROCEDURE Handler(<*UNUSED*> n: M3AST_AS_F.Handler): TEXT RAISES {}=
  BEGIN
    RETURN "Handler";
  END Handler;


PROCEDURE Binding(<*UNUSED*> n: M3AST_AS_F.Binding): TEXT RAISES {}=
  BEGIN
    RETURN "Binding";
  END Binding;


PROCEDURE By(<*UNUSED*> n: M3AST_AS_F.By): TEXT RAISES {}=
  BEGIN
    RETURN "By";
  END By;


PROCEDURE Bad_EXP(<*UNUSED*> n: M3AST_AS_F.Bad_EXP): TEXT RAISES {}=
  BEGIN
    RETURN "Bad_EXP";
  END Bad_EXP;


PROCEDURE Bad_M3TYPE(<*UNUSED*> n: M3AST_AS_F.Bad_M3TYPE): TEXT RAISES {}=
  BEGIN
    RETURN "Bad_M3TYPE";
  END Bad_M3TYPE;


PROCEDURE Bad_STM(<*UNUSED*> n: M3AST_AS_F.Bad_STM): TEXT RAISES {}=
  BEGIN
    RETURN "Bad_STM";
  END Bad_STM;

BEGIN
END M3AST_AS_Name.
