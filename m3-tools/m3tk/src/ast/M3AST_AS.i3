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
(**)
(* Copyright (C) 1991, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* "M3AST_AS" is the syntactic layer of the Modula-3 AST specification. *)

INTERFACE M3AST_AS;

IMPORT M3AST_LX;

<* PRAGMA FIELDS *>

TYPE
   SRC_NODE = M3AST_LX.SRC_NODE;
   SRC_NODE_C = M3AST_LX.SRC_NODE_C;

(* To support clients of previous versions of this interface, "SRC_NODE"
is passed through by this interface. *)

(* \subsection{Identifiers} *)

TYPE
   ID = M3AST_LX.ID;

(* The subtypes of the "ID" class are partitioned into two disjoint sets,
those which correspond to identifers that are definitions ("DEF_ID")
and those which are correspond to uses ("USED_ID"). Unlike the
concrete syntax, which does not distinguish identifier definitions,
each construct that can introduce a new identifier definition has an
aassociated, unique, subtype, e.g.  "Proc_id" for identifiers
associated with "PROCEDURE" declarations. *)

(* \subsubsection{Definitions} *)

  DEF_ID <: ID;
  DEF_ID_NULL = DEF_ID;

  UNIT_ID <: DEF_ID;               (* INTERFACE or MODULE *)
  Module_id <: UNIT_ID;            (* MODULE m *)
  Interface_id <: UNIT_ID;         (* INTERFACE i *)
  Interface_AS_id <: DEF_ID;       (* the J in IMPORT I AS J *)
  F_Interface_id <: DEF_ID;        (* generic formal *)

(* The class "TYPED_ID" is introduced as a placeholder for semantic
information, to mitigate the lack of multiple inheritance.  All the
identifiers below are subtypes of "TYPED_ID". *)

  TYPED_ID <: DEF_ID;
  FORMAL_ID <: TYPED_ID;           (* procedure formals *)
  F_Value_id <: FORMAL_ID;         (* VALUE v *)
  F_Var_id <: FORMAL_ID;           (* VAR v *)
  F_Readonly_id <: FORMAL_ID;      (* READONLY v *)

  Type_id <: TYPED_ID;             (* TYPE T *)
  Const_id <: TYPED_ID;            (* CONST C *)
  Var_id <: TYPED_ID;              (* VAR v (in blocks) *) 
  Proc_id <: TYPED_ID;             (* PROCEDURE P *)
  Enum_id <: TYPED_ID;             (* {Red,Green,Blue} *)
  METHOD_OVERRIDE_ID <: TYPED_ID;
  Method_id <: METHOD_OVERRIDE_ID; (* METHODS m *)
  Override_id <: METHOD_OVERRIDE_ID;  (* OVERRIDES m *)            
  Field_id <: TYPED_ID;            (* in RECORD/OBJECT *)
  For_id <: TYPED_ID;              (* FOR i *)
  Handler_id <: TYPED_ID;          (* EXCEPT E(v) *)
  Handler_id_NULL = Handler_id;
  Tcase_id <: TYPED_ID;            (* TYPECASE ... T(v) *)
  Tcase_id_NULL = Tcase_id;
  With_id <: TYPED_ID;             (* WITH b *)
  Exc_id <: TYPED_ID;              (* EXCEPTION E(T) *)

(* \subsubsection{Uses}
Identifier uses are separated into three distinct subtypes.
Firstly, there are several cases where the binding of the use is
required by the language definition to be to an "Interface_id", e.g.
"IMPORT I". To improve readability, such uses are denoted by the
subtype "Used_interface_id".  Secondly, there are occurrences such as
the identifier "N" in a "FROM I IMPORT N". These are denoted by the
subtype "Used_def_id". Finally, there are identifiers that can occur
in expressions. Here we have a problem, since such identifiers also
need to be a subtype of the class that denotes expressions ("EXP").
The solution, of which more later, is to make these a subtype of
"EXP", and call them "Exp_used_id". *)

  USED_ID <: ID;
  Used_interface_id <: USED_ID;
  Used_interface_id_NULL = Used_interface_id;
  Used_def_id <: USED_ID;

(* Qualified identifiers, e.g. "I.B", can also appear in both
expression and non-expression contexts. In the former case, it is not
known after syntax analysis, whether a construct of the form "a.b"
denotes a qualified identifier or not, until the binding for "a" is
resolved. In the non-expression case, e.g. in "REVEAL I.T = ...", a
qualified identifier is denoted by a separate node containing two
children of class "USED_ID". Note that the "interface" component of
such a node can be empty, denoted by NIL. *)

  Qual_used_id <: SRC_NODE_C;
  Qual_used_id_NULL = Qual_used_id; 
  <* FIELDS OF Qual_used_id
       as_intf_id: Used_interface_id_NULL;
       as_id: Used_def_id; *>


(* To support the {\em multiple inheritance} of the "ID", or "USED_ID"
classes, methods are provided to enquire whether any "M3AST.NODE" instance
is also a member of these classes. See, for example,
"Exp_used_id" in the expressions section. These methods are actually
revealed in the representation interface, e.g. "M3AST_AS_F".

|     METHODS
|       IsA_USED_ID(VAR (*out*) used_id): BOOLEAN;
|       IsA_ID(VAR (*out*) id): BOOLEAN; *)


(* \subsection{Compilation Units}

In order to provide a node in which to place to miscellaneous
attributes, e.g. compilation status, the AST is rooted in a node
called "Compilation_Unit", which has no direct counterpart in the
concrete syntax. *)

  Compilation_Unit <: SRC_NODE_C;
  <* FIELDS OF Compilation_Unit
     as_root: UNIT; *>

(* The class that corresponds to the non-terminal named "Compilation"
in the concrete grammar is called "UNIT", and it has subtypes to
denote generic definitions, generic instantiations and "normal"
interfaces and modules. Multiple inheritance would simplify the
structure here also, but in this case, it is simplest to simply repeat
(multiply inherited) attributes in the subtypes.  The "UNIT" class
carries the attribute denoting the identifier that is common to all
forms of "UNIT". *)

  UNIT <: SRC_NODE_C;
  <* FIELDS OF UNIT
     as_id: UNIT_ID; *>

(* Generic definitions and {\it normal} units both have {\it bodies}, i.e.
imports and declarations, and these are associated with the class
"UNIT_WITH_BODY". *)

  UNIT_WITH_BODY <: UNIT;
  <* FIELDS OF UNIT_WITH_BODY
     as_import_s: SEQUENCE OF IMPORTED;
     as_block: Block *>

(* Generic definitions contain a list of formal parameters, and this
is captured by the "UNIT_GEN_DEF" class. *)

  UNIT_GEN_DEF <: UNIT_WITH_BODY;
  <* FIELDS OF UNIT_GEN_DEF
     as_id_s: SEQUENCE OF F_Interface_id; *>

  Interface_gen_def <: UNIT_GEN_DEF;

  Module_gen_def <: UNIT_GEN_DEF;

(* Normal interfaces and modules can be "UNSAFE". (as can generic
instantiations, but this example of multiple inheritance is not
captured in the type hierarchy. *)

  UNIT_NORMAL <: UNIT_WITH_BODY;
  <* FIELDS OF UNIT_NORMAL
    as_unsafe: Unsafe_NULL; *>

  Interface <: UNIT_NORMAL;

(* Modules can have an EXPORT list (as can generically instantiated
modules, but this example of multiple inheritance is not captured in
the type hierarchy). *)

  Module <: UNIT_NORMAL;
  <* FIELDS OF Module
    as_export_s: SEQUENCE OF Used_interface_id; *>

(* Generic instantiations can be "UNSAFE", contain an identifier that
refers to the generic definition and a list of actual parameters.
These identifier occurrences must all bind to "Interface_id"s. *)

  UNIT_GEN_INS <: UNIT;
  <* FIELDS OF UNIT_GEN_INS
     as_unsafe: Unsafe_NULL;
     as_gen_id: Used_interface_id;
     as_id_s: SEQUENCE OF Used_interface_id *>

  Interface_gen_ins <: UNIT_GEN_INS;

  Module_gen_ins <: UNIT_GEN_INS;
  <* FIELDS OF Module_gen_ins
    as_export_s: SEQUENCE OF Used_interface_id; *>

(* "UNSAFE" is represented as node with no children. In order to record the
actual lexical token, it is declared as a subtype of "SRC_NODE_C". The
"lx_node_s" will contain a single "Token" element. *)

  Unsafe <: SRC_NODE_C;
  Unsafe_NULL = Unsafe;

(* Imports fall into two classes, denoted by "AsImport" and
"FromImport" in the concrete syntax. The class "IMPORTED" is used in
the AST to denote the choice, with subtypes "Simple_import" and
"From_import". *)

  IMPORTED <: SRC_NODE_C;
  Simple_import <: IMPORTED;
  <* FIELDS OF Simple_import
    as_import_item_s: SEQUENCE OF Import_item; *>

(* An "Import_item" node corresponds directly to "ImportItem" in the 
   concrete syntax *)

  Import_item <: SRC_NODE_C;
  <* FIELDS OF Import_item
     as_intf_id: Used_interface_id;
     as_id: Interface_AS_id; *> 

  From_import <: IMPORTED;
  <* FIELDS OF From_import
     as_intf_id: Used_interface_id;
     as_id_s: SEQUENCE OF Used_def_id; *>

(* \subsection{Declarations and Revelations}
The concrete syntax groups revelations under the "Decl" rules.  In
the AST we introduce a class "DECL_REVL" to handle either, and two
subtypes "DECL" and "Revelation_s" *)

  DECL_REVL <: SRC_NODE_C;

(* Declarations are somewhat tedious to represent, since each
occurrence of, say, "CONST", can introduce several actual declarations.
Accordingly, we introduce nodes of the form "X_s", which carry a
sequence attribute. *)

  DECL <: DECL_REVL;

  Const_decl_s <: DECL;            (* CONST ... *)
  <* FIELDS OF Const_decl_s
     as_const_decl_s: SEQUENCE OF Const_decl *>

  Type_decl_s <: DECL;             (* TYPE ... *)
  <* FIELDS OF Type_decl_s
     as_type_decl_s: SEQUENCE OF TYPE_DECL *>

  Var_decl_s <: DECL;              (* VAR ... *)
  <* FIELDS OF Var_decl_s
     as_var_decl_s: SEQUENCE OF Var_decl *>

  Exc_decl_s <: DECL;              (* EXCEPTION ... *)
  <* FIELDS OF Exc_decl_s
     as_exc_decl_s: SEQUENCE OF Exc_decl *>

(* "Proc_decl" is the exception to the rule. The "as_body" attribute
will be "NIL" in an AST that represents an interface.  *)

  Proc_decl <: DECL;
  <* FIELDS OF Proc_decl
    as_id: Proc_id;
    as_type: Procedure_type;
    as_body: Block_NULL; *>

(* Now the declarations proper. Note that the identifer in each node
is of the appropriate type, e.g. "Const_id" for a "Const_decl". *)

  Const_decl <: SRC_NODE_C;
  <* FIELDS OF Const_decl
     as_id: Const_id;
     as_type: M3TYPE_NULL;
     as_exp: EXP; *>

(* Type declarations are either opaque (subtype) or concrete. The
class "TYPE_DECL" captures this. *)

  TYPE_DECL <: SRC_NODE_C;
  <* FIELDS OF TYPE_DECL
     as_id: Type_id;
     as_type: M3TYPE; *>

(* The "as_type" attribute for a "Subtype_decl" is always an
"Opaque_type" node (see below). The value of "U" is encoded as an
attribute of the "Opaque_type" node. *)

  Subtype_decl <: TYPE_DECL;       (* TYPE T <: U *)
  Concrete_decl <: TYPE_DECL;      (* TYPE T = U *)

(* Variable declarations are unusual in that several identifiers can
be introduced in the same declaration. Note that either the "as_type"
attribute or the "as_default" attribute may be "NIL", but not both.  The
latter constraint is not expressed in the AST. *)

  Var_decl <: SRC_NODE_C;
  <* FIELDS OF Var_decl
     as_id_s: SEQUENCE OF Var_id;
     as_type: M3TYPE_NULL;
     as_default: EXP_NULL; *>

  Exc_decl <: SRC_NODE_C;
  <* FIELDS OF Exc_decl
     as_id: Exc_id;
     as_type: M3TYPE_NULL; *>
 
  Revelation_s <: DECL_REVL;
  <* FIELDS OF Revelation_s
    as_reveal_s: SEQUENCE OF REVELATION; *>

(* Like type declarations, revelations can be opaque (subtype) or concrete. *)

  REVELATION <: SRC_NODE_C;
  <* FIELDS OF REVELATION
     as_qual_id: Qual_used_id;
     as_type: M3TYPE; *>

  Subtype_reveal <: REVELATION;    (* REVEAL T <: U *)
  Concrete_reveal <: REVELATION;   (* REVEAL T = U *)

(* \subsubsection{Type Productions} *)

(* There are situations where an attribute can be either a type or an
expression, so we define the class "EXP_TYPE" to denote that choice. *)

  EXP_TYPE <: SRC_NODE_C;

(* Types can appear in the AST as type constructions or as qualified
identifiers (which must ultimately bind to a name declared as a
"Type_id". The class "M3TYPE" is used to denote this choice. *)

  M3TYPE <: EXP_TYPE;
  M3TYPE_NULL = M3TYPE;

  Named_type <: M3TYPE;
  <* FIELDS OF Named_type
    as_qual_id: Qual_used_id; *>

(* Type constructions (specifications) are grouped under the class
"TYPE_SPEC" *)

  TYPE_SPEC <: M3TYPE;

(* The following built-in types are primitive. Others, such as
"BOOLEAN", are expressed as instances of the apropriate "TYPE_SPEC"
subtype ("Enumeration_type" for "BOOLEAN"). *)

(* For convenience all the floating types are grouped under "FLOAT_TYPE" *)

  FLOAT_TYPE <: TYPE_SPEC;
  Real_type <: FLOAT_TYPE;               (* REAL *)        
  LongReal_type <: FLOAT_TYPE;           (* LONGREAL *)
  Extended_type <: FLOAT_TYPE;           (* EXTENDED *)

  Integer_type <: TYPE_SPEC;             (* INTEGER *)
  Null_type <: TYPE_SPEC;                (* NULL *)
  RefAny_type <: TYPE_SPEC;              (* REFANY *)
  Address_type <: TYPE_SPEC;             (* ADDRESS *)

  Root_type <: TYPE_SPEC;                (* ROOT/UNTRACED ROOT *)
  <* FIELDS OF Root_type
     as_trace_mode: Untraced_NULL *>

(* "UNTRACED ..." is denoted by a node of type "Untraced". In order to
record the actual lexical token, it is declared as a subtype of
"SRC_NODE_C". The "lx_node_s" will contain a single "Token" element. *)

  Untraced <: SRC_NODE_C;
  Untraced_NULL = Untraced;

  Packed_type <: TYPE_SPEC;
  <* FIELDS OF Packed_type
     as_exp: EXP;
     as_type: M3TYPE *>

  Array_type <: TYPE_SPEC;
  <* FIELDS OF Array_type
     as_indextype_s: SEQUENCE OF M3TYPE;
     as_elementtype: M3TYPE; *>

  Enumeration_type <: TYPE_SPEC;
  <* FIELDS OF Enumeration_type
     as_id_s: SEQUENCE OF Enum_id; *>

  Set_type <: TYPE_SPEC;
  <* FIELDS OF Set_type
     as_type: M3TYPE; *>

  Subrange_type <: TYPE_SPEC;
  <* FIELDS OF Subrange_type
     as_range: Range; *>

(* Several attributes need to encode a range of expressions, and in
Expressome cases a single value is a legal choice . To avoid duplication,
the class "RANGE_EXP" is introduced to denote this choice. *)

  RANGE_EXP <: SRC_NODE_C;

(* A single value is denoted by a "Range_EXP" (sic). *)
  Range_EXP <: RANGE_EXP;
  <* FIELDS OF Range_EXP
     as_exp: EXP; *>

  Range <: RANGE_EXP;
  <* FIELDS OF Range
     as_exp1, as_exp2: EXP *>

(* "RECORD" types simply contain a sequence of "Fields". *)

  Record_type <: TYPE_SPEC;
  <* FIELDS OF Record_type
     as_fields_s: SEQUENCE OF Fields; *>

(* Like VAR declarations, Each "field" can introduce several
identifiers of the same type and initial value. The remarks about
"as_type" and "as_default" in the "Var_decl" node apply to fields
also. *)

  Fields <: SRC_NODE_C;
  <* FIELDS OF Fields
    as_id_s: SEQUENCE OF Field_id;
    as_type: M3TYPE_NULL;
    as_default: EXP_NULL; *>

(* "OBJECT" types and "REF" types can be "BRANDED", and this is denoted by
the "BRANDED_TYPE" class. *)

  BRANDED_TYPE <: TYPE_SPEC;
  <* FIELDS OF BRANDED_TYPE
     as_brand: Brand_NULL *>

(* A user supplied brand is optional. *)
  Brand <: SRC_NODE_C;
  Brand_NULL = Brand;
  <* FIELDS OF Brand
    as_exp: EXP_NULL; *>

  Ref_type <: BRANDED_TYPE;
  <* FIELDS OF Ref_type
     as_trace_mode: Untraced_NULL;
     as_type: M3TYPE *>

(* The object supertype (ancestor) is encoded as an attribute of type
"M3TYPE". In fact only a "Named_type", "Object_type" or "Root_Type" is
legal. *)

  Object_type <: BRANDED_TYPE;
  <* FIELDS OF Object_type
     as_ancestor: M3TYPE_NULL;
     as_fields_s: SEQUENCE OF Fields;
     as_method_s: SEQUENCE OF Method;
     as_override_s: SEQUENCE OF Override; *>

(* Methods and Overrides have a similar syntactic structure, which is
encoded by the class "METHOD_OVERRIDE". An "Override" has the
additional constraint that "as_default # NIL". *)

  METHOD_OVERRIDE <: SRC_NODE_C;
  <* FIELDS OF METHOD_OVERRIDE
     as_id: METHOD_OVERRIDE_ID;
     as_default: EXP_NULL *>

  Method <: METHOD_OVERRIDE;
  <* FIELDS OF Method
    as_type: Procedure_type; *>

  Override <: METHOD_OVERRIDE;
  
  Procedure_type <: TYPE_SPEC;
  <* FIELDS OF Procedure_type
     as_formal_param_s: SEQUENCE OF Formal_param;
     as_result_type: M3TYPE_NULL;
     as_raises: RAISEES_NULL *>


(* As with "Var_decl"s, only one of "as_formal_type" and "as_default"
may be NIL. *)

  Formal_param <: SRC_NODE_C;
  <* FIELDS OF Formal_param
     as_id_s: SEQUENCE OF FORMAL_ID;
     as_formal_type: M3TYPE_NULL;
     as_default: EXP_NULL *>

  RAISEES <: SRC_NODE_C;
  RAISEES_NULL = RAISEES;

  Raisees_some <: RAISEES;
  <* FIELDS OF Raisees_some
     as_raisees_s: SEQUENCE OF Qual_used_id *>

  Raisees_any <: RAISEES;  (* RAISES ANY *)

(* Opaque types have no direct correspondence in the concrete syntax.
In effect they encode the "<:" in a "Subtype_decl", and provide a unique
node for each declaration, which is convenient for subsequent semantic
analysis. The "as_type" attribute encodes the "M3TYPE" that actually
appeared to the right of the "<:". *)

  Opaque_type <: TYPE_SPEC;
  <* FIELDS OF Opaque_type
    as_type: M3TYPE; *>

(* \subsection{Expression productions}

There is no analogue of "ConstExpr" in the abstract syntax. *)

  EXP <: EXP_TYPE;
  EXP_NULL = EXP; 

(* In "M3AST_LX", the class "LITERAL" was declared as a subtype of
"SRC_NODE". At this point we {\it reveal} that "LITERAL" is actually a
subtype of "EXP". We also introduce distinct subtypes of "LITERAL" to
denote the different cases. Note that since "EXP" is defined as a
subtype of "SRC_NODE_C", literals will inherit the "lx_node_s"
attribute, but the value will be the empty sequence. *)

  LITERAL = M3AST_LX.LITERAL;

REVEAL
  M3AST_LX.LITERAL <: EXP;

TYPE
  NUMERIC_LITERAL <: M3AST_LX.LITERAL;

  Integer_literal <: NUMERIC_LITERAL;
  Real_literal <: NUMERIC_LITERAL;
  LongReal_literal <: NUMERIC_LITERAL;
  Extended_literal <: NUMERIC_LITERAL;

  Char_literal <: M3AST_LX.LITERAL;
  Text_literal <: M3AST_LX.LITERAL;

(* Although we could simply represent "NIL" by an "Exp_used_id", it
occurs sufficiently frequently that we choose to denote it by a
unique subtype, "Nil_literal" *)

  Nil_literal <: M3AST_LX.LITERAL;

(* Single identifiers occurring in expressions are really subtypes of
"EXP" and "USED_ID". They are declared as subtypes of "EXP", with the
"USED_ID" attributes "multiply inherited" *) Exp_used_id <: EXP; 

  <* FIELDS OF Exp_used_id vUSED_ID: USED_ID; *>

(* Procedure call is denoted by a "Call" node. *)

  Call <: EXP;
  <* FIELDS OF Call
     as_callexp: EXP;
     as_param_s: SEQUENCE OF Actual *>

(* The built-in functions, e.g. "ABS", could be represented by 
unique node types, like "Plus". However, to reduce the number of node
types, they are represented by "Call" nodes. An implementation
of this interface is expected to provide appropriate support for
determining if a "Call" node denotes a built-in function. *)

(* The desugaring of "NEW(ObjectType, method := P)" is made easier if
the applications of NEW are denoted by an independent subtype. *)

  NEWCall <: Call;

(* Since it is legal for an actual parameter to some of the built-in
functions to be a type, "as_exp_type" is of class "EXP_TYPE".  The
value of "as_id" will be "NIL" for the built-in functions. *) 

  Actual <: SRC_NODE_C; 
  <* FIELDS OF Actual 
       as_id: EXP_NULL;
       as_exp_type: EXP_TYPE *>

  Index <: EXP;    (* a[x,y,z,...] *)
  <* FIELDS OF Index
     as_array: EXP;
     as_exp_s: SEQUENCE OF EXP *>

(* "ARRAY", "RECORD" and "SET" constructors are not distinguished in the
AST, since they are not, in general, distinguished by syntax alone.
The different kinds of elements are denoted by the class "CONS_ELEM".
Array element propagation is denoted a node of type "Propagate". *)

  Constructor <: EXP;
  <* FIELDS OF Constructor
     as_type: M3TYPE;
     as_element_s: SEQUENCE OF CONS_ELEM;
     as_propagate: Propagate_NULL *>

  Propagate <: SRC_NODE_C;
  Propagate_NULL = Propagate;

(* There are three different kinds of constructor elements, simple
expressions, ranges and keyword actuals. We use the already declared
types "RANGE_EXP" and "Actual" as the corresponding attribute types.
*)

  CONS_ELEM <: SRC_NODE_C;

  RANGE_EXP_elem <: CONS_ELEM;
  <* FIELDS OF RANGE_EXP_elem
    as_range_exp: RANGE_EXP; *>

  Actual_elem <: CONS_ELEM;
  <* FIELDS OF Actual_elem
     as_actual: Actual *>

(* The binary and unary operators are encode as subtypes of the
"BINARY" and "UNARY" classes. Selection,``.'', is not treated as a
binary operator. It is defined as a separate subtype of "EXP", because
it is almost always processed in a different manner to the other
binary operations. *)

  BINARY <: EXP;
  <* FIELDS OF BINARY
     as_exp1: EXP;
     as_exp2: EXP *>

  Plus <: BINARY;    (* + *)
  Minus <: BINARY;   (* - *)
  Times <: BINARY;   (* * *)
  Rdiv <: BINARY;    (* / *)
  Textcat <: BINARY; (* & *)
  Div <: BINARY;     (* DIV *)
  Mod <: BINARY;     (* MOD *)
  Eq <: BINARY;      (* = *)
  Ne <: BINARY;      (* # *)
  Gt <: BINARY;      (* > *)
  Lt <: BINARY;      (* < *)
  Ge <: BINARY;      (* >= *)
  Le <: BINARY;      (* <= *)
  And <: BINARY;     (* AND *)
  Or <: BINARY;      (* OR *)
  In <: BINARY;      (* IN *)

  UNARY <: EXP;
  <* FIELDS OF UNARY
     as_exp: EXP *>

  Not <: UNARY;        (* NOT *)
  Unaryplus <: UNARY;  (* + *)
  Unaryminus <: UNARY; (* - *)
  Deref <: UNARY;      (* ^ *)

  Select <: EXP;  (* . *)
  <* FIELDS OF Select
     as_exp: EXP;
     as_id: Exp_used_id *>

(* \subsubsection{Statements} *)

(* The "STM" class captures all possible statement productions. *)

  STM <: SRC_NODE_C;

(* Statements partition into two groups, those with "a sequence of statements"
   as an attribute, e.g. "WHILE", and those which do not, e.g. "RAISE".
   The class "STM_WSS" contains the former group. Some statements contain 
   a component that, although it cannot appear where a "STM" can, also
   contains "a sequence of statements", e.g. the "ELSE" clause in a "CASE"
   statement. These nodes are grouped into the class "SUBSTM_WSS". *)

  STM_WSS <: STM;
  <* FIELDS OF STM_WSS
    as_stm_s: SEQUENCE OF STM; *>

  SUBSTM_WSS <: SRC_NODE_C;
  <* FIELDS OF SUBSTM_WSS
    as_stm_s: SEQUENCE OF STM; *>

  Assign_st <: STM;
  <* FIELDS OF Assign_st
     as_lhs_exp: EXP;
     as_rhs_exp: EXP *>

  Call_st <: STM;
  <* FIELDS OF Call_st
     as_call: Call; *>

  Case_st <: STM;
  <* FIELDS OF Case_st
     as_exp: EXP;
     as_case_s: SEQUENCE OF Case;
     as_else: Else_stm_NULL; *>

  Case <: SUBSTM_WSS;
  <* FIELDS OF Case
    as_case_label_s: SEQUENCE OF RANGE_EXP; *>

  Else_stm <: SUBSTM_WSS;
  Else_stm_NULL = Else_stm;

  Eval_st <: STM;
  <* FIELDS OF Eval_st
    as_exp: EXP; *>

  Exit_st <: STM;

  Raise_st <: STM;
  <* FIELDS OF Raise_st
     as_qual_id: Qual_used_id;
     as_exp_void: EXP_NULL *>

  Typecase_st <: STM;
  <* FIELDS OF Typecase_st
     as_exp: EXP;
     as_tcase_s: SEQUENCE OF Tcase;
     as_else: Else_stm_NULL; *>

  Tcase <: SUBSTM_WSS;
  <* FIELDS OF Tcase
     as_type_s: SEQUENCE OF M3TYPE;
     as_id: Tcase_id_NULL *>

  Handler <: SUBSTM_WSS;
  <* FIELDS OF Handler
     as_qual_id_s: SEQUENCE OF Qual_used_id;
     as_id: Handler_id_NULL *>

  Return_st <: STM;
  <* FIELDS OF Return_st
    as_exp: EXP_NULL; *>

  For_st <: STM_WSS;
  <* FIELDS OF For_st
     as_id: For_id;
     as_from: EXP;
     as_to: EXP;
     as_by: By_NULL; *>

  By <: SRC_NODE_C;
  <* FIELDS OF By
    as_exp: EXP; *>
  By_NULL = By;

  If_st <: STM_WSS;
  <* FIELDS OF If_st
     as_exp: EXP;
     as_elsif_s: SEQUENCE OF Elsif;
     as_else: Else_stm_NULL; *>

  Elsif <: SUBSTM_WSS;
  <* FIELDS OF Elsif
    as_exp: EXP; *>

  Lock_st <: STM_WSS;
  <* FIELDS OF Lock_st
     as_exp: EXP; *>

  Loop_st <: STM_WSS;
  <* FIELDS OF Loop_st *>

  Repeat_st <: STM_WSS;
  <* FIELDS OF Repeat_st as_exp: EXP; *>

  Try_st <: STM_WSS;
  <* FIELDS OF Try_st as_try_tail: TRY_TAIL; *>

  TRY_TAIL <: SUBSTM_WSS;
  Try_except <: TRY_TAIL;
  <* FIELDS OF Try_except
    as_handler_s: SEQUENCE OF Handler;
    as_else: Else_stm_NULL *>

  Try_finally <: TRY_TAIL;

  While_st <: STM_WSS;
  <* FIELDS OF While_st
    as_exp: EXP; *>

  With_st <: STM_WSS;
  <* FIELDS OF With_st
     as_binding_s: SEQUENCE OF Binding; *>

  Binding <: SRC_NODE_C;
  <* FIELDS OF Binding as_id: With_id;
    as_exp: EXP *>
  
  Block <: STM_WSS;
  <* FIELDS OF Block
     as_decl_s: SEQUENCE OF DECL_REVL; *>
  Block_NULL = Block;


(* Syntax errors are handled in part by new subtypes of a class named
   "Bad_Class". I.e. a syntactically incorrect AST may contain an instance
   of "Bad_Class", wherever an attribute of type "Class" could appear. *)

  Bad_EXP <: EXP;
  Bad_M3TYPE <: M3TYPE;
  Bad_STM <: STM;

END M3AST_AS.

