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

(* The "M3AST_SM" interface defines the static semantic layer of the
Modula-3 AST. *)

 INTERFACE M3AST_SM;

IMPORT M3AST, M3AST_AS;

(* The reader is assumed to be familiar with the interfaces "AST" and
"M3AST_AS". As with "M3AST_AS", the exact representation of the node
attributes is left to a companion interface, e.g. "M3AST_SM_F".  A
value of "NIL" is legal for (almost) all semantic attributes and is
interpreted as {\it unset}, i.e. not computed.  However, there are a
few cases in which "NIL" is legal because the corresponding syntactic
attribute could legally be null, e.g. the default expression for a
variable declaration.  In this case we use another distinguishing
value of the appropriate class, named "UNSET_CLASS", to indicate
unset.  Some attributes have "INTEGER" type; for these "-1" is the
unset value.

Following semantic analysis, if a unit has no semantic errors then,
with the exception of unrevealed opaque types, one can assert that no
attributes are unset.

It is not obvious what set of semantic attributes should be computed.
Each client of this interface might well have a different opinion of
what information is important. Since the AST framework makes it
straightforward for each tool to define its own attributes, this
interface concentrates on generating that information which is hard to
compute, e.g. the binding of identifiers or the types of expressions.
In particular, there is scant use of back-pointers, since these can
always be generated if necessary with a single pass through the tree.

Rather than create new nodes to carry semantic information, the
strategy is to reuse syntactic nodes wherever possible. In particular,
nodes in the "DEF_ID" and "TYPE_SPEC" classes are reused extensively.
As a consequence many other semantic attributes are declared as having
these types.  In many cases a semantic attribute will be a set;
however, we continue use the sequence interface, "SeqElem", to denote
a set, with the understanding that there will be no duplicates.

*)

<* PRAGMA FIELDS *>

(*\subsection{Naming Conventions}

Semantic attributes are all prefixed with the characters "sm_".
It is conventional to use the "Name_UNSET" name to indicate an attribute
type that is either "unset" or has a value of type "Name". 
It is conventional to indicate a legal "NULL" value by the type
named "Name_NULL_UNSET". *)

TYPE 
  DEF_ID_UNSET = M3AST_AS.DEF_ID;
  TYPE_SPEC_UNSET = M3AST_AS.TYPE_SPEC;
  EXP_UNSET = M3AST_AS.EXP;
  Proc_decl_UNSET = M3AST_AS.Proc_decl;
  METHOD_OVERRIDE_UNSET = M3AST_AS.METHOD_OVERRIDE;

  EXP_NULL_UNSET = M3AST_AS.EXP_NULL;
  DEF_ID_NULL_UNSET = M3AST_AS.DEF_ID_NULL;

(* These functions return distinguished values that indicate {\it unset}. *)

PROCEDURE UNSET_EXP(): EXP_NULL_UNSET;

PROCEDURE UNSET_DEF_ID(): DEF_ID_NULL_UNSET;

(* \subsection{Unit Attributes} *)

(* A "UNIT" has a back pointer to the parent "Compilation_Unit". *)

  <* FIELDS OF M3AST_AS.UNIT
       sm_comp_unit: M3AST_AS.Compilation_Unit *>

(* A "UNIT_WITH_BODY" has the following semantic attributes:

\begin{itemize}
\item The set of units defined as the transitive closure of all imported 
      interfaces, plus, in the case of a module, the exported interfaces.

\item The set of all "OBJECT" types and traced "REF" types in the unit.

\item For each "REVEAL" of a particular opaque type in the unit, information 
      that is needed for consistency checking by, say, a Modula-3 linker. 
\end{itemize} *)

  <* FIELDS OF M3AST_AS.UNIT_WITH_BODY
       sm_import_s: SEQUENCE OF M3AST_AS_Used_interface_id.T;
       sm_type_spec_s: SEQUENCE OF M3AST_AS_TYPE_SPEC.T;
       sm_reveal_s: SEQUENCE OF Opaque_type_Revln *>


(* An "Opaque_type_Revln" is a new node type introduced at this level
to carry information about a revelation. The "sm_type_id" attribute is
a binding to the "Type_id" for which the revelation information
pertinent.  Even if there are multiple revelations for a single type,
there is only a single "Opaque_type_Revln" node constructed. The
"sm_concrete_rev" attribute is set to the "TYPE_SPEC" corresponding to
the right hand side of any concrete revelation in the unit. The
"sm_opaque_rev_s" attribute is the set of "TYPE_SPEC"s corresponding
to the right hand side of any partial revelations in the unit. "In the
unit" means that the corresponding "REVELATION" node occurs in the
tree rooted at "UNIT". *)

TYPE
  Opaque_type_Revln <: M3AST.NODE;
  <* FIELDS OF Opaque_type_Revln
     sm_type_id: M3AST_SM.DEF_ID_UNSET;
     sm_concrete_rev: M3AST_AS.TYPE_SPEC;
     sm_opaque_rev_s: SEQUENCE OF M3AST_AS_TYPE_SPEC.T; *>

(* Generic instantiations, "UNIT_GEN_INS" nodes, have an attribute
denoting the instantiated AST. This is defined as a
"Compilation_Unit", so that status information may be annotated on
both ASTs. *)

  <* FIELDS OF M3AST_AS.UNIT_GEN_INS
       sm_ins_comp_unit: M3AST_AS.Compilation_Unit *>

(* A "Module" has a normalised set of exported interfaces. I.e.  if no
"EXPORTS" clause is present, the "MODULE M" -> "MODULE M EXPORTS M"
desugaring is represented by the "sm_export_s" attribute. If an
"EXPORTS" clause is present, the "sm_export_s" sequence contains the
same members as the "as_export_s" attribute. *)

  <* FIELDS OF M3AST_AS.Module
       sm_export_s := SEQUENCE OF M3AST_AS_Used_interface_id *>

(*\subsection{Identifier Attributes}*)

(* A "UNIT_ID" node has a back pointer to the enclosing "UNIT" *)

  <* FIELDS OF M3AST_AS.UNIT_ID
       sm_spec: M3AST_AS.UNIT *>

(* All the defining identifier nodes that can appear in a declaration
that can be marked "EXTERNAL", multiply inherit the "EXTERNAL_ID"
class, which carries the same information as the "EXTERNAL_DECL"
class. See "M3AST_PG" interface for details. *)

  <*  FIELDS OF M3AST_AS.Interface_id, M3AST_AS.Type_id,
                M3AST_AS.Exc_id, M3AST_AS.Proc_id
        vEXTERNAL_ID: M3AST_PG.EXTERNAL_ID *>

(* Defining identifiers that can have initialising expressions
multiply inherit the "INIT_ID" class, which refers to the "EXP" node
in the initialising expression. *)

  INIT_ID <: M3AST.NODE;
  <* FIELDS OF INIT_ID
       sm_init_exp: M3AST_SM.EXP_NULL_UNSET *>

  <* FIELDS OF M3AST_AS.METHOD_OVERRIDE_ID, M3AST_AS.Field_id, 
               M3AST_AS.Const_id, M3AST_AS.Var_id, M3AST_AS.F_Value_id,
               M3AST_AS.F_Readonly_id, M3AST_AS.For_id, M3AST_AS.With_id
       vINIT_ID: M3AST_SM.INIT_ID *>

(* "Const_id" and "Enum_id" inherit a class "CCV_ID" that captures the
value of the constant expression or value representing the enumeration
member, respectively. The representation is specified in terms of an
opaque type "Exp_value", which is revealed by a particular compiler
implementation *)

  CCV_ID <: M3AST.NODE;
  <* FIELDS OF CCV_ID
       sm_exp_value: M3AST_SM.Exp_value *>

  <* FIELDS OF M3AST_AS.Const_id, M3AST_AS.Enum_id
       vCCV_ID: M3AST_SM.CCV_ID *>

  Exp_value <: REFANY;  

(* A back pointer from a "Field_id", a "Method_id" and and an
"Override_id" to the enclosing "Object_type" node, is useful and is
captured by the "RECOBJ_ID" class. *)

  RECOBJ_ID <: M3AST.NODE;
  <* FIELDS OF RECOBJ_ID
       sm_enc_type_spec: M3AST_SM.TYPE_SPEC_UNSET *>

  <* FIELDS OF M3AST_AS.Field_id, M3AST_AS.METHOD_OVERRIDE_ID
       vRECOBJ_ID: M3AST_SM.RECOBJ_ID *>

(* Some "DEF_ID" nodes, although they occur as separate nodes in an
AST are almost re-definitions, namely a "Proc_id" in a module that
exports its counterpart in an interface, and a method override in an
"Object_type". The connection is established through the "REDEF_ID"
class.  *)

  REDEF_ID <: M3AST.NODE;
  <* FIELDS OF REDEF_ID
       sm_int_def: M3AST_SM.DEF_ID_NULL_UNSET *>

(* For a "Proc_id" node in an interface AST, the value of "sm_int_def"
refers to itself. For a "Proc_id" node in a module AST, the value is
either "NIL", which denotes a local, or private, procedure, or it refers
to the corresponding "Proc_id" node in one of the interface ASTs in
the "sm_export_s" set of the module, and denotes a public or exported
procedure. *)

  <* FIELDS OF M3AST_AS.Proc_id
       vREDEF_ID: M3AST_SM.REDEF_ID *>

(* For a "Method_id" node, the value of "sm_int_def" refers to itself.
For an "Override_id" node, the value refers to the "Method_id" node
that is being overridden. *)

  <* FIELDS OF M3AST_AS.METHOD_OVERRIDE_ID
       vREDEF_ID: M3AST_SM.REDEF_ID *>

(* A "Proc_id" node has a back pointer to the containing "Proc_decl"
node. It also has an attribute "sm_concrete_proc_id", which is the
inverse of the "sm_int_def" attribute. In a module AST, the value of
"sm_concrete_proc_id" refers to itself. In an interface AST, the value
refers to the exporting "Proc_id" node in a module AST.  In
particular, if "m.sm_intf_def = i", then "i.sm_concrete_proc_id = m".
*)

  <* FIELDS OF M3AST_AS.Proc_id
       sm_spec: M3AST_SM.Proc_decl_UNSET;
       sm_concrete_proc_id: M3AST_SM.DEF_ID_NULL_UNSET *>

(* All used identifiers contain an attribute that denotes their
binding, or defining occurrence. *)

  <* FIELDS OF M3AST_AS.USED_ID
       sm_def: M3AST_SM.DEF_ID_UNSET *>

(* All members of the "TYPED_ID" class contain an attribute that
denotes their type. We will define the value of this attribute in
pseudo Modula-3, terms of the attributes of the nodes that contain the
identifier node. Assume a function "M3TYPE_To_TYPE_SPEC" that maps a
value of type "M3TYPE" to a value of "TYPE_SPEC", i.e. resolves the
identifiers in "Named_type" nodes. *)

  <* FIELDS OF M3AST_AS.TYPED_ID
       sm_type_spec: M3AST_SM.TYPE_SPEC_UNSET *>

(*
|   Const_id: const_decl.as_id.sm_type_spec =
|     IF const_decl.as_type = NIL THEN const_decl.as_exp.sm_exp_type_spec
|     ELSE M3TYPE_To_TYPE_SPEC(const_decl.as_type)

|   Type_id: type_decl.as_id.sm_type_spec =
|     IF ISTYPE(type_decl, Subtype_decl) THEN NewOpaque_type()
|     ELSE M3TYPE_To_TYPE_SPEC(type_decl.as_type)

|   Exc_id: exc_decl.as_id.sm_type_spec =
|     M3TYPE_To_TYPE_SPEC(exc_decl.as_type)

|   Var_id: ForAll v IN var_decl.as_id_s v.sm_type_spec =
|     IF var_decl.as_type # NIL THEN M3TYPE_To_TYPE_SPEC(var_decl.as_type)
|     ELSE var_decl.as_default.sm_exp_type_spec

|   FORMAL_ID: ForAll v IN formal_param.as_id_s v.sm_type_spec =
|     IF formal_param .as_type # NIL THEN 
|       M3TYPE_To_TYPE_SPEC(formal_param.as_type)
|     ELSE formal_param.as_default.sm_exp_type_spec

|   Enum_id: ForAll v IN enumeration_type.as_id_s v.sm_type_spec =
|     enumeration_type

|   Field_id: ForAll v IN fields.as_id_s v.sm_type_spec =
|     IF fields.as_type # NIL THEN M3TYPE_To_TYPE_SPEC(fields.as_type)
|     ELSE fields.as_default.sm_exp_type_spec

|   Proc_id: proc_decl.as_id.sm_type_spec = proc_decl.as_type;

|   Method_id: method.as_id.sm_type_spec = method.as_type;

|   Override_id: override_id.sm_type_spec =
|     override_id.vREDEF_ID.sm_int_def.sm_type_spec

|   For_id: for_st.as_id.sm_type_spec =
|     CommonBaseType(for_st.as_from.sm_exp_type_spec,
|                    for_st.as_to.sm_exp_type_spec)

|   Handler_id: handler.as_id.sm_type_spec =
|     NARROW(SeqM3AST_AS_Qual_used_id.First(handler.qual_id_s).sm_def,
|            M3AST_AS.Exc_id).sm_type_spec

|   Tcase_id: tcase.as_id.sm_type_spec =
|     M3TYPE_To_TYPE_SPEC(SeqM3AST_AS_M3TYPE.First(tcase.as_type_s);

|   With_id: binding.as_id.sm_type_spec =
|     binding.as_exp.sm_exp_type_spec; *)


(*\subsection{Type Attributes} *)

(* Enumeration types have an attribute specifying the number of elements. *)

  <* FIELDS OF M3AST_AS.Enumeration_type
       sm_num_elements: INTEGER *>

(* "Array_type" nodes have an attribute denoting their normalised
form.  E.g. 

| ARRAY [0..9], [0..9] OF T 
| ARRAY [0..9] OF ARRAY [0..9] OF T  (* normalsed *)

We reuse the same "Array_type" node with the constraint that the
"as_indextype_s" has at most one member. *)

  <* FIELDS OF M3AST_AS.Array_type
       sm_norm_type: M3AST_AS.Array_type *>

(* An "Opaque_type" node has attributes denoting all its revelations.
The scope of these attributes is "global" in the sense that whenever a
revelation that refers to this "Opaque_type" node is processed in any
AST, the corresponding "TYPE_SPEC" node is added to the the set.
Contrast this to the information in an "Opaque_type_Revln" node which
is local to a given AST. *)

  <* FIELDS OF M3AST_AS.Opaque_type
     sm_concrete_type_spec: M3AST_SM.TYPE_SPEC_UNSET;
     sm_type_spec_s: SEQUENCE OF M3AST_AS_TYPE_SPEC.T *>

(* "Named_type" nodes have an attribute denoting the resolution of the
name to a "TYPE_SPEC". Given that the name resolves to a "Type_id" node
"t", the value is given by "t.sm_type_spec". *)

  <* FIELDS OF M3AST_AS.Named_type
       sm_type_spec: M3AST_SM.TYPE_SPEC_UNSET *>

(* Subrange types have an attribute denoting their base type *)

  <* FIELDS OF M3AST_AS.Subrange_type
       sm_base_type_spec: M3AST_SM.TYPE_SPEC_UNSET *>

(* All "TYPE_SPEC" nodes have a size in bits and an alignment in bits.
Although these values are back-end specific, they can feature in
type-checking through the use of the "BITSIZE/BYTESIZE" function in type
constructors. *)

  <* FIELDS OF M3AST_AS.TYPE_SPEC
       sm_bitsize: INTEGER;
       sm_align: INTEGER *>

(* "Object_type" nodes have additional attributes to hold the size and
alignment of the referent; i.e. "sm_bitsize" for an "Object_type" is
the same as that for a "Ref_type". *)

  <* FIELDS OF M3AST_AS.Object_type
       sm_rf_bitsize: INTEGER;
       sm_rf_align: INTEGER *>

(* Irrespective of whether the programmer supplied an explicit brand,
one is made available as an "Exp_value" that will denote a text
literal. *)

  <* FIELDS OF M3AST_AS.Brand
       sm_brand: M3AST_SM.Exp_value *>

(* A "Procedure_type" is distinguished as to a procedure signature or
method signature by an attribute "sm_def_id", which refers to the
"Proc_id" or "Method_id", respectively. The case of a standalone
signature (i.e.  "T = PROCEDURE(...)") is indicated by "NIL".  In a
"Type.method" context, the "sm_exp_type_spec" attribute (see the
Expressions section) of the selection node refers to a
"Procedure_type", with "sm_def_id" referring to the "Type_id" node
denoting "Type". *)

  <* FIELDS OF M3AST_AS.Procedure_type
       sm_def_id: M3AST_SM.DEF_ID_NULL_UNSET *>

(* Types used to represent the arguments to the built-in (polymorphic)
procedures. These only occur in the AST that represents the built-in
types. *)

  Type_type <: M3AST_AS.TYPE_SPEC;
  Any_type <: M3AST_AS.TYPE_SPEC;

(* The notion of {\it no-type} is convenient, e.g. for a procedure
call that does not return a result. *)

  Void_type <: M3AST_AS.TYPE_SPEC;

(* \subsection{Expressin Attributes}*)

(* All expressions have an associated type defined by the rules for
expressions in the language definition. If an expression can be
evaluated at compile time, it will also have a constant value. *)

  <* FIELDS OF M3AST_AS.EXP
       sm_exp_type_spec: M3AST_SM.TYPE_SPEC_UNSET;
       sm_exp_value: M3AST_SM.Exp_value *>

(* Procedure calls have a normalised parameter list. The value of
"call.as_exp.sm_exp_type_spec", which will refer to a "Procedure_type"
node, defines the order and default values of the formal parameters.
The "sm_actual_s" list will correspond to the rules for procedure call
in section 2.3.2 of the language definition. "sm_actual_s" is
admittedly a bad choice of name, since it denotes a sequence of
"EXPs" not a sequence of "Actuals". *)

  <* FIELDS OF M3AST_AS.Call
       sm_actual_s: SEQUENCE OF M3AST_AS_EXP.T *>

(* Calls to NEW have the method binding desuraging computed.
   For example:

|  NEW(T, m := P, f := E) is desugared to:
|  NEW(T OBJECT OVERRIDES m := P END, f := E);

The "sm_exp_type_spec" attribute for a "NEWCall" node is the desugared
"Object_type". The methods that walk the children of a "NEWCall" node
are overridden at this level to walk the desugared parameters. *)

  <* FIELDS OF M3AST_AS.NEWCall
       sm_norm_actual_s: SEQUENCE OF M3AST_AS_Actual.T *>

(* Record constructors also have a normalised set of bindings, again
according to the rules in section 2.6.8 of the language definition. *)

  <* FIELDS OF M3AST_AS.Constructor
       sm_actual_s: SEQUENCE OF M3AST_AS_RANGE_EXP.T *>

(* When generating the normalised parameter bindings for a built-in
call with a "Type" as argument, we must invent a subtype of "EXP" to
denote it, since the "sm_actual_s" attribute is a sequence of "EXP"
nodes.  The value of "TypeActual.sm_exp_type_spec" is the "TYPE_SPEC"
node corresponding to the actual parameter. *)

  TypeActual <: M3AST_AS.EXP;

(*\subsection{Identifier Scopes} *)

(* It is sometimes convenient to enumerate all the "DEF_IDs" that are
in scope at a particular point in an AST. This can be used, for
example, to perform additional name resolution beyond that already
carried out for "USED_ID" nodes in the AST. This is achieved by
introducing a "SCOPE" class that is multiply inherited by the relevant
AST nodes. This class denotes the identifiers introduced into scope by
the associated node and references the enclosing "SCOPE" node. So, by
following the chain of enclosing scopes, one can enumerate all the
"DEF_IDs" in scope at any given point. Contrary to normal practice, a
back-pointer to the multiply inheriting node is recorded. Several
nodes may participate in defining a unique scope, and all of these
will have the same value of "sm_level". Identifiers will all be
distinct in "SCOPE"s at the same level. The built-in identifiers are
at level zero. The "SCOPE" of a "Block" node associated with a
"UNIT_ID" or "Proc_id" may be empty, in which case, the enclosing
"SCOPE" node will contain the associated declared identifiers. *)

   SCOPE <: M3AST.NODE;
   <* FIELDS OF SCOPE
      sm_def_id_s: SEQUENCE OF M3AST_AS_DEF_ID.T;
      sm_enc_scope: SCOPE;
      sm_level: INTEGER;
      sm_mi_node: M3AST_AS.SRC_NODE; *>

(* The following node types inherit the SCOPE class. *)

   <* FIELDS OF M3AST_AS.UNIT_ID, M3AST_AS.Block, M3AST_AS.Proc_id,
                M3AST_AS.Method_id, M3AST_AS.With_id, M3AST_AS.For_id,
                M3AST_AS.Tcase_id, M3AST_AS.Handler_id
        vSCOPE: SCOPE *>

(*\subsection{Multiple Inheritance Support} *)

(* Almost all the classes that were introduced at at this level
involve multiple inheritance, which cannot be expressed driectly in
Modula-3. The following methods are defined onan "M3AST.NODE", to be
used where you would otherwise use "ISTYPE".  If the result of the
call is TRUE, the "out" parameter is set to the multiply inherited
part of the (composite) node. The methods are revealed in the
representation interface (e.g, "M3AST_SM_F").  *)

(* 
|  METHODS (* OF M3AST.NODE *)
|    IsA_INIT_ID(VAR(*out*) init_id: INIT_ID): BOOLEAN;
|    IsA_CCV_ID(VAR(*out*) ccv_id: CCV_ID): BOOLEAN;
|    IsA_RECOBJ_ID(VAR(*out*) recobj_id: RECOBJ_ID): BOOLEAN;
|    IsA_REDEF_ID(VAR(*out*) redef_id: REDEF_ID): BOOLEAN;
|    IsA_SCOPE(VAR(*out*) scope: SCOPE): BOOLEAN;
*)

(* \subsection{Temporary Attributes}

The following attributes are defined to be set after semantic analysis
is complete, but have {\it temporary} status. The original notion of
temporary was defined such that these attributes would not be saved in
persistent ASTs, for example, in AST {\it pickles}. Certainly the
attributes can be recomputed in a single pass over the tree and the
expectation is that an implementation will make this transparent to
the client. For backward compatibility the attributes still contain
a "tmp_" prefix and are revealed in the representation interface
for "M3AST_TM" (e.g. "M3AST_TM_F"). *)

   <* FIELDS OF M3AST_AS.TYPE_SPEC 
        tmp_unit_id: M3AST_AS.UNIT_ID *>

   <* FIELDS OF M3AST_AS.DEF_ID  
        tmp_unit_id: M3AST_AS.UNIT_ID *>

(* These attributes denote which unit (AST) that a "DEF_ID" or "TYPE_SPEC"
belongs to. *)

END M3AST_SM.
