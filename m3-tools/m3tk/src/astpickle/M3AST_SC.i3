(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

<* PRAGMA FIELDS *>

INTERFACE M3AST_SC;

(* This interface defines some additional attributes that make
   an AST "self contained". Such an AST is capable of being pickled
   as a single entity, that is, with all references to entities
   on other ASTs represented (in the pickle) via surrogate nodes
   that provide for safe recombination when the AST is unpickled.
*)

IMPORT M3AST, M3AST_AS;

TYPE

(* Surrogate nodes that replace inter-AST references "multiply inherit"
   the "IMPORTED_NODE" class, which carries the information on how
   to locate the actual nodes. A "Unit_stub", shared between all
   reference to the same unit, locates the unit by name, type
   and unique identifier (allowing version checks). The "sc_eoi"
   field of an "IMPORTED_NODE" is the index in the "sc_exported_node_s"
   of the "Compilation_Unit" node of the AST that the "sc_unit_stub" field 
   locates. *)

  IMPORTED_NODE <: M3AST.NODE;
  <* FIELDS OF IMPORTED_NODE
       sc_unit_stub: Unit_stub;
       sc_eoi: INTEGER *>

  Unit_stub <: M3AST.NODE;
  <* FIELDS OF Unit_stub
       sc_unit_symrep: M3AST_LX.Symbol_rep;
       sc_unit_type: M3AST_FE.Unit_type;
       sc_unit_uid: M3AST_FE.Unit_uid;
  *>

(* The only inter-AST references thus far are to "DEF_ID" and
   "TYPE_SPEC" nodes. *)

  Imported_id <: M3AST_AS.DEF_ID;
  <* FIELDS OF Imported_id
       sc_actual_id: M3AST_AS.DEF_ID;
       vIMPORTED_NODE: IMPORTED_NODE;  *>

  Imported_type <: M3AST_AS.TYPE_SPEC;
  <* FIELDS OF Imported_type
       sc_actual_type: M3AST_AS.TYPE_SPEC;
       vIMPORTED_NODE: IMPORTED_NODE; *>

  Exported_node <: M3AST.NODE;
  <* FIELDS OF Exported_node
       sc_actual_node: M3AST_AS.SRC_NODE; (* TYPE_SPEC or DEF_ID *) *>

  <* FIELDS OF M3AST_AS.Compilation_Unit
       sc_unit_stub_s: SEQUENCE OF Unit_stub;
       sc_exported_node_s: SEQUENCE OF Exported_node; *>

(* The "sc_exported_node_s" tabulates the nodes defined in the AST
   rooted at this node. The "sc_unit_stub_s" denotes the units
   to which this AST has inter-AST references. *)

END M3AST_SC.
