(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Mon Dec 12 15:52:49 PST 1994 by kalsow     *)

INTERFACE WebInfo;

IMPORT Target, M3CG, M3;

(*
This interface defines the routines used to generate the WWW
browser information.
*)

TYPE (* see M3CG for the interpretation of these types *)
  Name    = M3CG.Name;
  Offset  = M3CG.BitOffset;
  Size    = M3CG.BitSize;
  TypeUID = M3CG.TypeUID;

(*------------------------------------------------------ set up/tear down ---*)

PROCEDURE Reset ();
(* initializes the info. *)

PROCEDURE Finish (): TEXT;
(* returns the accumulated browser info. *)

(*----------------------------------------------------- compilation units ---*)

PROCEDURE Import_unit (n: Name);
PROCEDURE Export_unit (n: Name);
(* note that the current compilation unit imports/exports the interface 'n' *)

(*------------------------------------------- debugging type declarations ---*)

(* The debugging information for a type is identified by small a integer
   within a compilation unit.  The information is identified by a global
   uid (an INTEGER) across compilation units. The following procedures generate
   the symbol table entries needed to describe Modula-3 types to the
   debugger.  Note that Modula-3's builtin types have the fixed IDs
   listed above.  The 'hint' passed to 'import_type' is the name of
   the source file that generated the type declaration.  *)

PROCEDURE Declare_typename (t: TypeUID;  x: M3.Value);

PROCEDURE Declare_array (t: TypeUID;  index, elt: TypeUID;  s: Size);
PROCEDURE Declare_open_array (t: TypeUID;  elt: TypeUID;  s: Size);

PROCEDURE Declare_enum (t: TypeUID;  n_elts: INTEGER;  s: Size);
PROCEDURE Declare_enum_elt (n: Name);

PROCEDURE Declare_packed  (t: TypeUID;  s: Size;  base: TypeUID);

PROCEDURE Declare_record (t: TypeUID;  s: Size;  n_fields: INTEGER);
PROCEDURE Declare_field (n: Name;  o: Offset;  s: Size;  t: TypeUID);

PROCEDURE Declare_set (t, domain: TypeUID;  s: Size);

PROCEDURE Declare_subrange (t, domain: TypeUID;  READONLY min, max: Target.Int;
                   s: Size);

PROCEDURE Declare_pointer (t, target: TypeUID;  brand: TEXT;  traced: BOOLEAN);

PROCEDURE Declare_indirect (t, target: TypeUID);

PROCEDURE Declare_proctype (t: TypeUID; n_formals: INTEGER;
                            result: TypeUID;  n_raises: INTEGER);
PROCEDURE Declare_formal (n: Name;  t: TypeUID);
PROCEDURE Declare_raises (n: Name);

PROCEDURE Declare_object (t, super: TypeUID;  brand: TEXT;  traced: BOOLEAN;
                 n_fields, n_methods, n_overrides: INTEGER;  field_size: Size);
PROCEDURE Declare_method (n: Name;  signature: TypeUID;  dfault: M3.Expr);
PROCEDURE Declare_override (n: Name;  dfault: M3.Expr);
PROCEDURE Declare_opaque (t, super: TypeUID);
PROCEDURE Reveal_opaque (lhs, rhs: TypeUID);

END WebInfo.
