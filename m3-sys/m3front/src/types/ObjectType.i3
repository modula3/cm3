(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: ObjectType.i3                                         *)
(* Last Modified On Mon Jul 25 08:44:06 PDT 1994 By kalsow     *)
(*      Modified On Fri Oct  5 07:29:44 1990 By muller         *)


INTERFACE ObjectType;

IMPORT M3ID, Type, Scope, Value, Brand;

PROCEDURE Parse (super: Type.T;  traced: BOOLEAN;  brand: Brand.T): Type.T;

PROCEDURE New   (super: Type.T;  traced: BOOLEAN;  brand: Brand.T;
                                        fields, methods: Scope.T): Type.T;

PROCEDURE Is (t: Type.T): BOOLEAN;
(* Including (opaque type or revelation) of an object type. *)

PROCEDURE IsBranded (t: Type.T): BOOLEAN;

PROCEDURE LookUp (t: Type.T;  id: M3ID.T;
                      VAR value: Value.T;  VAR visible: Type.T): BOOLEAN;

PROCEDURE MethodOffset (t: Type.T): INTEGER;
(* Returns the bit offset of the methods defined in object type t if
   all of its parents are visible, otherwise returns -1 *)

PROCEDURE GetFieldsOffsetAndAlign (t: Type.T;  VAR offset, align: INTEGER);
(* Sets 'align' to the alignment of 't's fields.  If all the parents of 't'
   are visible, sets 'offset' to the bit offset of 't's fields,
   otherwise sets 'offset' to -1. *)

PROCEDURE FieldAlignment (t: Type.T): INTEGER;
(* Return the alignment of 't's fields. *)

PROCEDURE Super (t: Type.T): Type.T;
(* Returns the super type of 't' if 't' is an object and it
   has been typechecked, otherwise NIL *)

PROCEDURE NoteOffsets (t, u: Type.T);
(* If 'u' is an object type, generate the magic information for the
   field offsets of 'u' under the name of 't'. *)

PROCEDURE NoteRefName (t: Type.T;  name: TEXT);
(* record a user name for the ref type 't' *)

PROCEDURE InitTypecell (t: Type.T;  offset, prev: INTEGER);

END ObjectType.

