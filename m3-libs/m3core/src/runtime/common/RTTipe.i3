(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Fri May  6 14:38:25 PDT 1994 by kalsow     *)

INTERFACE RTTipe;

IMPORT RT0, RTPacking;

TYPE
  T = OBJECT kind: Kind;  size, align: INTEGER := 0; END;

TYPE
  Kind = {
    Address, Array, Boolean, Cardinal, Char, Enum, Extended,
    Integer, Longreal, Null, Object, OpenArray, Packed, Proc, Real,
    Record, Ref, Refany, Set, Subrange, UntracedRef };

CONST
  BuiltinKinds = SET OF Kind {
    Kind.Address, Kind.Boolean, Kind.Cardinal, Kind.Char, Kind.Extended,
    Kind.Integer, Kind.Longreal, Kind.Null, Kind.Proc, Kind.Real, Kind.Refany
  };

TYPE
  Builtin   = T OBJECT END;
  Array     = T OBJECT n_elts: INTEGER; element: T; elt_pack: INTEGER; END;
  Enum      = T OBJECT n_elts: INTEGER; END;
  Object    = T OBJECT super: RT0.TypeDefn;  fields: Field;
                       field_size, field_align: INTEGER; END;
  OpenArray = T OBJECT n_dimensions: INTEGER; element: T; elt_pack:INTEGER END;
  Packed    = T OBJECT n_bits: INTEGER; base: T; END;
  Record    = T OBJECT fields: Field;  END;
  Ref       = T OBJECT traced: BOOLEAN; uid: INTEGER; self: RT0.TypeDefn; END;
  Set       = T OBJECT n_elts: INTEGER; END;
  Subrange  = T OBJECT min, max: INTEGER; END;

  Field = REF RECORD type: T;  next: Field;  offset: INTEGER := 0; END;

PROCEDURE Get (typecode: INTEGER;  READONLY packing: RTPacking.T): T;
(* Returns the type bound to 'typecode' if it is traced.
   Otherwise, returns NIL.  Sets the size, alignment and offset
   fields of the returned value to correspond to the specified
   packing.  The sizes, alignments and field offsets are
   specified in bits. *)

END RTTipe.

