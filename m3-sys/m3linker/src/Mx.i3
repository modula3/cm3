(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: Mx.i3                                                 *)
(* Last Modified On Mon Oct 24 13:37:22 PDT 1994 By kalsow     *)

INTERFACE Mx;

IMPORT M3ID;

(*------------------------------------------------------------------------*)

PROCEDURE NewSet (): LinkSet;
(* return a new empty link set. *)

PROCEDURE Contents (base: LinkSet): UnitList;
(* returns the list of units contained in 'base'.  It is an unchecked
   runtime error to modify this list or any of its referents. *)

(*------------------------------------------------------------------------*)
(* The data structures processed by the linker are defined below:         *)

CONST
  LinkerMagicWC16 = "M3 v4.2";
  LinkerMagicWCUni = "M3 v4.3";
  LinkerMagicLen = 7; (* Enough for either magic string. *) 
  UnicodeWideChar = FALSE; (* This must agree with the front end. *) 

CONST
  BuiltinUnitName = "M3_BUILTIN";

TYPE
  LinkSet <: REFANY;

TYPE
  Name     = M3ID.T;
  UnitList = REF RECORD  unit: Unit;  next: UnitList  END;
  Int32    = BITS 32 FOR [-16_7fffffff - 1 .. 16_7fffffff];
  TypeName = Int32;  (* a compiler-generated 32-bit uid *)

  InfoList = RECORD start, cnt: CARDINAL := 0; END;
  InfoVec  = REF ARRAY OF INTEGER;

TYPE
  File = REF RECORD
    name     : TEXT    := NIL;
    imported : BOOLEAN := FALSE;
  END;

TYPE
  Unit = REF RECORD
    name              : Name;
    file              : File;
    interface         : BOOLEAN       := FALSE;
    virtual           : BOOLEAN       := FALSE;
    exported_units    : InfoList; (* of M3ID.Ts *)
    imported_units    : InfoList; (* of M3ID.Ts *)
    imported_generics : InfoList; (* of M3ID.Ts *)
    used_interfaces   : InfoList; (* of M3ID.Ts *)
    used_modules      : InfoList; (* of M3ID.Ts *)
    import_def_syms   : InfoList; (* of MxVS.Ts *)
    import_use_syms   : InfoList; (* of MxVS.Ts *)
    export_def_syms   : InfoList; (* of MxVS.Ts *)
    export_use_syms   : InfoList; (* of MxVS.Ts *)
    imported_types    : InfoList; (* of TypeNames *)
    exported_types    : InfoList; (* of TypeNames *)
    wishes            : InfoList; (* of TypeNames *)
    opaques           : OpaqueType    := NIL;
    imported_objects  : ObjectType    := NIL;
    exported_objects  : ObjectType    := NIL;
    revelations       : Revelation    := NIL;
    info              : InfoVec       := NIL;
  END;

TYPE
  Revelation = REF RECORD
    source  : Name;
    lhs     : TypeName;
    rhs     : TypeName;
    next    : Revelation;
    partial : BOOLEAN;
    export  : BOOLEAN;
  END;

TYPE
  ObjectType = REF RECORD
    next        : ObjectType;
    source      : Name;
    type        : TypeName;
    super_type  : TypeName;
    data_size   : INTEGER;
    data_align  : INTEGER;
    method_size : INTEGER;
    export      : BOOLEAN;
    from_module : BOOLEAN;
  END;

TYPE
  OpaqueType = REF RECORD
    next       : OpaqueType;
    type       : TypeName;
    super_type : TypeName;
  END;

END Mx.
