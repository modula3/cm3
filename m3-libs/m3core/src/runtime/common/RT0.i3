(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Thu May  4 10:54:10 PDT 1995 by kalsow     *)
(*      modified on Fri May 28 14:34:04 PDT 1993 by muller     *)
(*      modified on Tue Sep  8 11:51:46 PDT 1992 by jdd        *)

(* RT0 is "the bottom of the world".  It contains
   types and constants that are shared by multiple modules of
   the runtime and/or the compiler and linker.

   If you're using this interface, you're a wizard!

   This interface and its implemenation MUST NOT import any
   other interface.
*)

INTERFACE RT0;

(* These types CANNOT be changed without a synchronized change to the
   output of the compiler and prelinker *)

(*------------------------------------ compiler generated data structures ---*)

TYPE
  Typecode    = [0 .. 16_FFFFF];         (* can fit in 20 bits *)
  Fingerprint = ARRAY [0..1] OF [-16_7fffffff-1 .. 16_7fffffff]; (* 64 bits *)
  String      = UNTRACED REF CHAR;       (* a '\000' terminated string *)
  ModulePtr   = UNTRACED REF ModuleInfo;
  ProcPtr     = UNTRACED REF ProcInfo;
  ImportPtr   = UNTRACED REF ImportInfo;
  Binder      = PROCEDURE (): ModulePtr;

TYPE (* allocated at offset 0 of each compilation unit's global data *)
  ModuleInfo = RECORD
    file           : String;
    type_cells     : ADDRESS;  (* initially a ref to a Typecell *)
    type_cell_ptrs : ADDRESS;  (* initially a ref to a TypeLink *)
    full_rev       : ADDRESS;  (* initially a ref to a Revelation *)
    partial_rev    : ADDRESS;  (* initially a ref to a Revelation *)
    proc_info      : ProcPtr;
    try_scopes     : ADDRESS;  (* RTExRep.Scope *)
    var_map        : ADDRESS;  (* RTTypeMap.T *)
    gc_map         : ADDRESS;  (* reduced RTTypeMap.T *)
    import_info    : ImportPtr;
    link           : PROCEDURE ();
    main           : PROCEDURE ();
  END;

TYPE (* one of these is generated for each imported interface reference *)
  ImportInfo = RECORD
    import : ModulePtr; (* initially an ImportPtr *)
    binder : Binder;    (* returns address of "import" *)
  END;

TYPE (* one of these is generated for each top-level procedure *)
  ProcInfo = RECORD
    proc   : ADDRESS;
    name   : String;
  END;

TYPE
  TypeLink = RECORD
    next : ADDRESS;  (* init code patches this to be a TypeDefn *)
    type : INTEGER;  (* init code patches this to be a Typecode *)
  END;

TYPE
  RevPtr     = UNTRACED REF Revelation;
  Revelation = RECORD lhs_id, rhs_id: INTEGER; END;

(*------------------------------------------- linker generated type cells ---*)

TYPE
  MethodSuite = UNTRACED REF RECORD
    typecode : INTEGER;  (* Typecode *)
    methods  : (* ARRAY [0..n] OF *) ADDRESS;
  END;

TYPE
  TypeDefn = UNTRACED REF Typecell;
  Typecell = RECORD
    typecode         : INTEGER; (*Typecode*)
    lastSubTypeTC    : INTEGER; (*Typecode*)
    selfID           : INTEGER;
    fp               : Fingerprint;
    traced           : INTEGER; (* 0=>untraced, 1=>traced *)
    dataOffset       : INTEGER;
    dataSize         : INTEGER;
    dataAlignment    : INTEGER;
    methodOffset     : INTEGER;
    methodSize       : INTEGER;
    nDimensions      : INTEGER;       (* > 0 iff open array *)
    elementSize      : INTEGER;
    defaultMethods   : ADDRESS;       (* # NIL iff object type *)
    type_map         : ADDRESS;       (* RTTypeMap.T *)
    gc_map           : ADDRESS;       (* reduced RTTypeMap.T for collector *)
    type_desc        : ADDRESS;
    initProc         : TypeInitProc;  (* called by NEW *)
    linkProc         : TypeSetupProc; (* called during initialization *)
    parentID         : INTEGER;
    parent           : TypeDefn;
    children         : TypeDefn;
    sibling          : TypeDefn;
    brand            : String;
    name             : String;
    next             : TypeDefn;
  END;
(*
  dataOffset:
     for object types, the quantity to add to the address of the object
             to get to the fields that belong to this object type;
     for refs, unused;
     for open arrays, the quantity to add to the address of the array
             to get to the elements
  dataSize:
     for object types, the size of the fields that belong to this type;
     for refs, the size of the referent;
     for open array types, the size of the "open overhead", including
             padding to align the elements; i.e. ADR (a[0]) - ADR (a)
  dataAlignment:
     for object types, the alignment of the referent;
     for refs, the alignment of the referent;
     for open arrays, the alignment of the full array, including the header
*)

TYPE
  TypeInitProc  = PROCEDURE (ref: ADDRESS);
  TypeSetupProc = PROCEDURE (def: TypeDefn);

(*----------------------------------------- compiler generated references ---*)

CONST
  NilTypecode  : Typecode = 0;
  TextTypecode : Typecode = 1;

CONST
  SB = BITSIZE (ADDRESS) - 24;  (* # spare bits in a ref header *)

TYPE
  RefHeader = RECORD
    forwarded : BITS  1 FOR BOOLEAN    := FALSE; (* used during collection *)
    typecode  : BITS 20 FOR Typecode   := 0;     (* the typecode *)
    weak      : BITS  1 FOR BOOLEAN    := FALSE; (* any weakrefs? *)
    marka     : BITS  1 FOR BOOLEAN    := FALSE; (* used during collection *)
    markb     : BITS  1 FOR BOOLEAN    := FALSE; (* used during collection *)
    spare     : BITS SB FOR [0 .. 255] := 0;     (* for future expansion *)
  END;

TYPE  (* header for compiler generated TEXT literals (1-D open array of char)*)
  TextHeader = RECORD
    chars  : String;
    length : INTEGER;
  END;

(*--------------------------------- compiler generated procedure closures ---*)

CONST
  ClosureMarker = -1;

TYPE
  ProcedureClosure = RECORD
    marker : INTEGER; (* == -1 *)
    proc   : ADDRESS; (* address of a nested procedure's body *)
    frame  : ADDRESS; (* its parent's frame pointer *)
  END;

END RT0.
