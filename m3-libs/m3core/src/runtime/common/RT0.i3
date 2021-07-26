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
  Byte        = [0 .. 255];              (* fits in 8 bits *)
  Typecode    = [0 .. 16_FFFFF];         (* can fit in 20 bits *)
  Fingerprint = ARRAY [0..7] OF BITS 8 FOR [0..255]; (* 64 bits *)
  String      = UNTRACED REF CHAR;       (* a '\000' terminated string *)
  ModulePtr   = UNTRACED REF ModuleInfo;
  ImportPtr   = UNTRACED REF ImportInfo;
  ProcPtr     = UNTRACED REF ProcInfo;
  Binder      = PROCEDURE (mode: INTEGER): ModulePtr;

TYPE (* allocated at offset 0 of each compilation unit's global data *)
  ModuleInfo = RECORD
    file           : String;
    type_cells     : TypeDefn;
    type_cell_ptrs : TypeLinkPtr;
    full_rev       : RevPtr;
    partial_rev    : RevPtr;
    proc_info      : ProcPtr;
    try_scopes     : ADDRESS;  (* RTExStack.Scope *)
    var_map        : ADDRESS;  (* RTTypeMap.T *)
    gc_map         : ADDRESS;  (* reduced RTTypeMap.T *)
    imports        : ImportPtr;
    link_state     : INTEGER;  (* 0=unlinked, 1=linking, 2=linked *)
    binder         : Binder;
    gc_flags       : INTEGER;  (* 0=none, 1=gen, 2=inc, 3=both *)
  END;

CONST
  GC_none = 0;
  GC_gen  = 1;
  GC_inc  = 2;
  GC_both = 3;

TYPE (* one of these is generated for each imported interface reference *)
  ImportInfo = RECORD
    import : ModulePtr;
    binder : Binder;    (* returns "import" pointer *)
    next   : ImportPtr;
  END;

TYPE (* one of these is generated for each top-level procedure *)
  ProcInfo = RECORD
    proc   : ADDRESS;
    name   : String;
  END;

TYPE
  TypeLinkPtr = UNTRACED REF TypeLink;
  TypeLink = RECORD
    defn     : TypeDefn;  (* initially a pointer to the next TypeLink *)
    typecode : INTEGER;   (* initially the compile-time UID of the type *)
  END;

TYPE
  RevPtr     = UNTRACED REF Revelation;
  Revelation = RECORD lhs_id, rhs_id: INTEGER; END;

(*------------------------------------------- linker generated type cells ---*)

TYPE
  TypeKind = { Unknown, Ref, Obj, Array };

TYPE
  TypeDefn = UNTRACED REF Typecell;
  Typecell = RECORD
    typecode         : INTEGER; (*Typecode*)
    selfID           : INTEGER;
    fp               : Fingerprint;
    traced           : Byte;  (* == ORD (BOOLEAN) *)
    kind             : Byte;  (* == ORD (TypeKind) *)
    link_state       : Byte;
    dataAlignment    : Byte;
    dataSize         : INTEGER;
    type_map         : ADDRESS;       (* RTTypeMap.T *)
    gc_map           : ADDRESS;       (* reduced RTTypeMap.T for collector *)
    type_desc        : ADDRESS;       (* enhanced RTTipe map for new pickles *)
    initProc         : TypeInitProc;  (* called by NEW *)
    brand_ptr        : BrandPtr;
    name             : String;
    next             : TypeDefn;
  END;

TYPE (* OBJECT types have this additional goo *)
  ObjectTypeDefn = UNTRACED REF ObjectTypecell;
  ObjectTypecell = RECORD
    common           : Typecell;
    parentID         : INTEGER;
    linkProc         : TypeSetupProc; (* called during initialization *)
    dataOffset       : INTEGER;
    methodOffset     : INTEGER;
    methodSize       : INTEGER;
    defaultMethods   : ADDRESS;
    parent           : TypeDefn;
  END;

TYPE (* and REF open array types have this additional goo *)
  ArrayTypeDefn = UNTRACED REF ArrayTypecell;
  ArrayTypecell = RECORD
    common           : Typecell;
    nDimensions      : INTEGER;
    elementSize      : INTEGER;
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
(* NOTE WELL:
   The above descriptions of dataSize and dataAligment are correct as
   the compiler front end initializes them, with the note that neither
   accounts for the heap object header word.  They can describe a heap
   object with any byte size and alignment. 

   The runtime allocation (RTAllocator.m3, AllocTraced) Assumes the
   next available address can have any byte alignment and pads it up
   to ensure dataAlignment.  It adds to the size to account for the
   size of the header, but does not account for the alignment of the
   header.

   This would lead to alignment faults, but for the fact that, in between,
   startup code in RTType.m3 alters the values.  However, not by adjusting
   dataAlignment, but by rounding up dataSize for every type to a multiple
   of the header alignment.  Thus, the previously allocated object will
   have left the next available address aligned for a header. 

   Since the header is a native word, the only time the padding in
   AllocTraced would do anything is on a 32-bit target, for an object
   containing a 64-bit aligned component, e.g. LONGINT or LONGREAL. 
*) 

TYPE
  TypeInitProc  = PROCEDURE (ref: ADDRESS);
  TypeSetupProc = PROCEDURE (def: TypeDefn);

TYPE
  BrandPtr = UNTRACED REF RECORD
    length : INTEGER;
    chars  : ARRAY [0..16_ffffff (*length-1*)] OF CHAR;
  END;

(*----------------------------------------- compiler generated references ---*)

CONST
  NilTypecode       : Typecode = 0;
  TextLitTypecode   : Typecode = 1;
  AddressTypecode   : Typecode = 2;
  RefanyTypecode    : Typecode = 3;
  RootTypecode      : Typecode = 4;
  UnRootTypecode    : Typecode = 5;
  MutexTypecode     : Typecode = 6;
  FirstUserTypecode : Typecode = 7;

CONST (* Type UIDs for the builtin types *)
  MutexID       = 16_1541f475;
  TextLiteralID = 1837570855 (*16_6d871b27*);
    (* ARRAY BOOLEAN OF INTEGER { 1837570855 (*16_6d871b27*), 16_72558b22 }
       [BITSIZE (INTEGER) = 64]; *)

CONST
  SB = BITSIZE (ADDRESS) - 26;  (* # spare bits in a ref header *)

TYPE
  RefHeader = RECORD
    forwarded : BITS  1 FOR BOOLEAN    := FALSE; (* used during collection *)
    typecode  : BITS 20 FOR Typecode   := 0;     (* the typecode *)
    dirty     : BITS  1 FOR BOOLEAN    := FALSE; (* used during collection *)
    gray      : BITS  1 FOR BOOLEAN    := FALSE; (* used during collection *)
    weak      : BITS  1 FOR BOOLEAN    := FALSE; (* any weakrefs? *)
    marka     : BITS  1 FOR BOOLEAN    := FALSE; (* used during collection *)
    markb     : BITS  1 FOR BOOLEAN    := FALSE; (* used during collection *)
    spare     : BITS SB FOR [0 .. 63]  := 0;     (* for future expansion *)
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

(*------------------------------------------------------------ exceptions ---*)

TYPE
  ExceptionPtr    = UNTRACED REF ExceptionDesc;
  ExceptionUID    = INTEGER; (* == fingerprint of fully qualified name *)
  ExceptionArg    = ADDRESS; (* actually, it's an untyped 4-byte field *)
  ActivationPtr   = UNTRACED REF RaiseActivation;

TYPE
  ExceptionDesc = RECORD
    uid      : ExceptionUID;
    name     : String;
    implicit : INTEGER;  (* == ORD (BOOLEAN) *)
  END;

TYPE
  RaiseActivation = RECORD
    exception : ExceptionPtr;
    arg       : ExceptionArg;
    module    : ModulePtr;  (* if available, the raising module that called RAISE *)
    line      : INTEGER;    (* if available, the source line number of the RAISE *)
    pc        : ADDRESS;    (* if available, the PC of the RAISE *)
    info0     : ADDRESS;    (* misc. info *)
    info1     : ADDRESS;    (* misc. info *)
    un_except : ExceptionPtr; (* the unhandled exception being reported *)
    un_arg    : ExceptionArg; (* the argument to the unhandled exception *)
  END;

END RT0.
