(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: Target.i3                                             *)
(* Last Modified On Tue Dec 20 14:03:42 PST 1994 By kalsow     *)
(*      Modified On Thu May 20 08:20:38 PDT 1993 By muller     *)

INTERFACE Target;

(*  Modula-3 target description

    This interface defines the host representation of target
    values and the target architecture.  Its primary client
    is the Modula-3 compiler.

    Unless otherwise specified, all sizes and alignments are
    specified in bits.

    'pack' is defined to be 'size' rounded up to an alignment
    boundary (i.e. (size + align - 1) DIV align * align).
*)

IMPORT TInt, TWord;

TYPE
  Systems = {
    ALPHA32_VMS,
    ALPHA64_VMS,
    ALPHA_LINUX,
    ALPHA_OPENBSD,
    ALPHA_OSF,
    AMD64_DARWIN,
    AMD64_FREEBSD,
    AMD64_LINUX,
    AMD64_NETBSD,
    AMD64_OPENBSD,
    AMD64_SOLARIS,
    ARM_DARWIN,
    ARM_LINUX,    (* little endian, v6, hard float, vfp *)
    ARMEL_LINUX,  (* same thing *)
    FreeBSD4,
    I386_CYGWIN,
    I386_DARWIN,
    I386_FREEBSD,
    I386_INTERIX,
    I386_LINUX,
    I386_MINGW,
    I386_NETBSD,
    I386_NT,
    I386_OPENBSD,
    I386_SOLARIS,
    IA64_FREEBSD,
    IA64_HPUX,
    IA64_LINUX,
    IA64_NETBSD,
    IA64_NT,
    IA64_OPENBSD,
    IA64_VMS,
    LINUXLIBC6,
    MIPS64_OPENBSD, (* e.g. SGI *)
    MIPS64EL_OPENBSD, (* e.g. Loongson *)
    NT386,
    PA32_HPUX,
    PA64_HPUX,
    PPC32_OPENBSD,
    PPC64_DARWIN,
    PPC_DARWIN,
    PPC_LINUX,
    SOLgnu,
    SOLsun,
    SPARC32_LINUX,
    SPARC32_SOLARIS,
    SPARC64_LINUX,
    SPARC64_OPENBSD,
    SPARC64_SOLARIS,
    AMD64_NT,
    Undefined
  };

CONST
  SystemNames = ARRAY OF TEXT {
    "ALPHA32_VMS",
    "ALPHA64_VMS",
    "ALPHA_LINUX",
    "ALPHA_OPENBSD",
    "ALPHA_OSF",
    "AMD64_DARWIN",
    "AMD64_FREEBSD",
    "AMD64_LINUX",
    "AMD64_NETBSD",
    "AMD64_OPENBSD",
    "AMD64_SOLARIS",
    "ARM_DARWIN",
    "ARM_LINUX",    (* little endian, v6, hard float, vfp *)
    "ARMEL_LINUX",  (* same thing *)
    "FreeBSD4",
    "I386_CYGWIN",
    "I386_DARWIN",
    "I386_FREEBSD",
    "I386_INTERIX",
    "I386_LINUX",
    "I386_MINGW",
    "I386_NETBSD",
    "I386_NT",
    "I386_OPENBSD",
    "I386_SOLARIS",
    "IA64_FREEBSD",
    "IA64_HPUX",
    "IA64_LINUX",
    "IA64_NETBSD",
    "IA64_NT",
    "IA64_OPENBSD",
    "IA64_VMS",
    "LINUXLIBC6",
    "MIPS64_OPENBSD",
    "MIPS64EL_OPENBSD",
    "NT386",
    "PA32_HPUX",
    "PA64_HPUX",
    "PPC32_OPENBSD",
    "PPC64_DARWIN",
    "PPC_DARWIN",
    "PPC_LINUX",
    "SOLgnu",
    "SOLsun",
    "SPARC32_LINUX",
    "SPARC32_SOLARIS",
    "SPARC64_LINUX",
    "SPARC64_OPENBSD",
    "SPARC64_SOLARIS",
    "AMD64_NT"
  };

CONST
  OSNames = ARRAY OF TEXT { "POSIX", "WIN32" };
  EndianNames = ARRAY OF TEXT { "LITTLE", "BIG" }; 

TYPE
  M3BackendMode_t =
  {
    (* The primary modes are currently 0 and 3. *)
    IntegratedObject,   (* "0"  -- don't call m3_backend, M3CG produces object code *)
    IntegratedAssembly, (* "1"  -- don't call m3_backend, M3CG produces assembly code *)
    ExternalObject,     (* "2"  -- call m3_backend, it produces object code *)
    ExternalAssembly,   (* "3"  -- call m3_backend, it produces assembly code *)
    C                   (* "4"  -- don't call m3_backend, call compile_c, M3CG produces C *)
  };

CONST
  BackendModeStrings = ARRAY M3BackendMode_t OF TEXT
  { "IntegratedObject",
    "IntegratedAssembly",
    "ExternalObject",
    "ExternalAssembly",
    "C" };

  BackendIntegrated = ARRAY M3BackendMode_t OF BOOLEAN { TRUE, TRUE, FALSE, FALSE, TRUE };
  (* BackendAssembly = ARRAY M3BackendMode_t OF BOOLEAN { FALSE, TRUE, FALSE, TRUE, FALSE };  *)

(*-------------------------------------------------------- initialization ---*)

PROCEDURE Init (system: TEXT; osname := "POSIX"; backend_mode := M3BackendMode_t.ExternalAssembly): BOOLEAN;
(* Initialize the variables of this interface to reflect the architecture
   of "system".  Returns TRUE iff the "system" was known and the initialization
   was successful.  *)

VAR (*CONST*)
  System: Systems := Systems.Undefined; (* initialized by "Init" *)

VAR (*CONST*)
  System_name: TEXT := NIL; (* initialized by "Init" *)

VAR (*CONST*)
  OS_name: TEXT := NIL; (* initialized by "Init" *)

(*------------------------------------------ machine/code generator types ---*)

TYPE (* machine supported types *)
  CGType = {
    Word8,  Int8,    (* 8-bit, unsigned & signed integer *)
    Word16, Int16,   (* 16-bit, unsigned & signed integer *)
    Word32, Int32,   (* 32-bit, unsigned & signed integer *)
    Word64, Int64,   (* 64-bit, unsigned & signed integer *)
    Reel,            (* single precision reals *)
    LReel,           (* double precision reals *)
    XReel,           (* extended precision reals *)
    Addr,            (* addresses *)
    Struct,          (* a block of memory *)
    Void             (* not-a-type *)
  };

CONST
  TypeNames = ARRAY CGType OF TEXT {
    "Word.8",  "Int.8",
    "Word.16", "Int.16",
    "Word.32", "Int.32",
    "Word.64", "Int.64",
    "Reel", "LReel", "XReel",
    "Addr",
    "Struct",
    "Void"
  };

CONST
  SignedType = ARRAY CGType OF BOOLEAN {
     FALSE, TRUE,  FALSE, TRUE,  (* Word8 .. Int16 *)
     FALSE, TRUE,  FALSE, TRUE,  (* Word32 .. Int64 *)
     TRUE,  TRUE,  TRUE,         (* Reel .. XReel *)
     FALSE, FALSE, FALSE         (* Addr .. Void *)
  };

CONST
  OrdinalType = ARRAY CGType OF BOOLEAN {
    TRUE,  TRUE,  TRUE,  TRUE,   (* Word.8, Int.8, Word.16, Int.16 *)
    TRUE,  TRUE,  TRUE,  TRUE,   (* Word.32, Int.32, Word.64, Int.64 *)
    FALSE, FALSE, FALSE,         (* Reel, LReel, XReel *)
    TRUE,  FALSE, FALSE          (* Addr, Struct, Void *)
  };

CONST
  FloatType = ARRAY CGType OF BOOLEAN {
    FALSE, FALSE, FALSE, FALSE,  (* Word.8, Int.8, Word.16, Int.16 *)
    FALSE, FALSE, FALSE, FALSE,  (* Word.32, Int.32, Word.64, Int.64 *)
    TRUE,  TRUE,  TRUE,          (* Reel, LReel, XReel *)
    FALSE, FALSE, FALSE          (* Addr, Struct, Void *)
  };

(*-------------------------------------------------------- integer values ---*)

(* The bits of a target integer (in 2's complement) are stored in
   an array of small host values, with the low order bits in the first
   element of the array. *)

TYPE
  Int = TInt.Int;
  IByte = TInt.IByte;

TYPE
  Int_type = RECORD
    cg_type : CGType;    (* representation *)
    size    : CARDINAL;  (* bit size *)
    align   : CARDINAL;  (* minimum bit alignment *)
    pack    : CARDINAL;  (* minimum width bit packing *)
    bytes   : CARDINAL;  (* byte size *)
    min     : Int;       (* minimum value of this type *)
    max     : Int;       (* maximum value of this type *)
  END;

(*------------------------------------------------- floating point values ---*)

TYPE
  Precision = { Short, Long, Extended };

  Float = (*OPAQUE*) RECORD
    pre      : Precision;
    exponent : INTEGER;
    fraction : EXTENDED;
  END;

  Float_type = RECORD
    cg_type : CGType;     (* representation *)
    pre     : Precision;  (* precision *)
    size    : CARDINAL;   (* bit size *)
    align   : CARDINAL;   (* minimum bit alignment *)
    bytes   : CARDINAL;   (* byte size *)
    min     : Float;      (* minimum value of this type *)
    max     : Float;      (* maximum value of this type *)
  END;

(*----------------------------------------------- machine supported types ---*)

(* build a generic 32-bit/IEEE system description *)

VAR (*CONST*)
  Address   := Int_type{CGType.Addr, 32, 32, 32, 4, TInt.Zero, TWord.Max32}; (* default for the 32-bit platforms *)
  Integer   := Int32; (* default for the 32-bit platforms *)
  Word      := Word32; (* default for the 32-bit platforms *)

CONST
  Longint   = Int64;
  Long      = Word64;
  Int8      = Int_type{CGType.Int8,    8,  8,  8, 1,  TInt.Min8, TInt.Max8};
  Int16     = Int_type{CGType.Int16,  16, 16, 16, 2, TInt.Min16, TInt.Max16};
  Int32     = Int_type{CGType.Int32,  32, 32, 32, 4, TInt.Min32, TInt.Max32};
  Int64     = Int_type{CGType.Int64,  64, 64, 64, 8, TInt.Min64, TInt.Max64};
  Word8     = Int_type{CGType.Word8,   8,  8,  8, 1, TInt.Zero,  TWord.Max8};
  Word16    = Int_type{CGType.Word16, 16, 16, 16, 2, TInt.Zero, TWord.Max16};
  Word32    = Int_type{CGType.Word32, 32, 32, 32, 4, TInt.Zero, TWord.Max32};
  Word64    = Int_type{CGType.Word64, 64, 64, 64, 8, TInt.Zero, TWord.Max64};
  Char      = Word8;
  Void      = Int_type{CGType.Void,  0,  8,  0, 0, TInt.Zero, TInt.Zero};
  Real      = Float_type{CGType.Reel, Precision.Short, 32, 32, 4, Float{Precision.Short, 0, -3.40282346638528860x+38}, Float{Precision.Short, 0, 3.40282346638528860x+38}};
  Longreal  = Float_type{CGType.LReel, Precision.Long, 64, 64, 8, Float{Precision.Long, 0, -1.79769313486231570x+308}, Float{Precision.Long, 0, 1.79769313486231570x+308}};
VAR
  Extended  := Float_type{CGType.XReel, Precision.Extended, 64, 64, 8, Float{Precision.Extended, 0, -1.79769313486231570x+308}, Float{Precision.Extended, 0, 1.79769313486231570x+308}};

CONST (* sorted list of supported machine alignments *)
  Alignments = ARRAY [0..3] OF CARDINAL{8, 16, 32, 64};

(*------------------------------------------------------- procedure calls ---*)

TYPE
  CallingConvention = REF RECORD
    name               : TEXT;
    m3cg_id            : INTEGER;
    args_left_to_right : BOOLEAN;
    results_on_left    : BOOLEAN;
    standard_structs   : BOOLEAN;
  END;

VAR (*CONST*)
  DefaultCall: CallingConvention := NIL;

PROCEDURE FindConvention (nm: TEXT): CallingConvention;
(* Return the convention with name "nm".  Otherwise, return NIL. *)

PROCEDURE ConventionFromID (id: INTEGER): CallingConvention;
(* Return the convention with "m3cg_id" "id".  Otherwise, return NIL. *)


(*
  name => the name recognized in an <*EXTERNAL*> pragma, or as a prefix
          to a PROCEDURE declaration. 

  m3cg_id => tag used to indicate convention to the back end.

  args_left_to_right => Procedure arguments should be pushed
                        left->right or right->left.

  results_on_left => when the front-end is passing structures the return
                     result is the left-most parameter.  Otherwise, it is
                     the right-most parameter.

  standard_structs =>
     TRUE => the front-end will take care of all structure passing:
       by VALUE parameters: callers pass the address of the structure
          and the callee copies it into a temporary.
       return results: the caller passes as the left-most or right-most
          parameter the address of the temporary that will hold the result
          and the callee copies the value there.
     FALSE =>
       by VALUE parameters: the back-end is responsible.
          (ie. M3CG.T.Pop_struct will be called)
       return results: the caller passes as the left-most or right-most
          parameter the address of the temporary that will hold the result
          and the callee copies the value there.  The start_call, call,
          and exit_proc methods are called with t=Struct.
*)

(*--------------------------------------------------------------- atomics ---*)

VAR (*CONST*)
  Atomic_lock_free: ARRAY [CGType.Word8 .. CGType.Addr] OF BOOLEAN;
  (* TRUE => platform has lock-free atomic primitives for this type *)

(*--------------------------------------------------- misc. configuration ---*)

(* sizes are specified in bits *)

CONST
  Byte = 8;  (* minimum addressable unit (in bits) *)

VAR (*CONST*)
  Little_endian : BOOLEAN;
  (* TRUE => byte[0] of an integer contains its least-significant bits *)

  PCC_bitfield_type_matters: BOOLEAN;
  (* TRUE => the C compiler uses the type rather than the size of
     a bit-field to compute the alignment of the struct *)

  Structure_size_boundary: CARDINAL;
  (* every structure size must be a multiple of this *)

  Allow_packed_byte_aligned: BOOLEAN;
  (* Allow the compiler to align scalar types on byte boundaries when packing.
     The target processor must support byte alignment of scalar store and
     loads. This does not remove the restriction that bitfields may not cross
     word boundaries. *)

  (* NIL checking *)
  First_readable_addr: CARDINAL;
  (* Read or write references to addresses in the range [0..First_readable-1]
     will cause an address faults.  Hence, no explicit NIL checks are needed
     for dereferencing with offsets in this range. *)

  (* Thread stacks *)
  Jumpbuf_size     : CARDINAL; (* size of a "jmp_buf" *)

  (* floating point values *)
  All_floats_legal : BOOLEAN;
  (* If all bit patterns are "legal" floating point values (i.e. they can
     be assigned without causing traps or faults). *)

  Has_stack_walker: BOOLEAN;
  (* TRUE => generate PC-tables for exception scopes.  Otherwise, generate
       an explicit stack of exception handler frames. *)

  Setjmp: TEXT;
  (* The C name of the routine used to capture the machine state in
       an exception handler frame. *)

  Aligned_procedures: BOOLEAN;
  (* TRUE => all procedure values are aligned to at least Integer.align
     and can be safely dereferenced.  Otherwise, the code generated to
     test for nested procedures passed as parameters must be more
     elaborate (e.g. HPPA). *)

END Target.
