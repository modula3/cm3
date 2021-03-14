(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: TargetT.i3                                             *)
(* Last Modified On Tue Dec 20 14:03:42 PST 1994 By kalsow     *)
(*      Modified On Thu May 20 08:20:38 PDT 1993 By muller     *)

INTERFACE TargetT;

IMPORT Target;
FROM Target IMORT Int_type, Float_type, CallingConvention;

(*  Modula-3 target description *)

TYPE T = RECORD
  system_name: TEXT; (* I386_LINUX, etc., use sparingly *)
  os_name: TEXT; (* POSIX, WIN32 *)
  backendmode: Target.M3BackendMode_t;
  address   : Int_type;
  integer   : Int_type;
  longint   : Int_type;
  word      : Int_type;
  long      : Int_type;
  real      : Float_type;
  longreal  : Float_type;
  extended  : Float_type;
  int8      : Int_type;
  int16     : Int_type;
  int32     : Int_type;
  int64     : Int_type;
  word8     : Int_type;
  word16    : Int_type;
  word32    : Int_type;
  word64    : Int_type;
  woid      : Int_type;
  char      : Int_type;
  defaultCall: CallingConvention := NIL;
  atomic_lock_free: ARRAY [CGType.Word8 .. CGType.Addr] OF BOOLEAN;
    (* TRUE => platform has lock-free atomic primitives for this type *)
  endian    := Endian.Little;
    (* Little => byte[0] of an integer contains its least-significant bits *)
  PCC_bitfield_type_matters: BOOLEAN := TRUE;
    (* TRUE => the C compiler uses the type rather than the size of
       a bit-field to compute the alignment of the struct *)
  structure_size_boundary: CARDINAL;
    (* every structure size must be a multiple of this *)
  allow_packed_byte_aligned: BOOLEAN;
    (* Allow the compiler to align scalar types on byte boundaries when packing.
       The target processor must support byte alignment of scalar store and
       loads. This does not remove the restriction that bitfields may not cross
       word boundaries. *)

  (* NIL checking *)
  first_readable_addr: CARDINAL;
    (* Read or write references to addresses in the range [0..First_readable-1]
       will cause an address faults.  Hence, no explicit NIL checks are needed
       for dereferencing with offsets in this range. *)

  (* Thread stacks *)
  jumpbuf_size     : CARDINAL; (* size of a "jmp_buf" *)
  jumpbuf_align    : CARDINAL; (* alignment of a "jmp_buf" *)

  (* floating point values *)
  all_floats_legal : BOOLEAN;
    (* If all bit patterns are "legal" floating point values (i.e. they can
       be assigned without causing traps or faults). *)

  has_stack_walker: BOOLEAN;
    (* TRUE => generate PC-tables for exception scopes.  Otherwise, generate
       an explicit stack of exception handler frames. *)

  setjmp: TEXT;
    (* The C name of the routine used to capture the machine state in
       an exception handler frame. *)

  aligned_procedures: BOOLEAN;
    (* TRUE => all procedure values are aligned to at least Integer.align
       and can be safely dereferenced.  Otherwise, the code generated to
       test for nested procedures passed as parameters must be more
       elaborate (e.g. HPPA). *)
END T;

END TargetT.
