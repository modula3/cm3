(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* Last modified on Fri Oct 28 08:12:05 PDT 1994 by kalsow        *)
(*      modified on Wed Oct  5 14:41:36 PDT 1994 by ericv         *)
(*      modified on Sat Jun 27 15:25:23 PDT 1992 by muller        *)

INTERFACE Utypes;

FROM Ctypes IMPORT 
	long, unsigned_long, int, unsigned_int, short, unsigned_short,
        unsigned_char, char_star, signed_char, long_star;

(*** <sgidefs.h> ***)

(*
 * sgidefs.h - SGI/MIPS basic software generation system constants & types
 *
 * This file MUST be includable in any language
 * and in the 'C' case must be ANSI compliant
 * In particular this means NO long long ...
 *
 * Constants and types defined here are to support
 * multi-isa (instruction set architecture) coding
 *
 * Each catagory has a define that the compilation system will set
 * based on the environment the compile is initiated in. Programs
 * can test the define using the manifest constants defined here.
 * (e.g. #if (MIPS_FPSET == 16) )
 *)

(*
 * Floating Point register set
 * Define:
 *	MIPS_FPSET
 * Can take on the values 16 or 32
 *)

(*
 * Instruction Set Architecture
 * Define:
 *	MIPS_ISA
 *)
CONST
  MIPS_ISA_MIPS1	 = 1;	(* R2/3K *)
  MIPS_ISA_MIPS2	 = 2;	(* R4K/6K *)
  MIPS_ISA_MIPS3	 = 3;	(* R4K *)
  MIPS_ISA_MIPS4	 = 4;	(* TFP *)

(*
 * Subprogram Interface Model
 * Define:
 *	MIPS_SIM
 *)
CONST
  MIPS_SIM_ABI32	 = 1;	(* MIPS MSIG calling convention *)
  MIPS_SIM_ABI64	 = 2;	(* MIPS 64 calling convention *)

(*
 * Data Types Sizes (C and C++)
 * Defines:
 *	MIPS_SZINT
 *	MIPS_SZLONG
 *	MIPS_SZPTR
 *
 * These can take on the values: 32, 64, 128
 *)

(*
 * Compilation Environments
 *	various compilers may offer a set of different compilation environments
 *	each one will pre-define the above defines appropriately.
 * The MIPS ABI environment will pre-define the following:
 *	(cc -systype=XXX)
 *
 *	-D_MIPS_FPSET=16 -D_MIPS_ISA=MIPS_ISA_MIPS1
 *	-D_MIPS_SIM=MIPS_SIM_ABI32 -D_MIPS_SZINT=32
 *	-D_MIPS_SZLONG=32 -D_MIPS_SZPTR=32
 *
 * The MIPS 64 bit environment will pre-define the following
 *	(cc -systype=XXX)
 *	-D_MIPS_FPSET=32 -D_MIPS_ISA=MIPS_ISA_MIPS3
 *	-D_MIPS_SIM=MIPS_SIM_ABI64 -D_MIPS_SZINT=32
 *	-D_MIPS_SZLONG=64 -D_MIPS_SZPTR=64
 *
 *)

(*
 * Language Specific
 * Type psint_t - a pointer sized int - this can be used:
 *	a) when casting a pointer so can perform e.g. a bit operation
 *	b) as a return code for functions incorrectly typed as int but
 *	   return a pointer.
 * User level code can also use the ANSI std ptrdiff_t, defined in stddef.h
 *	in place of psint_t
 * Type scint_t - a 'scaling' int - used when in fact one wants an 'int'
 *	that scales when moving to say 64 bit. (e.g. byte counts, bit lens)
 *)

(* Assuming MIPS ABI environment:
 *
 * MIPS_ISA    = MIPS_ISA_MIPS1        (* instruction set architecture *)
 * MIPS_SIM    = MIPS_SIM_ABI32        (* subprogram interface model *)
 * MIPS_SZINT  = 32                     (* size of "int" *)
 * MIPS_SZLONG = 32                     (* size of "long" *)
 * MIPS_SZPTR  = 32                     (* size of "void *" *)
 * MIPS_FPSET  = 16                     (* floating point register set *)
 *)

TYPE
  int32_t      = int;
  uint32_t     = unsigned_int;
  int64_t      = RECORD hi, lo: int END;
  uint64_t     = RECORD hi, lo: unsigned_int END;
  psint_t      = int32_t;
  psunsigned_t = uint32_t;
  scint_t      = int32_t;
  scunsigned_t = uint32_t;

(*** <sys/types.h> ***)

TYPE
  (* POSIX Extensions *)
  uchar_t  = unsigned_char;
  ushort_t = unsigned_short;
  uint_t   = unsigned_int;
  ulong_t  = unsigned_long;

  (* Primarily Kernel types *)
  addr_t    = char_star;  (* ?<core address> type *)
  caddr_t   = char_star;  (* ?<core address> type *)
  daddr_t   = long;       (* <disk address> type *)
  pgno_t    = long;       (* virtual page number type *)
  pfn_t     = uint32_t; (* physical page number type *)
  cnt_t     = short;      (* ?<count> type *)

TYPE
  boolean_t = int;

CONST
  B_FALSE = 0;
  B_TRUE = 1;

(*
 * The following type is for various kinds of identifiers.  The
 * actual type must be the same for all since some system calls
 * (such as sigsend) take arguments that may be any of these
 * types.  The enumeration type idtype_t defined in sys/procset.h
 * is used to indicate what type of id is being specified.
 *)

TYPE
  id_t = long;	(* A process id,	*)
		(* process group id,	*)
		(* session id, 		*)
		(* scheduling class id,	*)
		(* user id, or group id.*)


(* Typedefs for dev_t components *)

TYPE
  major_t = ulong_t;	(* major part of device number *)
  minor_t = ulong_t;	(* minor part of device number *)

TYPE
  mode_t  = unsigned_long;		(* file attrs *)
  dev_t   = unsigned_long;		(* device type *)
  uid_t   = long;
  gid_t   = long;
  nlink_t = unsigned_long;	(* used for link counts *)
  pid_t   = long;		(* proc & grp IDs  *)
  ino_t   = unsigned_long;		(* <inode> type *)

  swblk_t = long;
  off_t   = long;		(* ?<offset> type *)
  paddr_t = unsigned_long;	(* <physical address> type *)
  key_t   = int;		(* IPC key type *)
  use_t   = unsigned_char;		(* use count for swap.  *)
  sysid_t = short;
  index_t = short;
  lock_t  = unsigned_int;		(* <spinlock> type *)
  cpuid_t = signed_char;	(* cpuid *)

  size_t    = unsigned_int;
  ssize_t   = int;
  time_t    = long;		(* <time> type *)
  clock_t   = long; (* relative time in a specified resolution *)
  bitnum_t  = scunsigned_t;	(* bit number *)
  bitlen_t  = scunsigned_t;	(* bit length *)

(* a cpu machine register - user interface definitions *)

  machreg_t = unsigned_int;

(* a fpu machine register - user interface definitions *)

  fpreg_t   = uint32_t;

  processorid_t   = int;	(* a processor name *)
  toid_t          = int;	(* timeout id *)
  qaddr_t         = long_star;	(* XXX should be typedef quad * qaddr_t *)
  inst_t          = uint32_t;	(* an instruction *)

  int8_t          = signed_char;
  int16_t         = short;
  u_int8_t        = unsigned_char;
  u_int16_t       = unsigned_short;
  u_int32_t       = uint32_t;

(*
 * The following is the value of type id_t to use to indicate the
 * caller's current id.  See procset.h for the type idtype_t
 * which defines which kind of id is being specified.
 *)
CONST
  P_MYID = (-1);
  NOPID  = (-1);
  NODEV  = (-1);
  P_MYPID = 0;

(*
 * A host identifier is used to uniquely define a particular node
 * on an rfs network.  Its type is as follows.
 *)
TYPE
  hostid_t  = long;

(*
 * The following value of type is	: hostid_t used to indicate the
 * current host.
 *)
CONST
  P_MYHOSTID  = (-1);

TYPE
  k_sigset_t = RECORD                (* signal set type *)
    sigbits : ARRAY [0..1] OF uint32_t;
  END;
  k_fltset_t = uint32_t;     (* kernel fault set type *)


(************************ BSD stuff ********************)

(*** <sys/bsdtypes.h> ***)

(*
 * These typedefs are needed by lots of BSD derived code - especially
 * networking. Alas these are not POSIX compliant so cannot be part of
 * sys/types.h
 *)
  struct__physadr = RECORD r: ARRAY [0..0] OF int; END;
  physadr         = struct__physadr;

  unchar   = unsigned_char;
  u_char   = unsigned_char;
  ushort   = unsigned_short;
  u_short  = unsigned_short;
  uint     = unsigned_int;
  u_int    = unsigned_int;
  ulong    = unsigned_long;
  u_long   = unsigned_long;

  struct__quad = RECORD val: ARRAY [0..1] OF long; END;
  quad         = struct__quad;

(*** <sys/mkdev.h> ***)

CONST
  NBITSMAJOR	 = 14;	(* # of SVR4 major device bits *)
  NBITSMINOR	 = 18;	(* # of SVR4 minor device bits *)
  MAXMAJ	 = 16_ff;	(* XXXdh this may change if we decide to make
				 * lboot size the MAJOR array flexibly. *)

  MAXMIN	 = 16_3ffff;	(* MAX minor *)

<*EXTERNAL*> PROCEDURE makedev(maj: major_t; min: minor_t): dev_t;
<*EXTERNAL*> PROCEDURE major(dev: dev_t): major_t;
<*EXTERNAL*> PROCEDURE minor(dev: dev_t): minor_t;


(*** <sys/select.h> ***)

CONST
  (*
   * Select uses bit masks of file descriptors in longs.
   * These macros manipulate such bit fields (the filesystem macros use chars).
   * FD_SETSIZE may be defined by the user, but the default here
   * should be >= NOFILE (param.h).
   *)
  FD_SETSIZE = 1024;
  NBBY = 8;                           (* number of bits in a byte *)

TYPE
  fd_mask        = unsigned_long;
 
CONST
  NFDBITS = BYTESIZE (fd_mask) * NBBY;      (* bits per mask (power of 2!)*)

PROCEDURE howmany (x, y: int): int;

TYPE
  struct_fd_set = RECORD
       fds_bits: ARRAY [0 .. ((FD_SETSIZE + NFDBITS - 1) DIV NFDBITS) - 1]
                       OF fd_mask;
    END;
  fd_set = struct_fd_set;

PROCEDURE FD_SET   (n: int; p: UNTRACED REF fd_set): int;
PROCEDURE FD_CLEAR (n: int; p: UNTRACED REF fd_set): int;
PROCEDURE FD_ISSET (n: int; p: UNTRACED REF fd_set): int;
PROCEDURE FD_ZERO  (p: UNTRACED REF fd_set);

END Utypes.
