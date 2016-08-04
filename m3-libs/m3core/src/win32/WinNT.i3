(* Copyright (C) 1994, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* by Stephen Harrison                                       *)
(*                                                           *)
(* Last modified on Tue Nov  8 15:08:16 PST 1994 by kalsow   *)
(*      modified on Wed Feb 10 19:47:09 PST 1993 by harrison *)

INTERFACE WinNT;

(* Corresponds to build version 0082 of "winnt.h".  See that file for details. 
 *
 * This module defines the 32-Bit Windows types and constants that are
 * defined by NT, but exposed through the Win32 API.
 *
 * We ignore UNICODE for now.
 * 
 * And we ignore architecture dependent declarations.
 *
 * Some basic type declarations have been moved to WinBaseTypes.i3 to avoid circular
 * dependencies.
 *)

<* PRAGMA UNALIGNED *>

IMPORT Word, WinBaseTypes;
FROM Word IMPORT And, Or, Not;
FROM Ctypes IMPORT char_star;

TYPE
  ANYSIZE_ARRAY = [0 .. 0];

  (* reexport base types *)

  UINT8 = WinBaseTypes.UINT8;
  UINT16 = WinBaseTypes.UINT16;
  UINT32 = WinBaseTypes.UINT32;
  UINT64 = WinBaseTypes.UINT64;
  INT8 = WinBaseTypes.INT8;
  INT16 = WinBaseTypes.INT16;
  INT32 = WinBaseTypes.INT32;
  INT64 = WinBaseTypes.INT64;
  SIZE_T = WinBaseTypes.SIZE_T;   (* same size as a pointer, unsigned *)
  SSIZE_T = WinBaseTypes.SSIZE_T; (* same size as a pointer, signed, aka ptrdiff_t *)
  PUINT8 = WinBaseTypes.PUINT8;
  PUINT16 = WinBaseTypes.PUINT16;
  PUINT32 = WinBaseTypes.PUINT32;
  PUINT64 = WinBaseTypes.PUINT64;
  PINT8 = WinBaseTypes.PINT8;
  PINT16 = WinBaseTypes.PINT16;
  PINT32 = WinBaseTypes.PINT32;
  PINT64 = WinBaseTypes.PINT64;
  PVOID = WinBaseTypes.PVOID;
  PCVOID = WinBaseTypes.PCVOID;
  BOOL = WinBaseTypes.BOOL;
  CCHAR = WinBaseTypes.CCHAR;
  HANDLE = WinBaseTypes.HANDLE;
  PBOOL = WinBaseTypes.PBOOL;
  PFLOAT = WinBaseTypes.PFLOAT;
  PHANDLE = WinBaseTypes.PHANDLE;
  WFLOAT = WinBaseTypes.WFLOAT;
  WCHAR = WinBaseTypes.WCHAR; (* wc, 16-bit UNICODE character *)
  PSTR = WinBaseTypes.PSTR;
  PCSTR = WinBaseTypes.PCSTR;
  PWSTR = WinBaseTypes.PWSTR;
  PCWSTR = WinBaseTypes.PCWSTR;
  TCHAR = WinBaseTypes.TCHAR;
  PTSTR = WinBaseTypes.PTSTR;
  PCTSTR = WinBaseTypes.PCTSTR;
  PSZ = WinBaseTypes.PSZ;

  (* funny names for base types *)

  BYTE = UINT8;
  UCHAR = UINT8;
  PUCHAR = PUINT8;
  PBYTE = PUINT8;

  PSHORT = PINT16;
  SHORT = INT16;
  USHORT = UINT16;
  PUSHORT = PUINT16;
  WORD = UINT16;
  PWORD = PUINT16;

  ULONG = UINT32;
  DWORD = UINT32;
  UINT = UINT32;
  INT = INT32;
  PULONG = PUINT32;
  LONG = INT32;
  PINT = PINT32;
  LPINT = PINT32;
  LPLONG = PINT32;
  PDWORD = PUINT32;
  PUINT = PUINT32;
  PLONG = PINT32;

  LPBOOL = PBOOL;
  LPBYTE = PBYTE;
  LPWORD = PWORD;
  LPDWORD = PDWORD;
  LPVOID = PVOID;
  LPCVOID = PCVOID;

  DWORD_PTR = SIZE_T; (* same size as a pointer, unsigned *)
  UINT_PTR = SIZE_T; (* same size as a pointer, unsigned *)
  ULONG_PTR = SIZE_T; (* same size as a pointer, unsigned *)
  INT_PTR = SSIZE_T; (* same size as a pointer, signed *)
  LONG_PTR = SSIZE_T; (* same size as a pointer, signed *)

(*
 * UNICODE (Wide Character) types
 *)
  PWCHAR  = PWSTR;
  LPWCH   = PWSTR;
  PWCH    = PWSTR;
  LPCWCH  = PCWSTR;
  PCWCH   = PCWSTR;
  NWPSTR  = PWSTR;
  LPWSTR  = PWSTR;
  LPCWSTR = PCWSTR;

(*
 * ANSI (Multi-byte Character) types
 *)
  PCHAR  = PSTR;
  LPCH   = PSTR;
  PCH    = PSTR;

  LPCCH  = PCSTR;
  PCCH   = PCSTR;
  NPSTR  = PSTR;
  LPSTR  = PSTR;
  LPCSTR = PCSTR;

(*
 * Neutral ANSI types
 *)
  PTCHAR = PTSTR;
  LPTCH   = PTSTR;
  PTCH    = PTSTR;
  LPTSTR  = PTSTR;
  LPCTSTR = PCTSTR;
  LP      = PWSTR;

  LCID    = UINT32;
  PLCID   = PUINT32;
  LANGID  = UINT16;

CONST
  APPLICATION_ERROR_MASK       = 16_20000000;
  ERROR_SEVERITY_SUCCESS       = 16_00000000;
  ERROR_SEVERITY_INFORMATIONAL = 16_40000000;
  ERROR_SEVERITY_WARNING       = 16_80000000;
  ERROR_SEVERITY_ERROR         = 16_C0000000;

TYPE
  PLARGE_INTEGER = UNTRACED REF LARGE_INTEGER;
  LARGE_INTEGER = RECORD
    LowPart: UINT32;
    HighPart: INT32;
  END;

  PULARGE_INTEGER = UNTRACED REF ULARGE_INTEGER;
  ULARGE_INTEGER = RECORD
    LowPart: UINT32;
    HighPart: UINT32;
  END;

  LUID = LARGE_INTEGER;
  PLUID = UNTRACED REF LUID;

CONST
  UNICODE_NULL: WCHAR = 0;

TYPE
  (* !!! Name clash with Modula-3 built-in.  BOOLEAN -> WBOOLEAN. *)
  WBOOLEAN = CCHAR;
  PBOOLEAN = UNTRACED REF WBOOLEAN;

(*
 *  Doubly linked list structure.  Can be used as either a list head, or
 *  as link words.
 *)

  PLIST_ENTRY = UNTRACED REF LIST_ENTRY;
  LIST_ENTRY = RECORD
    Flink: UNTRACED REF LIST_ENTRY;
    Blink: UNTRACED REF LIST_ENTRY;
  END;


(*
 *  Singly linked list structure. Can be used as either a list head, or
 *  as link words.
 *)

TYPE
  PSINGLE_LIST_ENTRY = UNTRACED REF SINGLE_LIST_ENTRY;
  SINGLE_LIST_ENTRY = RECORD
    Next: UNTRACED REF SINGLE_LIST_ENTRY;
  END;

CONST
  MINCHAR  = 16_80;
  MAXCHAR  = 16_7f;
  MINSHORT = 16_8000;
  MAXSHORT = 16_7fff;
  MINLONG  = 16_80000000;
  MAXLONG  = 16_7fffffff;
  MAXBYTE  = 16_ff;
  MAXWORD  = 16_ffff;
  MAXDWORD = 16_ffffffff;

  STATUS_WAIT_0                  : UINT32 = 16_00000000;
  STATUS_ABANDONED_WAIT_0        : UINT32 = 16_00000080;
  STATUS_USER_APC                : UINT32 = 16_000000C0;
  STATUS_TIMEOUT                 : UINT32 = 16_00000102;
  STATUS_PENDING                 : UINT32 = 16_00000103;
  STATUS_DATATYPE_MISALIGNMENT   : UINT32 = 16_80000002;
  STATUS_BREAKPOINT              : UINT32 = 16_80000003;
  STATUS_SINGLE_STEP             : UINT32 = 16_80000004;
  STATUS_ACCESS_VIOLATION        : UINT32 = 16_C0000005;
  STATUS_ILLEGAL_INSTRUCTION     : UINT32 = 16_C000001D;
  STATUS_NONCONTINUABLE_EXCEPTION: UINT32 = 16_C0000025;
  STATUS_INVALID_DISPOSITION     : UINT32 = 16_C0000026;
  STATUS_ARRAY_BOUNDS_EXCEEDED   : UINT32 = 16_C000008C;
  STATUS_FLOAT_DENORMAL_OPERAND  : UINT32 = 16_C000008D;
  STATUS_FLOAT_DIVIDE_BY_ZERO    : UINT32 = 16_C000008E;
  STATUS_FLOAT_INEXACT_RESULT    : UINT32 = 16_C000008F;
  STATUS_FLOAT_INVALID_OPERATION : UINT32 = 16_C0000090;
  STATUS_FLOAT_OVERFLOW          : UINT32 = 16_C0000091;
  STATUS_FLOAT_STACK_CHECK       : UINT32 = 16_C0000092;
  STATUS_FLOAT_UNDERFLOW         : UINT32 = 16_C0000093;
  STATUS_INTEGER_DIVIDE_BY_ZERO  : UINT32 = 16_C0000094;
  STATUS_INTEGER_OVERFLOW        : UINT32 = 16_C0000095;
  STATUS_PRIVILEGED_INSTRUCTION  : UINT32 = 16_C0000096;
  STATUS_STACK_OVERFLOW          : UINT32 = 16_C00000FD;
  STATUS_CONTROL_C_EXIT          : UINT32 = 16_C000013A;
  DBG_EXCEPTION_HANDLED             = 16_00010001;
  STATUS_SEGMENT_NOTIFICATION       = 16_40000005;
  DBG_CONTROL_BREAK                 = 16_40010008;
  DBG_COMMAND_EXCEPTION             = 16_40010009;
  STATUS_GUARD_PAGE_VIOLATION       = 16_80000001;
  STATUS_IN_PAGE_ERROR              = 16_C0000006;
  STATUS_INVALID_HANDLE             = 16_C0000008;
  STATUS_NO_MEMORY                  = 16_C0000017;
  STATUS_FLOAT_MULTIPLE_FAULTS      = 16_C00002B4;
  STATUS_FLOAT_MULTIPLE_TRAPS       = 16_C00002B5;
  STATUS_REG_NAT_CONSUMPTION        = 16_C00002C9;
  STATUS_SXS_EARLY_DEACTIVATION     = 16_C015000F;
  STATUS_SXS_INVALID_DEACTIVATION   = 16_C0150010;

  MAXIMUM_WAIT_OBJECTS = 64;    (* Maximum number of wait objects *)

  MAXIMUM_SUSPEND_COUNT = MAXCHAR; (* Maximum times thread can be suspended *)

TYPE
KSPIN_LOCK = SIZE_T;

CONST
  EXCEPTION_NONCONTINUABLE = 16_1; (* Noncontinuable exception *)
  EXCEPTION_MAXIMUM_PARAMETERS = 15; (* maximum number of exception
                                        parameters *)

(*
 * Exception record definition.
 *)

TYPE
  PEXCEPTION_RECORD = UNTRACED REF EXCEPTION_RECORD;
  EXCEPTION_RECORD = RECORD
    ExceptionCode   : UINT32;
    ExceptionFlags  : UINT32;
    ExceptionRecord : UNTRACED REF EXCEPTION_RECORD;
    ExceptionAddress: PVOID;
    NumberParameters: UINT32;
    ExceptionInformation: ARRAY
                              [0 .. EXCEPTION_MAXIMUM_PARAMETERS - 1] OF
                              SIZE_T;
  END;


(*
 * Typedef for pointer returned by exception_info()
 *)

TYPE
  PEXCEPTION_POINTERS = UNTRACED REF EXCEPTION_POINTERS;
  EXCEPTION_POINTERS = RECORD
    ExceptionRecord: PEXCEPTION_RECORD;
    ContextRecord: PVOID;  (* !!! Architecture-dependent
                                          context pointer *)
  END;


CONST
  PROCESS_TERMINATE         = 16_0001;
  PROCESS_CREATE_THREAD     = 16_0002;
  PROCESS_SET_SESSIONID     = 16_0004;
  PROCESS_VM_OPERATION      = 16_0008;
  PROCESS_VM_READ           = 16_0010;
  PROCESS_VM_WRITE          = 16_0020;
  PROCESS_DUP_HANDLE        = 16_0040;
  PROCESS_CREATE_PROCESS    = 16_0080;
  PROCESS_SET_QUOTA         = 16_0100;
  PROCESS_SET_INFORMATION   = 16_0200;
  PROCESS_QUERY_INFORMATION = 16_0400;
  PROCESS_SUSPEND_RESUME    = 16_0800;
  PROCESS_QUERY_LIMITED_INFORMATION = 16_1000;
  PROCESS_ALL_ACCESS = Or(STANDARD_RIGHTS_REQUIRED,
                       Or(SYNCHRONIZE,
                          16_FFF)); (* This changes to FFFF depending on #defines *)

  THREAD_TERMINATE            = 16_0001;
  THREAD_SUSPEND_RESUME       = 16_0002;
  THREAD_GET_CONTEXT          = 16_0008;
  THREAD_SET_CONTEXT          = 16_0010;
  THREAD_SET_INFORMATION      = 16_0020;
  THREAD_QUERY_INFORMATION    = 16_0040;
  THREAD_SET_THREAD_TOKEN     = 16_0080;
  THREAD_IMPERSONATE          = 16_0100;
  THREAD_DIRECT_IMPERSONATION = 16_0200;
  THREAD_SET_LIMITED_INFORMATION = 16_0400;
  THREAD_QUERY_LIMITED_INFORMATION = 16_0800;

  THREAD_ALL_ACCESS = Or(STANDARD_RIGHTS_REQUIRED,
                      Or(SYNCHRONIZE,
                         16_3FF)); (* This changes to FFFF depending on #defines *)

  TLS_MINIMUM_AVAILABLE = 64;
  THREAD_BASE_PRIORITY_LOWRT = 15; (* value that gets a thread to
                                      LowRealtime-1 *)
  THREAD_BASE_PRIORITY_MAX  = 2; (* maximum thread base priority boost *)
  THREAD_BASE_PRIORITY_MIN  = -2; (* minimum thread base priority boost *)
  THREAD_BASE_PRIORITY_IDLE = -15; (* value that gets a thread to idle *)
  EVENT_MODIFY_STATE        = 16_0002;
  EVENT_ALL_ACCESS = Or(STANDARD_RIGHTS_REQUIRED,
                     Or(SYNCHRONIZE,
                        16_3));
  MUTANT_QUERY_STATE = 16_0001;

  MUTANT_ALL_ACCESS = Or(STANDARD_RIGHTS_REQUIRED,
                      Or(SYNCHRONIZE,
                         MUTANT_QUERY_STATE));
  SEMAPHORE_MODIFY_STATE = 16_0002;
  SEMAPHORE_ALL_ACCESS = Or(STANDARD_RIGHTS_REQUIRED,
                         Or(SYNCHRONIZE,
                            16_3));
  TIME_ZONE_ID_UNKNOWN  = 0;
  TIME_ZONE_ID_STANDARD = 1;
  TIME_ZONE_ID_DAYLIGHT = 2;

  PROCESSOR_INTEL_386     = 386;
  PROCESSOR_INTEL_486     = 486;
  PROCESSOR_INTEL_PENTIUM = 586;
  PROCESSOR_INTEL_IA64    = 2200;
  PROCESSOR_AMD_X8664     = 8664;
  PROCESSOR_MIPS_R4000    = 4000; (* incl R4101 & R3910 for Windows CE *)
  PROCESSOR_ALPHA_21064   = 21064;
  PROCESSOR_PPC_601       = 601;
  PROCESSOR_PPC_603       = 603;
  PROCESSOR_PPC_604       = 604;
  PROCESSOR_PPC_620       = 620;
  PROCESSOR_HITACHI_SH3   = 10003;  (* Windows CE *)
  PROCESSOR_HITACHI_SH3E  = 10004;  (* Windows CE *)
  PROCESSOR_HITACHI_SH4   = 10005;  (* Windows CE *)
  PROCESSOR_MOTOROLA_821  = 821;    (* Windows CE *)
  PROCESSOR_SHx_SH3       = 103;    (* Windows CE *)
  PROCESSOR_SHx_SH4       = 104;    (* Windows CE *)
  PROCESSOR_STRONGARM     = 2577;   (* Windows CE - 0xA11 *)
  PROCESSOR_ARM720        = 1824;   (* Windows CE - 0x720 *)
  PROCESSOR_ARM820        = 2080;   (* Windows CE - 0x820 *)
  PROCESSOR_ARM920        = 2336;   (* Windows CE - 0x920 *)
  PROCESSOR_ARM_7TDMI     = 70001;  (* Windows CE *)
  PROCESSOR_OPTIL         = 16_494f; (* MSIL *)

  PROCESSOR_ARCHITECTURE_INTEL   = 0;
  PROCESSOR_ARCHITECTURE_MIPS    = 1;
  PROCESSOR_ARCHITECTURE_ALPHA   = 2;
  PROCESSOR_ARCHITECTURE_PPC     = 3;
  PROCESSOR_ARCHITECTURE_SHX     = 4;
  PROCESSOR_ARCHITECTURE_ARM     = 5;
  PROCESSOR_ARCHITECTURE_IA64    = 6;
  PROCESSOR_ARCHITECTURE_ALPHA64 = 7;
  PROCESSOR_ARCHITECTURE_MSIL    = 8;
  PROCESSOR_ARCHITECTURE_AMD64   = 9;
  PROCESSOR_ARCHITECTURE_IA32_ON_WIN64 = 10;
  PROCESSOR_ARCHITECTURE_UNKNOWN = 16_FFFF;

TYPE
  PMEMORY_BASIC_INFORMATION = UNTRACED REF MEMORY_BASIC_INFORMATION;
  MEMORY_BASIC_INFORMATION = RECORD
    BaseAddress      : PVOID;
    AllocationBase   : PVOID;
    AllocationProtect: UINT32;
    RegionSize       : SIZE_T;
    State            : UINT32;
    Protect          : UINT32;
    Type             : UINT32;
  END;

CONST
  SECTION_QUERY       = 16_0001;
  SECTION_MAP_WRITE   = 16_0002;
  SECTION_MAP_READ    = 16_0004;
  SECTION_MAP_EXECUTE = 16_0008;
  SECTION_EXTEND_SIZE = 16_0010;
  SECTION_MAP_EXECUTE_EXPLICIT = 16_0020; (* not included in SECTION_ALL_ACCESS *)

  SECTION_ALL_ACCESS = Or(STANDARD_RIGHTS_REQUIRED,
                       Or(SECTION_QUERY,
                       Or(SECTION_MAP_WRITE,
                       Or(SECTION_MAP_READ,
                       Or(SECTION_MAP_EXECUTE, SECTION_EXTEND_SIZE)))));

  SESSION_QUERY_ACCESS = 16_0001;
  SESSION_MODIFY_ACCESS = 16_0002;

  SESSION_ALL_ACCESS = Or(STANDARD_RIGHTS_REQUIRED,
                       Or(SESSION_QUERY_ACCESS, SESSION_MODIFY_ACCESS));

  PAGE_NOACCESS                 = 16_01;
  PAGE_READONLY                 = 16_02;
  PAGE_READWRITE                = 16_04;
  PAGE_WRITECOPY                = 16_08;
  PAGE_EXECUTE                  = 16_10;
  PAGE_EXECUTE_READ             = 16_20;
  PAGE_EXECUTE_READWRITE        = 16_40;
  PAGE_EXECUTE_WRITECOPY        = 16_80;
  PAGE_GUARD                   = 16_100;
  PAGE_NOCACHE                 = 16_200;
  PAGE_WRITECOMBINE            = 16_400;
  MEM_COMMIT                    = 16_1000;
  MEM_RESERVE                   = 16_2000;
  MEM_DECOMMIT                  = 16_4000;
  MEM_RELEASE                   = 16_8000;
  MEM_FREE                      = 16_10000;
  MEM_PRIVATE       =    16_20000;
  MEM_MAPPED        =    16_40000;
  MEM_RESET         =    16_80000;
  MEM_TOP_DOWN      =   16_100000;
  MEM_WRITE_WATCH   =   16_200000;
  MEM_PHYSICAL      =   16_400000;
  MEM_ROTATE        =   16_800000;
  MEM_LARGE_PAGES   = 16_20000000;
  MEM_4MB_PAGES     = 16_80000000;

  FILE_SHARE_READ   = 16_00000001;
  FILE_SHARE_WRITE  = 16_00000002;
  FILE_SHARE_DELETE = 16_00000004;

  FILE_ATTRIBUTE_READONLY            = 16_00000001;
  FILE_ATTRIBUTE_HIDDEN              = 16_00000002;
  FILE_ATTRIBUTE_SYSTEM              = 16_00000004;
  FILE_ATTRIBUTE_DIRECTORY           = 16_00000010;
  FILE_ATTRIBUTE_ARCHIVE             = 16_00000020;
  FILE_ATTRIBUTE_DEVICE              = 16_00000040;
  FILE_ATTRIBUTE_NORMAL              = 16_00000080;
  FILE_ATTRIBUTE_TEMPORARY           = 16_00000100;
  (* FILE_ATTRIBUTE_ATOMIC_WRITE     = 16_00000200; *)
  (* FILE_ATTRIBUTE_XACTION_WRITE    = 16_00000400; *)
  FILE_ATTRIBUTE_SPARSE_FILE         = 16_00000200;
  FILE_ATTRIBUTE_REPARSE_POINT       = 16_00000400;
  FILE_ATTRIBUTE_COMPRESSED          = 16_00000800;
  FILE_ATTRIBUTE_OFFLINE             = 16_00001000;
  FILE_ATTRIBUTE_NOT_CONTENT_INDEXED = 16_00002000;
  FILE_ATTRIBUTE_ENCRYPTED           = 16_00004000;
  FILE_ATTRIBUTE_VIRTUAL             = 16_00010000;

  FILE_NOTIFY_CHANGE_FILE_NAME   = 16_00000001;
  FILE_NOTIFY_CHANGE_DIR_NAME    = 16_00000002;
  FILE_NOTIFY_CHANGE_ATTRIBUTES  = 16_00000004;
  FILE_NOTIFY_CHANGE_SIZE        = 16_00000008;
  FILE_NOTIFY_CHANGE_LAST_WRITE  = 16_00000010;
  FILE_NOTIFY_CHANGE_LAST_ACCESS = 16_00000020;
  FILE_NOTIFY_CHANGE_CREATION    = 16_00000040;
  FILE_NOTIFY_CHANGE_SECURITY    = 16_00000100;

  FILE_CASE_SENSITIVE_SEARCH    = 16_00000001;
  FILE_CASE_PRESERVED_NAMES     = 16_00000002;
  FILE_UNICODE_ON_DISK          = 16_00000004;
  FILE_PERSISTENT_ACLS          = 16_00000008;
  FILE_FILE_COMPRESSION         = 16_00000010;
  FILE_VOLUME_QUOTAS            = 16_00000020;
  FILE_SUPPORTS_SPARSE_FILES    = 16_00000040;
  FILE_SUPPORTS_REPARSE_POINTS  = 16_00000080;
  FILE_SUPPORTS_REMOTE_STORAGE  = 16_00000100;
  FILE_VOLUME_IS_COMPRESSED     = 16_00008000;
  FILE_SUPPORTS_OBJECT_IDS      = 16_00010000;
  FILE_SUPPORTS_ENCRYPTION      = 16_00020000;
  FILE_NAMED_STREAMS            = 16_00040000;
  FILE_READ_ONLY_VOLUME         = 16_00080000;
  FILE_SEQUENTIAL_WRITE_ONCE    = 16_00100000;
  FILE_SUPPORTS_TRANSACTIONS    = 16_00200000;

  DUPLICATE_CLOSE_SOURCE        = 16_00000001;
  DUPLICATE_SAME_ACCESS         = 16_00000002;

TYPE 
 PACCESS_TOKEN = PVOID;            
 PSECURITY_DESCRIPTOR = PVOID;     
 PSID = PVOID; 

(*////////////////////////////////////////////////////////////////////// *)
(*                                                                    // *)
(*                             ACCESS MASK                            // *)
(*                                                                    // *)
(*////////////////////////////////////////////////////////////////////// *)

(*  Define the access mask as a longword sized structure divided up as *)
(*  follows: *)
(*       3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 *)
(*       1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 *)
(*      +---------------+---------------+-------------------------------+ *)
(*      |G|G|G|G|Res'd|A| StandardRights|         SpecificRights        | *)
(*      |R|W|E|A|     |S|               |                               | *)
(*      +-+-------------+---------------+-------------------------------+ *)
(*      typedef struct _ACCESS_MASK { *)
(*          UINT16   SpecificRights; *)
(*          UINT8  StandardRights; *)
(*          UINT8  AccessSystemAcl : 1; *)
(*          UINT8  Reserved : 3; *)
(*          UINT8  GenericAll : 1; *)
(*          UINT8  GenericExecute : 1; *)
(*          UINT8  GenericWrite : 1; *)
(*          UINT8  GenericRead : 1; *)
(*      } ACCESS_MASK; *)
(*      typedef ACCESS_MASK *PACCESS_MASK; *)
(*  but to make life simple for programmer's we'll allow them to specify *)
(*  a desired access mask by simply OR'ing together mulitple single rights *)
(*  and treat an access mask as a ulong.  For example *)
(*      DesiredAccess = DELETE | READ_CONTROL *)
(*  So we'll declare ACCESS_MASK as UINT32 *)

TYPE
 ACCESS_MASK = UINT32;
 PACCESS_MASK = UNTRACED REF ACCESS_MASK;

(*////////////////////////////////////////////////////////////////////// *)
(*                                                                    // *)
(*                             ACCESS TYPES                           // *)
(*                                                                    // *)
(*////////////////////////////////////////////////////////////////////// *)

(*
 *  The following are masks for the predefined standard access types
 *)

CONST
  DELETE       = 16_00010000;
  READ_CONTROL = 16_00020000;
  WRITE_DAC    = 16_00040000;
  WRITE_OWNER  = 16_00080000;
  SYNCHRONIZE  = 16_00100000;

  STANDARD_RIGHTS_REQUIRED = 16_000F0000;

  STANDARD_RIGHTS_READ    = READ_CONTROL;
  STANDARD_RIGHTS_WRITE   = READ_CONTROL;
  STANDARD_RIGHTS_EXECUTE = READ_CONTROL;

  STANDARD_RIGHTS_ALL = 16_001F0000;

  SPECIFIC_RIGHTS_ALL = 16_0000FFFF;

(*
 * AccessSystemAcl access type
 *)
  ACCESS_SYSTEM_SECURITY = 16_01000000;

(*
 * MaximumAllowed access type
 *)
  MAXIMUM_ALLOWED  = 16_02000000;

(*
 *  These are the generic rights.
 *)
  GENERIC_READ    = 16_80000000;
  GENERIC_WRITE   = 16_40000000;
  GENERIC_EXECUTE = 16_20000000;
  GENERIC_ALL     = 16_10000000;


(*
 *  Define the generic mapping array.  This is used to denote the
 *  mapping of each generic access right to a specific access mask.
 *)

TYPE
  PGENERIC_MAPPING = UNTRACED REF GENERIC_MAPPING;
  GENERIC_MAPPING = RECORD
    GenericRead   : ACCESS_MASK;
    GenericWrite  : ACCESS_MASK;
    GenericExecute: ACCESS_MASK;
    GenericAll    : ACCESS_MASK;
  END;


(*////////////////////////////////////////////////////////////////////// *)
(*                                                                    // *)
(*                        LUID_AND_ATTRIBUTES                         // *)
(*                                                                    // *)
(*////////////////////////////////////////////////////////////////////// *)

  PLUID_AND_ATTRIBUTES = UNTRACED REF LUID_AND_ATTRIBUTES;
  LUID_AND_ATTRIBUTES = RECORD
    Luid      : LUID;
    Attributes: UINT32;
  END;

 LUID_AND_ATTRIBUTES_ARRAY = ARRAY ANYSIZE_ARRAY OF LUID_AND_ATTRIBUTES;
 PLUID_AND_ATTRIBUTES_ARRAY = UNTRACED REF LUID_AND_ATTRIBUTES_ARRAY;

(*////////////////////////////////////////////////////////////////////// *)
(*                                                                    // *)
(*              Security Id     (SID)                                 // *)
(*                                                                    // *)
(*////////////////////////////////////////////////////////////////////// *)
(* Pictorially the structure of an SID is as follows: *)
(*         1   1   1   1   1   1 *)
(*         5   4   3   2   1   0   9   8   7   6   5   4   3   2   1   0 *)
(*      +---------------------------------------------------------------+ *)
(*      |      SubAuthorityCount        |Reserved1 (SBZ)|   Revision    | *)
(*      +---------------------------------------------------------------+ *)
(*      |                   IdentifierAuthority[0]                      | *)
(*      +---------------------------------------------------------------+ *)
(*      |                   IdentifierAuthority[1]                      | *)
(*      +---------------------------------------------------------------+ *)
(*      |                   IdentifierAuthority[2]                      | *)
(*      +---------------------------------------------------------------+ *)
(*      |                                                               | *)
(*      +- -  -  -  -  -  -  -  SubAuthority[]  -  -  -  -  -  -  -  - -+ *)
(*      |                                                               | *)
(*      +---------------------------------------------------------------+ *)

  PSID_IDENTIFIER_AUTHORITY = UNTRACED REF SID_IDENTIFIER_AUTHORITY;
  SID_IDENTIFIER_AUTHORITY = RECORD
    Value: ARRAY [0 .. 5] OF UINT8;
  END;

  PISID = UNTRACED REF SID;
  SID = RECORD
   Revision           : UINT8;
   SubAuthorityCount  : UINT8;
   IdentifierAuthority: SID_IDENTIFIER_AUTHORITY;
   SubAuthority       : ARRAY ANYSIZE_ARRAY OF UINT32;
  END;


CONST
 SID_REVISION                    = 1; (* Current revision level *)
 SID_MAX_SUB_AUTHORITIES         = 15;
 SID_RECOMMENDED_SUB_AUTHORITIES = 1; (* Will change to around 6 in a future
                                         release. *)

TYPE
  SID_NAME_USE = [1 .. 8];
  PSID_NAME_USE = UNTRACED REF SID_NAME_USE;

CONST
  SidTypeUser          : SID_NAME_USE = 1;
  SidTypeGroup         : SID_NAME_USE = 2;
  SidTypeDomain        : SID_NAME_USE = 3;
  SidTypeAlias         : SID_NAME_USE = 4;
  SidTypeWellKnownGroup: SID_NAME_USE = 5;
  SidTypeDeletedAccount: SID_NAME_USE = 6;
  SidTypeInvalid       : SID_NAME_USE = 7;
  SidTypeUnknown       : SID_NAME_USE = 8;

TYPE
  PSID_AND_ATTRIBUTES = UNTRACED REF SID_AND_ATTRIBUTES;
  SID_AND_ATTRIBUTES = RECORD
    Sid: PSID;
    Attributes: UINT32;
  END;

  SID_AND_ATTRIBUTES_ARRAY = ARRAY ANYSIZE_ARRAY OF SID_AND_ATTRIBUTES;
  PSID_AND_ATTRIBUTES_ARRAY = UNTRACED REF SID_AND_ATTRIBUTES_ARRAY;


(*//////////////////////////////////////////////// *)
(*                                              // *)
(* Universal well-known SIDs                    // *)
(*                                              // *)
(*     Null SID              S-1-0-0            // *)
(*     World                 S-1-1-0            // *)
(*     Local                 S-1-2-0            // *)
(*     Creator Owner ID      S-1-3-0            // *)
(*     Creator Group ID      S-1-3-1            // *)
(*                                              // *)
(*//////////////////////////////////////////////// *)

CONST (* ??? *)
  SECURITY_NULL_SID_AUTHORITY    = SID_IDENTIFIER_AUTHORITY{
                                     ARRAY [0 .. 5] OF UINT8{0, 0, 0, 0, 0, 0}};
  SECURITY_WORLD_SID_AUTHORITY   = SID_IDENTIFIER_AUTHORITY{
                                     ARRAY [0 .. 5] OF UINT8{0, 0, 0, 0, 0, 1}};
  SECURITY_LOCAL_SID_AUTHORITY   = SID_IDENTIFIER_AUTHORITY{
                                     ARRAY [0 .. 5] OF UINT8{0, 0, 0, 0, 0, 2}};
  SECURITY_CREATOR_SID_AUTHORITY = SID_IDENTIFIER_AUTHORITY{
                                     ARRAY [0 .. 5] OF UINT8{0, 0, 0, 0, 0, 3}};

  SECURITY_NULL_RID : INT32 = 16_00000000;
  SECURITY_WORLD_RID: INT32 = 16_00000000;
  SECURITY_LOCAL_RID: INT32 = 16_00000000;

  SECURITY_CREATOR_OWNER_RID: INT32 = 16_00000000;
  SECURITY_CREATOR_GROUP_RID: INT32 = 16_00000001;



(*//////////////////////////////////////////////////////////*)
(*                                                          *)
(* NT well-known SIDs                                       *)
(*                                                          *)
(*     NT Authority          S-1-5                          *)
(*     Dialup                S-1-5-1                        *)
(*                                                          *)
(*     Network               S-1-5-2                        *)
(*     Batch                 S-1-5-3                        *)
(*     Interactive           S-1-5-4                        *)
(*     Service               S-1-5-6                        *)
(*                                                          *)
(*     (Logon IDs)           S-1-5-5-X-Y                    *)
(*                                                          *)
(*     (Built-in domain)     s-1-5-20                       *)
(*                                                          *)
(*//////////////////////////////////////////////////////////*)

  SECURITY_NT_AUTHORITY = SID_IDENTIFIER_AUTHORITY{
                            ARRAY [0 .. 5] OF UINT8{0, 0, 0, 0, 0, 5}};

  SECURITY_DIALUP_RID     : INT32 = 16_00000001;
  SECURITY_NETWORK_RID    : INT32 = 16_00000002;
  SECURITY_BATCH_RID      : INT32 = 16_00000003;
  SECURITY_INTERACTIVE_RID: INT32 = 16_00000004;
  SECURITY_SERVICE_RID    : INT32 = 16_00000006;

  SECURITY_LOGON_IDS_RID      : INT32 = 16_00000005;
  SECURITY_LOGON_IDS_RID_COUNT: INT32 = 3;

  SECURITY_LOCAL_SYSTEM_RID: INT32 = 16_00000012;

  SECURITY_BUILTIN_DOMAIN_RID: INT32 = 16_00000020;

(*//////////////////////////////////////////////////////////*)
(*                                                          *)
(* well-known domain relative sub-authority values (RIDs)...*)
(*                                                          *)
(*//////////////////////////////////////////////////////////*)

(* Well-known users ... *)

  DOMAIN_USER_RID_ADMIN: INT32 = 16_000001F4;
  DOMAIN_USER_RID_GUEST: INT32 = 16_000001F5;

(* well-known groups ... *)

  DOMAIN_GROUP_RID_ADMINS: INT32 = 16_00000200;
  DOMAIN_GROUP_RID_USERS : INT32 = 16_00000201;

(* well-known aliases ... *)

  DOMAIN_ALIAS_RID_ADMINS     : INT32 = 16_00000220;
  DOMAIN_ALIAS_RID_USERS      : INT32 = 16_00000221;
  DOMAIN_ALIAS_RID_GUESTS     : INT32 = 16_00000222;
  DOMAIN_ALIAS_RID_POWER_USERS: INT32 = 16_00000223;

  DOMAIN_ALIAS_RID_ACCOUNT_OPS: INT32 = 16_00000224;
  DOMAIN_ALIAS_RID_SYSTEM_OPS : INT32 = 16_00000225;
  DOMAIN_ALIAS_RID_PRINT_OPS  : INT32 = 16_00000226;
  DOMAIN_ALIAS_RID_BACKUP_OPS : INT32 = 16_00000227;

  DOMAIN_ALIAS_RID_REPLICATOR: INT32 = 16_00000228;

(*
 * Allocate the System Luid.  The first 1000 LUIDs are reserved.
 * Use #999 here (16_3E7 = 999)
 *)

  SYSTEM_LUID = LUID{16_0, 16_3E7};

(*////////////////////////////////////////////////////////////////////// *)
(*                                                                    // *)
(*                          User and Group related SID attributes     // *)
(*                                                                    // *)
(*////////////////////////////////////////////////////////////////////// *)

(* Group attributes *)

SE_GROUP_MANDATORY:UINT32=              16_00000001;
SE_GROUP_ENABLED_BY_DEFAULT:UINT32=     16_00000002;
SE_GROUP_ENABLED:UINT32=                16_00000004;
SE_GROUP_OWNER:UINT32=                  16_00000008;
SE_GROUP_LOGON_ID:UINT32=               16_C0000000;


(* User attributes *)

(* (None yet defined.) *)

(*////////////////////////////////////////////////////////////////////// *)
(*                                                                    // *)
(*                         ACL  and  ACE                              // *)
(*                                                                    // *)
(*////////////////////////////////////////////////////////////////////// *)

(*  Define an ACL and the ACE format.  The structure of an ACL header *)
(*  followed by one or more ACEs.  Pictorally the structure of an ACL header *)
(*  is as follows: *)
(*       3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 *)
(*       1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 *)
(*      +-------------------------------+---------------+---------------+ *)
(*      |            AclSize            |      Sbz1     |  AclRevision  | *)
(*      +-------------------------------+---------------+---------------+ *)
(*      |              Sbz2             |           AceCount            | *)
(*      +-------------------------------+-------------------------------+ *)
(*  The current AclRevision is defined to be ACL_REVISION. *)
(*  AclSize is the size, in bytes, allocated for the ACL.  This includes *)
(*  the ACL header, ACES, and remaining free space in the buffer. *)
(*  AceCount is the number of ACES in the ACL. *)

(* This is the *current* ACL revision *)

  ACL_REVISION = 2;

(* This is the history of ACL revisions.  Add a new one whenever *)
(* ACL_REVISION is updated *)

  ACL_REVISION1 = 1;
  ACL_REVISION2 = 2;

TYPE
  PACL = UNTRACED REF ACL;
  ACL = RECORD
    AclRevision: UINT8;
    Sbz1       : UINT8;
    AclSize    : UINT16;
    AceCount   : UINT16;
    Sbz2       : UINT16;
  END;

(*  The structure of an ACE is a common ace header followed by ace type *)
(*  specific data.  Pictorally the structure of the common ace header is *)
(*  as follows: *)
(*       3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 *)
(*       1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 *)
(*      +---------------+-------+-------+---------------+---------------+ *)
(*      |            AceSize            |    AceFlags   |     AceType   | *)
(*      +---------------+-------+-------+---------------+---------------+ *)
(*  AceType denotes the type of the ace, there are some predefined ace *)
(*  types *)
(*  AceSize is the size, in bytes, of ace. *)
(*  AceFlags are the Ace flags for audit and inheritance, defined shortly. *)

  PACE_HEADER = UNTRACED REF ACE_HEADER;
  ACE_HEADER = RECORD
    AceType : UINT8;
    AceFlags: UINT8;
    AceSize : UINT16;
  END;

(*  The following are the predefined ace types that go into the AceType *)
(*  field of an Ace header. *)

CONST
  ACCESS_ALLOWED_ACE_TYPE = 16_0;
  ACCESS_DENIED_ACE_TYPE  = 16_1;
  SYSTEM_AUDIT_ACE_TYPE   = 16_2;
  SYSTEM_ALARM_ACE_TYPE   = 16_3;

(*  The following are the inherit flags that go into the AceFlags field *)
(*  of an Ace header. *)

  OBJECT_INHERIT_ACE       = 16_1;
  CONTAINER_INHERIT_ACE    = 16_2;
  NO_PROPAGATE_INHERIT_ACE = 16_4;
  INHERIT_ONLY_ACE         = 16_8;
  VALID_INHERIT_FLAGS      = 16_F;

(*  The following are the currently defined ACE flags that go into the *)
(*  AceFlags field of an ACE header.  Each ACE type has its own set of *)
(*  AceFlags. *)
(*  SUCCESSFUL_ACCESS_ACE_FLAG - used only with system audit and alarm ACE *)
(*  types to indicate that a message is generated for successful accesses. *)
(*  FAILED_ACCESS_ACE_FLAG - used only with system audit and alarm ACE types *)
(*  to indicate that a message is generated for failed accesses. *)

(*  SYSTEM_AUDIT and SYSTEM_ALARM AceFlags *)
(*  These control the signaling of audit and alarms for success or failure. *)

  SUCCESSFUL_ACCESS_ACE_FLAG = 16_40;
  FAILED_ACCESS_ACE_FLAG     = 16_80;

(*  We'll define the structure of the predefined ACE types.  Pictorally *)
(*  the structure of the predefined ACE's is as follows: *)
(*       3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 *)
(*       1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 *)
(*      +---------------+-------+-------+---------------+---------------+ *)
(*      |    AceFlags   | Resd  |Inherit|    AceSize    |     AceType   | *)
(*      +---------------+-------+-------+---------------+---------------+ *)
(*      |                              Mask                             | *)
(*      +---------------------------------------------------------------+ *)
(*      |                                                               | *)
(*      +                                                               + *)
(*      |                                                               | *)
(*      +                              Sid                              + *)
(*      |                                                               | *)
(*      +                                                               + *)
(*      |                                                               | *)
(*      +---------------------------------------------------------------+ *)
(*  Mask is the access mask associated with the ACE.  This is either the *)
(*  access allowed, access denied, audit, or alarm mask. *)
(*  Sid is the Sid associated with the ACE. *)

(*  The following are the four predefined ACE types. *)

(*  Examine the AceType field in the Header to determine *)
(*  which structure is appropriate to use for casting. *)

TYPE
  PACCESS_ALLOWED_ACE = UNTRACED REF ACCESS_ALLOWED_ACE;
  ACCESS_ALLOWED_ACE = RECORD
    Header  : ACE_HEADER;
    Mask    : ACCESS_MASK;
    SidStart: UINT32;
  END;

  PACCESS_DENIED_ACE = UNTRACED REF ACCESS_DENIED_ACE;
  ACCESS_DENIED_ACE = RECORD
    Header  : ACE_HEADER;
    Mask    : ACCESS_MASK;
    SidStart: UINT32;
  END;

  PSYSTEM_AUDIT_ACE = UNTRACED REF SYSTEM_AUDIT_ACE;
  SYSTEM_AUDIT_ACE = RECORD
    Header  : ACE_HEADER;
    Mask    : ACCESS_MASK;
    SidStart: UINT32;
  END;

  PSYSTEM_ALARM_ACE = UNTRACED REF SYSTEM_ALARM_ACE;
  SYSTEM_ALARM_ACE = RECORD
    Header  : ACE_HEADER;
    Mask    : ACCESS_MASK;
    SidStart: UINT32;
  END;


(*  The following declarations are used for setting and querying information *)
(*  about and ACL.  First are the various information classes available to *)
(*  the user. *)

  ACL_INFORMATION_CLASS = [1 .. 2];

CONST
  AclRevisionInformation: ACL_INFORMATION_CLASS = 1;
  AclSizeInformation    : ACL_INFORMATION_CLASS = 2;

(*  This record is returned/sent if the user is requesting/setting the *)
(*  AclRevisionInformation *)

TYPE
  ACL_REVISION_INFORMATION = RECORD AclRevision: UINT32;  END;
  PACL_REVISION_INFORMATION = UNTRACED REF ACL_REVISION_INFORMATION;

(*  This record is returned if the user is requesting AclSizeInformation *)

  PACL_SIZE_INFORMATION = UNTRACED REF ACL_SIZE_INFORMATION;
  ACL_SIZE_INFORMATION = RECORD
    AceCount     : UINT32;
    AclBytesInUse: UINT32;
    AclBytesFree : UINT32;
  END;


(*////////////////////////////////////////////////////////////////////// *)
(*                                                                    // *)
(*                             SECURITY_DESCRIPTOR                    // *)
(*                                                                    // *)
(*////////////////////////////////////////////////////////////////////// *)
(*  Define the Security Descriptor and related data types. *)
(*  This is an opaque data structure. *)

(* Current security descriptor revision value *)

CONST
  SECURITY_DESCRIPTOR_REVISION  = 1;
  SECURITY_DESCRIPTOR_REVISION1 = 1;

(* Minimum length, in bytes, needed to build a security descriptor *)
(* (NOTE: This must manually be kept consistent with the) *)
(* (sizeof(SECURITY_DESCRIPTOR)                         ) *)

CONST SECURITY_DESCRIPTOR_MIN_LENGTH = 20;

TYPE
  SECURITY_DESCRIPTOR_CONTROL = UINT16;
  PSECURITY_DESCRIPTOR_CONTROL = UNTRACED REF UINT16;

CONST
  SE_OWNER_DEFAULTED = 16_0001;
  SE_GROUP_DEFAULTED = 16_0002;
  SE_DACL_PRESENT    = 16_0004;
  SE_DACL_DEFAULTED  = 16_0008;
  SE_SACL_PRESENT    = 16_0010;
  SE_SACL_DEFAULTED  = 16_0020;
  SE_SELF_RELATIVE   = 16_8000;

(*  Where: *)
(*      SE_OWNER_DEFAULTED - This boolean flag, when set, indicates that the *)
(*          SID pointed to by the Owner field was provided by a *)
(*          defaulting mechanism rather than explicitly provided by the *)
(*          original provider of the security descriptor.  This may *)
(*          affect the treatment of the SID with respect to inheritence *)
(*          of an owner. *)
(*      SE_GROUP_DEFAULTED - This boolean flag, when set, indicates that the *)
(*          SID in the Group field was provided by a defaulting mechanism *)
(*          rather than explicitly provided by the original provider of *)
(*          the security descriptor.  This may affect the treatment of *)
(*          the SID with respect to inheritence of a primary group. *)
(*      SE_DACL_PRESENT - This boolean flag, when set, indicates that the *)
(*          security descriptor contains a discretionary ACL.  If this *)
(*          flag is set and the Dacl field of the SECURITY_DESCRIPTOR is *)
(*          null, then a null ACL is explicitly being specified. *)
(*      SE_DACL_DEFAULTED - This boolean flag, when set, indicates that the *)
(*          ACL pointed to by the Dacl field was provided by a defaulting *)
(*          mechanism rather than explicitly provided by the original *)
(*          provider of the security descriptor.  This may affect the *)
(*          treatment of the ACL with respect to inheritence of an ACL. *)
(*          This flag is ignored if the DaclPresent flag is not set. *)
(*      SE_SACL_PRESENT - This boolean flag, when set,  indicates that the *)
(*          security descriptor contains a system ACL pointed to by the *)
(*          Sacl field.  If this flag is set and the Sacl field of the *)
(*          SECURITY_DESCRIPTOR is null, then an empty (but present) *)
(*          ACL is being specified. *)
(*      SE_SACL_DEFAULTED - This boolean flag, when set, indicates that the *)
(*          ACL pointed to by the Sacl field was provided by a defaulting *)
(*          mechanism rather than explicitly provided by the original *)
(*          provider of the security descriptor.  This may affect the *)
(*          treatment of the ACL with respect to inheritence of an ACL. *)
(*          This flag is ignored if the SaclPresent flag is not set. *)
(*      SE_SELF_RELATIVE - This boolean flag, when set, indicates that the *)
(*          security descriptor is in self-relative form.  In this form, *)
(*          all fields of the security descriptor are contiguous in memory *)
(*          and all pointer fields are expressed as offsets from the *)
(*          beginning of the security descriptor.  This form is useful *)
(*          for treating security descriptors as opaque data structures *)
(*          for transmission in communication protocol or for storage on *)
(*          secondary media. *)
(* Pictorially the structure of a security descriptor is as follows: *)
(*       3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 *)
(*       1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 *)
(*      +---------------------------------------------------------------+ *)
(*      |            Control            |Reserved1 (SBZ)|   Revision    | *)
(*      +---------------------------------------------------------------+ *)
(*      |                            Owner                              | *)
(*      +---------------------------------------------------------------+ *)
(*      |                            Group                              | *)
(*      +---------------------------------------------------------------+ *)
(*      |                            Sacl                               | *)
(*      +---------------------------------------------------------------+ *)
(*      |                            Dacl                               | *)
(*      +---------------------------------------------------------------+ *)
(* In general, this data structure should be treated opaquely to ensure future *)
(* compatibility. *)

TYPE
  PISECURITY_DESCRIPTOR = UNTRACED REF SECURITY_DESCRIPTOR;
  SECURITY_DESCRIPTOR = RECORD
    Revision: UINT8;
    Sbz1    : UINT8;
    Control : SECURITY_DESCRIPTOR_CONTROL;
    Owner   : PSID;
    Group   : PSID;
    Sacl    : PACL;
    Dacl    : PACL;
  END;

(* Where: *)
(*     Revision - Contains the revision level of the security *)
(*         descriptor.  This allows this structure to be passed between *)
(*         systems or stored on disk even though it is expected to *)
(*         change in the future. *)
(*     Control - A set of flags which qualify the meaning of the *)
(*         security descriptor or individual fields of the security *)
(*         descriptor. *)
(*     Owner - is a pointer to an SID representing an object's owner. *)
(*         If this field is null, then no owner SID is present in the *)
(*         security descriptor.  If the security descriptor is in *)
(*         self-relative form, then this field contains an offset to *)
(*         the SID, rather than a pointer. *)
(*     Group - is a pointer to an SID representing an object's primary *)
(*         group.  If this field is null, then no primary group SID is *)
(*         present in the security descriptor.  If the security descriptor *)
(*         is in self-relative form, then this field contains an offset to *)
(*         the SID, rather than a pointer. *)
(*     Sacl - is a pointer to a system ACL.  This field value is only *)
(*         valid if the DaclPresent control flag is set.  If the *)
(*         SaclPresent flag is set and this field is null, then a null *)
(*         ACL  is specified.  If the security descriptor is in *)
(*         self-relative form, then this field contains an offset to *)
(*         the ACL, rather than a pointer. *)
(*     Dacl - is a pointer to a discretionary ACL.  This field value is *)
(*         only valid if the DaclPresent control flag is set.  If the *)
(*         DaclPresent flag is set and this field is null, then a null *)
(*         ACL (unconditionally granting access) is specified.  If the *)
(*         security descriptor is in self-relative form, then this field *)
(*         contains an offset to the ACL, rather than a pointer. *)



(*////////////////////////////////////////////////////////////////////// *)
(*                                                                    // *)
(*               Privilege Related Data Structures                    // *)
(*                                                                    // *)
(*////////////////////////////////////////////////////////////////////// *)


(* Privilege attributes *)

CONST
  SE_PRIVILEGE_ENABLED_BY_DEFAULT: UINT32 = 16_00000001;
  SE_PRIVILEGE_ENABLED           : UINT32 = 16_00000002;
  SE_PRIVILEGE_USED_FOR_ACCESS   : UINT32 = 16_80000000;



(* Privilege Set Control flags *)

CONST PRIVILEGE_SET_ALL_NECESSARY = 1;

(*  Privilege Set - This is defined for a privilege set of one. *)
(*                  If more than one privilege is needed, then this structure *)
(*                  will need to be allocated with more space. *)
(*  Note: don't change this structure without fixing the INITIAL_PRIVILEGE_SET *)
(*  structure (defined in se.h) *)

TYPE
  PPRIVILEGE_SET = UNTRACED REF PRIVILEGE_SET;
  PRIVILEGE_SET = RECORD
    PrivilegeCount: UINT32;
    Control       : UINT32;
    Privilege: ARRAY ANYSIZE_ARRAY OF LUID_AND_ATTRIBUTES;
  END;

(*////////////////////////////////////////////////////////////////////// *)
(*                                                                    // *)
(*               NT Defined Privileges                                // *)
(*                                                                    // *)
(*////////////////////////////////////////////////////////////////////// *)

CONST
  SE_CREATE_TOKEN_NAME        = "SeCreateTokenPrivilege";
  SE_ASSIGNPRIMARYTOKEN_NAME  = "SeAssignPrimaryTokenPrivilege";
  SE_LOCK_MEMORY_NAME         = "SeLockMemoryPrivilege";
  SE_INCREASE_QUOTA_NAME      = "SeIncreaseQuotaPrivilege";
  SE_UNSOLICITED_INPUT_NAME   = "SeUnsolicitedInputPrivilege";
  SE_TCB_NAME                 = "SeTcbPrivilege";
  SE_SECURITY_NAME            = "SeSecurityPrivilege";
  SE_TAKE_OWNERSHIP_NAME      = "SeTakeOwnershipPrivilege";
  SE_LOAD_DRIVER_NAME         = "SeLoadDriverPrivilege";
  SE_SYSTEM_PROFILE_NAME      = "SeSystemProfilePrivilege";
  SE_SYSTEMTIME_NAME          = "SeSystemtimePrivilege";
  SE_PROF_SINGLE_PROCESS_NAME = "SeProfileSingleProcessPrivilege";
  SE_INC_BASE_PRIORITY_NAME   = "SeIncreaseBasePriorityPrivilege";
  SE_CREATE_PAGEFILE_NAME     = "SeCreatePagefilePrivilege";
  SE_CREATE_PERMANENT_NAME    = "SeCreatePermanentPrivilege";
  SE_BACKUP_NAME              = "SeBackupPrivilege";
  SE_RESTORE_NAME             = "SeRestorePrivilege";
  SE_SHUTDOWN_NAME            = "SeShutdownPrivilege";
  SE_DEBUG_NAME               = "SeDebugPrivilege";
  SE_AUDIT_NAME               = "SeAuditPrivilege";
  SE_SYSTEM_ENVIRONMENT_NAME  = "SeSystemEnvironmentPrivilege";
  SE_CHANGE_NOTIFY_NAME       = "SeChangeNotifyPrivilege";
  SE_REMOTE_SHUTDOWN_NAME     = "SeRemoteShutdownPrivilege";


(*////////////////////////////////////////////////////////////////// *)
(*                                                                // *)
(*           Security Quality Of Service                          // *)
(*                                                                // *)
(*                                                                // *)
(*////////////////////////////////////////////////////////////////// *)

(* Impersonation Level *)
(* Impersonation level is represented by a pair of bits in Windows. *)
(* If a new impersonation level is added or lowest value is changed from *)
(* 0 to something else, fix the Windows CreateFile call. *)

TYPE
  SECURITY_IMPERSONATION_LEVEL = BITS 32 FOR [0 .. 3];
  PSECURITY_IMPERSONATION_LEVEL =
    UNTRACED REF SECURITY_IMPERSONATION_LEVEL;

CONST
  SecurityAnonymous      = 0;
  SecurityIdentification = 1;
  SecurityImpersonation  = 2;
  SecurityDelegation     = 3;

  SECURITY_MAX_IMPERSONATION_LEVEL = SecurityDelegation;
  DEFAULT_IMPERSONATION_LEVEL      = SecurityImpersonation;

(* Security Tracking Mode *)

  SECURITY_DYNAMIC_TRACKING = TRUE;
  SECURITY_STATIC_TRACKING  = FALSE;

TYPE
  SECURITY_CONTEXT_TRACKING_MODE = WBOOLEAN;
  PSECURITY_CONTEXT_TRACKING_MODE =
    UNTRACED REF SECURITY_CONTEXT_TRACKING_MODE;


(* Quality Of Service *)

TYPE
  PSECURITY_QUALITY_OF_SERVICE = UNTRACED REF SECURITY_QUALITY_OF_SERVICE;
  SECURITY_QUALITY_OF_SERVICE = RECORD
    Length             : UINT32;
    ImpersonationLevel : SECURITY_IMPERSONATION_LEVEL;
    ContextTrackingMode: SECURITY_CONTEXT_TRACKING_MODE;
    EffectiveOnly      : WBOOLEAN;
  END;

(* Used to represent information related to a thread impersonation *)

  PSE_IMPERSONATION_STATE = UNTRACED REF SE_IMPERSONATION_STATE;
  SE_IMPERSONATION_STATE = RECORD
    Token        : PACCESS_TOKEN;
    CopyOnOpen   : WBOOLEAN;
    EffectiveOnly: WBOOLEAN;
    paddding : UINT16;
    Level        : SECURITY_IMPERSONATION_LEVEL;
  END;

(*////////////////////////////////////////////////////////////////// *)
(*                                                                // *)
(*           Token Object Definitions                             // *)
(*                                                                // *)
(*                                                                // *)
(*////////////////////////////////////////////////////////////////// *)


(* Token Specific Access Rights. *)

CONST
  TOKEN_ASSIGN_PRIMARY    = 16_0001;
  TOKEN_DUPLICATE         = 16_0002;
  TOKEN_IMPERSONATE       = 16_0004;
  TOKEN_QUERY             = 16_0008;
  TOKEN_QUERY_SOURCE      = 16_0010;
  TOKEN_ADJUST_PRIVILEGES = 16_0020;
  TOKEN_ADJUST_GROUPS     = 16_0040;
  TOKEN_ADJUST_DEFAULT    = 16_0080;

  TOKEN_ALL_ACCESS = Or(STANDARD_RIGHTS_REQUIRED,
                     Or(TOKEN_ASSIGN_PRIMARY,
                     Or(TOKEN_DUPLICATE,
                     Or(TOKEN_IMPERSONATE,
                     Or(TOKEN_QUERY,
                     Or(TOKEN_QUERY_SOURCE,
                     Or(TOKEN_ADJUST_PRIVILEGES,
                     Or(TOKEN_ADJUST_GROUPS,
                        TOKEN_ADJUST_DEFAULT))))))));


  TOKEN_READ = Or(STANDARD_RIGHTS_READ, TOKEN_QUERY);


  TOKEN_WRITE = Or(STANDARD_RIGHTS_WRITE,
                Or(TOKEN_ADJUST_PRIVILEGES,
                Or(TOKEN_ADJUST_GROUPS,
                   TOKEN_ADJUST_DEFAULT)));

  TOKEN_EXECUTE = Or(STANDARD_RIGHTS_EXECUTE,
                     TOKEN_IMPERSONATE);

(* Token Types *)

TYPE
  TOKEN_TYPE = BITS 32 FOR [1 .. 2];
  PTOKEN_TYPE = UNTRACED REF TOKEN_TYPE;

CONST
  TokenPrimary      : TOKEN_TYPE = 1;
  TokenImpersonation: TOKEN_TYPE = 2;


(* Token Information Classes. *)

TYPE
  TOKEN_INFORMATION_CLASS = [1 .. 10];
  PTOKEN_INFORMATION_CLASS = UNTRACED REF TOKEN_INFORMATION_CLASS;

CONST
  TokenUser              : TOKEN_INFORMATION_CLASS = 1;
  TokenGroups            : TOKEN_INFORMATION_CLASS = 2;
  TokenPrivileges        : TOKEN_INFORMATION_CLASS = 3;
  TokenOwner             : TOKEN_INFORMATION_CLASS = 4;
  TokenPrimaryGroup      : TOKEN_INFORMATION_CLASS = 5;
  TokenDefaultDacl       : TOKEN_INFORMATION_CLASS = 6;
  TokenSource            : TOKEN_INFORMATION_CLASS = 7;
  TokenType              : TOKEN_INFORMATION_CLASS = 8;
  TokenImpersonationLevel: TOKEN_INFORMATION_CLASS = 9;
  TokenStatistics        : TOKEN_INFORMATION_CLASS = 10;

(* Token information class structures *)

TYPE
  TOKEN_USER = RECORD User: SID_AND_ATTRIBUTES;  END;
  PTOKEN_USER = UNTRACED REF TOKEN_USER;


  PTOKEN_GROUPS = UNTRACED REF TOKEN_GROUPS;
  TOKEN_GROUPS = RECORD
    GroupCount: UINT32;
    Groups    : ARRAY ANYSIZE_ARRAY OF SID_AND_ATTRIBUTES;
  END;


  PTOKEN_PRIVILEGES = UNTRACED REF TOKEN_PRIVILEGES;
  TOKEN_PRIVILEGES = RECORD
    PrivilegeCount: UINT32;
    Privileges    : ARRAY ANYSIZE_ARRAY OF LUID_AND_ATTRIBUTES;
  END;


  TOKEN_OWNER = RECORD Owner: PSID;  END;
  PTOKEN_OWNER = UNTRACED REF TOKEN_OWNER;


  TOKEN_PRIMARY_GROUP = RECORD PrimaryGroup: PSID;  END;
  PTOKEN_PRIMARY_GROUP = UNTRACED REF TOKEN_PRIMARY_GROUP;


  TOKEN_DEFAULT_DACL = RECORD DefaultDacl: PACL;  END;
  PTOKEN_DEFAULT_DACL = UNTRACED REF TOKEN_DEFAULT_DACL;


CONST TOKEN_SOURCE_LENGTH = 8;

TYPE
  PTOKEN_SOURCE = UNTRACED REF TOKEN_SOURCE;
  TOKEN_SOURCE = RECORD
    SourceName      : ARRAY [0 .. TOKEN_SOURCE_LENGTH - 1] OF CHAR;
    SourceIdentifier: LUID;
  END;


  PTOKEN_STATISTICS = UNTRACED REF TOKEN_STATISTICS;
  TOKEN_STATISTICS = RECORD
    TokenId           : LUID;
    AuthenticationId  : LUID;
    ExpirationTime    : LARGE_INTEGER;
    TokenType         : TOKEN_TYPE;
    ImpersonationLevel: SECURITY_IMPERSONATION_LEVEL;
    DynamicCharged    : UINT32;
    DynamicAvailable  : UINT32;
    GroupCount        : UINT32;
    PrivilegeCount    : UINT32;
    ModifiedId        : LUID;
  END;


  PTOKEN_CONTROL = UNTRACED REF TOKEN_CONTROL;
  TOKEN_CONTROL = RECORD
    TokenId         : LUID;
    AuthenticationId: LUID;
    ModifiedId      : LUID;
    TokenSource     : TOKEN_SOURCE;
  END;


  SECURITY_INFORMATION = UINT32;
  PSECURITY_INFORMATION = UNTRACED REF UINT32;

CONST
  OWNER_SECURITY_INFORMATION: INT32 = 16_00000001;
  GROUP_SECURITY_INFORMATION: INT32 = 16_00000002;
  DACL_SECURITY_INFORMATION : INT32 = 16_00000004;
  SACL_SECURITY_INFORMATION : INT32 = 16_00000008;
  HEAP_SERIALIZE                   = 16_00000001;

TYPE
  PRTL_CRITICAL_SECTION_DEBUG = UNTRACED REF RTL_CRITICAL_SECTION_DEBUG;
  RTL_CRITICAL_SECTION_DEBUG = RECORD
  (* This is opaque. *)
    Type                 : UINT16;
    CreatorBackTraceIndex: UINT16;
    CriticalSection      : UNTRACED REF RTL_CRITICAL_SECTION;
    ProcessLocksList     : LIST_ENTRY;
    EntryCount           : UINT32;
    ContentionCount      : UINT32;
    Flags                : UINT32;
    CreatorBackTraceIndexHigh: UINT16;
    SpareWORD            : UINT16;
  END;

CONST
  RTL_CRITSECT_TYPE = 0;
  RTL_RESOURCE_TYPE = 1;

TYPE
  PRTL_CRITICAL_SECTION = UNTRACED REF RTL_CRITICAL_SECTION;
  RTL_CRITICAL_SECTION = RECORD
  (* This is opaque. *)
    DebugInfo: PRTL_CRITICAL_SECTION_DEBUG;
    (*
     *  The following three fields control entering and exiting the critical
     *  section for the resource
     *)
    LockCount      : INT32;
    RecursionCount : INT32;
    OwningThread   : HANDLE;    (* from the thread's ClientId->UniqueThread *)
    LockSemaphore  : HANDLE;
    SpinCount      : SIZE_T;    (* force size on 64-bit systems when packed *)
  END;

CONST
  DLL_PROCESS_ATTACH = 1;
  DLL_THREAD_ATTACH  = 2;
  DLL_THREAD_DETACH  = 3;
  DLL_PROCESS_DETACH = 0;

(* Defines for the READ flags for Eventlogging *)
  EVENTLOG_SEQUENTIAL_READ = 16_0001;
  EVENTLOG_SEEK_READ       = 16_0002;
  EVENTLOG_FORWARDS_READ   = 16_0004;
  EVENTLOG_BACKWARDS_READ  = 16_0008;

(* The types of events that can be logged. *)
  EVENTLOG_SUCCESS          = 16_0000;
  EVENTLOG_ERROR_TYPE       = 16_0001;
  EVENTLOG_WARNING_TYPE     = 16_0002;
  EVENTLOG_INFORMATION_TYPE = 16_0004;
  EVENTLOG_AUDIT_SUCCESS    = 16_0008;
  EVENTLOG_AUDIT_FAILURE    = 16_0010;

(* Defines for the WRITE flags used by Auditing for paired events *)
(* These are not implemented in Product 1 *)
  EVENTLOG_START_PAIRED_EVENT    = 16_0001;
  EVENTLOG_END_PAIRED_EVENT      = 16_0002;
  EVENTLOG_END_ALL_PAIRED_EVENTS = 16_0004;
  EVENTLOG_PAIRED_EVENT_ACTIVE   = 16_0008;
  EVENTLOG_PAIRED_EVENT_INACTIVE = 16_000C;

(* Structure that defines the header of the Eventlog record. This is the *)
(* fixed-sized portion before all the variable-length strings, binary *)
(* data and pad bytes. *)
(* TimeGenerated is the time it was generated at the client. *)
(* TimeWritten is the time it was put into the log at the server end. *)

TYPE
  PEVENTLOGRECORD = UNTRACED REF EVENTLOGRECORD;
  EVENTLOGRECORD = RECORD
    Length       : UINT32;     (* Length of full record *)
    Reserved     : UINT32;     (* Used by the service *)
    RecordNumber : UINT32;     (* Absolute record number *)
    TimeGenerated: UINT32;     (* Seconds since 1-1-1970 *)
    TimeWritten  : UINT32;     (* Seconds since 1-1-1970 *)
    EventID      : UINT32;
    EventType    : UINT16;
    NumStrings   : UINT16;
    EventCategory: UINT16;
    ReservedFlags: UINT16;      (* For use with paired events (auditing) *)
    ClosingRecordNumber: UINT32;  (* For use with paired events
    (auditing) *)
    StringOffset : UINT32;     (* Offset from beginning of record *)
    UserSidLength: UINT32;
    UserSidOffset: UINT32;
    DataLength   : UINT32;
    DataOffset   : UINT32;     (* Offset from beginning of record *)
    (*
    (* *)
    (* Then follow: *)
    (* *)
    (* WCHAR SourceName[] *)
    (* WCHAR Computername[] *)
    (* SID   UserSid *)
    (* WCHAR Strings[] *)
    (* UINT8  Data[] *)
    (* CHAR  Pad[] *)
    (* UINT32 Length; *)
    (* *)
    *)
  END;

CONST
  DBG_CONTINUE             : UINT32 = 16_00010002;
  DBG_TERMINATE_THREAD     : UINT32 = 16_40010003;
  DBG_TERMINATE_PROCESS    : UINT32 = 16_40010004;
  DBG_CONTROL_C            : UINT32 = 16_40010005;
  DBG_EXCEPTION_NOT_HANDLED: UINT32 = 16_80010001;

(* Registry Specific Access Rights. *)

  KEY_QUERY_VALUE        = 16_0001;
  KEY_SET_VALUE          = 16_0002;
  KEY_CREATE_SUB_KEY     = 16_0004;
  KEY_ENUMERATE_SUB_KEYS = 16_0008;
  KEY_NOTIFY             = 16_0010;
  KEY_CREATE_LINK        = 16_0020;

  KEY_READ = And(Or(STANDARD_RIGHTS_READ,
                    Or(KEY_QUERY_VALUE,
                       Or(KEY_ENUMERATE_SUB_KEYS, KEY_NOTIFY))),
                 Not(SYNCHRONIZE));


  KEY_WRITE = And(Or(STANDARD_RIGHTS_WRITE,
                     Or(KEY_SET_VALUE, KEY_CREATE_SUB_KEY)),
                  Not(SYNCHRONIZE));

  KEY_EXECUTE = And(KEY_READ, Not(SYNCHRONIZE));

  KEY_ALL_ACCESS = And(Or(STANDARD_RIGHTS_ALL,
                          Or(KEY_QUERY_VALUE,
                             Or(KEY_SET_VALUE,
                                Or(KEY_CREATE_SUB_KEY,
                                   Or(KEY_ENUMERATE_SUB_KEYS,
                                      Or(KEY_NOTIFY, KEY_CREATE_LINK)))))),
                       Not(SYNCHRONIZE));



(* Open/Create Options *)

  REG_OPTION_RESERVED: INT32 = 16_00000000; (* Parameter is reserved *)

  REG_OPTION_NON_VOLATILE: INT32 = 16_00000000; (* Key is preserved when
                                                  system is rebooted *)

  REG_OPTION_VOLATILE: INT32 = 16_00000001; (* Key is not preserved when
                                              system is rebooted *)

  REG_OPTION_CREATE_LINK: INT32 = 16_00000002; (* Created key is a symbolic
                                                 link *)

(* Key creation/open disposition *)

  REG_CREATED_NEW_KEY: INT32 = 16_00000001; (* New Registry Key created *)
  REG_OPENED_EXISTING_KEY: INT32 = 16_00000002; (* Existing Key opened *)

(* Key restore flags *)

  REG_WHOLE_HIVE_VOLATILE: INT32 = 16_00000001; (* Restore whole hive
                                                  volatile *)

(* Notify filter values *)

  REG_NOTIFY_CHANGE_NAME: INT32 = 16_00000001; (* Create or delete
                                                 (child) *)
  REG_NOTIFY_CHANGE_ATTRIBUTES: INT32 = 16_00000002;
  REG_NOTIFY_CHANGE_LAST_SET  : INT32 = 16_00000004; (* time stamp *)
  REG_NOTIFY_CHANGE_SECURITY  : INT32 = 16_00000008;

  REG_LEGAL_CHANGE_FILTER = Or(REG_NOTIFY_CHANGE_NAME,
                               Or(REG_NOTIFY_CHANGE_ATTRIBUTES,
                                  Or(REG_NOTIFY_CHANGE_LAST_SET,
                                     REG_NOTIFY_CHANGE_SECURITY)));


(* Predefined Value Types. *)

  REG_NONE = 0;                 (* No value type *)
  REG_SZ   = 1;                 (* Unicode nul terminated string *)
  REG_EXPAND_SZ = 2;            (* Unicode nul terminated string (with
                                   environment variable references) *)
  REG_BINARY              = 3;  (* Free form binary *)
  REG_DWORD               = 4;  (* 32-bit number *)
  REG_DWORD_LITTLE_ENDIAN = 4;  (* 32-bit number (same as REG_DWORD) *)
  REG_DWORD_BIG_ENDIAN    = 5;  (* 32-bit number *)
  REG_LINK                = 6;  (* Symbolic Link (unicode) *)
  REG_MULTI_SZ            = 7;  (* Multiple Unicode strings *)
  REG_RESOURCE_LIST       = 8;  (* Resource list in the resource map *)
  REG_FULL_RESOURCE_DESCRIPTOR = 9; (* Resource list in the hardware
                                       description *)

(* Service Types (Bit Mask) *)

  SERVICE_KERNEL_DRIVER      = 16_00000001;
  SERVICE_FILE_SYSTEM_DRIVER = 16_00000002;
  SERVICE_DRIVER = Or(SERVICE_KERNEL_DRIVER, SERVICE_FILE_SYSTEM_DRIVER);

  SERVICE_ADAPTER = 16_00000004;

  SERVICE_WIN32_OWN_PROCESS   = 16_00000010;
  SERVICE_WIN32_SHARE_PROCESS = 16_00000020;
  SERVICE_WIN32 = Or(
                    SERVICE_WIN32_OWN_PROCESS, SERVICE_WIN32_SHARE_PROCESS);

  SERVICE_TYPE_ALL = Or(SERVICE_WIN32, Or(SERVICE_ADAPTER, SERVICE_DRIVER));

(* Start Type *)

  SERVICE_BOOT_START   = 16_00000000;
  SERVICE_SYSTEM_START = 16_00000001;
  SERVICE_AUTO_START   = 16_00000002;
  SERVICE_DEMAND_START = 16_00000003;
  SERVICE_DISABLED     = 16_00000004;

(* Error control type *)

  SERVICE_ERROR_NORMAL   = 16_00000001;
  SERVICE_ERROR_SEVERE   = 16_00000002;
  SERVICE_ERROR_CRITICAL = 16_00000003;

(* Define the registry driver node enumerations *)

TYPE
  (* !!!  This is scrambled enum *)
  SERVICE_NODE_TYPE = INT32;

CONST
  DriverType            : SERVICE_NODE_TYPE = SERVICE_KERNEL_DRIVER;
  FileSystemType        : SERVICE_NODE_TYPE = SERVICE_FILE_SYSTEM_DRIVER;
  Win32ServiceOwnProcess: SERVICE_NODE_TYPE = SERVICE_WIN32_OWN_PROCESS;
  Win32ServiceShareProcess: SERVICE_NODE_TYPE = SERVICE_WIN32_SHARE_PROCESS;
  AdapterType: SERVICE_NODE_TYPE = SERVICE_ADAPTER;

TYPE SERVICE_LOAD_TYPE = [0 .. 4];

CONST
  BootLoad   : SERVICE_LOAD_TYPE = SERVICE_BOOT_START;
  SystemLoad : SERVICE_LOAD_TYPE = SERVICE_SYSTEM_START;
  AutoLoad   : SERVICE_LOAD_TYPE = SERVICE_AUTO_START;
  DemandLoad : SERVICE_LOAD_TYPE = SERVICE_DEMAND_START;
  DisableLoad: SERVICE_LOAD_TYPE = SERVICE_DISABLED;

TYPE ERROR_CONTROL_TYPE = [1 .. 3];

CONST
  NormalError  : ERROR_CONTROL_TYPE = SERVICE_ERROR_NORMAL;
  SevereError  : ERROR_CONTROL_TYPE = SERVICE_ERROR_SEVERE;
  CriticalError: ERROR_CONTROL_TYPE = SERVICE_ERROR_CRITICAL;

(* IOCTL_TAPE_ERASE definitions *)

CONST
  TAPE_ERASE_SHORT      : INT32 = 0;
  TAPE_ERASE_SHORT_IMMED: INT32 = 1;
  TAPE_ERASE_LONG       : INT32 = 2;
  TAPE_ERASE_LONG_IMMED : INT32 = 3;

TYPE
  TAPE_ERASE = RECORD Type: UINT32;  END;
  PTAPE_ERASE = UNTRACED REF TAPE_ERASE;

(* IOCTL_TAPE_PREPARE definitions *)

CONST
  TAPE_LOAD         : INT32 = 0;
  TAPE_LOAD_IMMED   : INT32 = 1;
  TAPE_UNLOAD       : INT32 = 2;
  TAPE_UNLOAD_IMMED : INT32 = 3;
  TAPE_TENSION      : INT32 = 4;
  TAPE_TENSION_IMMED: INT32 = 5;
  TAPE_LOCK         : INT32 = 6;
  TAPE_UNLOCK       : INT32 = 7;

TYPE
  TAPE_PREPARE = RECORD Operation: UINT32;  END;
  PTAPE_PREPARE = UNTRACED REF TAPE_PREPARE;

(* IOCTL_TAPE_WRITE_MARKS definitions *)

CONST
  TAPE_SETMARKS             : INT32 = 0;
  TAPE_SETMARKS_IMMED       : INT32 = 1;
  TAPE_FILEMARKS            : INT32 = 2;
  TAPE_FILEMARKS_IMMED      : INT32 = 3;
  TAPE_SHORT_FILEMARKS      : INT32 = 4;
  TAPE_SHORT_FILEMARKS_IMMED: INT32 = 5;
  TAPE_LONG_FILEMARKS       : INT32 = 6;
  TAPE_LONG_FILEMARKS_IMMED : INT32 = 7;

TYPE
  PTAPE_WRITE_MARKS = UNTRACED REF TAPE_WRITE_MARKS;
  TAPE_WRITE_MARKS = RECORD
    Type : UINT32;
    Count: UINT32;
  END;

(* IOCTL_TAPE_GET_POSITION definitions *)

CONST
  TAPE_ABSOLUTE_POSITION: INT32 = 0;
  TAPE_LOGICAL_POSITION : INT32 = 1;

TYPE
  PTAPE_GET_POSITION = UNTRACED REF TAPE_GET_POSITION;
  TAPE_GET_POSITION = RECORD
    Type      : UINT32;
    Partition : UINT32;
    OffsetLow : UINT32;
    OffsetHigh: UINT32;
  END;

(* IOCTL_TAPE_SET_POSITION definitions *)

CONST
  TAPE_REWIND               : INT32 = 0;
  TAPE_REWIND_IMMED         : INT32 = 1;
  TAPE_ABSOLUTE_BLOCK       : INT32 = 2;
  TAPE_ABSOLUTE_BLOCK_IMMED : INT32 = 3;
  TAPE_LOGICAL_BLOCK        : INT32 = 4;
  TAPE_LOGICAL_BLOCK_IMMED  : INT32 = 5;
  TAPE_SPACE_END_OF_DATA    : INT32 = 6;
  TAPE_SPACE_RELATIVE_BLOCKS: INT32 = 7;
  TAPE_SPACE_FILEMARKS      : INT32 = 8;
  TAPE_SPACE_SEQUENTIAL_FMKS: INT32 = 9;
  TAPE_SPACE_SETMARKS       : INT32 = 10;
  TAPE_SPACE_SEQUENTIAL_SMKS: INT32 = 11;

TYPE
  PTAPE_SET_POSITION = UNTRACED REF TAPE_SET_POSITION;
  TAPE_SET_POSITION = RECORD
    Method    : UINT32;
    Partition : UINT32;
    OffsetLow : UINT32;
    OffsetHigh: UINT32;
  END;

(* IOCTL_TAPE_GET_DRIVE_PARAMS definitions *)

(* Definitions for FeaturesLow parameter *)

CONST
  TAPE_DRIVE_FIXED     = 16_00000001;
  TAPE_DRIVE_SELECT    = 16_00000002;
  TAPE_DRIVE_INITIATOR = 16_00000004;

  TAPE_DRIVE_ERASE_SHORT    = 16_00000010;
  TAPE_DRIVE_ERASE_LONG     = 16_00000020;
  TAPE_DRIVE_ERASE_BOP_ONLY = 16_00000040;

  TAPE_DRIVE_TAPE_CAPACITY  = 16_00000100;
  TAPE_DRIVE_TAPE_REMAINING = 16_00000200;
  TAPE_DRIVE_FIXED_BLOCK    = 16_00000400;
  TAPE_DRIVE_VARIABLE_BLOCK = 16_00000800;
  TAPE_DRIVE_WRITE_PROTECT  = 16_00001000;

  TAPE_DRIVE_ECC         = 16_00010000;
  TAPE_DRIVE_COMPRESSION = 16_00020000;
  TAPE_DRIVE_PADDING     = 16_00040000;
  TAPE_DRIVE_REPORT_SMKS = 16_00080000;

  TAPE_DRIVE_GET_ABSOLUTE_BLK = 16_00100000;
  TAPE_DRIVE_GET_LOGICAL_BLK  = 16_00200000;

(* Definitions for FeaturesHigh parameter *)

  TAPE_DRIVE_LOAD_UNLOAD = 16_00000001;
  TAPE_DRIVE_TENSION     = 16_00000002;
  TAPE_DRIVE_LOCK_UNLOCK = 16_00000004;

  TAPE_DRIVE_SET_BLOCK_SIZE = 16_00000010;

  TAPE_DRIVE_SET_ECC         = 16_00000100;
  TAPE_DRIVE_SET_COMPRESSION = 16_00000200;
  TAPE_DRIVE_SET_PADDING     = 16_00000400;
  TAPE_DRIVE_SET_REPORT_SMKS = 16_00000800;

  TAPE_DRIVE_ABSOLUTE_BLK     = 16_00001000;
  TAPE_DRIVE_ABS_BLK_IMMED    = 16_00002000;
  TAPE_DRIVE_LOGICAL_BLK      = 16_00004000;
  TAPE_DRIVE_LOG_BLK_IMMED    = 16_00008000;
  TAPE_DRIVE_END_OF_DATA      = 16_00010000;
  TAPE_DRIVE_RELATIVE_BLKS    = 16_00020000;
  TAPE_DRIVE_FILEMARKS        = 16_00040000;
  TAPE_DRIVE_SEQUENTIAL_FMKS  = 16_00080000;
  TAPE_DRIVE_SETMARKS         = 16_00100000;
  TAPE_DRIVE_SEQUENTIAL_SMKS  = 16_00200000;
  TAPE_DRIVE_REVERSE_POSITION = 16_00400000;

  TAPE_DRIVE_WRITE_SETMARKS   = 16_01000000;
  TAPE_DRIVE_WRITE_FILEMARKS  = 16_02000000;
  TAPE_DRIVE_WRITE_SHORT_FMKS = 16_04000000;
  TAPE_DRIVE_WRITE_LONG_FMKS  = 16_08000000;

TYPE
  PTAPE_GET_DRIVE_PARAMETERS = UNTRACED REF TAPE_GET_DRIVE_PARAMETERS;
  TAPE_GET_DRIVE_PARAMETERS = RECORD
    ECC                  : WBOOLEAN;
    Compression          : WBOOLEAN;
    DataPadding          : WBOOLEAN;
    ReportSetmarks       : WBOOLEAN;
    DefaultBlockSize     : UINT32;
    MaximumBlockSize     : UINT32;
    MinimumBlockSize     : UINT32;
    MaximumPartitionCount: UINT32;
    FeaturesLow          : UINT32;
    FeaturesHigh         : UINT32;
  END;

(* IOCTL_TAPE_SET_DRIVE_PARAMETERS definitions *)

  PTAPE_SET_DRIVE_PARAMETERS = UNTRACED REF TAPE_SET_DRIVE_PARAMETERS;
  TAPE_SET_DRIVE_PARAMETERS = RECORD
    ECC           : WBOOLEAN;
    Compression   : WBOOLEAN;
    DataPadding   : WBOOLEAN;
    ReportSetmarks: WBOOLEAN;
  END;

(* IOCTL_TAPE_GET_MEDIA_PARAMETERS definitions *)

  PTAPE_GET_MEDIA_PARAMETERS = UNTRACED REF TAPE_GET_MEDIA_PARAMETERS;
  TAPE_GET_MEDIA_PARAMETERS = RECORD
    CapacityLow   : UINT32;
    CapacityHigh  : UINT32;
    RemainingLow  : UINT32;
    RemainingHigh : UINT32;
    BlockSize     : UINT32;
    PartitionCount: UINT32;
    WriteProtected: WBOOLEAN;
  END;

(* IOCTL_TAPE_SET_MEDIA_PARAMETERS definitions *)

TYPE
  TAPE_SET_MEDIA_PARAMETERS = RECORD BlockSize: UINT32;  END;
  PTAPE_SET_MEDIA_PARAMETERS = UNTRACED REF TAPE_SET_MEDIA_PARAMETERS;

(* IOCTL_TAPE_CREATE_PARTITION definitions *)

CONST
  TAPE_FIXED_PARTITIONS    : INT32 = 0;
  TAPE_SELECT_PARTITIONS   : INT32 = 1;
  TAPE_INITIATOR_PARTITIONS: INT32 = 2;

TYPE
  PTAPE_CREATE_PARTITION = UNTRACED REF TAPE_CREATE_PARTITION;
  TAPE_CREATE_PARTITION = RECORD
    Method: UINT32;
    Count : UINT32;
    Size  : UINT32;
  END;


(* Image Format *)

CONST
  IMAGE_DOS_SIGNATURE    = 16_5A4D; (* MZ *)
  IMAGE_OS2_SIGNATURE    = 16_454E; (* NE *)
  IMAGE_OS2_SIGNATURE_LE = 16_454C; (* LE *)
  IMAGE_NT_SIGNATURE     = 16_00004550; (* PE00 *)

TYPE
  PIMAGE_DOS_HEADER = UNTRACED REF IMAGE_DOS_HEADER;
  IMAGE_DOS_HEADER = RECORD   (* DOS .EXE header *)
    e_magic: UINT16;            (* Magic number *)
    e_cblp: UINT16;             (* Bytes on last page of file *)
    e_cp      : UINT16;         (* Pages in file *)
    e_crlc    : UINT16;         (* Relocations *)
    e_cparhdr : UINT16;         (* Size of header in paragraphs *)
    e_minalloc: UINT16;         (* Minimum extra paragraphs needed *)
    e_maxalloc: UINT16;         (* Maximum extra paragraphs needed *)
    e_ss      : UINT16;         (* Initial (relative) SS value *)
    e_sp      : UINT16;         (* Initial SP value *)
    e_csum    : UINT16;         (* Checksum *)
    e_ip      : UINT16;         (* Initial IP value *)
    e_cs      : UINT16;         (* Initial (relative) CS value *)
    e_lfarlc  : UINT16;         (* File address of relocation table *)
    e_ovno    : UINT16;         (* Overlay number *)
    e_res: ARRAY [0 .. 3] OF UINT16;  (* Reserved words *)
    e_oemid  : UINT16;          (* OEM identifier (for e_oeminfo) *)
    e_oeminfo: UINT16;          (* OEM information; e_oemid specific *)
    e_res2: ARRAY [0 .. 9] OF UINT16;  (* Reserved words *)
    e_lfanew: INT32;           (* File address of new exe header *)
  END;

  PIMAGE_OS2_HEADER = UNTRACED REF IMAGE_OS2_HEADER;
  IMAGE_OS2_HEADER = RECORD   (* OS/2 .EXE header *)
    ne_magic       : UINT16;    (* Magic number *)
    ne_ver         : CHAR;    (* Version number *)
    ne_rev         : CHAR;    (* Revision number *)
    ne_enttab      : UINT16;    (* Offset of Entry Table *)
    ne_cbenttab    : UINT16;    (* Number of bytes in Entry Table *)
    ne_crc         : INT32;    (* Checksum of whole file *)
    ne_flags       : UINT16;    (* Flag word *)
    ne_autodata    : UINT16;    (* Automatic data segment number *)
    ne_heap        : UINT16;    (* Initial heap allocation *)
    ne_stack       : UINT16;    (* Initial stack allocation *)
    ne_csip        : INT32;    (* Initial CS:IP setting *)
    ne_sssp        : INT32;    (* Initial SS:SP setting *)
    ne_cseg        : UINT16;    (* Count of file segments *)
    ne_cmod        : UINT16;    (* Entries in Module Reference Table *)
    ne_cbnrestab   : UINT16;    (* Size of non-resident name table *)
    ne_segtab      : UINT16;    (* Offset of Segment Table *)
    ne_rsrctab     : UINT16;    (* Offset of Resource Table *)
    ne_restab      : UINT16;    (* Offset of resident name table *)
    ne_modtab      : UINT16;    (* Offset of Module Reference Table *)
    ne_imptab      : UINT16;    (* Offset of Imported Names Table *)
    ne_nrestab     : INT32;    (* Offset of Non-resident Names Table *)
    ne_cmovent     : UINT16;    (* Count of movable entries *)
    ne_align       : UINT16;    (* Segment alignment shift count *)
    ne_cres        : UINT16;    (* Count of resource segments *)
    ne_exetyp      : UINT8;    (* Target Operating system *)
    ne_flagsothers : UINT8;    (* Other .EXE flags *)
    ne_pretthunks  : UINT16;    (* offset to return thunks *)
    ne_psegrefbytes: UINT16;    (* offset to segment ref.  bytes *)
    ne_swaparea    : UINT16;    (* Minimum code swap area size *)
    ne_expver      : UINT16;    (* Expected Windows version number *)
  END;

(* File header format. *)

  PIMAGE_FILE_HEADER = UNTRACED REF IMAGE_FILE_HEADER;
  IMAGE_FILE_HEADER = RECORD
    Machine             : UINT16;
    NumberOfSections    : UINT16;
    TimeDateStamp       : UINT32;
    PointerToSymbolTable: UINT32;
    NumberOfSymbols     : UINT32;
    SizeOfOptionalHeader: UINT16;
    Characteristics     : UINT16;
  END;

CONST
  IMAGE_SIZEOF_FILE_HEADER = 20;

  IMAGE_FILE_RELOCS_STRIPPED = 16_0001; (* Relocation info stripped from
    file. *)
  IMAGE_FILE_EXECUTABLE_IMAGE = 16_0002; (* File is executable (i.e.  no
                                            unresolved externel
                                            references). *)
  IMAGE_FILE_LINE_NUMS_STRIPPED = 16_0004; (* Line nunbers stripped from
                                              file. *)
  IMAGE_FILE_LOCAL_SYMS_STRIPPED = 16_0008; (* Local symbols stripped from
                                               file. *)
  IMAGE_FILE_MINIMAL_OBJECT = 16_0010; (* Reserved. *)
  IMAGE_FILE_UPDATE_OBJECT  = 16_0020; (* Reserved. *)
  IMAGE_FILE_16BIT_MACHINE  = 16_0040; (* 16 bit word machine. *)
  IMAGE_FILE_BYTES_REVERSED_LO = 16_0080; (* Bytes of machine word are
                                             reversed. *)
  IMAGE_FILE_32BIT_MACHINE = 16_0100; (* 32 bit word machine. *)
  IMAGE_FILE_PATCH         = 16_0400; (* Reserved. *)
  IMAGE_FILE_SYSTEM        = 16_1000; (* System File. *)
  IMAGE_FILE_DLL           = 16_2000; (* File is a DLL. *)
  IMAGE_FILE_BYTES_REVERSED_HI = 16_8000; (* Bytes of machine word are
                                             reversed. *)

  IMAGE_FILE_MACHINE_UNKNOWN = 0;
  IMAGE_FILE_MACHINE_I860    = 16_14d; (* Intel 860. *)
  IMAGE_FILE_MACHINE_I386    = 16_14c; (* Intel 386. *)
  IMAGE_FILE_MACHINE_R3000 = 16_162; (* MIPS little-endian, 0540
                                        big-endian *)
  IMAGE_FILE_MACHINE_R4000 = 16_166; (* MIPS little-endian *)
  IMAGE_FILE_MACHINE_IA64 = 16_200;    (* Intel Itanium *)
  IMAGE_FILE_MACHINE_AMD64 = 16_8664;  (* AMD64 *)

(* Directory format. *)

TYPE
  PIMAGE_DATA_DIRECTORY = UNTRACED REF IMAGE_DATA_DIRECTORY;
  IMAGE_DATA_DIRECTORY = RECORD
    VirtualAddress: UINT32;
    Size          : UINT32;
  END;

CONST IMAGE_NUMBEROF_DIRECTORY_ENTRIES = 16;

(* Optional header format. *)

TYPE
  PIMAGE_OPTIONAL_HEADER = UNTRACED REF IMAGE_OPTIONAL_HEADER;
  IMAGE_OPTIONAL_HEADER = RECORD
    (*
    (* Standard fields. *)
    *)

    Magic                  : UINT16;
    MajorLinkerVersion     : UINT8;
    MinorLinkerVersion     : UINT8;
    SizeOfCode             : UINT32;
    SizeOfInitializedData  : UINT32;
    SizeOfUninitializedData: UINT32;
    AddressOfEntryPoint    : UINT32;
    BaseOfCode             : UINT32;
    BaseOfData             : UINT32;

    (*
    (* NT additional fields. *)
    *)

    ImageBase                  : UINT32;
    SectionAlignment           : UINT32;
    FileAlignment              : UINT32;
    MajorOperatingSystemVersion: UINT16;
    MinorOperatingSystemVersion: UINT16;
    MajorImageVersion          : UINT16;
    MinorImageVersion          : UINT16;
    MajorSubsystemVersion      : UINT16;
    MinorSubsystemVersion      : UINT16;
    Reserved1                  : UINT32;
    SizeOfImage                : UINT32;
    SizeOfHeaders              : UINT32;
    CheckSum                   : UINT32;
    Subsystem                  : UINT16;
    DllCharacteristics         : UINT16;
    SizeOfStackReserve         : UINT32;
    SizeOfStackCommit          : UINT32;
    SizeOfHeapReserve          : UINT32;
    SizeOfHeapCommit           : UINT32;
    AddressOfTlsIndex          : UINT32;
    NumberOfRvaAndSizes        : UINT32;
    DataDirectory        : ARRAY [0 .. IMAGE_NUMBEROF_DIRECTORY_ENTRIES - 1] OF
                             IMAGE_DATA_DIRECTORY;
  END;

CONST
 IMAGE_SIZEOF_STD_OPTIONAL_HEADER  =    28;
 IMAGE_SIZEOF_NT_OPTIONAL_HEADER   =   224;

TYPE
  PIMAGE_NT_HEADERS = UNTRACED REF IMAGE_NT_HEADERS;
  IMAGE_NT_HEADERS = RECORD
    Signature     : UINT32;
    FileHeader    : IMAGE_FILE_HEADER;
    OptionalHeader: IMAGE_OPTIONAL_HEADER;
  END;

(*!!!???
#define IMAGE_FIRST_SECTION( ntheader ) ((PIMAGE_SECTION_HEADER)        \
    ((UINT32)ntheader +                                                  \
     FIELD_OFFSET( IMAGE_NT_HEADERS, OptionalHeader ) +                 \
     ((PIMAGE_NT_HEADERS)(ntheader))->FileHeader.SizeOfOptionalHeader   \
    ))
*)

(* Subsystem Values *)

CONST
  IMAGE_SUBSYSTEM_UNKNOWN = 0;  (* Unknown subsystem. *)
  IMAGE_SUBSYSTEM_NATIVE  = 1;  (* Image doesn't require a subsystem. *)
  IMAGE_SUBSYSTEM_WINDOWS_GUI = 2; (* Image runs in the Windows GUI
                                      subsystem. *)
  IMAGE_SUBSYSTEM_WINDOWS_CUI = 3; (* Image runs in the Windows character
                                      subsystem. *)
  IMAGE_SUBSYSTEM_OS2_CUI = 5;  (* image runs in the OS/2 character
                                   subsystem. *)
  IMAGE_SUBSYSTEM_POSIX_CUI = 7; (* image run in the Posix character
                                    subsystem. *)

(* Dll Characteristics *)

  IMAGE_LIBRARY_PROCESS_INIT = 1; (* Dll has a process initialization
                                     routine. *)
  IMAGE_LIBRARY_PROCESS_TERM = 2; (* Dll has a thread termination
                                     routine. *)
  IMAGE_LIBRARY_THREAD_INIT = 4; (* Dll has a thread initialization
                                    routine. *)
  IMAGE_LIBRARY_THREAD_TERM = 8; (* Dll has a thread termination
                                    routine. *)

(* Directory Entries *)

  IMAGE_DIRECTORY_ENTRY_EXPORT       = 0; (* Export Directory *)
  IMAGE_DIRECTORY_ENTRY_IMPORT       = 1; (* Import Directory *)
  IMAGE_DIRECTORY_ENTRY_RESOURCE     = 2; (* Resource Directory *)
  IMAGE_DIRECTORY_ENTRY_EXCEPTION    = 3; (* Exception Directory *)
  IMAGE_DIRECTORY_ENTRY_SECURITY     = 4; (* Security Directory *)
  IMAGE_DIRECTORY_ENTRY_BASERELOC    = 5; (* Base Relocation Table *)
  IMAGE_DIRECTORY_ENTRY_DEBUG        = 6; (* Debug Directory *)
  IMAGE_DIRECTORY_ENTRY_COPYRIGHT    = 7; (* Description String *)
  IMAGE_DIRECTORY_ENTRY_GLOBALPTR    = 8; (* Machine Value (MIPS GP) *)
  IMAGE_DIRECTORY_ENTRY_THREAD_SPACE = 9; (* Thread Local Storage *)
  IMAGE_DIRECTORY_ENTRY_CALLBACKS = 10; (* Other interesting entrypoints *)

(* Section header format. *)

CONST IMAGE_SIZEOF_SHORT_NAME = 8;

TYPE
  PIMAGE_SECTION_HEADER = UNTRACED REF IMAGE_SECTION_HEADER;
  IMAGE_SECTION_HEADER = RECORD
    Name: ARRAY [0 .. IMAGE_SIZEOF_SHORT_NAME - 1] OF UINT8;
    Misc: UINT32;
      (*???
          union {
                  UINT32   PhysicalAddress;
                  UINT32   VirtualSize;
          } Misc;
      *)
    VirtualAddress      : UINT32;
    SizeOfRawData       : UINT32;
    PointerToRawData    : UINT32;
    PointerToRelocations: UINT32;
    PointerToLinenumbers: UINT32;
    NumberOfRelocations : UINT16;
    NumberOfLinenumbers : UINT16;
    Characteristics     : UINT32;
  END;

CONST
  IMAGE_SIZEOF_SECTION_HEADER = 40;

  IMAGE_SCN_TYPE_REGULAR = 16_00000000; (* *)
  IMAGE_SCN_TYPE_DUMMY   = 16_00000001; (* Reserved. *)
  IMAGE_SCN_TYPE_NO_LOAD = 16_00000002; (* *)
  IMAGE_SCN_TYPE_GROUPED = 16_00000004; (* Used for 16-bit offset code. *)
  IMAGE_SCN_TYPE_NO_PAD = 16_00000008; (* Specifies if section should not
                                          be padded to next boundary. *)
  IMAGE_SCN_TYPE_COPY = 16_00000010; (* Reserved. *)
  IMAGE_SCN_CNT_CODE  = 16_00000020; (* Section contains code. *)
  IMAGE_SCN_CNT_INITIALIZED_DATA = 16_00000040; (* Section contains
                                                   initialized data. *)
  IMAGE_SCN_CNT_UNINITIALIZED_DATA = 16_00000080; (* Section contains
                                                     uninitialized data. *)
  IMAGE_SCN_LNK_OTHER = 16_00000100; (* Reserved. *)
  IMAGE_SCN_LNK_INFO = 16_00000200; (* Section contains comments or some
                                       other type of information. *)
  IMAGE_SCN_LNK_OVERLAY = 16_00000400; (* Section contains an overlay. *)
  IMAGE_SCN_LNK_REMOVE = 16_00000800; (* Section contents will not become
                                         part of image. *)
  IMAGE_SCN_LNK_COMDAT      = 16_00001000; (* Section contents comdat. *)
  IMAGE_SCN_MEM_DISCARDABLE = 16_02000000; (* Section can be discarded. *)
  IMAGE_SCN_MEM_NOT_CACHED  = 16_04000000; (* Section is not cachable. *)
  IMAGE_SCN_MEM_NOT_PAGED   = 16_08000000; (* Section is not pageable. *)
  IMAGE_SCN_MEM_SHARED      = 16_10000000; (* Section is shareable. *)
  IMAGE_SCN_MEM_EXECUTE     = 16_20000000; (* Section is executable. *)
  IMAGE_SCN_MEM_READ        = 16_40000000; (* Section is readable. *)
  IMAGE_SCN_MEM_WRITE       = 16_80000000; (* Section is writeable. *)

(* Symbol format. *)

TYPE
  PIMAGE_SYMBOL = <* UNALIGNED *> UNTRACED REF IMAGE_SYMBOL;
  IMAGE_SYMBOL = RECORD
    N: ARRAY [0 .. 7] OF UINT8;
    (*???
    union {
        UINT8    ShortName[8];
        struct {
            UINT32   Short;     (* if 0, use LongName *)
            UINT32   Long;      (* offset into string table *)
        } Name;
        PUINT8   LongName[2];
    } N;
    *)
    Value              : UINT32;
    SectionNumber      : INT16;
    Type               : UINT16;
    StorageClass       : UINT8;
    NumberOfAuxSymbols : UINT8;
  END;

CONST IMAGE_SIZEOF_SYMBOL = 18;

(* Section values. *)
(* Symbols have a section number of the section in which they are *)
(* defined. Otherwise, section numbers have the following meanings: *)

  IMAGE_SYM_UNDEFINED: INT16 = 0; (* Symbol is undefined or is common. *)
  IMAGE_SYM_ABSOLUTE : INT16 = -1; (* Symbol is an absolute value. *)
  IMAGE_SYM_DEBUG    : INT16 = -2; (* Symbol is a special debug item. *)

(* Type (fundamental) values. *)

  IMAGE_SYM_TYPE_NULL   = 0;    (* no type. *)
  IMAGE_SYM_TYPE_VOID   = 1;    (* *)
  IMAGE_SYM_TYPE_CHAR   = 2;    (* type character. *)
  IMAGE_SYM_TYPE_SHORT  = 3;    (* type short integer. *)
  IMAGE_SYM_TYPE_INT    = 4;    (* *)
  IMAGE_SYM_TYPE_LONG   = 5;    (* *)
  IMAGE_SYM_TYPE_FLOAT  = 6;    (* *)
  IMAGE_SYM_TYPE_DOUBLE = 7;    (* *)
  IMAGE_SYM_TYPE_STRUCT = 8;    (* *)
  IMAGE_SYM_TYPE_UNION  = 9;    (* *)
  IMAGE_SYM_TYPE_ENUM   = 10;   (* enumeration. *)
  IMAGE_SYM_TYPE_MOE    = 11;   (* member of enumeration. *)
  IMAGE_SYM_TYPE_BYTE   = 12;   (* *)
  IMAGE_SYM_TYPE_WORD   = 13;   (* *)
  IMAGE_SYM_TYPE_UINT   = 14;   (* *)
  IMAGE_SYM_TYPE_DWORD  = 15;   (* *)

(* Type (derived) values. *)

  IMAGE_SYM_DTYPE_NULL     = 0; (* no derived type. *)
  IMAGE_SYM_DTYPE_POINTER  = 1; (* pointer. *)
  IMAGE_SYM_DTYPE_FUNCTION = 2; (* function. *)
  IMAGE_SYM_DTYPE_ARRAY    = 3; (* array. *)

(* Storage classes. *)

  IMAGE_SYM_CLASS_END_OF_FUNCTION (*???: UINT8*) = -1;
  IMAGE_SYM_CLASS_NULL                   = 0;
  IMAGE_SYM_CLASS_AUTOMATIC              = 1;
  IMAGE_SYM_CLASS_EXTERNAL               = 2;
  IMAGE_SYM_CLASS_STATIC                 = 3;
  IMAGE_SYM_CLASS_REGISTER               = 4;
  IMAGE_SYM_CLASS_EXTERNAL_DEF           = 5;
  IMAGE_SYM_CLASS_LABEL                  = 6;
  IMAGE_SYM_CLASS_UNDEFINED_LABEL        = 7;
  IMAGE_SYM_CLASS_MEMBER_OF_STRUCT       = 8;
  IMAGE_SYM_CLASS_ARGUMENT               = 9;
  IMAGE_SYM_CLASS_STRUCT_TAG             = 10;
  IMAGE_SYM_CLASS_MEMBER_OF_UNION        = 11;
  IMAGE_SYM_CLASS_UNION_TAG              = 12;
  IMAGE_SYM_CLASS_TYPE_DEFINITION        = 13;
  IMAGE_SYM_CLASS_UNDEFINED_STATIC       = 14;
  IMAGE_SYM_CLASS_ENUM_TAG               = 15;
  IMAGE_SYM_CLASS_MEMBER_OF_ENUM         = 16;
  IMAGE_SYM_CLASS_REGISTER_PARAM         = 17;
  IMAGE_SYM_CLASS_BIT_FIELD              = 18;
  IMAGE_SYM_CLASS_BLOCK                  = 100;
  IMAGE_SYM_CLASS_FUNCTION               = 101;
  IMAGE_SYM_CLASS_END_OF_STRUCT          = 102;
  IMAGE_SYM_CLASS_FILE                   = 103;
  (* new *)
  IMAGE_SYM_CLASS_SECTION       = 104;
  IMAGE_SYM_CLASS_WEAK_EXTERNAL = 105;

(* type packing constants *)

  N_BTMASK = 8_17;
  N_TMASK  = 8_60;
  N_TMASK1 = 8_300;
  N_TMASK2 = 8_360;
  N_BTSHFT = 4;
  N_TSHIFT = 2;

(* MACROS *)

TYPE
(* Basic Type of x *)
<* INLINE *>
PROCEDURE BTYPE (x: UINT16): UINT16;

(* Is x a pointer? *)

<* INLINE *>
PROCEDURE ISPTR (x: UINT16): BOOLEAN;

(* Is x a function? *)
<* INLINE *>
PROCEDURE ISFCN (x: UINT16): BOOLEAN;

(* Is x an array? *)

<* INLINE *>
PROCEDURE ISARY (x: UINT16): BOOLEAN;

(* Is x a structure, union, or enumeration TAG? *)
<* INLINE *>
PROCEDURE ISTAG (x: UINT16): BOOLEAN;

<* INLINE *>
PROCEDURE INCREF (x: UINT16): UINT16;

<* INLINE *>
PROCEDURE DECREF (x: UINT16): UINT16;

(* Auxiliary entry format. *)

TYPE
  PIMAGE_AUX_SYMBOL = <* UNALIGNED *> UNTRACED REF IMAGE_AUX_SYMBOL;
  IMAGE_AUX_SYMBOL = RECORD
    Sym: RECORD
      TagIndex: UINT32;
    END;
      (*
      struct {
          UINT32    TagIndex;             (* struct, union, or enum tag index *)
          union {
              struct {
                  UINT16    Linenumber;    (* declaration line number *)
                  UINT16    Size;          (* size of struct, union, or enum *)
              } LnSz;
             UINT32    TotalSize;
          } Misc;
          union {
              struct {                   (* if ISFCN, tag, or .bb *)
                  UINT32    PointerToLinenumber;
                  UINT32    PointerToNextFunction;
              } Function;
              struct {                   (* if ISARY, up to 4 dimen. *)
                  UINT16     Dimension[4];
              } Array;
          } FcnAry;
          UINT16    TvIndex;               (* tv index *)
      } Sym;
      *)

    File: RECORD
      Name: ARRAY [0 .. IMAGE_SIZEOF_SYMBOL - 1] OF UINT8;
    END;

    Section: RECORD
      Length: UINT32;  (* section length *)
      NumberOfRelocations: UINT16;  (* number of relocation entries *)
      NumberOfLinenumbers: UINT16;   (* number of line numbers *)
      CheckSum           : UINT32;  (* checksum for communal *)
      Number   : INT16;  (* section number to associate with *)
      Selection: UINT8;   (* communal selection type *)
    END;
  END;

CONST IMAGE_SIZEOF_AUX_SYMBOL = 18;

(* Communal selection types. *)

  IMAGE_COMDAT_SELECT_UNKNOWN      = 0;
  IMAGE_COMDAT_SELECT_NODUPLICATES = 1;
  IMAGE_COMDAT_SELECT_ANY          = 2;
  IMAGE_COMDAT_SELECT_SAME_SIZE    = 3;
  IMAGE_COMDAT_SELECT_EXACT_MATCH  = 4;
  IMAGE_COMDAT_SELECT_ASSOCIATIVE  = 5;

  IMAGE_WEAK_EXTERN_SEARCH_UNKNOWN   = 0;
  IMAGE_WEAK_EXTERN_SEARCH_NOLIBRARY = 1;
  IMAGE_WEAK_EXTERN_SEARCH_LIBRARY   = 2;

(* Relocation format. *)

TYPE
  PIMAGE_RELOCATION = <* UNALIGNED *> UNTRACED REF IMAGE_RELOCATION;
  IMAGE_RELOCATION = RECORD
    VirtualAddress  : UINT32;
    SymbolTableIndex: UINT32;
    Type            : UINT16;
  END;

CONST IMAGE_SIZEOF_RELOCATION = 10;

(* I860 relocation types. *)

  IMAGE_REL_I860_ABSOLUTE = 8_0; (* Reference is absolute, no relocation is
    necessary *)
  IMAGE_REL_I860_DIR32 = 8_06;  (* Direct 32-bit reference to the symbols
                                   virtual address *)
  IMAGE_REL_I860_DIR32NB = 8_07;
  IMAGE_REL_I860_SECTION = 8_012;
  IMAGE_REL_I860_SECREL  = 8_013;
  IMAGE_REL_I860_PAIR    = 8_034;
  IMAGE_REL_I860_HIGH    = 8_036;
  IMAGE_REL_I860_LOW0    = 8_037;
  IMAGE_REL_I860_LOW1    = 8_040;
  IMAGE_REL_I860_LOW2    = 8_041;
  IMAGE_REL_I860_LOW3    = 8_042;
  IMAGE_REL_I860_LOW4    = 8_043;
  IMAGE_REL_I860_SPLIT0  = 8_044;
  IMAGE_REL_I860_SPLIT1  = 8_045;
  IMAGE_REL_I860_SPLIT2  = 8_046;
  IMAGE_REL_I860_HIGHADJ = 8_047;
  IMAGE_REL_I860_BRADDR  = 8_050;

(* I386 relocation types. *)

CONST
  IMAGE_REL_I386_ABSOLUTE = 8_0; (* Reference is absolute, no relocation is
                                    necessary *)
  IMAGE_REL_I386_DIR16 = 8_01;  (* Direct 16-bit reference to the symbols
                                   virtual address *)
  IMAGE_REL_I386_REL16 = 8_02;  (* PC-relative 16-bit reference to the
                                   symbols virtual address *)
  IMAGE_REL_I386_DIR32 = 8_06;  (* Direct 32-bit reference to the symbols
                                   virtual address *)
  IMAGE_REL_I386_DIR32NB = 8_07; (* Direct 32-bit reference to the symbols
                                    virtual address, base not included *)
  IMAGE_REL_I386_SEG12 = 8_011; (* Direct 16-bit reference to the
                                   segment-selector bits of a 32-bit
                                   virtual address *)
  IMAGE_REL_I386_SECTION = 8_012;
  IMAGE_REL_I386_SECREL  = 8_013;
  IMAGE_REL_I386_REL32 = 8_024; (* PC-relative 32-bit reference to the
                                   symbols virtual address *)

(* MIPS relocation types. *)

CONST
  IMAGE_REL_MIPS_ABSOLUTE = 8_0; (* Reference is absolute, no relocation is
                                    necessary *)
  IMAGE_REL_MIPS_REFHALF   = 8_01;
  IMAGE_REL_MIPS_REFWORD   = 8_02;
  IMAGE_REL_MIPS_JMPADDR   = 8_03;
  IMAGE_REL_MIPS_REFHI     = 8_04;
  IMAGE_REL_MIPS_REFLO     = 8_05;
  IMAGE_REL_MIPS_GPREL     = 8_06;
  IMAGE_REL_MIPS_LITERAL   = 8_07;
  IMAGE_REL_MIPS_SECTION   = 8_012;
  IMAGE_REL_MIPS_SECREL    = 8_013;
  IMAGE_REL_MIPS_REFWORDNB = 8_042;
  IMAGE_REL_MIPS_PAIR      = 8_045;

(* Based relocation format. *)

TYPE
  PIMAGE_BASE_RELOCATION = UNTRACED REF IMAGE_BASE_RELOCATION;
  IMAGE_BASE_RELOCATION = RECORD
    VirtualAddress: UINT32;
    SizeOfBlock   : UINT32;
     (* (* UINT16 TypeOffset[1]; *) *)
  END;

CONST IMAGE_SIZEOF_BASE_RELOCATION = 8;

(* Based relocation types. *)

CONST
  IMAGE_REL_BASED_ABSOLUTE     = 0;
  IMAGE_REL_BASED_HIGH         = 1;
  IMAGE_REL_BASED_LOW          = 2;
  IMAGE_REL_BASED_HIGHLOW      = 3;
  IMAGE_REL_BASED_HIGHADJ      = 4;
  IMAGE_REL_BASED_MIPS_JMPADDR = 5;
  IMAGE_REL_BASED_I860_BRADDR  = 6;
  IMAGE_REL_BASED_I860_SPLIT   = 7;

(* Line number format. *)

TYPE
  PIMAGE_LINENUMBER = <* UNALIGNED *> UNTRACED REF IMAGE_LINENUMBER;
  IMAGE_LINENUMBER = RECORD
    Type: UINT32;
    (*
        union {
            UINT32   SymbolTableIndex; (* Symbol table index of function name if Linenumber is 0. *)
            UINT32   VirtualAddress; (* Virtual address of line number. *)
        } Type;
      *)
    Linenumber: UINT16;         (* Line number. *)
  END;

CONST IMAGE_SIZEOF_LINENUMBER = 6;

(* Archive format. *)

CONST IMAGE_ARCHIVE_START_SIZE = 8;
VAR                             (* !!!  CONST *)
  IMAGE_ARCHIVE_START        : char_star; (* := TtoS("!<arch>\n")*)
  IMAGE_ARCHIVE_END          : char_star; (* := TtoS("`\n")*)
  IMAGE_ARCHIVE_PAD          : char_star; (* := TtoS("\n")*)
  IMAGE_ARCHIVE_LINKER_MEMBER: char_star; (* := TtoS("/ ")*)
  IMAGE_ARCHIVE_LONGNAMES_MEMBER: char_star; (* := TtoS("// ")*)

TYPE
  PIMAGE_ARCHIVE_MEMBER_HEADER = UNTRACED REF IMAGE_ARCHIVE_MEMBER_HEADER;
  IMAGE_ARCHIVE_MEMBER_HEADER = RECORD
    Name     : ARRAY [0 .. 15] OF UINT8;  (* member name - `/' terminated. *)
    Date     : ARRAY [0 .. 11] OF UINT8;  (* member date - decimal secs since 1970 *)
    UserID   : ARRAY [0 .. 5]  OF UINT8;  (* member user id - decimal. *)
    GroupID  : ARRAY [0 .. 5]  OF UINT8;  (* member group id - decimal. *)
    Mode     : ARRAY [0 .. 7]  OF UINT8;  (* member mode - octal. *)
    Size     : ARRAY [0 .. 9]  OF UINT8;  (* member size - decimal. *)
    EndHeader: ARRAY [0 .. 1]  OF UINT8;  (* String to end header. *)
  END;

CONST IMAGE_SIZEOF_ARCHIVE_MEMBER_HDR = 60;

(* DLL support. *)

(* Export Format *)

TYPE
  PIMAGE_EXPORT_DIRECTORY = UNTRACED REF IMAGE_EXPORT_DIRECTORY;
  IMAGE_EXPORT_DIRECTORY = RECORD
    Characteristics      : UINT32;
    TimeDateStamp        : UINT32;
    MajorVersion         : UINT16;
    MinorVersion         : UINT16;
    Name                 : UINT32;
    Base                 : UINT32;
    NumberOfFunctions    : UINT32;
    NumberOfNames        : UINT32;
    AddressOfFunctions   : UNTRACED REF PUINT32;
    AddressOfNames       : UNTRACED REF PUINT32;
    AddressOfNameOrdinals: UNTRACED REF PUINT16;
  END;

(* Import Format *)

TYPE
  PIMAGE_IMPORT_BY_NAME = UNTRACED REF IMAGE_IMPORT_BY_NAME;
  IMAGE_IMPORT_BY_NAME = RECORD
    Hint  :  UINT16;
    Name  : ARRAY [0 .. 1 - 1] OF  UINT8;
  END;

  PIMAGE_THUNK_DATA = UNTRACED REF IMAGE_THUNK_DATA;
  IMAGE_THUNK_DATA = RECORD
    u1: UINT32;
    (*
    union {
    PUINT32 Function;
    UINT32 Ordinal;
    PIMAGE_IMPORT_BY_NAME AddressOfData;
    } u1;
    *)
  END;

CONST
 IMAGE_ORDINAL_FLAG = 16_80000000;
 IMAGE_ORDINAL_FLAG32 = 16_80000000;

TYPE
<* INLINE *>
PROCEDURE IMAGE_SNAP_BY_ORDINAL (Ordinal: UINT16): BOOLEAN;

<* INLINE *>
PROCEDURE IMAGE_ORDINAL (Ordinal: UINT16): UINT16;

TYPE
  PIMAGE_IMPORT_DESCRIPTOR = UNTRACED REF IMAGE_IMPORT_DESCRIPTOR;
  IMAGE_IMPORT_DESCRIPTOR = RECORD
    Characteristics: UINT32;
    TimeDateStamp  : UINT32;
    MajorVersion   : UINT16;
    MinorVersion   : UINT16;
    Name           : UINT32;
    FirstThunk     : PIMAGE_THUNK_DATA;
  END;

(* Resource Format. *)

(* Resource directory consists of two counts, following by a variable length *)
(* array of directory entries.  The first count is the number of entries at *)
(* beginning of the array that have actual names associated with each entry. *)
(* The entries are in ascending order, case insensitive strings.  The second *)
(* count is the number of entries that immediately follow the named entries. *)
(* This second count identifies the number of entries that have 31-bit integer *)
(* Ids as their name.  These entries are also sorted in ascending order. *)
(* This structure allows fast lookup by either name or number, but for any *)
(* given resource entry only one form of lookup is supported, not both. *)
(* This is consistant with the syntax of the .RC file and the .RES file. *)

TYPE
  PIMAGE_RESOURCE_DIRECTORY = UNTRACED REF IMAGE_RESOURCE_DIRECTORY;
  IMAGE_RESOURCE_DIRECTORY = RECORD
    Characteristics     : UINT32;
    TimeDateStamp       : UINT32;
    MajorVersion        : UINT16;
    MinorVersion        : UINT16;
    NumberOfNamedEntries: UINT16;
    NumberOfIdEntries   : UINT16;
    (* (* IMAGE_RESOURCE_DIRECTORY_ENTRY DirectoryEntries[]; *) *)
  END;

CONST
  IMAGE_RESOURCE_NAME_IS_STRING    = 16_80000000;
  IMAGE_RESOURCE_DATA_IS_DIRECTORY = 16_80000000;

(* Each directory contains the 32-bit Name of the entry and an offset, *)
(* relative to the beginning of the resource directory of the data associated *)
(* with this directory entry.  If the name of the entry is an actual text *)
(* string instead of an integer Id, then the high order bit of the name field *)
(* is set to one and the low order 31-bits are an offset, relative to the *)
(* beginning of the resource directory of the string, which is of type *)
(* IMAGE_RESOURCE_DIRECTORY_STRING.  Otherwise the high bit is clear and the *)
(* low-order 31-bits are the integer Id that identify this resource directory *)
(* entry. If the directory entry is yet another resource directory (i.e. a *)
(* subdirectory), then the high order bit of the offset field will be *)
(* set to indicate this.  Otherwise the high bit is clear and the offset *)
(* field points to a resource data entry. *)

TYPE
  PIMAGE_RESOURCE_DIRECTORY_ENTRY =UNTRACED REF IMAGE_RESOURCE_DIRECTORY_ENTRY;
  IMAGE_RESOURCE_DIRECTORY_ENTRY = RECORD
    Name        : UINT32;
    OffsetToData: UINT32;
  END;

(* For resource directory entries that have actual string names, the Name *)
(* field of the directory entry points to an object of the following type. *)
(* All of these string objects are stored together after the last resource *)
(* directory entry and before the first resource data object.  This minimizes *)
(* the impact of these variable length objects on the alignment of the fixed *)
(* size directory entry objects. *)

TYPE
  PIMAGE_RESOURCE_DIRECTORY_STRING = UNTRACED REF IMAGE_RESOURCE_DIRECTORY_STRING;
  IMAGE_RESOURCE_DIRECTORY_STRING = RECORD
    Length    : UINT16;
    NameString: ARRAY [0 .. 0] OF CHAR;
  END;

  PIMAGE_RESOURCE_DIR_STRING_U = UNTRACED REF IMAGE_RESOURCE_DIR_STRING_U;
  IMAGE_RESOURCE_DIR_STRING_U = RECORD
    Length    : UINT16;
    NameString: ARRAY [0 .. 0] OF WCHAR;
  END;

(* Each resource data entry describes a leaf node in the resource directory *)
(* tree.  It contains an offset, relative to the beginning of the resource *)
(* directory of the data for the resource, a size field that gives the number *)
(* of bytes of data at that offset, a CodePage that should be used when *)
(* decoding code point values within the resource data.  Typically for new *)
(* applications the code page would be the unicode code page. *)

TYPE
  PIMAGE_RESOURCE_DATA_ENTRY = UNTRACED REF IMAGE_RESOURCE_DATA_ENTRY;
  IMAGE_RESOURCE_DATA_ENTRY = RECORD
    OffsetToData : UINT32;
    Size         : UINT32;
    CodePage     : UINT32;
    Reserved     : UINT32;
  END;

(* Debug Format *)

TYPE
  PIMAGE_DEBUG_DIRECTORY = UNTRACED REF IMAGE_DEBUG_DIRECTORY;
  IMAGE_DEBUG_DIRECTORY = RECORD
    Characteristics : UINT32;
    TimeDateStamp   : UINT32;
    MajorVersion    : UINT16;
    MinorVersion    : UINT16;
    Type            : UINT32;
    SizeOfData      : UINT32;
    AddressOfRawData: UINT32;
    PointerToRawData: UINT32;
  END;

CONST
  IMAGE_DEBUG_TYPE_UNKNOWN  = 0;
  IMAGE_DEBUG_TYPE_COFF     = 1;
  IMAGE_DEBUG_TYPE_CODEVIEW = 2;

TYPE
  PIMAGE_DEBUG_INFO = UNTRACED REF IMAGE_DEBUG_INFO;
  IMAGE_DEBUG_INFO = RECORD
    NumberOfSymbols     : UINT32;
    LvaToFirstSymbol    : UINT32;
    NumberOfLinenumbers : UINT32;
    LvaToFirstLinenumber: UINT32;
    RvaToFirstByteOfCode: UINT32;
    RvaToLastByteOfCode : UINT32;
    RvaToFirstByteOfData: UINT32;
    RvaToLastByteOfData : UINT32;
  END;

(* End Image Format *)

<*EXTERNAL "WinNT__MemoryBarrier"*>
PROCEDURE MemoryBarrier();

END WinNT. 
