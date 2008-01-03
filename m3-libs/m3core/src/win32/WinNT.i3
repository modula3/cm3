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
 * Some basic type declarations have been moved to WinDef.i3 to avoid circular
 * dependencies.
 *)

<* PRAGMA UNALIGNED *>

IMPORT Ctypes, Word, WinBaseTypes;

FROM Word IMPORT And, Or, Not;
FROM WinDef IMPORT BYTE, CCHAR, DWORD, INT, PDWORD, PWORD, WORD;

TYPE
  ANYSIZE_ARRAY = [0 .. 0];

  PVOID = Ctypes.void_star;

  SHORT = WinBaseTypes.SHORT;
  LONG = WinBaseTypes.LONG;

(*
 * UNICODE (Wide Character) types
 *)

TYPE
  WCHAR = Ctypes.unsigned_short;    (* wc,   16-bit UNICODE character *)

  PWCHAR  = UNTRACED REF WCHAR;
  LPWCH   = UNTRACED REF WCHAR;
  PWCH    = UNTRACED REF WCHAR;
  LPCWCH  = UNTRACED REF WCHAR;
  PCWCH   = UNTRACED REF WCHAR;
  NWPSTR  = UNTRACED REF WCHAR;
  LPWSTR  = UNTRACED REF WCHAR;
  PWSTR   = UNTRACED REF WCHAR;
  LPCWSTR = UNTRACED REF WCHAR;
  PCWSTR  = UNTRACED REF WCHAR;

(*
 * ANSI (Multi-byte Character) types
 *)

TYPE
  PCHAR  = Ctypes.char_star;
  LPCH   = Ctypes.char_star;
  PCH    = Ctypes.char_star;

  LPCCH  = Ctypes.char_star;
  PCCH   = Ctypes.char_star;
  NPSTR  = Ctypes.char_star;
  LPSTR  = Ctypes.char_star;
  PSTR   = Ctypes.char_star;
  LPCSTR = Ctypes.char_star;
  PCSTR  = Ctypes.char_star;

(*
 * Neutral ANSI types
 *)

TYPE
  TCHAR  = Ctypes.char;
  PTCHAR = Ctypes.char_star;

  LPTCH   = LPSTR;
  PTCH    = LPSTR;
  PTSTR   = LPSTR;
  LPTSTR  = LPSTR;
  LPCTSTR = LPCSTR;
  LP      = LPWSTR;

  PSHORT  = WinBaseTypes.PSHORT;
  PLONG   = WinBaseTypes.PLONG;

  HANDLE  = WinBaseTypes.HANDLE;
  PHANDLE = WinBaseTypes.PHANDLE;

  LCID    = DWORD;
  PLCID   = PDWORD;
  LANGID  = WORD;

CONST
  APPLICATION_ERROR_MASK       = 16_20000000;
  ERROR_SEVERITY_SUCCESS       = 16_00000000;
  ERROR_SEVERITY_INFORMATIONAL = 16_40000000;
  ERROR_SEVERITY_WARNING       = 16_80000000;
  ERROR_SEVERITY_ERROR         = 16_C0000000;

TYPE
  PLARGE_INTEGER = UNTRACED REF LARGE_INTEGER;
  LARGE_INTEGER = RECORD
    LowPart: DWORD;
    HighPart: LONG;
  END;

  PULARGE_INTEGER = UNTRACED REF ULARGE_INTEGER;
  ULARGE_INTEGER = RECORD
    LowPart: DWORD;
    HighPart: DWORD;
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

TYPE
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

CONST
  STATUS_WAIT_0                  : DWORD = 16_00000000;
  STATUS_ABANDONED_WAIT_0        : DWORD = 16_00000080;
  STATUS_USER_APC                : DWORD = 16_000000C0;
  STATUS_TIMEOUT                 : DWORD = 16_00000102;
  STATUS_PENDING                 : DWORD = 16_00000103;
  STATUS_DATATYPE_MISALIGNMENT   : DWORD = 16_80000002;
  STATUS_BREAKPOINT              : DWORD = 16_80000003;
  STATUS_SINGLE_STEP             : DWORD = 16_80000004;
  STATUS_ACCESS_VIOLATION        : DWORD = 16_C0000005;
  STATUS_ILLEGAL_INSTRUCTION     : DWORD = 16_C000001D;
  STATUS_NONCONTINUABLE_EXCEPTION: DWORD = 16_C0000025;
  STATUS_INVALID_DISPOSITION     : DWORD = 16_C0000026;
  STATUS_ARRAY_BOUNDS_EXCEEDED   : DWORD = 16_C000008C;
  STATUS_FLOAT_DENORMAL_OPERAND  : DWORD = 16_C000008D;
  STATUS_FLOAT_DIVIDE_BY_ZERO    : DWORD = 16_C000008E;
  STATUS_FLOAT_INEXACT_RESULT    : DWORD = 16_C000008F;
  STATUS_FLOAT_INVALID_OPERATION : DWORD = 16_C0000090;
  STATUS_FLOAT_OVERFLOW          : DWORD = 16_C0000091;
  STATUS_FLOAT_STACK_CHECK       : DWORD = 16_C0000092;
  STATUS_FLOAT_UNDERFLOW         : DWORD = 16_C0000093;
  STATUS_INTEGER_DIVIDE_BY_ZERO  : DWORD = 16_C0000094;
  STATUS_INTEGER_OVERFLOW        : DWORD = 16_C0000095;
  STATUS_PRIVILEGED_INSTRUCTION  : DWORD = 16_C0000096;
  STATUS_STACK_OVERFLOW          : DWORD = 16_C00000FD;
  STATUS_CONTROL_C_EXIT          : DWORD = 16_C000013A;

  MAXIMUM_WAIT_OBJECTS = 64;    (* Maximum number of wait objects *)

  MAXIMUM_SUSPEND_COUNT = MAXCHAR; (* Maximum times thread can be suspended *)

TYPE
KSPIN_LOCK = DWORD;  

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
    ExceptionCode   : DWORD;
    ExceptionFlags  : DWORD;
    ExceptionRecord : UNTRACED REF EXCEPTION_RECORD;
    ExceptionAddress: PVOID;
    NumberParameters: DWORD;
    ExceptionInformation: ARRAY
                              [0 .. EXCEPTION_MAXIMUM_PARAMETERS - 1] OF
                              DWORD;
  END;


(*
 * Typedef for pointer returned by exception_info()
 *)

TYPE
  PEXCEPTION_POINTERS = UNTRACED REF EXCEPTION_POINTERS;
  EXCEPTION_POINTERS = RECORD
    ExceptionRecord: PEXCEPTION_RECORD;
    ContextRecord: Ctypes.void_star;  (* !!! Architecture-dependent
                                          context pointer *)
  END;


CONST
  PROCESS_TERMINATE         = (16_0001);
  PROCESS_CREATE_THREAD     = (16_0002);
  PROCESS_VM_OPERATION      = (16_0008);
  PROCESS_VM_READ           = (16_0010);
  PROCESS_VM_WRITE          = (16_0020);
  PROCESS_DUP_HANDLE        = (16_0040);
  PROCESS_CREATE_PROCESS    = (16_0080);
  PROCESS_SET_INFORMATION   = (16_0200);
  PROCESS_QUERY_INFORMATION = (16_0400);
  PROCESS_ALL_ACCESS = Or(STANDARD_RIGHTS_REQUIRED,
                       Or(SYNCHRONIZE,
                          16_FFF));

  THREAD_TERMINATE            = (16_0001);
  THREAD_SUSPEND_RESUME       = (16_0002);
  THREAD_GET_CONTEXT          = (16_0008);
  THREAD_SET_CONTEXT          = (16_0010);
  THREAD_SET_INFORMATION      = (16_0020);
  THREAD_QUERY_INFORMATION    = (16_0040);
  THREAD_SET_THREAD_TOKEN     = (16_0080);
  THREAD_IMPERSONATE          = (16_0100);
  THREAD_DIRECT_IMPERSONATION = (16_0200);
  THREAD_ALL_ACCESS = Or(STANDARD_RIGHTS_REQUIRED,
                      Or(SYNCHRONIZE,
                         16_3FF));

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
  PROCESSOR_MIPS_R4000    = 4000;
  PROCESSOR_ALPHA_21064   = 21064;

  PROCESSOR_ARCHITECTURE_INTEL   = 0;
  PROCESSOR_ARCHITECTURE_MIPS    = 1;
  PROCESSOR_ARCHITECTURE_ALPHA   = 2;
  PROCESSOR_ARCHITECTURE_PPC     = 3;
  PROCESSOR_ARCHITECTURE_UNKNOWN = 16_FFFF;

TYPE
  PMEMORY_BASIC_INFORMATION = UNTRACED REF MEMORY_BASIC_INFORMATION;
  MEMORY_BASIC_INFORMATION = RECORD
    BaseAddress      : PVOID;
    AllocationBase   : PVOID;
    AllocationProtect: DWORD;
    RegionSize       : DWORD;
    State            : DWORD;
    Protect          : DWORD;
    Type             : DWORD;
  END;

CONST
  SECTION_QUERY       = 16_0001;
  SECTION_MAP_WRITE   = 16_0002;
  SECTION_MAP_READ    = 16_0004;
  SECTION_MAP_EXECUTE = 16_0008;
  SECTION_EXTEND_SIZE = 16_0010;

  SECTION_ALL_ACCESS = Or(STANDARD_RIGHTS_REQUIRED,
                       Or(SECTION_QUERY,
                       Or(SECTION_MAP_WRITE,
                       Or(SECTION_MAP_READ,
                       Or(SECTION_MAP_EXECUTE,                                                               SECTION_EXTEND_SIZE)))));
  PAGE_NOACCESS                 = 16_01;
  PAGE_READONLY                 = 16_02;
  PAGE_READWRITE                = 16_04;
  MEM_COMMIT                    = 16_1000;
  MEM_RESERVE                   = 16_2000;
  MEM_DECOMMIT                  = 16_4000;
  MEM_RELEASE                   = 16_8000;
  MEM_FREE                      = 16_10000;
  MEM_PRIVATE                   = 16_20000;
  FILE_SHARE_READ               = 16_00000001;
  FILE_SHARE_WRITE              = 16_00000002;
  FILE_ATTRIBUTE_READONLY       = 16_00000001;
  FILE_ATTRIBUTE_HIDDEN         = 16_00000002;
  FILE_ATTRIBUTE_SYSTEM         = 16_00000004;
  FILE_ATTRIBUTE_DIRECTORY      = 16_00000010;
  FILE_ATTRIBUTE_ARCHIVE        = 16_00000020;
  FILE_ATTRIBUTE_NORMAL         = 16_00000080;
  FILE_ATTRIBUTE_TEMPORARY      = 16_00000100;
  FILE_ATTRIBUTE_ATOMIC_WRITE   = 16_00000200;
  FILE_ATTRIBUTE_XACTION_WRITE  = 16_00000400;
  FILE_NOTIFY_CHANGE_FILE_NAME  = 16_00000001;
  FILE_NOTIFY_CHANGE_DIR_NAME   = 16_00000002;
  FILE_NOTIFY_CHANGE_ATTRIBUTES = 16_00000004;
  FILE_NOTIFY_CHANGE_SIZE       = 16_00000008;
  FILE_NOTIFY_CHANGE_LAST_WRITE = 16_00000010;
  FILE_NOTIFY_CHANGE_SECURITY   = 16_00000100;
  FILE_CASE_SENSITIVE_SEARCH    = 16_00000001;
  FILE_CASE_PRESERVED_NAMES     = 16_00000002;
  FILE_UNICODE_ON_DISK          = 16_00000004;
  FILE_PERSISTENT_ACLS          = 16_00000008;
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
(*          WORD   SpecificRights; *)
(*          BYTE  StandardRights; *)
(*          BYTE  AccessSystemAcl : 1; *)
(*          BYTE  Reserved : 3; *)
(*          BYTE  GenericAll : 1; *)
(*          BYTE  GenericExecute : 1; *)
(*          BYTE  GenericWrite : 1; *)
(*          BYTE  GenericRead : 1; *)
(*      } ACCESS_MASK; *)
(*      typedef ACCESS_MASK *PACCESS_MASK; *)
(*  but to make life simple for programmer's we'll allow them to specify *)
(*  a desired access mask by simply OR'ing together mulitple single rights *)
(*  and treat an access mask as a ulong.  For example *)
(*      DesiredAccess = DELETE | READ_CONTROL *)
(*  So we'll declare ACCESS_MASK as DWORD *)

TYPE
 ACCESS_MASK = DWORD;
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

CONST
  ACCESS_SYSTEM_SECURITY = 16_01000000;

(*
 * MaximumAllowed access type
 *)

CONST
  MAXIMUM_ALLOWED  = 16_02000000;

(*
 *  These are the generic rights.
 *)

CONST
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

TYPE
  PLUID_AND_ATTRIBUTES = UNTRACED REF LUID_AND_ATTRIBUTES;
  LUID_AND_ATTRIBUTES = RECORD
    Luid      : LUID;
    Attributes: DWORD;
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

TYPE
  PSID_IDENTIFIER_AUTHORITY = UNTRACED REF SID_IDENTIFIER_AUTHORITY;
  SID_IDENTIFIER_AUTHORITY = RECORD
    Value: ARRAY [0 .. 5] OF BYTE;
  END;

  PISID = UNTRACED REF SID;
  SID = RECORD
   Revision           : BYTE;
   SubAuthorityCount  : BYTE;
   IdentifierAuthority: SID_IDENTIFIER_AUTHORITY;
   SubAuthority       : ARRAY ANYSIZE_ARRAY OF DWORD;
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
    Attributes: DWORD;
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
                                     ARRAY [0 .. 5] OF BYTE{0, 0, 0, 0, 0, 0}};
  SECURITY_WORLD_SID_AUTHORITY   = SID_IDENTIFIER_AUTHORITY{
                                     ARRAY [0 .. 5] OF BYTE{0, 0, 0, 0, 0, 1}};
  SECURITY_LOCAL_SID_AUTHORITY   = SID_IDENTIFIER_AUTHORITY{
                                     ARRAY [0 .. 5] OF BYTE{0, 0, 0, 0, 0, 2}};
  SECURITY_CREATOR_SID_AUTHORITY = SID_IDENTIFIER_AUTHORITY{
                                     ARRAY [0 .. 5] OF BYTE{0, 0, 0, 0, 0, 3}};

  SECURITY_NULL_RID : LONG = 16_00000000;
  SECURITY_WORLD_RID: LONG = 16_00000000;
  SECURITY_LOCAL_RID: LONG = 16_00000000;

  SECURITY_CREATOR_OWNER_RID: LONG = 16_00000000;
  SECURITY_CREATOR_GROUP_RID: LONG = 16_00000001;



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

CONST
  SECURITY_NT_AUTHORITY = SID_IDENTIFIER_AUTHORITY{
                            ARRAY [0 .. 5] OF BYTE{0, 0, 0, 0, 0, 5}};

  SECURITY_DIALUP_RID     : LONG = 16_00000001;
  SECURITY_NETWORK_RID    : LONG = 16_00000002;
  SECURITY_BATCH_RID      : LONG = 16_00000003;
  SECURITY_INTERACTIVE_RID: LONG = 16_00000004;
  SECURITY_SERVICE_RID    : LONG = 16_00000006;

  SECURITY_LOGON_IDS_RID      : LONG = 16_00000005;
  SECURITY_LOGON_IDS_RID_COUNT: LONG = 3;

  SECURITY_LOCAL_SYSTEM_RID: LONG = 16_00000012;

  SECURITY_BUILTIN_DOMAIN_RID: LONG = 16_00000020;

(*//////////////////////////////////////////////////////////*)
(*                                                          *)
(* well-known domain relative sub-authority values (RIDs)...*)
(*                                                          *)
(*//////////////////////////////////////////////////////////*)

(* Well-known users ... *)

CONST
  DOMAIN_USER_RID_ADMIN: LONG = 16_000001F4;
  DOMAIN_USER_RID_GUEST: LONG = 16_000001F5;

(* well-known groups ... *)

CONST
  DOMAIN_GROUP_RID_ADMINS: LONG = 16_00000200;
  DOMAIN_GROUP_RID_USERS : LONG = 16_00000201;

(* well-known aliases ... *)

CONST
  DOMAIN_ALIAS_RID_ADMINS     : LONG = 16_00000220;
  DOMAIN_ALIAS_RID_USERS      : LONG = 16_00000221;
  DOMAIN_ALIAS_RID_GUESTS     : LONG = 16_00000222;
  DOMAIN_ALIAS_RID_POWER_USERS: LONG = 16_00000223;

  DOMAIN_ALIAS_RID_ACCOUNT_OPS: LONG = 16_00000224;
  DOMAIN_ALIAS_RID_SYSTEM_OPS : LONG = 16_00000225;
  DOMAIN_ALIAS_RID_PRINT_OPS  : LONG = 16_00000226;
  DOMAIN_ALIAS_RID_BACKUP_OPS : LONG = 16_00000227;

  DOMAIN_ALIAS_RID_REPLICATOR: LONG = 16_00000228;

(*
 * Allocate the System Luid.  The first 1000 LUIDs are reserved.
 * Use #999 here (16_3E7 = 999)
 *)

CONST
  SYSTEM_LUID = LUID{16_0, 16_3E7};

(*////////////////////////////////////////////////////////////////////// *)
(*                                                                    // *)
(*                          User and Group related SID attributes     // *)
(*                                                                    // *)
(*////////////////////////////////////////////////////////////////////// *)

(* Group attributes *)

CONST
SE_GROUP_MANDATORY:LONG=              16_00000001;
SE_GROUP_ENABLED_BY_DEFAULT:LONG=     16_00000002;
SE_GROUP_ENABLED:LONG=                16_00000004;
SE_GROUP_OWNER:LONG=                  16_00000008;
SE_GROUP_LOGON_ID:LONG=               16_C0000000;


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

CONST
  ACL_REVISION = 2;

(* This is the history of ACL revisions.  Add a new one whenever *)
(* ACL_REVISION is updated *)

CONST
  ACL_REVISION1 = 1;
  ACL_REVISION2 = 2;

TYPE
  PACL = UNTRACED REF ACL;
  ACL = RECORD
    AclRevision: BYTE;
    Sbz1       : BYTE;
    AclSize    : WORD;
    AceCount   : WORD;
    Sbz2       : WORD;
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

TYPE
  PACE_HEADER = UNTRACED REF ACE_HEADER;
  ACE_HEADER = RECORD
    AceType : BYTE;
    AceFlags: BYTE;
    AceSize : WORD;
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

CONST
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

CONST
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
    SidStart: DWORD;
  END;

  PACCESS_DENIED_ACE = UNTRACED REF ACCESS_DENIED_ACE;
  ACCESS_DENIED_ACE = RECORD
    Header  : ACE_HEADER;
    Mask    : ACCESS_MASK;
    SidStart: DWORD;
  END;

  PSYSTEM_AUDIT_ACE = UNTRACED REF SYSTEM_AUDIT_ACE;
  SYSTEM_AUDIT_ACE = RECORD
    Header  : ACE_HEADER;
    Mask    : ACCESS_MASK;
    SidStart: DWORD;
  END;

  PSYSTEM_ALARM_ACE = UNTRACED REF SYSTEM_ALARM_ACE;
  SYSTEM_ALARM_ACE = RECORD
    Header  : ACE_HEADER;
    Mask    : ACCESS_MASK;
    SidStart: DWORD;
  END;


(*  The following declarations are used for setting and querying information *)
(*  about and ACL.  First are the various information classes available to *)
(*  the user. *)

TYPE
  ACL_INFORMATION_CLASS = [1 .. 2];

CONST
  AclRevisionInformation: ACL_INFORMATION_CLASS = 1;
  AclSizeInformation    : ACL_INFORMATION_CLASS = 2;

(*  This record is returned/sent if the user is requesting/setting the *)
(*  AclRevisionInformation *)

TYPE
  ACL_REVISION_INFORMATION = RECORD AclRevision: DWORD;  END;
  PACL_REVISION_INFORMATION = UNTRACED REF ACL_REVISION_INFORMATION;

(*  This record is returned if the user is requesting AclSizeInformation *)

TYPE
  PACL_SIZE_INFORMATION = UNTRACED REF ACL_SIZE_INFORMATION;
  ACL_SIZE_INFORMATION = RECORD
    AceCount     : DWORD;
    AclBytesInUse: DWORD;
    AclBytesFree : DWORD;
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
  SECURITY_DESCRIPTOR_CONTROL = WORD;
  PSECURITY_DESCRIPTOR_CONTROL = UNTRACED REF WORD;

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
    Revision: BYTE;
    Sbz1    : BYTE;
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
  SE_PRIVILEGE_ENABLED_BY_DEFAULT: LONG = 16_00000001;
  SE_PRIVILEGE_ENABLED           : LONG = 16_00000002;
  SE_PRIVILEGE_USED_FOR_ACCESS   : LONG = 16_80000000;



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
    PrivilegeCount: DWORD;
    Control       : DWORD;
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
  SECURITY_IMPERSONATION_LEVEL = [0 .. 3];
  PSECURITY_IMPERSONATION_LEVEL =
    UNTRACED REF SECURITY_IMPERSONATION_LEVEL;

CONST
  SecurityAnonymous      = 0;
  SecurityIdentification = 1;
  SecurityImpersonation  = 2;
  SecurityDelegation     = 3;


CONST
  SECURITY_MAX_IMPERSONATION_LEVEL = SecurityDelegation;
  DEFAULT_IMPERSONATION_LEVEL      = SecurityImpersonation;

(* Security Tracking Mode *)

CONST
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
    Length             : DWORD;
    ImpersonationLevel : SECURITY_IMPERSONATION_LEVEL;
    ContextTrackingMode: SECURITY_CONTEXT_TRACKING_MODE;
    EffectiveOnly      : WBOOLEAN;
  END;

(* Used to represent information related to a thread impersonation *)

TYPE
  PSE_IMPERSONATION_STATE = UNTRACED REF SE_IMPERSONATION_STATE;
  SE_IMPERSONATION_STATE = RECORD
    Token        : PACCESS_TOKEN;
    CopyOnOpen   : WBOOLEAN;
    EffectiveOnly: WBOOLEAN;
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
  TOKEN_TYPE = [1 .. 2];
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
    GroupCount: DWORD;
    Groups    : ARRAY ANYSIZE_ARRAY OF SID_AND_ATTRIBUTES;
  END;


  PTOKEN_PRIVILEGES = UNTRACED REF TOKEN_PRIVILEGES;
  TOKEN_PRIVILEGES = RECORD
    PrivilegeCount: DWORD;
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
    DynamicCharged    : DWORD;
    DynamicAvailable  : DWORD;
    GroupCount        : DWORD;
    PrivilegeCount    : DWORD;
    ModifiedId        : LUID;
  END;


  PTOKEN_CONTROL = UNTRACED REF TOKEN_CONTROL;
  TOKEN_CONTROL = RECORD
    TokenId         : LUID;
    AuthenticationId: LUID;
    ModifiedId      : LUID;
    TokenSource     : TOKEN_SOURCE;
  END;


  SECURITY_INFORMATION = DWORD;
  PSECURITY_INFORMATION = UNTRACED REF DWORD;

CONST
  OWNER_SECURITY_INFORMATION: LONG = 16_00000001;
  GROUP_SECURITY_INFORMATION: LONG = 16_00000002;
  DACL_SECURITY_INFORMATION : LONG = 16_00000004;
  SACL_SECURITY_INFORMATION : LONG = 16_00000008;
  HEAP_SERIALIZE                   = 16_00000001;

TYPE
  PRTL_CRITICAL_SECTION_DEBUG = UNTRACED REF RTL_CRITICAL_SECTION_DEBUG;
  RTL_CRITICAL_SECTION_DEBUG = RECORD
    Type                 : WORD;
    CreatorBackTraceIndex: WORD;
    CriticalSection      : UNTRACED REF RTL_CRITICAL_SECTION;
    ProcessLocksList     : LIST_ENTRY;
    EntryCount           : DWORD;
    ContentionCount      : DWORD;
    Depth                : DWORD;
    OwnerBackTrace       : ARRAY [0 .. 4] OF PVOID;
  END;

CONST
  RTL_CRITSECT_TYPE = 0;
  RTL_RESOURCE_TYPE = 1;

(* NOTE: "PRTL_CRITICAL_SECTION" is declared as a Modula-3 opaque
   type and then immediately revealed so that "EnterCriticalSection()"
   and "LeaveCriticalSection()" won't require checks by the GC wrapper
   routines.  Clearly, if a Windows critical section is inside an M3
   traced ref (which moves!) the system is going to crash.  Hence, the
   extra GC check isn't needed. *)

TYPE   PRTL_CRITICAL_SECTION <: ADDRESS;
REVEAL PRTL_CRITICAL_SECTION = UNTRACED BRANDED "WinNT.PRTL_CRITICAL_SECTION"
                               REF RTL_CRITICAL_SECTION;

TYPE
  RTL_CRITICAL_SECTION = RECORD
    DebugInfo: PRTL_CRITICAL_SECTION_DEBUG;

    (*
    (* *)
    (*  The following three fields control entering and exiting the critical *)
    (*  section for the resource *)
    (* *)
    *)

    LockCount      : LONG;
    RecursionCount : LONG;
    OwningThread   : HANDLE;  (* from the thread's ClientId->UniqueThread *)
    LockSemaphore  : HANDLE;
    Reserved       : DWORD;
  END;

CONST
  DLL_PROCESS_ATTACH = 1;
  DLL_THREAD_ATTACH  = 2;
  DLL_THREAD_DETACH  = 3;
  DLL_PROCESS_DETACH = 0;

(* Defines for the READ flags for Eventlogging *)
CONST
  EVENTLOG_SEQUENTIAL_READ = 16_0001;
  EVENTLOG_SEEK_READ       = 16_0002;
  EVENTLOG_FORWARDS_READ   = 16_0004;
  EVENTLOG_BACKWARDS_READ  = 16_0008;

(* The types of events that can be logged. *)
CONST
  EVENTLOG_SUCCESS          = 16_0000;
  EVENTLOG_ERROR_TYPE       = 16_0001;
  EVENTLOG_WARNING_TYPE     = 16_0002;
  EVENTLOG_INFORMATION_TYPE = 16_0004;
  EVENTLOG_AUDIT_SUCCESS    = 16_0008;
  EVENTLOG_AUDIT_FAILURE    = 16_0010;

(* Defines for the WRITE flags used by Auditing for paired events *)
(* These are not implemented in Product 1 *)
CONST
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
    Length       : DWORD;     (* Length of full record *)
    Reserved     : DWORD;     (* Used by the service *)
    RecordNumber : DWORD;     (* Absolute record number *)
    TimeGenerated: DWORD;     (* Seconds since 1-1-1970 *)
    TimeWritten  : DWORD;     (* Seconds since 1-1-1970 *)
    EventID      : DWORD;
    EventType    : WORD;
    NumStrings   : WORD;
    EventCategory: WORD;
    ReservedFlags: WORD;      (* For use with paired events (auditing) *)
    ClosingRecordNumber: DWORD;  (* For use with paired events
    (auditing) *)
    StringOffset : DWORD;     (* Offset from beginning of record *)
    UserSidLength: DWORD;
    UserSidOffset: DWORD;
    DataLength   : DWORD;
    DataOffset   : DWORD;     (* Offset from beginning of record *)
    (*
    (* *)
    (* Then follow: *)
    (* *)
    (* WCHAR SourceName[] *)
    (* WCHAR Computername[] *)
    (* SID   UserSid *)
    (* WCHAR Strings[] *)
    (* BYTE  Data[] *)
    (* CHAR  Pad[] *)
    (* DWORD Length; *)
    (* *)
    *)
  END;

CONST
  DBG_CONTINUE             : DWORD = 16_00010002;
  DBG_TERMINATE_THREAD     : DWORD = 16_40010003;
  DBG_TERMINATE_PROCESS    : DWORD = 16_40010004;
  DBG_CONTROL_C            : DWORD = 16_40010005;
  DBG_EXCEPTION_NOT_HANDLED: DWORD = 16_80010001;

(* Registry Specific Access Rights. *)

CONST
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

CONST
  REG_OPTION_RESERVED: LONG = 16_00000000; (* Parameter is reserved *)

  REG_OPTION_NON_VOLATILE: LONG = 16_00000000; (* Key is preserved when
                                                  system is rebooted *)

  REG_OPTION_VOLATILE: LONG = 16_00000001; (* Key is not preserved when
                                              system is rebooted *)

  REG_OPTION_CREATE_LINK: LONG = 16_00000002; (* Created key is a symbolic
                                                 link *)

(* Key creation/open disposition *)

CONST
  REG_CREATED_NEW_KEY: LONG = 16_00000001; (* New Registry Key created *)
  REG_OPENED_EXISTING_KEY: LONG = 16_00000002; (* Existing Key opened *)

(* Key restore flags *)

CONST
  REG_WHOLE_HIVE_VOLATILE: LONG = 16_00000001; (* Restore whole hive
                                                  volatile *)

(* Notify filter values *)

CONST
  REG_NOTIFY_CHANGE_NAME: LONG = 16_00000001; (* Create or delete
                                                 (child) *)
  REG_NOTIFY_CHANGE_ATTRIBUTES: LONG = 16_00000002;
  REG_NOTIFY_CHANGE_LAST_SET  : LONG = 16_00000004; (* time stamp *)
  REG_NOTIFY_CHANGE_SECURITY  : LONG = 16_00000008;

  REG_LEGAL_CHANGE_FILTER = Or(REG_NOTIFY_CHANGE_NAME,
                               Or(REG_NOTIFY_CHANGE_ATTRIBUTES,
                                  Or(REG_NOTIFY_CHANGE_LAST_SET,
                                     REG_NOTIFY_CHANGE_SECURITY)));


(* Predefined Value Types. *)

CONST
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

CONST
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

CONST
  SERVICE_BOOT_START   = 16_00000000;
  SERVICE_SYSTEM_START = 16_00000001;
  SERVICE_AUTO_START   = 16_00000002;
  SERVICE_DEMAND_START = 16_00000003;
  SERVICE_DISABLED     = 16_00000004;

(* Error control type *)

CONST
  SERVICE_ERROR_NORMAL   = 16_00000001;
  SERVICE_ERROR_SEVERE   = 16_00000002;
  SERVICE_ERROR_CRITICAL = 16_00000003;

(* Define the registry driver node enumerations *)

TYPE
  (* !!!  This is scrambled enum *)
  SERVICE_NODE_TYPE = INT;

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
  TAPE_ERASE_SHORT      : LONG = 0;
  TAPE_ERASE_SHORT_IMMED: LONG = 1;
  TAPE_ERASE_LONG       : LONG = 2;
  TAPE_ERASE_LONG_IMMED : LONG = 3;

TYPE
  TAPE_ERASE = RECORD Type: DWORD;  END;
  PTAPE_ERASE = UNTRACED REF TAPE_ERASE;

(* IOCTL_TAPE_PREPARE definitions *)

CONST
  TAPE_LOAD         : LONG = 0;
  TAPE_LOAD_IMMED   : LONG = 1;
  TAPE_UNLOAD       : LONG = 2;
  TAPE_UNLOAD_IMMED : LONG = 3;
  TAPE_TENSION      : LONG = 4;
  TAPE_TENSION_IMMED: LONG = 5;
  TAPE_LOCK         : LONG = 6;
  TAPE_UNLOCK       : LONG = 7;

TYPE
  TAPE_PREPARE = RECORD Operation: DWORD;  END;
  PTAPE_PREPARE = UNTRACED REF TAPE_PREPARE;

(* IOCTL_TAPE_WRITE_MARKS definitions *)

CONST
  TAPE_SETMARKS             : LONG = 0;
  TAPE_SETMARKS_IMMED       : LONG = 1;
  TAPE_FILEMARKS            : LONG = 2;
  TAPE_FILEMARKS_IMMED      : LONG = 3;
  TAPE_SHORT_FILEMARKS      : LONG = 4;
  TAPE_SHORT_FILEMARKS_IMMED: LONG = 5;
  TAPE_LONG_FILEMARKS       : LONG = 6;
  TAPE_LONG_FILEMARKS_IMMED : LONG = 7;

TYPE
  PTAPE_WRITE_MARKS = UNTRACED REF TAPE_WRITE_MARKS;
  TAPE_WRITE_MARKS = RECORD
    Type : DWORD;
    Count: DWORD;
  END;

(* IOCTL_TAPE_GET_POSITION definitions *)

CONST
  TAPE_ABSOLUTE_POSITION: LONG = 0;
  TAPE_LOGICAL_POSITION : LONG = 1;

TYPE
  PTAPE_GET_POSITION = UNTRACED REF TAPE_GET_POSITION;
  TAPE_GET_POSITION = RECORD
    Type      : DWORD;
    Partition : DWORD;
    OffsetLow : DWORD;
    OffsetHigh: DWORD;
  END;

(* IOCTL_TAPE_SET_POSITION definitions *)

CONST
  TAPE_REWIND               : LONG = 0;
  TAPE_REWIND_IMMED         : LONG = 1;
  TAPE_ABSOLUTE_BLOCK       : LONG = 2;
  TAPE_ABSOLUTE_BLOCK_IMMED : LONG = 3;
  TAPE_LOGICAL_BLOCK        : LONG = 4;
  TAPE_LOGICAL_BLOCK_IMMED  : LONG = 5;
  TAPE_SPACE_END_OF_DATA    : LONG = 6;
  TAPE_SPACE_RELATIVE_BLOCKS: LONG = 7;
  TAPE_SPACE_FILEMARKS      : LONG = 8;
  TAPE_SPACE_SEQUENTIAL_FMKS: LONG = 9;
  TAPE_SPACE_SETMARKS       : LONG = 10;
  TAPE_SPACE_SEQUENTIAL_SMKS: LONG = 11;

TYPE
  PTAPE_SET_POSITION = UNTRACED REF TAPE_SET_POSITION;
  TAPE_SET_POSITION = RECORD
    Method    : DWORD;
    Partition : DWORD;
    OffsetLow : DWORD;
    OffsetHigh: DWORD;
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

CONST
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
    DefaultBlockSize     : DWORD;
    MaximumBlockSize     : DWORD;
    MinimumBlockSize     : DWORD;
    MaximumPartitionCount: DWORD;
    FeaturesLow          : DWORD;
    FeaturesHigh         : DWORD;
  END;

(* IOCTL_TAPE_SET_DRIVE_PARAMETERS definitions *)

TYPE
  PTAPE_SET_DRIVE_PARAMETERS = UNTRACED REF TAPE_SET_DRIVE_PARAMETERS;
  TAPE_SET_DRIVE_PARAMETERS = RECORD
    ECC           : WBOOLEAN;
    Compression   : WBOOLEAN;
    DataPadding   : WBOOLEAN;
    ReportSetmarks: WBOOLEAN;
  END;

(* IOCTL_TAPE_GET_MEDIA_PARAMETERS definitions *)

TYPE
  PTAPE_GET_MEDIA_PARAMETERS = UNTRACED REF TAPE_GET_MEDIA_PARAMETERS;
  TAPE_GET_MEDIA_PARAMETERS = RECORD
    CapacityLow   : DWORD;
    CapacityHigh  : DWORD;
    RemainingLow  : DWORD;
    RemainingHigh : DWORD;
    BlockSize     : DWORD;
    PartitionCount: DWORD;
    WriteProtected: WBOOLEAN;
  END;

(* IOCTL_TAPE_SET_MEDIA_PARAMETERS definitions *)

TYPE
  TAPE_SET_MEDIA_PARAMETERS = RECORD BlockSize: DWORD;  END;
  PTAPE_SET_MEDIA_PARAMETERS = UNTRACED REF TAPE_SET_MEDIA_PARAMETERS;

(* IOCTL_TAPE_CREATE_PARTITION definitions *)

CONST
  TAPE_FIXED_PARTITIONS    : LONG = 0;
  TAPE_SELECT_PARTITIONS   : LONG = 1;
  TAPE_INITIATOR_PARTITIONS: LONG = 2;

TYPE
  PTAPE_CREATE_PARTITION = UNTRACED REF TAPE_CREATE_PARTITION;
  TAPE_CREATE_PARTITION = RECORD
    Method: DWORD;
    Count : DWORD;
    Size  : DWORD;
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
    e_magic: WORD;            (* Magic number *)

    e_cp      : WORD;         (* Pages in file *)
    e_crlc    : WORD;         (* Relocations *)
    e_cparhdr : WORD;         (* Size of header in paragraphs *)
    e_minalloc: WORD;         (* Minimum extra paragraphs needed *)
    e_maxalloc: WORD;         (* Maximum extra paragraphs needed *)
    e_ss      : WORD;         (* Initial (relative) SS value *)
    e_sp      : WORD;         (* Initial SP value *)
    e_csum    : WORD;         (* Checksum *)
    e_ip      : WORD;         (* Initial IP value *)
    e_cs      : WORD;         (* Initial (relative) CS value *)
    e_lfarlc  : WORD;         (* File address of relocation table *)
    e_ovno    : WORD;         (* Overlay number *)
    e_res: ARRAY [0 .. 3] OF WORD;  (* Reserved words *)
    e_oemid  : WORD;          (* OEM identifier (for e_oeminfo) *)
    e_oeminfo: WORD;          (* OEM information; e_oemid specific *)
    e_res2: ARRAY [0 .. 9] OF WORD;  (* Reserved words *)
    e_lfanew: LONG;           (* File address of new exe header *)
  END;

  PIMAGE_OS2_HEADER = UNTRACED REF IMAGE_OS2_HEADER;
  IMAGE_OS2_HEADER = RECORD   (* OS/2 .EXE header *)
    ne_magic       : WORD;    (* Magic number *)
    ne_ver         : CHAR;    (* Version number *)
    ne_rev         : CHAR;    (* Revision number *)
    ne_enttab      : WORD;    (* Offset of Entry Table *)
    ne_cbenttab    : WORD;    (* Number of bytes in Entry Table *)
    ne_crc         : LONG;    (* Checksum of whole file *)
    ne_flags       : WORD;    (* Flag word *)
    ne_autodata    : WORD;    (* Automatic data segment number *)
    ne_heap        : WORD;    (* Initial heap allocation *)
    ne_stack       : WORD;    (* Initial stack allocation *)
    ne_csip        : LONG;    (* Initial CS:IP setting *)
    ne_sssp        : LONG;    (* Initial SS:SP setting *)
    ne_cseg        : WORD;    (* Count of file segments *)
    ne_cmod        : WORD;    (* Entries in Module Reference Table *)
    ne_cbnrestab   : WORD;    (* Size of non-resident name table *)
    ne_segtab      : WORD;    (* Offset of Segment Table *)
    ne_rsrctab     : WORD;    (* Offset of Resource Table *)
    ne_restab      : WORD;    (* Offset of resident name table *)
    ne_modtab      : WORD;    (* Offset of Module Reference Table *)
    ne_imptab      : WORD;    (* Offset of Imported Names Table *)
    ne_nrestab     : LONG;    (* Offset of Non-resident Names Table *)
    ne_cmovent     : WORD;    (* Count of movable entries *)
    ne_align       : WORD;    (* Segment alignment shift count *)
    ne_cres        : WORD;    (* Count of resource segments *)
    ne_exetyp      : BYTE;    (* Target Operating system *)
    ne_flagsothers : BYTE;    (* Other .EXE flags *)
    ne_pretthunks  : WORD;    (* offset to return thunks *)
    ne_psegrefbytes: WORD;    (* offset to segment ref.  bytes *)
    ne_swaparea    : WORD;    (* Minimum code swap area size *)
    ne_expver      : WORD;    (* Expected Windows version number *)
  END;

(* File header format. *)

TYPE
  PIMAGE_FILE_HEADER = UNTRACED REF IMAGE_FILE_HEADER;
  IMAGE_FILE_HEADER = RECORD
    Machine             : WORD;
    NumberOfSections    : WORD;
    TimeDateStamp       : DWORD;
    PointerToSymbolTable: DWORD;
    NumberOfSymbols     : DWORD;
    SizeOfOptionalHeader: WORD;
    Characteristics     : WORD;
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

(* Directory format. *)

TYPE
  PIMAGE_DATA_DIRECTORY = UNTRACED REF IMAGE_DATA_DIRECTORY;
  IMAGE_DATA_DIRECTORY = RECORD
    VirtualAddress: DWORD;
    Size          : DWORD;
  END;

CONST IMAGE_NUMBEROF_DIRECTORY_ENTRIES = 16;

(* Optional header format. *)

TYPE
  PIMAGE_OPTIONAL_HEADER = UNTRACED REF IMAGE_OPTIONAL_HEADER;
  IMAGE_OPTIONAL_HEADER = RECORD
    (*
    (* Standard fields. *)
    *)

    Magic                  : WORD;
    MajorLinkerVersion     : BYTE;
    MinorLinkerVersion     : BYTE;
    SizeOfCode             : DWORD;
    SizeOfInitializedData  : DWORD;
    SizeOfUninitializedData: DWORD;
    AddressOfEntryPoint    : DWORD;
    BaseOfCode             : DWORD;
    BaseOfData             : DWORD;

    (*
    (* NT additional fields. *)
    *)

    ImageBase                  : DWORD;
    SectionAlignment           : DWORD;
    FileAlignment              : DWORD;
    MajorOperatingSystemVersion: WORD;
    MinorOperatingSystemVersion: WORD;
    MajorImageVersion          : WORD;
    MinorImageVersion          : WORD;
    MajorSubsystemVersion      : WORD;
    MinorSubsystemVersion      : WORD;
    Reserved1                  : DWORD;
    SizeOfImage                : DWORD;
    SizeOfHeaders              : DWORD;
    CheckSum                   : DWORD;
    Subsystem                  : WORD;
    DllCharacteristics         : WORD;
    SizeOfStackReserve         : DWORD;
    SizeOfStackCommit          : DWORD;
    SizeOfHeapReserve          : DWORD;
    SizeOfHeapCommit           : DWORD;
    AddressOfTlsIndex          : DWORD;
    NumberOfRvaAndSizes        : DWORD;
    DataDirectory        : ARRAY [0 .. IMAGE_NUMBEROF_DIRECTORY_ENTRIES - 1] OF
                             IMAGE_DATA_DIRECTORY;
  END;

CONST
 IMAGE_SIZEOF_STD_OPTIONAL_HEADER  =    28;
 IMAGE_SIZEOF_NT_OPTIONAL_HEADER   =   224;

TYPE
  PIMAGE_NT_HEADERS = UNTRACED REF IMAGE_NT_HEADERS;
  IMAGE_NT_HEADERS = RECORD
    Signature     : DWORD;
    FileHeader    : IMAGE_FILE_HEADER;
    OptionalHeader: IMAGE_OPTIONAL_HEADER;
  END;

(*!!!???
#define IMAGE_FIRST_SECTION( ntheader ) ((PIMAGE_SECTION_HEADER)        \
    ((DWORD)ntheader +                                                  \
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

CONST
  IMAGE_LIBRARY_PROCESS_INIT = 1; (* Dll has a process initialization
                                     routine. *)
  IMAGE_LIBRARY_PROCESS_TERM = 2; (* Dll has a thread termination
                                     routine. *)
  IMAGE_LIBRARY_THREAD_INIT = 4; (* Dll has a thread initialization
                                    routine. *)
  IMAGE_LIBRARY_THREAD_TERM = 8; (* Dll has a thread termination
                                    routine. *)

(* Directory Entries *)

CONST
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
    Name: ARRAY [0 .. IMAGE_SIZEOF_SHORT_NAME - 1] OF BYTE;
    Misc: DWORD;
      (*???
          union {
                  DWORD   PhysicalAddress;
                  DWORD   VirtualSize;
          } Misc;
      *)
    VirtualAddress      : DWORD;
    SizeOfRawData       : DWORD;
    PointerToRawData    : DWORD;
    PointerToRelocations: DWORD;
    PointerToLinenumbers: DWORD;
    NumberOfRelocations : WORD;
    NumberOfLinenumbers : WORD;
    Characteristics     : DWORD;
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
    N: ARRAY [0 .. 7] OF BYTE;
    (*???
    union {
        BYTE    ShortName[8];
        struct {
            DWORD   Short;     (* if 0, use LongName *)
            DWORD   Long;      (* offset into string table *)
        } Name;
        PBYTE   LongName[2];
    } N;
    *)
    Value              : DWORD;
    SectionNumber      : SHORT;
    Type               : WORD;
    StorageClass       : BYTE;
    NumberOfAuxSymbols : BYTE;
  END;

CONST IMAGE_SIZEOF_SYMBOL = 18;

(* Section values. *)
(* Symbols have a section number of the section in which they are *)
(* defined. Otherwise, section numbers have the following meanings: *)

CONST
  IMAGE_SYM_UNDEFINED: SHORT = 0; (* Symbol is undefined or is common. *)
  IMAGE_SYM_ABSOLUTE : SHORT = -1; (* Symbol is an absolute value. *)
  IMAGE_SYM_DEBUG    : SHORT = -2; (* Symbol is a special debug item. *)

(* Type (fundamental) values. *)

CONST
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

CONST
  IMAGE_SYM_DTYPE_NULL     = 0; (* no derived type. *)
  IMAGE_SYM_DTYPE_POINTER  = 1; (* pointer. *)
  IMAGE_SYM_DTYPE_FUNCTION = 2; (* function. *)
  IMAGE_SYM_DTYPE_ARRAY    = 3; (* array. *)

(* Storage classes. *)

CONST
  IMAGE_SYM_CLASS_END_OF_FUNCTION (*???: BYTE*) = -1;
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

CONST
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
PROCEDURE BTYPE (x: WORD): WORD;

(* Is x a pointer? *)

<* INLINE *>
PROCEDURE ISPTR (x: WORD): BOOLEAN;

(* Is x a function? *)
<* INLINE *>
PROCEDURE ISFCN (x: WORD): BOOLEAN;

(* Is x an array? *)

<* INLINE *>
PROCEDURE ISARY (x: WORD): BOOLEAN;

(* Is x a structure, union, or enumeration TAG? *)
<* INLINE *>
PROCEDURE ISTAG (x: WORD): BOOLEAN;

<* INLINE *>
PROCEDURE INCREF (x: WORD): WORD;

<* INLINE *>
PROCEDURE DECREF (x: WORD): WORD;

(* Auxiliary entry format. *)

TYPE
  PIMAGE_AUX_SYMBOL = <* UNALIGNED *> UNTRACED REF IMAGE_AUX_SYMBOL;
  IMAGE_AUX_SYMBOL = RECORD
    Sym: RECORD
      TagIndex: DWORD;
    END;
      (*
      struct {
          DWORD    TagIndex;             (* struct, union, or enum tag index *)
          union {
              struct {
                  WORD    Linenumber;    (* declaration line number *)
                  WORD    Size;          (* size of struct, union, or enum *)
              } LnSz;
             DWORD    TotalSize;
          } Misc;
          union {
              struct {                   (* if ISFCN, tag, or .bb *)
                  DWORD    PointerToLinenumber;
                  DWORD    PointerToNextFunction;
              } Function;
              struct {                   (* if ISARY, up to 4 dimen. *)
                  WORD     Dimension[4];
              } Array;
          } FcnAry;
          WORD    TvIndex;               (* tv index *)
      } Sym;
      *)

    File: RECORD
      Name: ARRAY [0 .. IMAGE_SIZEOF_SYMBOL - 1] OF BYTE;
    END;

    Section: RECORD
      Length: DWORD;  (* section length *)
      NumberOfRelocations: WORD;  (* number of relocation entries *)
      NumberOfLinenumbers: WORD;   (* number of line numbers *)
      CheckSum           : DWORD;  (* checksum for communal *)
      Number   : SHORT;  (* section number to associate with *)
      Selection: BYTE;   (* communal selection type *)
    END;
  END;

CONST IMAGE_SIZEOF_AUX_SYMBOL = 18;

(* Communal selection types. *)

CONST
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
    VirtualAddress  : DWORD;
    SymbolTableIndex: DWORD;
    Type            : WORD;
  END;

CONST IMAGE_SIZEOF_RELOCATION = 10;

(* I860 relocation types. *)

CONST
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
    VirtualAddress: DWORD;
    SizeOfBlock   : DWORD;
     (* (* WORD TypeOffset[1]; *) *)
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
    Type: DWORD;
    (*
        union {
            DWORD   SymbolTableIndex; (* Symbol table index of function name if Linenumber is 0. *)
            DWORD   VirtualAddress; (* Virtual address of line number. *)
        } Type;
      *)
    Linenumber: WORD;         (* Line number. *)
  END;

CONST IMAGE_SIZEOF_LINENUMBER = 6;

(* Archive format. *)

CONST IMAGE_ARCHIVE_START_SIZE = 8;
VAR                             (* !!!  CONST *)
  IMAGE_ARCHIVE_START        : Ctypes.char_star; (* := TtoS("!<arch>\n")*)
  IMAGE_ARCHIVE_END          : Ctypes.char_star; (* := TtoS("`\n")*)
  IMAGE_ARCHIVE_PAD          : Ctypes.char_star; (* := TtoS("\n")*)
  IMAGE_ARCHIVE_LINKER_MEMBER: Ctypes.char_star; (* := TtoS("/ ")*)
  IMAGE_ARCHIVE_LONGNAMES_MEMBER: Ctypes.char_star; (* := TtoS("// ")*)

TYPE
  PIMAGE_ARCHIVE_MEMBER_HEADER = UNTRACED REF IMAGE_ARCHIVE_MEMBER_HEADER;
  IMAGE_ARCHIVE_MEMBER_HEADER = RECORD
    Name     : ARRAY [0 .. 15] OF BYTE;  (* member name - `/' terminated. *)
    Date     : ARRAY [0 .. 11] OF BYTE;  (* member date - decimal secs since 1970 *)
    UserID   : ARRAY [0 .. 5]  OF BYTE;  (* member user id - decimal. *)
    GroupID  : ARRAY [0 .. 5]  OF BYTE;  (* member group id - decimal. *)
    Mode     : ARRAY [0 .. 7]  OF BYTE;  (* member mode - octal. *)
    Size     : ARRAY [0 .. 9]  OF BYTE;  (* member size - decimal. *)
    EndHeader: ARRAY [0 .. 1]  OF BYTE;  (* String to end header. *)
  END;

CONST IMAGE_SIZEOF_ARCHIVE_MEMBER_HDR = 60;

(* DLL support. *)

(* Export Format *)

TYPE
  PIMAGE_EXPORT_DIRECTORY = UNTRACED REF IMAGE_EXPORT_DIRECTORY;
  IMAGE_EXPORT_DIRECTORY = RECORD
    Characteristics      : DWORD;
    TimeDateStamp        : DWORD;
    MajorVersion         : WORD;
    MinorVersion         : WORD;
    Name                 : DWORD;
    Base                 : DWORD;
    NumberOfFunctions    : DWORD;
    NumberOfNames        : DWORD;
    AddressOfFunctions   : UNTRACED REF PDWORD;
    AddressOfNames       : UNTRACED REF PDWORD;
    AddressOfNameOrdinals: UNTRACED REF PWORD;
  END;

(* Import Format *)

TYPE
  PIMAGE_IMPORT_BY_NAME = UNTRACED REF IMAGE_IMPORT_BY_NAME;
  IMAGE_IMPORT_BY_NAME = RECORD
    Hint  :  WORD;
    Name  : ARRAY [0 .. 1 - 1] OF  BYTE;
  END;

  PIMAGE_THUNK_DATA = UNTRACED REF IMAGE_THUNK_DATA;
  IMAGE_THUNK_DATA = RECORD
    u1: DWORD;
    (*
    union {
    PDWORD Function;
    DWORD Ordinal;
    PIMAGE_IMPORT_BY_NAME AddressOfData;
    } u1;
    *)
  END;

CONST
 IMAGE_ORDINAL_FLAG = 16_80000000;

TYPE
<* INLINE *>
PROCEDURE IMAGE_SNAP_BY_ORDINAL (Ordinal: WORD): BOOLEAN;

<* INLINE *>
PROCEDURE IMAGE_ORDINAL (Ordinal: WORD): WORD;

TYPE
  PIMAGE_IMPORT_DESCRIPTOR = UNTRACED REF IMAGE_IMPORT_DESCRIPTOR;
  IMAGE_IMPORT_DESCRIPTOR = RECORD
    Characteristics: DWORD;
    TimeDateStamp  : DWORD;
    MajorVersion   : WORD;
    MinorVersion   : WORD;
    Name           : DWORD;
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
    Characteristics     : DWORD;
    TimeDateStamp       : DWORD;
    MajorVersion        : WORD;
    MinorVersion        : WORD;
    NumberOfNamedEntries: WORD;
    NumberOfIdEntries   : WORD;
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
    Name        : DWORD;
    OffsetToData: DWORD;
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
    Length    : WORD;
    NameString: ARRAY [0 .. 0] OF CHAR;
  END;

  PIMAGE_RESOURCE_DIR_STRING_U = UNTRACED REF IMAGE_RESOURCE_DIR_STRING_U;
  IMAGE_RESOURCE_DIR_STRING_U = RECORD
    Length    : WORD;
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
    OffsetToData : DWORD;
    Size         : DWORD;
    CodePage     : DWORD;
    Reserved     : DWORD;
  END;

(* Debug Format *)

TYPE
  PIMAGE_DEBUG_DIRECTORY = UNTRACED REF IMAGE_DEBUG_DIRECTORY;
  IMAGE_DEBUG_DIRECTORY = RECORD
    Characteristics : DWORD;
    TimeDateStamp   : DWORD;
    MajorVersion    : WORD;
    MinorVersion    : WORD;
    Type            : DWORD;
    SizeOfData      : DWORD;
    AddressOfRawData: DWORD;
    PointerToRawData: DWORD;
  END;

CONST
  IMAGE_DEBUG_TYPE_UNKNOWN  = 0;
  IMAGE_DEBUG_TYPE_COFF     = 1;
  IMAGE_DEBUG_TYPE_CODEVIEW = 2;

TYPE
  PIMAGE_DEBUG_INFO = UNTRACED REF IMAGE_DEBUG_INFO;
  IMAGE_DEBUG_INFO = RECORD
    NumberOfSymbols     : DWORD;
    LvaToFirstSymbol    : DWORD;
    NumberOfLinenumbers : DWORD;
    LvaToFirstLinenumber: DWORD;
    RvaToFirstByteOfCode: DWORD;
    RvaToLastByteOfCode : DWORD;
    RvaToFirstByteOfData: DWORD;
    RvaToLastByteOfData : DWORD;
  END;

(* End Image Format *)

END WinNT. 
