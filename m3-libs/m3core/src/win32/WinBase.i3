(* Copyright (C) 1994, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* by Stephen Harrison                                       *)
(*                                                           *)
(* Last modified on Thu May 15 12:09:35 PDT 1997 by heydon   *)
(*      modified on Mon Oct  9 10:03:11 PDT 1995 by najork   *)
(*      modified on Wed Apr 12 14:38:08 PDT 1995 by kalsow   *)
(*      modified on Thu Jul  1 15:56:26 PDT 1993 by mcjones  *)
(*      modified on Wed Feb 10 19:52:26 PST 1993 by harrison *)

(* I added the constant STARTF_USESTDHANDLES and the hStdInput,
   hStdOutput, and hStdError fields to _STARTUPINFOA and _STARTUPINFOW
   record types. PMcJ 7/1/93 *)

INTERFACE WinBase;

(* Corresponds to build version 0001 of "winbase.h".
   See that file for details.

   This module defines the 32-Bit Windows Base APIs.
*)

IMPORT WinNT;

FROM Word IMPORT Or, Shift;
FROM WinBaseTypes IMPORT UINT8, UINT16, UINT32, INT32, PUINT16, PUINT32,
  PINT32, PVOID, PBOOL, WCHAR, PSTR, PCSTR, PWSTR, PCWSTR, PTSTR, PCTSTR,
  BOOL, PCVOID, SIZE_T, PSIZE_T, PUINT8;
FROM WinDef IMPORT HGLOBAL, HINSTANCE, FARPROC, HLOCAL, HWND, HRSRC, ATOM,
  HFILE, MAX_PATH, HMODULE;
FROM WinNT IMPORT PSID, PACL, PLUID, LARGE_INTEGER, HANDLE, PHANDLE;
FROM Ctypes IMPORT char;

(*
 * Compatibility macros
 *)

CONST
  INVALID_HANDLE_VALUE (*???: HANDLE*) = -1;
  FILE_BEGIN                           = 0;
  FILE_CURRENT                         = 1;
  FILE_END                             = 2;

  WAIT_OBJECT_0 = ((WinNT.STATUS_WAIT_0) + 0);

  WAIT_ABANDONED   = ((WinNT.STATUS_ABANDONED_WAIT_0) + 0);
  WAIT_ABANDONED_0 = ((WinNT.STATUS_ABANDONED_WAIT_0) + 0);

  WAIT_TIMEOUT                    = WinNT.STATUS_TIMEOUT;
  WAIT_IO_COMPLETION              = WinNT.STATUS_USER_APC;
  STILL_ACTIVE                    = WinNT.STATUS_PENDING;
  EXCEPTION_ACCESS_VIOLATION      = WinNT.STATUS_ACCESS_VIOLATION;
  EXCEPTION_DATATYPE_MISALIGNMENT = WinNT.STATUS_DATATYPE_MISALIGNMENT;
  EXCEPTION_BREAKPOINT            = WinNT.STATUS_BREAKPOINT;
  EXCEPTION_SINGLE_STEP           = WinNT.STATUS_SINGLE_STEP;
  EXCEPTION_ARRAY_BOUNDS_EXCEEDED = WinNT.STATUS_ARRAY_BOUNDS_EXCEEDED;
  EXCEPTION_FLT_DENORMAL_OPERAND  = WinNT.STATUS_FLOAT_DENORMAL_OPERAND;
  EXCEPTION_FLT_DIVIDE_BY_ZERO    = WinNT.STATUS_FLOAT_DIVIDE_BY_ZERO;
  EXCEPTION_FLT_INEXACT_RESULT    = WinNT.STATUS_FLOAT_INEXACT_RESULT;
  EXCEPTION_FLT_INVALID_OPERATION = WinNT.STATUS_FLOAT_INVALID_OPERATION;
  EXCEPTION_FLT_OVERFLOW          = WinNT.STATUS_FLOAT_OVERFLOW;
  EXCEPTION_FLT_STACK_CHECK       = WinNT.STATUS_FLOAT_STACK_CHECK;
  EXCEPTION_FLT_UNDERFLOW         = WinNT.STATUS_FLOAT_UNDERFLOW;
  EXCEPTION_INT_DIVIDE_BY_ZERO    = WinNT.STATUS_INTEGER_DIVIDE_BY_ZERO;
  EXCEPTION_INT_OVERFLOW          = WinNT.STATUS_INTEGER_OVERFLOW;
  EXCEPTION_PRIV_INSTRUCTION      = WinNT.STATUS_PRIVILEGED_INSTRUCTION;
  CONTROL_C_EXIT                  = WinNT.STATUS_CONTROL_C_EXIT;

(* File creation flags must start at the high end since they *)
(* are combined with the attributes *)

  FILE_FLAG_WRITE_THROUGH    = 16_80000000;
  FILE_FLAG_OVERLAPPED       = 16_40000000;
  FILE_FLAG_NO_BUFFERING     = 16_20000000;
  FILE_FLAG_RANDOM_ACCESS    = 16_10000000;
  FILE_FLAG_SEQUENTIAL_SCAN  = 16_08000000;
  FILE_FLAG_DELETE_ON_CLOSE  = 16_04000000;
  FILE_FLAG_BACKUP_SEMANTICS = 16_02000000;
  FILE_FLAG_POSIX_SEMANTICS  = 16_01000000;

  CREATE_NEW        = 1;
  CREATE_ALWAYS     = 2;
  OPEN_EXISTING     = 3;
  OPEN_ALWAYS       = 4;
  TRUNCATE_EXISTING = 5;

(* Define the NamedPipe definitions *)


(* Define the dwOpenMode values for CreateNamedPipe *)

  PIPE_ACCESS_INBOUND  = 16_00000001;
  PIPE_ACCESS_OUTBOUND = 16_00000002;
  PIPE_ACCESS_DUPLEX   = 16_00000003;

(* Define the Named Pipe End flags for GetNamedPipeInfo *)

  PIPE_CLIENT_END = 16_00000000;
  PIPE_SERVER_END = 16_00000001;

(* Define the dwPipeMode values for CreateNamedPipe *)

  PIPE_WAIT             = 16_00000000;
  PIPE_NOWAIT           = 16_00000001;
  PIPE_READMODE_BYTE    = 16_00000000;
  PIPE_READMODE_MESSAGE = 16_00000002;
  PIPE_TYPE_BYTE        = 16_00000000;
  PIPE_TYPE_MESSAGE     = 16_00000004;

(* Define the well known values for CreateNamedPipe nMaxInstances *)

  PIPE_UNLIMITED_INSTANCES = 255;

(* Define the Security Quality of Service bits to be passed *)
(* into CreateFile *)

  SECURITY_ANONYMOUS      = Shift(WinNT.SecurityAnonymous, 16);
  SECURITY_IDENTIFICATION = Shift(WinNT.SecurityIdentification, 16);
  SECURITY_IMPERSONATION  = Shift(WinNT.SecurityImpersonation, 16);
  SECURITY_DELEGATION     = Shift(WinNT.SecurityDelegation, 16);

  SECURITY_CONTEXT_TRACKING = 16_00040000;
  SECURITY_EFFECTIVE_ONLY   = 16_00080000;

  SECURITY_SQOS_PRESENT     = 16_00100000;
  SECURITY_VALID_SQOS_FLAGS = 16_001F0000;


(* Special values for mailslot information. *)

(* Special value for NextMessageSize to indicate that there is no next *)
(* message. *)

  MAILSLOT_NO_MESSAGE: INT32 = -1;

(* Special value for read timeout to indicate that mailslot reads should *)
(* never timeout. *)

  MAILSLOT_WAIT_FOREVER: INT32 = -1;

(* File structures *)

TYPE
  POVERLAPPED = UNTRACED REF OVERLAPPED;
  LPOVERLAPPED = POVERLAPPED; (* compat *)
  OVERLAPPED = RECORD
    Internal    : UINT32;
    InternalHigh: UINT32;
    Offset      : UINT32;
    OffsetHigh  : UINT32;
    hEvent      : HANDLE;
  END;

  PSECURITY_ATTRIBUTES = UNTRACED REF SECURITY_ATTRIBUTES;
  LPSECURITY_ATTRIBUTES = PSECURITY_ATTRIBUTES; (* compat *)
  SECURITY_ATTRIBUTES = RECORD
    nLength             : UINT32;
    lpSecurityDescriptor: PVOID;
    bInheritHandle      : BOOL;
  END;

  PPROCESS_INFORMATION = UNTRACED REF PROCESS_INFORMATION;
  LPPROCESS_INFORMATION = PPROCESS_INFORMATION; (* compat *)
  PROCESS_INFORMATION = RECORD
    hProcess   : HANDLE;
    hThread    : HANDLE;
    dwProcessId: UINT32;
    dwThreadId : UINT32;
  END;

(* File System time stamps are represented with the following structure: *)

  PFILETIME = UNTRACED REF FILETIME;
  LPFILETIME = PFILETIME; (* compat *)
  FILETIME = BITS 64 FOR RECORD
    dwLowDateTime : UINT32;
    dwHighDateTime: UINT32;
  END;

(* System time is represented with the following structure: *)

TYPE
  PSYSTEMTIME = UNTRACED REF SYSTEMTIME;
  LPSYSTEMTIME = PSYSTEMTIME; (* compat *)
  SYSTEMTIME = RECORD
    wYear        : UINT16;
    wMonth       : UINT16;
    wDayOfWeek   : UINT16;
    wDay         : UINT16;
    wHour        : UINT16;
    wMinute      : UINT16;
    wSecond      : UINT16;
    wMilliseconds: UINT16;
  END;

  PTHREAD_START_ROUTINE = <*WINAPI*> PROCEDURE
                             (lpThreadParameter: PVOID): UINT32;

  LPTHREAD_START_ROUTINE = PTHREAD_START_ROUTINE; (* compat *)

  CRITICAL_SECTION = WinNT.RTL_CRITICAL_SECTION;
  PCRITICAL_SECTION = WinNT.PRTL_CRITICAL_SECTION;
  LPCRITICAL_SECTION = PCRITICAL_SECTION; (* compat *)

  CRITICAL_SECTION_DEBUG = WinNT.RTL_CRITICAL_SECTION_DEBUG;
  PCRITICAL_SECTION_DEBUG = WinNT.PRTL_CRITICAL_SECTION_DEBUG;
  LPCRITICAL_SECTION_DEBUG = PCRITICAL_SECTION_DEBUG; (* compat *)

  PLDT_ENTRY = PVOID;
  LPLDT_ENTRY = PLDT_ENTRY; (* compat *)

CONST
  MUTEX_MODIFY_STATE = WinNT.MUTANT_QUERY_STATE;
  MUTEX_ALL_ACCESS   = WinNT.MUTANT_ALL_ACCESS;

(* Serial provider type. *)

  SP_SERIALCOMM: UINT32 = 16_00000001;

(* Provider SubTypes *)

  PST_UNSPECIFIED   : UINT32 = 16_00000000;
  PST_RS232         : UINT32 = 16_00000001;
  PST_PARALLELPORT  : UINT32 = 16_00000002;
  PST_RS422         : UINT32 = 16_00000003;
  PST_RS423         : UINT32 = 16_00000004;
  PST_RS449         : UINT32 = 16_00000005;
  PST_FAX           : UINT32 = 16_00000021;
  PST_SCANNER       : UINT32 = 16_00000022;
  PST_NETWORK_BRIDGE: UINT32 = 16_00000100;
  PST_LAT           : UINT32 = 16_00000101;
  PST_TCPIP_TELNET  : UINT32 = 16_00000102;
  PST_X25           : UINT32 = 16_00000103;


(* Provider capabilities flags. *)

  PCF_DTRDSR       : UINT32 = 16_0001;
  PCF_RTSCTS       : UINT32 = 16_0002;
  PCF_RLSD         : UINT32 = 16_0004;
  PCF_PARITY_CHECK : UINT32 = 16_0008;
  PCF_XONXOFF      : UINT32 = 16_0010;
  PCF_SETXCHAR     : UINT32 = 16_0020;
  PCF_TOTALTIMEOUTS: UINT32 = 16_0040;
  PCF_INTTIMEOUTS  : UINT32 = 16_0080;
  PCF_SPECIALCHARS : UINT32 = 16_0100;
  PCF_16BITMODE    : UINT32 = 16_0200;

(* Comm provider settable parameters. *)

  SP_PARITY      : UINT32 = 16_0001;
  SP_BAUD        : UINT32 = 16_0002;
  SP_DATABITS    : UINT32 = 16_0004;
  SP_STOPBITS    : UINT32 = 16_0008;
  SP_HANDSHAKING : UINT32 = 16_0010;
  SP_PARITY_CHECK: UINT32 = 16_0020;
  SP_RLSD        : UINT32 = 16_0040;

(* Settable baud rates in the provider. *)

  BAUD_075  : UINT32 = 16_00000001;
  BAUD_110  : UINT32 = 16_00000002;
  BAUD_134_5: UINT32 = 16_00000004;
  BAUD_150  : UINT32 = 16_00000008;
  BAUD_300  : UINT32 = 16_00000010;
  BAUD_600  : UINT32 = 16_00000020;
  BAUD_1200 : UINT32 = 16_00000040;
  BAUD_1800 : UINT32 = 16_00000080;
  BAUD_2400 : UINT32 = 16_00000100;
  BAUD_4800 : UINT32 = 16_00000200;
  BAUD_7200 : UINT32 = 16_00000400;
  BAUD_9600 : UINT32 = 16_00000800;
  BAUD_14400: UINT32 = 16_00001000;
  BAUD_19200: UINT32 = 16_00002000;
  BAUD_38400: UINT32 = 16_00004000;
  BAUD_56K  : UINT32 = 16_00008000;
  BAUD_128K : UINT32 = 16_00010000;
  BAUD_USER : UINT32 = 16_10000000;

(* Settable Data Bits *)

  DATABITS_5  : UINT16 = 16_0001;
  DATABITS_6  : UINT16 = 16_0002;
  DATABITS_7  : UINT16 = 16_0004;
  DATABITS_8  : UINT16 = 16_0008;
  DATABITS_16 : UINT16 = 16_0010;
  DATABITS_16X: UINT16 = 16_0020;

(* Settable Stop and Parity bits. *)

  STOPBITS_10 : UINT16 = 16_0001;
  STOPBITS_15 : UINT16 = 16_0002;
  STOPBITS_20 : UINT16 = 16_0004;
  PARITY_NONE : UINT16 = 16_0100;
  PARITY_ODD  : UINT16 = 16_0200;
  PARITY_EVEN : UINT16 = 16_0400;
  PARITY_MARK : UINT16 = 16_0800;
  PARITY_SPACE: UINT16 = 16_1000;

TYPE
  Int1  = BITS  1 FOR [0..1];
  Int2  = BITS  2 FOR [0..3];
  Int17 = BITS 17 FOR [0..16_1ffff];
  Int25 = BITS 25 FOR [0..16_1ffffff];

  PCOMMPROP = UNTRACED REF COMMPROP;
  LPCOMMPROP = PCOMMPROP; (* compat *)
  COMMPROP = RECORD
    wPacketLength      : UINT16;
    wPacketVersion     : UINT16;
    dwServiceMask      : UINT32;
    dwReserved1        : UINT32;
    dwMaxTxQueue       : UINT32;
    dwMaxRxQueue       : UINT32;
    dwMaxBaud          : UINT32;
    dwProvSubType      : UINT32;
    dwProvCapabilities : UINT32;
    dwSettableParams   : UINT32;
    dwSettableBaud     : UINT32;
    wSettableData      : UINT16;
    wSettableStopParity: UINT16;
    dwCurrentTxQueue   : UINT32;
    dwCurrentRxQueue   : UINT32;
    dwProvSpec1        : UINT32;
    dwProvSpec2        : UINT32;
    wcProvChar         : ARRAY [0 .. 0] OF WCHAR;
  END;

  PCOMSTAT = UNTRACED REF COMSTAT;
  LPCOMSTAT = PCOMSTAT; (* compat *)
  COMSTAT = RECORD
    fCtsHold : Int1;
    fDsrHold : Int1;
    fRlsdHold: Int1;
    fXoffHold: Int1;
    fXoffSent: Int1;
    fEof     : Int1;
    fTxim    : Int1;
    fReserved: Int25;
    cbInQue  : UINT32;
    cbOutQue : UINT32;
  END;

(* DTR Control Flow Values. *)
CONST
  DTR_CONTROL_DISABLE   = 16_00;
  DTR_CONTROL_ENABLE    = 16_01;
  DTR_CONTROL_HANDSHAKE = 16_02;

(* RTS Control Flow Values *)

  RTS_CONTROL_DISABLE   = 16_00;
  RTS_CONTROL_ENABLE    = 16_01;
  RTS_CONTROL_HANDSHAKE = 16_02;
  RTS_CONTROL_TOGGLE    = 16_03;

TYPE
  PDCB = UNTRACED REF DCB;
  LPDCB = PDCB; (* compat *)
  DCB = RECORD
    DCBlength         : UINT32; (* sizeof(DCB) *)
    BaudRate          : UINT32; (* Baudrate at which running *)
    fBinary           : Int1;  (* Binary Mode (skip EOF check) *)
    fParity           : Int1;  (* Enable parity checking *)
    fOutxCtsFlow      : Int1;  (* CTS handshaking on output *)
    fOutxDsrFlow      : Int1;  (* DSR handshaking on output *)
    fDtrControl       : Int2;  (* DTR Flow control *)
    fDsrSensitivity   : Int1;  (* DSR Sensitivity *)
    fTXContinueOnXoff : Int1;  (* Continue TX when Xoff sent *)
    fOutX             : Int1;  (* Enable output X-ON/X-OFF *)
    fInX              : Int1;  (* Enable input X-ON/X-OFF *)
    fErrorChar        : Int1;  (* Enable Err Replacement *)
    fNull             : Int1;  (* Enable Null stripping *)
    fRtsControl       : Int2;  (* Rts Flow control *)
    fAbortOnError     : Int1;  (* Abort all reads and writes on Error *)
    fDummy2           : Int17; (* Reserved *)
    wReserved         : UINT16;  (* Not currently used *)
    XonLim            : UINT16;  (* Transmit X-ON threshold *)
    XoffLim           : UINT16;  (* Transmit X-OFF threshold *)
    ByteSize          : UINT8;  (* Number of bits/byte, 4-8 *)
    Parity            : UINT8;  (* 0-4=None,Odd,Even,Mark,Space *)
    StopBits          : UINT8;  (* 0,1,2 = 1, 1.5, 2 *)
    XonChar           : char;  (* Tx and Rx X-ON character *)
    XoffChar          : char;  (* Tx and Rx X-OFF character *)
    ErrorChar         : char;  (* Error replacement char *)
    EofChar           : char;  (* End of Input character *)
    EvtChar           : char;  (* Recieved Event character *)
    wReserved1        : UINT16;  (* Fill for now. *)
  END;

  PCOMMTIMEOUTS = UNTRACED REF COMMTIMEOUTS;
  LPCOMMTIMEOUTS = PCOMMTIMEOUTS; (* compat *)
  COMMTIMEOUTS = RECORD
    ReadIntervalTimeout        : UINT32;  (* Max time between read chars. *)
    ReadTotalTimeoutMultiplier : UINT32;  (* Multiplier of characters. *)
    ReadTotalTimeoutConstant   : UINT32;  (* Constant in milliseconds. *)
    WriteTotalTimeoutMultiplier: UINT32;  (* Multiplier of characters. *)
    WriteTotalTimeoutConstant  : UINT32;  (* Constant in milliseconds. *)
  END;

  PCOMMCONFIG = UNTRACED REF COMMCONFIG;
  LPCOMMCONFIG = PCOMMCONFIG; (* compat *)
  COMMCONFIG = RECORD
    dwSize            : UINT32;  (* Size of the entire struct *)
    wVersion          : UINT16;   (* version of the structure *)
    wReserved         : UINT16;   (* alignment *)
    dcb               : DCB;    (* device control block *)
    dwProviderSubType : UINT32;  (* ordinal value for identifying
                                   provider-defined data structure format*)
    dwProviderOffset  : UINT32;  (* Specifies the offset of provider specific
                                   data field in bytes from the start *)
    dwProviderSize    : UINT32;  (* size of the provider-specific data field *)
    wcProviderData    : ARRAY [0..0] OF WCHAR; (* provider-specific data *)
  END;

  PSYSTEM_INFO = UNTRACED REF SYSTEM_INFO;
  LPSYSTEM_INFO = PSYSTEM_INFO; (* compat *)
  SYSTEM_INFO = RECORD
    wProcessorArchitecture     : UINT16;
    wReserved0                 : UINT16;
    dwPageSize                 : UINT32;
    lpMinimumApplicationAddress: PVOID;
    lpMaximumApplicationAddress: PVOID;
    dwActiveProcessorMask      : UINT32;
    dwNumberOfProcessors       : UINT32;
    dwProcessorType            : UINT32;
    dwAllocationGranularity    : UINT32;
    wProcessorLevel            : UINT16;
    wProcessorRevision         : UINT16;
  END;

(*???  #define FreeModule(hLibModule) FreeLibrary((hLibModule)) #define
   MakeProcInstance(lpProc,hInstance) (lpProc) #define
   FreeProcInstance(lpProc) (lpProc) *)

(* Global Memory Flags *)
CONST
  GMEM_FIXED          = 16_0000;
  GMEM_MOVEABLE       = 16_0002;
  GMEM_NOCOMPACT      = 16_0010;
  GMEM_NODISCARD      = 16_0020;
  GMEM_ZEROINIT       = 16_0040;
  GMEM_MODIFY         = 16_0080;
  GMEM_DISCARDABLE    = 16_0100;
  GMEM_NOT_BANKED     = 16_1000;
  GMEM_SHARE          = 16_2000;
  GMEM_DDESHARE       = 16_2000;
  GMEM_NOTIFY         = 16_4000;
  GMEM_LOWER          = GMEM_NOT_BANKED;
  GMEM_VALID_FLAGS    = 16_7F72;
  GMEM_INVALID_HANDLE = 16_8000;

  GHND = Or(GMEM_MOVEABLE, GMEM_ZEROINIT);
  GPTR = Or(GMEM_FIXED, GMEM_ZEROINIT);

(*???  #define GlobalLRUNewest( h ) (HANDLE)(h) #define GlobalLRUOldest( h
   ) (HANDLE)(h) #define GlobalDiscard( h ) GlobalReAlloc( (h), 0,
   GMEM_MOVEABLE ) *)

(* Flags returned by GlobalFlags (in addition to GMEM_DISCARDABLE) *)
  GMEM_DISCARDED = 16_4000;
  GMEM_LOCKCOUNT = 16_00FF;

TYPE
  LPMEMORYSTATUS = UNTRACED REF MEMORYSTATUS;
  MEMORYSTATUS = RECORD
    dwLength       : UINT32;
    dwMemoryLoad   : UINT32;
    dwTotalPhys    : SIZE_T;
    dwAvailPhys    : SIZE_T;
    dwTotalPageFile: SIZE_T;
    dwAvailPageFile: SIZE_T;
    dwTotalVirtual : SIZE_T;
    dwAvailVirtual : SIZE_T;
  END;

(* Local Memory Flags *)
CONST
  LMEM_FIXED          = 16_0000;
  LMEM_MOVEABLE       = 16_0002;
  LMEM_NOCOMPACT      = 16_0010;
  LMEM_NODISCARD      = 16_0020;
  LMEM_ZEROINIT       = 16_0040;
  LMEM_MODIFY         = 16_0080;
  LMEM_DISCARDABLE    = 16_0F00;
  LMEM_VALID_FLAGS    = 16_0F72;
  LMEM_INVALID_HANDLE = 16_8000;

  LHND = Or(LMEM_MOVEABLE, LMEM_ZEROINIT);
  LPTR = Or(LMEM_FIXED, LMEM_ZEROINIT);

  NONZEROLHND = (LMEM_MOVEABLE);
  NONZEROLPTR = (LMEM_FIXED);

(*???  #define LocalDiscard( h ) LocalReAlloc( (h), 0, LMEM_MOVEABLE ) *)

(* Flags returned by LocalFlags (in addition to LMEM_DISCARDABLE) *)
  LMEM_DISCARDED = 16_4000;
  LMEM_LOCKCOUNT = 16_00FF;

(* dwCreationFlag values *)

  DEBUG_PROCESS           = 16_00000001;
  DEBUG_ONLY_THIS_PROCESS = 16_00000002;

  CREATE_SUSPENDED = 16_00000004;

  DETACHED_PROCESS = 16_00000008;

  CREATE_NEW_CONSOLE = 16_00000010;

  NORMAL_PRIORITY_CLASS   = 16_00000020;
  IDLE_PRIORITY_CLASS     = 16_00000040;
  HIGH_PRIORITY_CLASS     = 16_00000080;
  REALTIME_PRIORITY_CLASS = 16_00000100;

  CREATE_NEW_PROCESS_GROUP = 16_00000200;

  CREATE_NO_WINDOW = 16_08000000;

  PROFILE_USER   = 16_10000000;
  PROFILE_KERNEL = 16_20000000;
  PROFILE_SERVER = 16_40000000;

  THREAD_PRIORITY_LOWEST       = WinNT.THREAD_BASE_PRIORITY_MIN;
  THREAD_PRIORITY_BELOW_NORMAL = (THREAD_PRIORITY_LOWEST + 1);
  THREAD_PRIORITY_NORMAL       = 0;
  THREAD_PRIORITY_HIGHEST      = WinNT.THREAD_BASE_PRIORITY_MAX;
  THREAD_PRIORITY_ABOVE_NORMAL = (THREAD_PRIORITY_HIGHEST - 1);
  THREAD_PRIORITY_ERROR_RETURN = (WinNT.MAXLONG);

  THREAD_PRIORITY_TIME_CRITICAL = WinNT.THREAD_BASE_PRIORITY_LOWRT;
  THREAD_PRIORITY_IDLE          = WinNT.THREAD_BASE_PRIORITY_IDLE;

(* Debug APIs *)
  EXCEPTION_DEBUG_EVENT      = 1;
  CREATE_THREAD_DEBUG_EVENT  = 2;
  CREATE_PROCESS_DEBUG_EVENT = 3;
  EXIT_THREAD_DEBUG_EVENT    = 4;
  EXIT_PROCESS_DEBUG_EVENT   = 5;
  LOAD_DLL_DEBUG_EVENT       = 6;
  UNLOAD_DLL_DEBUG_EVENT     = 7;
  OUTPUT_DEBUG_STRING_EVENT  = 8;
  RIP_EVENT                  = 9;

TYPE
  LPEXCEPTION_DEBUG_INFO = UNTRACED REF EXCEPTION_DEBUG_INFO;
  EXCEPTION_DEBUG_INFO = RECORD
    ExceptionRecord: WinNT.EXCEPTION_RECORD;
    dwFirstChance  : UINT32;
  END;

  LPCREATE_THREAD_DEBUG_INFO = UNTRACED REF CREATE_THREAD_DEBUG_INFO;
  CREATE_THREAD_DEBUG_INFO = RECORD
    hThread       : HANDLE;
    lpStartAddress: PTHREAD_START_ROUTINE;
  END;

  LPCREATE_PROCESS_DEBUG_INFO = UNTRACED REF CREATE_PROCESS_DEBUG_INFO;
  CREATE_PROCESS_DEBUG_INFO = RECORD
    hFile                : HANDLE;
    hProcess             : HANDLE;
    hThread              : HANDLE;
    lpBaseOfImage        : PVOID;
    dwDebugInfoFileOffset: UINT32;
    nDebugInfoSize       : UINT32;
    lpStartAddress       : PTHREAD_START_ROUTINE;
  END;

  EXIT_THREAD_DEBUG_INFO = RECORD dwExitCode: UINT32;  END;
  LPEXIT_THREAD_DEBUG_INFO = UNTRACED REF EXIT_THREAD_DEBUG_INFO;

  EXIT_PROCESS_DEBUG_INFO = RECORD dwExitCode: UINT32;  END;
  LPEXIT_PROCESS_DEBUG_INFO = UNTRACED REF EXIT_PROCESS_DEBUG_INFO;

  LPLOAD_DLL_DEBUG_INFO = UNTRACED REF LOAD_DLL_DEBUG_INFO;
  LOAD_DLL_DEBUG_INFO = RECORD
    hFile                : HANDLE;
    lpBaseOfDll          : PVOID;
    dwDebugInfoFileOffset: UINT32;
    nDebugInfoSize       : UINT32;
  END;

  UNLOAD_DLL_DEBUG_INFO = RECORD lpBaseOfDll: PVOID;  END;
  LPUNLOAD_DLL_DEBUG_INFO = UNTRACED REF UNLOAD_DLL_DEBUG_INFO;

  LPOUTPUT_DEBUG_STRING_INFO = UNTRACED REF OUTPUT_DEBUG_STRING_INFO;
  OUTPUT_DEBUG_STRING_INFO = RECORD
    lpDebugStringData : PSTR;
    fUnicode          : UINT16;
    nDebugStringLength: UINT16;
  END;

  LPRIP_INFO = UNTRACED REF RIP_INFO;
  RIP_INFO = RECORD
    dwError: UINT32;
    dwType : UINT32;
  END;

CONST
  MAX_DEBUG_EVENT = MAX (BYTESIZE (EXCEPTION_DEBUG_INFO),
                    MAX (BYTESIZE (CREATE_THREAD_DEBUG_INFO),
                    MAX (BYTESIZE (CREATE_PROCESS_DEBUG_INFO),
                    MAX (BYTESIZE (EXIT_THREAD_DEBUG_INFO),
                    MAX (BYTESIZE (EXIT_PROCESS_DEBUG_INFO),
                    MAX (BYTESIZE (LOAD_DLL_DEBUG_INFO),
                    MAX (BYTESIZE (UNLOAD_DLL_DEBUG_INFO),
                    MAX (BYTESIZE (OUTPUT_DEBUG_STRING_INFO),
                    MAX (BYTESIZE (RIP_INFO),
                         0 )))))))));

TYPE
  LPDEBUG_EVENT = UNTRACED REF DEBUG_EVENT;
  DEBUG_EVENT = RECORD
    dwDebugEventCode: UINT32;
    dwProcessId     : UINT32;
    dwThreadId      : UINT32;
    u               : ARRAY [0..MAX_DEBUG_EVENT-1] OF CHAR;
    (*!!!
      union {
        EXCEPTION_DEBUG_INFO Exception;
        CREATE_THREAD_DEBUG_INFO CreateThread;
        CREATE_PROCESS_DEBUG_INFO CreateProcessInfo;
        EXIT_THREAD_DEBUG_INFO ExitThread;
        EXIT_PROCESS_DEBUG_INFO ExitProcess;
        LOAD_DLL_DEBUG_INFO LoadDll;
        UNLOAD_DLL_DEBUG_INFO UnloadDll;
        OUTPUT_DEBUG_STRING_INFO DebugString;
        RIP_INFO RipInfo;
      } u;
    *)
  END;

  LPDEBUG_EXCEPTION = UNTRACED REF DEBUG_EXCEPTION;
  DEBUG_EXCEPTION = RECORD
    dwDebugEventCode: UINT32;
    dwProcessId     : UINT32;
    dwThreadId      : UINT32;
    Exception       : EXCEPTION_DEBUG_INFO;
  END;

  LPDEBUG_CREATE_THREAD = UNTRACED REF DEBUG_CREATE_THREAD;
  DEBUG_CREATE_THREAD = RECORD
    dwDebugEventCode: UINT32;
    dwProcessId     : UINT32;
    dwThreadId      : UINT32;
    CreateThread    : CREATE_THREAD_DEBUG_INFO;
  END;

  LPDEBUG_CREATE_PROCESS = UNTRACED REF DEBUG_CREATE_PROCESS;
  DEBUG_CREATE_PROCESS = RECORD
    dwDebugEventCode : UINT32;
    dwProcessId      : UINT32;
    dwThreadId       : UINT32;
    CreateProcessInfo: CREATE_PROCESS_DEBUG_INFO;
  END;

  LPDEBUG_EXIT_THREAD = UNTRACED REF DEBUG_EXIT_THREAD;
  DEBUG_EXIT_THREAD = RECORD
    dwDebugEventCode: UINT32;
    dwProcessId     : UINT32;
    dwThreadId      : UINT32;
    ExitThread      : EXIT_THREAD_DEBUG_INFO;
  END;

  LPDEBUG_EXIT_PROCESS = UNTRACED REF DEBUG_EXIT_PROCESS;
  DEBUG_EXIT_PROCESS = RECORD
    dwDebugEventCode: UINT32;
    dwProcessId     : UINT32;
    dwThreadId      : UINT32;
    ExitProcess     : EXIT_PROCESS_DEBUG_INFO;
  END;

  LPDEBUG_LOAD_DLL = UNTRACED REF DEBUG_LOAD_DLL;
  DEBUG_LOAD_DLL = RECORD
    dwDebugEventCode: UINT32;
    dwProcessId     : UINT32;
    dwThreadId      : UINT32;
    LoadDll         : LOAD_DLL_DEBUG_INFO;
  END;

  LPDEBUG_UNLOAD_DLL = UNTRACED REF DEBUG_UNLOAD_DLL;
  DEBUG_UNLOAD_DLL = RECORD
    dwDebugEventCode: UINT32;
    dwProcessId     : UINT32;
    dwThreadId      : UINT32;
    UnloadDll       : UNLOAD_DLL_DEBUG_INFO;
  END;

  LPDEBUG_OUTPUT_STRING = UNTRACED REF DEBUG_OUTPUT_STRING;
  DEBUG_OUTPUT_STRING = RECORD
    dwDebugEventCode: UINT32;
    dwProcessId     : UINT32;
    dwThreadId      : UINT32;
    DebugString     : OUTPUT_DEBUG_STRING_INFO;
  END;

  LPDEBUG_RIP = UNTRACED REF DEBUG_RIP;
  DEBUG_RIP = RECORD
    dwDebugEventCode: UINT32;
    dwProcessId     : UINT32;
    dwThreadId      : UINT32;
    RipInfo         : RIP_INFO;
  END;

TYPE
  LPCONTEXT = ADDRESS;          (*!!!PCONTEXT*)
  LPEXCEPTION_RECORD = WinNT.PEXCEPTION_RECORD;
  LPEXCEPTION_POINTERS = WinNT.PEXCEPTION_POINTERS;

CONST
  DRIVE_REMOVABLE = 2;
  DRIVE_FIXED     = 3;
  DRIVE_REMOTE    = 4;
  DRIVE_CDROM     = 5;
  DRIVE_RAMDISK   = 6;

(*???  #define GetFreeSpace(w) (16_100000L) *)

  FILE_TYPE_UNKNOWN = 16_0000;
  FILE_TYPE_DISK    = 16_0001;
  FILE_TYPE_CHAR    = 16_0002;
  FILE_TYPE_PIPE    = 16_0003;
  FILE_TYPE_REMOTE  = 16_8000;


  STD_INPUT_HANDLE : INT32 = -10;
  STD_OUTPUT_HANDLE: INT32 = -11;
  STD_ERROR_HANDLE : INT32 = -12;

  NOPARITY    = 0;
  ODDPARITY   = 1;
  EVENPARITY  = 2;
  MARKPARITY  = 3;
  SPACEPARITY = 4;

  ONESTOPBIT   = 0;
  ONE5STOPBITS = 1;
  TWOSTOPBITS  = 2;

  IGNORE   = 0;                 (* Ignore signal *)
  INFINITE = 16_FFFFFFFF;       (* Infinite timeout *)

(* Basud rates at which the communication device operates *)

  CBR_110    = 110;
  CBR_300    = 300;
  CBR_600    = 600;
  CBR_1200   = 1200;
  CBR_2400   = 2400;
  CBR_4800   = 4800;
  CBR_9600   = 9600;
  CBR_14400  = 14400;
  CBR_19200  = 19200;
  CBR_38400  = 38400;
  CBR_56000  = 56000;
  CBR_128000 = 128000;
  CBR_256000 = 256000;

(* Error Flags *)

  CE_RXOVER   = 16_0001;        (* Receive Queue overflow *)
  CE_OVERRUN  = 16_0002;        (* Receive Overrun Error *)
  CE_RXPARITY = 16_0004;        (* Receive Parity Error *)
  CE_FRAME    = 16_0008;        (* Receive Framing error *)
  CE_BREAK    = 16_0010;        (* Break Detected *)
  CE_TXFULL   = 16_0100;        (* TX Queue is full *)
  CE_PTO      = 16_0200;        (* LPTx Timeout *)
  CE_IOE      = 16_0400;        (* LPTx I/O Error *)
  CE_DNS      = 16_0800;        (* LPTx Device not selected *)
  CE_OOP      = 16_1000;        (* LPTx Out-Of-Paper *)
  CE_MODE     = 16_8000;        (* Requested mode unsupported *)

  IE_BADID    = (-1);           (* Invalid or unsupported id *)
  IE_OPEN     = (-2);           (* Device Already Open *)
  IE_NOPEN    = (-3);           (* Device Not Open *)
  IE_MEMORY   = (-4);           (* Unable to allocate queues *)
  IE_DEFAULT  = (-5);           (* Error in default parameters *)
  IE_HARDWARE = (-10);          (* Hardware Not Present *)
  IE_BYTESIZE = (-11);          (* Illegal Byte Size *)
  IE_BAUDRATE = (-12);          (* Unsupported BaudRate *)

(* Events *)

  EV_RXCHAR   = 16_0001;        (* Any Character received *)
  EV_RXFLAG   = 16_0002;        (* Received certain character *)
  EV_TXEMPTY  = 16_0004;        (* Transmitt Queue Empty *)
  EV_CTS      = 16_0008;        (* CTS changed state *)
  EV_DSR      = 16_0010;        (* DSR changed state *)
  EV_RLSD     = 16_0020;        (* RLSD changed state *)
  EV_BREAK    = 16_0040;        (* BREAK received *)
  EV_ERR      = 16_0080;        (* Line status error occurred *)
  EV_RING     = 16_0100;        (* Ring signal detected *)
  EV_PERR     = 16_0200;        (* Printer error occured *)
  EV_RX80FULL = 16_0400;        (* Receive buffer is 80 percent full *)
  EV_EVENT1   = 16_0800;        (* Provider specific event 1 *)
  EV_EVENT2   = 16_1000;        (* Provider specific event 2 *)

(* Escape Functions *)

  SETXOFF  = 1;                 (* Simulate XOFF received *)
  SETXON   = 2;                 (* Simulate XON received *)
  SETRTS   = 3;                 (* Set RTS high *)
  CLRRTS   = 4;                 (* Set RTS low *)
  SETDTR   = 5;                 (* Set DTR high *)
  CLRDTR   = 6;                 (* Set DTR low *)
  RESETDEV = 7;                 (* Reset device if possible *)
  SETBREAK = 8;                 (* Set the device break line. *)
  CLRBREAK = 9;                 (* Clear the device break line. *)

(* PURGE function flags. *)
  PURGE_TXABORT = 16_0001;      (* Kill the pending/current writes to the
                                   comm port. *)
  PURGE_RXABORT = 16_0002;      (* Kill the pending/current reads to the
                                   comm port. *)
  PURGE_TXCLEAR = 16_0004;      (* Kill the transmit queue if there. *)
  PURGE_RXCLEAR = 16_0008;      (* Kill the typeahead buffer if there. *)

  LPTx = 16_80;                 (* Set if ID is for LPT device *)

(* Modem Status Flags *)
  MS_CTS_ON : UINT32 = 16_0010;
  MS_DSR_ON : UINT32 = 16_0020;
  MS_RING_ON: UINT32 = 16_0040;
  MS_RLSD_ON: UINT32 = 16_0080;

(* WaitSoundState() Constants *)

  S_QUEUEEMPTY   = 0;
  S_THRESHOLD    = 1;
  S_ALLTHRESHOLD = 2;

(* Accent Modes *)

  S_NORMAL   = 0;
  S_LEGATO   = 1;
  S_STACCATO = 2;

(* SetSoundNoise() Sources *)

  S_PERIOD512 = 0;              (* Freq = N/512 high pitch, less coarse
                                   hiss *)
  S_PERIOD1024 = 1;             (* Freq = N/1024 *)
  S_PERIOD2048 = 2;             (* Freq = N/2048 low pitch, more coarse
                                   hiss *)
  S_PERIODVOICE = 3;            (* Source is frequency from voice channel
                                   (3) *)
  S_WHITE512 = 4;               (* Freq = N/512 high pitch, less coarse
                                   hiss *)
  S_WHITE1024 = 5;              (* Freq = N/1024 *)
  S_WHITE2048 = 6;              (* Freq = N/2048 low pitch, more coarse
                                   hiss *)
  S_WHITEVOICE = 7;             (* Source is frequency from voice channel
                                   (3) *)

  S_SERDVNA = (-1);             (* Device not available *)
  S_SEROFM  = (-2);             (* Out of memory *)
  S_SERMACT = (-3);             (* Music active *)
  S_SERQFUL = (-4);             (* Queue full *)
  S_SERBDNT = (-5);             (* Invalid note *)
  S_SERDLN  = (-6);             (* Invalid note length *)
  S_SERDCC  = (-7);             (* Invalid note count *)
  S_SERDTP  = (-8);             (* Invalid tempo *)
  S_SERDVL  = (-9);             (* Invalid volume *)
  S_SERDMD  = (-10);            (* Invalid mode *)
  S_SERDSH  = (-11);            (* Invalid shape *)
  S_SERDPT  = (-12);            (* Invalid pitch *)
  S_SERDFQ  = (-13);            (* Invalid frequency *)
  S_SERDDR  = (-14);            (* Invalid duration *)
  S_SERDSR  = (-15);            (* Invalid source *)
  S_SERDST  = (-16);            (* Invalid state *)

  NMPWAIT_WAIT_FOREVER     = 16_ffffffff;
  NMPWAIT_NOWAIT           = 16_00000001;
  NMPWAIT_USE_DEFAULT_WAIT = 16_00000000;

  FS_CASE_IS_PRESERVED      = WinNT.FILE_CASE_PRESERVED_NAMES;
  FS_CASE_SENSITIVE         = WinNT.FILE_CASE_SENSITIVE_SEARCH;
  FS_UNICODE_STORED_ON_DISK = WinNT.FILE_UNICODE_ON_DISK;

  FILE_MAP_WRITE      = WinNT.SECTION_MAP_WRITE;
  FILE_MAP_READ       = WinNT.SECTION_MAP_READ;
  FILE_MAP_ALL_ACCESS = WinNT.SECTION_ALL_ACCESS;

  OF_READ             = 16_00000000;
  OF_WRITE            = 16_00000001;
  OF_READWRITE        = 16_00000002;
  OF_SHARE_COMPAT     = 16_00000000;
  OF_SHARE_EXCLUSIVE  = 16_00000010;
  OF_SHARE_DENY_WRITE = 16_00000020;
  OF_SHARE_DENY_READ  = 16_00000030;
  OF_SHARE_DENY_NONE  = 16_00000040;
  OF_PARSE            = 16_00000100;
  OF_DELETE           = 16_00000200;
  OF_VERIFY           = 16_00000400;
  OF_CANCEL           = 16_00000800;
  OF_CREATE           = 16_00001000;
  OF_PROMPT           = 16_00002000;
  OF_EXIST            = 16_00004000;
  OF_REOPEN           = 16_00008000;

  OFS_MAXPATHNAME = 128;

TYPE
  POFSTRUCT = UNTRACED REF OFSTRUCT;
  LPOFSTRUCT = POFSTRUCT; (* compat -- actually this whole OpenFile thing is for compat *)
  OFSTRUCT = RECORD
    cBytes    : UINT8;
    fFixedDisk: UINT8;
    nErrCode  : UINT16;
    Reserved1 : UINT16;
    Reserved2 : UINT16;
    szPathName: ARRAY [0 .. OFS_MAXPATHNAME - 1] OF UINT8;
  END;

<*EXTERNAL InterlockedIncrement:WINAPI*>
PROCEDURE InterlockedIncrement (lpAddend: PINT32): INT32;

<*EXTERNAL InterlockedDecrement:WINAPI*>
PROCEDURE InterlockedDecrement (lpAddend: PINT32): INT32;

<*EXTERNAL InterlockedExchange:WINAPI*>
PROCEDURE InterlockedExchange (target: PINT32; value: INT32): INT32;

<*EXTERNAL InterlockedCompareExchange:WINAPI*>
PROCEDURE InterlockedCompareExchange (destination: PINT32; exchange: INT32; comparand: INT32): INT32;

<*EXTERNAL FreeResource:WINAPI*>
PROCEDURE FreeResource (hResData: HGLOBAL): BOOL;

<*EXTERNAL LockResource:WINAPI*>
PROCEDURE LockResource (hResData: HGLOBAL): PVOID;

(*???  #define UnlockResource(hResData) ((hResData), 0) *)

CONST MAXINTATOM = 16_C000;

(*???  #define MAKEINTATOM(i) (PTSTR)((UINT32)((UINT16)(i))) INVALID_ATOM =
   ((ATOM)0); *)

<*EXTERNAL WinMain:WINAPI*>
PROCEDURE WinMain (hInstance    : HINSTANCE;
                   hPrevInstance: HINSTANCE;
                   lpCmdLine    : PSTR;
                   nShowCmd     : INT32        ): INT32;

<*EXTERNAL FreeLibrary:WINAPI*>
PROCEDURE FreeLibrary (hLibModule: HMODULE): BOOL;

<*EXTERNAL FreeLibraryAndExitThread:WINAPI*>
PROCEDURE FreeLibraryAndExitThread (hLibModule: HMODULE;  dwExitCode: UINT32);

<*EXTERNAL DisableThreadLibraryCalls:WINAPI*>
PROCEDURE DisableThreadLibraryCalls (hLibModule: HMODULE): BOOL;

<*EXTERNAL GetProcAddress:WINAPI*>
PROCEDURE GetProcAddress (hModule: HINSTANCE; lpProcName: PCSTR): FARPROC;

<*EXTERNAL GetVersion:WINAPI*>
PROCEDURE GetVersion (): UINT32;

<*EXTERNAL GlobalAlloc:WINAPI*>
PROCEDURE GlobalAlloc (uFlags: UINT32; dwBytes: SIZE_T): HGLOBAL;

<*EXTERNAL GlobalReAlloc:WINAPI*>
PROCEDURE GlobalReAlloc (hMem: HGLOBAL; dwBytes: SIZE_T; uFlags: UINT32): HGLOBAL;

<*EXTERNAL GlobalSize:WINAPI*>
PROCEDURE GlobalSize (hMem: HGLOBAL): UINT32;

<*EXTERNAL GlobalFlags:WINAPI*>
PROCEDURE GlobalFlags (hMem: HGLOBAL): UINT32;

<*EXTERNAL GlobalLock:WINAPI*>
PROCEDURE GlobalLock (hMem: HGLOBAL): PVOID;

(*!!!MWH My version win31 = UINT32 WINAPI GlobalHandle(UINT32) *)
<*EXTERNAL GlobalHandle:WINAPI*>
PROCEDURE GlobalHandle (pMem: PVOID): HGLOBAL;

<*EXTERNAL GlobalUnlock:WINAPI*>
PROCEDURE GlobalUnlock (hMem: HGLOBAL): BOOL;

<*EXTERNAL GlobalFree:WINAPI*>
PROCEDURE GlobalFree (hMem: HGLOBAL): HGLOBAL;

<*EXTERNAL GlobalCompact:WINAPI*>
PROCEDURE GlobalCompact (dwMinFree: UINT32): SIZE_T;

<*EXTERNAL GlobalFix:WINAPI*>
PROCEDURE GlobalFix (hMem: HGLOBAL);

<*EXTERNAL GlobalUnfix:WINAPI*>
PROCEDURE GlobalUnfix (hMem: HGLOBAL);

<*EXTERNAL GlobalWire:WINAPI*>
PROCEDURE GlobalWire (hMem: HGLOBAL): PVOID;

<*EXTERNAL GlobalUnWire:WINAPI*>
PROCEDURE GlobalUnWire (hMem: HGLOBAL): BOOL;

<*EXTERNAL GlobalMemoryStatus:WINAPI*>
PROCEDURE GlobalMemoryStatus (lpBuffer: LPMEMORYSTATUS);

<*EXTERNAL LocalAlloc:WINAPI*>
PROCEDURE LocalAlloc (uFlags: UINT32; uBytes: SIZE_T): HLOCAL;

<*EXTERNAL LocalReAlloc:WINAPI*>
PROCEDURE LocalReAlloc (hMem: HLOCAL; uBytes: SIZE_T; uFlags: UINT32): HLOCAL;

<*EXTERNAL LocalLock:WINAPI*>
PROCEDURE LocalLock (hMem: HLOCAL): PVOID;

<*EXTERNAL LocalHandle:WINAPI*>
PROCEDURE LocalHandle (pMem: PVOID): HLOCAL;

<*EXTERNAL LocalUnlock:WINAPI*>
PROCEDURE LocalUnlock (hMem: HLOCAL): BOOL;

<*EXTERNAL LocalSize:WINAPI*>
PROCEDURE LocalSize (hMem: HLOCAL): UINT32;

<*EXTERNAL LocalFlags:WINAPI*>
PROCEDURE LocalFlags (hMem: HLOCAL): UINT32;

<*EXTERNAL LocalFree:WINAPI*>
PROCEDURE LocalFree (hMem: HLOCAL): HLOCAL;

<*EXTERNAL LocalShrink:WINAPI*>
PROCEDURE LocalShrink (hMem: HLOCAL; cbNewSize: UINT32): UINT32;

<*EXTERNAL LocalCompact:WINAPI*>
PROCEDURE LocalCompact (uMinFree: UINT32): UINT32;

<*EXTERNAL FlushInstructionCache:WINAPI*>
PROCEDURE FlushInstructionCache (hProcess     : HANDLE;
                                 lpBaseAddress: PVOID;
                                 dwSize       : SIZE_T  ): BOOL;

<*EXTERNAL VirtualAlloc:WINAPI*>
PROCEDURE VirtualAlloc (lpAddress       : PVOID;
                        dwSize          : SIZE_T;
                        flAllocationType: UINT32;
                        flProtect       : UINT32   ): PVOID;

<*EXTERNAL VirtualFree:WINAPI*>
PROCEDURE VirtualFree (lpAddress : PVOID;
                       dwSize    : SIZE_T;
                       dwFreeType: UINT32   ): BOOL;

<*EXTERNAL VirtualProtect:WINAPI*>
PROCEDURE VirtualProtect (lpAddress     : SIZE_T;  (** was PVOID -- don't want GC check --- WKK **)
                          dwSize        : SIZE_T;
                          flNewProtect  : UINT32;
                          lpflOldProtect: PUINT32  ): BOOL;

<*EXTERNAL VirtualQuery:WINAPI*>
PROCEDURE VirtualQuery (lpAddress: PVOID;
                        lpBuffer : WinNT.PMEMORY_BASIC_INFORMATION;
                        dwLength : SIZE_T                           ): SIZE_T;

<*EXTERNAL VirtualProtectEx:WINAPI*>
PROCEDURE VirtualProtectEx (hProcess      : HANDLE;
                            lpAddress     : PVOID;
                            dwSize        : SIZE_T;
                            flNewProtect  : UINT32;
                            lpflOldProtect: PUINT32  ): BOOL;

<*EXTERNAL VirtualQueryEx:WINAPI*>
PROCEDURE VirtualQueryEx (hProcess : HANDLE;
                          lpAddress: PVOID;
                          lpBuffer : WinNT.PMEMORY_BASIC_INFORMATION;
                          dwLength : SIZE_T                           ): SIZE_T;

<*EXTERNAL HeapCreate:WINAPI*>
PROCEDURE HeapCreate (flOptions    : UINT32;
                      dwInitialSize: SIZE_T;
                      dwMaximumSize: SIZE_T  ): HANDLE;

<*EXTERNAL HeapDestroy:WINAPI*>
PROCEDURE HeapDestroy (hHeap: HANDLE): BOOL;

<*EXTERNAL HeapAlloc:WINAPI*>
PROCEDURE HeapAlloc (hHeap: HANDLE; dwFlags: UINT32;  dwBytes: UINT32): PVOID;

<*EXTERNAL HeapReAlloc:WINAPI*>
PROCEDURE HeapReAlloc (hHeap   : HANDLE;
                       dwFlags : UINT32;
                       lpMem   : PVOID;
                       dwBytes : UINT32): PVOID;

<*EXTERNAL HeapFree:WINAPI*>
PROCEDURE HeapFree (hHeap: HANDLE;  dwFlags: UINT32;  lpMem: PSTR): BOOL;

<*EXTERNAL HeapSize:WINAPI*>
PROCEDURE HeapSize (hHeap: HANDLE;  dwFlags: UINT32;  lpMem: PSTR): UINT32;

<*EXTERNAL HeapValidate:WINAPI*>
PROCEDURE HeapValidate (hHeap: HANDLE; dwFlags: UINT32; lpMem: PCVOID): BOOL;

<*EXTERNAL HeapCompact:WINAPI*>
PROCEDURE HeapCompact (hHeap: HANDLE; dwFlags: UINT32): UINT32;

<*EXTERNAL GetProcessHeap:WINAPI*>
PROCEDURE GetProcessHeap (): HANDLE;

<*EXTERNAL GetProcessHeaps:WINAPI*>
PROCEDURE GetProcessHeaps (NumberOfHeaps: UINT32; ProcessHeaps: PHANDLE): UINT32;

TYPE
  union_Block = RECORD
    hMem       : HANDLE;
    dwReserved : ARRAY [0..2] OF UINT32;
  END;

  union_Region = RECORD
    dwCommittedSize   : UINT32;
    dwUnCommittedSize : UINT32;
    lpFirstBlock      : PVOID;
    lpLastBlock       : PVOID;
  END;

TYPE
  PPROCESS_HEAP_ENTRY_Block = UNTRACED REF PROCESS_HEAP_ENTRY_Block;
  LPPROCESS_HEAP_ENTRY_Block = UNTRACED REF PROCESS_HEAP_ENTRY_Block;
  PROCESS_HEAP_ENTRY_Block = RECORD
    lpData       : PVOID;
    cbData       : UINT32;
    cbOverhead   : UINT8;
    iRegionIndex : UINT8;
    wFlags       : UINT16;
    Block        : union_Block;
  END;

TYPE
  PPROCESS_HEAP_ENTRY_Region = UNTRACED REF PROCESS_HEAP_ENTRY_Region;
  LPPROCESS_HEAP_ENTRY_Region = UNTRACED REF PROCESS_HEAP_ENTRY_Region;
  PROCESS_HEAP_ENTRY_Region = RECORD
    lpData       : PVOID;
    cbData       : UINT32;
    cbOverhead   : UINT8;
    iRegionIndex : UINT8;
    wFlags       : UINT16;
    Region       : union_Region;
  END;

TYPE (* Actually, it's a union over { _Block, _Region } *)
  PPROCESS_HEAP_ENTRY  = PPROCESS_HEAP_ENTRY_Region;
  LPPROCESS_HEAP_ENTRY = LPPROCESS_HEAP_ENTRY_Region;
  PROCESS_HEAP_ENTRY   = PROCESS_HEAP_ENTRY_Region;

CONST  
  PROCESS_HEAP_REGION             = 16_0001;
  PROCESS_HEAP_UNCOMMITTED_RANGE  = 16_0002;
  PROCESS_HEAP_ENTRY_BUSY         = 16_0004;
  PROCESS_HEAP_ENTRY_MOVEABLE     = 16_0010;
  PROCESS_HEAP_ENTRY_DDESHARE     = 16_0020;

<*EXTERNAL HeapLock:WINAPI*>
PROCEDURE HeapLock (hHeap: HANDLE): BOOL;

<*EXTERNAL HeapUnlock:WINAPI*>
PROCEDURE HeapUnlock (hHeap: HANDLE): BOOL;

<*EXTERNAL HeapWalk:WINAPI*>
PROCEDURE HeapWalk (hHeap: HANDLE; lpEntry: LPPROCESS_HEAP_ENTRY): BOOL;

(* GetBinaryType return values. *)
CONST
  SCS_32BIT_BINARY = 0;
  SCS_DOS_BINARY   = 1;
  SCS_WOW_BINARY   = 2;
  SCS_PIF_BINARY   = 3;
  SCS_POSIX_BINARY = 4;
  SCS_OS216_BINARY = 5;

<*EXTERNAL GetBinaryTypeA:WINAPI*>
PROCEDURE GetBinaryTypeA (lpApplicationName: PCSTR; lpBinaryType: PUINT32): BOOL;

<*EXTERNAL GetBinaryTypeW:WINAPI*>
PROCEDURE GetBinaryTypeW (lpApplicationName: PCWSTR; lpBinaryType: PUINT32): BOOL;

CONST GetBinaryType = GetBinaryTypeA;

<*EXTERNAL GetShortPathNameA:WINAPI*>
PROCEDURE GetShortPathNameA (lpszLongPath  : PCSTR;
                             lpszShortPath : PSTR;
                             cchBuffer     : UINT32): UINT32;

<*EXTERNAL GetShortPathNameW:WINAPI*>
PROCEDURE GetShortPathNameW (lpszLongPath  : PCWSTR;
                             lpszShortPath : PWSTR;
                             cchBuffer     : UINT32): UINT32;

CONST GetShortPathName = GetShortPathNameA;

<*EXTERNAL GetProcessAffinityMask:WINAPI*>
PROCEDURE GetProcessAffinityMask (hProcess              : HANDLE;
                                  lpProcessAffinityMask : PUINT32;
                                  lpSystemAffinityMask  : PUINT32): BOOL;

<*EXTERNAL GetProcessTimes:WINAPI*>
PROCEDURE GetProcessTimes (hProcess       : HANDLE;
                           lpCreationTime : PFILETIME;
                           lpExitTime     : PFILETIME;
                           lpKernelTime   : PFILETIME;
                           lpUserTime     : PFILETIME ): BOOL;

<*EXTERNAL GetProcessWorkingSetSize:WINAPI*>
PROCEDURE GetProcessWorkingSetSize (hProcess                : HANDLE;
                                    lpMinimumWorkingSetSize : PSIZE_T;
                                    lpMaximumWorkingSetSize : PSIZE_T ): BOOL;

<*EXTERNAL SetProcessWorkingSetSize:WINAPI*>
PROCEDURE SetProcessWorkingSetSize (hProcess                : HANDLE;
                                    dwMinimumWorkingSetSize : PSIZE_T;
                                    dwMaximumWorkingSetSize : PSIZE_T ): BOOL;

<*EXTERNAL OpenProcess:WINAPI*>
PROCEDURE OpenProcess (dwDesiredAccess: UINT32;
                         bInheritHandle : BOOL;
                         dwProcessId    : UINT32  ): HANDLE;

<*EXTERNAL GetCurrentProcess:WINAPI*>
PROCEDURE GetCurrentProcess (): HANDLE;

<*EXTERNAL GetCurrentProcessId:WINAPI*>
PROCEDURE GetCurrentProcessId (): UINT32;

<*EXTERNAL ExitProcess:WINAPI*>
PROCEDURE ExitProcess (uExitCode: UINT32);

<*EXTERNAL TerminateProcess:WINAPI*>
PROCEDURE TerminateProcess (hProcess: HANDLE; uExitCode: UINT32): BOOL;

<*EXTERNAL GetExitCodeProcess:WINAPI*>
PROCEDURE GetExitCodeProcess (hProcess: HANDLE; lpExitCode: PUINT32): BOOL;


<*EXTERNAL FatalExit:WINAPI*>
PROCEDURE FatalExit (ExitCode: INT32);

<*EXTERNAL GetEnvironmentStringsA:WINAPI*>
PROCEDURE GetEnvironmentStringsA (): PSTR;

<*EXTERNAL GetEnvironmentStringsW:WINAPI*>
PROCEDURE GetEnvironmentStringsW (): PWSTR;

CONST GetEnvironmentStrings = GetEnvironmentStringsA;

<*EXTERNAL FreeEnvironmentStringsA:WINAPI*>
PROCEDURE FreeEnvironmentStringsA (lpStr: PSTR): BOOL;

<*EXTERNAL FreeEnvironmentStringsW:WINAPI*>
PROCEDURE FreeEnvironmentStringsW (lpStr: PWSTR): BOOL;

CONST FreeEnvironmentStrings = FreeEnvironmentStringsA;

<*EXTERNAL RaiseException:WINAPI*>
PROCEDURE RaiseException (dwExceptionCode   : UINT32;
                          dwExceptionFlags  : UINT32;
                          nNumberOfArguments: UINT32;
                          lpArguments       : PUINT32);

<*EXTERNAL UnhandledExceptionFilter:WINAPI*>
PROCEDURE UnhandledExceptionFilter (ExceptionInfo: WinNT.PEXCEPTION_POINTERS
                                     ): INT32;

TYPE
  LPTOP_LEVEL_EXCEPTION_FILTER = PTOP_LEVEL_EXCEPTION_FILTER;
  PTOP_LEVEL_EXCEPTION_FILTER
    = <*WINAPI*> PROCEDURE (ExceptionInfo: WinNT.PEXCEPTION_POINTERS): INT32;

<*EXTERNAL SetUnhandledExceptionFilter:WINAPI*>
PROCEDURE SetUnhandledExceptionFilter (
                     lpTopLevelExceptionFilter: LPTOP_LEVEL_EXCEPTION_FILTER
                                      ): PTOP_LEVEL_EXCEPTION_FILTER;

<*EXTERNAL CreateThread:WINAPI*>
PROCEDURE CreateThread (lpThreadAttributes: PSECURITY_ATTRIBUTES;
                        dwStackSize       : UINT32;
                        lpStartAddress    : PTHREAD_START_ROUTINE;
                        lpParameter       : PVOID;
                        dwCreationFlags   : UINT32;
                        lpThreadId        : PUINT32                 ): HANDLE;

<*EXTERNAL CreateRemoteThread:WINAPI*>
PROCEDURE CreateRemoteThread (hProcess          : HANDLE;
                              lpThreadAttributes: PSECURITY_ATTRIBUTES;
                              dwStackSize       : UINT32;
                              lpStartAddress : PTHREAD_START_ROUTINE;
                              lpParameter    : PVOID;
                              dwCreationFlags: UINT32;
                              lpThreadId     : PUINT32              ): HANDLE;

<*EXTERNAL GetCurrentThread:WINAPI*>
PROCEDURE GetCurrentThread (): HANDLE;

<*EXTERNAL GetCurrentThreadId:WINAPI*>
PROCEDURE GetCurrentThreadId (): UINT32;

<*EXTERNAL SetThreadAffinityMask:WINAPI*>
PROCEDURE SetThreadAffinityMask (hThread              : HANDLE;
                                 dwThreadAffinityMask : UINT32): UINT32;

<*EXTERNAL SetThreadPriority:WINAPI*>
PROCEDURE SetThreadPriority (hThread: HANDLE; nPriority: INT32): BOOL;

<*EXTERNAL GetThreadPriority:WINAPI*>
PROCEDURE GetThreadPriority (hThread: HANDLE): INT32;

<*EXTERNAL GetThreadTimes:WINAPI*>
PROCEDURE GetThreadTimes (hThread        : HANDLE;
                          lpCreationTime : PFILETIME;
                          lpExitTime     : PFILETIME;
                          lpKernelTime   : PFILETIME;
                          lpUserTime     : PFILETIME): BOOL;

<*EXTERNAL ExitThread:WINAPI*>
PROCEDURE ExitThread (dwExitCode: UINT32);

<*EXTERNAL TerminateThread:WINAPI*>
PROCEDURE TerminateThread (hThread: HANDLE; dwExitCode: UINT32): BOOL;

<*EXTERNAL GetExitCodeThread:WINAPI*>
PROCEDURE GetExitCodeThread (hThread: HANDLE; lpExitCode: PUINT32): BOOL;

<*EXTERNAL GetThreadSelectorEntry:WINAPI*>
PROCEDURE GetThreadSelectorEntry (hThread        : HANDLE;
                                  dwSelector     : UINT32;
                                  lpSelectorEntry: PLDT_ENTRY): BOOL;

<*EXTERNAL GetLastError:WINAPI*>
PROCEDURE GetLastError (): UINT32;

<*EXTERNAL SetLastError:WINAPI*>
PROCEDURE SetLastError (dwErrCode: UINT32);

(* SetLastErrorEx() types. *)
CONST
  SLE_ERROR      = 16_00000001;
  SLE_MINORERROR = 16_00000002;
  SLE_WARNING    = 16_00000003;

<*EXTERNAL SetLastErrorEx:WINAPI*>
PROCEDURE SetLastErrorEx (dwErrCode: UINT32; dwType: UINT32);

<*EXTERNAL GetOverlappedResult:WINAPI*>
PROCEDURE GetOverlappedResult (hFile                     : HANDLE;
                               lpOverlapped              : POVERLAPPED;
                               lpNumberOfBytesTransferred: PUINT32;
                               bWait                     : BOOL        ): BOOL;

<*EXTERNAL CreateIoCompletionPort:WINAPI*>
PROCEDURE CreateIoCompletionPort (FileHandle               : HANDLE;
                                  ExistingCompletionPort   : HANDLE;
                                  CompletionKey            : SIZE_T;
                                  NumberOfConcurrentThreads: UINT32  ): HANDLE;

<*EXTERNAL GetQueuedCompletionStatus:WINAPI*>
PROCEDURE GetQueuedCompletionStatus (CompletionPort    : HANDLE;
                                     lpNumberOfBytesTransferred : PUINT32;
                                     lpCompletionKey   : PSIZE_T;
                                     lpOverlapped      : UNTRACED REF POVERLAPPED;
                                     dwMilliseconds    : UINT32 ): BOOL;

<*EXTERNAL PostQueuedCompletionStatus:WINAPI*>
PROCEDURE PostQueuedCompletionStatus (CompletionPort            : HANDLE;
                                      dwNumberOfBytesTransferred: UINT32;
                                      dwCompletionKey           : SIZE_T;
                                      lpOverlapped              : POVERLAPPED): BOOL;

CONST
  SEM_FAILCRITICALERRORS     = 16_0001;
  SEM_NOGPFAULTERRORBOX      = 16_0002;
  SEM_NOALIGNMENTFAULTEXCEPT = 16_0004;
  SEM_NOOPENFILEERRORBOX     = 16_8000;

<*EXTERNAL SetDebugErrorLevel:WINAPI*>
PROCEDURE SetDebugErrorLevel (dwLevel: UINT32);

<*EXTERNAL SetErrorMode:WINAPI*>
PROCEDURE SetErrorMode (uMode: UINT32): UINT32;


<*EXTERNAL ReadProcessMemory:WINAPI*>
PROCEDURE ReadProcessMemory (hProcess           : HANDLE;
                             lpBaseAddress      : PCVOID;
                             lpBuffer           : PVOID;
                             nSize              : SIZE_T;
                             lpNumberOfBytesRead: PSIZE_T ): BOOL;

<*EXTERNAL WriteProcessMemory:WINAPI*>
PROCEDURE WriteProcessMemory (hProcess              : HANDLE;
                              lpBaseAddress         : PVOID;
                              lpBuffer              : PCVOID;
                              nSize                 : SIZE_T;
                              lpNumberOfBytesWritten: PSIZE_T ): BOOL;

<*EXTERNAL GetThreadContext:WINAPI*>
PROCEDURE GetThreadContext (hThread: HANDLE; lpContext: LPCONTEXT): BOOL;

<*EXTERNAL SetThreadContext:WINAPI*>
PROCEDURE SetThreadContext (hThread: HANDLE; lpContext: LPCONTEXT): BOOL;

<*EXTERNAL SuspendThread:WINAPI*>
PROCEDURE SuspendThread (hThread: HANDLE): UINT32;

<*EXTERNAL ResumeThread:WINAPI*>
PROCEDURE ResumeThread (hThread: HANDLE): UINT32;

<*EXTERNAL IsDebuggerPresent:WINAPI*>
PROCEDURE IsDebuggerPresent (): BOOL;

<*EXTERNAL DebugBreak:WINAPI*>
PROCEDURE DebugBreak ();

<*EXTERNAL WaitForDebugEvent:WINAPI*>
PROCEDURE WaitForDebugEvent (lpDebugEvent  : LPDEBUG_EVENT;
                             dwMilliseconds: UINT32          ): BOOL;

<*EXTERNAL ContinueDebugEvent:WINAPI*>
PROCEDURE ContinueDebugEvent (dwProcessId     : UINT32;
                              dwThreadId      : UINT32;
                              dwContinueStatus: UINT32  ): BOOL;

<*EXTERNAL DebugActiveProcess:WINAPI*>
PROCEDURE DebugActiveProcess (dwProcessId: UINT32): BOOL;

<*EXTERNAL DebugSnapShotProcessHeaps:WINAPI*>
PROCEDURE DebugSnapShotProcessHeaps (dwProcessId: UINT32): HANDLE;

TYPE
  PDEBUG_HEAP_BACKTRACE = UNTRACED REF DEBUG_HEAP_BACKTRACE;
  DEBUG_HEAP_BACKTRACE = RECORD
    Depth          : UINT32;
    ReturnAddresses: ARRAY [0 .. 0] OF UINT32;
  END;

  PDEBUG_HEAP_ALLOCATOR = UNTRACED REF DEBUG_HEAP_ALLOCATOR;
  DEBUG_HEAP_ALLOCATOR = RECORD
    TotalBytesAllocated: UINT32;
    OffsetToBackTrace  : UINT32;
  END;

  PDEBUG_HEAP_ENTRY = UNTRACED REF DEBUG_HEAP_ENTRY;
  DEBUG_HEAP_ENTRY = RECORD
    Address  : UINT32;
    Size     : UINT16;
    Allocator: UINT16;
  END;

CONST
  DEBUG_HEAP_ENTRY_FREE (*!!!:UINT16*) = -1;

TYPE
  PDEBUG_HEAP_INFO = UNTRACED REF DEBUG_HEAP_INFO;
  DEBUG_HEAP_INFO = RECORD
    Creator                : UINT16;
    HeaderSizeLog2         : UINT16;
    ReservedPages          : UINT16;
    CommittedPages         : UINT16;
    AllocatorReservedPages : UINT16;
    AllocatorCommittedPages: UINT16;
    NumberOfEntries        : UINT32;
    Entries: ARRAY [0 .. 0] OF DEBUG_HEAP_ENTRY;
  END;

  PDEBUG_HEAP_DUMP = UNTRACED REF DEBUG_HEAP_DUMP;
  DEBUG_HEAP_DUMP = RECORD
    OffsetToAllocatorTable: UINT32;
    NumberOfHeaps         : UINT32;
    HeapInfo: ARRAY [0 .. 0] OF DEBUG_HEAP_INFO;
  END;

<*EXTERNAL InitializeCriticalSection:WINAPI*>
PROCEDURE InitializeCriticalSection (lpCriticalSection: PCRITICAL_SECTION);

<*EXTERNAL EnterCriticalSection:WINAPI*>
PROCEDURE EnterCriticalSection (lpCriticalSection: PCRITICAL_SECTION);

<*EXTERNAL LeaveCriticalSection:WINAPI*>
PROCEDURE LeaveCriticalSection (lpCriticalSection: PCRITICAL_SECTION);

<*EXTERNAL DeleteCriticalSection:WINAPI*>
PROCEDURE DeleteCriticalSection (lpCriticalSection: PCRITICAL_SECTION);

<*EXTERNAL SetEvent:WINAPI*>
PROCEDURE SetEvent (hEvent: HANDLE): BOOL;

<*EXTERNAL ResetEvent:WINAPI*>
PROCEDURE ResetEvent (hEvent: HANDLE): BOOL;

<*EXTERNAL PulseEvent:WINAPI*>
PROCEDURE PulseEvent (hEvent: HANDLE): BOOL;

<*EXTERNAL ReleaseSemaphore:WINAPI*>
PROCEDURE ReleaseSemaphore (hSemaphore     : HANDLE;
                            lReleaseCount  : INT32;
                            lpPreviousCount: PINT32  ): BOOL;

<*EXTERNAL ReleaseMutex:WINAPI*>
PROCEDURE ReleaseMutex (hMutex: HANDLE): BOOL;

<*EXTERNAL WaitForSingleObject:WINAPI*>
PROCEDURE WaitForSingleObject (hHandle: HANDLE; dwMilliseconds: UINT32): UINT32;

<*EXTERNAL WaitForMultipleObjects:WINAPI*>
PROCEDURE WaitForMultipleObjects (nCount        : UINT32;
                                  lpHandles     : PHANDLE;
                                  bWaitAll      : BOOL;
                                  dwMilliseconds: UINT32     ): UINT32;

<*EXTERNAL Sleep:WINAPI*>
PROCEDURE Sleep (dwMilliseconds: UINT32);

<*EXTERNAL LoadResource:WINAPI*>
PROCEDURE LoadResource (hModule: HINSTANCE; hResInfo: HRSRC): HRSRC;

<*EXTERNAL SizeofResource:WINAPI*>
PROCEDURE SizeofResource (hModule: HINSTANCE; hResInfo: HRSRC): UINT32;


<*EXTERNAL CloseProfileUserMapping:WINAPI*>
PROCEDURE CloseProfileUserMapping (): BOOLEAN;

<*EXTERNAL OpenProfileUserMapping:WINAPI*>
PROCEDURE OpenProfileUserMapping (): BOOLEAN;


<*EXTERNAL GlobalDeleteAtom:WINAPI*>
PROCEDURE GlobalDeleteAtom (nAtom: ATOM): ATOM;

<*EXTERNAL InitAtomTable:WINAPI*>
PROCEDURE InitAtomTable (nSize: UINT32): BOOL;

<*EXTERNAL DeleteAtom:WINAPI*>
PROCEDURE DeleteAtom (nAtom: ATOM): ATOM;

<*EXTERNAL SetHandleCount:WINAPI*>
PROCEDURE SetHandleCount (uNumber: UINT32): UINT32;

<*EXTERNAL GetLogicalDrives:WINAPI*>
PROCEDURE GetLogicalDrives (): UINT32;

<*EXTERNAL LockFile:WINAPI*>
PROCEDURE LockFile (hFile                   : HANDLE;
                    dwFileOffsetLow         : UINT32;
                    dwFileOffsetHigh        : UINT32;
                    nNumberOfBytesToLockLow : UINT32;
                    nNumberOfBytesToLockHigh: UINT32   ): BOOL;

<*EXTERNAL UnlockFile:WINAPI*>
PROCEDURE UnlockFile (hFile                     : HANDLE;
                      dwFileOffsetLow           : UINT32;
                      dwFileOffsetHigh          : UINT32;
                      nNumberOfBytesToUnlockLow : UINT32;
                      nNumberOfBytesToUnlockHigh: UINT32   ): BOOL;

<*EXTERNAL LockFileEx:WINAPI*>
PROCEDURE LockFileEx (hFile                   : HANDLE;
                      dwFlags                 : UINT32;
                      dwReserved              : UINT32;
                      nNumberOfBytesToLockLow : UINT32;
                      nNumberOfBytesToLockHigh: UINT32;
                      lpOverlapped            : POVERLAPPED): BOOL;

CONST
  LOCKFILE_FAIL_IMMEDIATELY = 16_00000001;
  LOCKFILE_EXCLUSIVE_LOCK   = 16_00000002;

<*EXTERNAL UnlockFileEx:WINAPI*>
PROCEDURE UnlockFileEx (hFile                     : HANDLE;
                        dwReserved                : UINT32;
                        nNumberOfBytesToUnlockLow : UINT32;
                        nNumberOfBytesToUnlockHigh: UINT32;
                        lpOverlapped              : POVERLAPPED): BOOL;


TYPE
  PBY_HANDLE_FILE_INFORMATION = UNTRACED REF BY_HANDLE_FILE_INFORMATION;
  LPBY_HANDLE_FILE_INFORMATION = UNTRACED REF BY_HANDLE_FILE_INFORMATION;
  BY_HANDLE_FILE_INFORMATION = RECORD
    dwFileAttributes    : UINT32;
    ftCreationTime      : FILETIME;
    ftLastAccessTime    : FILETIME;
    ftLastWriteTime     : FILETIME;
    dwVolumeSerialNumber: UINT32;
    nFileSizeHigh       : UINT32;
    nFileSizeLow        : UINT32;
    nNumberOfLinks      : UINT32;
    nFileIndexHigh      : UINT32;
    nFileIndexLow       : UINT32;
  END;

<*EXTERNAL GetFileInformationByHandle:WINAPI*>
PROCEDURE GetFileInformationByHandle (hFile: HANDLE;
              lpFileInformation: LPBY_HANDLE_FILE_INFORMATION): BOOL;

<*EXTERNAL GetFileType:WINAPI*>
PROCEDURE GetFileType (hFile: HANDLE): UINT32;

<*EXTERNAL GetFileSize:WINAPI*>
PROCEDURE GetFileSize (hFile: HANDLE; lpFileSizeHigh: PUINT32): UINT32;

<*EXTERNAL GetStdHandle:WINAPI*>
PROCEDURE GetStdHandle (nStdHandle: INT32): HANDLE;

<*EXTERNAL SetStdHandle:WINAPI*>
PROCEDURE SetStdHandle (nStdHandle: INT32; hHandle: HANDLE): BOOL;

<*EXTERNAL WriteFile:WINAPI*>
PROCEDURE WriteFile (hFile                 : HANDLE;
                     lpBuffer              : PVOID;
                     nNumberOfBytesToWrite : UINT32;
                     lpNumberOfBytesWritten: PUINT32;
                     lpOverlapped          : POVERLAPPED): BOOL;

<*EXTERNAL ReadFile:WINAPI*>
PROCEDURE ReadFile (hFile               : HANDLE;
                    lpBuffer            : PVOID;
                    nNumberOfBytesToRead: UINT32;
                    lpNumberOfBytesRead : PUINT32;
                    lpOverlapped        : POVERLAPPED): BOOL;

<*EXTERNAL FlushFileBuffers:WINAPI*>
PROCEDURE FlushFileBuffers (hFile: HANDLE): BOOL;

<*EXTERNAL DeviceIoControl:WINAPI*>
PROCEDURE DeviceIoControl (hDevice        : HANDLE;
                           dwIoControlCode: UINT32;
                           lpInBuffer     : PVOID;
                           nInBufferSize  : UINT32;
                           lpOutBuffer    : PVOID;
                           nOutBufferSize : UINT32;
                           lpBytesReturned: PUINT32;
                           lpOverlapped   : POVERLAPPED): BOOL;

<*EXTERNAL SetEndOfFile:WINAPI*>
PROCEDURE SetEndOfFile (hFile: HANDLE): BOOL;

<*EXTERNAL SetFilePointer:WINAPI*>
PROCEDURE SetFilePointer (hFile               : HANDLE;
                          lDistanceToMove     : INT32;
                          lpDistanceToMoveHigh: PINT32;
                          dwMoveMethod        : UINT32   ): UINT32;

<*EXTERNAL FindClose:WINAPI*>
PROCEDURE FindClose (hFindFile: HANDLE): BOOL;

<*EXTERNAL GetFileTime:WINAPI*>
PROCEDURE GetFileTime (hFile           : HANDLE;
                       lpCreationTime  : PFILETIME;
                       lpLastAccessTime: PFILETIME;
                       lpLastWriteTime : PFILETIME  ): BOOL;

<*EXTERNAL SetFileTime:WINAPI*>
PROCEDURE SetFileTime (hFile           : HANDLE;
                       lpCreationTime  : PFILETIME;
                       lpLastAccessTime: PFILETIME;
                       lpLastWriteTime : PFILETIME  ): BOOL;

<*EXTERNAL CloseHandle:WINAPI*>
PROCEDURE CloseHandle (hObject: HANDLE): BOOL;

<*EXTERNAL DuplicateHandle:WINAPI*>
PROCEDURE DuplicateHandle (hSourceProcessHandle: HANDLE;
                           hSourceHandle       : HANDLE;
                           hTargetProcessHandle: HANDLE;
                           lpTargetHandle      : PHANDLE;
                           dwDesiredAccess     : UINT32;
                           bInheritHandle      : BOOL;
                           dwOptions           : UINT32     ): BOOL;

<*EXTERNAL LoadModule:WINAPI*>
PROCEDURE LoadModule (lpModuleName: PCSTR; lpParameterBlock: PVOID): UINT32;

<*EXTERNAL WinExec:WINAPI*>
PROCEDURE WinExec (lpCmdLine: PCSTR; uCmdShow: UINT32): UINT32;

(*???  Redefined later--sjrh <*EXTERNAL BuildCommDCB:WINAPI*>
PROCEDURE BuildCommDCB( lpDef: PSTR; lpDCB: PDCB ):BOOL;
*)

<*EXTERNAL ClearCommBreak:WINAPI*>
PROCEDURE ClearCommBreak (hFile: HANDLE): BOOL;

<*EXTERNAL ClearCommError:WINAPI*>
PROCEDURE ClearCommError (hFile   : HANDLE;
                          lpErrors: PUINT32;
                          lpStat  : PCOMSTAT): BOOL;

<*EXTERNAL SetupComm:WINAPI*>
PROCEDURE SetupComm (hFile: HANDLE; dwInQueue: UINT32; dwOutQueue: UINT32): BOOL;

<*EXTERNAL EscapeCommFunction:WINAPI*>
PROCEDURE EscapeCommFunction (hFile: HANDLE; dwFunc: UINT32): BOOL;

<*EXTERNAL GetCommMask:WINAPI*>
PROCEDURE GetCommMask (hFile: HANDLE; lpEvtMask: PUINT32): BOOL;

<*EXTERNAL GetCommProperties:WINAPI*>
PROCEDURE GetCommProperties (hFile: HANDLE; lpCommProp: PCOMMPROP): BOOL;

<*EXTERNAL GetCommModemStatus:WINAPI*>
PROCEDURE GetCommModemStatus (hFile: HANDLE; lpModemStat: PUINT32): BOOL;

<*EXTERNAL GetCommState:WINAPI*>
PROCEDURE GetCommState (hFile: HANDLE; lpDCB: PDCB): BOOL;

<*EXTERNAL GetCommTimeouts:WINAPI*>
PROCEDURE GetCommTimeouts (hFile: HANDLE; lpCommTimeouts: PCOMMTIMEOUTS):BOOL;

<*EXTERNAL PurgeComm:WINAPI*>
PROCEDURE PurgeComm (hFile: HANDLE; dwFlags: UINT32): BOOL;

<*EXTERNAL SetCommBreak:WINAPI*>
PROCEDURE SetCommBreak (hFile: HANDLE): BOOL;

<*EXTERNAL SetCommMask:WINAPI*>
PROCEDURE SetCommMask (hFile: HANDLE; dwEvtMask: UINT32): BOOL;

<*EXTERNAL SetCommState:WINAPI*>
PROCEDURE SetCommState (hFile: HANDLE; lpDCB: PDCB): BOOL;

<*EXTERNAL SetCommTimeouts:WINAPI*>
PROCEDURE SetCommTimeouts (hFile: HANDLE; lpCommTimeouts: PCOMMTIMEOUTS):BOOL;

<*EXTERNAL TransmitCommChar:WINAPI*>
PROCEDURE TransmitCommChar (hFile: HANDLE; cChar: char): BOOL;

<*EXTERNAL WaitCommEvent:WINAPI*>
PROCEDURE WaitCommEvent (hFile       : HANDLE;
                         lpEvtMask   : PUINT32;
                         lpOverlapped: POVERLAPPED): BOOL;


<*EXTERNAL SetTapePosition:WINAPI*>
PROCEDURE SetTapePosition (hDevice         : HANDLE;
                           dwPositionMethod: UINT32;
                           dwPartition     : UINT32;
                           dwOffsetLow     : UINT32;
                           dwOffsetHigh    : UINT32;
                           bImmediate      : BOOL): UINT32;

<*EXTERNAL GetTapePosition:WINAPI*>
PROCEDURE GetTapePosition (hDevice       : HANDLE;
                           dwPositionType: UINT32;
                           lpdwPartition : PUINT32;
                           lpdwOffsetLow : PUINT32;
                           lpdwOffsetHigh: PUINT32  ): UINT32;

<*EXTERNAL PrepareTape:WINAPI*>
PROCEDURE PrepareTape (hDevice     : HANDLE;
                       dwOperation : UINT32;
                       bImmediate  : BOOL): UINT32;

<*EXTERNAL EraseTape:WINAPI*>
PROCEDURE EraseTape (hDevice     : HANDLE;
                     dwEraseType : UINT32;
                     bImmediate  : BOOL): UINT32;

<*EXTERNAL CreateTapePartition:WINAPI*>
PROCEDURE CreateTapePartition (hDevice          : HANDLE;
                               dwPartitionMethod: UINT32;
                               dwCount          : UINT32;
                               dwSize           : UINT32   ): UINT32;

<*EXTERNAL WriteTapemark:WINAPI*>
PROCEDURE WriteTapemark (hDevice        : HANDLE;
                         dwTapemarkType : UINT32;
                         dwTapemarkCount: UINT32;
                         bImmediate     : BOOL): UINT32;

<*EXTERNAL GetTapeStatus:WINAPI*>
PROCEDURE GetTapeStatus (hDevice: HANDLE): UINT32;

<*EXTERNAL GetTapeParameters:WINAPI*>
PROCEDURE GetTapeParameters (hDevice          : HANDLE;
                             dwOperation      : UINT32;
                             lpdwSize         : PUINT32;
                             lpTapeInformation: PVOID   ): UINT32;

CONST
  GET_TAPE_MEDIA_INFORMATION = 0;
  GET_TAPE_DRIVE_INFORMATION = 1;

<*EXTERNAL SetTapeParameters:WINAPI*>
PROCEDURE SetTapeParameters (hDevice          : HANDLE;
                             dwOperation      : UINT32;
                             lpTapeInformation: PVOID  ): UINT32;

CONST
  SET_TAPE_MEDIA_INFORMATION = 0;
  SET_TAPE_DRIVE_INFORMATION = 1;

<*EXTERNAL Beep:WINAPI*>
PROCEDURE Beep (dwFreq: UINT32; dwDuration: UINT32): BOOL;

<*EXTERNAL OpenSound:WINAPI*>
PROCEDURE OpenSound ();

<*EXTERNAL CloseSound:WINAPI*>
PROCEDURE CloseSound ();

<*EXTERNAL StartSound:WINAPI*>
PROCEDURE StartSound ();

<*EXTERNAL StopSound:WINAPI*>
PROCEDURE StopSound ();

<*EXTERNAL WaitSoundState:WINAPI*>
PROCEDURE WaitSoundState (nState: UINT32): UINT32;

<*EXTERNAL SyncAllVoices:WINAPI*>
PROCEDURE SyncAllVoices (): UINT32;

<*EXTERNAL CountVoiceNotes:WINAPI*>
PROCEDURE CountVoiceNotes (nVoice: UINT32): UINT32;

<*EXTERNAL GetThresholdEvent:WINAPI*>
PROCEDURE GetThresholdEvent (): PUINT32;

<*EXTERNAL GetThresholdStatus:WINAPI*>
PROCEDURE GetThresholdStatus (): UINT32;

<*EXTERNAL SetSoundNoise:WINAPI*>
PROCEDURE SetSoundNoise (nSource: UINT32; nDuration: UINT32): UINT32;

<*EXTERNAL SetVoiceAccent:WINAPI*>
PROCEDURE SetVoiceAccent (nVoice : UINT32;
                          nTempo : UINT32;
                          nVolume: UINT32;
                          nMode  : UINT32;
                          nPitch : UINT32  ): UINT32;

<*EXTERNAL SetVoiceEnvelope:WINAPI*>
PROCEDURE SetVoiceEnvelope (nVoice, nShape, nRepeat: UINT32): UINT32;

<*EXTERNAL SetVoiceNote:WINAPI*>
PROCEDURE SetVoiceNote (nVoice, nValue, nLength, nCdots: UINT32): UINT32;

<*EXTERNAL SetVoiceQueueSize:WINAPI*>
PROCEDURE SetVoiceQueueSize (nVoice: UINT32; nBytes: UINT32): UINT32;

<*EXTERNAL SetVoiceSound:WINAPI*>
PROCEDURE SetVoiceSound (nVoice, Frequency, nDuration: UINT32): UINT32;

<*EXTERNAL SetVoiceThreshold:WINAPI*>
PROCEDURE SetVoiceThreshold (nVoice: UINT32; nNotes: UINT32): UINT32;

<*EXTERNAL MulDiv:WINAPI*>
PROCEDURE MulDiv (nNumber: INT32; nNumerator: INT32; nDenominator: INT32): INT32;

<*EXTERNAL GetSystemTime:WINAPI*>
PROCEDURE GetSystemTime (lpSystemTime: PSYSTEMTIME);

(* This is not in NT 3.1 but was introduced I think in NT 3.5.
   It is in Win95. Not sure about Win32s. *)
<*EXTERNAL GetSystemTimeAsFileTime:WINAPI*>
PROCEDURE GetSystemTimeAsFileTime(VAR FileTime : FILETIME);

<*EXTERNAL SetSystemTime:WINAPI*>
PROCEDURE SetSystemTime (lpSystemTime: PSYSTEMTIME): BOOL;

<*EXTERNAL GetLocalTime:WINAPI*>
PROCEDURE GetLocalTime (lpSystemTime: PSYSTEMTIME);

<*EXTERNAL SetLocalTime:WINAPI*>
PROCEDURE SetLocalTime (lpSystemTime: PSYSTEMTIME): BOOL;

<*EXTERNAL GetSystemInfo:WINAPI*>
PROCEDURE GetSystemInfo (lpSystemInfo: PSYSTEM_INFO);

TYPE
  PTIME_ZONE_INFORMATION = UNTRACED REF TIME_ZONE_INFORMATION;
  LPTIME_ZONE_INFORMATION = PTIME_ZONE_INFORMATION; (* compat *)
  TIME_ZONE_INFORMATION = RECORD
    Bias        : INT32;
    StandardName: ARRAY [0 .. 31] OF WCHAR;
    StandardDate: SYSTEMTIME;
    StandardBias: INT32;
    DaylightName: ARRAY [0 .. 31] OF WCHAR;
    DaylightDate: SYSTEMTIME;
    DaylightBias: INT32;
  END;

<*EXTERNAL GetTimeZoneInformation:WINAPI*>
PROCEDURE GetTimeZoneInformation
            (lpTimeZoneInformation: PTIME_ZONE_INFORMATION): UINT32;

<*EXTERNAL SetTimeZoneInformation:WINAPI*>
PROCEDURE SetTimeZoneInformation
            (lpTimeZoneInformation: PTIME_ZONE_INFORMATION): BOOL;

(* Routines to convert back and forth between system time and file time *)

<*EXTERNAL SystemTimeToFileTime:WINAPI*>
PROCEDURE SystemTimeToFileTime (lpSystemTime: PSYSTEMTIME;
                                lpFileTime  : PFILETIME    ): BOOL;

(* Note: As of 14-May-97, the following routine is implemented on
   Windows/NT, but not on Windows 95. -CAH *)
<*EXTERNAL SystemTimeToTzSpecificLocalTime:WINAPI*>
PROCEDURE SystemTimeToTzSpecificLocalTime (
            lpTimeZoneInformation: PTIME_ZONE_INFORMATION;
            lpUniversalTime      : PSYSTEMTIME;
            lpLocalTime          : PSYSTEMTIME            ): BOOL;

<*EXTERNAL FileTimeToLocalFileTime:WINAPI*>
PROCEDURE FileTimeToLocalFileTime (lpFileTime     : PFILETIME;
                                   lpLocalFileTime: PFILETIME  ): BOOL;

<*EXTERNAL LocalFileTimeToFileTime:WINAPI*>
PROCEDURE LocalFileTimeToFileTime (lpLocalFileTime: PFILETIME;
                                   lpFileTime     : PFILETIME  ): BOOL;

<*EXTERNAL FileTimeToSystemTime:WINAPI*>
PROCEDURE FileTimeToSystemTime (lpFileTime  : PFILETIME;
                                lpSystemTime: PSYSTEMTIME): BOOL;

<*EXTERNAL CompareFileTime:WINAPI*>
PROCEDURE CompareFileTime (lpFileTime1: PFILETIME;
                           lpFileTime2: PFILETIME  ): INT32;

<*EXTERNAL FileTimeToDosDateTime:WINAPI*>
PROCEDURE FileTimeToDosDateTime (lpFileTime: PFILETIME;
                                 lpFatDate : PUINT16;
                                 lpFatTime : PUINT16      ): BOOL;

<*EXTERNAL DosDateTimeToFileTime:WINAPI*>
PROCEDURE DosDateTimeToFileTime (wFatDate  : UINT16;
                                 wFatTime  : UINT16;
                                 lpFileTime: PFILETIME): BOOL;

<*EXTERNAL GetTickCount:WINAPI*>
PROCEDURE GetTickCount (): UINT32;

<*EXTERNAL SetSystemTimeAdjustment:WINAPI*>
PROCEDURE SetSystemTimeAdjustment (dwTimeAdjustment        : UINT32;
                                   bTimeAdjustmentDisabled : BOOL): BOOL;

<*EXTERNAL GetSystemTimeAdjustment:WINAPI*>
PROCEDURE GetSystemTimeAdjustment (lpTimeAdjustment         : PUINT32;
                                   lpTimeIncrement          : PUINT32;
                                   lpTimeAdjustmentDisabled : PBOOL): BOOL;

<*EXTERNAL FormatMessageA:WINAPI*>
PROCEDURE FormatMessageA (dwFlags     : UINT32;
                          lpSource    : PVOID;
                          dwMessageId : UINT32;
                          dwLanguageId: UINT32;
                          lpBuffer    : PSTR;
                          nSize       : UINT32;
                          lpArguments : PVOID  ): UINT32;

<*EXTERNAL FormatMessageW:WINAPI*>
PROCEDURE FormatMessageW (dwFlags     : UINT32;
                          lpSource    : PVOID;
                          dwMessageId : UINT32;
                          dwLanguageId: UINT32;
                          lpBuffer    : PWSTR;
                          nSize       : UINT32;
                          lpArguments : PVOID  ): UINT32;

CONST FormatMessage = FormatMessageA;

CONST
  FORMAT_MESSAGE_ALLOCATE_BUFFER = 16_00000100;
  FORMAT_MESSAGE_IGNORE_INSERTS  = 16_00000200;
  FORMAT_MESSAGE_FROM_STRING     = 16_00000400;
  FORMAT_MESSAGE_FROM_HMODULE    = 16_00000800;
  FORMAT_MESSAGE_FROM_SYSTEM     = 16_00001000;
  FORMAT_MESSAGE_MAX_WIDTH_MASK  = 16_000000FF;


<*EXTERNAL CreatePipe:WINAPI*>
PROCEDURE CreatePipe (hReadPipe       : PHANDLE;
                      hWritePipe      : PHANDLE;
                      lpPipeAttributes: PSECURITY_ATTRIBUTES;
                      nSize           : UINT32                  ): BOOL;

<*EXTERNAL ConnectNamedPipe:WINAPI*>
PROCEDURE ConnectNamedPipe (hNamedPipe  : HANDLE;
                            lpOverlapped: POVERLAPPED): BOOL;

<*EXTERNAL DisconnectNamedPipe:WINAPI*>
PROCEDURE DisconnectNamedPipe (hNamedPipe: HANDLE): BOOL;

<*EXTERNAL SetNamedPipeHandleState:WINAPI*>
PROCEDURE SetNamedPipeHandleState (hNamedPipe          : HANDLE;
                                   lpMode              : PUINT32;
                                   lpMaxCollectionCount: PUINT32;
                                   lpCollectDataTimeout: PUINT32  ): BOOL;

<*EXTERNAL GetNamedPipeInfo:WINAPI*>
PROCEDURE GetNamedPipeInfo (hNamedPipe     : HANDLE;
                            lpFlags        : PUINT32;
                            lpOutBufferSize: PUINT32;
                            lpInBufferSize : PUINT32;
                            lpMaxInstances : PUINT32  ): BOOL;

<*EXTERNAL PeekNamedPipe:WINAPI*>
PROCEDURE PeekNamedPipe (hNamedPipe            : HANDLE;
                         lpBuffer              : PVOID;
                         nBufferSize           : UINT32;
                         lpBytesRead           : PUINT32;
                         lpTotalBytesAvail     : PUINT32;
                         lpBytesLeftThisMessage: PUINT32  ): BOOL;

<*EXTERNAL TransactNamedPipe:WINAPI*>
PROCEDURE TransactNamedPipe (hNamedPipe    : HANDLE;
                             lpInBuffer    : PVOID;
                             nInBufferSize : UINT32;
                             lpOutBuffer   : PVOID;
                             nOutBufferSize: UINT32;
                             lpBytesRead   : PUINT32;
                             lpOverlapped  : POVERLAPPED): BOOL;

<*EXTERNAL CreateMailslotA:WINAPI*>
PROCEDURE CreateMailslotA (lpName              : PSTR;
                           nMaxMessageSize     : UINT32;
                           lReadTimeout        : UINT32;
                           lpSecurityAttributes: PSECURITY_ATTRIBUTES):HANDLE;

<*EXTERNAL CreateMailslotW:WINAPI*>
PROCEDURE CreateMailslotW (lpName              : PWSTR;
                           nMaxMessageSize     : UINT32;
                           lReadTimeout        : UINT32;
                           lpSecurityAttributes: PSECURITY_ATTRIBUTES):HANDLE;

CONST CreateMailslot = CreateMailslotA;

<*EXTERNAL GetMailslotInfo:WINAPI*>
PROCEDURE GetMailslotInfo (hMailslot       : HANDLE;
                           lpMaxMessageSize: PUINT32;
                           lpNextSize      : PUINT32;
                           lpMessageCount  : PUINT32;
                           lpReadTimeout   : PUINT32  ): BOOL;

<*EXTERNAL SetMailslotInfo:WINAPI*>
PROCEDURE SetMailslotInfo (hMailslot: HANDLE; lReadTimeout: UINT32): BOOL;

<*EXTERNAL MapViewOfFile:WINAPI*>
PROCEDURE MapViewOfFile (hFileMappingObject  : HANDLE;
                         dwDesiredAccess     : UINT32;
                         dwFileOffsetHigh    : UINT32;
                         dwFileOffsetLow     : UINT32;
                         dwNumberOfBytesToMap: SIZE_T  ): PVOID;

<*EXTERNAL FlushViewOfFile:WINAPI*>
PROCEDURE FlushViewOfFile (lpBaseAddress         : PVOID;
                           dwNumberOfBytesToFlush: SIZE_T  ): BOOL;

<*EXTERNAL UnmapViewOfFile:WINAPI*>
PROCEDURE UnmapViewOfFile (lpBaseAddress: PVOID): BOOL;

(* _l Compat Functions *)

<*EXTERNAL lstrcmpA:WINAPI*>
PROCEDURE lstrcmpA (lpString1: PCSTR; lpString2: PCSTR): INT32;
<*EXTERNAL lstrcmpW:WINAPI*>
PROCEDURE lstrcmpW (lpString1: PCWSTR; lpString2: PCWSTR): INT32;

CONST lstrcmp = lstrcmpA;

<*EXTERNAL lstrcmpiA:WINAPI*>
PROCEDURE lstrcmpiA (lpString1: PCSTR; lpString2: PCSTR): INT32;
<*EXTERNAL lstrcmpiW:WINAPI*>
PROCEDURE lstrcmpiW (lpString1: PCWSTR; lpString2: PCWSTR): INT32;

CONST lstrcmpi = lstrcmpiA;

<*EXTERNAL lstrcpynA:WINAPI*>
PROCEDURE lstrcpynA (lpString1: PSTR; lpString2: PCSTR; iMaxLength: INT32): PSTR;
<*EXTERNAL lstrcpynW:WINAPI*>
PROCEDURE lstrcpynW (lpString1: PWSTR; lpString2: PCWSTR; iMaxLenght: INT32): PWSTR;

CONST lstrcpyn = lstrcpynA;

<*EXTERNAL lstrcpyA:WINAPI*>
PROCEDURE lstrcpyA (lpString1: PSTR; lpString2: PCSTR): PSTR;
<*EXTERNAL lstrcpyW:WINAPI*>
PROCEDURE lstrcpyW (lpString1: PWSTR; lpString2: PCWSTR): PWSTR;

CONST lstrcpy = lstrcpyA;

<*EXTERNAL lstrcatA:WINAPI*>
PROCEDURE lstrcatA (lpString1: PSTR; lpString2: PCSTR): PSTR;
<*EXTERNAL lstrcatW:WINAPI*>
PROCEDURE lstrcatW (lpString1: PWSTR; lpString2: PCWSTR): PWSTR;
CONST lstrcat = lstrcatA;

<*EXTERNAL lstrlenA:WINAPI*>
PROCEDURE lstrlenA (lpString: PCSTR): INT32;
<*EXTERNAL lstrlenW:WINAPI*>
PROCEDURE lstrlenW (lpString: PCWSTR): INT32;
CONST lstrlen = lstrlenA;

<*EXTERNAL OpenFile:WINAPI*>
PROCEDURE OpenFile (lpFileName  : PCSTR;
                    lpReOpenBuff: POFSTRUCT;
                    uStyle      : UINT32        ): HFILE;

<*EXTERNAL "_lopen":WINAPI*>
PROCEDURE lopen (lpPathName: PCSTR; iReadWrite: INT32): HFILE;

<*EXTERNAL "_lcreat":WINAPI*>
PROCEDURE lcreat (lpPathName: PCSTR; iAttribute: INT32): HFILE;

<*EXTERNAL "_lread":WINAPI*>
PROCEDURE lread (hFile: HFILE; lpBuffer: PVOID; uBytes: UINT32): UINT32;

<*EXTERNAL "_lwrite":WINAPI*>
PROCEDURE lwrite (hFile: HFILE; lpBuffer: PCSTR; uBytes: UINT32): UINT32;

<*EXTERNAL "_lclose":WINAPI*>
PROCEDURE lclose (hFile: HFILE): HFILE;

<*EXTERNAL "_llseek":WINAPI*>
PROCEDURE llseek (hFile: HFILE; lOffset: INT32; iOrigin: INT32): INT32;


<*EXTERNAL TlsAlloc:WINAPI*>
PROCEDURE TlsAlloc (): UINT32;

CONST
  TLS_OUT_OF_INDEXES    = 16_FFFFFFFF;

(* The values passed to "TlsSetValue" and returned by "TlsGetValue"
   were originally declared "PVOID".  But, since it's clear that they're
   going to stuff the values somewhere hidden to the collector, any attempt
   to pass an address referring to a traced reference would cause disaster.
   So, to avoid the additional check done by the GC wrappers, we declare
   the values as "SIZE_T"s instead.  These routines are called *very frequently*
   by the low-level thread and exception machinery.  Avoiding the extra
   wrapper check is worth it.  *)

<*EXTERNAL TlsGetValue:WINAPI*>
PROCEDURE TlsGetValue (dwTlsIndex: UINT32): SIZE_T;

<*EXTERNAL TlsSetValue:WINAPI*>
PROCEDURE TlsSetValue (dwTlsIndex: UINT32; lpTlsValue: SIZE_T): BOOL;

<*EXTERNAL TlsFree:WINAPI*>
PROCEDURE TlsFree (dwTlsIndex: UINT32): BOOL;

TYPE
  POVERLAPPED_COMPLETION_ROUTINE =
    <*WINAPI*> PROCEDURE (dwErrorCode              : UINT32;
                          dwNumberOfBytesTransfered: UINT32;
                          lpOverlapped             : POVERLAPPED);

  LPOVERLAPPED_COMPLETION_ROUTINE = POVERLAPPED_COMPLETION_ROUTINE; (* compat *)

<*EXTERNAL SleepEx:WINAPI*>
PROCEDURE SleepEx (dwMilliseconds: UINT32; bAlertable: BOOL): UINT32;

<*EXTERNAL WaitForSingleObjectEx:WINAPI*>
PROCEDURE WaitForSingleObjectEx (hHandle       : HANDLE;
                                 dwMilliseconds: UINT32;
                                 bAlertable    : BOOL    ): UINT32;

<*EXTERNAL WaitForMultipleObjectsEx:WINAPI*>
PROCEDURE WaitForMultipleObjectsEx (nCount        : UINT32;
                                    lpHandles     : PHANDLE;
                                    bWaitAll      : BOOL;
                                    dwMilliseconds: UINT32;
                                      bAlertable    : BOOL      ): UINT32;

<*EXTERNAL ReadFileEx:WINAPI*>
PROCEDURE ReadFileEx (hFile               : HANDLE;
                      lpBuffer            : PVOID;
                      nNumberOfBytesToRead: UINT32;
                      lpOverlapped        : POVERLAPPED;
                      lpCompletionRoutine : POVERLAPPED_COMPLETION_ROUTINE): BOOL;

<*EXTERNAL WriteFileEx:WINAPI*>
PROCEDURE WriteFileEx (hFile                : HANDLE;
                       lpBuffer             : PVOID;
                       nNumberOfBytesToWrite: UINT32;
                       lpOverlapped         : POVERLAPPED;
                       lpCompletionRoutine: POVERLAPPED_COMPLETION_ROUTINE): BOOL;

<*EXTERNAL BackupRead:WINAPI*>
PROCEDURE BackupRead (hFile               : HANDLE;
                      lpBuffer            : PUINT8;
                      nNumberOfBytesToRead: UINT32;
                      lpNumberOfBytesRead : PUINT32;
                      bAbort              : BOOL;
                      bProcessSecurity    : BOOL;
                      lpContext           : UNTRACED REF PVOID): BOOL;

<*EXTERNAL BackupSeek:WINAPI*>
PROCEDURE BackupSeek (hFile              : HANDLE;
                      dwLowBytesToSeek   : UINT32;
                      dwHighBytesToSeek  : UINT32;
                      lpdwLowByteSeeked  : PUINT32;
                      lpdwHighByteSeeked : PUINT32;
                      lpContext          : UNTRACED REF PVOID): BOOL;

<*EXTERNAL BackupWrite:WINAPI*>
PROCEDURE BackupWrite (hFile                 : HANDLE;
                       lpBuffer              : PUINT8;
                       nNumberOfBytesToWrite : UINT32;
                       lpNumberOfBytesWritten: PUINT32;
                       bAbort                : BOOL;
                       bProcessSecurity      : BOOL;
                       lpContext             : UNTRACED REF PVOID): BOOL;

(* Dual Mode API below this line.  Dual Mode Structures also included. *)

CONST
  STARTF_USESHOWWINDOW    = 16_00000001;
  STARTF_USESIZE          = 16_00000002;
  STARTF_USEPOSITION      = 16_00000004;
  STARTF_USECOUNTCHARS    = 16_00000008;
  STARTF_USEFILLATTRIBUTE = 16_00000010;
  STARTF_RUNFULLSCREEN    = 16_00000020; (* ignored for non-x86 platforms *)
  STARTF_FORCEONFEEDBACK  = 16_00000040;
  STARTF_FORCEOFFFEEDBACK = 16_00000080;
  STARTF_USESTDHANDLES    = 16_00000100;
  STARTF_USEHOTKEY        = 16_00000200;

TYPE
  LPSTARTUPINFOA = UNTRACED REF STARTUPINFOA;
  STARTUPINFOA = RECORD
    cb             : UINT32;
    lpReserved     : PSTR;
    lpDesktop      : PSTR;
    lpTitle        : PSTR;
    dwX            : UINT32;
    dwY            : UINT32;
    dwXSize        : UINT32;
    dwYSize        : UINT32;
    dwXCountChars  : UINT32;
    dwYCountChars  : UINT32;
    dwFillAttribute: UINT32;
    dwFlags        : UINT32;
    wShowWindow    : UINT16;
    cbReserved2    : UINT16;
    lpReserved2    : PUINT8;
    hStdInput      : HANDLE;
    hStdOutput     : HANDLE;
    hStdError      : HANDLE;
  END;

  LPSTARTUPINFOW = UNTRACED REF STARTUPINFOW;
  STARTUPINFOW = RECORD
    cb             : UINT32;
    lpReserved     : PWSTR;
    lpDesktop      : PWSTR;
    lpTitle        : PWSTR;
    dwX            : UINT32;
    dwY            : UINT32;
    dwXSize        : UINT32;
    dwYSize        : UINT32;
    dwXCountChars  : UINT32;
    dwYCountChars  : UINT32;
    dwFillAttribute: UINT32;
    dwFlags        : UINT32;
    wShowWindow    : UINT16;
    cbReserved2    : UINT16;
    lpReserved2    : PUINT8;
    hStdInput      : HANDLE;
    hStdOutput     : HANDLE;
    hStdError      : HANDLE;
  END;

  STARTUPINFO   = STARTUPINFOA;
  LPSTARTUPINFO = LPSTARTUPINFOA;

CONST SHUTDOWN_NORETRY = 16_00000001;

TYPE
  PWIN32_FIND_DATAA = UNTRACED REF WIN32_FIND_DATAA;
  LPWIN32_FIND_DATAA = UNTRACED REF WIN32_FIND_DATAA;
  WIN32_FIND_DATAA = RECORD
    dwFileAttributes  : UINT32;
    ftCreationTime    : FILETIME;
    ftLastAccessTime  : FILETIME;
    ftLastWriteTime   : FILETIME;
    nFileSizeHigh     : UINT32;
    nFileSizeLow      : UINT32;
    dwReserved0       : UINT32;
    dwReserved1       : UINT32;
    cFileName         : ARRAY [0 .. MAX_PATH - 1] OF CHAR;
    cAlternateFileName: ARRAY [0 .. 14 - 1] OF CHAR;
  END;

  PWIN32_FIND_DATAW = UNTRACED REF WIN32_FIND_DATAW;
  LPWIN32_FIND_DATAW = UNTRACED REF WIN32_FIND_DATAW;
  WIN32_FIND_DATAW = RECORD
    dwFileAttributes  : UINT32;
    ftCreationTime    : FILETIME;
    ftLastAccessTime  : FILETIME;
    ftLastWriteTime   : FILETIME;
    nFileSizeHigh     : UINT32;
    nFileSizeLow      : UINT32;
    dwReserved0       : UINT32;
    dwReserved1       : UINT32;
    cFileName         : ARRAY [0 .. MAX_PATH - 1] OF WCHAR;
    cAlternateFileName: ARRAY [0 .. 14 - 1] OF WCHAR;
  END;

  WIN32_FIND_DATA   = WIN32_FIND_DATAA;
  PWIN32_FIND_DATA  = PWIN32_FIND_DATAA;
  LPWIN32_FIND_DATA = LPWIN32_FIND_DATAA;

<*EXTERNAL CreateMutexA:WINAPI*>
PROCEDURE CreateMutexA (lpMutexAttributes: PSECURITY_ATTRIBUTES;
                        bInitialOwner    : BOOL;
                        lpName           : PSTR                  ): HANDLE;

<*EXTERNAL CreateMutexW:WINAPI*>
PROCEDURE CreateMutexW (lpMutexAttributes: PSECURITY_ATTRIBUTES;
                        bInitialOwner    : BOOL;
                        lpName           : PWSTR                 ): HANDLE;

CONST CreateMutex = CreateMutexA;

<*EXTERNAL OpenMutexA:WINAPI*>
PROCEDURE OpenMutexA (dwDesiredAccess: UINT32;
                      bInheritHandle : BOOL;
                      lpName         : PSTR  ): HANDLE;

<*EXTERNAL OpenMutexW:WINAPI*>
PROCEDURE OpenMutexW (dwDesiredAccess: UINT32;
                      bInheritHandle : BOOL;
                      lpName         : PWSTR ): HANDLE;

CONST OpenMutex = OpenMutexA;

<*EXTERNAL CreateEventA:WINAPI*>
PROCEDURE CreateEventA (lpEventAttributes: PSECURITY_ATTRIBUTES;
                        bManualReset     : BOOL;
                        bInitialState    : BOOL;
                        lpName           : PSTR                  ): HANDLE;

<*EXTERNAL CreateEventW:WINAPI*>
PROCEDURE CreateEventW (lpEventAttributes: PSECURITY_ATTRIBUTES;
                        bManualReset     : BOOL;
                        bInitialState    : BOOL;
                        lpName           : PWSTR                 ): HANDLE;

CONST CreateEvent = CreateEventA;

<*EXTERNAL OpenEventA:WINAPI*>
PROCEDURE OpenEventA (dwDesiredAccess: UINT32;
                      bInheritHandle : BOOL;
                      lpName         : PSTR  ): HANDLE;

<*EXTERNAL OpenEventW:WINAPI*>
PROCEDURE OpenEventW (dwDesiredAccess: UINT32;
                      bInheritHandle : BOOL;
                      lpName         : PWSTR ): HANDLE;

CONST OpenEvent = OpenEventA;

<*EXTERNAL CreateSemaphoreA:WINAPI*>
PROCEDURE CreateSemaphoreA (lpSemaphoreAttributes: PSECURITY_ATTRIBUTES;
                            lInitialCount: INT32;
                            lMaximumCount: INT32;
                            lpName       : PSTR ): HANDLE;

<*EXTERNAL CreateSemaphoreW:WINAPI*>
PROCEDURE CreateSemaphoreW (lpSemaphoreAttributes: PSECURITY_ATTRIBUTES;
                            lInitialCount: INT32;
                            lMaximumCount: INT32;
                            lpName       : PWSTR): HANDLE;

CONST CreateSemaphore = CreateSemaphoreA;

<*EXTERNAL OpenSemaphoreA:WINAPI*>
PROCEDURE OpenSemaphoreA (dwDesiredAccess: UINT32;
                          bInheritHandle : BOOL;
                          lpName         : PSTR  ): HANDLE;

<*EXTERNAL OpenSemaphoreW:WINAPI*>
PROCEDURE OpenSemaphoreW (dwDesiredAccess: UINT32;
                          bInheritHandle : BOOL;
                          lpName         : PWSTR ): HANDLE;

CONST OpenSemaphore = OpenSemaphoreA;

<*EXTERNAL CreateFileMappingA:WINAPI*>
PROCEDURE CreateFileMappingA (hFile: HANDLE;
                              lpFileMappingAttributes: PSECURITY_ATTRIBUTES;
                              flProtect        : UINT32;
                              dwMaximumSizeHigh: UINT32;
                              dwMaximumSizeLow : UINT32;
                              lpName           : PSTR  ): HANDLE;

<*EXTERNAL CreateFileMappingW:WINAPI*>
PROCEDURE CreateFileMappingW (hFile: HANDLE;
                              lpFileMappingAttributes: PSECURITY_ATTRIBUTES;
                              flProtect        : UINT32;
                              dwMaximumSizeHigh: UINT32;
                              dwMaximumSizeLow : UINT32;
                              lpName           : PWSTR ): HANDLE;

CONST CreateFileMapping = CreateFileMappingA;

<*EXTERNAL OpenFileMappingA:WINAPI*>
PROCEDURE OpenFileMappingA (dwDesiredAccess: UINT32;
                            bInheritHandle : BOOL;
                            lpName         : PSTR  ): HANDLE;

<*EXTERNAL OpenFileMappingW:WINAPI*>
PROCEDURE OpenFileMappingW (dwDesiredAccess: UINT32;
                            bInheritHandle : BOOL;
                            lpName         : PWSTR ): HANDLE;

CONST OpenFileMapping = OpenFileMappingA;

<*EXTERNAL GetLogicalDriveStringsA:WINAPI*>
PROCEDURE GetLogicalDriveStringsA(nBufferLength: UINT32; lpBuffer: PSTR):UINT32;

<*EXTERNAL GetLogicalDriveStringsW:WINAPI*>
PROCEDURE GetLogicalDriveStringsW(nBufferLength: UINT32; lpBuffer:PWSTR):UINT32;

CONST GetLogicalDriveStrings = GetLogicalDriveStringsA;

<*EXTERNAL LoadLibraryA:WINAPI*>
PROCEDURE LoadLibraryA (lpLibFileName: PCSTR): HINSTANCE;

<*EXTERNAL LoadLibraryW:WINAPI*>
PROCEDURE LoadLibraryW (lpLibFileName: PCWSTR): HINSTANCE;

CONST LoadLibrary = LoadLibraryA;

<*EXTERNAL LoadLibraryExA:WINAPI*>
PROCEDURE LoadLibraryExA (lpLibFileName: PCSTR;
                          hFile        : HANDLE;
                          dwFlags      : UINT32   ): HINSTANCE;

<*EXTERNAL LoadLibraryExW:WINAPI*>
PROCEDURE LoadLibraryExW (lpLibFileName: PCWSTR;
                          hFile        : HANDLE;
                          dwFlags      : UINT32    ): HINSTANCE;

CONST LoadLibraryEx = LoadLibraryExA;

CONST DONT_RESOLVE_DLL_REFERENCES = 16_00000001;


<*EXTERNAL GetModuleFileNameA:WINAPI*>
PROCEDURE GetModuleFileNameA (hModule   : HINSTANCE;
                              lpFilename: PSTR;
                              nSize     : UINT32      ): UINT32;

<*EXTERNAL GetModuleFileNameW:WINAPI*>
PROCEDURE GetModuleFileNameW (hModule   : HINSTANCE;
                              lpFilename: PWSTR;
                              nSize     : UINT32      ): UINT32;

CONST GetModuleFileName = GetModuleFileNameA;

<*EXTERNAL GetModuleHandleA:WINAPI*>
PROCEDURE GetModuleHandleA (lpModuleName: PCSTR): HMODULE;

<*EXTERNAL GetModuleHandleW:WINAPI*>
PROCEDURE GetModuleHandleW (lpModuleName: PCWSTR): HMODULE;

CONST GetModuleHandle = GetModuleHandleA;

<*EXTERNAL CreateProcessA:WINAPI*>
PROCEDURE CreateProcessA (lpApplicationName   : PCSTR;
                          lpCommandLine       : PCSTR;
                          lpProcessAttributes : PSECURITY_ATTRIBUTES;
                          lpThreadAttributes  : PSECURITY_ATTRIBUTES;
                          bInheritHandles     : BOOL;
                          dwCreationFlags     : UINT32;
                          lpEnvironment       : PVOID;
                          lpCurrentDirectory  : PSTR;
                          lpStartupInfo       : LPSTARTUPINFOA;
                          lpProcessInformation: LPPROCESS_INFORMATION  ): BOOL;

<*EXTERNAL CreateProcessW:WINAPI*>
PROCEDURE CreateProcessW (lpApplicationName   : PCWSTR;
                          lpCommandLine       : PCWSTR;
                          lpProcessAttributes : PSECURITY_ATTRIBUTES;
                          lpThreadAttributes  : PSECURITY_ATTRIBUTES;
                          bInheritHandles     : BOOL;
                          dwCreationFlags     : UINT32;
                          lpEnvironment       : PVOID;
                          lpCurrentDirectory  : PWSTR;
                          lpStartupInfo       : LPSTARTUPINFOW;
                          lpProcessInformation: LPPROCESS_INFORMATION  ): BOOL;

CONST CreateProcess = CreateProcessA;

<*EXTERNAL SetProcessShutdownParameters:WINAPI*>
PROCEDURE SetProcessShutdownParameters (dwLevel: UINT32; dwFlags: UINT32): BOOL;

<*EXTERNAL GetProcessShutdownParameters:WINAPI*>
PROCEDURE GetProcessShutdownParameters (lpdwLevel: PUINT32;
                                        lpdwFlags: PUINT32  ): BOOL;

<*EXTERNAL FatalAppExitA:WINAPI*>
PROCEDURE FatalAppExitA (uAction: UINT32; lpMessageText: PCSTR);

<*EXTERNAL FatalAppExitW:WINAPI*>
PROCEDURE FatalAppExitW (uAction: UINT32; lpMessageText: PCWSTR);

CONST FatalAppExit = FatalAppExitA;

<*EXTERNAL GetStartupInfoA:WINAPI*>
PROCEDURE GetStartupInfoA (lpStartupInfo: LPSTARTUPINFOA);

<*EXTERNAL GetStartupInfoW:WINAPI*>
PROCEDURE GetStartupInfoW (lpStartupInfo: LPSTARTUPINFOW);

CONST GetStartupInfo = GetStartupInfoA;

<*EXTERNAL GetCommandLineA:WINAPI*>
PROCEDURE GetCommandLineA (): PSTR;

<*EXTERNAL GetCommandLineW:WINAPI*>
PROCEDURE GetCommandLineW (): PWSTR;

CONST GetCommandLine = GetCommandLineA;

<*EXTERNAL GetEnvironmentVariableA:WINAPI*>
PROCEDURE GetEnvironmentVariableA (lpName  : PSTR;
                                   lpBuffer: PSTR;
                                   nSize   : UINT32  ): UINT32;

<*EXTERNAL GetEnvironmentVariableW:WINAPI*>
PROCEDURE GetEnvironmentVariableW (lpName  : PWSTR;
                                   lpBuffer: PWSTR;
                                   nSize   : UINT32   ): UINT32;

CONST GetEnvironmentVariable = GetEnvironmentVariableA;

<*EXTERNAL SetEnvironmentVariableA:WINAPI*>
PROCEDURE SetEnvironmentVariableA (lpName: PSTR; lpValue: PSTR): BOOL;

<*EXTERNAL SetEnvironmentVariableW:WINAPI*>
PROCEDURE SetEnvironmentVariableW (lpName: PWSTR; lpValue: PWSTR): BOOL;

CONST SetEnvironmentVariable = SetEnvironmentVariableA;

<*EXTERNAL ExpandEnvironmentStringsA:WINAPI*>
PROCEDURE ExpandEnvironmentStringsA (lpSrc: PCSTR;
                                     lpDst: PSTR;
                                     nSize: UINT32   ): UINT32;

<*EXTERNAL ExpandEnvironmentStringsW:WINAPI*>
PROCEDURE ExpandEnvironmentStringsW (lpSrc: PCWSTR;
                                     lpDst: PWSTR;
                                     nSize: UINT32    ): UINT32;

CONST ExpandEnvironmentStrings = ExpandEnvironmentStringsA;

<*EXTERNAL OutputDebugStringA:WINAPI*>
PROCEDURE OutputDebugStringA (lpOutputString: PCSTR);

<*EXTERNAL OutputDebugStringW:WINAPI*>
PROCEDURE OutputDebugStringW (lpOutputString: PCWSTR);

CONST OutputDebugString = OutputDebugStringA;

<*EXTERNAL FindResourceA:WINAPI*>
PROCEDURE FindResourceA (hModule: HINSTANCE;
                         lpName : PCSTR;
                         lpType : PCSTR     ): HRSRC;

<*EXTERNAL FindResourceW:WINAPI*>
PROCEDURE FindResourceW (hModule: HINSTANCE;
                         lpName : PCWSTR;
                         lpType : PCWSTR    ): HRSRC;

CONST FindResource = FindResourceA;

<*EXTERNAL FindResourceExA:WINAPI*>
PROCEDURE FindResourceExA (hModule  : HINSTANCE;
                           lpType   : PCSTR;
                           lpName   : PCSTR;
                           wLanguage: UINT16       ): HRSRC;

<*EXTERNAL FindResourceExW:WINAPI*>
PROCEDURE FindResourceExW (hModule  : HINSTANCE;
                           lpType   : PCWSTR;
                           lpName   : PCWSTR;
                           wLanguage: UINT16       ): HRSRC;

CONST FindResourceEx = FindResourceExA;

TYPE
  ENUMRESTYPEPROC = <*CALLBACK*> PROCEDURE (hModule: HINSTANCE;
                                            lpType : PTSTR;
                                            lParam : INT32      ): BOOL;

  ENUMRESNAMEPROC = <*CALLBACK*> PROCEDURE (hModule: HINSTANCE;
                                            lpType : PCTSTR;
                                            lpName : PTSTR;
                                            lParam : INT32      ): BOOL;

  ENUMRESLANGPROC = <*CALLBACK*> PROCEDURE (hModule  : HINSTANCE;
                                            lpType   : PCTSTR;
                                            lpName   : PCTSTR;
                                            wLanguage: UINT16;
                                            lParam   : INT32       ): BOOL;


<*EXTERNAL EnumResourceTypesA:WINAPI*>
PROCEDURE EnumResourceTypesA (hModule   : HINSTANCE;
                              lpEnumFunc: ENUMRESTYPEPROC;
                              lParam    : INT32             ): BOOL;

<*EXTERNAL EnumResourceTypesW:WINAPI*>
PROCEDURE EnumResourceTypesW (hModule   : HINSTANCE;
                              lpEnumFunc: ENUMRESTYPEPROC;
                              lParam    : INT32             ): BOOL;

CONST EnumResourceTypes = EnumResourceTypesA;


<*EXTERNAL EnumResourceNamesA:WINAPI*>
PROCEDURE EnumResourceNamesA (hModule   : HINSTANCE;
                              lpType    : PCSTR;
                              lpEnumFunc: ENUMRESNAMEPROC;
                              lParam    : INT32             ): BOOL;

<*EXTERNAL EnumResourceNamesW:WINAPI*>
PROCEDURE EnumResourceNamesW (hModule   : HINSTANCE;
                              lpType    : PCWSTR;
                              lpEnumFunc: ENUMRESNAMEPROC;
                              lParam    : INT32             ): BOOL;

CONST EnumResourceNames = EnumResourceNamesA;

<*EXTERNAL EnumResourceLanguagesA:WINAPI*>
PROCEDURE EnumResourceLanguagesA (hModule   : HINSTANCE;
                                  lpType    : PCSTR;
                                  lpName    : PCSTR;
                                  lpEnumFunc: ENUMRESLANGPROC;
                                  lParam    : INT32             ): BOOL;

<*EXTERNAL EnumResourceLanguagesW:WINAPI*>
PROCEDURE EnumResourceLanguagesW (hModule   : HINSTANCE;
                                  lpType    : PCWSTR;
                                  lpName    : PCWSTR;
                                  lpEnumFunc: ENUMRESLANGPROC;
                                  lParam    : INT32             ): BOOL;

CONST EnumResourceLanguages = EnumResourceLanguagesA;

<*EXTERNAL BeginUpdateResourceA:WINAPI*>
PROCEDURE BeginUpdateResourceA (pFileName               : PSTR;
                                bDeleteExistingResources: BOOL   ): HANDLE;

<*EXTERNAL BeginUpdateResourceW:WINAPI*>
PROCEDURE BeginUpdateResourceW (pFileName               : PWSTR;
                                bDeleteExistingResources: BOOL    ): HANDLE;

CONST BeginUpdateResource = BeginUpdateResourceA;

<*EXTERNAL UpdateResourceA:WINAPI*>
PROCEDURE UpdateResourceA (hUpdate  : HANDLE;
                           lpType   : PSTR;
                           lpName   : PSTR;
                           wLanguage: UINT16;
                           lpData   : PVOID;
                           cbData   : UINT32   ): BOOL;

<*EXTERNAL UpdateResourceW:WINAPI*>
PROCEDURE UpdateResourceW (hUpdate  : HANDLE;
                           lpType   : PWSTR;
                           lpName   : PWSTR;
                           wLanguage: UINT16;
                           lpData   : PVOID;
                           cbData   : UINT32   ): BOOL;

CONST UpdateResource = UpdateResourceA;

<*EXTERNAL EndUpdateResourceA:WINAPI*>
PROCEDURE EndUpdateResourceA (hUpdate: HANDLE; fDiscard: BOOL): BOOL;

<*EXTERNAL EndUpdateResourceW:WINAPI*>
PROCEDURE EndUpdateResourceW (hUpdate: HANDLE; fDiscard: BOOL): BOOL;

CONST EndUpdateResource = EndUpdateResourceA;

<*EXTERNAL GlobalAddAtomA:WINAPI*>
PROCEDURE GlobalAddAtomA (lpString: PCSTR): ATOM;

<*EXTERNAL GlobalAddAtomW:WINAPI*>
PROCEDURE GlobalAddAtomW (lpString: PCWSTR): ATOM;

CONST GlobalAddAtom = GlobalAddAtomA;

<*EXTERNAL GlobalFindAtomA:WINAPI*>
PROCEDURE GlobalFindAtomA (lpString: PCSTR): ATOM;

<*EXTERNAL GlobalFindAtomW:WINAPI*>
PROCEDURE GlobalFindAtomW (lpString: PCWSTR): ATOM;

CONST GlobalFindAtom = GlobalFindAtomA;

<*EXTERNAL GlobalGetAtomNameA:WINAPI*>
PROCEDURE GlobalGetAtomNameA (nAtom: ATOM; lpBuffer: PSTR; nSize: INT32): UINT32;

<*EXTERNAL GlobalGetAtomNameW:WINAPI*>
PROCEDURE GlobalGetAtomNameW (nAtom: ATOM; lpBuffer: PWSTR; nSize: INT32): UINT32;

CONST GlobalGetAtomName = GlobalGetAtomNameA;

<*EXTERNAL AddAtomA:WINAPI*>
PROCEDURE AddAtomA (lpString: PCSTR): ATOM;

<*EXTERNAL AddAtomW:WINAPI*>
PROCEDURE AddAtomW (lpString: PCWSTR): ATOM;

CONST AddAtom = AddAtomA;

<*EXTERNAL FindAtomA:WINAPI*>
PROCEDURE FindAtomA (lpString: PCSTR): ATOM;

<*EXTERNAL FindAtomW:WINAPI*>
PROCEDURE FindAtomW (lpString: PCWSTR): ATOM;

CONST FindAtom = FindAtomA;

<*EXTERNAL GetAtomNameA:WINAPI*>
PROCEDURE GetAtomNameA (nAtom: ATOM; lpBuffer: PSTR; nSize: INT32): UINT32;

<*EXTERNAL GetAtomNameW:WINAPI*>
PROCEDURE GetAtomNameW (nAtom: ATOM; lpBuffer: PWSTR; nSize: INT32): UINT32;

CONST GetAtomName = GetAtomNameA;

<*EXTERNAL GetProfileIntA:WINAPI*>
PROCEDURE GetProfileIntA (lpAppName: PCSTR;
                          lpKeyName: PCSTR;
                          nDefault : UINT32   ): UINT32;

<*EXTERNAL GetProfileIntW:WINAPI*>
PROCEDURE GetProfileIntW (lpAppName: PCWSTR;
                          lpKeyName: PCWSTR;
                          nDefault : UINT32    ): UINT32;

CONST GetProfileInt = GetProfileIntA;

<*EXTERNAL GetProfileStringA:WINAPI*>
PROCEDURE GetProfileStringA (lpAppName       : PCSTR;
                             lpKeyName       : PCSTR;
                             lpDefault       : PCSTR;
                             lpReturnedString: PSTR;
                             nSize           : UINT32   ): UINT32;

<*EXTERNAL GetProfileStringW:WINAPI*>
PROCEDURE GetProfileStringW (lpAppName       : PCWSTR;
                             lpKeyName       : PCWSTR;
                             lpDefault       : PCWSTR;
                             lpReturnedString: PWSTR;
                             nSize           : UINT32    ): UINT32;

CONST GetProfileString = GetProfileStringA;

<*EXTERNAL WriteProfileStringA:WINAPI*>
PROCEDURE WriteProfileStringA (lpAppName: PCSTR;
                               lpKeyName: PCSTR;
                               lpString : PCSTR  ): BOOL;

<*EXTERNAL WriteProfileStringW:WINAPI*>
PROCEDURE WriteProfileStringW (lpAppName: PCWSTR;
                               lpKeyName: PCWSTR;
                               lpString : PCWSTR  ): BOOL;

CONST WriteProfileString = WriteProfileStringA;

<*EXTERNAL GetProfileSectionA:WINAPI*>
PROCEDURE GetProfileSectionA (lpAppName       : PCSTR;
                              lpReturnedString: PSTR;
                              nSize           : UINT32   ): UINT32;

<*EXTERNAL GetProfileSectionW:WINAPI*>
PROCEDURE GetProfileSectionW (lpAppName       : PCWSTR;
                              lpReturnedString: PWSTR;
                              nSize           : UINT32    ): UINT32;

CONST GetProfileSection = GetProfileSectionA;

<*EXTERNAL WriteProfileSectionA:WINAPI*>
PROCEDURE WriteProfileSectionA (lpAppName: PCSTR; lpString: PCSTR): BOOL;

<*EXTERNAL WriteProfileSectionW:WINAPI*>
PROCEDURE WriteProfileSectionW (lpAppName: PCWSTR; lpString: PCWSTR): BOOL;

CONST WriteProfileSection = WriteProfileSectionA;

<*EXTERNAL GetPrivateProfileIntA:WINAPI*>
PROCEDURE GetPrivateProfileIntA (lpAppName : PCSTR;
                                 lpKeyName : PCSTR;
                                 nDefault  : UINT32;
                                 lpFileName: PCSTR  ): UINT32;

<*EXTERNAL GetPrivateProfileIntW:WINAPI*>
PROCEDURE GetPrivateProfileIntW (lpAppName : PCWSTR;
                                 lpKeyName : PCWSTR;
                                 nDefault  : UINT32;
                                 lpFileName: PCWSTR  ): UINT32;

CONST GetPrivateProfileInt = GetPrivateProfileIntA;

<*EXTERNAL GetPrivateProfileStringA:WINAPI*>
PROCEDURE GetPrivateProfileStringA (lpAppName       : PCSTR;
                                    lpKeyName       : PCSTR;
                                    lpDefault       : PCSTR;
                                    lpReturnedString: PSTR;
                                    nSize           : UINT32;
                                    lpFileName      : PCSTR  ): UINT32;

<*EXTERNAL GetPrivateProfileStringW:WINAPI*>
PROCEDURE GetPrivateProfileStringW (lpAppName       : PCWSTR;
                                    lpKeyName       : PCWSTR;
                                    lpDefault       : PCWSTR;
                                    lpReturnedString: PWSTR;
                                    nSize           : UINT32;
                                    lpFileName      : PCWSTR  ): UINT32;

CONST GetPrivateProfileString = GetPrivateProfileStringA;

<*EXTERNAL WritePrivateProfileStringA:WINAPI*>
PROCEDURE WritePrivateProfileStringA (lpAppName : PCSTR;
                                      lpKeyName : PCSTR;
                                      lpString  : PCSTR;
                                      lpFileName: PCSTR  ): BOOL;

<*EXTERNAL WritePrivateProfileStringW:WINAPI*>
PROCEDURE WritePrivateProfileStringW (lpAppName : PCWSTR;
                                      lpKeyName : PCWSTR;
                                      lpString  : PCWSTR;
                                      lpFileName: PCWSTR  ): BOOL;

CONST WritePrivateProfileString = WritePrivateProfileStringA;

<*EXTERNAL GetPrivateProfileSectionA:WINAPI*>
PROCEDURE GetPrivateProfileSectionA (lpAppName       : PCSTR;
                                     lpReturnedString: PSTR;
                                     nSize           : UINT32;
                                     lpFileName      : PCSTR  ): UINT32;

<*EXTERNAL GetPrivateProfileSectionW:WINAPI*>
PROCEDURE GetPrivateProfileSectionW (lpAppName       : PCWSTR;
                                     lpReturnedString: PWSTR;
                                     nSize           : UINT32;
                                     lpFileName      : PCWSTR  ): UINT32;

CONST GetPrivateProfileSection = GetPrivateProfileSectionA;

<*EXTERNAL WritePrivateProfileSectionA:WINAPI*>
PROCEDURE WritePrivateProfileSectionA (lpAppName : PCSTR;
                                       lpString  : PCSTR;
                                       lpFileName: PCSTR  ): BOOL;

<*EXTERNAL WritePrivateProfileSectionW:WINAPI*>
PROCEDURE WritePrivateProfileSectionW (lpAppName : PCWSTR;
                                       lpString  : PCWSTR;
                                       lpFileName: PCWSTR  ): BOOL;

CONST WritePrivateProfileSection = WritePrivateProfileSectionA;

<*EXTERNAL GetPrivateProfileSectionNamesA:WINAPI*>
PROCEDURE GetPrivateProfileSectionNamesA (lpszReturnBuffer : PSTR;
                                          nSize            : UINT32;
                                          lpFileName       : PCSTR): UINT32;

<*EXTERNAL GetPrivateProfileSectionNamesW:WINAPI*>
PROCEDURE GetPrivateProfileSectionNamesW (lpszReturnBuffer : PWSTR;
                                          nSize            : UINT32;
                                          lpFileName       : PCWSTR): UINT32;

CONST GetPrivateProfileSectionNames = GetPrivateProfileSectionNamesA;

<*EXTERNAL GetPrivateProfileStructA:WINAPI*>
PROCEDURE GetPrivateProfileStructA (lpszSection : PCSTR;
                                    lpszKey     : PCSTR;
                                    lpStruct    : PVOID;
                                    uSizeStruct : UINT32;
                                    szFile      : PCSTR): BOOL;

<*EXTERNAL GetPrivateProfileStructW:WINAPI*>
PROCEDURE GetPrivateProfileStructW (lpszSection : PCWSTR;
                                    lpszKey     : PCWSTR;
                                    lpStruct    : PVOID;
                                    uSizeStruct : UINT32;
                                    szFile      : PCWSTR): BOOL;

CONST GetPrivateProfileStruct = GetPrivateProfileStructA;

<*EXTERNAL WritePrivateProfileStructA:WINAPI*>
PROCEDURE WritePrivateProfileStructA (lpszSection : PCSTR;
                                      lpszKey     : PCSTR;
                                      lpStruct    : PVOID;
                                      uSizeStruct : UINT32;
                                      szFile      : PCSTR): BOOL;

<*EXTERNAL WritePrivateProfileStructW:WINAPI*>
PROCEDURE WritePrivateProfileStructW (lpszSection : PCWSTR;
                                      lpszKey     : PCWSTR;
                                      lpStruct    : PVOID;
                                      uSizeStruct : UINT32;
                                      szFile      : PCWSTR): BOOL;

CONST WritePrivateProfileStruct = WritePrivateProfileStructA;

<*EXTERNAL GetDriveTypeA:WINAPI*>
PROCEDURE GetDriveTypeA (lpRootPathName: PSTR): UINT32;

<*EXTERNAL GetDriveTypeW:WINAPI*>
PROCEDURE GetDriveTypeW (lpRootPathName: PWSTR): UINT32;

CONST GetDriveType = GetDriveTypeA;

<*EXTERNAL GetSystemDirectoryA:WINAPI*>
PROCEDURE GetSystemDirectoryA (lpBuffer: PSTR; uSize: UINT32): UINT32;

<*EXTERNAL GetSystemDirectoryW:WINAPI*>
PROCEDURE GetSystemDirectoryW (lpBuffer: PWSTR; uSize: UINT32): UINT32;

CONST GetSystemDirectory = GetSystemDirectoryA;

<*EXTERNAL GetTempPathA:WINAPI*>
PROCEDURE GetTempPathA (nBufferLength: UINT32; lpBuffer: PSTR): UINT32;

<*EXTERNAL GetTempPathW:WINAPI*>
PROCEDURE GetTempPathW (nBufferLength: UINT32; lpBuffer: PWSTR): UINT32;

CONST GetTempPath = GetTempPathA;

<*EXTERNAL GetTempFileNameA:WINAPI*>
PROCEDURE GetTempFileNameA (lpPathName    : PCSTR;
                            lpPrefixString: PCSTR;
                            uUnique       : UINT32;
                            lpTempFileName: PSTR   ): UINT32;

<*EXTERNAL GetTempFileNameW:WINAPI*>
PROCEDURE GetTempFileNameW (lpPathName    : PCWSTR;
                            lpPrefixString: PCWSTR;
                            uUnique       : UINT32;
                            lpTempFileName: PWSTR   ): UINT32;

CONST GetTempFileName = GetTempFileNameA;

<*EXTERNAL GetWindowsDirectoryA:WINAPI*>
PROCEDURE GetWindowsDirectoryA (lpBuffer: PSTR; uSize: UINT32): UINT32;

<*EXTERNAL GetWindowsDirectoryW:WINAPI*>
PROCEDURE GetWindowsDirectoryW (lpBuffer: PWSTR; uSize: UINT32): UINT32;

CONST GetWindowsDirectory = GetWindowsDirectoryA;

<*EXTERNAL SetCurrentDirectoryA:WINAPI*>
PROCEDURE SetCurrentDirectoryA (lpPathName: PSTR): BOOL;

<*EXTERNAL SetCurrentDirectoryW:WINAPI*>
PROCEDURE SetCurrentDirectoryW (lpPathName: PWSTR): BOOL;

CONST SetCurrentDirectory = SetCurrentDirectoryA;

<*EXTERNAL GetCurrentDirectoryA:WINAPI*>
PROCEDURE GetCurrentDirectoryA (nBufferLength: UINT32; lpBuffer: PSTR): UINT32;

<*EXTERNAL GetCurrentDirectoryW:WINAPI*>
PROCEDURE GetCurrentDirectoryW (nBufferLength: UINT32; lpBuffer: PWSTR): UINT32;

CONST GetCurrentDirectory = GetCurrentDirectoryA;

<*EXTERNAL GetDiskFreeSpaceA:WINAPI*>
PROCEDURE GetDiskFreeSpaceA (lpRootPathName         : PSTR;
                             lpSectorsPerCluster    : PUINT32;
                             lpBytesPerSector       : PUINT32;
                             lpNumberOfFreeClusters : PUINT32;
                             lpTotalNumberOfClusters: PUINT32  ): BOOL;

<*EXTERNAL GetDiskFreeSpaceW:WINAPI*>
PROCEDURE GetDiskFreeSpaceW (lpRootPathName         : PWSTR;
                             lpSectorsPerCluster    : PUINT32;
                             lpBytesPerSector       : PUINT32;
                             lpNumberOfFreeClusters : PUINT32;
                             lpTotalNumberOfClusters: PUINT32  ): BOOL;

CONST GetDiskFreeSpace = GetDiskFreeSpaceA;

<*EXTERNAL CreateDirectoryA:WINAPI*>
PROCEDURE CreateDirectoryA (lpPathName          : PSTR;
                            lpSecurityAttributes: PSECURITY_ATTRIBUTES): BOOL;

<*EXTERNAL CreateDirectoryW:WINAPI*>
PROCEDURE CreateDirectoryW (lpPathName          : PWSTR;
                            lpSecurityAttributes: PSECURITY_ATTRIBUTES): BOOL;

CONST CreateDirectory = CreateDirectoryA;

<*EXTERNAL CreateDirectoryExA:WINAPI*>
PROCEDURE CreateDirectoryExA (lpTemplateDirectory : PCSTR;
                              lpNewDirectory      : PCSTR;
                              lpSecurityAttributes: PSECURITY_ATTRIBUTES): BOOL;

<*EXTERNAL CreateDirectoryExW:WINAPI*>
PROCEDURE CreateDirectoryExW (lpTemplateDirectory : PCWSTR;
                              lpNewDirectory      : PCWSTR;
                              lpSecurityAttributes: PSECURITY_ATTRIBUTES): BOOL;

CONST CreateDirectoryEx = CreateDirectoryExA;

<*EXTERNAL RemoveDirectoryA:WINAPI*>
PROCEDURE RemoveDirectoryA (lpPathName: PSTR): BOOL;

<*EXTERNAL RemoveDirectoryW:WINAPI*>
PROCEDURE RemoveDirectoryW (lpPathName: PWSTR): BOOL;

CONST RemoveDirectory = RemoveDirectoryA;

<*EXTERNAL GetFullPathNameA:WINAPI*>
PROCEDURE GetFullPathNameA (lpFileName   : PCSTR;
                            nBufferLength: UINT32;
                            lpBuffer     : PSTR;
                            lpFilePart   : UNTRACED REF PSTR): UINT32;

<*EXTERNAL GetFullPathNameW:WINAPI*>
PROCEDURE GetFullPathNameW (lpFileName   : PCWSTR;
                            nBufferLength: UINT32;
                            lpBuffer     : PWSTR;
                            lpFilePart   : UNTRACED REF PWSTR): UINT32;

CONST GetFullPathName = GetFullPathNameA;

CONST
  DDD_RAW_TARGET_PATH       = 16_00000001;
  DDD_REMOVE_DEFINITION     = 16_00000002;
  DDD_EXACT_MATCH_ON_REMOVE = 16_00000004;

<*EXTERNAL DefineDosDeviceA:WINAPI*>
PROCEDURE DefineDosDeviceA (dwFlags      : UINT32;
                            lpDeviceName : PCSTR;
                            lpTargetPath : PCSTR): BOOL;

<*EXTERNAL DefineDosDeviceW:WINAPI*>
PROCEDURE DefineDosDeviceW (dwFlags      : UINT32;
                            lpDeviceName : PCWSTR;
                            lpTargetPath : PCWSTR): BOOL;

CONST DefineDosDevice = DefineDosDeviceA;

<*EXTERNAL QueryDosDeviceA:WINAPI*>
PROCEDURE QueryDosDeviceA (lpDeviceName : PCSTR;
                           lpTargetPath : PSTR;
                           ucchMax      : UINT32): UINT32;

<*EXTERNAL QueryDosDeviceW:WINAPI*>
PROCEDURE QueryDosDeviceW (lpDeviceName : PCWSTR;
                           lpTargetPath : PWSTR;
                           ucchMax      : UINT32): UINT32;

CONST QueryDosDevice = QueryDosDeviceA;

CONST EXPAND_LOCAL_DRIVES = TRUE;
(* #define EXPAND_LOCAL_DRIVES *)

<*EXTERNAL CreateFileA:WINAPI*>
PROCEDURE CreateFileA (lpFileName           : PCSTR;
                       dwDesiredAccess      : UINT32;
                       dwShareMode          : UINT32;
                       lpSecurityAttributes : PSECURITY_ATTRIBUTES;
                       dwCreationDisposition: UINT32;
                       dwFlagsAndAttributes : UINT32;
                       hTemplateFile        : HANDLE                 ): HANDLE;

<*EXTERNAL CreateFileW:WINAPI*>
PROCEDURE CreateFileW (lpFileName           : PCWSTR;
                       dwDesiredAccess      : UINT32;
                       dwShareMode          : UINT32;
                       lpSecurityAttributes : PSECURITY_ATTRIBUTES;
                       dwCreationDisposition: UINT32;
                       dwFlagsAndAttributes : UINT32;
                       hTemplateFile        : HANDLE                 ): HANDLE;

CONST CreateFile = CreateFileA;

<*EXTERNAL SetFileAttributesA:WINAPI*>
PROCEDURE SetFileAttributesA (lpFileName: PSTR; dwFileAttributes: UINT32):BOOL;

<*EXTERNAL SetFileAttributesW:WINAPI*>
PROCEDURE SetFileAttributesW (lpFileName: PWSTR; dwFileAttributes:UINT32):BOOL;

CONST SetFileAttributes = SetFileAttributesA;

<*EXTERNAL GetFileAttributesA:WINAPI*>
PROCEDURE GetFileAttributesA (lpFileName: PSTR): UINT32;

<*EXTERNAL GetFileAttributesW:WINAPI*>
PROCEDURE GetFileAttributesW (lpFileName: PWSTR): UINT32;

CONST GetFileAttributes = GetFileAttributesA;

<*EXTERNAL GetCompressedFileSizeA:WINAPI*>
PROCEDURE GetCompressedFileSizeA (lpFileName     : PCSTR;
                                  lpFileSizeHigh : PUINT32): UINT32;

<*EXTERNAL GetCompressedFileSizeW:WINAPI*>
PROCEDURE GetCompressedFileSizeW (lpFileName     : PCWSTR;
                                  lpFileSizeHigh : PUINT32): UINT32;

CONST GetCompressedFileSize = GetCompressedFileSizeA;

<*EXTERNAL DeleteFileA:WINAPI*>
PROCEDURE DeleteFileA (lpFileName: PSTR): BOOL;

<*EXTERNAL DeleteFileW:WINAPI*>
PROCEDURE DeleteFileW (lpFileName: PWSTR): BOOL;

CONST DeleteFile = DeleteFileA;

<*EXTERNAL FindFirstFileA:WINAPI*>
PROCEDURE FindFirstFileA (lpFileName    : PSTR;
                          lpFindFileData: LPWIN32_FIND_DATAA): HANDLE;

<*EXTERNAL FindFirstFileW:WINAPI*>
PROCEDURE FindFirstFileW (lpFileName    : PWSTR;
                          lpFindFileData: LPWIN32_FIND_DATAW): HANDLE;

CONST FindFirstFile = FindFirstFileA;

<*EXTERNAL FindNextFileA:WINAPI*>
PROCEDURE FindNextFileA (hFindFile     : HANDLE;
                         lpFindFileData: LPWIN32_FIND_DATAA): BOOL;

<*EXTERNAL FindNextFileW:WINAPI*>
PROCEDURE FindNextFileW (hFindFile     : HANDLE;
                         lpFindFileData: LPWIN32_FIND_DATAW): BOOL;

CONST FindNextFile = FindNextFileA;

<*EXTERNAL SearchPathA:WINAPI*>
PROCEDURE SearchPathA (lpPath       : PCSTR;
                       lpFileName   : PCSTR;
                       lpExtension  : PCSTR;
                       nBufferLength: UINT32;
                       lpBuffer     : PSTR;
                       lpFilePart   : UNTRACED REF PSTR): UINT32;

<*EXTERNAL SearchPathW:WINAPI*>
PROCEDURE SearchPathW (lpPath       : PCWSTR;
                       lpFileName   : PCWSTR;
                       lpExtension  : PCWSTR;
                       nBufferLength: UINT32;
                       lpBuffer     : PWSTR;
                       lpFilePart   : UNTRACED REF PWSTR): UINT32;

CONST SearchPath = SearchPathA;

<*EXTERNAL CopyFileA:WINAPI*>
PROCEDURE CopyFileA (lpExistingFileName: PSTR;
                     lpNewFileName     : PSTR;
                     bFailIfExists     : BOOL   ): BOOL;

<*EXTERNAL CopyFileW:WINAPI*>
PROCEDURE CopyFileW (lpExistingFileName: PWSTR;
                     lpNewFileName     : PWSTR;
                     bFailIfExists     : BOOL    ): BOOL;

CONST CopyFile = CopyFileA;

<*EXTERNAL MoveFileA:WINAPI*>
PROCEDURE MoveFileA (lpExistingFileName: PSTR; lpNewFileName: PSTR): BOOL;
<*EXTERNAL MoveFileW:WINAPI*>

PROCEDURE MoveFileW (lpExistingFileName: PWSTR; lpNewFileName: PWSTR): BOOL;
CONST MoveFile = MoveFileA;

<*EXTERNAL MoveFileExA:WINAPI*>
PROCEDURE MoveFileExA (lpExistingFileName: PSTR;
                       lpNewFileName     : PSTR;
                       dwFlags           : UINT32  ): BOOL;

<*EXTERNAL MoveFileExW:WINAPI*>
PROCEDURE MoveFileExW (lpExistingFileName: PWSTR;
                       lpNewFileName     : PWSTR;
                       dwFlags           : UINT32   ): BOOL;

CONST MoveFileEx = MoveFileExA;

CONST
  MOVEFILE_REPLACE_EXISTING   = 16_00000001;
  MOVEFILE_COPY_ALLOWED       = 16_00000002;
  MOVEFILE_DELAY_UNTIL_REBOOT = 16_00000004;

<*EXTERNAL CreateNamedPipeA:WINAPI*>
PROCEDURE CreateNamedPipeA (lpName              : PSTR;
                            dwOpenMode          : UINT32;
                            dwPipeMode          : UINT32;
                            nMaxInstances       : UINT32;
                            nOutBufferSize      : UINT32;
                            nInBufferSize       : UINT32;
                            nDefaultTimeOut     : UINT32;
                          lpSecurityAttributes: PSECURITY_ATTRIBUTES): HANDLE;

<*EXTERNAL CreateNamedPipeW:WINAPI*>
PROCEDURE CreateNamedPipeW (lpName              : PWSTR;
                            dwOpenMode          : UINT32;
                            dwPipeMode          : UINT32;
                            nMaxInstances       : UINT32;
                            nOutBufferSize      : UINT32;
                            nInBufferSize       : UINT32;
                            nDefaultTimeOut     : UINT32;
                          lpSecurityAttributes: PSECURITY_ATTRIBUTES): HANDLE;

CONST CreateNamedPipe = CreateNamedPipeA;

<*EXTERNAL GetNamedPipeHandleStateA:WINAPI*>
PROCEDURE GetNamedPipeHandleStateA (hNamedPipe          : HANDLE;
                                    lpState             : PUINT32;
                                    lpCurInstances      : PUINT32;
                                    lpMaxCollectionCount: PUINT32;
                                    lpCollectDataTimeout: PUINT32;
                                    lpUserName          : PSTR;
                                    nMaxUserNameSize    : UINT32    ): BOOL;

<*EXTERNAL GetNamedPipeHandleStateW:WINAPI*>
PROCEDURE GetNamedPipeHandleStateW (hNamedPipe          : HANDLE;
                                    lpState             : PUINT32;
                                    lpCurInstances      : PUINT32;
                                    lpMaxCollectionCount: PUINT32;
                                    lpCollectDataTimeout: PUINT32;
                                    lpUserName          : PWSTR;
                                    nMaxUserNameSize    : UINT32    ): BOOL;

CONST GetNamedPipeHandleState = GetNamedPipeHandleStateA;

<*EXTERNAL CallNamedPipeA:WINAPI*>
PROCEDURE CallNamedPipeA (lpNamedPipeName: PSTR;
                          lpInBuffer     : PVOID;
                          nInBufferSize  : UINT32;
                          lpOutBuffer    : PVOID;
                          nOutBufferSize : UINT32;
                          lpBytesRead    : PUINT32;
                          nTimeOut       : UINT32    ): BOOL;

<*EXTERNAL CallNamedPipeW:WINAPI*>
PROCEDURE CallNamedPipeW (lpNamedPipeName: PWSTR;
                          lpInBuffer     : PVOID;
                          nInBufferSize  : UINT32;
                          lpOutBuffer    : PVOID;
                          nOutBufferSize : UINT32;
                          lpBytesRead    : PUINT32;
                          nTimeOut       : UINT32    ): BOOL;

CONST CallNamedPipe = CallNamedPipeA;

<*EXTERNAL WaitNamedPipeA:WINAPI*>
PROCEDURE WaitNamedPipeA (lpNamedPipeName: PSTR; nTimeOut: UINT32): BOOL;

<*EXTERNAL WaitNamedPipeW:WINAPI*>
PROCEDURE WaitNamedPipeW (lpNamedPipeName: PWSTR; nTimeOut: UINT32): BOOL;

CONST WaitNamedPipe = WaitNamedPipeA;

<*EXTERNAL SetVolumeLabelA:WINAPI*>
PROCEDURE SetVolumeLabelA (lpRootPathName : PCSTR;
                           lpVolumeName   : PCSTR): BOOL;

<*EXTERNAL SetVolumeLabelW:WINAPI*>
PROCEDURE SetVolumeLabelW (lpRootPathName : PCWSTR;
                           lpVolumeName   : PCWSTR): BOOL;

CONST SetVolumeLabel = SetVolumeLabelA;

<*EXTERNAL SetFileApisToOEM:WINAPI*>
PROCEDURE SetFileApisToOEM ();

<*EXTERNAL SetFileApisToANSI:WINAPI*>
PROCEDURE SetFileApisToANSI ();

<*EXTERNAL AreFileApisANSI:WINAPI*>
PROCEDURE AreFileApisANSI (): BOOL;

<*EXTERNAL GetVolumeInformationA:WINAPI*>
PROCEDURE GetVolumeInformationA (lpRootPathName          : PSTR;
                                 lpVolumeNameBuffer      : PSTR;
                                 nVolumeNameSize         : UINT32;
                                 lpVolumeSerialNumber    : PUINT32;
                                 lpMaximumComponentLength: PUINT32;
                                 lpFileSystemFlags       : PUINT32;
                                 lpFileSystemNameBuffer  : PSTR;
                                 nFileSystemNameSize     : UINT32    ): BOOL;

<*EXTERNAL GetVolumeInformationW:WINAPI*>
PROCEDURE GetVolumeInformationW (lpRootPathName          : PWSTR;
                                 lpVolumeNameBuffer      : PWSTR;
                                 nVolumeNameSize         : UINT32;
                                 lpVolumeSerialNumber    : PUINT32;
                                 lpMaximumComponentLength: PUINT32;
                                 lpFileSystemFlags       : PUINT32;
                                 lpFileSystemNameBuffer  : PWSTR;
                                 nFileSystemNameSize     : UINT32    ): BOOL;

CONST GetVolumeInformation = GetVolumeInformationA;

(* Event logging APIs *)

<*EXTERNAL ClearEventLogA:WINAPI*>
PROCEDURE ClearEventLogA (hEventLog: HANDLE; lpBackupFileName: PSTR): BOOL;

<*EXTERNAL ClearEventLogW:WINAPI*>
PROCEDURE ClearEventLogW (hEventLog: HANDLE; lpBackupFileName: PWSTR): BOOL;

CONST ClearEventLog = ClearEventLogA;

<*EXTERNAL BackupEventLogA:WINAPI*>
PROCEDURE BackupEventLogA (hEventLog: HANDLE; lpBackupFileName: PSTR): BOOL;

<*EXTERNAL BackupEventLogW:WINAPI*>
PROCEDURE BackupEventLogW (hEventLog: HANDLE; lpBackupFileName: PWSTR): BOOL;

CONST BackupEventLog = BackupEventLogA;

<*EXTERNAL CloseEventLog:WINAPI*>
PROCEDURE CloseEventLog (hEventLog: HANDLE): BOOL;

<*EXTERNAL DeregisterEventSource:WINAPI*>
PROCEDURE DeregisterEventSource (hEventLog: HANDLE): BOOL;

<*EXTERNAL GetNumberOfEventLogRecords:WINAPI*>
PROCEDURE GetNumberOfEventLogRecords (hEventLog      : HANDLE;
                                      NumberOfRecords: PUINT32  ): BOOL;

<*EXTERNAL GetOldestEventLogRecord:WINAPI*>
PROCEDURE GetOldestEventLogRecord (hEventLog   : HANDLE;
                                   OldestRecord: PUINT32  ): BOOL;

<*EXTERNAL OpenEventLogA:WINAPI*>
PROCEDURE OpenEventLogA (lpUNCServerName: PSTR; lpSourceName: PSTR): HANDLE;

<*EXTERNAL OpenEventLogW:WINAPI*>
PROCEDURE OpenEventLogW (lpUNCServerName: PWSTR; lpSourceName: PWSTR):HANDLE;

CONST OpenEventLog = OpenEventLogA;

<*EXTERNAL RegisterEventSourceA:WINAPI*>
PROCEDURE RegisterEventSourceA (lpUNCServerName: PSTR;
                                lpSourceName   : PSTR  ): HANDLE;

<*EXTERNAL RegisterEventSourceW:WINAPI*>
PROCEDURE RegisterEventSourceW (lpUNCServerName: PWSTR;
                                lpSourceName   : PWSTR  ): HANDLE;

CONST RegisterEventSource = RegisterEventSourceA;

<*EXTERNAL OpenBackupEventLogA:WINAPI*>
PROCEDURE OpenBackupEventLogA (lpUNCServerName: PSTR;
                               lpFileName     : PSTR):  HANDLE;

<*EXTERNAL OpenBackupEventLogW:WINAPI*>
PROCEDURE OpenBackupEventLogW (lpUNCServerName: PWSTR;
                               lpFileName     : PWSTR  ): HANDLE;

CONST OpenBackupEventLog = OpenBackupEventLogA;

<*EXTERNAL ReadEventLogA:WINAPI*>
PROCEDURE ReadEventLogA (hEventLog               : HANDLE;
                         dwReadFlags             : UINT32;
                         dwRecordOffset          : UINT32;
                         lpBuffer                : PVOID;
                         nNumberOfBytesToRead    : UINT32;
                         pnBytesRead             : UNTRACED REF UINT32;
                         pnMinNumberOfBytesNeeded: UNTRACED REF UINT32  ): BOOL;

<*EXTERNAL ReadEventLogW:WINAPI*>
PROCEDURE ReadEventLogW (hEventLog               : HANDLE;
                         dwReadFlags             : UINT32;
                         dwRecordOffset          : UINT32;
                         lpBuffer                : PVOID;
                         nNumberOfBytesToRead    : UINT32;
                         pnBytesRead             : UNTRACED REF UINT32;
                         pnMinNumberOfBytesNeeded: UNTRACED REF UINT32  ): BOOL;

CONST ReadEventLog = ReadEventLogA;

<*EXTERNAL ReportEventA:WINAPI*>
PROCEDURE ReportEventA (hEventLog  : HANDLE;
                        wType      : UINT16;
                        wCategory  : UINT16;
                        dwEventID  : UINT32;
                        lpUserSid  : PSID;
                        wNumStrings: UINT16;
                        dwDataSize : UINT32;
                        lpStrings  : UNTRACED REF PSTR;
                        lpRawData  : PVOID              ): BOOL;

<*EXTERNAL ReportEventW:WINAPI*>
PROCEDURE ReportEventW (hEventLog  : HANDLE;
                        wType      : UINT16;
                        wCategory  : UINT16;
                        dwEventID  : UINT32;
                        lpUserSid  : PSID;
                        wNumStrings: UINT16;
                        dwDataSize : UINT32;
                        lpStrings  : UNTRACED REF PWSTR;
                        lpRawData  : PVOID               ): BOOL;

CONST ReportEvent = ReportEventA;

(* Security APIs *)


<*EXTERNAL DuplicateToken:WINAPI*>
PROCEDURE DuplicateToken (ExistingTokenHandle: HANDLE;
                        ImpersonationLevel: WinNT.SECURITY_IMPERSONATION_LEVEL;
                          DuplicateTokenHandle: PHANDLE): BOOL;

<*EXTERNAL GetKernelObjectSecurity:WINAPI*>
PROCEDURE GetKernelObjectSecurity (Handle        : HANDLE;
                             RequestedInformation: WinNT.SECURITY_INFORMATION;
                             pSecurityDescriptor : WinNT.PSECURITY_DESCRIPTOR;
                             nLength             : UINT32;
                             lpnLengthNeeded     : PUINT32): BOOL;

<*EXTERNAL ImpersonateNamedPipeClient:WINAPI*>
PROCEDURE ImpersonateNamedPipeClient (hNamedPipe: HANDLE): BOOL;

<*EXTERNAL ImpersonateSelf:WINAPI*>
PROCEDURE ImpersonateSelf (
            ImpersonationLevel: WinNT.SECURITY_IMPERSONATION_LEVEL): BOOL;


<*EXTERNAL RevertToSelf:WINAPI*>
PROCEDURE RevertToSelf (): BOOL;


<*EXTERNAL AccessCheck:WINAPI*>
PROCEDURE AccessCheck (pSecurityDescriptor: WinNT.PSECURITY_DESCRIPTOR;
                       ClientToken        : HANDLE;
                       DesiredAccess      : UINT32;
                       GenericMapping     : WinNT.PGENERIC_MAPPING;
                       PrivilegeSet       : WinNT.PPRIVILEGE_SET;
                       PrivilegeSetLength : PUINT32;
                       GrantedAccess      : PUINT32;
                       AccessStatus       : PBOOL                    ): BOOL;


<*EXTERNAL OpenProcessToken:WINAPI*>
PROCEDURE OpenProcessToken (ProcessHandle: HANDLE;
                            DesiredAccess: UINT32;
                            TokenHandle  : PHANDLE ): BOOL;


<*EXTERNAL OpenThreadToken:WINAPI*>
PROCEDURE OpenThreadToken (ThreadHandle : HANDLE;
                           DesiredAccess: UINT32;
                           OpenAsSelf   : BOOL;
                           TokenHandle  : PHANDLE ): BOOL;


<*EXTERNAL GetTokenInformation:WINAPI*>
PROCEDURE GetTokenInformation (TokenHandle: HANDLE;
                          TokenInformationClass: WinNT.TOKEN_INFORMATION_CLASS;
                               TokenInformation      : PVOID;
                               TokenInformationLength: UINT32;
                               ReturnLength          : PUINT32  ): BOOL;


<*EXTERNAL SetTokenInformation:WINAPI*>
PROCEDURE SetTokenInformation (TokenHandle: HANDLE;
                          TokenInformationClass: WinNT.TOKEN_INFORMATION_CLASS;
                               TokenInformation      : PVOID;
                               TokenInformationLength: UINT32   ): BOOL;


<*EXTERNAL AdjustTokenPrivileges:WINAPI*>
PROCEDURE AdjustTokenPrivileges (TokenHandle         : HANDLE;
                                 DisableAllPrivileges: BOOL;
                                 NewState     : WinNT.PTOKEN_PRIVILEGES;
                                 BufferLength : UINT32;
                                 PreviousState: WinNT.PTOKEN_PRIVILEGES;
                                 ReturnLength : PUINT32                 ): BOOL;


<*EXTERNAL AdjustTokenGroups:WINAPI*>
PROCEDURE AdjustTokenGroups (TokenHandle   : HANDLE;
                             ResetToDefault: BOOL;
                             NewState      : WinNT.PTOKEN_GROUPS;
                             BufferLength  : UINT32;
                             PreviousState : WinNT.PTOKEN_GROUPS;
                             ReturnLength  : PUINT32               ): BOOL;


<*EXTERNAL PrivilegeCheck:WINAPI*>
PROCEDURE PrivilegeCheck (ClientToken       : HANDLE;
                          RequiredPrivileges: WinNT.PPRIVILEGE_SET;
                          pfResult          : PBOOL                ): BOOL;


<*EXTERNAL AccessCheckAndAuditAlarmA:WINAPI*>
PROCEDURE AccessCheckAndAuditAlarmA (SubsystemName    : PSTR;
                                     HandleId         : PVOID;
                                     ObjectTypeName   : PSTR;
                                     ObjectName       : PSTR;
                                SecurityDescriptor: WinNT.PSECURITY_DESCRIPTOR;
                                     DesiredAccess    : UINT32;
                                     GenericMapping   : WinNT.PGENERIC_MAPPING;
                                     ObjectCreation   : BOOL;
                                     GrantedAccess    : PUINT32;
                                     AccessStatus     : PBOOL;
                                     pfGenerateOnClose: PBOOL   ): BOOL;

<*EXTERNAL AccessCheckAndAuditAlarmW:WINAPI*>
PROCEDURE AccessCheckAndAuditAlarmW (SubsystemName    : PWSTR;
                                     HandleId         : PVOID;
                                     ObjectTypeName   : PWSTR;
                                     ObjectName       : PWSTR;
                                SecurityDescriptor: WinNT.PSECURITY_DESCRIPTOR;
                                     DesiredAccess    : UINT32;
                                    GenericMapping    : WinNT.PGENERIC_MAPPING;
                                     ObjectCreation   : BOOL;
                                     GrantedAccess    : PUINT32;
                                     AccessStatus     : PBOOL;
                                     pfGenerateOnClose: PBOOL   ): BOOL;

CONST AccessCheckAndAuditAlarm = AccessCheckAndAuditAlarmA;


<*EXTERNAL ObjectOpenAuditAlarmA:WINAPI*>
PROCEDURE ObjectOpenAuditAlarmA (SubsystemName  : PSTR;
                                 HandleId       : PVOID;
                                 ObjectTypeName : PSTR;
                                 ObjectName     : PSTR;
                             pSecurityDescriptor: WinNT.PSECURITY_DESCRIPTOR;
                                 ClientToken    : HANDLE;
                                 DesiredAccess  : UINT32;
                                 GrantedAccess  : UINT32;
                                 Privileges     : WinNT.PPRIVILEGE_SET;
                                 ObjectCreation : BOOL;
                                 AccessGranted  : BOOL;
                                 GenerateOnClose: PBOOL               ): BOOL;

<*EXTERNAL ObjectOpenAuditAlarmW:WINAPI*>
PROCEDURE ObjectOpenAuditAlarmW (SubsystemName : PWSTR;
                                 HandleId      : PVOID;
                                 ObjectTypeName: PWSTR;
                                 ObjectName    : PWSTR;
                            pSecurityDescriptor: WinNT.PSECURITY_DESCRIPTOR;
                                 ClientToken    : HANDLE;
                                 DesiredAccess  : UINT32;
                                 GrantedAccess  : UINT32;
                                 Privileges     : WinNT.PPRIVILEGE_SET;
                                 ObjectCreation : BOOL;
                                 AccessGranted  : BOOL;
                                 GenerateOnClose: PBOOL              ): BOOL;

CONST ObjectOpenAuditAlarm = ObjectOpenAuditAlarmA;


<*EXTERNAL ObjectPrivilegeAuditAlarmA:WINAPI*>
PROCEDURE ObjectPrivilegeAuditAlarmA (SubsystemName: PSTR;
                                      HandleId     : PVOID;
                                      ClientToken  : HANDLE;
                                      DesiredAccess: UINT32;
                                      Privileges: WinNT.PPRIVILEGE_SET;
                                      AccessGranted: BOOL): BOOL;

<*EXTERNAL ObjectPrivilegeAuditAlarmW:WINAPI*>
PROCEDURE ObjectPrivilegeAuditAlarmW (SubsystemName: PWSTR;
                                      HandleId     : PVOID;
                                      ClientToken  : HANDLE;
                                      DesiredAccess: UINT32;
                                      Privileges: WinNT.PPRIVILEGE_SET;
                                      AccessGranted: BOOL): BOOL;

CONST ObjectPrivilegeAuditAlarm = ObjectPrivilegeAuditAlarmA;


<*EXTERNAL ObjectCloseAuditAlarmA:WINAPI*>
PROCEDURE ObjectCloseAuditAlarmA (SubsystemName  : PSTR;
                                  HandleId       : PVOID;
                                  GenerateOnClose: BOOL    ): BOOL;

<*EXTERNAL ObjectCloseAuditAlarmW:WINAPI*>
PROCEDURE ObjectCloseAuditAlarmW (SubsystemName  : PWSTR;
                                  HandleId       : PVOID;
                                  GenerateOnClose: BOOL    ): BOOL;

CONST ObjectCloseAuditAlarm = ObjectCloseAuditAlarmA;


<*EXTERNAL PrivilegedServiceAuditAlarmA:WINAPI*>
PROCEDURE PrivilegedServiceAuditAlarmA (SubsystemName: PSTR;
                                        ServiceName  : PSTR;
                                        ClientToken  : HANDLE;
                                        Privileges: WinNT.PPRIVILEGE_SET;
                                        AccessGranted: BOOL): BOOL;

<*EXTERNAL PrivilegedServiceAuditAlarmW:WINAPI*>
PROCEDURE PrivilegedServiceAuditAlarmW (SubsystemName: PWSTR;
                                        ServiceName  : PWSTR;
                                        ClientToken  : HANDLE;
                                        Privileges: WinNT.PPRIVILEGE_SET;
                                        AccessGranted: BOOL): BOOL;

CONST PrivilegedServiceAuditAlarm = PrivilegedServiceAuditAlarmA;


<*EXTERNAL IsValidSid:WINAPI*>
PROCEDURE IsValidSid (pSid: PSID): BOOL;


<*EXTERNAL EqualSid:WINAPI*>
PROCEDURE EqualSid (pSid1: PSID; pSid2: PSID): BOOL;


<*EXTERNAL EqualPrefixSid:WINAPI*>
PROCEDURE EqualPrefixSid (pSid1: PSID; pSid2: PSID): BOOL;


<*EXTERNAL GetSidLengthRequired:WINAPI*>
PROCEDURE GetSidLengthRequired (nSubAuthorityCount: UINT8): UINT32;


<*EXTERNAL AllocateAndInitializeSid:WINAPI*>
PROCEDURE AllocateAndInitializeSid (
                         pIdentifierAuthority: WinNT.PSID_IDENTIFIER_AUTHORITY;
                         nSubAuthorityCount: UINT8;
                         nSubAuthority0    : UINT32;
                         nSubAuthority1    : UINT32;
                         nSubAuthority2    : UINT32;
                         nSubAuthority3    : UINT32;
                         nSubAuthority4    : UINT32;
                         nSubAuthority5    : UINT32;
                         nSubAuthority6    : UINT32;
                         nSubAuthority7    : UINT32;
                         pSid              : UNTRACED REF PSID): BOOL;

<*EXTERNAL FreeSid:WINAPI*>
PROCEDURE FreeSid (pSid: PSID): PVOID;

<*EXTERNAL InitializeSid:WINAPI*>
PROCEDURE InitializeSid (Sid                 : PSID;
                         pIdentifierAuthority: WinNT.PSID_IDENTIFIER_AUTHORITY;
                         nSubAuthorityCount  : UINT8): BOOL;


<*EXTERNAL GetSidIdentifierAuthority:WINAPI*>
PROCEDURE GetSidIdentifierAuthority (
              pSid: PSID): WinNT.PSID_IDENTIFIER_AUTHORITY;


<*EXTERNAL GetSidSubAuthority:WINAPI*>
PROCEDURE GetSidSubAuthority (pSid: PSID; nSubAuthority: UINT32): PUINT32;


<*EXTERNAL GetSidSubAuthorityCount:WINAPI*>
PROCEDURE GetSidSubAuthorityCount (pSid: PSID): PUINT8;


<*EXTERNAL GetLengthSid:WINAPI*>
PROCEDURE GetLengthSid (pSid: PSID): UINT32;


<*EXTERNAL CopySid:WINAPI*>
PROCEDURE CopySid (nDestinationSidLength: UINT32;
                   pDestinationSid      : PSID;
                   pSourceSid           : PSID   ): BOOL;


<*EXTERNAL AreAllAccessesGranted:WINAPI*>
PROCEDURE AreAllAccessesGranted (GrantedAccess: UINT32;
                                 DesiredAccess: UINT32  ): BOOL;


<*EXTERNAL AreAnyAccessesGranted:WINAPI*>
PROCEDURE AreAnyAccessesGranted (GrantedAccess: UINT32;
                                 DesiredAccess: UINT32  ): BOOL;


<*EXTERNAL MapGenericMask:WINAPI*>
PROCEDURE MapGenericMask (AccessMask    : PUINT32;
                          GenericMapping: WinNT.PGENERIC_MAPPING);


<*EXTERNAL IsValidAcl:WINAPI*>
PROCEDURE IsValidAcl (pAcl: PACL): BOOL;


<*EXTERNAL InitializeAcl:WINAPI*>
PROCEDURE InitializeAcl (pAcl         : PACL;
                         nAclLength   : UINT32;
                         dwAclRevision: UINT32  ): BOOL;


<*EXTERNAL GetAclInformation:WINAPI*>
PROCEDURE GetAclInformation (pAcl                 : PACL;
                             pAclInformation      : PVOID;
                             nAclInformationLength: UINT32;
                     dwAclInformationClass: WinNT.ACL_INFORMATION_CLASS): BOOL;


<*EXTERNAL SetAclInformation:WINAPI*>
PROCEDURE SetAclInformation (pAcl                 : PACL;
                             pAclInformation      : PVOID;
                             nAclInformationLength: UINT32;
                     dwAclInformationClass: WinNT.ACL_INFORMATION_CLASS): BOOL;


<*EXTERNAL AddAce:WINAPI*>
PROCEDURE AddAce (pAcl              : PACL;
                  dwAceRevision     : UINT32;
                  dwStartingAceIndex: UINT32;
                  pAceList          : PVOID;
                  nAceListLength    : UINT32   ): BOOL;


<*EXTERNAL DeleteAce:WINAPI*>
PROCEDURE DeleteAce (pAcl: PACL; dwAceIndex: UINT32): BOOL;


<*EXTERNAL GetAce:WINAPI*>
PROCEDURE GetAce (pAcl      : PACL;
                  dwAceIndex: UINT32;
                  pAce      : UNTRACED REF PVOID): BOOL;


<*EXTERNAL AddAccessAllowedAce:WINAPI*>
PROCEDURE AddAccessAllowedAce (pAcl         : PACL;
                               dwAceRevision: UINT32;
                               AccessMask   : UINT32;
                               pSid         : PSID   ): BOOL;


<*EXTERNAL AddAccessDeniedAce:WINAPI*>
PROCEDURE AddAccessDeniedAce (pAcl         : PACL;
                              dwAceRevision: UINT32;
                              AccessMask   : UINT32;
                              pSid         : PSID   ): BOOL;


<*EXTERNAL AddAuditAccessAce:WINAPI*>
PROCEDURE AddAuditAccessAce (pAcl         : PACL;
                             dwAceRevision: UINT32;
                             dwAccessMask : UINT32;
                             pSid         : PSID;
                             bAuditSuccess: BOOL;
                             bAuditFailure: BOOL   ): BOOL;


<*EXTERNAL FindFirstFreeAce:WINAPI*>
PROCEDURE FindFirstFreeAce (pAcl: PACL; pAce: UNTRACED REF PVOID): BOOL;


<*EXTERNAL InitializeSecurityDescriptor:WINAPI*>
PROCEDURE InitializeSecurityDescriptor (
            pSecurityDescriptor: WinNT.PSECURITY_DESCRIPTOR;
            dwRevision         : UINT32                      ): BOOL;


<*EXTERNAL IsValidSecurityDescriptor:WINAPI*>
PROCEDURE IsValidSecurityDescriptor (
            pSecurityDescriptor: WinNT.PSECURITY_DESCRIPTOR): BOOL;


<*EXTERNAL GetSecurityDescriptorLength:WINAPI*>
PROCEDURE GetSecurityDescriptorLength (
            pSecurityDescriptor: WinNT.PSECURITY_DESCRIPTOR): UINT32;


<*EXTERNAL GetSecurityDescriptorControl:WINAPI*>
PROCEDURE GetSecurityDescriptorControl (
            pSecurityDescriptor: WinNT.PSECURITY_DESCRIPTOR;
            pControl           : WinNT.PSECURITY_DESCRIPTOR_CONTROL;
            lpdwRevision       : PUINT32                             ): BOOL;


<*EXTERNAL SetSecurityDescriptorDacl:WINAPI*>
PROCEDURE SetSecurityDescriptorDacl (
              pSecurityDescriptor: WinNT.PSECURITY_DESCRIPTOR;
              bDaclPresent       : BOOL;
              pDacl              : PACL;
              bDaclDefaulted     : BOOL  ): BOOL;


<*EXTERNAL GetSecurityDescriptorDacl:WINAPI*>
PROCEDURE GetSecurityDescriptorDacl (
              pSecurityDescriptor: WinNT.PSECURITY_DESCRIPTOR;
              lpbDaclPresent     : PBOOL;
              pDacl              : UNTRACED REF PACL;
              lpbDaclDefaulted   : PBOOL): BOOL;


<*EXTERNAL SetSecurityDescriptorSacl:WINAPI*>
PROCEDURE SetSecurityDescriptorSacl (
              pSecurityDescriptor: WinNT.PSECURITY_DESCRIPTOR;
              bSaclPresent       : BOOL;
              pSacl              : PACL;
              bSaclDefaulted     : BOOL  ): BOOL;


<*EXTERNAL GetSecurityDescriptorSacl:WINAPI*>
PROCEDURE GetSecurityDescriptorSacl (
              pSecurityDescriptor: WinNT.PSECURITY_DESCRIPTOR;
              lpbSaclPresent     : PBOOL;
              pSacl              : UNTRACED REF PACL;
              lpbSaclDefaulted   : PBOOL): BOOL;


<*EXTERNAL SetSecurityDescriptorOwner:WINAPI*>
PROCEDURE SetSecurityDescriptorOwner (
              pSecurityDescriptor: WinNT.PSECURITY_DESCRIPTOR;
              pOwner             : PSID;
              bOwnerDefaulted    : BOOL  ): BOOL;


<*EXTERNAL GetSecurityDescriptorOwner:WINAPI*>
PROCEDURE GetSecurityDescriptorOwner (
              pSecurityDescriptor : WinNT.PSECURITY_DESCRIPTOR;
              pOwner              : UNTRACED REF PSID;
              lpbOwnerDefaulted   : PBOOL): BOOL;


<*EXTERNAL SetSecurityDescriptorGroup:WINAPI*>
PROCEDURE SetSecurityDescriptorGroup (
              pSecurityDescriptor: WinNT.PSECURITY_DESCRIPTOR;
              pGroup             : PSID;
              bGroupDefaulted    : BOOL  ): BOOL;


<*EXTERNAL GetSecurityDescriptorGroup:WINAPI*>
PROCEDURE GetSecurityDescriptorGroup (
              pSecurityDescriptor: WinNT.PSECURITY_DESCRIPTOR;
              pGroup             : UNTRACED REF PSID;
              lpbGroupDefaulted  : PBOOL): BOOL;


<*EXTERNAL CreatePrivateObjectSecurity:WINAPI*>
PROCEDURE CreatePrivateObjectSecurity (
              ParentDescriptor : WinNT.PSECURITY_DESCRIPTOR;
              CreatorDescriptor: WinNT.PSECURITY_DESCRIPTOR;
              NewDescriptor    : UNTRACED REF WinNT.PSECURITY_DESCRIPTOR;
              IsDirectoryObject: BOOL;
              Token            : HANDLE;
              GenericMapping   : WinNT.PGENERIC_MAPPING): BOOL;


<*EXTERNAL SetPrivateObjectSecurity:WINAPI*>
PROCEDURE SetPrivateObjectSecurity (
            SecurityInformation      : WinNT.SECURITY_INFORMATION;
            ModificationDescriptor   : WinNT.PSECURITY_DESCRIPTOR;
            ObjectsSecurityDescriptor: UNTRACED REF WinNT.PSECURITY_DESCRIPTOR;
            GenericMapping           : WinNT.PGENERIC_MAPPING;
            Token                    : HANDLE): BOOL;


<*EXTERNAL GetPrivateObjectSecurity:WINAPI*>
PROCEDURE GetPrivateObjectSecurity (
              ObjectDescriptor   : WinNT.PSECURITY_DESCRIPTOR;
              SecurityInformation: WinNT.SECURITY_INFORMATION;
              ResultantDescriptor: WinNT.PSECURITY_DESCRIPTOR;
              DescriptorLength   : UINT32;
              ReturnLength       : PUINT32 ): BOOL;


<*EXTERNAL DestroyPrivateObjectSecurity:WINAPI*>
PROCEDURE DestroyPrivateObjectSecurity (
              ObjectDescriptor: UNTRACED REF WinNT.PSECURITY_DESCRIPTOR): BOOL;


<*EXTERNAL MakeSelfRelativeSD:WINAPI*>
PROCEDURE MakeSelfRelativeSD (
              pAbsoluteSecurityDescriptor    : WinNT.PSECURITY_DESCRIPTOR;
              pSelfRelativeSecurityDescriptor: WinNT.PSECURITY_DESCRIPTOR;
              lpdwBufferLength               : PUINT32): BOOL;


<*EXTERNAL MakeAbsoluteSD:WINAPI*>
PROCEDURE MakeAbsoluteSD (
              pSelfRelativeSecurityDescriptor   : WinNT.PSECURITY_DESCRIPTOR;
              pAbsoluteSecurityDescriptor       : WinNT.PSECURITY_DESCRIPTOR;
              lpdwAbsoluteSecurityDescriptorSize: PUINT32;
              pDacl                             : PACL;
              lpdwDaclSize                      : PUINT32;
              pSacl                             : PACL;
              lpdwSaclSize                      : PUINT32;
              pOwner                            : PSID;
              lpdwOwnerSize                     : PUINT32;
              pPrimaryGroup                     : PSID;
              lpdwPrimaryGroupSize              : PUINT32  ): BOOL;


<*EXTERNAL SetFileSecurityA:WINAPI*>
PROCEDURE SetFileSecurityA (
              lpFileName         : PSTR;
              SecurityInformation: WinNT.SECURITY_INFORMATION;
              pSecurityDescriptor: WinNT.PSECURITY_DESCRIPTOR): BOOL;

<*EXTERNAL SetFileSecurityW:WINAPI*>
PROCEDURE SetFileSecurityW (
              lpFileName         : PWSTR;
              SecurityInformation: WinNT.SECURITY_INFORMATION;
              pSecurityDescriptor: WinNT.PSECURITY_DESCRIPTOR): BOOL;

CONST SetFileSecurity = SetFileSecurityA;


<*EXTERNAL GetFileSecurityA:WINAPI*>
PROCEDURE GetFileSecurityA (lpFileName: PSTR;
                            RequestedInformation: WinNT.SECURITY_INFORMATION;
                            pSecurityDescriptor: WinNT.PSECURITY_DESCRIPTOR;
                            nLength        : UINT32;
                            lpnLengthNeeded: PUINT32): BOOL;

<*EXTERNAL GetFileSecurityW:WINAPI*>
PROCEDURE GetFileSecurityW (lpFileName: PWSTR;
                            RequestedInformation: WinNT.SECURITY_INFORMATION;
                            pSecurityDescriptor: WinNT.PSECURITY_DESCRIPTOR;
                            nLength        : UINT32;
                            lpnLengthNeeded: PUINT32): BOOL;

CONST GetFileSecurity = GetFileSecurityA;


<*EXTERNAL SetKernelObjectSecurity:WINAPI*>
PROCEDURE SetKernelObjectSecurity (
              Handle             : HANDLE;
              SecurityInformation: WinNT.SECURITY_INFORMATION;
              SecurityDescriptor : WinNT.PSECURITY_DESCRIPTOR): BOOL;



<*EXTERNAL FindFirstChangeNotificationA:WINAPI*>
PROCEDURE FindFirstChangeNotificationA (lpPathName    : PSTR;
                                        bWatchSubtree : BOOL;
                                        dwNotifyFilter: UINT32  ): HANDLE;

<*EXTERNAL FindFirstChangeNotificationW:WINAPI*>
PROCEDURE FindFirstChangeNotificationW (lpPathName    : PWSTR;
                                        bWatchSubtree : BOOL;
                                        dwNotifyFilter: UINT32  ): HANDLE;

CONST FindFirstChangeNotification = FindFirstChangeNotificationA;

<*EXTERNAL FindNextChangeNotification:WINAPI*>
PROCEDURE FindNextChangeNotification (hChangeHandle: HANDLE): BOOL;

<*EXTERNAL FindCloseChangeNotification:WINAPI*>
PROCEDURE FindCloseChangeNotification (hChangeHandle: HANDLE): BOOL;

<*EXTERNAL VirtualLock:WINAPI*>
PROCEDURE VirtualLock (lpAddress: PVOID; dwSize: SIZE_T): BOOL;

<*EXTERNAL VirtualUnlock:WINAPI*>
PROCEDURE VirtualUnlock (lpAddress: PVOID; dwSize: SIZE_T): BOOL;

<*EXTERNAL MapViewOfFileEx:WINAPI*>
PROCEDURE MapViewOfFileEx (hFileMappingObject  : HANDLE;
                           dwDesiredAccess     : UINT32;
                           dwFileOffsetHigh    : UINT32;
                           dwFileOffsetLow     : UINT32;
                           dwNumberOfBytesToMap: SIZE_T;
                           lpBaseAddress       : PVOID  ): PVOID;

<*EXTERNAL SetPriorityClass:WINAPI*>
PROCEDURE SetPriorityClass (hProcess: HANDLE; dwPriorityClass: UINT32): BOOL;

<*EXTERNAL GetPriorityClass:WINAPI*>
PROCEDURE GetPriorityClass (hProcess: HANDLE): UINT32;

<*EXTERNAL IsBadReadPtr:WINAPI*>
PROCEDURE IsBadReadPtr (lp: PVOID; ucb: UINT32): BOOL;

<*EXTERNAL IsBadWritePtr:WINAPI*>
PROCEDURE IsBadWritePtr (lp: PVOID; ucb: UINT32): BOOL;

<*EXTERNAL IsBadHugeReadPtr:WINAPI*>
PROCEDURE IsBadHugeReadPtr (lp: PVOID; ucb: UINT32): BOOL;

<*EXTERNAL IsBadHugeWritePtr:WINAPI*>
PROCEDURE IsBadHugeWritePtr (lp: PVOID; ucb: UINT32): BOOL;

<*EXTERNAL IsBadCodePtr:WINAPI*>
PROCEDURE IsBadCodePtr (lpfn: FARPROC): BOOL;

<*EXTERNAL IsBadStringPtrA:WINAPI*>
PROCEDURE IsBadStringPtrA (lpsz: PCSTR; ucchMax: UINT32): BOOL;

<*EXTERNAL IsBadStringPtrW:WINAPI*>
PROCEDURE IsBadStringPtrW (lpsz: PCWSTR; ucchMax: UINT32): BOOL;

CONST IsBadStringPtr = IsBadStringPtrA;

<*EXTERNAL LookupAccountSidA:WINAPI*>
PROCEDURE LookupAccountSidA (lpSystemName          : PSTR;
                             Sid                   : PSID;
                             Name                  : PSTR;
                             cbName                : PUINT32;
                             ReferencedDomainName  : PSTR;
                             cbReferencedDomainName: PUINT32;
                             peUse                 : WinNT.PSID_NAME_USE):BOOL;

<*EXTERNAL LookupAccountSidW:WINAPI*>
PROCEDURE LookupAccountSidW (lpSystemName          : PWSTR;
                             Sid                   : PSID;
                             Name                  : PWSTR;
                             cbName                : PUINT32;
                             ReferencedDomainName  : PWSTR;
                             cbReferencedDomainName: PUINT32;
                             peUse                 : WinNT.PSID_NAME_USE):BOOL;

CONST LookupAccountSid = LookupAccountSidA;

<*EXTERNAL LookupAccountNameA:WINAPI*>
PROCEDURE LookupAccountNameA (lpSystemName          : PSTR;
                              lpAccountName         : PSTR;
                              Sid                   : PSID;
                              cbSid                 : PUINT32;
                              ReferencedDomainName  : PSTR;
                              cbReferencedDomainName: PUINT32;
                              peUse                : WinNT.PSID_NAME_USE):BOOL;

<*EXTERNAL LookupAccountNameW:WINAPI*>
PROCEDURE LookupAccountNameW (lpSystemName          : PWSTR;
                              lpAccountName         : PWSTR;
                              Sid                   : PSID;
                              cbSid                 : PUINT32;
                              ReferencedDomainName  : PWSTR;
                              cbReferencedDomainName: PUINT32;
                              peUse                : WinNT.PSID_NAME_USE):BOOL;

CONST LookupAccountName = LookupAccountNameA;

<*EXTERNAL LookupPrivilegeValueA:WINAPI*>
PROCEDURE LookupPrivilegeValueA (lpSystemName: PSTR;
                                 lpName      : PSTR;
                                 lpLuid      : PLUID  ): BOOL;

<*EXTERNAL LookupPrivilegeValueW:WINAPI*>
PROCEDURE LookupPrivilegeValueW (lpSystemName: PWSTR;
                                 lpName      : PWSTR;
                                 lpLuid      : PLUID   ): BOOL;

CONST LookupPrivilegeValue = LookupPrivilegeValueA;

<*EXTERNAL LookupPrivilegeNameA:WINAPI*>
PROCEDURE LookupPrivilegeNameA (lpSystemName: PSTR;
                                lpLuid      : PLUID;
                                lpName      : PSTR;
                                cbName      : PUINT32): BOOL;

<*EXTERNAL LookupPrivilegeNameW:WINAPI*>
PROCEDURE LookupPrivilegeNameW (lpSystemName: PWSTR;
                                lpLuid      : PLUID;
                                lpName      : PWSTR;
                                cbName      : PUINT32 ): BOOL;

CONST LookupPrivilegeName = LookupPrivilegeNameA;

<*EXTERNAL LookupPrivilegeDisplayNameA:WINAPI*>
PROCEDURE LookupPrivilegeDisplayNameA (lpSystemName : PSTR;
                                       lpName       : PSTR;
                                       lpDisplayName: PSTR;
                                       cbDisplayName: PUINT32;
                                       lpLanguageId : PUINT32  ): BOOL;

<*EXTERNAL LookupPrivilegeDisplayNameW:WINAPI*>
PROCEDURE LookupPrivilegeDisplayNameW (lpSystemName : PWSTR;
                                       lpName       : PWSTR;
                                       lpDisplayName: PWSTR;
                                       cbDisplayName: PUINT32;
                                       lpLanguageId : PUINT32  ): BOOL;

CONST LookupPrivilegeDisplayName = LookupPrivilegeDisplayNameA;

<*EXTERNAL AllocateLocallyUniqueId:WINAPI*>
PROCEDURE AllocateLocallyUniqueId (Luid: PLUID): BOOL;

<*EXTERNAL BuildCommDCBA:WINAPI*>
PROCEDURE BuildCommDCBA (lpDef: PCSTR; lpDCB: LPDCB): BOOL;

<*EXTERNAL BuildCommDCBW:WINAPI*>
PROCEDURE BuildCommDCBW (lpDef: PCWSTR; lpDCB: LPDCB): BOOL;

CONST BuildCommDCB = BuildCommDCBA;

<*EXTERNAL BuildCommDCBAndTimeoutsA:WINAPI*>
PROCEDURE BuildCommDCBAndTimeoutsA (lpDef          : PCSTR;
                                    lpDCB          : LPDCB;
                                    lpCommTimeouts : LPCOMMTIMEOUTS): BOOL;

<*EXTERNAL BuildCommDCBAndTimeoutsW:WINAPI*>
PROCEDURE BuildCommDCBAndTimeoutsW (lpDef          : PCWSTR;
                                    lpDCB          : LPDCB;
                                    lpCommTimeouts : LPCOMMTIMEOUTS): BOOL;

CONST BuildCommDCBAndTimeouts = BuildCommDCBAndTimeoutsA;

<*EXTERNAL CommConfigDialogA:WINAPI*>
PROCEDURE CommConfigDialogA (lpszName : PCSTR;
                             hWnd     : HWND;
                             lpCC     : LPCOMMCONFIG ): BOOL;

<*EXTERNAL CommConfigDialogW:WINAPI*>
PROCEDURE CommConfigDialogW (lpszName : PCWSTR;
                             hWnd     : HWND;
                             lpCC     : LPCOMMCONFIG ): BOOL;

CONST CommConfigDialog = CommConfigDialogA;

<*EXTERNAL GetDefaultCommConfigA:WINAPI*>
PROCEDURE GetDefaultCommConfigA (lpszName : PCSTR;
                                 lpCC     : LPCOMMCONFIG;
                                 lpdwSize : PUINT32): BOOL;

<*EXTERNAL GetDefaultCommConfigW:WINAPI*>
PROCEDURE GetDefaultCommConfigW (lpszName : PCWSTR;
                                 lpCC     : LPCOMMCONFIG;
                                 lpdwSize : PUINT32): BOOL;

CONST GetDefaultCommConfig = GetDefaultCommConfigA;

<*EXTERNAL SetDefaultCommConfigA:WINAPI*>
PROCEDURE SetDefaultCommConfigA (lpszName : PCSTR;
                                 lpCC     : LPCOMMCONFIG;
                                 dwSize   : UINT32): BOOL;

<*EXTERNAL SetDefaultCommConfigW:WINAPI*>
PROCEDURE SetDefaultCommConfigW (lpszName : PCWSTR;
                                 lpCC     : LPCOMMCONFIG;
                                 dwSize   : UINT32): BOOL;

CONST SetDefaultCommConfig = SetDefaultCommConfigA;

CONST MAX_COMPUTERNAME_LENGTH = 15;

<*EXTERNAL GetComputerNameA:WINAPI*>
PROCEDURE GetComputerNameA (lpBuffer: PSTR; nSize: PUINT32): BOOL;

<*EXTERNAL GetComputerNameW:WINAPI*>
PROCEDURE GetComputerNameW (lpBuffer: PWSTR; nSize: PUINT32): BOOL;

CONST GetComputerName = GetComputerNameA;

<*EXTERNAL SetComputerNameA:WINAPI*>
PROCEDURE SetComputerNameA (lpComputerName: PSTR): BOOL;

<*EXTERNAL SetComputerNameW:WINAPI*>
PROCEDURE SetComputerNameW (lpComputerName: PWSTR): BOOL;

CONST SetComputerName = SetComputerNameA;

<*EXTERNAL GetUserNameA:WINAPI*>
PROCEDURE GetUserNameA (lpBuffer: PSTR; nSize: PUINT32): BOOL;

<*EXTERNAL GetUserNameW:WINAPI*>
PROCEDURE GetUserNameW (lpBuffer: PWSTR; nSize: PUINT32): BOOL;

CONST GetUserName = GetUserNameA;

(* Performance counter API's *)

<*EXTERNAL QueryPerformanceCounter:WINAPI*>
PROCEDURE QueryPerformanceCounter (
              lpPerformanceCount: UNTRACED REF LARGE_INTEGER): BOOL;

<*EXTERNAL QueryPerformanceFrequency:WINAPI*>
PROCEDURE QueryPerformanceFrequency (
              lpFrequency: UNTRACED REF LARGE_INTEGER): BOOL;


(* dwPlatformId defines: *)

TYPE
  POSVERSIONINFOA = UNTRACED REF OSVERSIONINFOA;
  LPOSVERSIONINFOA = POSVERSIONINFOA; (* compat *)
  OSVERSIONINFOA = RECORD
    dwOSVersionInfoSize : UINT32;
    dwMajorVersion      : UINT32;
    dwMinorVersion      : UINT32;
    dwBuildNumber       : UINT32;
    dwPlatformId        : UINT32;
    szCSDVersion        : ARRAY [0..127] OF CHAR; (* Maintenance string
                                                     for PSS usage *)
  END;

  POSVERSIONINFOW = UNTRACED REF OSVERSIONINFOW;
  LPOSVERSIONINFOW = POSVERSIONINFOW; (* compat *)
  OSVERSIONINFOW = RECORD
    dwOSVersionInfoSize : UINT32;
    dwMajorVersion      : UINT32;
    dwMinorVersion      : UINT32;
    dwBuildNumber       : UINT32;
    dwPlatformId        : UINT32;
    szCSDVersion        : ARRAY [0..127] OF WCHAR; (* Maintenance string
                                                     for PSS usage *)
  END;

  OSVERSIONINFO   = OSVERSIONINFOA;
  POSVERSIONINFO  = POSVERSIONINFOA;
  LPOSVERSIONINFO = POSVERSIONINFOA; (* compat *)

CONST
  VER_PLATFORM_WIN32s        = 0;
  VER_PLATFORM_WIN32_WINDOWS = 1;
  VER_PLATFORM_WIN32_NT      = 2;

<*EXTERNAL GetVersionExA:WINAPI*>
PROCEDURE GetVersionExA (lpVersionInformation: POSVERSIONINFOA): BOOL;

<*EXTERNAL GetVersionExW:WINAPI*>
PROCEDURE GetVersionExW (lpVersionInformation: POSVERSIONINFOW): BOOL;

CONST GetVersionEx = GetVersionExA;

(* DOS and OS/2 Compatible Error Code definitions returned by the Win32 Base
   API functions. *)

(* Abnormal termination codes *)

  TC_NORMAL  = 0;
  TC_HARDERR = 1;
  TC_GP_TRAP = 2;
  TC_SIGNAL  = 3;

(* Power Management APIs *)

  AC_LINE_OFFLINE             = 16_00;
  AC_LINE_ONLINE              = 16_01;
  AC_LINE_BACKUP_POWER        = 16_02;
  AC_LINE_UNKNOWN             = 16_FF;

  BATTERY_FLAG_HIGH           = 16_01;
  BATTERY_FLAG_LOW            = 16_02;
  BATTERY_FLAG_CRITICAL       = 16_04;
  BATTERY_FLAG_CHARGING       = 16_08;
  BATTERY_FLAG_NO_BATTERY     = 16_80;
  BATTERY_FLAG_UNKNOWN        = 16_FF;

  BATTERY_PERCENTAGE_UNKNOWN  = 16_FF;

  BATTERY_LIFE_UNKNOWN        = 16_FFFFFFFF;

TYPE
  PSYSTEM_POWER_STATUS = UNTRACED REF SYSTEM_POWER_STATUS;
  LPSYSTEM_POWER_STATUS = PSYSTEM_POWER_STATUS; (* compat *)
  SYSTEM_POWER_STATUS = RECORD
    ACLineStatus        : UINT8;
    BatteryFlag         : UINT8;
    BatteryLifePercent  : UINT8;
    Reserved1           : UINT8;
    BatteryLifeTime     : UINT32;
    BatteryFullLifeTime : UINT32;
  END;

<*EXTERNAL GetSystemPowerStatus:WINAPI*>
PROCEDURE GetSystemPowerStatus (lpSystemPowerStatus: PSYSTEM_POWER_STATUS): BOOL;

<*EXTERNAL SetSystemPowerState:WINAPI*>
PROCEDURE SetSystemPowerState (fSuspend: BOOL;  fForce: BOOL): BOOL;

END WinBase.
(*********
  AllocLSCallback      (ord: 104  offset: 16_21af6)
  AllocSLCallback      (ord: 105  offset: 16_21b29)
  Callback12           (ord: 119  offset: 16_21959)
  Callback16           (ord: 120  offset: 16_21966)
  Callback20           (ord: 121  offset: 16_21973)
  Callback24           (ord: 122  offset: 16_21980)
  Callback28           (ord: 123  offset: 16_2198d)
  Callback32           (ord: 124  offset: 16_2199a)
  Callback36           (ord: 125  offset: 16_219a7)
  Callback40           (ord: 126  offset: 16_219b4)
  Callback44           (ord: 127  offset: 16_219c1)
  Callback48           (ord: 128  offset: 16_219ce)
  Callback4            (ord: 129  offset: 16_21940)
  Callback52           (ord: 130  offset: 16_219db)
  Callback56           (ord: 131  offset: 16_219e8)
  Callback60           (ord: 132  offset: 16_219f5)
  Callback64           (ord: 133  offset: 16_21a02)
  Callback8            (ord: 134  offset: 16_2194c)
  FT_Exit0             (ord: 218  offset: 16_2c2d)
  FT_Exit12            (ord: 219  offset: 16_2c73)
  FT_Exit16            (ord: 220  offset: 16_2c8b)
  FT_Exit20            (ord: 221  offset: 16_2ca3)
  FT_Exit24            (ord: 222  offset: 16_2cbb)
  FT_Exit28            (ord: 223  offset: 16_2cd3)
  FT_Exit32            (ord: 224  offset: 16_2ceb)
  FT_Exit36            (ord: 225  offset: 16_2d03)
  FT_Exit4             (ord: 226  offset: 16_2c43)
  FT_Exit40            (ord: 227  offset: 16_2d1b)
  FT_Exit44            (ord: 228  offset: 16_2d33)
  FT_Exit48            (ord: 229  offset: 16_2d4b)
  FT_Exit52            (ord: 230  offset: 16_2d63)
  FT_Exit56            (ord: 231  offset: 16_2d7b)
  FT_Exit8             (ord: 232  offset: 16_2c5b)
  FT_Prolog            (ord: 233  offset: 16_2791)
  FreeLSCallback       (ord: 270  offset: 16_21d72)
  FreeSLCallback       (ord: 274  offset: 16_21dad)
  GetLSCallbackTarget  (ord: 337  offset: 16_21cc9)
  IsSLCallback         (ord: 487  offset: 16_21c69)
  MapHInstLS           (ord: 516  offset: 16_1adc)
  MapHInstSL           (ord: 518  offset: 16_1b08)
  MapHModuleLS         (ord: 520  offset: 16_1b59)
  MapLS                (ord: 522  offset: 16_1fdf)
  MapHModuleSL         (ord: 521  offset: 16_1b68)
  MapSL                (ord: 523  offset: 16_1bc1)
  MapSLFix             (ord: 524  offset: 16_1b78)
  QT_Thunk             (ord: 559  offset: 16_28d0)
  RtlUnwind            (ord: 590  offset: 16_15d79)
  SMapLS               (ord: 592  offset: 16_1ec8)
  SMapLS_IP_EBP_12     (ord: 593  offset: 16_1f7e)
  SMapLS_IP_EBP_16     (ord: 594  offset: 16_1f79)
  SMapLS_IP_EBP_20     (ord: 595  offset: 16_1f74)
  SMapLS_IP_EBP_24     (ord: 596  offset: 16_1f6f)
  SMapLS_IP_EBP_28     (ord: 597  offset: 16_1f6a)
  SMapLS_IP_EBP_32     (ord: 598  offset: 16_1f65)
  SMapLS_IP_EBP_36     (ord: 599  offset: 16_1f60)
  SMapLS_IP_EBP_40     (ord: 600  offset: 16_1f5b)
  SMapLS_IP_EBP_8      (ord: 601  offset: 16_1f83)
  SUnMapLS             (ord: 602  offset: 16_1f46)
  SUnMapLS_IP_EBP_12   (ord: 603  offset: 16_1f3c)
  SUnMapLS_IP_EBP_16   (ord: 604  offset: 16_1f37)
  SUnMapLS_IP_EBP_20   (ord: 605  offset: 16_1f32)
  SUnMapLS_IP_EBP_24   (ord: 606  offset: 16_1f2d)
  SUnMapLS_IP_EBP_28   (ord: 607  offset: 16_1f28)
  SUnMapLS_IP_EBP_32   (ord: 608  offset: 16_1f23)
  SUnMapLS_IP_EBP_36   (ord: 609  offset: 16_1f1e)
  SUnMapLS_IP_EBP_40   (ord: 610  offset: 16_1f19)
  SUnMapLS_IP_EBP_8    (ord: 611  offset: 16_1f41)
  UTRegister           (ord: 698  offset: 16_21ef9)
  UTUnRegister         (ord: 699  offset: 16_21f24)
  UnMapLS              (ord: 700  offset: 16_1feb)
  UnMapSLFixArray      (ord: 701  offset: 16_1c4e)
  _DebugOut            (ord: 752  offset: 16_3e99a)

  CloseSystemHandle           (ord: 139  offset: 16_2688d)
  CommConfigDialogA           (ord: 140  offset: 16_347e0)
  CommConfigDialogW           (ord: 141  offset: 16_34c7f)
  ConvertToGlobalHandle       (ord: 148  offset: 16_12354)
  CreateKernelThread          (ord: 163  offset: 16_2cda1)
  CreateSocketHandle          (ord: 176  offset: 16_32448)
  FT_Thunk                    (ord: 234  offset: 16_2a57)
  GetCommConfig               (ord: 283  offset: 16_34a26)
  GetDaylightFlag             (ord: 312  offset: 16_2adb5)
  GetErrorMode                (ord: 324  offset: 16_27dec)
  GetHandleContext            (ord: 335  offset: 16_325db)
  GetHandleInformation        (ord: 336  offset: 16_34c64)
  GetLSCallbackTemplate       (ord: 338  offset: 16_21ca2)
  GetProcessFlags             (ord: 374  offset: 16_1e422)
  GetProcessVersion           (ord: 379  offset: 16_24639)
  GetProductName              (ord: 381  offset: 16_27dc3)
  GetSLCallbackTarget         (ord: 389  offset: 16_21d14)
  GetSLCallbackTemplate       (ord: 390  offset: 16_21cec)
  GetSystemTimeAsFileTime     (ord: 408  offset: 16_2cf74)
  HeapReAlloc                 (ord: 465  offset: 16_6d55)
  HeapSetFlags                (ord: 466  offset: 16_34c76)
  InterlockedExchange         (ord: 474  offset: 16_2662a)
  InvalidateNLSCache          (ord: 476  offset: 16_3ec38)
  IsLSCallback                (ord: 486  offset: 16_21c35)
  K32Thk1632Epilog            (ord: 490  offset: 16_1907)
  K32Thk1632Prolog            (ord: 491  offset: 16_18e2)
  MakeCriticalSectionGlobal   (ord: 515  offset: 16_169f9)
  MapHInstLS_PN               (ord: 517  offset: 16_1ad7)
  MapHInstSL_PN               (ord: 519  offset: 16_1b01)
  NotifyNLSUserCache          (ord: 535  offset: 16_42c5d)
  OpenVxDHandle               (ord: 547  offset: 16_268fc)
  QueryNumberOfEventLogRecords (ord: 562  offset: 16_34c64)
  QueryOldestEventLogRecord   (ord: 563  offset: 16_34c64)
  QueueUserAPC                (ord: 566  offset: 16_24ae8)
  RegisterServiceProcess      (ord: 580  offset: 16_1bfc2)
  ReinitializeCriticalSection (ord: 581  offset: 16_128fd)
  RtlFillMemory               (ord: 588  offset: 16_2ca39)
  RtlMoveMemory               (ord: 589  offset: 16_7b6a)
  RtlZeroMemory               (ord: 591  offset: 16_7ba7)
  SetCommConfig               (ord: 617  offset: 16_34a67)
  SetDaylightFlag             (ord: 637  offset: 16_2adc5)
  SetHandleContext            (ord: 651  offset: 16_325a7)
  SetHandleInformation        (ord: 653  offset: 16_34c7f)
  SystemTimeToTzSpecificLocalTime (ord: 683  offset: 16_34c7f)
  ThunkConnect32              (ord: 688  offset: 16_2dff)
  TlsAllocInternal            (ord: 690  offset: 16_1a05f)
  TlsFreeInternal             (ord: 692  offset: 16_2a97b)
  UninitializeCriticalSection (ord: 703  offset: 16_1e0ed)
  _DebugPrintf                (ord: 753  offset: 16_3ea4d)
  _hread                      (ord: 754  offset: 16_72e1)
  _hwrite                     (ord: 755  offset: 16_2caa0)
  dprintf                     (ord: 762  offset: 16_3ea4d)
*********)
