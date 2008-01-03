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
FROM WinDef IMPORT ULONG, DWORD, LPVOID, BOOL, WORD, BYTE, LPLONG, LONG,
                   HGLOBAL, HINSTANCE, UINT, FARPROC, HLOCAL, PDWORD, HWND,
                   LPDWORD, USHORT, LPHANDLE, HRSRC, ATOM, PLONG, LPWORD,
                   HFILE, LPBYTE, MAX_PATH, LPBOOL, HMODULE, UCHAR, PUCHAR,
                   LPCVOID;
FROM WinNT IMPORT WCHAR, LPSTR, LPCSTR, LPWSTR, LPCWSTR, LPTSTR, LPCTSTR,
                  PSID, PACL, PVOID, PLUID, LARGE_INTEGER, HANDLE, PHANDLE;
FROM Ctypes IMPORT char, int, void_star;

(*
 * Compatability macros
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

CONST
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

CONST
  PIPE_ACCESS_INBOUND  = 16_00000001;
  PIPE_ACCESS_OUTBOUND = 16_00000002;
  PIPE_ACCESS_DUPLEX   = 16_00000003;

(* Define the Named Pipe End flags for GetNamedPipeInfo *)

CONST
  PIPE_CLIENT_END = 16_00000000;
  PIPE_SERVER_END = 16_00000001;

(* Define the dwPipeMode values for CreateNamedPipe *)

CONST
  PIPE_WAIT             = 16_00000000;
  PIPE_NOWAIT           = 16_00000001;
  PIPE_READMODE_BYTE    = 16_00000000;
  PIPE_READMODE_MESSAGE = 16_00000002;
  PIPE_TYPE_BYTE        = 16_00000000;
  PIPE_TYPE_MESSAGE     = 16_00000004;

(* Define the well known values for CreateNamedPipe nMaxInstances *)

CONST PIPE_UNLIMITED_INSTANCES = 255;

(* Define the Security Quality of Service bits to be passed *)
(* into CreateFile *)

CONST
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

CONST MAILSLOT_NO_MESSAGE: ULONG = -1;

(* Special value for read timeout to indicate that mailslot reads should *)
(* never timeout. *)

CONST MAILSLOT_WAIT_FOREVER: ULONG = -1;

(* File structures *)

TYPE
  LPOVERLAPPED = UNTRACED REF OVERLAPPED;
  OVERLAPPED = RECORD
    Internal    : DWORD;
    InternalHigh: DWORD;
    Offset      : DWORD;
    OffsetHigh  : DWORD;
    hEvent      : HANDLE;
  END;

  PSECURITY_ATTRIBUTES = UNTRACED REF SECURITY_ATTRIBUTES;
  LPSECURITY_ATTRIBUTES = UNTRACED REF SECURITY_ATTRIBUTES;
  SECURITY_ATTRIBUTES = RECORD
    nLength             : DWORD;
    lpSecurityDescriptor: LPVOID;
    bInheritHandle      : BOOL;
  END;

  PPROCESS_INFORMATION = UNTRACED REF PROCESS_INFORMATION;
  LPPROCESS_INFORMATION = UNTRACED REF PROCESS_INFORMATION;
  PROCESS_INFORMATION = RECORD
    hProcess   : HANDLE;
    hThread    : HANDLE;
    dwProcessId: DWORD;
    dwThreadId : DWORD;
  END;

(* File System time stamps are represented with the following structure: *)

TYPE
  PFILETIME = UNTRACED REF FILETIME;
  LPFILETIME = UNTRACED REF FILETIME;
  FILETIME = RECORD
    dwLowDateTime : DWORD;
    dwHighDateTime: DWORD;
  END;

(* System time is represented with the following structure: *)

TYPE
  PSYSTEMTIME = UNTRACED REF SYSTEMTIME;
  LPSYSTEMTIME = UNTRACED REF SYSTEMTIME;
  SYSTEMTIME = RECORD
    wYear        : WORD;
    wMonth       : WORD;
    wDayOfWeek   : WORD;
    wDay         : WORD;
    wHour        : WORD;
    wMinute      : WORD;
    wSecond      : WORD;
    wMilliseconds: WORD;
  END;

  PTHREAD_START_ROUTINE = <*WINAPI*> PROCEDURE
                             (lpThreadParameter: LPVOID): DWORD;

  LPTHREAD_START_ROUTINE = PTHREAD_START_ROUTINE;

  CRITICAL_SECTION = WinNT.RTL_CRITICAL_SECTION;
  PCRITICAL_SECTION = WinNT.PRTL_CRITICAL_SECTION;
  LPCRITICAL_SECTION = WinNT.PRTL_CRITICAL_SECTION;

  CRITICAL_SECTION_DEBUG = WinNT.RTL_CRITICAL_SECTION_DEBUG;
  PCRITICAL_SECTION_DEBUG = WinNT.PRTL_CRITICAL_SECTION_DEBUG;
  LPCRITICAL_SECTION_DEBUG = WinNT.PRTL_CRITICAL_SECTION_DEBUG;

  LPLDT_ENTRY = LPVOID;

CONST
  MUTEX_MODIFY_STATE = WinNT.MUTANT_QUERY_STATE;
  MUTEX_ALL_ACCESS   = WinNT.MUTANT_ALL_ACCESS;

(* Serial provider type. *)

CONST SP_SERIALCOMM: DWORD = 16_00000001;

(* Provider SubTypes *)

CONST
  PST_UNSPECIFIED   : DWORD = 16_00000000;
  PST_RS232         : DWORD = 16_00000001;
  PST_PARALLELPORT  : DWORD = 16_00000002;
  PST_RS422         : DWORD = 16_00000003;
  PST_RS423         : DWORD = 16_00000004;
  PST_RS449         : DWORD = 16_00000005;
  PST_FAX           : DWORD = 16_00000021;
  PST_SCANNER       : DWORD = 16_00000022;
  PST_NETWORK_BRIDGE: DWORD = 16_00000100;
  PST_LAT           : DWORD = 16_00000101;
  PST_TCPIP_TELNET  : DWORD = 16_00000102;
  PST_X25           : DWORD = 16_00000103;


(* Provider capabilities flags. *)

CONST
  PCF_DTRDSR       : DWORD = 16_0001;
  PCF_RTSCTS       : DWORD = 16_0002;
  PCF_RLSD         : DWORD = 16_0004;
  PCF_PARITY_CHECK : DWORD = 16_0008;
  PCF_XONXOFF      : DWORD = 16_0010;
  PCF_SETXCHAR     : DWORD = 16_0020;
  PCF_TOTALTIMEOUTS: DWORD = 16_0040;
  PCF_INTTIMEOUTS  : DWORD = 16_0080;
  PCF_SPECIALCHARS : DWORD = 16_0100;
  PCF_16BITMODE    : DWORD = 16_0200;

(* Comm provider settable parameters. *)

CONST
  SP_PARITY      : DWORD = 16_0001;
  SP_BAUD        : DWORD = 16_0002;
  SP_DATABITS    : DWORD = 16_0004;
  SP_STOPBITS    : DWORD = 16_0008;
  SP_HANDSHAKING : DWORD = 16_0010;
  SP_PARITY_CHECK: DWORD = 16_0020;
  SP_RLSD        : DWORD = 16_0040;

(* Settable baud rates in the provider. *)

CONST
  BAUD_075  : DWORD = 16_00000001;
  BAUD_110  : DWORD = 16_00000002;
  BAUD_134_5: DWORD = 16_00000004;
  BAUD_150  : DWORD = 16_00000008;
  BAUD_300  : DWORD = 16_00000010;
  BAUD_600  : DWORD = 16_00000020;
  BAUD_1200 : DWORD = 16_00000040;
  BAUD_1800 : DWORD = 16_00000080;
  BAUD_2400 : DWORD = 16_00000100;
  BAUD_4800 : DWORD = 16_00000200;
  BAUD_7200 : DWORD = 16_00000400;
  BAUD_9600 : DWORD = 16_00000800;
  BAUD_14400: DWORD = 16_00001000;
  BAUD_19200: DWORD = 16_00002000;
  BAUD_38400: DWORD = 16_00004000;
  BAUD_56K  : DWORD = 16_00008000;
  BAUD_128K : DWORD = 16_00010000;
  BAUD_USER : DWORD = 16_10000000;

(* Settable Data Bits *)

CONST
  DATABITS_5  : WORD = 16_0001;
  DATABITS_6  : WORD = 16_0002;
  DATABITS_7  : WORD = 16_0004;
  DATABITS_8  : WORD = 16_0008;
  DATABITS_16 : WORD = 16_0010;
  DATABITS_16X: WORD = 16_0020;

(* Settable Stop and Parity bits. *)

CONST
  STOPBITS_10 : WORD = 16_0001;
  STOPBITS_15 : WORD = 16_0002;
  STOPBITS_20 : WORD = 16_0004;
  PARITY_NONE : WORD = 16_0100;
  PARITY_ODD  : WORD = 16_0200;
  PARITY_EVEN : WORD = 16_0400;
  PARITY_MARK : WORD = 16_0800;
  PARITY_SPACE: WORD = 16_1000;

TYPE
  Int1  = BITS  1 FOR [0..1];
  Int2  = BITS  2 FOR [0..3];
  Int17 = BITS 17 FOR [0..16_1ffff];

  LPCOMMPROP = UNTRACED REF COMMPROP;
  COMMPROP = RECORD
    wPacketLength      : WORD;
    wPacketVersion     : WORD;
    dwServiceMask      : DWORD;
    dwReserved1        : DWORD;
    dwMaxTxQueue       : DWORD;
    dwMaxRxQueue       : DWORD;
    dwMaxBaud          : DWORD;
    dwProvSubType      : DWORD;
    dwProvCapabilities : DWORD;
    dwSettableParams   : DWORD;
    dwSettableBaud     : DWORD;
    wSettableData      : WORD;
    wSettableStopParity: WORD;
    dwCurrentTxQueue   : DWORD;
    dwCurrentRxQueue   : DWORD;
    dwProvSpec1        : DWORD;
    dwProvSpec2        : DWORD;
    wcProvChar         : ARRAY [0 .. 0] OF WCHAR;
  END;

  LPCOMSTAT = UNTRACED REF COMSTAT;
  COMSTAT = RECORD
    fCtsHold : Int1;
    fDsrHold : Int1;
    fRlsdHold: Int1;
    fXoffHold: Int1;
    fXoffSent: Int1;
    fEof     : Int1;
    fTxim    : Int1;
    fReserved: Int17;
    cbInQue  : DWORD;
    cbOutQue : DWORD;
  END;

(* DTR Control Flow Values. *)
CONST
  DTR_CONTROL_DISABLE   = 16_00;
  DTR_CONTROL_ENABLE    = 16_01;
  DTR_CONTROL_HANDSHAKE = 16_02;

(* RTS Control Flow Values *)
CONST
  RTS_CONTROL_DISABLE   = 16_00;
  RTS_CONTROL_ENABLE    = 16_01;
  RTS_CONTROL_HANDSHAKE = 16_02;
  RTS_CONTROL_TOGGLE    = 16_03;

TYPE
  LPDCB = UNTRACED REF DCB;
  DCB = RECORD
    DCBlength         : DWORD; (* sizeof(DCB) *)
    BaudRate          : DWORD; (* Baudrate at which running *)
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
    wReserved         : WORD;  (* Not currently used *)
    XonLim            : WORD;  (* Transmit X-ON threshold *)
    XoffLim           : WORD;  (* Transmit X-OFF threshold *)
    ByteSize          : BYTE;  (* Number of bits/byte, 4-8 *)
    Parity            : BYTE;  (* 0-4=None,Odd,Even,Mark,Space *)
    StopBits          : BYTE;  (* 0,1,2 = 1, 1.5, 2 *)
    XonChar           : char;  (* Tx and Rx X-ON character *)
    XoffChar          : char;  (* Tx and Rx X-OFF character *)
    ErrorChar         : char;  (* Error replacement char *)
    EofChar           : char;  (* End of Input character *)
    EvtChar           : char;  (* Recieved Event character *)
    wReserved1        : WORD;  (* Fill for now. *)
  END;

  LPCOMMTIMEOUTS = UNTRACED REF COMMTIMEOUTS;
  COMMTIMEOUTS = RECORD
    ReadIntervalTimeout        : DWORD;  (* Max time between read chars. *)
    ReadTotalTimeoutMultiplier : DWORD;  (* Multiplier of characters. *)
    ReadTotalTimeoutConstant   : DWORD;  (* Constant in milliseconds. *)
    WriteTotalTimeoutMultiplier: DWORD;  (* Multiplier of characters. *)
    WriteTotalTimeoutConstant  : DWORD;  (* Constant in milliseconds. *)
  END;

  LPCOMMCONFIG = UNTRACED REF COMMCONFIG;
  COMMCONFIG = RECORD
    dwSize            : DWORD;  (* Size of the entire struct *)
    wVersion          : WORD;   (* version of the structure *)
    wReserved         : WORD;   (* alignment *)
    dcb               : DCB;    (* device control block *)
    dwProviderSubType : DWORD;  (* ordinal value for identifying
                                   provider-defined data structure format*)
    dwProviderOffset  : DWORD;  (* Specifies the offset of provider specific
                                   data field in bytes from the start *)
    dwProviderSize    : DWORD;  (* size of the provider-specific data field *)
    wcProviderData    : ARRAY [0..0] OF WCHAR; (* provider-specific data *)
  END;

  LPSYSTEM_INFO = UNTRACED REF SYSTEM_INFO;
  SYSTEM_INFO = RECORD
    wProcessorArchitecture     : WORD;
    wReserved0                 : WORD;
    dwPageSize                 : DWORD;
    lpMinimumApplicationAddress: LPVOID;
    lpMaximumApplicationAddress: LPVOID;
    dwActiveProcessorMask      : DWORD;
    dwNumberOfProcessors       : DWORD;
    dwProcessorType            : DWORD;
    dwAllocationGranularity    : DWORD;
    wProcessorLevel            : WORD;
    wProcessorRevision         : WORD;
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
CONST
  GMEM_DISCARDED = 16_4000;
  GMEM_LOCKCOUNT = 16_00FF;

TYPE
  LPMEMORYSTATUS = UNTRACED REF MEMORYSTATUS;
  MEMORYSTATUS = RECORD
    dwLength       : DWORD;
    dwMemoryLoad   : DWORD;
    dwTotalPhys    : DWORD;
    dwAvailPhys    : DWORD;
    dwTotalPageFile: DWORD;
    dwAvailPageFile: DWORD;
    dwTotalVirtual : DWORD;
    dwAvailVirtual : DWORD;
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
CONST
  LMEM_DISCARDED = 16_4000;
  LMEM_LOCKCOUNT = 16_00FF;

(* dwCreationFlag values *)

CONST
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
CONST
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
    dwFirstChance  : DWORD;
  END;

  LPCREATE_THREAD_DEBUG_INFO = UNTRACED REF CREATE_THREAD_DEBUG_INFO;
  CREATE_THREAD_DEBUG_INFO = RECORD
    hThread       : HANDLE;
    lpStartAddress: LPTHREAD_START_ROUTINE;
  END;

  LPCREATE_PROCESS_DEBUG_INFO = UNTRACED REF CREATE_PROCESS_DEBUG_INFO;
  CREATE_PROCESS_DEBUG_INFO = RECORD
    hFile                : HANDLE;
    hProcess             : HANDLE;
    hThread              : HANDLE;
    lpBaseOfImage        : LPVOID;
    dwDebugInfoFileOffset: DWORD;
    nDebugInfoSize       : DWORD;
    lpStartAddress       : LPTHREAD_START_ROUTINE;
  END;

  EXIT_THREAD_DEBUG_INFO = RECORD dwExitCode: DWORD;  END;
  LPEXIT_THREAD_DEBUG_INFO = UNTRACED REF EXIT_THREAD_DEBUG_INFO;

  EXIT_PROCESS_DEBUG_INFO = RECORD dwExitCode: DWORD;  END;
  LPEXIT_PROCESS_DEBUG_INFO = UNTRACED REF EXIT_PROCESS_DEBUG_INFO;

  LPLOAD_DLL_DEBUG_INFO = UNTRACED REF LOAD_DLL_DEBUG_INFO;
  LOAD_DLL_DEBUG_INFO = RECORD
    hFile                : HANDLE;
    lpBaseOfDll          : LPVOID;
    dwDebugInfoFileOffset: DWORD;
    nDebugInfoSize       : DWORD;
  END;

  UNLOAD_DLL_DEBUG_INFO = RECORD lpBaseOfDll: LPVOID;  END;
  LPUNLOAD_DLL_DEBUG_INFO = UNTRACED REF UNLOAD_DLL_DEBUG_INFO;

  LPOUTPUT_DEBUG_STRING_INFO = UNTRACED REF OUTPUT_DEBUG_STRING_INFO;
  OUTPUT_DEBUG_STRING_INFO = RECORD
    lpDebugStringData : LPSTR;
    fUnicode          : WORD;
    nDebugStringLength: WORD;
  END;

  LPRIP_INFO = UNTRACED REF RIP_INFO;
  RIP_INFO = RECORD
    dwError: DWORD;
    dwType : DWORD;
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
    dwDebugEventCode: DWORD;
    dwProcessId     : DWORD;
    dwThreadId      : DWORD;
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
    dwDebugEventCode: DWORD;
    dwProcessId     : DWORD;
    dwThreadId      : DWORD;
    Exception       : EXCEPTION_DEBUG_INFO;
  END;

  LPDEBUG_CREATE_THREAD = UNTRACED REF DEBUG_CREATE_THREAD;
  DEBUG_CREATE_THREAD = RECORD
    dwDebugEventCode: DWORD;
    dwProcessId     : DWORD;
    dwThreadId      : DWORD;
    CreateThread    : CREATE_THREAD_DEBUG_INFO;
  END;

  LPDEBUG_CREATE_PROCESS = UNTRACED REF DEBUG_CREATE_PROCESS;
  DEBUG_CREATE_PROCESS = RECORD
    dwDebugEventCode : DWORD;
    dwProcessId      : DWORD;
    dwThreadId       : DWORD;
    CreateProcessInfo: CREATE_PROCESS_DEBUG_INFO;
  END;

  LPDEBUG_EXIT_THREAD = UNTRACED REF DEBUG_EXIT_THREAD;
  DEBUG_EXIT_THREAD = RECORD
    dwDebugEventCode: DWORD;
    dwProcessId     : DWORD;
    dwThreadId      : DWORD;
    ExitThread      : EXIT_THREAD_DEBUG_INFO;
  END;

  LPDEBUG_EXIT_PROCESS = UNTRACED REF DEBUG_EXIT_PROCESS;
  DEBUG_EXIT_PROCESS = RECORD
    dwDebugEventCode: DWORD;
    dwProcessId     : DWORD;
    dwThreadId      : DWORD;
    ExitProcess     : EXIT_PROCESS_DEBUG_INFO;
  END;

  LPDEBUG_LOAD_DLL = UNTRACED REF DEBUG_LOAD_DLL;
  DEBUG_LOAD_DLL = RECORD
    dwDebugEventCode: DWORD;
    dwProcessId     : DWORD;
    dwThreadId      : DWORD;
    LoadDll         : LOAD_DLL_DEBUG_INFO;
  END;

  LPDEBUG_UNLOAD_DLL = UNTRACED REF DEBUG_UNLOAD_DLL;
  DEBUG_UNLOAD_DLL = RECORD
    dwDebugEventCode: DWORD;
    dwProcessId     : DWORD;
    dwThreadId      : DWORD;
    UnloadDll       : UNLOAD_DLL_DEBUG_INFO;
  END;

  LPDEBUG_OUTPUT_STRING = UNTRACED REF DEBUG_OUTPUT_STRING;
  DEBUG_OUTPUT_STRING = RECORD
    dwDebugEventCode: DWORD;
    dwProcessId     : DWORD;
    dwThreadId      : DWORD;
    DebugString     : OUTPUT_DEBUG_STRING_INFO;
  END;

  LPDEBUG_RIP = UNTRACED REF DEBUG_RIP;
  DEBUG_RIP = RECORD
    dwDebugEventCode: DWORD;
    dwProcessId     : DWORD;
    dwThreadId      : DWORD;
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

CONST
  FILE_TYPE_UNKNOWN = 16_0000;
  FILE_TYPE_DISK    = 16_0001;
  FILE_TYPE_CHAR    = 16_0002;
  FILE_TYPE_PIPE    = 16_0003;
  FILE_TYPE_REMOTE  = 16_8000;


  STD_INPUT_HANDLE : DWORD = -10;
  STD_OUTPUT_HANDLE: DWORD = -11;
  STD_ERROR_HANDLE : DWORD = -12;

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

CONST
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

CONST
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

CONST
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

CONST
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
CONST
  PURGE_TXABORT = 16_0001;      (* Kill the pending/current writes to the
                                   comm port. *)
  PURGE_RXABORT = 16_0002;      (* Kill the pending/current reads to the
                                   comm port. *)
  PURGE_TXCLEAR = 16_0004;      (* Kill the transmit queue if there. *)
  PURGE_RXCLEAR = 16_0008;      (* Kill the typeahead buffer if there. *)

  LPTx = 16_80;                 (* Set if ID is for LPT device *)

(* Modem Status Flags *)
CONST
  MS_CTS_ON : DWORD = 16_0010;
  MS_DSR_ON : DWORD = 16_0020;
  MS_RING_ON: DWORD = 16_0040;
  MS_RLSD_ON: DWORD = 16_0080;

(* WaitSoundState() Constants *)

CONST
  S_QUEUEEMPTY   = 0;
  S_THRESHOLD    = 1;
  S_ALLTHRESHOLD = 2;

(* Accent Modes *)

CONST
  S_NORMAL   = 0;
  S_LEGATO   = 1;
  S_STACCATO = 2;

(* SetSoundNoise() Sources *)

CONST
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
  LPOFSTRUCT = UNTRACED REF OFSTRUCT;
  OFSTRUCT = RECORD
    cBytes    : BYTE;
    fFixedDisk: BYTE;
    nErrCode  : WORD;
    Reserved1 : WORD;
    Reserved2 : WORD;
    szPathName: ARRAY [0 .. OFS_MAXPATHNAME - 1] OF BYTE;
  END;

<*EXTERNAL InterlockedIncrement:WINAPI*>
PROCEDURE InterlockedIncrement (lpAddend: LPLONG): LONG;

<*EXTERNAL InterlockedDecrement:WINAPI*>
PROCEDURE InterlockedDecrement (lpAddend: LPLONG): LONG;

<*EXTERNAL FreeResource:WINAPI*>
PROCEDURE FreeResource (hResData: HGLOBAL): BOOL;

<*EXTERNAL LockResource:WINAPI*>
PROCEDURE LockResource (hResData: HGLOBAL): LPVOID;

(*???  #define UnlockResource(hResData) ((hResData), 0) *)

CONST MAXINTATOM = 16_C000;

(*???  #define MAKEINTATOM(i) (LPTSTR)((DWORD)((WORD)(i))) INVALID_ATOM =
   ((ATOM)0); *)

<*EXTERNAL WinMain:WINAPI*>
PROCEDURE WinMain (hInstance    : HINSTANCE;
                   hPrevInstance: HINSTANCE;
                   lpCmdLine    : LPSTR;
                   nShowCmd     : int        ): int;

<*EXTERNAL FreeLibrary:WINAPI*>
PROCEDURE FreeLibrary (hLibModule: HMODULE): BOOL;

<*EXTERNAL FreeLibraryAndExitThread:WINAPI*>
PROCEDURE FreeLibraryAndExitThread (hLibModule: HMODULE;  dwExitCode: DWORD);

<*EXTERNAL DisableThreadLibraryCalls:WINAPI*>
PROCEDURE DisableThreadLibraryCalls (hLibModule: HMODULE): BOOL;

<*EXTERNAL GetProcAddress:WINAPI*>
PROCEDURE GetProcAddress (hModule: HINSTANCE; lpProcName: LPCSTR): FARPROC;

<*EXTERNAL GetVersion:WINAPI*>
PROCEDURE GetVersion (): DWORD;

<*EXTERNAL GlobalAlloc:WINAPI*>
PROCEDURE GlobalAlloc (uFlags: UINT; dwBytes: DWORD): HGLOBAL;

<*EXTERNAL GlobalReAlloc:WINAPI*>
PROCEDURE GlobalReAlloc (hMem: HGLOBAL; dwBytes: DWORD; uFlags: UINT): HGLOBAL;

<*EXTERNAL GlobalSize:WINAPI*>
PROCEDURE GlobalSize (hMem: HGLOBAL): DWORD;

<*EXTERNAL GlobalFlags:WINAPI*>
PROCEDURE GlobalFlags (hMem: HGLOBAL): UINT;

<*EXTERNAL GlobalLock:WINAPI*>
PROCEDURE GlobalLock (hMem: HGLOBAL): LPVOID;

(*!!!MWH My version win31 = DWORD WINAPI GlobalHandle(UINT) *)
<*EXTERNAL GlobalHandle:WINAPI*>
PROCEDURE GlobalHandle (pMem: LPVOID): HGLOBAL;

<*EXTERNAL GlobalUnlock:WINAPI*>
PROCEDURE GlobalUnlock (hMem: HGLOBAL): BOOL;

<*EXTERNAL GlobalFree:WINAPI*>
PROCEDURE GlobalFree (hMem: HGLOBAL): HGLOBAL;

<*EXTERNAL GlobalCompact:WINAPI*>
PROCEDURE GlobalCompact (dwMinFree: DWORD): UINT;

<*EXTERNAL GlobalFix:WINAPI*>
PROCEDURE GlobalFix (hMem: HGLOBAL);

<*EXTERNAL GlobalUnfix:WINAPI*>
PROCEDURE GlobalUnfix (hMem: HGLOBAL);

<*EXTERNAL GlobalWire:WINAPI*>
PROCEDURE GlobalWire (hMem: HGLOBAL): LPVOID;

<*EXTERNAL GlobalUnWire:WINAPI*>
PROCEDURE GlobalUnWire (hMem: HGLOBAL): BOOL;

<*EXTERNAL GlobalMemoryStatus:WINAPI*>
PROCEDURE GlobalMemoryStatus (lpBuffer: LPMEMORYSTATUS);

<*EXTERNAL LocalAlloc:WINAPI*>
PROCEDURE LocalAlloc (uFlags: UINT; uBytes: UINT): HLOCAL;

<*EXTERNAL LocalReAlloc:WINAPI*>
PROCEDURE LocalReAlloc (hMem: HLOCAL; uBytes: UINT; uFlags: UINT): HLOCAL;

<*EXTERNAL LocalLock:WINAPI*>
PROCEDURE LocalLock (hMem: HLOCAL): LPVOID;

<*EXTERNAL LocalHandle:WINAPI*>
PROCEDURE LocalHandle (pMem: LPVOID): HLOCAL;

<*EXTERNAL LocalUnlock:WINAPI*>
PROCEDURE LocalUnlock (hMem: HLOCAL): BOOL;

<*EXTERNAL LocalSize:WINAPI*>
PROCEDURE LocalSize (hMem: HLOCAL): UINT;

<*EXTERNAL LocalFlags:WINAPI*>
PROCEDURE LocalFlags (hMem: HLOCAL): UINT;

<*EXTERNAL LocalFree:WINAPI*>
PROCEDURE LocalFree (hMem: HLOCAL): HLOCAL;

<*EXTERNAL LocalShrink:WINAPI*>
PROCEDURE LocalShrink (hMem: HLOCAL; cbNewSize: UINT): UINT;

<*EXTERNAL LocalCompact:WINAPI*>
PROCEDURE LocalCompact (uMinFree: UINT): UINT;

<*EXTERNAL FlushInstructionCache:WINAPI*>
PROCEDURE FlushInstructionCache (hProcess     : HANDLE;
                                 lpBaseAddress: LPVOID;
                                 dwSize       : DWORD   ): BOOL;

<*EXTERNAL VirtualAlloc:WINAPI*>
PROCEDURE VirtualAlloc (lpAddress       : LPVOID;
                        dwSize          : DWORD;
                        flAllocationType: DWORD;
                        flProtect       : DWORD   ): LPVOID;

<*EXTERNAL VirtualFree:WINAPI*>
PROCEDURE VirtualFree (lpAddress : LPVOID;
                       dwSize    : DWORD;
                       dwFreeType: DWORD   ): BOOL;

<*EXTERNAL VirtualProtect:WINAPI*>
PROCEDURE VirtualProtect (lpAddress     : DWORD;  (** was LPVOID -- don't want GC check --- WKK **)
                          dwSize        : DWORD;
                          flNewProtect  : DWORD;
                          lpflOldProtect: PDWORD  ): BOOL;

<*EXTERNAL VirtualQuery:WINAPI*>
PROCEDURE VirtualQuery (lpAddress: LPVOID;
                        lpBuffer : WinNT.PMEMORY_BASIC_INFORMATION;
                        dwLength : DWORD                            ): DWORD;

<*EXTERNAL VirtualProtectEx:WINAPI*>
PROCEDURE VirtualProtectEx (hProcess      : HANDLE;
                            lpAddress     : LPVOID;
                            dwSize        : DWORD;
                            flNewProtect  : DWORD;
                            lpflOldProtect: PDWORD  ): BOOL;

<*EXTERNAL VirtualQueryEx:WINAPI*>
PROCEDURE VirtualQueryEx (hProcess : HANDLE;
                          lpAddress: LPVOID;
                          lpBuffer : WinNT.PMEMORY_BASIC_INFORMATION;
                          dwLength : DWORD                            ): DWORD;

<*EXTERNAL HeapCreate:WINAPI*>
PROCEDURE HeapCreate (flOptions    : DWORD;
                      dwInitialSize: DWORD;
                      dwMaximumSize: DWORD  ): HANDLE;

<*EXTERNAL HeapDestroy:WINAPI*>
PROCEDURE HeapDestroy (hHeap: HANDLE): BOOL;

<*EXTERNAL HeapAlloc:WINAPI*>
PROCEDURE HeapAlloc (hHeap: HANDLE; dwFlags: DWORD;  dwBytes: DWORD): LPVOID;

<*EXTERNAL HeapReAlloc:WINAPI*>
PROCEDURE HeapReAlloc (hHeap   : HANDLE;
                       dwFlags : DWORD;
                       lpMem   : LPVOID;
                       dwBytes : DWORD): LPVOID;

<*EXTERNAL HeapFree:WINAPI*>
PROCEDURE HeapFree (hHeap: HANDLE;  dwFlags: DWORD;  lpMem: LPSTR): BOOL;

<*EXTERNAL HeapSize:WINAPI*>
PROCEDURE HeapSize (hHeap: HANDLE;  dwFlags: DWORD;  lpMem: LPSTR): DWORD;

<*EXTERNAL HeapValidate:WINAPI*>
PROCEDURE HeapValidate (hHeap: HANDLE; dwFlags: DWORD; lpMem: LPCVOID): BOOL;

<*EXTERNAL HeapCompact:WINAPI*>
PROCEDURE HeapCompact (hHeap: HANDLE; dwFlags: DWORD): UINT;

<*EXTERNAL GetProcessHeap:WINAPI*>
PROCEDURE GetProcessHeap (): HANDLE;

<*EXTERNAL GetProcessHeaps:WINAPI*>
PROCEDURE GetProcessHeaps (NumberOfHeaps: DWORD; ProcessHeaps: PHANDLE): DWORD;

TYPE
  union_Block = RECORD
    hMem       : HANDLE;
    dwReserved : ARRAY [0..2] OF DWORD;
  END;

  union_Region = RECORD
    dwCommittedSize   : DWORD;
    dwUnCommittedSize : DWORD;
    lpFirstBlock      : LPVOID;
    lpLastBlock       : LPVOID;
  END;

TYPE
  PPROCESS_HEAP_ENTRY_Block = UNTRACED REF PROCESS_HEAP_ENTRY_Block;
  LPPROCESS_HEAP_ENTRY_Block = UNTRACED REF PROCESS_HEAP_ENTRY_Block;
  PROCESS_HEAP_ENTRY_Block = RECORD
    lpData       : PVOID;
    cbData       : DWORD;
    cbOverhead   : BYTE;
    iRegionIndex : BYTE;
    wFlags       : WORD;
    Block        : union_Block;
  END;

TYPE
  PPROCESS_HEAP_ENTRY_Region = UNTRACED REF PROCESS_HEAP_ENTRY_Region;
  LPPROCESS_HEAP_ENTRY_Region = UNTRACED REF PROCESS_HEAP_ENTRY_Region;
  PROCESS_HEAP_ENTRY_Region = RECORD
    lpData       : PVOID;
    cbData       : DWORD;
    cbOverhead   : BYTE;
    iRegionIndex : BYTE;
    wFlags       : WORD;
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
PROCEDURE GetBinaryTypeA (lpApplicationName: LPCSTR; lpBinaryType: LPDWORD): BOOL;

<*EXTERNAL GetBinaryTypeW:WINAPI*>
PROCEDURE GetBinaryTypeW (lpApplicationName: LPCWSTR; lpBinaryType: LPDWORD): BOOL;

CONST GetBinaryType = GetBinaryTypeA;

<*EXTERNAL GetShortPathNameA:WINAPI*>
PROCEDURE GetShortPathNameA (lpszLongPath  : LPCSTR;
                             lpszShortPath : LPSTR;
                             cchBuffer     : DWORD): DWORD;

<*EXTERNAL GetShortPathNameW:WINAPI*>
PROCEDURE GetShortPathNameW (lpszLongPath  : LPCWSTR;
                             lpszShortPath : LPWSTR;
                             cchBuffer     : DWORD): DWORD;

CONST GetShortPathName = GetShortPathNameA;

<*EXTERNAL GetProcessAffinityMask:WINAPI*>
PROCEDURE GetProcessAffinityMask (hProcess              : HANDLE;
                                  lpProcessAffinityMask : LPDWORD;
                                  lpSystemAffinityMask  : LPDWORD): BOOL;

<*EXTERNAL GetProcessTimes:WINAPI*>
PROCEDURE GetProcessTimes (hProcess       : HANDLE;
                           lpCreationTime : LPFILETIME;
                           lpExitTime     : LPFILETIME;
                           lpKernelTime   : LPFILETIME;
                           lpUserTime     : LPFILETIME ): BOOL;

<*EXTERNAL GetProcessWorkingSetSize:WINAPI*>
PROCEDURE GetProcessWorkingSetSize (hProcess                : HANDLE;
                                    lpMinimumWorkingSetSize : LPDWORD;
                                    lpMaximumWorkingSetSize : LPDWORD ): BOOL;

<*EXTERNAL SetProcessWorkingSetSize:WINAPI*>
PROCEDURE SetProcessWorkingSetSize (hProcess                : HANDLE;
                                    dwMinimumWorkingSetSize : DWORD;
                                    dwMaximumWorkingSetSize : DWORD ): BOOL;

<*EXTERNAL OpenProcess:WINAPI*>
PROCEDURE OpenProcess (dwDesiredAccess: DWORD;
                         bInheritHandle : BOOL;
                         dwProcessId    : DWORD  ): HANDLE;

<*EXTERNAL GetCurrentProcess:WINAPI*>
PROCEDURE GetCurrentProcess (): HANDLE;

<*EXTERNAL GetCurrentProcessId:WINAPI*>
PROCEDURE GetCurrentProcessId (): DWORD;

<*EXTERNAL ExitProcess:WINAPI*>
PROCEDURE ExitProcess (uExitCode: UINT);

<*EXTERNAL TerminateProcess:WINAPI*>
PROCEDURE TerminateProcess (hProcess: HANDLE; uExitCode: UINT): BOOL;

<*EXTERNAL GetExitCodeProcess:WINAPI*>
PROCEDURE GetExitCodeProcess (hProcess: HANDLE; lpExitCode: LPDWORD): BOOL;


<*EXTERNAL FatalExit:WINAPI*>
PROCEDURE FatalExit (ExitCode: int);

<*EXTERNAL GetEnvironmentStringsA:WINAPI*>
PROCEDURE GetEnvironmentStringsA (): LPSTR;

<*EXTERNAL GetEnvironmentStringsW:WINAPI*>
PROCEDURE GetEnvironmentStringsW (): LPWSTR;

CONST GetEnvironmentStrings = GetEnvironmentStringsA;

<*EXTERNAL FreeEnvironmentStringsA:WINAPI*>
PROCEDURE FreeEnvironmentStringsA (lpStr: LPSTR): BOOL;

<*EXTERNAL FreeEnvironmentStringsW:WINAPI*>
PROCEDURE FreeEnvironmentStringsW (lpStr: LPWSTR): BOOL;

CONST FreeEnvironmentStrings = FreeEnvironmentStringsA;

<*EXTERNAL RaiseException:WINAPI*>
PROCEDURE RaiseException (dwExceptionCode   : DWORD;
                          dwExceptionFlags  : DWORD;
                          nNumberOfArguments: DWORD;
                          lpArguments       : LPDWORD);

<*EXTERNAL UnhandledExceptionFilter:WINAPI*>
PROCEDURE UnhandledExceptionFilter (ExceptionInfo: WinNT.PEXCEPTION_POINTERS
                                     ): LONG;

TYPE
  LPTOP_LEVEL_EXCEPTION_FILTER = PTOP_LEVEL_EXCEPTION_FILTER;
  PTOP_LEVEL_EXCEPTION_FILTER
    = <*WINAPI*> PROCEDURE (ExceptionInfo: WinNT.PEXCEPTION_POINTERS): LONG;

<*EXTERNAL SetUnhandledExceptionFilter:WINAPI*>
PROCEDURE SetUnhandledExceptionFilter (
                     lpTopLevelExceptionFilter: LPTOP_LEVEL_EXCEPTION_FILTER
                                      ): PTOP_LEVEL_EXCEPTION_FILTER;

<*EXTERNAL CreateThread:WINAPI*>
PROCEDURE CreateThread (lpThreadAttributes: LPSECURITY_ATTRIBUTES;
                        dwStackSize       : DWORD;
                        lpStartAddress    : LPTHREAD_START_ROUTINE;
                        lpParameter       : LPVOID;
                        dwCreationFlags   : DWORD;
                        lpThreadId        : LPDWORD                 ): HANDLE;

<*EXTERNAL CreateRemoteThread:WINAPI*>
PROCEDURE CreateRemoteThread (hProcess          : HANDLE;
                              lpThreadAttributes: LPSECURITY_ATTRIBUTES;
                              dwStackSize       : DWORD;
                              lpStartAddress : LPTHREAD_START_ROUTINE;
                              lpParameter    : LPVOID;
                              dwCreationFlags: DWORD;
                              lpThreadId     : LPDWORD              ): HANDLE;

<*EXTERNAL GetCurrentThread:WINAPI*>
PROCEDURE GetCurrentThread (): HANDLE;

<*EXTERNAL GetCurrentThreadId:WINAPI*>
PROCEDURE GetCurrentThreadId (): DWORD;

<*EXTERNAL SetThreadAffinityMask:WINAPI*>
PROCEDURE SetThreadAffinityMask (hThread              : HANDLE;
                                 dwThreadAffinityMask : DWORD): DWORD;

<*EXTERNAL SetThreadPriority:WINAPI*>
PROCEDURE SetThreadPriority (hThread: HANDLE; nPriority: int): BOOL;

<*EXTERNAL GetThreadPriority:WINAPI*>
PROCEDURE GetThreadPriority (hThread: HANDLE): int;

<*EXTERNAL GetThreadTimes:WINAPI*>
PROCEDURE GetThreadTimes (hThread        : HANDLE;
                          lpCreationTime : LPFILETIME;
                          lpExitTime     : LPFILETIME;
                          lpKernelTime   : LPFILETIME;
                          lpUserTime     : LPFILETIME): BOOL;

<*EXTERNAL ExitThread:WINAPI*>
PROCEDURE ExitThread (dwExitCode: DWORD);

<*EXTERNAL TerminateThread:WINAPI*>
PROCEDURE TerminateThread (hThread: HANDLE; dwExitCode: DWORD): BOOL;

<*EXTERNAL GetExitCodeThread:WINAPI*>
PROCEDURE GetExitCodeThread (hThread: HANDLE; lpExitCode: LPDWORD): BOOL;

<*EXTERNAL GetThreadSelectorEntry:WINAPI*>
PROCEDURE GetThreadSelectorEntry (hThread        : HANDLE;
                                  dwSelector     : DWORD;
                                  lpSelectorEntry: LPLDT_ENTRY): BOOL;

<*EXTERNAL GetLastError:WINAPI*>
PROCEDURE GetLastError (): DWORD;

<*EXTERNAL SetLastError:WINAPI*>
PROCEDURE SetLastError (dwErrCode: DWORD);

(* SetLastErrorEx() types. *)
CONST
  SLE_ERROR      = 16_00000001;
  SLE_MINORERROR = 16_00000002;
  SLE_WARNING    = 16_00000003;

<*EXTERNAL SetLastErrorEx:WINAPI*>
PROCEDURE SetLastErrorEx (dwErrCode: DWORD; dwType: DWORD);

<*EXTERNAL GetOverlappedResult:WINAPI*>
PROCEDURE GetOverlappedResult (hFile                     : HANDLE;
                               lpOverlapped              : LPOVERLAPPED;
                               lpNumberOfBytesTransferred: LPDWORD;
                               bWait                     : BOOL        ): BOOL;

<*EXTERNAL CreateIoCompletionPort:WINAPI*>
PROCEDURE CreateIoCompletionPort (FileHandle               : HANDLE;
                                  ExistingCompletionPort   : HANDLE;
                                  CompletionKey            : DWORD;
                                  NumberOfConcurrentThreads: DWORD  ): HANDLE;

<*EXTERNAL GetQueuedCompletionStatus:WINAPI*>
PROCEDURE GetQueuedCompletionStatus (CompletionPort    : HANDLE;
                                     lpNumberOfBytesTransferred : LPDWORD;
                                     lpCompletionKey   : LPDWORD;
                                     lpOverlapped      : UNTRACED REF LPOVERLAPPED;
                                     dwMilliseconds    : DWORD ): BOOL;

<*EXTERNAL PostQueuedCompletionStatus:WINAPI*>
PROCEDURE PostQueuedCompletionStatus (CompletionPort            : HANDLE;
                                      dwNumberOfBytesTransferred: DWORD;
                                      dwCompletionKey           : DWORD;
                                      lpOverlapped              : LPOVERLAPPED): BOOL;

CONST
  SEM_FAILCRITICALERRORS     = 16_0001;
  SEM_NOGPFAULTERRORBOX      = 16_0002;
  SEM_NOALIGNMENTFAULTEXCEPT = 16_0004;
  SEM_NOOPENFILEERRORBOX     = 16_8000;

<*EXTERNAL SetDebugErrorLevel:WINAPI*>
PROCEDURE SetDebugErrorLevel (dwLevel: DWORD);

<*EXTERNAL SetErrorMode:WINAPI*>
PROCEDURE SetErrorMode (uMode: UINT): UINT;


<*EXTERNAL ReadProcessMemory:WINAPI*>
PROCEDURE ReadProcessMemory (hProcess           : HANDLE;
                             lpBaseAddress      : LPVOID;
                             lpBuffer           : LPVOID;
                             nSize              : DWORD;
                             lpNumberOfBytesRead: LPDWORD ): BOOL;

<*EXTERNAL WriteProcessMemory:WINAPI*>
PROCEDURE WriteProcessMemory (hProcess              : HANDLE;
                              lpBaseAddress         : LPVOID;
                              lpBuffer              : LPVOID;
                              nSize                 : DWORD;
                              lpNumberOfBytesWritten: LPDWORD ): BOOL;

<*EXTERNAL GetThreadContext:WINAPI*>
PROCEDURE GetThreadContext (hThread: HANDLE; lpContext: LPCONTEXT): BOOL;

<*EXTERNAL SetThreadContext:WINAPI*>
PROCEDURE SetThreadContext (hThread: HANDLE; lpContext: LPCONTEXT): BOOL;

<*EXTERNAL SuspendThread:WINAPI*>
PROCEDURE SuspendThread (hThread: HANDLE): DWORD;

<*EXTERNAL ResumeThread:WINAPI*>
PROCEDURE ResumeThread (hThread: HANDLE): DWORD;

<*EXTERNAL DebugBreak:WINAPI*>
PROCEDURE DebugBreak ();

<*EXTERNAL WaitForDebugEvent:WINAPI*>
PROCEDURE WaitForDebugEvent (lpDebugEvent  : LPDEBUG_EVENT;
                             dwMilliseconds: DWORD          ): BOOL;

<*EXTERNAL ContinueDebugEvent:WINAPI*>
PROCEDURE ContinueDebugEvent (dwProcessId     : DWORD;
                              dwThreadId      : DWORD;
                              dwContinueStatus: DWORD  ): BOOL;

<*EXTERNAL DebugActiveProcess:WINAPI*>
PROCEDURE DebugActiveProcess (dwProcessId: DWORD): BOOL;

<*EXTERNAL DebugSnapShotProcessHeaps:WINAPI*>
PROCEDURE DebugSnapShotProcessHeaps (dwProcessId: DWORD): HANDLE;

TYPE
  PDEBUG_HEAP_BACKTRACE = UNTRACED REF DEBUG_HEAP_BACKTRACE;
  DEBUG_HEAP_BACKTRACE = RECORD
    Depth          : DWORD;
    ReturnAddresses: ARRAY [0 .. 0] OF DWORD;
  END;

  PDEBUG_HEAP_ALLOCATOR = UNTRACED REF DEBUG_HEAP_ALLOCATOR;
  DEBUG_HEAP_ALLOCATOR = RECORD
    TotalBytesAllocated: DWORD;
    OffsetToBackTrace  : DWORD;
  END;

  PDEBUG_HEAP_ENTRY = UNTRACED REF DEBUG_HEAP_ENTRY;
  DEBUG_HEAP_ENTRY = RECORD
    Address  : DWORD;
    Size     : USHORT;
    Allocator: USHORT;
  END;

CONST
  DEBUG_HEAP_ENTRY_FREE (*!!!:USHORT*) = -1;

TYPE
  PDEBUG_HEAP_INFO = UNTRACED REF DEBUG_HEAP_INFO;
  DEBUG_HEAP_INFO = RECORD
    Creator                : USHORT;
    HeaderSizeLog2         : USHORT;
    ReservedPages          : USHORT;
    CommittedPages         : USHORT;
    AllocatorReservedPages : USHORT;
    AllocatorCommittedPages: USHORT;
    NumberOfEntries        : DWORD;
    Entries: ARRAY [0 .. 0] OF DEBUG_HEAP_ENTRY;
  END;

  PDEBUG_HEAP_DUMP = UNTRACED REF DEBUG_HEAP_DUMP;
  DEBUG_HEAP_DUMP = RECORD
    OffsetToAllocatorTable: DWORD;
    NumberOfHeaps         : DWORD;
    HeapInfo: ARRAY [0 .. 0] OF DEBUG_HEAP_INFO;
  END;

<*EXTERNAL InitializeCriticalSection:WINAPI*>
PROCEDURE InitializeCriticalSection (lpCriticalSection: LPCRITICAL_SECTION);

<*EXTERNAL EnterCriticalSection:WINAPI*>
PROCEDURE EnterCriticalSection (lpCriticalSection: LPCRITICAL_SECTION);

<*EXTERNAL LeaveCriticalSection:WINAPI*>
PROCEDURE LeaveCriticalSection (lpCriticalSection: LPCRITICAL_SECTION);

<*EXTERNAL DeleteCriticalSection:WINAPI*>
PROCEDURE DeleteCriticalSection (lpCriticalSection: LPCRITICAL_SECTION);

<*EXTERNAL SetEvent:WINAPI*>
PROCEDURE SetEvent (hEvent: HANDLE): BOOL;

<*EXTERNAL ResetEvent:WINAPI*>
PROCEDURE ResetEvent (hEvent: HANDLE): BOOL;

<*EXTERNAL PulseEvent:WINAPI*>
PROCEDURE PulseEvent (hEvent: HANDLE): BOOL;

<*EXTERNAL ReleaseSemaphore:WINAPI*>
PROCEDURE ReleaseSemaphore (hSemaphore     : HANDLE;
                            lReleaseCount  : LONG;
                            lpPreviousCount: LPLONG  ): BOOL;

<*EXTERNAL ReleaseMutex:WINAPI*>
PROCEDURE ReleaseMutex (hMutex: HANDLE): BOOL;

<*EXTERNAL WaitForSingleObject:WINAPI*>
PROCEDURE WaitForSingleObject (hHandle: HANDLE; dwMilliseconds: DWORD): DWORD;

<*EXTERNAL WaitForMultipleObjects:WINAPI*>
PROCEDURE WaitForMultipleObjects (nCount        : DWORD;
                                  lpHandles     : LPHANDLE;
                                  bWaitAll      : BOOL;
                                  dwMilliseconds: DWORD     ): DWORD;

<*EXTERNAL Sleep:WINAPI*>
PROCEDURE Sleep (dwMilliseconds: DWORD);

<*EXTERNAL LoadResource:WINAPI*>
PROCEDURE LoadResource (hModule: HINSTANCE; hResInfo: HRSRC): HRSRC;

<*EXTERNAL SizeofResource:WINAPI*>
PROCEDURE SizeofResource (hModule: HINSTANCE; hResInfo: HRSRC): DWORD;


<*EXTERNAL CloseProfileUserMapping:WINAPI*>
PROCEDURE CloseProfileUserMapping (): BOOLEAN;

<*EXTERNAL OpenProfileUserMapping:WINAPI*>
PROCEDURE OpenProfileUserMapping (): BOOLEAN;


<*EXTERNAL GlobalDeleteAtom:WINAPI*>
PROCEDURE GlobalDeleteAtom (nAtom: ATOM): ATOM;

<*EXTERNAL InitAtomTable:WINAPI*>
PROCEDURE InitAtomTable (nSize: DWORD): BOOL;

<*EXTERNAL DeleteAtom:WINAPI*>
PROCEDURE DeleteAtom (nAtom: ATOM): ATOM;

<*EXTERNAL SetHandleCount:WINAPI*>
PROCEDURE SetHandleCount (uNumber: UINT): UINT;

<*EXTERNAL GetLogicalDrives:WINAPI*>
PROCEDURE GetLogicalDrives (): DWORD;

<*EXTERNAL LockFile:WINAPI*>
PROCEDURE LockFile (hFile                   : HANDLE;
                    dwFileOffsetLow         : DWORD;
                    dwFileOffsetHigh        : DWORD;
                    nNumberOfBytesToLockLow : DWORD;
                    nNumberOfBytesToLockHigh: DWORD   ): BOOL;

<*EXTERNAL UnlockFile:WINAPI*>
PROCEDURE UnlockFile (hFile                     : HANDLE;
                      dwFileOffsetLow           : DWORD;
                      dwFileOffsetHigh          : DWORD;
                      nNumberOfBytesToUnlockLow : DWORD;
                      nNumberOfBytesToUnlockHigh: DWORD   ): BOOL;

<*EXTERNAL LockFileEx:WINAPI*>
PROCEDURE LockFileEx (hFile                   : HANDLE;
                      dwFlags                 : DWORD;
                      dwReserved              : DWORD;
                      nNumberOfBytesToLockLow : DWORD;
                      nNumberOfBytesToLockHigh: DWORD;
                      lpOverlapped            : LPOVERLAPPED): BOOL;

CONST
  LOCKFILE_FAIL_IMMEDIATELY = 16_00000001;
  LOCKFILE_EXCLUSIVE_LOCK   = 16_00000002;

<*EXTERNAL UnlockFileEx:WINAPI*>
PROCEDURE UnlockFileEx (hFile                     : HANDLE;
                        dwReserved                : DWORD;
                        nNumberOfBytesToUnlockLow : DWORD;
                        nNumberOfBytesToUnlockHigh: DWORD;
                        lpOverlapped              : LPOVERLAPPED): BOOL;


TYPE
  PBY_HANDLE_FILE_INFORMATION = UNTRACED REF BY_HANDLE_FILE_INFORMATION;
  LPBY_HANDLE_FILE_INFORMATION = UNTRACED REF BY_HANDLE_FILE_INFORMATION;
  BY_HANDLE_FILE_INFORMATION = RECORD
    dwFileAttributes    : DWORD;
    ftCreationTime      : FILETIME;
    ftLastAccessTime    : FILETIME;
    ftLastWriteTime     : FILETIME;
    dwVolumeSerialNumber: DWORD;
    nFileSizeHigh       : DWORD;
    nFileSizeLow        : DWORD;
    nNumberOfLinks      : DWORD;
    nFileIndexHigh      : DWORD;
    nFileIndexLow       : DWORD;
  END;

<*EXTERNAL GetFileInformationByHandle:WINAPI*>
PROCEDURE GetFileInformationByHandle (hFile: HANDLE;
              lpFileInformation: LPBY_HANDLE_FILE_INFORMATION): BOOL;

<*EXTERNAL GetFileType:WINAPI*>
PROCEDURE GetFileType (hFile: HANDLE): DWORD;

<*EXTERNAL GetFileSize:WINAPI*>
PROCEDURE GetFileSize (hFile: HANDLE; lpFileSizeHigh: LPDWORD): DWORD;

<*EXTERNAL GetStdHandle:WINAPI*>
PROCEDURE GetStdHandle (nStdHandle: DWORD): HANDLE;

<*EXTERNAL SetStdHandle:WINAPI*>
PROCEDURE SetStdHandle (nStdHandle: DWORD; hHandle: HANDLE): BOOL;

<*EXTERNAL WriteFile:WINAPI*>
PROCEDURE WriteFile (hFile                 : HANDLE;
                     lpBuffer              : void_star;
                     nNumberOfBytesToWrite : DWORD;
                     lpNumberOfBytesWritten: LPDWORD;
                     lpOverlapped          : LPOVERLAPPED): BOOL;

<*EXTERNAL ReadFile:WINAPI*>
PROCEDURE ReadFile (hFile               : HANDLE;
                    lpBuffer            : LPVOID;
                    nNumberOfBytesToRead: DWORD;
                    lpNumberOfBytesRead : LPDWORD;
                    lpOverlapped        : LPOVERLAPPED): BOOL;

<*EXTERNAL FlushFileBuffers:WINAPI*>
PROCEDURE FlushFileBuffers (hFile: HANDLE): BOOL;

<*EXTERNAL DeviceIoControl:WINAPI*>
PROCEDURE DeviceIoControl (hDevice        : HANDLE;
                           dwIoControlCode: DWORD;
                           lpInBuffer     : LPVOID;
                           nInBufferSize  : DWORD;
                           lpOutBuffer    : LPVOID;
                           nOutBufferSize : DWORD;
                           lpBytesReturned: LPDWORD;
                           lpOverlapped   : LPOVERLAPPED): BOOL;

<*EXTERNAL SetEndOfFile:WINAPI*>
PROCEDURE SetEndOfFile (hFile: HANDLE): BOOL;

<*EXTERNAL SetFilePointer:WINAPI*>
PROCEDURE SetFilePointer (hFile               : HANDLE;
                          lDistanceToMove     : LONG;
                          lpDistanceToMoveHigh: PLONG;
                          dwMoveMethod        : DWORD   ): DWORD;

<*EXTERNAL FindClose:WINAPI*>
PROCEDURE FindClose (hFindFile: HANDLE): BOOL;

<*EXTERNAL GetFileTime:WINAPI*>
PROCEDURE GetFileTime (hFile           : HANDLE;
                       lpCreationTime  : LPFILETIME;
                       lpLastAccessTime: LPFILETIME;
                       lpLastWriteTime : LPFILETIME  ): BOOL;

<*EXTERNAL SetFileTime:WINAPI*>
PROCEDURE SetFileTime (hFile           : HANDLE;
                       lpCreationTime  : LPFILETIME;
                       lpLastAccessTime: LPFILETIME;
                       lpLastWriteTime : LPFILETIME  ): BOOL;

<*EXTERNAL CloseHandle:WINAPI*>
PROCEDURE CloseHandle (hObject: HANDLE): BOOL;

<*EXTERNAL DuplicateHandle:WINAPI*>
PROCEDURE DuplicateHandle (hSourceProcessHandle: HANDLE;
                           hSourceHandle       : HANDLE;
                           hTargetProcessHandle: HANDLE;
                           lpTargetHandle      : LPHANDLE;
                           dwDesiredAccess     : DWORD;
                           bInheritHandle      : BOOL;
                           dwOptions           : DWORD     ): BOOL;

<*EXTERNAL LoadModule:WINAPI*>
PROCEDURE LoadModule (lpModuleName: LPCSTR; lpParameterBlock: LPVOID): DWORD;

<*EXTERNAL WinExec:WINAPI*>
PROCEDURE WinExec (lpCmdLine: LPCSTR; uCmdShow: UINT): UINT;

(*???  Redefined later--sjrh <*EXTERNAL BuildCommDCB:WINAPI*>
PROCEDURE BuildCommDCB( lpDef: LPSTR; lpDCB: LPDCB ):BOOL;
*)

<*EXTERNAL ClearCommBreak:WINAPI*>
PROCEDURE ClearCommBreak (hFile: HANDLE): BOOL;

<*EXTERNAL ClearCommError:WINAPI*>
PROCEDURE ClearCommError (hFile   : HANDLE;
                          lpErrors: LPDWORD;
                          lpStat  : LPCOMSTAT): BOOL;

<*EXTERNAL SetupComm:WINAPI*>
PROCEDURE SetupComm (hFile: HANDLE; dwInQueue: DWORD; dwOutQueue: DWORD): BOOL;

<*EXTERNAL EscapeCommFunction:WINAPI*>
PROCEDURE EscapeCommFunction (hFile: HANDLE; dwFunc: DWORD): BOOL;

<*EXTERNAL GetCommMask:WINAPI*>
PROCEDURE GetCommMask (hFile: HANDLE; lpEvtMask: LPDWORD): BOOL;

<*EXTERNAL GetCommProperties:WINAPI*>
PROCEDURE GetCommProperties (hFile: HANDLE; lpCommProp: LPCOMMPROP): BOOL;

<*EXTERNAL GetCommModemStatus:WINAPI*>
PROCEDURE GetCommModemStatus (hFile: HANDLE; lpModemStat: LPDWORD): BOOL;

<*EXTERNAL GetCommState:WINAPI*>
PROCEDURE GetCommState (hFile: HANDLE; lpDCB: LPDCB): BOOL;

<*EXTERNAL GetCommTimeouts:WINAPI*>
PROCEDURE GetCommTimeouts (hFile: HANDLE; lpCommTimeouts: LPCOMMTIMEOUTS):BOOL;

<*EXTERNAL PurgeComm:WINAPI*>
PROCEDURE PurgeComm (hFile: HANDLE; dwFlags: DWORD): BOOL;

<*EXTERNAL SetCommBreak:WINAPI*>
PROCEDURE SetCommBreak (hFile: HANDLE): BOOL;

<*EXTERNAL SetCommMask:WINAPI*>
PROCEDURE SetCommMask (hFile: HANDLE; dwEvtMask: DWORD): BOOL;

<*EXTERNAL SetCommState:WINAPI*>
PROCEDURE SetCommState (hFile: HANDLE; lpDCB: LPDCB): BOOL;

<*EXTERNAL SetCommTimeouts:WINAPI*>
PROCEDURE SetCommTimeouts (hFile: HANDLE; lpCommTimeouts: LPCOMMTIMEOUTS):BOOL;

<*EXTERNAL TransmitCommChar:WINAPI*>
PROCEDURE TransmitCommChar (hFile: HANDLE; cChar: char): BOOL;

<*EXTERNAL WaitCommEvent:WINAPI*>
PROCEDURE WaitCommEvent (hFile       : HANDLE;
                         lpEvtMask   : LPDWORD;
                         lpOverlapped: LPOVERLAPPED): BOOL;


<*EXTERNAL SetTapePosition:WINAPI*>
PROCEDURE SetTapePosition (hDevice         : HANDLE;
                           dwPositionMethod: DWORD;
                           dwPartition     : DWORD;
                           dwOffsetLow     : DWORD;
                           dwOffsetHigh    : DWORD;
                           bImmediate      : BOOL): DWORD;

<*EXTERNAL GetTapePosition:WINAPI*>
PROCEDURE GetTapePosition (hDevice       : HANDLE;
                           dwPositionType: DWORD;
                           lpdwPartition : LPDWORD;
                           lpdwOffsetLow : LPDWORD;
                           lpdwOffsetHigh: LPDWORD  ): DWORD;

<*EXTERNAL PrepareTape:WINAPI*>
PROCEDURE PrepareTape (hDevice     : HANDLE;
                       dwOperation : DWORD;
                       bImmediate  : BOOL): DWORD;

<*EXTERNAL EraseTape:WINAPI*>
PROCEDURE EraseTape (hDevice     : HANDLE;
                     dwEraseType : DWORD;
                     bImmediate  : BOOL): DWORD;

<*EXTERNAL CreateTapePartition:WINAPI*>
PROCEDURE CreateTapePartition (hDevice          : HANDLE;
                               dwPartitionMethod: DWORD;
                               dwCount          : DWORD;
                               dwSize           : DWORD   ): DWORD;

<*EXTERNAL WriteTapemark:WINAPI*>
PROCEDURE WriteTapemark (hDevice        : HANDLE;
                         dwTapemarkType : DWORD;
                         dwTapemarkCount: DWORD;
                         bImmediate     : BOOL): DWORD;

<*EXTERNAL GetTapeStatus:WINAPI*>
PROCEDURE GetTapeStatus (hDevice: HANDLE): DWORD;

<*EXTERNAL GetTapeParameters:WINAPI*>
PROCEDURE GetTapeParameters (hDevice          : HANDLE;
                             dwOperation      : DWORD;
                             lpdwSize         : LPDWORD;
                             lpTapeInformation: LPVOID   ): DWORD;

CONST
  GET_TAPE_MEDIA_INFORMATION = 0;
  GET_TAPE_DRIVE_INFORMATION = 1;

<*EXTERNAL SetTapeParameters:WINAPI*>
PROCEDURE SetTapeParameters (hDevice          : HANDLE;
                             dwOperation      : DWORD;
                             lpTapeInformation: LPVOID  ): DWORD;

CONST
  SET_TAPE_MEDIA_INFORMATION = 0;
  SET_TAPE_DRIVE_INFORMATION = 1;

<*EXTERNAL Beep:WINAPI*>
PROCEDURE Beep (dwFreq: DWORD; dwDuration: DWORD): BOOL;

<*EXTERNAL OpenSound:WINAPI*>
PROCEDURE OpenSound ();

<*EXTERNAL CloseSound:WINAPI*>
PROCEDURE CloseSound ();

<*EXTERNAL StartSound:WINAPI*>
PROCEDURE StartSound ();

<*EXTERNAL StopSound:WINAPI*>
PROCEDURE StopSound ();

<*EXTERNAL WaitSoundState:WINAPI*>
PROCEDURE WaitSoundState (nState: DWORD): DWORD;

<*EXTERNAL SyncAllVoices:WINAPI*>
PROCEDURE SyncAllVoices (): DWORD;

<*EXTERNAL CountVoiceNotes:WINAPI*>
PROCEDURE CountVoiceNotes (nVoice: DWORD): DWORD;

<*EXTERNAL GetThresholdEvent:WINAPI*>
PROCEDURE GetThresholdEvent (): LPDWORD;

<*EXTERNAL GetThresholdStatus:WINAPI*>
PROCEDURE GetThresholdStatus (): DWORD;

<*EXTERNAL SetSoundNoise:WINAPI*>
PROCEDURE SetSoundNoise (nSource: DWORD; nDuration: DWORD): DWORD;

<*EXTERNAL SetVoiceAccent:WINAPI*>
PROCEDURE SetVoiceAccent (nVoice : DWORD;
                          nTempo : DWORD;
                          nVolume: DWORD;
                          nMode  : DWORD;
                          nPitch : DWORD  ): DWORD;

<*EXTERNAL SetVoiceEnvelope:WINAPI*>
PROCEDURE SetVoiceEnvelope (nVoice, nShape, nRepeat: DWORD): DWORD;

<*EXTERNAL SetVoiceNote:WINAPI*>
PROCEDURE SetVoiceNote (nVoice, nValue, nLength, nCdots: DWORD): DWORD;

<*EXTERNAL SetVoiceQueueSize:WINAPI*>
PROCEDURE SetVoiceQueueSize (nVoice: DWORD; nBytes: DWORD): DWORD;

<*EXTERNAL SetVoiceSound:WINAPI*>
PROCEDURE SetVoiceSound (nVoice, Frequency, nDuration: DWORD): DWORD;

<*EXTERNAL SetVoiceThreshold:WINAPI*>
PROCEDURE SetVoiceThreshold (nVoice: DWORD; nNotes: DWORD): DWORD;

<*EXTERNAL MulDiv:WINAPI*>
PROCEDURE MulDiv (nNumber: int; nNumerator: int; nDenominator: int): int;

<*EXTERNAL GetSystemTime:WINAPI*>
PROCEDURE GetSystemTime (lpSystemTime: LPSYSTEMTIME);

<*EXTERNAL SetSystemTime:WINAPI*>
PROCEDURE SetSystemTime (lpSystemTime: LPSYSTEMTIME): BOOL;

<*EXTERNAL GetLocalTime:WINAPI*>
PROCEDURE GetLocalTime (lpSystemTime: LPSYSTEMTIME);

<*EXTERNAL SetLocalTime:WINAPI*>
PROCEDURE SetLocalTime (lpSystemTime: LPSYSTEMTIME): BOOL;

<*EXTERNAL GetSystemInfo:WINAPI*>
PROCEDURE GetSystemInfo (lpSystemInfo: LPSYSTEM_INFO);

TYPE
  PTIME_ZONE_INFORMATION = UNTRACED REF TIME_ZONE_INFORMATION;
  LPTIME_ZONE_INFORMATION = UNTRACED REF TIME_ZONE_INFORMATION;
  TIME_ZONE_INFORMATION = RECORD
    Bias        : LONG;
    StandardName: ARRAY [0 .. 31] OF WCHAR;
    StandardDate: SYSTEMTIME;
    StandardBias: LONG;
    DaylightName: ARRAY [0 .. 31] OF WCHAR;
    DaylightDate: SYSTEMTIME;
    DaylightBias: LONG;
  END;

<*EXTERNAL GetTimeZoneInformation:WINAPI*>
PROCEDURE GetTimeZoneInformation
            (lpTimeZoneInformation: LPTIME_ZONE_INFORMATION): DWORD;

<*EXTERNAL SetTimeZoneInformation:WINAPI*>
PROCEDURE SetTimeZoneInformation
            (lpTimeZoneInformation: LPTIME_ZONE_INFORMATION): BOOL;

(* Routines to convert back and forth between system time and file time *)

<*EXTERNAL SystemTimeToFileTime:WINAPI*>
PROCEDURE SystemTimeToFileTime (lpSystemTime: LPSYSTEMTIME;
                                lpFileTime  : LPFILETIME    ): BOOL;

(* Note: As of 14-May-97, the following routine is implemented on
   Windows/NT, but not on Windows 95. -CAH *)
<*EXTERNAL SystemTimeToTzSpecificLocalTime:WINAPI*>
PROCEDURE SystemTimeToTzSpecificLocalTime (
            lpTimeZoneInformation: LPTIME_ZONE_INFORMATION;
            lpUniversalTime      : LPSYSTEMTIME;
            lpLocalTime          : LPSYSTEMTIME            ): BOOL;

<*EXTERNAL FileTimeToLocalFileTime:WINAPI*>
PROCEDURE FileTimeToLocalFileTime (lpFileTime     : LPFILETIME;
                                   lpLocalFileTime: LPFILETIME  ): BOOL;

<*EXTERNAL LocalFileTimeToFileTime:WINAPI*>
PROCEDURE LocalFileTimeToFileTime (lpLocalFileTime: LPFILETIME;
                                   lpFileTime     : LPFILETIME  ): BOOL;

<*EXTERNAL FileTimeToSystemTime:WINAPI*>
PROCEDURE FileTimeToSystemTime (lpFileTime  : LPFILETIME;
                                lpSystemTime: LPSYSTEMTIME): BOOL;

<*EXTERNAL CompareFileTime:WINAPI*>
PROCEDURE CompareFileTime (lpFileTime1: LPFILETIME;
                           lpFileTime2: LPFILETIME  ): LONG;

<*EXTERNAL FileTimeToDosDateTime:WINAPI*>
PROCEDURE FileTimeToDosDateTime (lpFileTime: LPFILETIME;
                                 lpFatDate : LPWORD;
                                 lpFatTime : LPWORD      ): BOOL;

<*EXTERNAL DosDateTimeToFileTime:WINAPI*>
PROCEDURE DosDateTimeToFileTime (wFatDate  : WORD;
                                 wFatTime  : WORD;
                                 lpFileTime: LPFILETIME): BOOL;

<*EXTERNAL GetTickCount:WINAPI*>
PROCEDURE GetTickCount (): DWORD;

<*EXTERNAL SetSystemTimeAdjustment:WINAPI*>
PROCEDURE SetSystemTimeAdjustment (dwTimeAdjustment        : DWORD;
                                   bTimeAdjustmentDisabled : BOOL): BOOL;

<*EXTERNAL GetSystemTimeAdjustment:WINAPI*>
PROCEDURE GetSystemTimeAdjustment (lpTimeAdjustment         : PDWORD;
                                   lpTimeIncrement          : PDWORD;
                                   lpTimeAdjustmentDisabled : LPBOOL): BOOL;

<*EXTERNAL FormatMessageA:WINAPI*>
PROCEDURE FormatMessageA (dwFlags     : DWORD;
                          lpSource    : LPVOID;
                          dwMessageId : DWORD;
                          dwLanguageId: DWORD;
                          lpBuffer    : LPSTR;
                          nSize       : DWORD;
                          lpArguments : LPVOID  ): DWORD;

<*EXTERNAL FormatMessageW:WINAPI*>
PROCEDURE FormatMessageW (dwFlags     : DWORD;
                          lpSource    : LPVOID;
                          dwMessageId : DWORD;
                          dwLanguageId: DWORD;
                          lpBuffer    : LPWSTR;
                          nSize       : DWORD;
                          lpArguments : LPVOID  ): DWORD;

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
                      lpPipeAttributes: LPSECURITY_ATTRIBUTES;
                      nSize           : DWORD                  ): BOOL;

<*EXTERNAL ConnectNamedPipe:WINAPI*>
PROCEDURE ConnectNamedPipe (hNamedPipe  : HANDLE;
                            lpOverlapped: LPOVERLAPPED): BOOL;

<*EXTERNAL DisconnectNamedPipe:WINAPI*>
PROCEDURE DisconnectNamedPipe (hNamedPipe: HANDLE): BOOL;

<*EXTERNAL SetNamedPipeHandleState:WINAPI*>
PROCEDURE SetNamedPipeHandleState (hNamedPipe          : HANDLE;
                                   lpMode              : LPDWORD;
                                   lpMaxCollectionCount: LPDWORD;
                                   lpCollectDataTimeout: LPDWORD  ): BOOL;

<*EXTERNAL GetNamedPipeInfo:WINAPI*>
PROCEDURE GetNamedPipeInfo (hNamedPipe     : HANDLE;
                            lpFlags        : LPDWORD;
                            lpOutBufferSize: LPDWORD;
                            lpInBufferSize : LPDWORD;
                            lpMaxInstances : LPDWORD  ): BOOL;

<*EXTERNAL PeekNamedPipe:WINAPI*>
PROCEDURE PeekNamedPipe (hNamedPipe            : HANDLE;
                         lpBuffer              : LPVOID;
                         nBufferSize           : DWORD;
                         lpBytesRead           : LPDWORD;
                         lpTotalBytesAvail     : LPDWORD;
                         lpBytesLeftThisMessage: LPDWORD  ): BOOL;

<*EXTERNAL TransactNamedPipe:WINAPI*>
PROCEDURE TransactNamedPipe (hNamedPipe    : HANDLE;
                             lpInBuffer    : LPVOID;
                             nInBufferSize : DWORD;
                             lpOutBuffer   : LPVOID;
                             nOutBufferSize: DWORD;
                             lpBytesRead   : LPDWORD;
                             lpOverlapped  : LPOVERLAPPED): BOOL;

<*EXTERNAL CreateMailslotA:WINAPI*>
PROCEDURE CreateMailslotA (lpName              : LPSTR;
                           nMaxMessageSize     : DWORD;
                           lReadTimeout        : DWORD;
                           lpSecurityAttributes: LPSECURITY_ATTRIBUTES):HANDLE;

<*EXTERNAL CreateMailslotW:WINAPI*>
PROCEDURE CreateMailslotW (lpName              : LPWSTR;
                           nMaxMessageSize     : DWORD;
                           lReadTimeout        : DWORD;
                           lpSecurityAttributes: LPSECURITY_ATTRIBUTES):HANDLE;

CONST CreateMailslot = CreateMailslotA;

<*EXTERNAL GetMailslotInfo:WINAPI*>
PROCEDURE GetMailslotInfo (hMailslot       : HANDLE;
                           lpMaxMessageSize: LPDWORD;
                           lpNextSize      : LPDWORD;
                           lpMessageCount  : LPDWORD;
                           lpReadTimeout   : LPDWORD  ): BOOL;

<*EXTERNAL SetMailslotInfo:WINAPI*>
PROCEDURE SetMailslotInfo (hMailslot: HANDLE; lReadTimeout: DWORD): BOOL;

<*EXTERNAL MapViewOfFile:WINAPI*>
PROCEDURE MapViewOfFile (hFileMappingObject  : HANDLE;
                         dwDesiredAccess     : DWORD;
                         dwFileOffsetHigh    : DWORD;
                         dwFileOffsetLow     : DWORD;
                         dwNumberOfBytesToMap: DWORD   ): LPVOID;

<*EXTERNAL FlushViewOfFile:WINAPI*>
PROCEDURE FlushViewOfFile (lpBaseAddress         : LPVOID;
                           dwNumberOfBytesToFlush: DWORD   ): BOOL;

<*EXTERNAL UnmapViewOfFile:WINAPI*>
PROCEDURE UnmapViewOfFile (lpBaseAddress: LPVOID): BOOL;

(* _l Compat Functions *)

<*EXTERNAL lstrcmpA:WINAPI*>
PROCEDURE lstrcmpA (lpString1: LPCSTR; lpString2: LPCSTR): int;
<*EXTERNAL lstrcmpW:WINAPI*>
PROCEDURE lstrcmpW (lpString1: LPCWSTR; lpString2: LPCWSTR): int;

CONST lstrcmp = lstrcmpA;

<*EXTERNAL lstrcmpiA:WINAPI*>
PROCEDURE lstrcmpiA (lpString1: LPCSTR; lpString2: LPCSTR): int;
<*EXTERNAL lstrcmpiW:WINAPI*>
PROCEDURE lstrcmpiW (lpString1: LPCWSTR; lpString2: LPCWSTR): int;

CONST lstrcmpi = lstrcmpiA;

<*EXTERNAL lstrcpynA:WINAPI*>
PROCEDURE lstrcpynA (lpString1: LPSTR; lpString2: LPCSTR; iMaxLength: int): LPSTR;
<*EXTERNAL lstrcpynW:WINAPI*>
PROCEDURE lstrcpynW (lpString1: LPWSTR; lpString2: LPCWSTR; iMaxLenght: int): LPWSTR;

CONST lstrcpyn = lstrcpynA;

<*EXTERNAL lstrcpyA:WINAPI*>
PROCEDURE lstrcpyA (lpString1: LPSTR; lpString2: LPCSTR): LPSTR;
<*EXTERNAL lstrcpyW:WINAPI*>
PROCEDURE lstrcpyW (lpString1: LPWSTR; lpString2: LPCWSTR): LPWSTR;

CONST lstrcpy = lstrcpyA;

<*EXTERNAL lstrcatA:WINAPI*>
PROCEDURE lstrcatA (lpString1: LPSTR; lpString2: LPCSTR): LPSTR;
<*EXTERNAL lstrcatW:WINAPI*>
PROCEDURE lstrcatW (lpString1: LPWSTR; lpString2: LPCWSTR): LPWSTR;
CONST lstrcat = lstrcatA;

<*EXTERNAL lstrlenA:WINAPI*>
PROCEDURE lstrlenA (lpString: LPCSTR): int;
<*EXTERNAL lstrlenW:WINAPI*>
PROCEDURE lstrlenW (lpString: LPCWSTR): int;
CONST lstrlen = lstrlenA;

<*EXTERNAL OpenFile:WINAPI*>
PROCEDURE OpenFile (lpFileName  : LPCSTR;
                    lpReOpenBuff: LPOFSTRUCT;
                    uStyle      : UINT        ): HFILE;

<*EXTERNAL "_lopen":WINAPI*>
PROCEDURE lopen (lpPathName: LPCSTR; iReadWrite: int): HFILE;

<*EXTERNAL "_lcreat":WINAPI*>
PROCEDURE lcreat (lpPathName: LPCSTR; iAttribute: int): HFILE;

<*EXTERNAL "_lread":WINAPI*>
PROCEDURE lread (hFile: HFILE; lpBuffer: LPVOID; uBytes: UINT): UINT;

<*EXTERNAL "_lwrite":WINAPI*>
PROCEDURE lwrite (hFile: HFILE; lpBuffer: LPCSTR; uBytes: UINT): UINT;

<*EXTERNAL "_lclose":WINAPI*>
PROCEDURE lclose (hFile: HFILE): HFILE;

<*EXTERNAL "_llseek":WINAPI*>
PROCEDURE llseek (hFile: HFILE; lOffset: LONG; iOrigin: int): LONG;

<*EXTERNAL TlsAlloc:WINAPI*>
PROCEDURE TlsAlloc (): DWORD;

CONST
  TLS_OUT_OF_INDEXES    = 16_FFFFFFFF;

(* The values passed to "TlsSetValue" and returned by "TlsGetValue"
   were originally declared "LPVOID".  But, since it's clear that they're
   going to stuff the values somewhere hidden to the collector, any attempt
   to pass an address referring to a traced reference would cause disaster.
   So, to avoid the additional check done by the GC wrappers, we declare
   the values as "DWORD"s instead.  These routines are called *very frequently*
   by the low-level thread and exception machinery.  Avoiding the extra
   wrapper check is worth it.  *)

<*EXTERNAL TlsGetValue:WINAPI*>
PROCEDURE TlsGetValue (dwTlsIndex: DWORD): DWORD;

<*EXTERNAL TlsSetValue:WINAPI*>
PROCEDURE TlsSetValue (dwTlsIndex: DWORD; lpTlsValue: DWORD): BOOL;

<*EXTERNAL TlsFree:WINAPI*>
PROCEDURE TlsFree (dwTlsIndex: DWORD): BOOL;

TYPE
  LPOVERLAPPED_COMPLETION_ROUTINE =
    <*WINAPI*> PROCEDURE (dwErrorCode              : DWORD;
                          dwNumberOfBytesTransfered: DWORD;
                          lpOverlapped             : LPOVERLAPPED);

<*EXTERNAL SleepEx:WINAPI*>
PROCEDURE SleepEx (dwMilliseconds: DWORD; bAlertable: BOOL): DWORD;

<*EXTERNAL WaitForSingleObjectEx:WINAPI*>
PROCEDURE WaitForSingleObjectEx (hHandle       : HANDLE;
                                 dwMilliseconds: DWORD;
                                 bAlertable    : BOOL    ): DWORD;

<*EXTERNAL WaitForMultipleObjectsEx:WINAPI*>
PROCEDURE WaitForMultipleObjectsEx (nCount        : DWORD;
                                    lpHandles     : LPHANDLE;
                                    bWaitAll      : BOOL;
                                    dwMilliseconds: DWORD;
                                      bAlertable    : BOOL      ): DWORD;

<*EXTERNAL ReadFileEx:WINAPI*>
PROCEDURE ReadFileEx (hFile               : HANDLE;
                      lpBuffer            : LPVOID;
                      nNumberOfBytesToRead: DWORD;
                      lpOverlapped        : LPOVERLAPPED;
                      lpCompletionRoutine : LPOVERLAPPED_COMPLETION_ROUTINE): BOOL;

<*EXTERNAL WriteFileEx:WINAPI*>
PROCEDURE WriteFileEx (hFile                : HANDLE;
                       lpBuffer             : LPVOID;
                       nNumberOfBytesToWrite: DWORD;
                       lpOverlapped         : LPOVERLAPPED;
                       lpCompletionRoutine: LPOVERLAPPED_COMPLETION_ROUTINE): BOOL;

<*EXTERNAL BackupRead:WINAPI*>
PROCEDURE BackupRead (hFile               : HANDLE;
                      lpBuffer            : LPBYTE;
                      nNumberOfBytesToRead: DWORD;
                      lpNumberOfBytesRead : LPDWORD;
                      bAbort              : BOOL;
                      bProcessSecurity    : BOOL;
                      lpContext           : UNTRACED REF LPVOID): BOOL;

<*EXTERNAL BackupSeek:WINAPI*>
PROCEDURE BackupSeek (hFile              : HANDLE;
                      dwLowBytesToSeek   : DWORD;
                      dwHighBytesToSeek  : DWORD;
                      lpdwLowByteSeeked  : LPDWORD;
                      lpdwHighByteSeeked : LPDWORD;
                      lpContext          : UNTRACED REF LPVOID): BOOL;

<*EXTERNAL BackupWrite:WINAPI*>
PROCEDURE BackupWrite (hFile                 : HANDLE;
                       lpBuffer              : LPBYTE;
                       nNumberOfBytesToWrite : DWORD;
                       lpNumberOfBytesWritten: LPDWORD;
                       bAbort                : BOOL;
                       bProcessSecurity      : BOOL;
                       lpContext             : UNTRACED REF LPVOID): BOOL;

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
    cb             : DWORD;
    lpReserved     : LPSTR;
    lpDesktop      : LPSTR;
    lpTitle        : LPSTR;
    dwX            : DWORD;
    dwY            : DWORD;
    dwXSize        : DWORD;
    dwYSize        : DWORD;
    dwXCountChars  : DWORD;
    dwYCountChars  : DWORD;
    dwFillAttribute: DWORD;
    dwFlags        : DWORD;
    wShowWindow    : WORD;
    cbReserved2    : WORD;
    lpReserved2    : LPBYTE;
    hStdInput      : HANDLE;
    hStdOutput     : HANDLE;
    hStdError      : HANDLE;
  END;

  LPSTARTUPINFOW = UNTRACED REF STARTUPINFOW;
  STARTUPINFOW = RECORD
    cb             : DWORD;
    lpReserved     : LPWSTR;
    lpDesktop      : LPWSTR;
    lpTitle        : LPWSTR;
    dwX            : DWORD;
    dwY            : DWORD;
    dwXSize        : DWORD;
    dwYSize        : DWORD;
    dwXCountChars  : DWORD;
    dwYCountChars  : DWORD;
    dwFillAttribute: DWORD;
    dwFlags        : DWORD;
    wShowWindow    : WORD;
    cbReserved2    : WORD;
    lpReserved2    : LPBYTE;
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
    dwFileAttributes  : DWORD;
    ftCreationTime    : FILETIME;
    ftLastAccessTime  : FILETIME;
    ftLastWriteTime   : FILETIME;
    nFileSizeHigh     : DWORD;
    nFileSizeLow      : DWORD;
    dwReserved0       : DWORD;
    dwReserved1       : DWORD;
    cFileName         : ARRAY [0 .. MAX_PATH - 1] OF CHAR;
    cAlternateFileName: ARRAY [0 .. 14 - 1] OF CHAR;
  END;

  PWIN32_FIND_DATAW = UNTRACED REF WIN32_FIND_DATAW;
  LPWIN32_FIND_DATAW = UNTRACED REF WIN32_FIND_DATAW;
  WIN32_FIND_DATAW = RECORD
    dwFileAttributes  : DWORD;
    ftCreationTime    : FILETIME;
    ftLastAccessTime  : FILETIME;
    ftLastWriteTime   : FILETIME;
    nFileSizeHigh     : DWORD;
    nFileSizeLow      : DWORD;
    dwReserved0       : DWORD;
    dwReserved1       : DWORD;
    cFileName         : ARRAY [0 .. MAX_PATH - 1] OF WCHAR;
    cAlternateFileName: ARRAY [0 .. 14 - 1] OF WCHAR;
  END;

  WIN32_FIND_DATA   = WIN32_FIND_DATAA;
  PWIN32_FIND_DATA  = PWIN32_FIND_DATAA;
  LPWIN32_FIND_DATA = LPWIN32_FIND_DATAA;

<*EXTERNAL CreateMutexA:WINAPI*>
PROCEDURE CreateMutexA (lpMutexAttributes: LPSECURITY_ATTRIBUTES;
                        bInitialOwner    : BOOL;
                        lpName           : LPSTR                  ): HANDLE;

<*EXTERNAL CreateMutexW:WINAPI*>
PROCEDURE CreateMutexW (lpMutexAttributes: LPSECURITY_ATTRIBUTES;
                        bInitialOwner    : BOOL;
                        lpName           : LPWSTR                 ): HANDLE;

CONST CreateMutex = CreateMutexA;

<*EXTERNAL OpenMutexA:WINAPI*>
PROCEDURE OpenMutexA (dwDesiredAccess: DWORD;
                      bInheritHandle : BOOL;
                      lpName         : LPSTR  ): HANDLE;

<*EXTERNAL OpenMutexW:WINAPI*>
PROCEDURE OpenMutexW (dwDesiredAccess: DWORD;
                      bInheritHandle : BOOL;
                      lpName         : LPWSTR ): HANDLE;

CONST OpenMutex = OpenMutexA;

<*EXTERNAL CreateEventA:WINAPI*>
PROCEDURE CreateEventA (lpEventAttributes: LPSECURITY_ATTRIBUTES;
                        bManualReset     : BOOL;
                        bInitialState    : BOOL;
                        lpName           : LPSTR                  ): HANDLE;

<*EXTERNAL CreateEventW:WINAPI*>
PROCEDURE CreateEventW (lpEventAttributes: LPSECURITY_ATTRIBUTES;
                        bManualReset     : BOOL;
                        bInitialState    : BOOL;
                        lpName           : LPWSTR                 ): HANDLE;

CONST CreateEvent = CreateEventA;

<*EXTERNAL OpenEventA:WINAPI*>
PROCEDURE OpenEventA (dwDesiredAccess: DWORD;
                      bInheritHandle : BOOL;
                      lpName         : LPSTR  ): HANDLE;

<*EXTERNAL OpenEventW:WINAPI*>
PROCEDURE OpenEventW (dwDesiredAccess: DWORD;
                      bInheritHandle : BOOL;
                      lpName         : LPWSTR ): HANDLE;

CONST OpenEvent = OpenEventA;

<*EXTERNAL CreateSemaphoreA:WINAPI*>
PROCEDURE CreateSemaphoreA (lpSemaphoreAttributes: LPSECURITY_ATTRIBUTES;
                            lInitialCount: LONG;
                            lMaximumCount: LONG;
                            lpName       : LPSTR ): HANDLE;

<*EXTERNAL CreateSemaphoreW:WINAPI*>
PROCEDURE CreateSemaphoreW (lpSemaphoreAttributes: LPSECURITY_ATTRIBUTES;
                            lInitialCount: LONG;
                            lMaximumCount: LONG;
                            lpName       : LPWSTR): HANDLE;

CONST CreateSemaphore = CreateSemaphoreA;

<*EXTERNAL OpenSemaphoreA:WINAPI*>
PROCEDURE OpenSemaphoreA (dwDesiredAccess: DWORD;
                          bInheritHandle : BOOL;
                          lpName         : LPSTR  ): HANDLE;

<*EXTERNAL OpenSemaphoreW:WINAPI*>
PROCEDURE OpenSemaphoreW (dwDesiredAccess: DWORD;
                          bInheritHandle : BOOL;
                          lpName         : LPWSTR ): HANDLE;

CONST OpenSemaphore = OpenSemaphoreA;

<*EXTERNAL CreateFileMappingA:WINAPI*>
PROCEDURE CreateFileMappingA (hFile: HANDLE;
                              lpFileMappingAttributes: LPSECURITY_ATTRIBUTES;
                              flProtect        : DWORD;
                              dwMaximumSizeHigh: DWORD;
                              dwMaximumSizeLow : DWORD;
                              lpName           : LPSTR  ): HANDLE;

<*EXTERNAL CreateFileMappingW:WINAPI*>
PROCEDURE CreateFileMappingW (hFile: HANDLE;
                              lpFileMappingAttributes: LPSECURITY_ATTRIBUTES;
                              flProtect        : DWORD;
                              dwMaximumSizeHigh: DWORD;
                              dwMaximumSizeLow : DWORD;
                              lpName           : LPWSTR ): HANDLE;

CONST CreateFileMapping = CreateFileMappingA;

<*EXTERNAL OpenFileMappingA:WINAPI*>
PROCEDURE OpenFileMappingA (dwDesiredAccess: DWORD;
                            bInheritHandle : BOOL;
                            lpName         : LPSTR  ): HANDLE;

<*EXTERNAL OpenFileMappingW:WINAPI*>
PROCEDURE OpenFileMappingW (dwDesiredAccess: DWORD;
                            bInheritHandle : BOOL;
                            lpName         : LPWSTR ): HANDLE;

CONST OpenFileMapping = OpenFileMappingA;

<*EXTERNAL GetLogicalDriveStringsA:WINAPI*>
PROCEDURE GetLogicalDriveStringsA(nBufferLength: DWORD; lpBuffer: LPSTR):DWORD;

<*EXTERNAL GetLogicalDriveStringsW:WINAPI*>
PROCEDURE GetLogicalDriveStringsW(nBufferLength: DWORD; lpBuffer:LPWSTR):DWORD;

CONST GetLogicalDriveStrings = GetLogicalDriveStringsA;

<*EXTERNAL LoadLibraryA:WINAPI*>
PROCEDURE LoadLibraryA (lpLibFileName: LPCSTR): HINSTANCE;

<*EXTERNAL LoadLibraryW:WINAPI*>
PROCEDURE LoadLibraryW (lpLibFileName: LPCWSTR): HINSTANCE;

CONST LoadLibrary = LoadLibraryA;

<*EXTERNAL LoadLibraryExA:WINAPI*>
PROCEDURE LoadLibraryExA (lpLibFileName: LPCSTR;
                          hFile        : HANDLE;
                          dwFlags      : DWORD   ): HINSTANCE;

<*EXTERNAL LoadLibraryExW:WINAPI*>
PROCEDURE LoadLibraryExW (lpLibFileName: LPCWSTR;
                          hFile        : HANDLE;
                          dwFlags      : DWORD    ): HINSTANCE;

CONST LoadLibraryEx = LoadLibraryExA;

CONST DONT_RESOLVE_DLL_REFERENCES = 16_00000001;


<*EXTERNAL GetModuleFileNameA:WINAPI*>
PROCEDURE GetModuleFileNameA (hModule   : HINSTANCE;
                              lpFilename: LPSTR;
                              nSize     : DWORD      ): DWORD;

<*EXTERNAL GetModuleFileNameW:WINAPI*>
PROCEDURE GetModuleFileNameW (hModule   : HINSTANCE;
                              lpFilename: LPWSTR;
                              nSize     : DWORD      ): DWORD;

CONST GetModuleFileName = GetModuleFileNameA;

<*EXTERNAL GetModuleHandleA:WINAPI*>
PROCEDURE GetModuleHandleA (lpModuleName: LPCSTR): HMODULE;

<*EXTERNAL GetModuleHandleW:WINAPI*>
PROCEDURE GetModuleHandleW (lpModuleName: LPCWSTR): HMODULE;

CONST GetModuleHandle = GetModuleHandleA;

<*EXTERNAL CreateProcessA:WINAPI*>
PROCEDURE CreateProcessA (lpApplicationName   : LPCSTR;
                          lpCommandLine       : LPCSTR;
                          lpProcessAttributes : LPSECURITY_ATTRIBUTES;
                          lpThreadAttributes  : LPSECURITY_ATTRIBUTES;
                          bInheritHandles     : BOOL;
                          dwCreationFlags     : DWORD;
                          lpEnvironment       : LPVOID;
                          lpCurrentDirectory  : LPSTR;
                          lpStartupInfo       : LPSTARTUPINFOA;
                          lpProcessInformation: LPPROCESS_INFORMATION  ): BOOL;

<*EXTERNAL CreateProcessW:WINAPI*>
PROCEDURE CreateProcessW (lpApplicationName   : LPCWSTR;
                          lpCommandLine       : LPCWSTR;
                          lpProcessAttributes : LPSECURITY_ATTRIBUTES;
                          lpThreadAttributes  : LPSECURITY_ATTRIBUTES;
                          bInheritHandles     : BOOL;
                          dwCreationFlags     : DWORD;
                          lpEnvironment       : LPVOID;
                          lpCurrentDirectory  : LPWSTR;
                          lpStartupInfo       : LPSTARTUPINFOW;
                          lpProcessInformation: LPPROCESS_INFORMATION  ): BOOL;

CONST CreateProcess = CreateProcessA;

<*EXTERNAL SetProcessShutdownParameters:WINAPI*>
PROCEDURE SetProcessShutdownParameters (dwLevel: DWORD; dwFlags: DWORD): BOOL;

<*EXTERNAL GetProcessShutdownParameters:WINAPI*>
PROCEDURE GetProcessShutdownParameters (lpdwLevel: LPDWORD;
                                        lpdwFlags: LPDWORD  ): BOOL;

<*EXTERNAL FatalAppExitA:WINAPI*>
PROCEDURE FatalAppExitA (uAction: UINT; lpMessageText: LPCSTR);

<*EXTERNAL FatalAppExitW:WINAPI*>
PROCEDURE FatalAppExitW (uAction: UINT; lpMessageText: LPCWSTR);

CONST FatalAppExit = FatalAppExitA;

<*EXTERNAL GetStartupInfoA:WINAPI*>
PROCEDURE GetStartupInfoA (lpStartupInfo: LPSTARTUPINFOA);

<*EXTERNAL GetStartupInfoW:WINAPI*>
PROCEDURE GetStartupInfoW (lpStartupInfo: LPSTARTUPINFOW);

CONST GetStartupInfo = GetStartupInfoA;

<*EXTERNAL GetCommandLineA:WINAPI*>
PROCEDURE GetCommandLineA (): LPSTR;

<*EXTERNAL GetCommandLineW:WINAPI*>
PROCEDURE GetCommandLineW (): LPWSTR;

CONST GetCommandLine = GetCommandLineA;

<*EXTERNAL GetEnvironmentVariableA:WINAPI*>
PROCEDURE GetEnvironmentVariableA (lpName  : LPSTR;
                                   lpBuffer: LPSTR;
                                   nSize   : DWORD  ): DWORD;

<*EXTERNAL GetEnvironmentVariableW:WINAPI*>
PROCEDURE GetEnvironmentVariableW (lpName  : LPWSTR;
                                   lpBuffer: LPWSTR;
                                   nSize   : DWORD   ): DWORD;

CONST GetEnvironmentVariable = GetEnvironmentVariableA;

<*EXTERNAL SetEnvironmentVariableA:WINAPI*>
PROCEDURE SetEnvironmentVariableA (lpName: LPSTR; lpValue: LPSTR): BOOL;

<*EXTERNAL SetEnvironmentVariableW:WINAPI*>
PROCEDURE SetEnvironmentVariableW (lpName: LPWSTR; lpValue: LPWSTR): BOOL;

CONST SetEnvironmentVariable = SetEnvironmentVariableA;

<*EXTERNAL ExpandEnvironmentStringsA:WINAPI*>
PROCEDURE ExpandEnvironmentStringsA (lpSrc: LPCSTR;
                                     lpDst: LPSTR;
                                     nSize: DWORD   ): DWORD;

<*EXTERNAL ExpandEnvironmentStringsW:WINAPI*>
PROCEDURE ExpandEnvironmentStringsW (lpSrc: LPCWSTR;
                                     lpDst: LPWSTR;
                                     nSize: DWORD    ): DWORD;

CONST ExpandEnvironmentStrings = ExpandEnvironmentStringsA;

<*EXTERNAL OutputDebugStringA:WINAPI*>
PROCEDURE OutputDebugStringA (lpOutputString: LPCSTR);

<*EXTERNAL OutputDebugStringW:WINAPI*>
PROCEDURE OutputDebugStringW (lpOutputString: LPCWSTR);

CONST OutputDebugString = OutputDebugStringA;

<*EXTERNAL FindResourceA:WINAPI*>
PROCEDURE FindResourceA (hModule: HINSTANCE;
                         lpName : LPCSTR;
                         lpType : LPCSTR     ): HRSRC;

<*EXTERNAL FindResourceW:WINAPI*>
PROCEDURE FindResourceW (hModule: HINSTANCE;
                         lpName : LPCWSTR;
                         lpType : LPCWSTR    ): HRSRC;

CONST FindResource = FindResourceA;

<*EXTERNAL FindResourceExA:WINAPI*>
PROCEDURE FindResourceExA (hModule  : HINSTANCE;
                           lpType   : LPCSTR;
                           lpName   : LPCSTR;
                           wLanguage: WORD       ): HRSRC;

<*EXTERNAL FindResourceExW:WINAPI*>
PROCEDURE FindResourceExW (hModule  : HINSTANCE;
                           lpType   : LPCWSTR;
                           lpName   : LPCWSTR;
                           wLanguage: WORD       ): HRSRC;

CONST FindResourceEx = FindResourceExA;

TYPE
  ENUMRESTYPEPROC = <*CALLBACK*> PROCEDURE (hModule: HINSTANCE;
                                            lpType : LPTSTR;
                                            lParam : LONG      ): BOOL;

  ENUMRESNAMEPROC = <*CALLBACK*> PROCEDURE (hModule: HINSTANCE;
                                            lpType : LPCTSTR;
                                            lpName : LPTSTR;
                                            lParam : LONG      ): BOOL;

  ENUMRESLANGPROC = <*CALLBACK*> PROCEDURE (hModule  : HINSTANCE;
                                            lpType   : LPCTSTR;
                                            lpName   : LPCTSTR;
                                            wLanguage: WORD;
                                            lParam   : LONG       ): BOOL;


<*EXTERNAL EnumResourceTypesA:WINAPI*>
PROCEDURE EnumResourceTypesA (hModule   : HINSTANCE;
                              lpEnumFunc: ENUMRESTYPEPROC;
                              lParam    : LONG             ): BOOL;

<*EXTERNAL EnumResourceTypesW:WINAPI*>
PROCEDURE EnumResourceTypesW (hModule   : HINSTANCE;
                              lpEnumFunc: ENUMRESTYPEPROC;
                              lParam    : LONG             ): BOOL;

CONST EnumResourceTypes = EnumResourceTypesA;


<*EXTERNAL EnumResourceNamesA:WINAPI*>
PROCEDURE EnumResourceNamesA (hModule   : HINSTANCE;
                              lpType    : LPCSTR;
                              lpEnumFunc: ENUMRESNAMEPROC;
                              lParam    : LONG             ): BOOL;

<*EXTERNAL EnumResourceNamesW:WINAPI*>
PROCEDURE EnumResourceNamesW (hModule   : HINSTANCE;
                              lpType    : LPCWSTR;
                              lpEnumFunc: ENUMRESNAMEPROC;
                              lParam    : LONG             ): BOOL;

CONST EnumResourceNames = EnumResourceNamesA;

<*EXTERNAL EnumResourceLanguagesA:WINAPI*>
PROCEDURE EnumResourceLanguagesA (hModule   : HINSTANCE;
                                  lpType    : LPCSTR;
                                  lpName    : LPCSTR;
                                  lpEnumFunc: ENUMRESLANGPROC;
                                  lParam    : LONG             ): BOOL;

<*EXTERNAL EnumResourceLanguagesW:WINAPI*>
PROCEDURE EnumResourceLanguagesW (hModule   : HINSTANCE;
                                  lpType    : LPCWSTR;
                                  lpName    : LPCWSTR;
                                  lpEnumFunc: ENUMRESLANGPROC;
                                  lParam    : LONG             ): BOOL;

CONST EnumResourceLanguages = EnumResourceLanguagesA;

<*EXTERNAL BeginUpdateResourceA:WINAPI*>
PROCEDURE BeginUpdateResourceA (pFileName               : LPSTR;
                                bDeleteExistingResources: BOOL   ): HANDLE;

<*EXTERNAL BeginUpdateResourceW:WINAPI*>
PROCEDURE BeginUpdateResourceW (pFileName               : LPWSTR;
                                bDeleteExistingResources: BOOL    ): HANDLE;

CONST BeginUpdateResource = BeginUpdateResourceA;

<*EXTERNAL UpdateResourceA:WINAPI*>
PROCEDURE UpdateResourceA (hUpdate  : HANDLE;
                           lpType   : LPSTR;
                           lpName   : LPSTR;
                           wLanguage: WORD;
                           lpData   : LPVOID;
                           cbData   : DWORD   ): BOOL;

<*EXTERNAL UpdateResourceW:WINAPI*>
PROCEDURE UpdateResourceW (hUpdate  : HANDLE;
                           lpType   : LPWSTR;
                           lpName   : LPWSTR;
                           wLanguage: WORD;
                           lpData   : LPVOID;
                           cbData   : DWORD   ): BOOL;

CONST UpdateResource = UpdateResourceA;

<*EXTERNAL EndUpdateResourceA:WINAPI*>
PROCEDURE EndUpdateResourceA (hUpdate: HANDLE; fDiscard: BOOL): BOOL;

<*EXTERNAL EndUpdateResourceW:WINAPI*>
PROCEDURE EndUpdateResourceW (hUpdate: HANDLE; fDiscard: BOOL): BOOL;

CONST EndUpdateResource = EndUpdateResourceA;

<*EXTERNAL GlobalAddAtomA:WINAPI*>
PROCEDURE GlobalAddAtomA (lpString: LPCSTR): ATOM;

<*EXTERNAL GlobalAddAtomW:WINAPI*>
PROCEDURE GlobalAddAtomW (lpString: LPCWSTR): ATOM;

CONST GlobalAddAtom = GlobalAddAtomA;

<*EXTERNAL GlobalFindAtomA:WINAPI*>
PROCEDURE GlobalFindAtomA (lpString: LPCSTR): ATOM;

<*EXTERNAL GlobalFindAtomW:WINAPI*>
PROCEDURE GlobalFindAtomW (lpString: LPCWSTR): ATOM;

CONST GlobalFindAtom = GlobalFindAtomA;

<*EXTERNAL GlobalGetAtomNameA:WINAPI*>
PROCEDURE GlobalGetAtomNameA (nAtom: ATOM; lpBuffer: LPSTR; nSize: int): UINT;

<*EXTERNAL GlobalGetAtomNameW:WINAPI*>
PROCEDURE GlobalGetAtomNameW (nAtom: ATOM; lpBuffer: LPWSTR; nSize: int): UINT;

CONST GlobalGetAtomName = GlobalGetAtomNameA;

<*EXTERNAL AddAtomA:WINAPI*>
PROCEDURE AddAtomA (lpString: LPCSTR): ATOM;

<*EXTERNAL AddAtomW:WINAPI*>
PROCEDURE AddAtomW (lpString: LPCWSTR): ATOM;

CONST AddAtom = AddAtomA;

<*EXTERNAL FindAtomA:WINAPI*>
PROCEDURE FindAtomA (lpString: LPCSTR): ATOM;

<*EXTERNAL FindAtomW:WINAPI*>
PROCEDURE FindAtomW (lpString: LPCWSTR): ATOM;

CONST FindAtom = FindAtomA;

<*EXTERNAL GetAtomNameA:WINAPI*>
PROCEDURE GetAtomNameA (nAtom: ATOM; lpBuffer: LPSTR; nSize: int): UINT;

<*EXTERNAL GetAtomNameW:WINAPI*>
PROCEDURE GetAtomNameW (nAtom: ATOM; lpBuffer: LPWSTR; nSize: int): UINT;

CONST GetAtomName = GetAtomNameA;

<*EXTERNAL GetProfileIntA:WINAPI*>
PROCEDURE GetProfileIntA (lpAppName: LPCSTR;
                          lpKeyName: LPCSTR;
                          nDefault : DWORD   ): UINT;

<*EXTERNAL GetProfileIntW:WINAPI*>
PROCEDURE GetProfileIntW (lpAppName: LPCWSTR;
                          lpKeyName: LPCWSTR;
                          nDefault : DWORD    ): UINT;

CONST GetProfileInt = GetProfileIntA;

<*EXTERNAL GetProfileStringA:WINAPI*>
PROCEDURE GetProfileStringA (lpAppName       : LPCSTR;
                             lpKeyName       : LPCSTR;
                             lpDefault       : LPCSTR;
                             lpReturnedString: LPSTR;
                             nSize           : DWORD   ): DWORD;

<*EXTERNAL GetProfileStringW:WINAPI*>
PROCEDURE GetProfileStringW (lpAppName       : LPCWSTR;
                             lpKeyName       : LPCWSTR;
                             lpDefault       : LPCWSTR;
                             lpReturnedString: LPWSTR;
                             nSize           : DWORD    ): DWORD;

CONST GetProfileString = GetProfileStringA;

<*EXTERNAL WriteProfileStringA:WINAPI*>
PROCEDURE WriteProfileStringA (lpAppName: LPCSTR;
                               lpKeyName: LPCSTR;
                               lpString : LPCSTR  ): BOOL;

<*EXTERNAL WriteProfileStringW:WINAPI*>
PROCEDURE WriteProfileStringW (lpAppName: LPCWSTR;
                               lpKeyName: LPCWSTR;
                               lpString : LPCWSTR  ): BOOL;

CONST WriteProfileString = WriteProfileStringA;

<*EXTERNAL GetProfileSectionA:WINAPI*>
PROCEDURE GetProfileSectionA (lpAppName       : LPCSTR;
                              lpReturnedString: LPSTR;
                              nSize           : DWORD   ): DWORD;

<*EXTERNAL GetProfileSectionW:WINAPI*>
PROCEDURE GetProfileSectionW (lpAppName       : LPCWSTR;
                              lpReturnedString: LPWSTR;
                              nSize           : DWORD    ): DWORD;

CONST GetProfileSection = GetProfileSectionA;

<*EXTERNAL WriteProfileSectionA:WINAPI*>
PROCEDURE WriteProfileSectionA (lpAppName: LPCSTR; lpString: LPCSTR): BOOL;

<*EXTERNAL WriteProfileSectionW:WINAPI*>
PROCEDURE WriteProfileSectionW (lpAppName: LPCWSTR; lpString: LPCWSTR): BOOL;

CONST WriteProfileSection = WriteProfileSectionA;

<*EXTERNAL GetPrivateProfileIntA:WINAPI*>
PROCEDURE GetPrivateProfileIntA (lpAppName : LPCSTR;
                                 lpKeyName : LPCSTR;
                                 nDefault  : DWORD;
                                 lpFileName: LPCSTR  ): UINT;

<*EXTERNAL GetPrivateProfileIntW:WINAPI*>
PROCEDURE GetPrivateProfileIntW (lpAppName : LPCWSTR;
                                 lpKeyName : LPCWSTR;
                                 nDefault  : DWORD;
                                 lpFileName: LPCWSTR  ): UINT;

CONST GetPrivateProfileInt = GetPrivateProfileIntA;

<*EXTERNAL GetPrivateProfileStringA:WINAPI*>
PROCEDURE GetPrivateProfileStringA (lpAppName       : LPCSTR;
                                    lpKeyName       : LPCSTR;
                                    lpDefault       : LPCSTR;
                                    lpReturnedString: LPSTR;
                                    nSize           : DWORD;
                                    lpFileName      : LPCSTR  ): DWORD;

<*EXTERNAL GetPrivateProfileStringW:WINAPI*>
PROCEDURE GetPrivateProfileStringW (lpAppName       : LPCWSTR;
                                    lpKeyName       : LPCWSTR;
                                    lpDefault       : LPCWSTR;
                                    lpReturnedString: LPWSTR;
                                    nSize           : DWORD;
                                    lpFileName      : LPCWSTR  ): DWORD;

CONST GetPrivateProfileString = GetPrivateProfileStringA;

<*EXTERNAL WritePrivateProfileStringA:WINAPI*>
PROCEDURE WritePrivateProfileStringA (lpAppName : LPCSTR;
                                      lpKeyName : LPCSTR;
                                      lpString  : LPCSTR;
                                      lpFileName: LPCSTR  ): BOOL;

<*EXTERNAL WritePrivateProfileStringW:WINAPI*>
PROCEDURE WritePrivateProfileStringW (lpAppName : LPCWSTR;
                                      lpKeyName : LPCWSTR;
                                      lpString  : LPCWSTR;
                                      lpFileName: LPCWSTR  ): BOOL;

CONST WritePrivateProfileString = WritePrivateProfileStringA;

<*EXTERNAL GetPrivateProfileSectionA:WINAPI*>
PROCEDURE GetPrivateProfileSectionA (lpAppName       : LPCSTR;
                                     lpReturnedString: LPSTR;
                                     nSize           : DWORD;
                                     lpFileName      : LPCSTR  ): DWORD;

<*EXTERNAL GetPrivateProfileSectionW:WINAPI*>
PROCEDURE GetPrivateProfileSectionW (lpAppName       : LPCWSTR;
                                     lpReturnedString: LPWSTR;
                                     nSize           : DWORD;
                                     lpFileName      : LPCWSTR  ): DWORD;

CONST GetPrivateProfileSection = GetPrivateProfileSectionA;

<*EXTERNAL WritePrivateProfileSectionA:WINAPI*>
PROCEDURE WritePrivateProfileSectionA (lpAppName : LPCSTR;
                                       lpString  : LPCSTR;
                                       lpFileName: LPCSTR  ): BOOL;

<*EXTERNAL WritePrivateProfileSectionW:WINAPI*>
PROCEDURE WritePrivateProfileSectionW (lpAppName : LPCWSTR;
                                       lpString  : LPCWSTR;
                                       lpFileName: LPCWSTR  ): BOOL;

CONST WritePrivateProfileSection = WritePrivateProfileSectionA;

<*EXTERNAL GetPrivateProfileSectionNamesA:WINAPI*>
PROCEDURE GetPrivateProfileSectionNamesA (lpszReturnBuffer : LPSTR;
                                          nSize            : DWORD;
                                          lpFileName       : LPCSTR): DWORD;

<*EXTERNAL GetPrivateProfileSectionNamesW:WINAPI*>
PROCEDURE GetPrivateProfileSectionNamesW (lpszReturnBuffer : LPWSTR;
                                          nSize            : DWORD;
                                          lpFileName       : LPCWSTR): DWORD;

CONST GetPrivateProfileSectionNames = GetPrivateProfileSectionNamesA;

<*EXTERNAL GetPrivateProfileStructA:WINAPI*>
PROCEDURE GetPrivateProfileStructA (lpszSection : LPCSTR;
                                    lpszKey     : LPCSTR;
                                    lpStruct    : LPVOID;
                                    uSizeStruct : UINT;
                                    szFile      : LPCSTR): BOOL;

<*EXTERNAL GetPrivateProfileStructW:WINAPI*>
PROCEDURE GetPrivateProfileStructW (lpszSection : LPCWSTR;
                                    lpszKey     : LPCWSTR;
                                    lpStruct    : LPVOID;
                                    uSizeStruct : UINT;
                                    szFile      : LPCWSTR): BOOL;

CONST GetPrivateProfileStruct = GetPrivateProfileStructA;

<*EXTERNAL WritePrivateProfileStructA:WINAPI*>
PROCEDURE WritePrivateProfileStructA (lpszSection : LPCSTR;
                                      lpszKey     : LPCSTR;
                                      lpStruct    : LPVOID;
                                      uSizeStruct : UINT;
                                      szFile      : LPCSTR): BOOL;

<*EXTERNAL WritePrivateProfileStructW:WINAPI*>
PROCEDURE WritePrivateProfileStructW (lpszSection : LPCWSTR;
                                      lpszKey     : LPCWSTR;
                                      lpStruct    : LPVOID;
                                      uSizeStruct : UINT;
                                      szFile      : LPCWSTR): BOOL;

CONST WritePrivateProfileStruct = WritePrivateProfileStructA;

<*EXTERNAL GetDriveTypeA:WINAPI*>
PROCEDURE GetDriveTypeA (lpRootPathName: LPSTR): UINT;

<*EXTERNAL GetDriveTypeW:WINAPI*>
PROCEDURE GetDriveTypeW (lpRootPathName: LPWSTR): UINT;

CONST GetDriveType = GetDriveTypeA;

<*EXTERNAL GetSystemDirectoryA:WINAPI*>
PROCEDURE GetSystemDirectoryA (lpBuffer: LPSTR; uSize: UINT): UINT;

<*EXTERNAL GetSystemDirectoryW:WINAPI*>
PROCEDURE GetSystemDirectoryW (lpBuffer: LPWSTR; uSize: UINT): UINT;

CONST GetSystemDirectory = GetSystemDirectoryA;

<*EXTERNAL GetTempPathA:WINAPI*>
PROCEDURE GetTempPathA (nBufferLength: DWORD; lpBuffer: LPSTR): DWORD;

<*EXTERNAL GetTempPathW:WINAPI*>
PROCEDURE GetTempPathW (nBufferLength: DWORD; lpBuffer: LPWSTR): DWORD;

CONST GetTempPath = GetTempPathA;

<*EXTERNAL GetTempFileNameA:WINAPI*>
PROCEDURE GetTempFileNameA (lpPathName    : LPCSTR;
                            lpPrefixString: LPCSTR;
                            uUnique       : UINT;
                            lpTempFileName: LPSTR   ): UINT;

<*EXTERNAL GetTempFileNameW:WINAPI*>
PROCEDURE GetTempFileNameW (lpPathName    : LPCWSTR;
                            lpPrefixString: LPCWSTR;
                            uUnique       : UINT;
                            lpTempFileName: LPWSTR   ): UINT;

CONST GetTempFileName = GetTempFileNameA;

<*EXTERNAL GetWindowsDirectoryA:WINAPI*>
PROCEDURE GetWindowsDirectoryA (lpBuffer: LPSTR; uSize: UINT): UINT;

<*EXTERNAL GetWindowsDirectoryW:WINAPI*>
PROCEDURE GetWindowsDirectoryW (lpBuffer: LPWSTR; uSize: UINT): UINT;

CONST GetWindowsDirectory = GetWindowsDirectoryA;

<*EXTERNAL SetCurrentDirectoryA:WINAPI*>
PROCEDURE SetCurrentDirectoryA (lpPathName: LPSTR): BOOL;

<*EXTERNAL SetCurrentDirectoryW:WINAPI*>
PROCEDURE SetCurrentDirectoryW (lpPathName: LPWSTR): BOOL;

CONST SetCurrentDirectory = SetCurrentDirectoryA;

<*EXTERNAL GetCurrentDirectoryA:WINAPI*>
PROCEDURE GetCurrentDirectoryA (nBufferLength: DWORD; lpBuffer: LPSTR): DWORD;

<*EXTERNAL GetCurrentDirectoryW:WINAPI*>
PROCEDURE GetCurrentDirectoryW (nBufferLength: DWORD; lpBuffer: LPWSTR): DWORD;

CONST GetCurrentDirectory = GetCurrentDirectoryA;

<*EXTERNAL GetDiskFreeSpaceA:WINAPI*>
PROCEDURE GetDiskFreeSpaceA (lpRootPathName         : LPSTR;
                             lpSectorsPerCluster    : LPDWORD;
                             lpBytesPerSector       : LPDWORD;
                             lpNumberOfFreeClusters : LPDWORD;
                             lpTotalNumberOfClusters: LPDWORD  ): BOOL;

<*EXTERNAL GetDiskFreeSpaceW:WINAPI*>
PROCEDURE GetDiskFreeSpaceW (lpRootPathName         : LPWSTR;
                             lpSectorsPerCluster    : LPDWORD;
                             lpBytesPerSector       : LPDWORD;
                             lpNumberOfFreeClusters : LPDWORD;
                             lpTotalNumberOfClusters: LPDWORD  ): BOOL;

CONST GetDiskFreeSpace = GetDiskFreeSpaceA;

<*EXTERNAL CreateDirectoryA:WINAPI*>
PROCEDURE CreateDirectoryA (lpPathName          : LPSTR;
                            lpSecurityAttributes: LPSECURITY_ATTRIBUTES): BOOL;

<*EXTERNAL CreateDirectoryW:WINAPI*>
PROCEDURE CreateDirectoryW (lpPathName          : LPWSTR;
                            lpSecurityAttributes: LPSECURITY_ATTRIBUTES): BOOL;

CONST CreateDirectory = CreateDirectoryA;

<*EXTERNAL CreateDirectoryExA:WINAPI*>
PROCEDURE CreateDirectoryExA (lpTemplateDirectory : LPCSTR;
                              lpNewDirectory      : LPCSTR;
                              lpSecurityAttributes: LPSECURITY_ATTRIBUTES): BOOL;

<*EXTERNAL CreateDirectoryExW:WINAPI*>
PROCEDURE CreateDirectoryExW (lpTemplateDirectory : LPCWSTR;
                              lpNewDirectory      : LPCWSTR;
                              lpSecurityAttributes: LPSECURITY_ATTRIBUTES): BOOL;

CONST CreateDirectoryEx = CreateDirectoryExA;

<*EXTERNAL RemoveDirectoryA:WINAPI*>
PROCEDURE RemoveDirectoryA (lpPathName: LPSTR): BOOL;

<*EXTERNAL RemoveDirectoryW:WINAPI*>
PROCEDURE RemoveDirectoryW (lpPathName: LPWSTR): BOOL;

CONST RemoveDirectory = RemoveDirectoryA;

<*EXTERNAL GetFullPathNameA:WINAPI*>
PROCEDURE GetFullPathNameA (lpFileName   : LPCSTR;
                            nBufferLength: DWORD;
                            lpBuffer     : LPSTR;
                            lpFilePart   : UNTRACED REF LPSTR): DWORD;

<*EXTERNAL GetFullPathNameW:WINAPI*>
PROCEDURE GetFullPathNameW (lpFileName   : LPCWSTR;
                            nBufferLength: DWORD;
                            lpBuffer     : LPWSTR;
                            lpFilePart   : UNTRACED REF LPWSTR): DWORD;

CONST GetFullPathName = GetFullPathNameA;

CONST
  DDD_RAW_TARGET_PATH       = 16_00000001;
  DDD_REMOVE_DEFINITION     = 16_00000002;
  DDD_EXACT_MATCH_ON_REMOVE = 16_00000004;

<*EXTERNAL DefineDosDeviceA:WINAPI*>
PROCEDURE DefineDosDeviceA (dwFlags      : DWORD;
                            lpDeviceName : LPCSTR;
                            lpTargetPath : LPCSTR): BOOL;

<*EXTERNAL DefineDosDeviceW:WINAPI*>
PROCEDURE DefineDosDeviceW (dwFlags      : DWORD;
                            lpDeviceName : LPCWSTR;
                            lpTargetPath : LPCWSTR): BOOL;

CONST DefineDosDevice = DefineDosDeviceA;

<*EXTERNAL QueryDosDeviceA:WINAPI*>
PROCEDURE QueryDosDeviceA (lpDeviceName : LPCSTR;
                           lpTargetPath : LPSTR;
                           ucchMax      : DWORD): DWORD;

<*EXTERNAL QueryDosDeviceW:WINAPI*>
PROCEDURE QueryDosDeviceW (lpDeviceName : LPCWSTR;
                           lpTargetPath : LPWSTR;
                           ucchMax      : DWORD): DWORD;

CONST QueryDosDevice = QueryDosDeviceA;

CONST EXPAND_LOCAL_DRIVES = TRUE;
(* #define EXPAND_LOCAL_DRIVES *)

<*EXTERNAL CreateFileA:WINAPI*>
PROCEDURE CreateFileA (lpFileName           : LPCSTR;
                       dwDesiredAccess      : DWORD;
                       dwShareMode          : DWORD;
                       lpSecurityAttributes : LPSECURITY_ATTRIBUTES;
                       dwCreationDisposition: DWORD;
                       dwFlagsAndAttributes : DWORD;
                       hTemplateFile        : HANDLE                 ): HANDLE;

<*EXTERNAL CreateFileW:WINAPI*>
PROCEDURE CreateFileW (lpFileName           : LPCWSTR;
                       dwDesiredAccess      : DWORD;
                       dwShareMode          : DWORD;
                       lpSecurityAttributes : LPSECURITY_ATTRIBUTES;
                       dwCreationDisposition: DWORD;
                       dwFlagsAndAttributes : DWORD;
                       hTemplateFile        : HANDLE                 ): HANDLE;

CONST CreateFile = CreateFileA;

<*EXTERNAL SetFileAttributesA:WINAPI*>
PROCEDURE SetFileAttributesA (lpFileName: LPSTR; dwFileAttributes: DWORD):BOOL;

<*EXTERNAL SetFileAttributesW:WINAPI*>
PROCEDURE SetFileAttributesW (lpFileName: LPWSTR; dwFileAttributes:DWORD):BOOL;

CONST SetFileAttributes = SetFileAttributesA;

<*EXTERNAL GetFileAttributesA:WINAPI*>
PROCEDURE GetFileAttributesA (lpFileName: LPSTR): DWORD;

<*EXTERNAL GetFileAttributesW:WINAPI*>
PROCEDURE GetFileAttributesW (lpFileName: LPWSTR): DWORD;

CONST GetFileAttributes = GetFileAttributesA;

<*EXTERNAL GetCompressedFileSizeA:WINAPI*>
PROCEDURE GetCompressedFileSizeA (lpFileName     : LPCSTR;
                                  lpFileSizeHigh : LPDWORD): DWORD;

<*EXTERNAL GetCompressedFileSizeW:WINAPI*>
PROCEDURE GetCompressedFileSizeW (lpFileName     : LPCWSTR;
                                  lpFileSizeHigh : LPDWORD): DWORD;

CONST GetCompressedFileSize = GetCompressedFileSizeA;

<*EXTERNAL DeleteFileA:WINAPI*>
PROCEDURE DeleteFileA (lpFileName: LPSTR): BOOL;

<*EXTERNAL DeleteFileW:WINAPI*>
PROCEDURE DeleteFileW (lpFileName: LPWSTR): BOOL;

CONST DeleteFile = DeleteFileA;

<*EXTERNAL FindFirstFileA:WINAPI*>
PROCEDURE FindFirstFileA (lpFileName    : LPSTR;
                          lpFindFileData: LPWIN32_FIND_DATAA): HANDLE;

<*EXTERNAL FindFirstFileW:WINAPI*>
PROCEDURE FindFirstFileW (lpFileName    : LPWSTR;
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
PROCEDURE SearchPathA (lpPath       : LPCSTR;
                       lpFileName   : LPCSTR;
                       lpExtension  : LPCSTR;
                       nBufferLength: DWORD;
                       lpBuffer     : LPSTR;
                       lpFilePart   : UNTRACED REF LPSTR): DWORD;

<*EXTERNAL SearchPathW:WINAPI*>
PROCEDURE SearchPathW (lpPath       : LPCWSTR;
                       lpFileName   : LPCWSTR;
                       lpExtension  : LPCWSTR;
                       nBufferLength: DWORD;
                       lpBuffer     : LPWSTR;
                       lpFilePart   : UNTRACED REF LPWSTR): DWORD;

CONST SearchPath = SearchPathA;

<*EXTERNAL CopyFileA:WINAPI*>
PROCEDURE CopyFileA (lpExistingFileName: LPSTR;
                     lpNewFileName     : LPSTR;
                     bFailIfExists     : BOOL   ): BOOL;

<*EXTERNAL CopyFileW:WINAPI*>
PROCEDURE CopyFileW (lpExistingFileName: LPWSTR;
                     lpNewFileName     : LPWSTR;
                     bFailIfExists     : BOOL    ): BOOL;

CONST CopyFile = CopyFileA;

<*EXTERNAL MoveFileA:WINAPI*>
PROCEDURE MoveFileA (lpExistingFileName: LPSTR; lpNewFileName: LPSTR): BOOL;
<*EXTERNAL MoveFileW:WINAPI*>

PROCEDURE MoveFileW (lpExistingFileName: LPWSTR; lpNewFileName: LPWSTR): BOOL;
CONST MoveFile = MoveFileA;

<*EXTERNAL MoveFileExA:WINAPI*>
PROCEDURE MoveFileExA (lpExistingFileName: LPSTR;
                       lpNewFileName     : LPSTR;
                       dwFlags           : DWORD  ): BOOL;

<*EXTERNAL MoveFileExW:WINAPI*>
PROCEDURE MoveFileExW (lpExistingFileName: LPWSTR;
                       lpNewFileName     : LPWSTR;
                       dwFlags           : DWORD   ): BOOL;

CONST MoveFileEx = MoveFileExA;

CONST
  MOVEFILE_REPLACE_EXISTING   = 16_00000001;
  MOVEFILE_COPY_ALLOWED       = 16_00000002;
  MOVEFILE_DELAY_UNTIL_REBOOT = 16_00000004;

<*EXTERNAL CreateNamedPipeA:WINAPI*>
PROCEDURE CreateNamedPipeA (lpName              : LPSTR;
                            dwOpenMode          : DWORD;
                            dwPipeMode          : DWORD;
                            nMaxInstances       : DWORD;
                            nOutBufferSize      : DWORD;
                            nInBufferSize       : DWORD;
                            nDefaultTimeOut     : DWORD;
                          lpSecurityAttributes: LPSECURITY_ATTRIBUTES): HANDLE;

<*EXTERNAL CreateNamedPipeW:WINAPI*>
PROCEDURE CreateNamedPipeW (lpName              : LPWSTR;
                            dwOpenMode          : DWORD;
                            dwPipeMode          : DWORD;
                            nMaxInstances       : DWORD;
                            nOutBufferSize      : DWORD;
                            nInBufferSize       : DWORD;
                            nDefaultTimeOut     : DWORD;
                          lpSecurityAttributes: LPSECURITY_ATTRIBUTES): HANDLE;

CONST CreateNamedPipe = CreateNamedPipeA;

<*EXTERNAL GetNamedPipeHandleStateA:WINAPI*>
PROCEDURE GetNamedPipeHandleStateA (hNamedPipe          : HANDLE;
                                    lpState             : LPDWORD;
                                    lpCurInstances      : LPDWORD;
                                    lpMaxCollectionCount: LPDWORD;
                                    lpCollectDataTimeout: LPDWORD;
                                    lpUserName          : LPSTR;
                                    nMaxUserNameSize    : DWORD    ): BOOL;

<*EXTERNAL GetNamedPipeHandleStateW:WINAPI*>
PROCEDURE GetNamedPipeHandleStateW (hNamedPipe          : HANDLE;
                                    lpState             : LPDWORD;
                                    lpCurInstances      : LPDWORD;
                                    lpMaxCollectionCount: LPDWORD;
                                    lpCollectDataTimeout: LPDWORD;
                                    lpUserName          : LPWSTR;
                                    nMaxUserNameSize    : DWORD    ): BOOL;

CONST GetNamedPipeHandleState = GetNamedPipeHandleStateA;

<*EXTERNAL CallNamedPipeA:WINAPI*>
PROCEDURE CallNamedPipeA (lpNamedPipeName: LPSTR;
                          lpInBuffer     : LPVOID;
                          nInBufferSize  : DWORD;
                          lpOutBuffer    : LPVOID;
                          nOutBufferSize : DWORD;
                          lpBytesRead    : LPDWORD;
                          nTimeOut       : DWORD    ): BOOL;

<*EXTERNAL CallNamedPipeW:WINAPI*>
PROCEDURE CallNamedPipeW (lpNamedPipeName: LPWSTR;
                          lpInBuffer     : LPVOID;
                          nInBufferSize  : DWORD;
                          lpOutBuffer    : LPVOID;
                          nOutBufferSize : DWORD;
                          lpBytesRead    : LPDWORD;
                          nTimeOut       : DWORD    ): BOOL;

CONST CallNamedPipe = CallNamedPipeA;

<*EXTERNAL WaitNamedPipeA:WINAPI*>
PROCEDURE WaitNamedPipeA (lpNamedPipeName: LPSTR; nTimeOut: DWORD): BOOL;

<*EXTERNAL WaitNamedPipeW:WINAPI*>
PROCEDURE WaitNamedPipeW (lpNamedPipeName: LPWSTR; nTimeOut: DWORD): BOOL;

CONST WaitNamedPipe = WaitNamedPipeA;

<*EXTERNAL SetVolumeLabelA:WINAPI*>
PROCEDURE SetVolumeLabelA (lpRootPathName : LPCSTR;
                           lpVolumeName   : LPCSTR): BOOL;

<*EXTERNAL SetVolumeLabelW:WINAPI*>
PROCEDURE SetVolumeLabelW (lpRootPathName : LPCWSTR;
                           lpVolumeName   : LPCWSTR): BOOL;

CONST SetVolumeLabel = SetVolumeLabelA;

<*EXTERNAL SetFileApisToOEM:WINAPI*>
PROCEDURE SetFileApisToOEM ();

<*EXTERNAL SetFileApisToANSI:WINAPI*>
PROCEDURE SetFileApisToANSI ();

<*EXTERNAL AreFileApisANSI:WINAPI*>
PROCEDURE AreFileApisANSI (): BOOL;

<*EXTERNAL GetVolumeInformationA:WINAPI*>
PROCEDURE GetVolumeInformationA (lpRootPathName          : LPSTR;
                                 lpVolumeNameBuffer      : LPSTR;
                                 nVolumeNameSize         : DWORD;
                                 lpVolumeSerialNumber    : LPDWORD;
                                 lpMaximumComponentLength: LPDWORD;
                                 lpFileSystemFlags       : LPDWORD;
                                 lpFileSystemNameBuffer  : LPSTR;
                                 nFileSystemNameSize     : DWORD    ): BOOL;

<*EXTERNAL GetVolumeInformationW:WINAPI*>
PROCEDURE GetVolumeInformationW (lpRootPathName          : LPWSTR;
                                 lpVolumeNameBuffer      : LPWSTR;
                                 nVolumeNameSize         : DWORD;
                                 lpVolumeSerialNumber    : LPDWORD;
                                 lpMaximumComponentLength: LPDWORD;
                                 lpFileSystemFlags       : LPDWORD;
                                 lpFileSystemNameBuffer  : LPWSTR;
                                 nFileSystemNameSize     : DWORD    ): BOOL;

CONST GetVolumeInformation = GetVolumeInformationA;

(* Event logging APIs *)

<*EXTERNAL ClearEventLogA:WINAPI*>
PROCEDURE ClearEventLogA (hEventLog: HANDLE; lpBackupFileName: LPSTR): BOOL;

<*EXTERNAL ClearEventLogW:WINAPI*>
PROCEDURE ClearEventLogW (hEventLog: HANDLE; lpBackupFileName: LPWSTR): BOOL;

CONST ClearEventLog = ClearEventLogA;

<*EXTERNAL BackupEventLogA:WINAPI*>
PROCEDURE BackupEventLogA (hEventLog: HANDLE; lpBackupFileName: LPSTR): BOOL;

<*EXTERNAL BackupEventLogW:WINAPI*>
PROCEDURE BackupEventLogW (hEventLog: HANDLE; lpBackupFileName: LPWSTR): BOOL;

CONST BackupEventLog = BackupEventLogA;

<*EXTERNAL CloseEventLog:WINAPI*>
PROCEDURE CloseEventLog (hEventLog: HANDLE): BOOL;

<*EXTERNAL DeregisterEventSource:WINAPI*>
PROCEDURE DeregisterEventSource (hEventLog: HANDLE): BOOL;

<*EXTERNAL GetNumberOfEventLogRecords:WINAPI*>
PROCEDURE GetNumberOfEventLogRecords (hEventLog      : HANDLE;
                                      NumberOfRecords: PDWORD  ): BOOL;

<*EXTERNAL GetOldestEventLogRecord:WINAPI*>
PROCEDURE GetOldestEventLogRecord (hEventLog   : HANDLE;
                                   OldestRecord: PDWORD  ): BOOL;

<*EXTERNAL OpenEventLogA:WINAPI*>
PROCEDURE OpenEventLogA (lpUNCServerName: LPSTR; lpSourceName: LPSTR): HANDLE;

<*EXTERNAL OpenEventLogW:WINAPI*>
PROCEDURE OpenEventLogW (lpUNCServerName: LPWSTR; lpSourceName: LPWSTR):HANDLE;

CONST OpenEventLog = OpenEventLogA;

<*EXTERNAL RegisterEventSourceA:WINAPI*>
PROCEDURE RegisterEventSourceA (lpUNCServerName: LPSTR;
                                lpSourceName   : LPSTR  ): HANDLE;

<*EXTERNAL RegisterEventSourceW:WINAPI*>
PROCEDURE RegisterEventSourceW (lpUNCServerName: LPWSTR;
                                lpSourceName   : LPWSTR  ): HANDLE;

CONST RegisterEventSource = RegisterEventSourceA;

<*EXTERNAL OpenBackupEventLogA:WINAPI*>
PROCEDURE OpenBackupEventLogA (lpUNCServerName: LPSTR;
                               lpFileName     : LPSTR):  HANDLE;

<*EXTERNAL OpenBackupEventLogW:WINAPI*>
PROCEDURE OpenBackupEventLogW (lpUNCServerName: LPWSTR;
                               lpFileName     : LPWSTR  ): HANDLE;

CONST OpenBackupEventLog = OpenBackupEventLogA;

<*EXTERNAL ReadEventLogA:WINAPI*>
PROCEDURE ReadEventLogA (hEventLog               : HANDLE;
                         dwReadFlags             : DWORD;
                         dwRecordOffset          : DWORD;
                         lpBuffer                : LPVOID;
                         nNumberOfBytesToRead    : DWORD;
                         pnBytesRead             : UNTRACED REF DWORD;
                         pnMinNumberOfBytesNeeded: UNTRACED REF DWORD  ): BOOL;

<*EXTERNAL ReadEventLogW:WINAPI*>
PROCEDURE ReadEventLogW (hEventLog               : HANDLE;
                         dwReadFlags             : DWORD;
                         dwRecordOffset          : DWORD;
                         lpBuffer                : LPVOID;
                         nNumberOfBytesToRead    : DWORD;
                         pnBytesRead             : UNTRACED REF DWORD;
                         pnMinNumberOfBytesNeeded: UNTRACED REF DWORD  ): BOOL;

CONST ReadEventLog = ReadEventLogA;

<*EXTERNAL ReportEventA:WINAPI*>
PROCEDURE ReportEventA (hEventLog  : HANDLE;
                        wType      : WORD;
                        wCategory  : WORD;
                        dwEventID  : DWORD;
                        lpUserSid  : PSID;
                        wNumStrings: WORD;
                        dwDataSize : DWORD;
                        lpStrings  : UNTRACED REF LPSTR;
                        lpRawData  : LPVOID              ): BOOL;

<*EXTERNAL ReportEventW:WINAPI*>
PROCEDURE ReportEventW (hEventLog  : HANDLE;
                        wType      : WORD;
                        wCategory  : WORD;
                        dwEventID  : DWORD;
                        lpUserSid  : PSID;
                        wNumStrings: WORD;
                        dwDataSize : DWORD;
                        lpStrings  : UNTRACED REF LPWSTR;
                        lpRawData  : LPVOID               ): BOOL;

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
                             nLength             : DWORD;
                             lpnLengthNeeded     : LPDWORD): BOOL;

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
                       DesiredAccess      : DWORD;
                       GenericMapping     : WinNT.PGENERIC_MAPPING;
                       PrivilegeSet       : WinNT.PPRIVILEGE_SET;
                       PrivilegeSetLength : LPDWORD;
                       GrantedAccess      : LPDWORD;
                       AccessStatus       : LPBOOL                    ): BOOL;


<*EXTERNAL OpenProcessToken:WINAPI*>
PROCEDURE OpenProcessToken (ProcessHandle: HANDLE;
                            DesiredAccess: DWORD;
                            TokenHandle  : PHANDLE ): BOOL;


<*EXTERNAL OpenThreadToken:WINAPI*>
PROCEDURE OpenThreadToken (ThreadHandle : HANDLE;
                           DesiredAccess: DWORD;
                           OpenAsSelf   : BOOL;
                           TokenHandle  : PHANDLE ): BOOL;


<*EXTERNAL GetTokenInformation:WINAPI*>
PROCEDURE GetTokenInformation (TokenHandle: HANDLE;
                          TokenInformationClass: WinNT.TOKEN_INFORMATION_CLASS;
                               TokenInformation      : LPVOID;
                               TokenInformationLength: DWORD;
                               ReturnLength          : PDWORD  ): BOOL;


<*EXTERNAL SetTokenInformation:WINAPI*>
PROCEDURE SetTokenInformation (TokenHandle: HANDLE;
                          TokenInformationClass: WinNT.TOKEN_INFORMATION_CLASS;
                               TokenInformation      : LPVOID;
                               TokenInformationLength: DWORD   ): BOOL;


<*EXTERNAL AdjustTokenPrivileges:WINAPI*>
PROCEDURE AdjustTokenPrivileges (TokenHandle         : HANDLE;
                                 DisableAllPrivileges: BOOL;
                                 NewState     : WinNT.PTOKEN_PRIVILEGES;
                                 BufferLength : DWORD;
                                 PreviousState: WinNT.PTOKEN_PRIVILEGES;
                                 ReturnLength : PDWORD                 ): BOOL;


<*EXTERNAL AdjustTokenGroups:WINAPI*>
PROCEDURE AdjustTokenGroups (TokenHandle   : HANDLE;
                             ResetToDefault: BOOL;
                             NewState      : WinNT.PTOKEN_GROUPS;
                             BufferLength  : DWORD;
                             PreviousState : WinNT.PTOKEN_GROUPS;
                             ReturnLength  : PDWORD               ): BOOL;


<*EXTERNAL PrivilegeCheck:WINAPI*>
PROCEDURE PrivilegeCheck (ClientToken       : HANDLE;
                          RequiredPrivileges: WinNT.PPRIVILEGE_SET;
                          pfResult          : LPBOOL                ): BOOL;


<*EXTERNAL AccessCheckAndAuditAlarmA:WINAPI*>
PROCEDURE AccessCheckAndAuditAlarmA (SubsystemName    : LPSTR;
                                     HandleId         : LPVOID;
                                     ObjectTypeName   : LPSTR;
                                     ObjectName       : LPSTR;
                                SecurityDescriptor: WinNT.PSECURITY_DESCRIPTOR;
                                     DesiredAccess    : DWORD;
                                     GenericMapping   : WinNT.PGENERIC_MAPPING;
                                     ObjectCreation   : BOOL;
                                     GrantedAccess    : LPDWORD;
                                     AccessStatus     : LPBOOL;
                                     pfGenerateOnClose: LPBOOL   ): BOOL;

<*EXTERNAL AccessCheckAndAuditAlarmW:WINAPI*>
PROCEDURE AccessCheckAndAuditAlarmW (SubsystemName    : LPWSTR;
                                     HandleId         : LPVOID;
                                     ObjectTypeName   : LPWSTR;
                                     ObjectName       : LPWSTR;
                                SecurityDescriptor: WinNT.PSECURITY_DESCRIPTOR;
                                     DesiredAccess    : DWORD;
                                    GenericMapping    : WinNT.PGENERIC_MAPPING;
                                     ObjectCreation   : BOOL;
                                     GrantedAccess    : LPDWORD;
                                     AccessStatus     : LPBOOL;
                                     pfGenerateOnClose: LPBOOL   ): BOOL;

CONST AccessCheckAndAuditAlarm = AccessCheckAndAuditAlarmA;


<*EXTERNAL ObjectOpenAuditAlarmA:WINAPI*>
PROCEDURE ObjectOpenAuditAlarmA (SubsystemName  : LPSTR;
                                 HandleId       : LPVOID;
                                 ObjectTypeName : LPSTR;
                                 ObjectName     : LPSTR;
                             pSecurityDescriptor: WinNT.PSECURITY_DESCRIPTOR;
                                 ClientToken    : HANDLE;
                                 DesiredAccess  : DWORD;
                                 GrantedAccess  : DWORD;
                                 Privileges     : WinNT.PPRIVILEGE_SET;
                                 ObjectCreation : BOOL;
                                 AccessGranted  : BOOL;
                                 GenerateOnClose: LPBOOL               ): BOOL;

<*EXTERNAL ObjectOpenAuditAlarmW:WINAPI*>
PROCEDURE ObjectOpenAuditAlarmW (SubsystemName : LPWSTR;
                                 HandleId      : LPVOID;
                                 ObjectTypeName: LPWSTR;
                                 ObjectName    : LPWSTR;
                            pSecurityDescriptor: WinNT.PSECURITY_DESCRIPTOR;
                                 ClientToken    : HANDLE;
                                 DesiredAccess  : DWORD;
                                 GrantedAccess  : DWORD;
                                 Privileges     : WinNT.PPRIVILEGE_SET;
                                 ObjectCreation : BOOL;
                                 AccessGranted  : BOOL;
                                 GenerateOnClose: LPBOOL              ): BOOL;

CONST ObjectOpenAuditAlarm = ObjectOpenAuditAlarmA;


<*EXTERNAL ObjectPrivilegeAuditAlarmA:WINAPI*>
PROCEDURE ObjectPrivilegeAuditAlarmA (SubsystemName: LPSTR;
                                      HandleId     : LPVOID;
                                      ClientToken  : HANDLE;
                                      DesiredAccess: DWORD;
                                      Privileges: WinNT.PPRIVILEGE_SET;
                                      AccessGranted: BOOL): BOOL;

<*EXTERNAL ObjectPrivilegeAuditAlarmW:WINAPI*>
PROCEDURE ObjectPrivilegeAuditAlarmW (SubsystemName: LPWSTR;
                                      HandleId     : LPVOID;
                                      ClientToken  : HANDLE;
                                      DesiredAccess: DWORD;
                                      Privileges: WinNT.PPRIVILEGE_SET;
                                      AccessGranted: BOOL): BOOL;

CONST ObjectPrivilegeAuditAlarm = ObjectPrivilegeAuditAlarmA;


<*EXTERNAL ObjectCloseAuditAlarmA:WINAPI*>
PROCEDURE ObjectCloseAuditAlarmA (SubsystemName  : LPSTR;
                                  HandleId       : LPVOID;
                                  GenerateOnClose: BOOL    ): BOOL;

<*EXTERNAL ObjectCloseAuditAlarmW:WINAPI*>
PROCEDURE ObjectCloseAuditAlarmW (SubsystemName  : LPWSTR;
                                  HandleId       : LPVOID;
                                  GenerateOnClose: BOOL    ): BOOL;

CONST ObjectCloseAuditAlarm = ObjectCloseAuditAlarmA;


<*EXTERNAL PrivilegedServiceAuditAlarmA:WINAPI*>
PROCEDURE PrivilegedServiceAuditAlarmA (SubsystemName: LPSTR;
                                        ServiceName  : LPSTR;
                                        ClientToken  : HANDLE;
                                        Privileges: WinNT.PPRIVILEGE_SET;
                                        AccessGranted: BOOL): BOOL;

<*EXTERNAL PrivilegedServiceAuditAlarmW:WINAPI*>
PROCEDURE PrivilegedServiceAuditAlarmW (SubsystemName: LPWSTR;
                                        ServiceName  : LPWSTR;
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
PROCEDURE GetSidLengthRequired (nSubAuthorityCount: UCHAR): DWORD;


<*EXTERNAL AllocateAndInitializeSid:WINAPI*>
PROCEDURE AllocateAndInitializeSid (
                         pIdentifierAuthority: WinNT.PSID_IDENTIFIER_AUTHORITY;
                         nSubAuthorityCount: BYTE;
                         nSubAuthority0    : DWORD;
                         nSubAuthority1    : DWORD;
                         nSubAuthority2    : DWORD;
                         nSubAuthority3    : DWORD;
                         nSubAuthority4    : DWORD;
                         nSubAuthority5    : DWORD;
                         nSubAuthority6    : DWORD;
                         nSubAuthority7    : DWORD;
                         pSid              : UNTRACED REF PSID): BOOL;

<*EXTERNAL FreeSid:WINAPI*>
PROCEDURE FreeSid (pSid: PSID): PVOID;

<*EXTERNAL InitializeSid:WINAPI*>
PROCEDURE InitializeSid (Sid                 : PSID;
                         pIdentifierAuthority: WinNT.PSID_IDENTIFIER_AUTHORITY;
                         nSubAuthorityCount  : BYTE): BOOL;


<*EXTERNAL GetSidIdentifierAuthority:WINAPI*>
PROCEDURE GetSidIdentifierAuthority (
              pSid: PSID): WinNT.PSID_IDENTIFIER_AUTHORITY;


<*EXTERNAL GetSidSubAuthority:WINAPI*>
PROCEDURE GetSidSubAuthority (pSid: PSID; nSubAuthority: DWORD): PDWORD;


<*EXTERNAL GetSidSubAuthorityCount:WINAPI*>
PROCEDURE GetSidSubAuthorityCount (pSid: PSID): PUCHAR;


<*EXTERNAL GetLengthSid:WINAPI*>
PROCEDURE GetLengthSid (pSid: PSID): DWORD;


<*EXTERNAL CopySid:WINAPI*>
PROCEDURE CopySid (nDestinationSidLength: DWORD;
                   pDestinationSid      : PSID;
                   pSourceSid           : PSID   ): BOOL;


<*EXTERNAL AreAllAccessesGranted:WINAPI*>
PROCEDURE AreAllAccessesGranted (GrantedAccess: DWORD;
                                 DesiredAccess: DWORD  ): BOOL;


<*EXTERNAL AreAnyAccessesGranted:WINAPI*>
PROCEDURE AreAnyAccessesGranted (GrantedAccess: DWORD;
                                 DesiredAccess: DWORD  ): BOOL;


<*EXTERNAL MapGenericMask:WINAPI*>
PROCEDURE MapGenericMask (AccessMask    : PDWORD;
                          GenericMapping: WinNT.PGENERIC_MAPPING);


<*EXTERNAL IsValidAcl:WINAPI*>
PROCEDURE IsValidAcl (pAcl: PACL): BOOL;


<*EXTERNAL InitializeAcl:WINAPI*>
PROCEDURE InitializeAcl (pAcl         : PACL;
                         nAclLength   : DWORD;
                         dwAclRevision: DWORD  ): BOOL;


<*EXTERNAL GetAclInformation:WINAPI*>
PROCEDURE GetAclInformation (pAcl                 : PACL;
                             pAclInformation      : LPVOID;
                             nAclInformationLength: DWORD;
                     dwAclInformationClass: WinNT.ACL_INFORMATION_CLASS): BOOL;


<*EXTERNAL SetAclInformation:WINAPI*>
PROCEDURE SetAclInformation (pAcl                 : PACL;
                             pAclInformation      : LPVOID;
                             nAclInformationLength: DWORD;
                     dwAclInformationClass: WinNT.ACL_INFORMATION_CLASS): BOOL;


<*EXTERNAL AddAce:WINAPI*>
PROCEDURE AddAce (pAcl              : PACL;
                  dwAceRevision     : DWORD;
                  dwStartingAceIndex: DWORD;
                  pAceList          : LPVOID;
                  nAceListLength    : DWORD   ): BOOL;


<*EXTERNAL DeleteAce:WINAPI*>
PROCEDURE DeleteAce (pAcl: PACL; dwAceIndex: DWORD): BOOL;


<*EXTERNAL GetAce:WINAPI*>
PROCEDURE GetAce (pAcl      : PACL;
                  dwAceIndex: DWORD;
                  pAce      : UNTRACED REF LPVOID): BOOL;


<*EXTERNAL AddAccessAllowedAce:WINAPI*>
PROCEDURE AddAccessAllowedAce (pAcl         : PACL;
                               dwAceRevision: DWORD;
                               AccessMask   : DWORD;
                               pSid         : PSID   ): BOOL;


<*EXTERNAL AddAccessDeniedAce:WINAPI*>
PROCEDURE AddAccessDeniedAce (pAcl         : PACL;
                              dwAceRevision: DWORD;
                              AccessMask   : DWORD;
                              pSid         : PSID   ): BOOL;


<*EXTERNAL AddAuditAccessAce:WINAPI*>
PROCEDURE AddAuditAccessAce (pAcl         : PACL;
                             dwAceRevision: DWORD;
                             dwAccessMask : DWORD;
                             pSid         : PSID;
                             bAuditSuccess: BOOL;
                             bAuditFailure: BOOL   ): BOOL;


<*EXTERNAL FindFirstFreeAce:WINAPI*>
PROCEDURE FindFirstFreeAce (pAcl: PACL; pAce: UNTRACED REF LPVOID): BOOL;


<*EXTERNAL InitializeSecurityDescriptor:WINAPI*>
PROCEDURE InitializeSecurityDescriptor (
            pSecurityDescriptor: WinNT.PSECURITY_DESCRIPTOR;
            dwRevision         : DWORD                      ): BOOL;


<*EXTERNAL IsValidSecurityDescriptor:WINAPI*>
PROCEDURE IsValidSecurityDescriptor (
            pSecurityDescriptor: WinNT.PSECURITY_DESCRIPTOR): BOOL;


<*EXTERNAL GetSecurityDescriptorLength:WINAPI*>
PROCEDURE GetSecurityDescriptorLength (
            pSecurityDescriptor: WinNT.PSECURITY_DESCRIPTOR): DWORD;


<*EXTERNAL GetSecurityDescriptorControl:WINAPI*>
PROCEDURE GetSecurityDescriptorControl (
            pSecurityDescriptor: WinNT.PSECURITY_DESCRIPTOR;
            pControl           : WinNT.PSECURITY_DESCRIPTOR_CONTROL;
            lpdwRevision       : LPDWORD                             ): BOOL;


<*EXTERNAL SetSecurityDescriptorDacl:WINAPI*>
PROCEDURE SetSecurityDescriptorDacl (
              pSecurityDescriptor: WinNT.PSECURITY_DESCRIPTOR;
              bDaclPresent       : BOOL;
              pDacl              : PACL;
              bDaclDefaulted     : BOOL  ): BOOL;


<*EXTERNAL GetSecurityDescriptorDacl:WINAPI*>
PROCEDURE GetSecurityDescriptorDacl (
              pSecurityDescriptor: WinNT.PSECURITY_DESCRIPTOR;
              lpbDaclPresent     : LPBOOL;
              pDacl              : UNTRACED REF PACL;
              lpbDaclDefaulted   : LPBOOL): BOOL;


<*EXTERNAL SetSecurityDescriptorSacl:WINAPI*>
PROCEDURE SetSecurityDescriptorSacl (
              pSecurityDescriptor: WinNT.PSECURITY_DESCRIPTOR;
              bSaclPresent       : BOOL;
              pSacl              : PACL;
              bSaclDefaulted     : BOOL  ): BOOL;


<*EXTERNAL GetSecurityDescriptorSacl:WINAPI*>
PROCEDURE GetSecurityDescriptorSacl (
              pSecurityDescriptor: WinNT.PSECURITY_DESCRIPTOR;
              lpbSaclPresent     : LPBOOL;
              pSacl              : UNTRACED REF PACL;
              lpbSaclDefaulted   : LPBOOL): BOOL;


<*EXTERNAL SetSecurityDescriptorOwner:WINAPI*>
PROCEDURE SetSecurityDescriptorOwner (
              pSecurityDescriptor: WinNT.PSECURITY_DESCRIPTOR;
              pOwner             : PSID;
              bOwnerDefaulted    : BOOL  ): BOOL;


<*EXTERNAL GetSecurityDescriptorOwner:WINAPI*>
PROCEDURE GetSecurityDescriptorOwner (
              pSecurityDescriptor : WinNT.PSECURITY_DESCRIPTOR;
              pOwner              : UNTRACED REF PSID;
              lpbOwnerDefaulted   : LPBOOL): BOOL;


<*EXTERNAL SetSecurityDescriptorGroup:WINAPI*>
PROCEDURE SetSecurityDescriptorGroup (
              pSecurityDescriptor: WinNT.PSECURITY_DESCRIPTOR;
              pGroup             : PSID;
              bGroupDefaulted    : BOOL  ): BOOL;


<*EXTERNAL GetSecurityDescriptorGroup:WINAPI*>
PROCEDURE GetSecurityDescriptorGroup (
              pSecurityDescriptor: WinNT.PSECURITY_DESCRIPTOR;
              pGroup             : UNTRACED REF PSID;
              lpbGroupDefaulted  : LPBOOL): BOOL;


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
              DescriptorLength   : DWORD;
              ReturnLength       : PDWORD ): BOOL;


<*EXTERNAL DestroyPrivateObjectSecurity:WINAPI*>
PROCEDURE DestroyPrivateObjectSecurity (
              ObjectDescriptor: UNTRACED REF WinNT.PSECURITY_DESCRIPTOR): BOOL;


<*EXTERNAL MakeSelfRelativeSD:WINAPI*>
PROCEDURE MakeSelfRelativeSD (
              pAbsoluteSecurityDescriptor    : WinNT.PSECURITY_DESCRIPTOR;
              pSelfRelativeSecurityDescriptor: WinNT.PSECURITY_DESCRIPTOR;
              lpdwBufferLength               : LPDWORD): BOOL;


<*EXTERNAL MakeAbsoluteSD:WINAPI*>
PROCEDURE MakeAbsoluteSD (
              pSelfRelativeSecurityDescriptor   : WinNT.PSECURITY_DESCRIPTOR;
              pAbsoluteSecurityDescriptor       : WinNT.PSECURITY_DESCRIPTOR;
              lpdwAbsoluteSecurityDescriptorSize: LPDWORD;
              pDacl                             : PACL;
              lpdwDaclSize                      : LPDWORD;
              pSacl                             : PACL;
              lpdwSaclSize                      : LPDWORD;
              pOwner                            : PSID;
              lpdwOwnerSize                     : LPDWORD;
              pPrimaryGroup                     : PSID;
              lpdwPrimaryGroupSize              : LPDWORD  ): BOOL;


<*EXTERNAL SetFileSecurityA:WINAPI*>
PROCEDURE SetFileSecurityA (
              lpFileName         : LPSTR;
              SecurityInformation: WinNT.SECURITY_INFORMATION;
              pSecurityDescriptor: WinNT.PSECURITY_DESCRIPTOR): BOOL;

<*EXTERNAL SetFileSecurityW:WINAPI*>
PROCEDURE SetFileSecurityW (
              lpFileName         : LPWSTR;
              SecurityInformation: WinNT.SECURITY_INFORMATION;
              pSecurityDescriptor: WinNT.PSECURITY_DESCRIPTOR): BOOL;

CONST SetFileSecurity = SetFileSecurityA;


<*EXTERNAL GetFileSecurityA:WINAPI*>
PROCEDURE GetFileSecurityA (lpFileName: LPSTR;
                            RequestedInformation: WinNT.SECURITY_INFORMATION;
                            pSecurityDescriptor: WinNT.PSECURITY_DESCRIPTOR;
                            nLength        : DWORD;
                            lpnLengthNeeded: LPDWORD): BOOL;

<*EXTERNAL GetFileSecurityW:WINAPI*>
PROCEDURE GetFileSecurityW (lpFileName: LPWSTR;
                            RequestedInformation: WinNT.SECURITY_INFORMATION;
                            pSecurityDescriptor: WinNT.PSECURITY_DESCRIPTOR;
                            nLength        : DWORD;
                            lpnLengthNeeded: LPDWORD): BOOL;

CONST GetFileSecurity = GetFileSecurityA;


<*EXTERNAL SetKernelObjectSecurity:WINAPI*>
PROCEDURE SetKernelObjectSecurity (
              Handle             : HANDLE;
              SecurityInformation: WinNT.SECURITY_INFORMATION;
              SecurityDescriptor : WinNT.PSECURITY_DESCRIPTOR): BOOL;



<*EXTERNAL FindFirstChangeNotificationA:WINAPI*>
PROCEDURE FindFirstChangeNotificationA (lpPathName    : LPSTR;
                                        bWatchSubtree : BOOL;
                                        dwNotifyFilter: DWORD  ): HANDLE;

<*EXTERNAL FindFirstChangeNotificationW:WINAPI*>
PROCEDURE FindFirstChangeNotificationW (lpPathName    : LPWSTR;
                                        bWatchSubtree : BOOL;
                                        dwNotifyFilter: DWORD  ): HANDLE;

CONST FindFirstChangeNotification = FindFirstChangeNotificationA;

<*EXTERNAL FindNextChangeNotification:WINAPI*>
PROCEDURE FindNextChangeNotification (hChangeHandle: HANDLE): BOOL;

<*EXTERNAL FindCloseChangeNotification:WINAPI*>
PROCEDURE FindCloseChangeNotification (hChangeHandle: HANDLE): BOOL;

<*EXTERNAL VirtualLock:WINAPI*>
PROCEDURE VirtualLock (lpAddress: LPVOID; dwSize: DWORD): BOOL;

<*EXTERNAL VirtualUnlock:WINAPI*>
PROCEDURE VirtualUnlock (lpAddress: LPVOID; dwSize: DWORD): BOOL;

<*EXTERNAL MapViewOfFileEx:WINAPI*>
PROCEDURE MapViewOfFileEx (hFileMappingObject  : HANDLE;
                           dwDesiredAccess     : DWORD;
                           dwFileOffsetHigh    : DWORD;
                           dwFileOffsetLow     : DWORD;
                           dwNumberOfBytesToMap: DWORD;
                           lpBaseAddress       : LPVOID  ): LPVOID;

<*EXTERNAL SetPriorityClass:WINAPI*>
PROCEDURE SetPriorityClass (hProcess: HANDLE; dwPriorityClass: DWORD): BOOL;

<*EXTERNAL GetPriorityClass:WINAPI*>
PROCEDURE GetPriorityClass (hProcess: HANDLE): DWORD;

<*EXTERNAL IsBadReadPtr:WINAPI*>
PROCEDURE IsBadReadPtr (lp: void_star; ucb: UINT): BOOL;

<*EXTERNAL IsBadWritePtr:WINAPI*>
PROCEDURE IsBadWritePtr (lp: LPVOID; ucb: UINT): BOOL;

<*EXTERNAL IsBadHugeReadPtr:WINAPI*>
PROCEDURE IsBadHugeReadPtr (lp: void_star; ucb: UINT): BOOL;

<*EXTERNAL IsBadHugeWritePtr:WINAPI*>
PROCEDURE IsBadHugeWritePtr (lp: LPVOID; ucb: UINT): BOOL;

<*EXTERNAL IsBadCodePtr:WINAPI*>
PROCEDURE IsBadCodePtr (lpfn: FARPROC): BOOL;

<*EXTERNAL IsBadStringPtrA:WINAPI*>
PROCEDURE IsBadStringPtrA (lpsz: LPCSTR; ucchMax: UINT): BOOL;

<*EXTERNAL IsBadStringPtrW:WINAPI*>
PROCEDURE IsBadStringPtrW (lpsz: LPCWSTR; ucchMax: UINT): BOOL;

CONST IsBadStringPtr = IsBadStringPtrA;

<*EXTERNAL LookupAccountSidA:WINAPI*>
PROCEDURE LookupAccountSidA (lpSystemName          : LPSTR;
                             Sid                   : PSID;
                             Name                  : LPSTR;
                             cbName                : LPDWORD;
                             ReferencedDomainName  : LPSTR;
                             cbReferencedDomainName: LPDWORD;
                             peUse                 : WinNT.PSID_NAME_USE):BOOL;

<*EXTERNAL LookupAccountSidW:WINAPI*>
PROCEDURE LookupAccountSidW (lpSystemName          : LPWSTR;
                             Sid                   : PSID;
                             Name                  : LPWSTR;
                             cbName                : LPDWORD;
                             ReferencedDomainName  : LPWSTR;
                             cbReferencedDomainName: LPDWORD;
                             peUse                 : WinNT.PSID_NAME_USE):BOOL;

CONST LookupAccountSid = LookupAccountSidA;

<*EXTERNAL LookupAccountNameA:WINAPI*>
PROCEDURE LookupAccountNameA (lpSystemName          : LPSTR;
                              lpAccountName         : LPSTR;
                              Sid                   : PSID;
                              cbSid                 : LPDWORD;
                              ReferencedDomainName  : LPSTR;
                              cbReferencedDomainName: LPDWORD;
                              peUse                : WinNT.PSID_NAME_USE):BOOL;

<*EXTERNAL LookupAccountNameW:WINAPI*>
PROCEDURE LookupAccountNameW (lpSystemName          : LPWSTR;
                              lpAccountName         : LPWSTR;
                              Sid                   : PSID;
                              cbSid                 : LPDWORD;
                              ReferencedDomainName  : LPWSTR;
                              cbReferencedDomainName: LPDWORD;
                              peUse                : WinNT.PSID_NAME_USE):BOOL;

CONST LookupAccountName = LookupAccountNameA;

<*EXTERNAL LookupPrivilegeValueA:WINAPI*>
PROCEDURE LookupPrivilegeValueA (lpSystemName: LPSTR;
                                 lpName      : LPSTR;
                                 lpLuid      : PLUID  ): BOOL;

<*EXTERNAL LookupPrivilegeValueW:WINAPI*>
PROCEDURE LookupPrivilegeValueW (lpSystemName: LPWSTR;
                                 lpName      : LPWSTR;
                                 lpLuid      : PLUID   ): BOOL;

CONST LookupPrivilegeValue = LookupPrivilegeValueA;

<*EXTERNAL LookupPrivilegeNameA:WINAPI*>
PROCEDURE LookupPrivilegeNameA (lpSystemName: LPSTR;
                                lpLuid      : PLUID;
                                lpName      : LPSTR;
                                cbName      : LPDWORD): BOOL;

<*EXTERNAL LookupPrivilegeNameW:WINAPI*>
PROCEDURE LookupPrivilegeNameW (lpSystemName: LPWSTR;
                                lpLuid      : PLUID;
                                lpName      : LPWSTR;
                                cbName      : LPDWORD ): BOOL;

CONST LookupPrivilegeName = LookupPrivilegeNameA;

<*EXTERNAL LookupPrivilegeDisplayNameA:WINAPI*>
PROCEDURE LookupPrivilegeDisplayNameA (lpSystemName : LPSTR;
                                       lpName       : LPSTR;
                                       lpDisplayName: LPSTR;
                                       cbDisplayName: LPDWORD;
                                       lpLanguageId : LPDWORD  ): BOOL;

<*EXTERNAL LookupPrivilegeDisplayNameW:WINAPI*>
PROCEDURE LookupPrivilegeDisplayNameW (lpSystemName : LPWSTR;
                                       lpName       : LPWSTR;
                                       lpDisplayName: LPWSTR;
                                       cbDisplayName: LPDWORD;
                                       lpLanguageId : LPDWORD  ): BOOL;

CONST LookupPrivilegeDisplayName = LookupPrivilegeDisplayNameA;

<*EXTERNAL AllocateLocallyUniqueId:WINAPI*>
PROCEDURE AllocateLocallyUniqueId (Luid: PLUID): BOOL;

<*EXTERNAL BuildCommDCBA:WINAPI*>
PROCEDURE BuildCommDCBA (lpDef: LPCSTR; lpDCB: LPDCB): BOOL;

<*EXTERNAL BuildCommDCBW:WINAPI*>
PROCEDURE BuildCommDCBW (lpDef: LPCWSTR; lpDCB: LPDCB): BOOL;

CONST BuildCommDCB = BuildCommDCBA;

<*EXTERNAL BuildCommDCBAndTimeoutsA:WINAPI*>
PROCEDURE BuildCommDCBAndTimeoutsA (lpDef          : LPCSTR;
                                    lpDCB          : LPDCB;
                                    lpCommTimeouts : LPCOMMTIMEOUTS): BOOL;

<*EXTERNAL BuildCommDCBAndTimeoutsW:WINAPI*>
PROCEDURE BuildCommDCBAndTimeoutsW (lpDef          : LPCWSTR;
                                    lpDCB          : LPDCB;
                                    lpCommTimeouts : LPCOMMTIMEOUTS): BOOL;

CONST BuildCommDCBAndTimeouts = BuildCommDCBAndTimeoutsA;

<*EXTERNAL CommConfigDialogA:WINAPI*>
PROCEDURE CommConfigDialogA (lpszName : LPCSTR;
                             hWnd     : HWND;
                             lpCC     : LPCOMMCONFIG ): BOOL;

<*EXTERNAL CommConfigDialogW:WINAPI*>
PROCEDURE CommConfigDialogW (lpszName : LPCWSTR;
                             hWnd     : HWND;
                             lpCC     : LPCOMMCONFIG ): BOOL;

CONST CommConfigDialog = CommConfigDialogA;

<*EXTERNAL GetDefaultCommConfigA:WINAPI*>
PROCEDURE GetDefaultCommConfigA (lpszName : LPCSTR;
                                 lpCC     : LPCOMMCONFIG;
                                 lpdwSize : LPDWORD): BOOL;

<*EXTERNAL GetDefaultCommConfigW:WINAPI*>
PROCEDURE GetDefaultCommConfigW (lpszName : LPCWSTR;
                                 lpCC     : LPCOMMCONFIG;
                                 lpdwSize : LPDWORD): BOOL;

CONST GetDefaultCommConfig = GetDefaultCommConfigA;

<*EXTERNAL SetDefaultCommConfigA:WINAPI*>
PROCEDURE SetDefaultCommConfigA (lpszName : LPCSTR;
                                 lpCC     : LPCOMMCONFIG;
                                 dwSize   : DWORD): BOOL;

<*EXTERNAL SetDefaultCommConfigW:WINAPI*>
PROCEDURE SetDefaultCommConfigW (lpszName : LPCWSTR;
                                 lpCC     : LPCOMMCONFIG;
                                 dwSize   : DWORD): BOOL;

CONST SetDefaultCommConfig = SetDefaultCommConfigA;

CONST MAX_COMPUTERNAME_LENGTH = 15;

<*EXTERNAL GetComputerNameA:WINAPI*>
PROCEDURE GetComputerNameA (lpBuffer: LPSTR; nSize: LPDWORD): BOOL;

<*EXTERNAL GetComputerNameW:WINAPI*>
PROCEDURE GetComputerNameW (lpBuffer: LPWSTR; nSize: LPDWORD): BOOL;

CONST GetComputerName = GetComputerNameA;

<*EXTERNAL SetComputerNameA:WINAPI*>
PROCEDURE SetComputerNameA (lpComputerName: LPSTR): BOOL;

<*EXTERNAL SetComputerNameW:WINAPI*>
PROCEDURE SetComputerNameW (lpComputerName: LPWSTR): BOOL;

CONST SetComputerName = SetComputerNameA;

<*EXTERNAL GetUserNameA:WINAPI*>
PROCEDURE GetUserNameA (lpBuffer: LPSTR; nSize: LPDWORD): BOOL;

<*EXTERNAL GetUserNameW:WINAPI*>
PROCEDURE GetUserNameW (lpBuffer: LPWSTR; nSize: LPDWORD): BOOL;

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
  LPOSVERSIONINFOA = UNTRACED REF OSVERSIONINFOA;
  OSVERSIONINFOA = RECORD
    dwOSVersionInfoSize : DWORD;
    dwMajorVersion      : DWORD;
    dwMinorVersion      : DWORD;
    dwBuildNumber       : DWORD;
    dwPlatformId        : DWORD;
    szCSDVersion        : ARRAY [0..127] OF CHAR; (* Maintenance string
                                                     for PSS usage *)
  END;

  POSVERSIONINFOW = UNTRACED REF OSVERSIONINFOW;
  LPOSVERSIONINFOW = UNTRACED REF OSVERSIONINFOW;
  OSVERSIONINFOW = RECORD
    dwOSVersionInfoSize : DWORD;
    dwMajorVersion      : DWORD;
    dwMinorVersion      : DWORD;
    dwBuildNumber       : DWORD;
    dwPlatformId        : DWORD;
    szCSDVersion        : ARRAY [0..127] OF WCHAR; (* Maintenance string
                                                     for PSS usage *)
  END;

  OSVERSIONINFO   = OSVERSIONINFOA;
  POSVERSIONINFO  = POSVERSIONINFOA;
  LPOSVERSIONINFO = LPOSVERSIONINFOA;

CONST
  VER_PLATFORM_WIN32s        = 0;
  VER_PLATFORM_WIN32_WINDOWS = 1;
  VER_PLATFORM_WIN32_NT      = 2;

<*EXTERNAL GetVersionExA:WINAPI*>
PROCEDURE GetVersionExA (lpVersionInformation: LPOSVERSIONINFOA): BOOL;

<*EXTERNAL GetVersionExW:WINAPI*>
PROCEDURE GetVersionExW (lpVersionInformation: LPOSVERSIONINFOW): BOOL;

CONST GetVersionEx = GetVersionExA;

(* DOS and OS/2 Compatible Error Code definitions returned by the Win32 Base
   API functions. *)

(* Abnormal termination codes *)

CONST
  TC_NORMAL  = 0;
  TC_HARDERR = 1;
  TC_GP_TRAP = 2;
  TC_SIGNAL  = 3;

(* Power Management APIs *)

CONST
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
  LPSYSTEM_POWER_STATUS = UNTRACED REF SYSTEM_POWER_STATUS;
  SYSTEM_POWER_STATUS = RECORD
    ACLineStatus        : BYTE;
    BatteryFlag         : BYTE;
    BatteryLifePercent  : BYTE;
    Reserved1           : BYTE;
    BatteryLifeTime     : DWORD;
    BatteryFullLifeTime : DWORD;
  END;

<*EXTERNAL GetSystemPowerStatus:WINAPI*>
PROCEDURE GetSystemPowerStatus (lpSystemPowerStatus: LPSYSTEM_POWER_STATUS): BOOL;

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
