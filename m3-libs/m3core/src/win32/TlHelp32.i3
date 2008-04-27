(* Copyright (C) 1996, Critcal Mass, Inc.  All rights reserved.   *)
(*                                                                *)
(* derived from Microsoft's TLHELP32.H version 1.0 by Bill Kalsow *)

(******************************************************************
*                                                                 *
* tlhelp32.h - WIN32 tool help functions, types, and definitions  *
*                                                                 *
*******************************************************************)

INTERFACE TlHelp32;

FROM Ctypes IMPORT char;
FROM WinDef IMPORT HANDLE, UINT32, BOOL, HMODULE, INT32, PVOID,
 PCVOID, PUINT32, PUINT8, MAX_PATH;

CONST
  MAX_MODULE_NAME32 = 255;

(****** Shapshot function **********************************************)

<*EXTERNAL CreateToolhelp32Snapshot:WINAPI*>
PROCEDURE CreateToolhelp32Snapshot (dwFlags, th32ProcessID: UINT32): HANDLE;
(*
// The th32ProcessID argument is only used if TH32CS_SNAPHEAPLIST or
// TH32CS_SNAPMODULE is specified. th32ProcessID == 0 means the current
// process.
//
// NOTE that all of the snapshots are global except for the heap and module
//      lists which are process specific. To enumerate the heap or module
//      state for all WIN32 processes call with TH32CS_SNAPALL and the
//      current process. Then for each process in the TH32CS_SNAPPROCESS
//      list that isn't the current process, do a call with just
//      TH32CS_SNAPHEAPLIST and/or TH32CS_SNAPMODULE.
*)

CONST (* dwFlags for create snapshot *)
  TH32CS_SNAPHEAPLIST = 16_00000001;
  TH32CS_SNAPPROCESS  = 16_00000002;
  TH32CS_SNAPTHREAD   = 16_00000004;
  TH32CS_SNAPMODULE   = 16_00000008;
  TH32CS_SNAPALL      = TH32CS_SNAPHEAPLIST + TH32CS_SNAPPROCESS
                        + TH32CS_SNAPTHREAD + TH32CS_SNAPMODULE;
  TH32CS_INHERIT      = 16_80000000;

(*
// Use CloseHandle to destroy the snapshot
*)

(****** heap walking ***************************************************)

TYPE
  PHEAPLIST32 = UNTRACED REF HEAPLIST32;
  LPHEAPLIST32 = UNTRACED REF HEAPLIST32;
  HEAPLIST32 = RECORD
    dwSize        : UINT32;
    th32ProcessID : UINT32;  (* owning process *)
    th32HeapID    : UINT32;  (* heap (in owning process's context!) *)
    dwFlags       : UINT32;
  END;

CONST (* dwFlags for HEAPLIST32 *)
  HF32_DEFAULT = 1;  (* process's default heap *)
  HF32_SHARED  = 2;  (* is shared heap *)

<*EXTERNAL Heap32ListFirst:WINAPI*>
PROCEDURE Heap32ListFirst (hSnapshot: HANDLE;  lphl: LPHEAPLIST32): BOOL;

<*EXTERNAL Heap32ListNext:WINAPI*>
PROCEDURE Heap32ListNext (hSnapshot: HANDLE;  lphl: LPHEAPLIST32): BOOL;

TYPE
  PHEAPENTRY32 = UNTRACED REF HEAPENTRY32;
  LPHEAPENTRY32 = UNTRACED REF HEAPENTRY32;
  HEAPENTRY32 = RECORD
    dwSize        : UINT32;
    hHandle       : HANDLE; (* Handle of this heap block *)
    dwAddress     : UINT32;  (* Linear address of start of block *)
    dwBlockSize   : UINT32;  (* Size of block in bytes *)
    dwFlags       : UINT32;
    dwLockCount   : UINT32;
    dwResvd       : UINT32;
    th32ProcessID : UINT32;  (* owning process *)
    th32HeapID    : UINT32;  (* heap block is in *)
  END;

CONST (* dwFlags for HEAPENTRY32 *)
  LF32_FIXED    = 16_00000001;
  LF32_FREE     = 16_00000002;
  LF32_MOVEABLE = 16_00000004;


<*EXTERNAL Heap32First:WINAPI*>
PROCEDURE Heap32First (lphe          : LPHEAPENTRY32;
                       th32ProcessID : UINT32;
                       th32HeapID    : UINT32): BOOL;

<*EXTERNAL Heap32Next:WINAPI*>
PROCEDURE Heap32Next (lphe: LPHEAPENTRY32): BOOL;

<*EXTERNAL Toolhelp32ReadProcessMemory:WINAPI*>
PROCEDURE Toolhelp32ReadProcessMemory (th32ProcessID       : UINT32;
                                       lpBaseAddress       : PCVOID;
                                       lpBuffer            : PVOID;
                                       cbRead              : UINT32;
                                       lpNumberOfBytesRead : PUINT32): BOOL;

(****** Process walking *************************************************)

TYPE
  PPROCESSENTRY32 = UNTRACED REF PROCESSENTRY32;
  LPPROCESSENTRY32 = UNTRACED REF PROCESSENTRY32;
  PROCESSENTRY32 = RECORD
    dwSize              : UINT32;
    cntUsage            : UINT32;
    th32ProcessID       : UINT32;  (* this process *)
    th32DefaultHeapID   : UINT32;
    th32ModuleID        : UINT32;  (* associated exe *)
    cntThreads          : UINT32;
    th32ParentProcessID : UINT32;  (* this process's parent process *)
    pcPriClassBase      : INT32;   (* Base priority of process's threads *)
    dwFlags             : UINT32;
    szExeFile           : ARRAY [0..MAX_PATH-1] OF char;    (* Path *)
  END;

<*EXTERNAL Process32First:WINAPI*>
PROCEDURE Process32First (hSnapshot: HANDLE;  lppe: LPPROCESSENTRY32): BOOL;

<*EXTERNAL Process32Next:WINAPI*>
PROCEDURE Process32Next (hSnapshot: HANDLE;  lppe: LPPROCESSENTRY32): BOOL;

(***** Thread walking **************************************************)

TYPE
  PTHREADENTRY32 = UNTRACED REF THREADENTRY32;
  LPTHREADENTRY32 = UNTRACED REF THREADENTRY32;
  THREADENTRY32 = RECORD
    dwSize             : UINT32;
    cntUsage           : UINT32;
    th32ThreadID       : UINT32; (* this thread *)
    th32OwnerProcessID : UINT32; (* Process this thread is associated with *)
    tpBasePri          : INT32;
    tpDeltaPri         : INT32;
    dwFlags            : UINT32;
  END;

<*EXTERNAL Thread32First:WINAPI*>
PROCEDURE Thread32First (hSnapshot: HANDLE;  lpte: LPTHREADENTRY32): BOOL;

<*EXTERNAL Thread32Next:WINAPI*>
PROCEDURE Thread32Next (hSnapshot: HANDLE;  lpte: LPTHREADENTRY32): BOOL;

(***** Module walking *************************************************)

TYPE
  PMODULEENTRY32 = UNTRACED REF MODULEENTRY32;
  LPMODULEENTRY32 = UNTRACED REF MODULEENTRY32;
  MODULEENTRY32 = RECORD
    dwSize        : UINT32;
    th32ModuleID  : UINT32;   (* This module *)
    th32ProcessID : UINT32;   (* owning process *)
    GlblcntUsage  : UINT32;   (* Global usage count on the module *)
    ProccntUsage  : UINT32;   (* Module usage count in th32ProcessID's context *)
    modBaseAddr   : PUINT8;  (* Base address of module in th32ProcessID's context *)
    modBaseSize   : UINT32;   (* Size in bytes of module starting at modBaseAddr *)
    hModule       : HMODULE; (* The hModule of this module in th32ProcessID's context *)
    szModule      : ARRAY [0..MAX_MODULE_NAME32] OF char;
    szExePath     : ARRAY [0..MAX_PATH-1] OF char;
  END;

(*
// NOTE CAREFULLY that the modBaseAddr and hModule fields are valid ONLY
// in th32ProcessID's process context.
*)

<*EXTERNAL Module32First:WINAPI*>
PROCEDURE Module32First (hSnapshot: HANDLE;  lpme: LPMODULEENTRY32): BOOL;

<*EXTERNAL Module32Next:WINAPI*>
PROCEDURE Module32Next (hSnapshot: HANDLE;  lpme: LPMODULEENTRY32): BOOL;

END TlHelp32.
