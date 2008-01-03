(* Copyright (C) 1994, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* by Stephen Harrison                                       *)
(*                                                           *)
(* Last modified on Wed Dec 21 09:13:12 PST 1994 by kalsow   *)
(*      modified on Wed Feb 10 20:32:11 PST 1993 by harrison *)

INTERFACE WinReg;

(* Corresponds to build version 0001 of "winreg.h".
   See that file for details.

   This module contains the function prototypes and constant, type and
   structure definitions for the Windows 32-Bit Registry API. *)

IMPORT Ctypes, WinBase, WinNT;

FROM WinNT IMPORT LONG, LPSTR, LPWSTR, HANDLE;
FROM WinDef IMPORT DWORD, LPDWORD, LPBYTE, BOOL;

(* Requested Key access mask type. *)

TYPE REGSAM = WinNT.ACCESS_MASK;

(* Type definitions. *)

TYPE
  HKEY  = UNTRACED BRANDED "HKEY" REF RECORD unused: Ctypes.int;  END;
  PHKEY = UNTRACED REF PHKEY;

(* Reserved Key Handles. *)

VAR(*CONST*)
  HKEY_CLASSES_ROOT    : HKEY; (*:= LOOPHOLE (16_80000000, HKEY) *)
  HKEY_CURRENT_USER    : HKEY; (*:= LOOPHOLE (16_80000001, HKEY) *)
  HKEY_LOCAL_MACHINE   : HKEY; (*:= LOOPHOLE (16_80000002, HKEY) *)
  HKEY_USERS           : HKEY; (*:= LOOPHOLE (16_80000003, HKEY) *)
  HKEY_PERFORMANCE_DATA: HKEY; (*:= LOOPHOLE (16_80000004, HKEY) *)
  HKEY_CURERNT_CONFIG  : HKEY; (*:= LOOPHOLE (16_80000004, HKEY) *)
  HKEY_DYN_DATA        : HKEY; (*:= LOOPHOLE (16_80000004, HKEY) *)

(* API Prototypes. *)

<*EXTERNAL RegCloseKey:APIENTRY*>
PROCEDURE RegCloseKey (hKey: HKEY): LONG;

<*EXTERNAL RegConnectRegistryA:APIENTRY*>
PROCEDURE RegConnectRegistryA (lpMachineName: LPSTR;
                               hKey         : HKEY;
                               phkResult    : PHKEY  ): LONG;

<*EXTERNAL RegConnectRegistryW:APIENTRY*>
PROCEDURE RegConnectRegistryW (lpMachineName: LPWSTR;
                               hKey         : HKEY;
                               phkResult    : PHKEY   ): LONG;

CONST RegConnectRegistry = RegConnectRegistryA;

<*EXTERNAL RegCreateKeyA:APIENTRY*>
PROCEDURE RegCreateKeyA (hKey: HKEY; lpSubKey: LPSTR; phkResult: PHKEY): LONG;

<*EXTERNAL RegCreateKeyW:APIENTRY*>
PROCEDURE RegCreateKeyW (hKey: HKEY; lpSubKey: LPWSTR; phkResult: PHKEY): LONG;

CONST RegCreateKey = RegCreateKeyA;

<*EXTERNAL RegCreateKeyExA:APIENTRY*>
PROCEDURE RegCreateKeyExA (hKey                : HKEY;
                           lpSubKey            : LPSTR;
                           Reserved            : DWORD;
                           lpClass             : LPSTR;
                           dwOptions           : DWORD;
                           samDesired          : REGSAM;
                           lpSecurityAttributes: WinBase.LPSECURITY_ATTRIBUTES;
                           phkResult           : PHKEY;
                           lpdwDisposition     : LPDWORD                ): LONG;

<*EXTERNAL RegCreateKeyExW:APIENTRY*>
PROCEDURE RegCreateKeyExW (hKey                : HKEY;
                           lpSubKey            : LPWSTR;
                           Reserved            : DWORD;
                           lpClass             : LPWSTR;
                           dwOptions           : DWORD;
                           samDesired          : REGSAM;
                           lpSecurityAttributes: WinBase.LPSECURITY_ATTRIBUTES;
                           phkResult           : PHKEY;
                           lpdwDisposition     : LPDWORD                ): LONG;

CONST RegCreateKeyEx = RegCreateKeyExA;

<*EXTERNAL RegDeleteKeyA:APIENTRY*>
PROCEDURE RegDeleteKeyA (hKey: HKEY; lpSubKey: LPSTR): LONG;

<*EXTERNAL RegDeleteKeyW:APIENTRY*>
PROCEDURE RegDeleteKeyW (hKey: HKEY; lpSubKey: LPWSTR): LONG;

CONST RegDeleteKey = RegDeleteKeyA;

<*EXTERNAL RegDeleteValueA:APIENTRY*>
PROCEDURE RegDeleteValueA (hKey: HKEY; lpValueName: LPSTR): LONG;

<*EXTERNAL RegDeleteValueW:APIENTRY*>
PROCEDURE RegDeleteValueW (hKey: HKEY; lpValueName: LPWSTR): LONG;

CONST RegDeleteValue = RegDeleteValueA;

<*EXTERNAL RegEnumKeyA:APIENTRY*>
PROCEDURE RegEnumKeyA (hKey   : HKEY;
                                                     dwIndex: DWORD;
                                                     lpName : LPSTR;
                                                     cbName : DWORD  ): LONG;

<*EXTERNAL RegEnumKeyW:APIENTRY*>
PROCEDURE RegEnumKeyW (hKey   : HKEY;
                                                     dwIndex: DWORD;
                                                     lpName : LPWSTR;
                                                     cbName : DWORD   ): LONG;

CONST RegEnumKey = RegEnumKeyA;

<*EXTERNAL RegEnumKeyExA:APIENTRY*>
PROCEDURE RegEnumKeyExA (hKey             : HKEY;
                         dwIndex          : DWORD;
                         lpName           : LPSTR;
                         lpcbName         : LPDWORD;
                         lpReserved       : LPDWORD;
                         lpClass          : LPSTR;
                         lpcbClass        : LPDWORD;
                         lpftLastWriteTime: WinBase.PFILETIME): LONG;

<*EXTERNAL RegEnumKeyExW:APIENTRY*>
PROCEDURE RegEnumKeyExW (hKey             : HKEY;
                         dwIndex          : DWORD;
                         lpName           : LPWSTR;
                         lpcbName         : LPDWORD;
                         lpReserved       : LPDWORD;
                         lpClass          : LPWSTR;
                         lpcbClass        : LPDWORD;
                         lpftLastWriteTime: WinBase.PFILETIME): LONG;

CONST RegEnumKeyEx = RegEnumKeyExA;

<*EXTERNAL RegEnumValueA:APIENTRY*>
PROCEDURE RegEnumValueA (hKey         : HKEY;
                         dwIndex      : DWORD;
                         lpValueName  : LPSTR;
                         lpcbValueName: LPDWORD;
                         lpReserved   : LPDWORD;
                         lpType       : LPDWORD;
                         lpData       : LPBYTE;
                         lpcbData     : LPDWORD  ): LONG;

<*EXTERNAL RegEnumValueW:APIENTRY*>
PROCEDURE RegEnumValueW (hKey         : HKEY;
                         dwIndex      : DWORD;
                         lpValueName  : LPWSTR;
                         lpcbValueName: LPDWORD;
                         lpReserved   : LPDWORD;
                         lpType       : LPDWORD;
                         lpData       : LPBYTE;
                         lpcbData     : LPDWORD  ): LONG;

CONST RegEnumValue = RegEnumValueA;

<*EXTERNAL RegFlushKey:APIENTRY*>
PROCEDURE RegFlushKey (hKey: HKEY): LONG;

<*EXTERNAL RegGetKeySecurity:APIENTRY*>
PROCEDURE RegGetKeySecurity (hKey               : HKEY;
                             SecurityInformation: WinNT.SECURITY_INFORMATION;
                             pSecurityDescriptor: WinNT.PSECURITY_DESCRIPTOR;
                             lpcbSecurityDescriptor: LPDWORD): LONG;

<*EXTERNAL RegLoadKeyA:APIENTRY*>
PROCEDURE RegLoadKeyA (hKey: HKEY; lpSubKey: LPSTR; lpFile: LPSTR): LONG;

<*EXTERNAL RegLoadKeyW:APIENTRY*>
PROCEDURE RegLoadKeyW (hKey: HKEY; lpSubKey: LPWSTR; lpFile: LPWSTR): LONG;

CONST RegLoadKey = RegLoadKeyA;

<*EXTERNAL RegNotifyChangeKeyValue:APIENTRY*>
PROCEDURE RegNotifyChangeKeyValue (hKey          : HKEY;
                                   bWatchSubtree : BOOL;
                                   dwNotifyFilter: DWORD;
                                   hEvent        : HANDLE;
                                   fAsynchronus  : BOOL    ): LONG;

<*EXTERNAL RegOpenKeyA:APIENTRY*>
PROCEDURE RegOpenKeyA (hKey: HKEY; lpSubKey: LPSTR; phkResult: PHKEY): LONG;

<*EXTERNAL RegOpenKeyW:APIENTRY*>
PROCEDURE RegOpenKeyW (hKey: HKEY; lpSubKey: LPWSTR; phkResult: PHKEY): LONG;

CONST RegOpenKey = RegOpenKeyA;

<*EXTERNAL RegOpenKeyExA:APIENTRY*>
PROCEDURE RegOpenKeyExA (hKey      : HKEY;
                         lpSubKey  : LPSTR;
                         ulOptions : DWORD;
                         samDesired: REGSAM;
                         phkResult : PHKEY   ): LONG;

<*EXTERNAL RegOpenKeyExW:APIENTRY*>
PROCEDURE RegOpenKeyExW (hKey      : HKEY;
                         lpSubKey  : LPWSTR;
                         ulOptions : DWORD;
                         samDesired: REGSAM;
                         phkResult : PHKEY   ): LONG;

CONST RegOpenKeyEx = RegOpenKeyExA;

<*EXTERNAL RegQueryInfoKeyA:APIENTRY*>
PROCEDURE RegQueryInfoKeyA (hKey                  : HKEY;
                            lpClass               : LPSTR;
                            lpcbClass             : LPDWORD;
                            lpReserved            : LPDWORD;
                            lpcSubKeys            : LPDWORD;
                            lpcbMaxSubKeyLen      : LPDWORD;
                            lpcbMaxClassLen       : LPDWORD;
                            lpcValues             : LPDWORD;
                            lpcbMaxValueNameLen   : LPDWORD;
                            lpcbMaxValueLen       : LPDWORD;
                            lpcbSecurityDescriptor: LPDWORD;
                            lpftLastWriteTime     : WinBase.PFILETIME): LONG;

<*EXTERNAL RegQueryInfoKeyW:APIENTRY*>
PROCEDURE RegQueryInfoKeyW (hKey                  : HKEY;
                            lpClass               : LPWSTR;
                            lpcbClass             : LPDWORD;
                            lpReserved            : LPDWORD;
                            lpcSubKeys            : LPDWORD;
                            lpcbMaxSubKeyLen      : LPDWORD;
                            lpcbMaxClassLen       : LPDWORD;
                            lpcValues             : LPDWORD;
                            lpcbMaxValueNameLen   : LPDWORD;
                            lpcbMaxValueLen       : LPDWORD;
                            lpcbSecurityDescriptor: LPDWORD;
                            lpftLastWriteTime     : WinBase.PFILETIME): LONG;

CONST RegQueryInfoKey = RegQueryInfoKeyA;

<*EXTERNAL RegQueryValueA:APIENTRY*>
PROCEDURE RegQueryValueA (hKey     : HKEY;
                          lpSubKey : LPSTR;
                          lpValue  : LPSTR;
                          lpcbValue: LPDWORD): LONG;

<*EXTERNAL RegQueryValueW:APIENTRY*>
PROCEDURE RegQueryValueW (hKey     : HKEY;
                          lpSubKey : LPWSTR;
                          lpValue  : LPWSTR;
                          lpcbValue: LPDWORD ): LONG;

CONST RegQueryValue = RegQueryValueA;

<*EXTERNAL RegQueryValueExA:APIENTRY*>
PROCEDURE RegQueryValueExA (hKey       : HKEY;
                            lpValueName: LPSTR;
                            lpReserved : LPDWORD;
                            lpType     : LPDWORD;
                            lpData     : LPBYTE;
                            lpcbData   : LPDWORD  ): LONG;

<*EXTERNAL RegQueryValueExW:APIENTRY*>
PROCEDURE RegQueryValueExW (hKey       : HKEY;
                            lpValueName: LPWSTR;
                            lpReserved : LPDWORD;
                            lpType     : LPDWORD;
                            lpData     : LPBYTE;
                            lpcbData   : LPDWORD  ): LONG;

CONST RegQueryValueEx = RegQueryValueExA;

<*EXTERNAL RegReplaceKeyA:APIENTRY*>
PROCEDURE RegReplaceKeyA (hKey     : HKEY;
                          lpSubKey : LPSTR;
                          lpNewFile: LPSTR;
                          lpOldFile: LPSTR  ): LONG;

<*EXTERNAL RegReplaceKeyW:APIENTRY*>
PROCEDURE RegReplaceKeyW (hKey     : HKEY;
                          lpSubKey : LPWSTR;
                          lpNewFile: LPWSTR;
                          lpOldFile: LPWSTR  ): LONG;

CONST RegReplaceKey = RegReplaceKeyA;

<*EXTERNAL RegRestoreKeyA:APIENTRY*>
PROCEDURE RegRestoreKeyA (hKey: HKEY; lpFile: LPSTR; dwFlags: DWORD): LONG;

<*EXTERNAL RegRestoreKeyW:APIENTRY*>
PROCEDURE RegRestoreKeyW (hKey: HKEY; lpFile: LPWSTR; dwFlags: DWORD): LONG;

CONST RegRestoreKey = RegRestoreKeyA;

<*EXTERNAL RegSaveKeyA:APIENTRY*>
PROCEDURE RegSaveKeyA (hKey                : HKEY;
                       lpFile              : LPSTR;
                       lpSecurityAttributes: WinBase.LPSECURITY_ATTRIBUTES): LONG;

<*EXTERNAL RegSaveKeyW:APIENTRY*>
PROCEDURE RegSaveKeyW (hKey                : HKEY;
                       lpFile              : LPWSTR;
                       lpSecurityAttributes: WinBase.LPSECURITY_ATTRIBUTES): LONG;

CONST RegSaveKey = RegSaveKeyA;

<*EXTERNAL RegSetKeySecurity:APIENTRY*>
PROCEDURE RegSetKeySecurity (hKey               : HKEY;
                             SecurityInformation: WinNT.SECURITY_INFORMATION;
                             pSecurityDescriptor: WinNT.PSECURITY_DESCRIPTOR  ): LONG;

<*EXTERNAL RegSetValueA:APIENTRY*>
PROCEDURE RegSetValueA (hKey    : HKEY;
                        lpSubKey: LPSTR;
                        dwType  : DWORD;
                        lpData  : LPSTR;
                        cbData  : DWORD  ): LONG;

<*EXTERNAL RegSetValueW:APIENTRY*>
PROCEDURE RegSetValueW (hKey    : HKEY;
                        lpSubKey: LPWSTR;
                        dwType  : DWORD;
                        lpData  : LPWSTR;
                        cbData  : DWORD   ): LONG;

CONST RegSetValue = RegSetValueA;

<*EXTERNAL RegSetValueExA:APIENTRY*>
PROCEDURE RegSetValueExA (hKey       : HKEY;
                          lpValueName: LPSTR;
                          Reserved   : DWORD;
                          dwType     : DWORD;
                          lpData     : LPBYTE;
                          cbData     : DWORD   ): LONG;

<*EXTERNAL RegSetValueExW:APIENTRY*>
PROCEDURE RegSetValueExW (hKey       : HKEY;
                          lpValueName: LPWSTR;
                          Reserved   : DWORD;
                          dwType     : DWORD;
                          lpData     : LPBYTE;
                          cbData     : DWORD   ): LONG;

CONST RegSetValueEx = RegSetValueExA;

<*EXTERNAL RegUnLoadKeyA:APIENTRY*>
PROCEDURE RegUnLoadKeyA (hKey: HKEY; lpSubKey: LPSTR): LONG;

<*EXTERNAL RegUnLoadKeyW:APIENTRY*>
PROCEDURE RegUnLoadKeyW (hKey: HKEY; lpSubKey: LPWSTR): LONG;

CONST RegUnLoadKey = RegUnLoadKeyA;

(* Remoteable System Shutdown APIs *)

<*EXTERNAL InitiateSystemShutdownA:APIENTRY*>
PROCEDURE InitiateSystemShutdownA (lpMachineName       : LPSTR;
                                   lpMessage           : LPSTR;
                                   dwTimeout           : DWORD;
                                   bForceAppsClosed    : BOOL;
                                   bRebootAfterShutdown: BOOL   ): BOOL;

<*EXTERNAL InitiateSystemShutdownW:APIENTRY*>
PROCEDURE InitiateSystemShutdownW (lpMachineName       : LPWSTR;
                                   lpMessage           : LPWSTR;
                                   dwTimeout           : DWORD;
                                   bForceAppsClosed    : BOOL;
                                   bRebootAfterShutdown: BOOL    ): BOOL;

CONST InitiateSystemShutdown = InitiateSystemShutdownA;

<*EXTERNAL AbortSystemShutdownA:APIENTRY*>
PROCEDURE AbortSystemShutdownA (lpMachineName: LPSTR): BOOL;

<*EXTERNAL AbortSystemShutdownW:APIENTRY*>
PROCEDURE AbortSystemShutdownW (lpMachineName: LPWSTR): BOOL;

CONST AbortSystemShutdown = AbortSystemShutdownA;

END WinReg.
