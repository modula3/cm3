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

IMPORT WinBase, WinNT;

FROM WinNT IMPORT INT32, PSTR, PWSTR, HANDLE;
FROM WinDef IMPORT UINT32, PUINT32, PUINT8, BOOL;

(* Requested Key access mask type. *)

TYPE REGSAM = WinNT.ACCESS_MASK;

(* Type definitions. *)

TYPE
  HKEY  = UNTRACED BRANDED "HKEY" REF RECORD unused: INT32;  END;
  PHKEY = UNTRACED REF HKEY;

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
PROCEDURE RegCloseKey (hKey: HKEY): INT32;

<*EXTERNAL RegConnectRegistryA:APIENTRY*>
PROCEDURE RegConnectRegistryA (lpMachineName: PSTR;
                               hKey         : HKEY;
                               phkResult    : PHKEY  ): INT32;

<*EXTERNAL RegConnectRegistryW:APIENTRY*>
PROCEDURE RegConnectRegistryW (lpMachineName: PWSTR;
                               hKey         : HKEY;
                               phkResult    : PHKEY   ): INT32;

CONST RegConnectRegistry = RegConnectRegistryA;

<*EXTERNAL RegCreateKeyA:APIENTRY*>
PROCEDURE RegCreateKeyA (hKey: HKEY; lpSubKey: PSTR; phkResult: PHKEY): INT32;

<*EXTERNAL RegCreateKeyW:APIENTRY*>
PROCEDURE RegCreateKeyW (hKey: HKEY; lpSubKey: PWSTR; phkResult: PHKEY): INT32;

CONST RegCreateKey = RegCreateKeyA;

<*EXTERNAL RegCreateKeyExA:APIENTRY*>
PROCEDURE RegCreateKeyExA (hKey                : HKEY;
                           lpSubKey            : PSTR;
                           Reserved            : UINT32;
                           lpClass             : PSTR;
                           dwOptions           : UINT32;
                           samDesired          : REGSAM;
                           lpSecurityAttributes: WinBase.LPSECURITY_ATTRIBUTES;
                           phkResult           : PHKEY;
                           lpdwDisposition     : PUINT32                ): INT32;

<*EXTERNAL RegCreateKeyExW:APIENTRY*>
PROCEDURE RegCreateKeyExW (hKey                : HKEY;
                           lpSubKey            : PWSTR;
                           Reserved            : UINT32;
                           lpClass             : PWSTR;
                           dwOptions           : UINT32;
                           samDesired          : REGSAM;
                           lpSecurityAttributes: WinBase.LPSECURITY_ATTRIBUTES;
                           phkResult           : PHKEY;
                           lpdwDisposition     : PUINT32                ): INT32;

CONST RegCreateKeyEx = RegCreateKeyExA;

<*EXTERNAL RegDeleteKeyA:APIENTRY*>
PROCEDURE RegDeleteKeyA (hKey: HKEY; lpSubKey: PSTR): INT32;

<*EXTERNAL RegDeleteKeyW:APIENTRY*>
PROCEDURE RegDeleteKeyW (hKey: HKEY; lpSubKey: PWSTR): INT32;

CONST RegDeleteKey = RegDeleteKeyA;

<*EXTERNAL RegDeleteValueA:APIENTRY*>
PROCEDURE RegDeleteValueA (hKey: HKEY; lpValueName: PSTR): INT32;

<*EXTERNAL RegDeleteValueW:APIENTRY*>
PROCEDURE RegDeleteValueW (hKey: HKEY; lpValueName: PWSTR): INT32;

CONST RegDeleteValue = RegDeleteValueA;

<*EXTERNAL RegEnumKeyA:APIENTRY*>
PROCEDURE RegEnumKeyA (hKey   : HKEY;
                                                     dwIndex: UINT32;
                                                     lpName : PSTR;
                                                     cbName : UINT32  ): INT32;

<*EXTERNAL RegEnumKeyW:APIENTRY*>
PROCEDURE RegEnumKeyW (hKey   : HKEY;
                                                     dwIndex: UINT32;
                                                     lpName : PWSTR;
                                                     cbName : UINT32   ): INT32;

CONST RegEnumKey = RegEnumKeyA;

<*EXTERNAL RegEnumKeyExA:APIENTRY*>
PROCEDURE RegEnumKeyExA (hKey             : HKEY;
                         dwIndex          : UINT32;
                         lpName           : PSTR;
                         lpcbName         : PUINT32;
                         lpReserved       : PUINT32;
                         lpClass          : PSTR;
                         lpcbClass        : PUINT32;
                         lpftLastWriteTime: WinBase.PFILETIME): INT32;

<*EXTERNAL RegEnumKeyExW:APIENTRY*>
PROCEDURE RegEnumKeyExW (hKey             : HKEY;
                         dwIndex          : UINT32;
                         lpName           : PWSTR;
                         lpcbName         : PUINT32;
                         lpReserved       : PUINT32;
                         lpClass          : PWSTR;
                         lpcbClass        : PUINT32;
                         lpftLastWriteTime: WinBase.PFILETIME): INT32;

CONST RegEnumKeyEx = RegEnumKeyExA;

<*EXTERNAL RegEnumValueA:APIENTRY*>
PROCEDURE RegEnumValueA (hKey         : HKEY;
                         dwIndex      : UINT32;
                         lpValueName  : PSTR;
                         lpcbValueName: PUINT32;
                         lpReserved   : PUINT32;
                         lpType       : PUINT32;
                         lpData       : PUINT8;
                         lpcbData     : PUINT32  ): INT32;

<*EXTERNAL RegEnumValueW:APIENTRY*>
PROCEDURE RegEnumValueW (hKey         : HKEY;
                         dwIndex      : UINT32;
                         lpValueName  : PWSTR;
                         lpcbValueName: PUINT32;
                         lpReserved   : PUINT32;
                         lpType       : PUINT32;
                         lpData       : PUINT8;
                         lpcbData     : PUINT32  ): INT32;

CONST RegEnumValue = RegEnumValueA;

<*EXTERNAL RegFlushKey:APIENTRY*>
PROCEDURE RegFlushKey (hKey: HKEY): INT32;

<*EXTERNAL RegGetKeySecurity:APIENTRY*>
PROCEDURE RegGetKeySecurity (hKey               : HKEY;
                             SecurityInformation: WinNT.SECURITY_INFORMATION;
                             pSecurityDescriptor: WinNT.PSECURITY_DESCRIPTOR;
                             lpcbSecurityDescriptor: PUINT32): INT32;

<*EXTERNAL RegLoadKeyA:APIENTRY*>
PROCEDURE RegLoadKeyA (hKey: HKEY; lpSubKey: PSTR; lpFile: PSTR): INT32;

<*EXTERNAL RegLoadKeyW:APIENTRY*>
PROCEDURE RegLoadKeyW (hKey: HKEY; lpSubKey: PWSTR; lpFile: PWSTR): INT32;

CONST RegLoadKey = RegLoadKeyA;

<*EXTERNAL RegNotifyChangeKeyValue:APIENTRY*>
PROCEDURE RegNotifyChangeKeyValue (hKey          : HKEY;
                                   bWatchSubtree : BOOL;
                                   dwNotifyFilter: UINT32;
                                   hEvent        : HANDLE;
                                   fAsynchronus  : BOOL    ): INT32;

<*EXTERNAL RegOpenKeyA:APIENTRY*>
PROCEDURE RegOpenKeyA (hKey: HKEY; lpSubKey: PSTR; phkResult: PHKEY): INT32;

<*EXTERNAL RegOpenKeyW:APIENTRY*>
PROCEDURE RegOpenKeyW (hKey: HKEY; lpSubKey: PWSTR; phkResult: PHKEY): INT32;

CONST RegOpenKey = RegOpenKeyA;

<*EXTERNAL RegOpenKeyExA:APIENTRY*>
PROCEDURE RegOpenKeyExA (hKey      : HKEY;
                         lpSubKey  : PSTR;
                         ulOptions : UINT32;
                         samDesired: REGSAM;
                         phkResult : PHKEY   ): INT32;

<*EXTERNAL RegOpenKeyExW:APIENTRY*>
PROCEDURE RegOpenKeyExW (hKey      : HKEY;
                         lpSubKey  : PWSTR;
                         ulOptions : UINT32;
                         samDesired: REGSAM;
                         phkResult : PHKEY   ): INT32;

CONST RegOpenKeyEx = RegOpenKeyExA;

<*EXTERNAL RegQueryInfoKeyA:APIENTRY*>
PROCEDURE RegQueryInfoKeyA (hKey                  : HKEY;
                            lpClass               : PSTR;
                            lpcbClass             : PUINT32;
                            lpReserved            : PUINT32;
                            lpcSubKeys            : PUINT32;
                            lpcbMaxSubKeyLen      : PUINT32;
                            lpcbMaxClassLen       : PUINT32;
                            lpcValues             : PUINT32;
                            lpcbMaxValueNameLen   : PUINT32;
                            lpcbMaxValueLen       : PUINT32;
                            lpcbSecurityDescriptor: PUINT32;
                            lpftLastWriteTime     : WinBase.PFILETIME): INT32;

<*EXTERNAL RegQueryInfoKeyW:APIENTRY*>
PROCEDURE RegQueryInfoKeyW (hKey                  : HKEY;
                            lpClass               : PWSTR;
                            lpcbClass             : PUINT32;
                            lpReserved            : PUINT32;
                            lpcSubKeys            : PUINT32;
                            lpcbMaxSubKeyLen      : PUINT32;
                            lpcbMaxClassLen       : PUINT32;
                            lpcValues             : PUINT32;
                            lpcbMaxValueNameLen   : PUINT32;
                            lpcbMaxValueLen       : PUINT32;
                            lpcbSecurityDescriptor: PUINT32;
                            lpftLastWriteTime     : WinBase.PFILETIME): INT32;

CONST RegQueryInfoKey = RegQueryInfoKeyA;

<*EXTERNAL RegQueryValueA:APIENTRY*>
PROCEDURE RegQueryValueA (hKey     : HKEY;
                          lpSubKey : PSTR;
                          lpValue  : PSTR;
                          lpcbValue: PUINT32): INT32;

<*EXTERNAL RegQueryValueW:APIENTRY*>
PROCEDURE RegQueryValueW (hKey     : HKEY;
                          lpSubKey : PWSTR;
                          lpValue  : PWSTR;
                          lpcbValue: PUINT32 ): INT32;

CONST RegQueryValue = RegQueryValueA;

<*EXTERNAL RegQueryValueExA:APIENTRY*>
PROCEDURE RegQueryValueExA (hKey       : HKEY;
                            lpValueName: PSTR;
                            lpReserved : PUINT32;
                            lpType     : PUINT32;
                            lpData     : PUINT8;
                            lpcbData   : PUINT32  ): INT32;

<*EXTERNAL RegQueryValueExW:APIENTRY*>
PROCEDURE RegQueryValueExW (hKey       : HKEY;
                            lpValueName: PWSTR;
                            lpReserved : PUINT32;
                            lpType     : PUINT32;
                            lpData     : PUINT8;
                            lpcbData   : PUINT32  ): INT32;

CONST RegQueryValueEx = RegQueryValueExA;

<*EXTERNAL RegReplaceKeyA:APIENTRY*>
PROCEDURE RegReplaceKeyA (hKey     : HKEY;
                          lpSubKey : PSTR;
                          lpNewFile: PSTR;
                          lpOldFile: PSTR  ): INT32;

<*EXTERNAL RegReplaceKeyW:APIENTRY*>
PROCEDURE RegReplaceKeyW (hKey     : HKEY;
                          lpSubKey : PWSTR;
                          lpNewFile: PWSTR;
                          lpOldFile: PWSTR  ): INT32;

CONST RegReplaceKey = RegReplaceKeyA;

<*EXTERNAL RegRestoreKeyA:APIENTRY*>
PROCEDURE RegRestoreKeyA (hKey: HKEY; lpFile: PSTR; dwFlags: UINT32): INT32;

<*EXTERNAL RegRestoreKeyW:APIENTRY*>
PROCEDURE RegRestoreKeyW (hKey: HKEY; lpFile: PWSTR; dwFlags: UINT32): INT32;

CONST RegRestoreKey = RegRestoreKeyA;

<*EXTERNAL RegSaveKeyA:APIENTRY*>
PROCEDURE RegSaveKeyA (hKey                : HKEY;
                       lpFile              : PSTR;
                       lpSecurityAttributes: WinBase.LPSECURITY_ATTRIBUTES): INT32;

<*EXTERNAL RegSaveKeyW:APIENTRY*>
PROCEDURE RegSaveKeyW (hKey                : HKEY;
                       lpFile              : PWSTR;
                       lpSecurityAttributes: WinBase.LPSECURITY_ATTRIBUTES): INT32;

CONST RegSaveKey = RegSaveKeyA;

<*EXTERNAL RegSetKeySecurity:APIENTRY*>
PROCEDURE RegSetKeySecurity (hKey               : HKEY;
                             SecurityInformation: WinNT.SECURITY_INFORMATION;
                             pSecurityDescriptor: WinNT.PSECURITY_DESCRIPTOR  ): INT32;

<*EXTERNAL RegSetValueA:APIENTRY*>
PROCEDURE RegSetValueA (hKey    : HKEY;
                        lpSubKey: PSTR;
                        dwType  : UINT32;
                        lpData  : PSTR;
                        cbData  : UINT32  ): INT32;

<*EXTERNAL RegSetValueW:APIENTRY*>
PROCEDURE RegSetValueW (hKey    : HKEY;
                        lpSubKey: PWSTR;
                        dwType  : UINT32;
                        lpData  : PWSTR;
                        cbData  : UINT32   ): INT32;

CONST RegSetValue = RegSetValueA;

<*EXTERNAL RegSetValueExA:APIENTRY*>
PROCEDURE RegSetValueExA (hKey       : HKEY;
                          lpValueName: PSTR;
                          Reserved   : UINT32;
                          dwType     : UINT32;
                          lpData     : PUINT8;
                          cbData     : UINT32   ): INT32;

<*EXTERNAL RegSetValueExW:APIENTRY*>
PROCEDURE RegSetValueExW (hKey       : HKEY;
                          lpValueName: PWSTR;
                          Reserved   : UINT32;
                          dwType     : UINT32;
                          lpData     : PUINT8;
                          cbData     : UINT32   ): INT32;

CONST RegSetValueEx = RegSetValueExA;

<*EXTERNAL RegUnLoadKeyA:APIENTRY*>
PROCEDURE RegUnLoadKeyA (hKey: HKEY; lpSubKey: PSTR): INT32;

<*EXTERNAL RegUnLoadKeyW:APIENTRY*>
PROCEDURE RegUnLoadKeyW (hKey: HKEY; lpSubKey: PWSTR): INT32;

CONST RegUnLoadKey = RegUnLoadKeyA;

(* Remoteable System Shutdown APIs *)

<*EXTERNAL InitiateSystemShutdownA:APIENTRY*>
PROCEDURE InitiateSystemShutdownA (lpMachineName       : PSTR;
                                   lpMessage           : PSTR;
                                   dwTimeout           : UINT32;
                                   bForceAppsClosed    : BOOL;
                                   bRebootAfterShutdown: BOOL   ): BOOL;

<*EXTERNAL InitiateSystemShutdownW:APIENTRY*>
PROCEDURE InitiateSystemShutdownW (lpMachineName       : PWSTR;
                                   lpMessage           : PWSTR;
                                   dwTimeout           : UINT32;
                                   bForceAppsClosed    : BOOL;
                                   bRebootAfterShutdown: BOOL    ): BOOL;

CONST InitiateSystemShutdown = InitiateSystemShutdownA;

<*EXTERNAL AbortSystemShutdownA:APIENTRY*>
PROCEDURE AbortSystemShutdownA (lpMachineName: PSTR): BOOL;

<*EXTERNAL AbortSystemShutdownW:APIENTRY*>
PROCEDURE AbortSystemShutdownW (lpMachineName: PWSTR): BOOL;

CONST AbortSystemShutdown = AbortSystemShutdownA;

END WinReg.
