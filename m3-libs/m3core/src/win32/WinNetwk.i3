(* Copyright (C) 1994, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* by Stephen Harrison                                       *)
(*                                                           *)
(* Last modified on Wed Dec 21 09:17:33 PST 1994 by kalsow   *)
(*      modified on Thu Feb 11 10:05:47 PST 1993 by harrison *)

INTERFACE WinNetwk;

(* Corresponds to "winnetwk.h".
   See that file for details.

   Standard WINNET Header File for NT-WIN32 *)

IMPORT WinError;

FROM WinDef IMPORT BOOL, UINT32, PUINT32, PVOID, PHANDLE;
FROM WinNT IMPORT HANDLE, PSTR, PWSTR;

(* RESOURCE ENUMERATION *)

CONST
  RESOURCE_CONNECTED  = 16_00000001;
  RESOURCE_GLOBALNET  = 16_00000002;
  RESOURCE_REMEMBERED = 16_00000003;

  RESOURCETYPE_ANY   = 16_00000000;
  RESOURCETYPE_DISK  = 16_00000001;
  RESOURCETYPE_PRINT = 16_00000002;

  RESOURCEUSAGE_CONNECTABLE = 16_00000001;
  RESOURCEUSAGE_CONTAINER   = 16_00000002;
  RESOURCEUSAGE_RESERVED    = 16_80000000;

  RESOURCEDISPLAYTYPE_GENERIC = 16_00000000;
  RESOURCEDISPLAYTYPE_DOMAIN  = 16_00000001;
  RESOURCEDISPLAYTYPE_SERVER  = 16_00000002;
  RESOURCEDISPLAYTYPE_SHARE   = 16_00000003;

TYPE
  PNETRESOURCEA = UNTRACED REF NETRESOURCEA;
  LPNETRESOURCEA = PNETRESOURCEA; (* compat *)
  NETRESOURCEA = RECORD
    dwScope      : UINT32;
    dwType       : UINT32;
    dwDisplayType: UINT32;
    dwUsage      : UINT32;
    lpLocalName  : PSTR;
    lpRemoteName : PSTR;
    lpComment    : PSTR;
    lpProvider   : PSTR;
  END;

  PNETRESOURCEW = UNTRACED REF NETRESOURCEW;
  LPNETRESOURCEW = PNETRESOURCEW; (* compat *)
  NETRESOURCEW = RECORD
    dwScope      : UINT32;
    dwType       : UINT32;
    dwDisplayType: UINT32;
    dwUsage      : UINT32;
    lpLocalName  : PWSTR;
    lpRemoteName : PWSTR;
    lpComment    : PWSTR;
    lpProvider   : PWSTR;
  END;

  NETRESOURCE   = NETRESOURCEA;
  LPNETRESOURCE = PNETRESOURCEA; (* compat *)

(* CONNECTIONS *)

CONST CONNECT_UPDATE_PROFILE = 16_00000001;

TYPE

<*EXTERNAL WNetAddConnectionA:APIENTRY*>
PROCEDURE WNetAddConnectionA (lpRemoteName: PSTR;
                              lpPassword  : PSTR;
                              lpLocalName : PSTR  ): UINT32;

<*EXTERNAL WNetAddConnectionW:APIENTRY*>
PROCEDURE WNetAddConnectionW (lpRemoteName: PWSTR;
                              lpPassword  : PWSTR;
                              lpLocalName : PWSTR  ): UINT32;
CONST WNetAddConnection = WNetAddConnectionA;

<*EXTERNAL WNetAddConnection2A:APIENTRY*>
PROCEDURE WNetAddConnection2A (lpNetResource: PNETRESOURCEA;
                               lpPassword   : PSTR;
                               lpUserName   : PSTR;
                               dwFlags      : UINT32           ): UINT32;

<*EXTERNAL WNetAddConnection2W:APIENTRY*>
PROCEDURE WNetAddConnection2W (lpNetResource: PNETRESOURCEW;
                               lpPassword   : PWSTR;
                               lpUserName   : PWSTR;
                               dwFlags      : UINT32           ): UINT32;
CONST WNetAddConnection2 = WNetAddConnection2A;

<*EXTERNAL WNetCancelConnectionA:APIENTRY*>
PROCEDURE WNetCancelConnectionA (lpName: PSTR; fForce: BOOL): UINT32;

<*EXTERNAL WNetCancelConnectionW:APIENTRY*>
PROCEDURE WNetCancelConnectionW (lpName: PWSTR; fForce: BOOL): UINT32;
CONST WNetCancelConnection = WNetCancelConnectionA;

<*EXTERNAL WNetCancelConnection2A:APIENTRY*>
PROCEDURE WNetCancelConnection2A (lpName : PSTR;
                                  dwFlags: UINT32;
                                  fForce : BOOL   ): UINT32;

<*EXTERNAL WNetCancelConnection2W:APIENTRY*>
PROCEDURE WNetCancelConnection2W (lpName : PWSTR;
                                  dwFlags: UINT32;
                                  fForce : BOOL    ): UINT32;
CONST WNetCancelConnection2 = WNetCancelConnection2A;

<*EXTERNAL WNetGetConnectionA:APIENTRY*>
PROCEDURE WNetGetConnectionA (lpLocalName : PSTR;
                              lpRemoteName: PSTR;
                              lpnLength   : PUINT32): UINT32;

<*EXTERNAL WNetGetConnectionW:APIENTRY*>
PROCEDURE WNetGetConnectionW (lpLocalName : PWSTR;
                              lpRemoteName: PWSTR;
                              lpnLength   : PUINT32 ): UINT32;
CONST WNetGetConnection = WNetGetConnectionA;

<*EXTERNAL WNetOpenEnumA:APIENTRY*>
PROCEDURE WNetOpenEnumA (dwScope      : UINT32;
                         dwType       : UINT32;
                         dwUsage      : UINT32;
                         lpNetResource: PNETRESOURCEA;
                         lphEnum      : PHANDLE        ): UINT32;

<*EXTERNAL WNetOpenEnumW:APIENTRY*>
PROCEDURE WNetOpenEnumW (dwScope      : UINT32;
                         dwType       : UINT32;
                         dwUsage      : UINT32;
                         lpNetResource: PNETRESOURCEW;
                         lphEnum      : PHANDLE        ): UINT32;
CONST WNetOpenEnum = WNetOpenEnumA;

<*EXTERNAL WNetEnumResourceA:APIENTRY*>
PROCEDURE WNetEnumResourceA (hEnum       : HANDLE;
                             lpcCount    : PUINT32;
                             lpBuffer    : PVOID;
                             lpBufferSize: PUINT32  ): UINT32;

<*EXTERNAL WNetEnumResourceW:APIENTRY*>
PROCEDURE WNetEnumResourceW (hEnum       : HANDLE;
                             lpcCount    : PUINT32;
                             lpBuffer    : PVOID;
                             lpBufferSize: PUINT32  ): UINT32;
CONST WNetEnumResource = WNetEnumResourceA;

<*EXTERNAL WNetCloseEnum:APIENTRY*>
PROCEDURE WNetCloseEnum (hEnum: HANDLE): UINT32;

(* OTHER *)

<*EXTERNAL WNetGetUserA:APIENTRY*>
PROCEDURE WNetGetUserA (lpName    : PSTR;
                        lpUserName: PSTR;
                        lpnLength : PUINT32): UINT32;

<*EXTERNAL WNetGetUserW:APIENTRY*>
PROCEDURE WNetGetUserW (lpName    : PWSTR;
                        lpUserName: PWSTR;
                        lpnLength : PUINT32 ): UINT32;
CONST WNetGetUser = WNetGetUserA;

(* BROWSE DIALOG *)

<*EXTERNAL HWND:APIENTRY*>
PROCEDURE HWND (dwType: UINT32): UINT32;

(* ERRORS *)

<*EXTERNAL WNetGetLastErrorA:APIENTRY*>
PROCEDURE WNetGetLastErrorA (lpError      : PUINT32;
                             lpErrorBuf   : PSTR;
                             nErrorBufSize: UINT32;
                             lpNameBuf    : PSTR;
                             nNameBufSize : UINT32    ): UINT32;

<*EXTERNAL WNetGetLastErrorW:APIENTRY*>
PROCEDURE WNetGetLastErrorW (lpError      : PUINT32;
                             lpErrorBuf   : PWSTR;
                             nErrorBufSize: UINT32;
                             lpNameBuf    : PWSTR;
                             nNameBufSize : UINT32    ): UINT32;
CONST WNetGetLastError = WNetGetLastErrorA;

(* STATUS CODES *)

(* This section is provided for backward compatibility.  Use of the ERROR_*
   codes is preferred.  The WN_* error codes may not be available in future
   releases. *)

(* General *)

CONST
  WN_SUCCESS        = WinError.NO_ERROR;
  WN_NOT_SUPPORTED  = WinError.ERROR_NOT_SUPPORTED;
  WN_NET_ERROR      = WinError.ERROR_UNEXP_NET_ERR;
  WN_MORE_DATA      = WinError.ERROR_MORE_DATA;
  WN_BAD_POINTER    = WinError.ERROR_INVALID_ADDRESS;
  WN_BAD_VALUE      = WinError.ERROR_INVALID_PARAMETER;
  WN_BAD_PASSWORD   = WinError.ERROR_INVALID_PASSWORD;
  WN_ACCESS_DENIED  = WinError.ERROR_ACCESS_DENIED;
  WN_FUNCTION_BUSY  = WinError.ERROR_BUSY;
  WN_WINDOWS_ERROR  = WinError.ERROR_UNEXP_NET_ERR;
  WN_BAD_USER       = WinError.ERROR_BAD_USERNAME;
  WN_OUT_OF_MEMORY  = WinError.ERROR_NOT_ENOUGH_MEMORY;
  WN_NO_NETWORK     = WinError.ERROR_NO_NETWORK;
  WN_EXTENDED_ERROR = WinError.ERROR_EXTENDED_ERROR;

(* Connection *)

CONST
  WN_NOT_CONNECTED       = WinError.ERROR_NOT_CONNECTED;
  WN_OPEN_FILES          = WinError.ERROR_OPEN_FILES;
  WN_DEVICE_IN_USE       = WinError.ERROR_DEVICE_IN_USE;
  WN_BAD_NETNAME         = WinError.ERROR_BAD_NET_NAME;
  WN_BAD_LOCALNAME       = WinError.ERROR_BAD_DEVICE;
  WN_ALREADY_CONNECTED   = WinError.ERROR_ALREADY_ASSIGNED;
  WN_DEVICE_ERROR        = WinError.ERROR_GEN_FAILURE;
  WN_CONNECTION_CLOSED   = WinError.ERROR_CONNECTION_UNAVAIL;
  WN_NO_NET_OR_BAD_PATH  = WinError.ERROR_NO_NET_OR_BAD_PATH;
  WN_BAD_PROVIDER        = WinError.ERROR_BAD_PROVIDER;
  WN_CANNOT_OPEN_PROFILE = WinError.ERROR_CANNOT_OPEN_PROFILE;
  WN_BAD_PROFILE         = WinError.ERROR_BAD_PROFILE;

(* Enumeration *)

CONST
  WN_BAD_HANDLE      = WinError.ERROR_INVALID_HANDLE;
  WN_NO_MORE_ENTRIES = WinError.ERROR_NO_MORE_ITEMS;
  WN_NOT_CONTAINER   = WinError.ERROR_NOT_CONTAINER;

  WN_NO_ERROR = WinError.NO_ERROR;

END WinNetwk.
