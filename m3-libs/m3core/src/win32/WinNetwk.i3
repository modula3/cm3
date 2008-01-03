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

FROM WinDef IMPORT BOOL, DWORD, LPDWORD, LPVOID, LPHANDLE;
FROM WinNT IMPORT HANDLE, LPSTR, LPWSTR;

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
  LPNETRESOURCEA = UNTRACED REF NETRESOURCEA;
  NETRESOURCEA = RECORD
    dwScope      : DWORD;
    dwType       : DWORD;
    dwDisplayType: DWORD;
    dwUsage      : DWORD;
    lpLocalName  : LPSTR;
    lpRemoteName : LPSTR;
    lpComment    : LPSTR;
    lpProvider   : LPSTR;
  END;

  LPNETRESOURCEW = UNTRACED REF NETRESOURCEW;
  NETRESOURCEW = RECORD
    dwScope      : DWORD;
    dwType       : DWORD;
    dwDisplayType: DWORD;
    dwUsage      : DWORD;
    lpLocalName  : LPWSTR;
    lpRemoteName : LPWSTR;
    lpComment    : LPWSTR;
    lpProvider   : LPWSTR;
  END;

  NETRESOURCE   = NETRESOURCEA;
  LPNETRESOURCE = LPNETRESOURCEA;

(* CONNECTIONS *)

CONST CONNECT_UPDATE_PROFILE = 16_00000001;

TYPE

<*EXTERNAL WNetAddConnectionA:APIENTRY*>
PROCEDURE WNetAddConnectionA (lpRemoteName: LPSTR;
                              lpPassword  : LPSTR;
                              lpLocalName : LPSTR  ): DWORD;

<*EXTERNAL WNetAddConnectionW:APIENTRY*>
PROCEDURE WNetAddConnectionW (lpRemoteName: LPWSTR;
                              lpPassword  : LPWSTR;
                              lpLocalName : LPWSTR  ): DWORD;
CONST WNetAddConnection = WNetAddConnectionA;

<*EXTERNAL WNetAddConnection2A:APIENTRY*>
PROCEDURE WNetAddConnection2A (lpNetResource: LPNETRESOURCEA;
                               lpPassword   : LPSTR;
                               lpUserName   : LPSTR;
                               dwFlags      : DWORD           ): DWORD;

<*EXTERNAL WNetAddConnection2W:APIENTRY*>
PROCEDURE WNetAddConnection2W (lpNetResource: LPNETRESOURCEW;
                               lpPassword   : LPWSTR;
                               lpUserName   : LPWSTR;
                               dwFlags      : DWORD           ): DWORD;
CONST WNetAddConnection2 = WNetAddConnection2A;

<*EXTERNAL WNetCancelConnectionA:APIENTRY*>
PROCEDURE WNetCancelConnectionA (lpName: LPSTR; fForce: BOOL): DWORD;

<*EXTERNAL WNetCancelConnectionW:APIENTRY*>
PROCEDURE WNetCancelConnectionW (lpName: LPWSTR; fForce: BOOL): DWORD;
CONST WNetCancelConnection = WNetCancelConnectionA;

<*EXTERNAL WNetCancelConnection2A:APIENTRY*>
PROCEDURE WNetCancelConnection2A (lpName : LPSTR;
                                  dwFlags: DWORD;
                                  fForce : BOOL   ): DWORD;

<*EXTERNAL WNetCancelConnection2W:APIENTRY*>
PROCEDURE WNetCancelConnection2W (lpName : LPWSTR;
                                  dwFlags: DWORD;
                                  fForce : BOOL    ): DWORD;
CONST WNetCancelConnection2 = WNetCancelConnection2A;

<*EXTERNAL WNetGetConnectionA:APIENTRY*>
PROCEDURE WNetGetConnectionA (lpLocalName : LPSTR;
                              lpRemoteName: LPSTR;
                              lpnLength   : LPDWORD): DWORD;

<*EXTERNAL WNetGetConnectionW:APIENTRY*>
PROCEDURE WNetGetConnectionW (lpLocalName : LPWSTR;
                              lpRemoteName: LPWSTR;
                              lpnLength   : LPDWORD ): DWORD;
CONST WNetGetConnection = WNetGetConnectionA;

<*EXTERNAL WNetOpenEnumA:APIENTRY*>
PROCEDURE WNetOpenEnumA (dwScope      : DWORD;
                         dwType       : DWORD;
                         dwUsage      : DWORD;
                         lpNetResource: LPNETRESOURCEA;
                         lphEnum      : LPHANDLE        ): DWORD;

<*EXTERNAL WNetOpenEnumW:APIENTRY*>
PROCEDURE WNetOpenEnumW (dwScope      : DWORD;
                         dwType       : DWORD;
                         dwUsage      : DWORD;
                         lpNetResource: LPNETRESOURCEW;
                         lphEnum      : LPHANDLE        ): DWORD;
CONST WNetOpenEnum = WNetOpenEnumA;

<*EXTERNAL WNetEnumResourceA:APIENTRY*>
PROCEDURE WNetEnumResourceA (hEnum       : HANDLE;
                             lpcCount    : LPDWORD;
                             lpBuffer    : LPVOID;
                             lpBufferSize: LPDWORD  ): DWORD;

<*EXTERNAL WNetEnumResourceW:APIENTRY*>
PROCEDURE WNetEnumResourceW (hEnum       : HANDLE;
                             lpcCount    : LPDWORD;
                             lpBuffer    : LPVOID;
                             lpBufferSize: LPDWORD  ): DWORD;
CONST WNetEnumResource = WNetEnumResourceA;

<*EXTERNAL WNetCloseEnum:APIENTRY*>
PROCEDURE WNetCloseEnum (hEnum: HANDLE): DWORD;

(* OTHER *)

<*EXTERNAL WNetGetUserA:APIENTRY*>
PROCEDURE WNetGetUserA (lpName    : LPSTR;
                        lpUserName: LPSTR;
                        lpnLength : LPDWORD): DWORD;

<*EXTERNAL WNetGetUserW:APIENTRY*>
PROCEDURE WNetGetUserW (lpName    : LPWSTR;
                        lpUserName: LPWSTR;
                        lpnLength : LPDWORD ): DWORD;
CONST WNetGetUser = WNetGetUserA;

(* BROWSE DIALOG *)

<*EXTERNAL HWND:APIENTRY*>
PROCEDURE HWND (dwType: DWORD): DWORD;

(* ERRORS *)

<*EXTERNAL WNetGetLastErrorA:APIENTRY*>
PROCEDURE WNetGetLastErrorA (lpError      : LPDWORD;
                             lpErrorBuf   : LPSTR;
                             nErrorBufSize: DWORD;
                             lpNameBuf    : LPSTR;
                             nNameBufSize : DWORD    ): DWORD;

<*EXTERNAL WNetGetLastErrorW:APIENTRY*>
PROCEDURE WNetGetLastErrorW (lpError      : LPDWORD;
                             lpErrorBuf   : LPWSTR;
                             nErrorBufSize: DWORD;
                             lpNameBuf    : LPWSTR;
                             nNameBufSize : DWORD    ): DWORD;
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
