(* Copyright (C) 1994, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* by Stephen Harrison                                       *)
(*                                                           *)
(* Last modified on Wed Dec 21 09:18:40 PST 1994 by kalsow   *)
(*      modified on Tue Mar 23 18:10:43 PST 1993 by harrison *)

INTERFACE WinVer;

(*****************************************************************************\
*                                                                             *
* winver.h -    Version management functions, types, and definitions          *
*                                                                             *
*               Include file for VER.DLL.  This library is                    *
*               designed to allow version stamping of Windows executable files*
*               and of special .VER files for DOS executable files.           *
*                                                                             *
*               Copyright (c) 1992, Microsoft Corp.  All rights reserved      *
*                                                                             *
\*****************************************************************************)

FROM WinNT IMPORT LONG, LPWSTR, LPSTR, LPTSTR;
FROM WinDef IMPORT DWORD, LPDWORD, BOOL, LPVOID;

(* ----- Symbols ----- *)
VAR                             (* CONST *)
  VS_FILE_INFO: LPTSTR;

CONST
  VS_VERSION_INFO = 1;
  VS_USER_DEFINED = 100;

(* ----- VS_VERSION.dwFileFlags ----- *)
CONST
  VS_FFI_SIGNATURE    : LONG = 16_FEEF04BD;
  VS_FFI_STRUCVERSION : LONG = 16_00010000;
  VS_FFI_FILEFLAGSMASK: LONG = 16_0000003F;

(* ----- VS_VERSION.dwFileFlags ----- *)
CONST
  VS_FF_DEBUG       : LONG = 16_00000001;
  VS_FF_PRERELEASE  : LONG = 16_00000002;
  VS_FF_PATCHED     : LONG = 16_00000004;
  VS_FF_PRIVATEBUILD: LONG = 16_00000008;
  VS_FF_INFOINFERRED: LONG = 16_00000010;
  VS_FF_SPECIALBUILD: LONG = 16_00000020;

(* ----- VS_VERSION.dwFileOS ----- *)
CONST
  VOS_UNKNOWN: LONG = 16_00000000;
  VOS_DOS    : LONG = 16_00010000;
  VOS_OS216  : LONG = 16_00020000;
  VOS_OS232  : LONG = 16_00030000;
  VOS_NT     : LONG = 16_00040000;

  VOS__BASE     : LONG = 16_00000000;
  VOS__WINDOWS16: LONG = 16_00000001;
  VOS__PM16     : LONG = 16_00000002;
  VOS__PM32     : LONG = 16_00000003;
  VOS__WINDOWS32: LONG = 16_00000004;

  VOS_DOS_WINDOWS16: LONG = 16_00010001;
  VOS_DOS_WINDOWS32: LONG = 16_00010004;
  VOS_OS216_PM16   : LONG = 16_00020002;
  VOS_OS232_PM32   : LONG = 16_00030003;
  VOS_NT_WINDOWS32 : LONG = 16_00040004;

(* ----- VS_VERSION.dwFileType ----- *)
CONST
  VFT_UNKNOWN   : LONG = 16_00000000;
  VFT_APP       : LONG = 16_00000001;
  VFT_DLL       : LONG = 16_00000002;
  VFT_DRV       : LONG = 16_00000003;
  VFT_FONT      : LONG = 16_00000004;
  VFT_VXD       : LONG = 16_00000005;
  VFT_STATIC_LIB: LONG = 16_00000007;

(* ----- VS_VERSION.dwFileSubtype for VFT_WINDOWS_DRV ----- *)
CONST
  VFT2_UNKNOWN        : LONG = 16_00000000;
  VFT2_DRV_PRINTER    : LONG = 16_00000001;
  VFT2_DRV_KEYBOARD   : LONG = 16_00000002;
  VFT2_DRV_LANGUAGE   : LONG = 16_00000003;
  VFT2_DRV_DISPLAY    : LONG = 16_00000004;
  VFT2_DRV_MOUSE      : LONG = 16_00000005;
  VFT2_DRV_NETWORK    : LONG = 16_00000006;
  VFT2_DRV_SYSTEM     : LONG = 16_00000007;
  VFT2_DRV_INSTALLABLE: LONG = 16_00000008;
  VFT2_DRV_SOUND      : LONG = 16_00000009;
  VFT2_DRV_COMM       : LONG = 16_0000000A;

(* ----- VS_VERSION.dwFileSubtype for VFT_WINDOWS_FONT ----- *)
CONST
  VFT2_FONT_RASTER  : LONG = 16_00000001;
  VFT2_FONT_VECTOR  : LONG = 16_00000002;
  VFT2_FONT_TRUETYPE: LONG = 16_00000003;

(* ----- VerFindFile() flags ----- *)
CONST
  VFFF_ISSHAREDFILE = 16_0001;

  VFF_CURNEDEST    = 16_0001;
  VFF_FILEINUSE    = 16_0002;
  VFF_BUFFTOOSMALL = 16_0004;

(* ----- VerInstallFile() flags ----- *)
CONST
  VIFF_FORCEINSTALL  = 16_0001;
  VIFF_DONTDELETEOLD = 16_0002;

  VIF_TEMPFILE: LONG = 16_00000001;
  VIF_MISMATCH: LONG = 16_00000002;
  VIF_SRCOLD  : LONG = 16_00000004;

  VIF_DIFFLANG  : LONG = 16_00000008;
  VIF_DIFFCODEPG: LONG = 16_00000010;
  VIF_DIFFTYPE  : LONG = 16_00000020;

  VIF_WRITEPROT       : LONG = 16_00000040;
  VIF_FILEINUSE       : LONG = 16_00000080;
  VIF_OUTOFSPACE      : LONG = 16_00000100;
  VIF_ACCESSVIOLATION : LONG = 16_00000200;
  VIF_SHARINGVIOLATION: LONG = 16_00000400;
  VIF_CANNOTCREATE    : LONG = 16_00000800;
  VIF_CANNOTDELETE    : LONG = 16_00001000;
  VIF_CANNOTRENAME    : LONG = 16_00002000;
  VIF_CANNOTDELETECUR : LONG = 16_00004000;
  VIF_OUTOFMEMORY     : LONG = 16_00008000;

  VIF_CANNOTREADSRC: LONG = 16_00010000;
  VIF_CANNOTREADDST: LONG = 16_00020000;

  VIF_BUFFTOOSMALL: LONG = 16_00040000;

(* ----- Types and structures ----- *)

TYPE
  VS_FIXEDFILEINFO = RECORD
    dwSignature       : DWORD;  (* e.g.  16_feef04bd *)
    dwStrucVersion    : DWORD;  (* e.g.  16_00000042 = "0.42" *)
    dwFileVersionMS   : DWORD;  (* e.g.  16_00030075 = "3.75" *)
    dwFileVersionLS   : DWORD;  (* e.g.  16_00000031 = "0.31" *)
    dwProductVersionMS: DWORD;  (* e.g.  16_00030010 = "3.10" *)
    dwProductVersionLS: DWORD;  (* e.g.  16_00000031 = "0.31" *)
    dwFileFlagsMask   : DWORD;  (* = 16_3F for version "0.42" *)
    dwFileFlags       : DWORD;  (* e.g.  VFF_DEBUG | VFF_PRERELEASE *)
    dwFileOS          : DWORD;  (* e.g.  VOS_DOS_WINDOWS16 *)
    dwFileType        : DWORD;  (* e.g.  VFT_DRIVER *)
    dwFileSubtype     : DWORD;  (* e.g.  VFT2_DRV_KEYBOARD *)
    dwFileDateMS      : DWORD;  (* e.g.  0 *)
    dwFileDateLS      : DWORD;  (* e.g.  0 *)
  END;

(* ----- Function prototypes ----- *)

<*EXTERNAL VerFindFileA:APIENTRY*>
PROCEDURE VerFindFileA (uFlags       : DWORD;
                        szFileName   : LPSTR;
                        szWinDir     : LPSTR;
                        szAppDir     : LPSTR;
                        szCurDir     : LPSTR;
                        lpuCurDirLen : LPDWORD;
                        szDestDir    : LPSTR;
                        lpuDestDirLen: LPDWORD  ): DWORD;

<*EXTERNAL VerFindFileW:APIENTRY*>
PROCEDURE VerFindFileW (uFlags       : DWORD;
                        szFileName   : LPWSTR;
                        szWinDir     : LPWSTR;
                        szAppDir     : LPWSTR;
                        szCurDir     : LPWSTR;
                        lpuCurDirLen : LPDWORD;
                        szDestDir    : LPWSTR;
                        lpuDestDirLen: LPDWORD  ): DWORD;

CONST VerFindFile = VerFindFileA;

<*EXTERNAL VerInstallFileA:APIENTRY*>
PROCEDURE VerInstallFileA (uFlags        : DWORD;
                           szSrcFileName : LPSTR;
                           szDestFileName: LPSTR;
                           szSrcDir      : LPSTR;
                           szDestDir     : LPSTR;
                           szCurDir      : LPSTR;
                           szTmpFile     : LPSTR;
                           lpuTmpFileLen : LPDWORD): DWORD;

<*EXTERNAL VerInstallFileW:APIENTRY*>
PROCEDURE VerInstallFileW (uFlags        : DWORD;
                           szSrcFileName : LPWSTR;
                           szDestFileName: LPWSTR;
                           szSrcDir      : LPWSTR;
                           szDestDir     : LPWSTR;
                           szCurDir      : LPWSTR;
                           szTmpFile     : LPWSTR;
                           lpuTmpFileLen : LPDWORD ): DWORD;

CONST VerInstallFile = VerInstallFileA;

(* Returns size of version info in bytes *)
<*EXTERNAL GetFileVersionInfoSizeA:APIENTRY*>
PROCEDURE GetFileVersionInfoSizeA (lptstrFilename: LPSTR;  (* Filename of
                                                                version
                                                                stamped
                                                                file *)

                                   lpdwHandle: LPDWORD): DWORD;
                        (* Information for use by GetFileVersionInfo *)

(* Returns size of version info in bytes *)
<*EXTERNAL GetFileVersionInfoSizeW:APIENTRY*>
PROCEDURE GetFileVersionInfoSizeW (lptstrFilename: LPWSTR;  (* Filename
                                                                 of version
                                                                 stamped
                                                                 file *)

                                     lpdwHandle: LPDWORD): DWORD;
       (* Information for use by GetFileVersionInfo *)

CONST GetFileVersionInfoSize = GetFileVersionInfoSizeA;

(* Read version info into buffer *)
<*EXTERNAL GetFileVersionInfoA:APIENTRY*>
PROCEDURE GetFileVersionInfoA (lptstrFilename: LPSTR;  (* Filename of
                                                            version stamped
                                                            file *)

                                 dwHandle: DWORD;  (* Information from
                                                      GetFileVersionSize *)
                                 dwLen: DWORD;  (* Length of buffer for
                                                   info *)
                                 lpData: LPVOID): BOOL;
  (* Buffer to place the data structure *)

(* Read version info into buffer *)
<*EXTERNAL GetFileVersionInfoW:APIENTRY*>
PROCEDURE GetFileVersionInfoW (lptstrFilename: LPWSTR;  (* Filename of
                                                             version
                                                             stamped
                                                             file *)

                               dwHandle: DWORD;  (* Information from
                                                      GetFileVersionSize *)
                               dwLen: DWORD;  (* Length of buffer for
                                                   info *)
                               lpData: LPVOID): BOOL;
                         (* Buffer to place the data structure *)

CONST GetFileVersionInfo = GetFileVersionInfoA;

<*EXTERNAL VerLanguageNameA:APIENTRY*>
PROCEDURE VerLanguageNameA (wLang: DWORD; szLang: LPSTR; nSize: DWORD): DWORD;

<*EXTERNAL VerLanguageNameW:APIENTRY*>
PROCEDURE VerLanguageNameW (wLang: DWORD; szLang: LPWSTR; nSize: DWORD): DWORD;

CONST VerLanguageName = VerLanguageNameA;

<*EXTERNAL VerQueryValueA:APIENTRY*>
PROCEDURE VerQueryValueA (pBlock    : LPVOID;
                          lpSubBlock: LPSTR;
                          lplpBuffer: UNTRACED REF LPVOID;
                          lpuLen    : LPDWORD              ): BOOL;

<*EXTERNAL VerQueryValueW:APIENTRY*>
PROCEDURE VerQueryValueW (pBlock    : LPVOID;
                          lpSubBlock: LPWSTR;
                          lplpBuffer: UNTRACED REF LPVOID;
                          lpuLen    : LPDWORD              ): BOOL;

CONST VerQueryValue = VerQueryValueA;


END WinVer.
