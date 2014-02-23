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

FROM WinNT IMPORT INT32, PWSTR, PSTR, PTSTR;
FROM WinDef IMPORT UINT32, PUINT32, BOOL, PVOID;

(* ----- Symbols ----- *)
VAR                             (* CONST *)
  VS_FILE_INFO: PTSTR;

CONST
  VS_VERSION_INFO = 1;
  VS_USER_DEFINED = 100;

(* ----- VS_VERSION.dwFileFlags ----- *)
CONST
  VS_FFI_SIGNATURE    : INT32 = -17890115; (* 16_FEEF04BD *)
  VS_FFI_STRUCVERSION : INT32 = 16_00010000;
  VS_FFI_FILEFLAGSMASK: INT32 = 16_0000003F;

(* ----- VS_VERSION.dwFileFlags ----- *)
CONST
  VS_FF_DEBUG       : INT32 = 16_00000001;
  VS_FF_PRERELEASE  : INT32 = 16_00000002;
  VS_FF_PATCHED     : INT32 = 16_00000004;
  VS_FF_PRIVATEBUILD: INT32 = 16_00000008;
  VS_FF_INFOINFERRED: INT32 = 16_00000010;
  VS_FF_SPECIALBUILD: INT32 = 16_00000020;

(* ----- VS_VERSION.dwFileOS ----- *)
CONST
  VOS_UNKNOWN: INT32 = 16_00000000;
  VOS_DOS    : INT32 = 16_00010000;
  VOS_OS216  : INT32 = 16_00020000;
  VOS_OS232  : INT32 = 16_00030000;
  VOS_NT     : INT32 = 16_00040000;

  VOS__BASE     : INT32 = 16_00000000;
  VOS__WINDOWS16: INT32 = 16_00000001;
  VOS__PM16     : INT32 = 16_00000002;
  VOS__PM32     : INT32 = 16_00000003;
  VOS__WINDOWS32: INT32 = 16_00000004;

  VOS_DOS_WINDOWS16: INT32 = 16_00010001;
  VOS_DOS_WINDOWS32: INT32 = 16_00010004;
  VOS_OS216_PM16   : INT32 = 16_00020002;
  VOS_OS232_PM32   : INT32 = 16_00030003;
  VOS_NT_WINDOWS32 : INT32 = 16_00040004;

(* ----- VS_VERSION.dwFileType ----- *)
CONST
  VFT_UNKNOWN   : INT32 = 16_00000000;
  VFT_APP       : INT32 = 16_00000001;
  VFT_DLL       : INT32 = 16_00000002;
  VFT_DRV       : INT32 = 16_00000003;
  VFT_FONT      : INT32 = 16_00000004;
  VFT_VXD       : INT32 = 16_00000005;
  VFT_STATIC_LIB: INT32 = 16_00000007;

(* ----- VS_VERSION.dwFileSubtype for VFT_WINDOWS_DRV ----- *)
CONST
  VFT2_UNKNOWN        : INT32 = 16_00000000;
  VFT2_DRV_PRINTER    : INT32 = 16_00000001;
  VFT2_DRV_KEYBOARD   : INT32 = 16_00000002;
  VFT2_DRV_LANGUAGE   : INT32 = 16_00000003;
  VFT2_DRV_DISPLAY    : INT32 = 16_00000004;
  VFT2_DRV_MOUSE      : INT32 = 16_00000005;
  VFT2_DRV_NETWORK    : INT32 = 16_00000006;
  VFT2_DRV_SYSTEM     : INT32 = 16_00000007;
  VFT2_DRV_INSTALLABLE: INT32 = 16_00000008;
  VFT2_DRV_SOUND      : INT32 = 16_00000009;
  VFT2_DRV_COMM       : INT32 = 16_0000000A;

(* ----- VS_VERSION.dwFileSubtype for VFT_WINDOWS_FONT ----- *)
CONST
  VFT2_FONT_RASTER  : INT32 = 16_00000001;
  VFT2_FONT_VECTOR  : INT32 = 16_00000002;
  VFT2_FONT_TRUETYPE: INT32 = 16_00000003;

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

  VIF_TEMPFILE: INT32 = 16_00000001;
  VIF_MISMATCH: INT32 = 16_00000002;
  VIF_SRCOLD  : INT32 = 16_00000004;

  VIF_DIFFLANG  : INT32 = 16_00000008;
  VIF_DIFFCODEPG: INT32 = 16_00000010;
  VIF_DIFFTYPE  : INT32 = 16_00000020;

  VIF_WRITEPROT       : INT32 = 16_00000040;
  VIF_FILEINUSE       : INT32 = 16_00000080;
  VIF_OUTOFSPACE      : INT32 = 16_00000100;
  VIF_ACCESSVIOLATION : INT32 = 16_00000200;
  VIF_SHARINGVIOLATION: INT32 = 16_00000400;
  VIF_CANNOTCREATE    : INT32 = 16_00000800;
  VIF_CANNOTDELETE    : INT32 = 16_00001000;
  VIF_CANNOTRENAME    : INT32 = 16_00002000;
  VIF_CANNOTDELETECUR : INT32 = 16_00004000;
  VIF_OUTOFMEMORY     : INT32 = 16_00008000;

  VIF_CANNOTREADSRC: INT32 = 16_00010000;
  VIF_CANNOTREADDST: INT32 = 16_00020000;

  VIF_BUFFTOOSMALL: INT32 = 16_00040000;

(* ----- Types and structures ----- *)

TYPE
  VS_FIXEDFILEINFO = RECORD
    dwSignature       : UINT32;  (* e.g.  16_feef04bd *)
    dwStrucVersion    : UINT32;  (* e.g.  16_00000042 = "0.42" *)
    dwFileVersionMS   : UINT32;  (* e.g.  16_00030075 = "3.75" *)
    dwFileVersionLS   : UINT32;  (* e.g.  16_00000031 = "0.31" *)
    dwProductVersionMS: UINT32;  (* e.g.  16_00030010 = "3.10" *)
    dwProductVersionLS: UINT32;  (* e.g.  16_00000031 = "0.31" *)
    dwFileFlagsMask   : UINT32;  (* = 16_3F for version "0.42" *)
    dwFileFlags       : UINT32;  (* e.g.  VFF_DEBUG | VFF_PRERELEASE *)
    dwFileOS          : UINT32;  (* e.g.  VOS_DOS_WINDOWS16 *)
    dwFileType        : UINT32;  (* e.g.  VFT_DRIVER *)
    dwFileSubtype     : UINT32;  (* e.g.  VFT2_DRV_KEYBOARD *)
    dwFileDateMS      : UINT32;  (* e.g.  0 *)
    dwFileDateLS      : UINT32;  (* e.g.  0 *)
  END;

(* ----- Function prototypes ----- *)

<*EXTERNAL VerFindFileA:APIENTRY*>
PROCEDURE VerFindFileA (uFlags       : UINT32;
                        szFileName   : PSTR;
                        szWinDir     : PSTR;
                        szAppDir     : PSTR;
                        szCurDir     : PSTR;
                        lpuCurDirLen : PUINT32;
                        szDestDir    : PSTR;
                        lpuDestDirLen: PUINT32  ): UINT32;

<*EXTERNAL VerFindFileW:APIENTRY*>
PROCEDURE VerFindFileW (uFlags       : UINT32;
                        szFileName   : PWSTR;
                        szWinDir     : PWSTR;
                        szAppDir     : PWSTR;
                        szCurDir     : PWSTR;
                        lpuCurDirLen : PUINT32;
                        szDestDir    : PWSTR;
                        lpuDestDirLen: PUINT32  ): UINT32;

CONST VerFindFile = VerFindFileA;

<*EXTERNAL VerInstallFileA:APIENTRY*>
PROCEDURE VerInstallFileA (uFlags        : UINT32;
                           szSrcFileName : PSTR;
                           szDestFileName: PSTR;
                           szSrcDir      : PSTR;
                           szDestDir     : PSTR;
                           szCurDir      : PSTR;
                           szTmpFile     : PSTR;
                           lpuTmpFileLen : PUINT32): UINT32;

<*EXTERNAL VerInstallFileW:APIENTRY*>
PROCEDURE VerInstallFileW (uFlags        : UINT32;
                           szSrcFileName : PWSTR;
                           szDestFileName: PWSTR;
                           szSrcDir      : PWSTR;
                           szDestDir     : PWSTR;
                           szCurDir      : PWSTR;
                           szTmpFile     : PWSTR;
                           lpuTmpFileLen : PUINT32 ): UINT32;

CONST VerInstallFile = VerInstallFileA;

(* Returns size of version info in bytes *)
<*EXTERNAL GetFileVersionInfoSizeA:APIENTRY*>
PROCEDURE GetFileVersionInfoSizeA (lptstrFilename: PSTR;  (* Filename of
                                                                version
                                                                stamped
                                                                file *)

                                   lpdwHandle: PUINT32): UINT32;
                        (* Information for use by GetFileVersionInfo *)

(* Returns size of version info in bytes *)
<*EXTERNAL GetFileVersionInfoSizeW:APIENTRY*>
PROCEDURE GetFileVersionInfoSizeW (lptstrFilename: PWSTR;  (* Filename
                                                                 of version
                                                                 stamped
                                                                 file *)

                                     lpdwHandle: PUINT32): UINT32;
       (* Information for use by GetFileVersionInfo *)

CONST GetFileVersionInfoSize = GetFileVersionInfoSizeA;

(* Read version info into buffer *)
<*EXTERNAL GetFileVersionInfoA:APIENTRY*>
PROCEDURE GetFileVersionInfoA (lptstrFilename: PSTR;  (* Filename of
                                                            version stamped
                                                            file *)

                                 dwHandle: UINT32;  (* Information from
                                                      GetFileVersionSize *)
                                 dwLen: UINT32;  (* Length of buffer for
                                                   info *)
                                 lpData: PVOID): BOOL;
  (* Buffer to place the data structure *)

(* Read version info into buffer *)
<*EXTERNAL GetFileVersionInfoW:APIENTRY*>
PROCEDURE GetFileVersionInfoW (lptstrFilename: PWSTR;  (* Filename of
                                                             version
                                                             stamped
                                                             file *)

                               dwHandle: UINT32;  (* Information from
                                                      GetFileVersionSize *)
                               dwLen: UINT32;  (* Length of buffer for
                                                   info *)
                               lpData: PVOID): BOOL;
                         (* Buffer to place the data structure *)

CONST GetFileVersionInfo = GetFileVersionInfoA;

<*EXTERNAL VerLanguageNameA:APIENTRY*>
PROCEDURE VerLanguageNameA (wLang: UINT32; szLang: PSTR; nSize: UINT32): UINT32;

<*EXTERNAL VerLanguageNameW:APIENTRY*>
PROCEDURE VerLanguageNameW (wLang: UINT32; szLang: PWSTR; nSize: UINT32): UINT32;

CONST VerLanguageName = VerLanguageNameA;

<*EXTERNAL VerQueryValueA:APIENTRY*>
PROCEDURE VerQueryValueA (pBlock    : PVOID;
                          lpSubBlock: PSTR;
                          lplpBuffer: UNTRACED REF PVOID;
                          lpuLen    : PUINT32              ): BOOL;

<*EXTERNAL VerQueryValueW:APIENTRY*>
PROCEDURE VerQueryValueW (pBlock    : PVOID;
                          lpSubBlock: PWSTR;
                          lplpBuffer: UNTRACED REF PVOID;
                          lpuLen    : PUINT32              ): BOOL;

CONST VerQueryValue = VerQueryValueA;


END WinVer.
