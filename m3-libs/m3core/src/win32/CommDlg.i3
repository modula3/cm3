(* Copyright (C) 1994, Digital Equipment Corporation          *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(*                                                            *)
(* Last modified on Wed Dec 21 09:15:25 PST 1994 by kalsow    *)
(*                                                            *)
(* Translated from "commdlg.h", Copyright (c) 1992-1993       *)
(*    Microsoft Corporation                                   *)

INTERFACE CommDlg;

FROM Ctypes IMPORT short;

FROM WinDef IMPORT BOOL, WORD, DWORD, UINT, HWND, HDC, WPARAM, LPARAM,
                   HINSTANCE, INT, COLORREF, LPCOLORREF, HGLOBAL;

FROM WinNT   IMPORT LPSTR, LPWSTR, LPCSTR, LPCWSTR;
FROM WinGDI  IMPORT LPLOGFONTA, LPLOGFONTW;
FROM WinUser IMPORT WM_USER;

TYPE
  LPOFNHOOKPROC = <*APIENTRY*> PROCEDURE (a0: HWND;  a1: UINT;
                                        a2: WPARAM; a3: LPARAM): UINT;

TYPE
  LPOPENFILENAMEA = UNTRACED REF OPENFILENAMEA;
  OPENFILENAMEA = RECORD
    lStructSize       : DWORD;
    hwndOwner         : HWND;
    hInstance         : HINSTANCE;
    lpstrFilter       : LPCSTR;
    lpstrCustomFilter : LPSTR;
    nMaxCustFilter    : DWORD;
    nFilterIndex      : DWORD;
    lpstrFile         : LPSTR;
    nMaxFile          : DWORD;
    lpstrFileTitle    : LPSTR;
    nMaxFileTitle     : DWORD;
    lpstrInitialDir   : LPCSTR;
    lpstrTitle        : LPCSTR;
    Flags             : DWORD;
    nFileOffset       : WORD;
    nFileExtension    : WORD;
    lpstrDefExt       : LPCSTR;
    lCustData         : LPARAM;
    lpfnHook          : LPOFNHOOKPROC;
    lpTemplateName    : LPCSTR;
  END;

  LPOPENFILENAMEW = UNTRACED REF OPENFILENAMEW;
  OPENFILENAMEW = RECORD
    lStructSize       : DWORD;
    hwndOwner         : HWND;
    hInstance         : HINSTANCE;
    lpstrFilter       : LPCWSTR;
    lpstrCustomFilter : LPWSTR;
    nMaxCustFilter    : DWORD;
    nFilterIndex      : DWORD;
    lpstrFile         : LPWSTR;
    nMaxFile          : DWORD;
    lpstrFileTitle    : LPWSTR;
    nMaxFileTitle     : DWORD;
    lpstrInitialDir   : LPCWSTR;
    lpstrTitle        : LPCWSTR;
    Flags             : DWORD;
    nFileOffset       : WORD;
    nFileExtension    : WORD;
    lpstrDefExt       : LPCWSTR;
    lCustData         : LPARAM;
    lpfnHook          : LPOFNHOOKPROC;
    lpTemplateName    : LPCWSTR;
  END;

  OPENFILENAME = OPENFILENAMEA;
  LPOPENFILENAME = LPOPENFILENAMEA;

<*EXTERNAL GetOpenFileNameA:APIENTRY*>
PROCEDURE GetOpenFileNameA (a1: LPOPENFILENAMEA): BOOL;

<*EXTERNAL GetOpenFileNameW:APIENTRY*>
PROCEDURE GetOpenFileNameW (a1: LPOPENFILENAMEW): BOOL;

CONST GetOpenFileName = GetOpenFileNameA;

<*EXTERNAL GetSaveFileNameA:APIENTRY*>
PROCEDURE GetSaveFileNameA (a1: LPOPENFILENAMEA): BOOL;

<*EXTERNAL GetSaveFileNameW:APIENTRY*>
PROCEDURE GetSaveFileNameW (a1: LPOPENFILENAMEW): BOOL;

CONST GetSaveFileName = GetSaveFileNameA;

<*EXTERNAL GetFileTitleA:APIENTRY*>
PROCEDURE GetFileTitleA (a1: LPCSTR; a2: LPSTR; a3: WORD): short;

<*EXTERNAL GetFileTitleW:APIENTRY*>
PROCEDURE GetFileTitleW (a1: LPCWSTR; a2: LPWSTR; a3: WORD): short;

CONST GetFileTitle = GetFileTitleA;

CONST
  OFN_READONLY              = 16_00000001;
  OFN_OVERWRITEPROMPT       = 16_00000002;
  OFN_HIDEREADONLY          = 16_00000004;
  OFN_NOCHANGEDIR           = 16_00000008;
  OFN_SHOWHELP              = 16_00000010;
  OFN_ENABLEHOOK            = 16_00000020;
  OFN_ENABLETEMPLATE        = 16_00000040;
  OFN_ENABLETEMPLATEHANDLE  = 16_00000080;
  OFN_NOVALIDATE            = 16_00000100;
  OFN_ALLOWMULTISELECT      = 16_00000200;
  OFN_EXTENSIONDIFFERENT    = 16_00000400;
  OFN_PATHMUSTEXIST         = 16_00000800;
  OFN_FILEMUSTEXIST         = 16_00001000;
  OFN_CREATEPROMPT          = 16_00002000;
  OFN_SHAREAWARE            = 16_00004000;
  OFN_NOREADONLYRETURN      = 16_00008000;
  OFN_NOTESTFILECREATE      = 16_00010000;
  OFN_NONETWORKBUTTON       = 16_00020000;
  OFN_NOLONGNAMES           = 16_00040000;

(*  Return values for the registered message sent to the hook function *)
(*  when a sharing violation occurs.  OFN_SHAREFALLTHROUGH allows the  *)
(*  filename to be accepted, OFN_SHARENOWARN rejects the name but puts *)
(*  up no warning (returned when the app has already put up a warning  *)
(*  message), and OFN_SHAREWARN puts up the default warning message    *)
(*  for sharing violations.                                            *)
(*                                                                     *)
(*  Note:  Undefined return values map to OFN_SHAREWARN, but are       *)
(*         reserved for future use.                                    *)

CONST
  OFN_SHAREFALLTHROUGH = 2;
  OFN_SHARENOWARN      = 1;
  OFN_SHAREWARN        = 0;

TYPE
  LPCCHOOKPROC = <*APIENTRY*> PROCEDURE (a1: HWND;  a2: UINT;
                                         a3: WPARAM;  a4: LPARAM): UINT;

  LPCHOOSECOLORA = UNTRACED REF CHOOSECOLORA;
  CHOOSECOLORA = RECORD
    lStructSize    : DWORD;
    hwndOwner      : HWND;
    hInstance      : HWND;
    rgbResult      : COLORREF;
    lpCustColors   : LPCOLORREF;
    Flags          : DWORD;
    lCustData      : LPARAM;
    lpfnHook       : LPCCHOOKPROC;
    lpTemplateName : LPCSTR;
  END;

  LPCHOOSECOLORW = UNTRACED REF CHOOSECOLORW;
  CHOOSECOLORW = RECORD
    lStructSize    : DWORD;
    hwndOwner      : HWND;
    hInstance      : HWND;
    rgbResult      : COLORREF;
    lpCustColors   : LPCOLORREF;
    Flags          : DWORD;
    lCustData      : LPARAM;
    lpfnHook       : LPCCHOOKPROC;
    lpTemplateName : LPCWSTR;
  END;

  CHOOSECOLOR   = CHOOSECOLORA;
  LPCHOOSECOLOR = LPCHOOSECOLORA;

<*EXTERNAL ChooseColorA:APIENTRY*>
PROCEDURE ChooseColorA (a1: LPCHOOSECOLORA): BOOL;

<*EXTERNAL ChooseColorW:APIENTRY*>
PROCEDURE ChooseColorW (a1: LPCHOOSECOLORW): BOOL;

CONST ChooseColor = ChooseColorA;

CONST
  CC_RGBINIT              = 16_00000001;
  CC_FULLOPEN             = 16_00000002;
  CC_PREVENTFULLOPEN      = 16_00000004;
  CC_SHOWHELP             = 16_00000008;
  CC_ENABLEHOOK           = 16_00000010;
  CC_ENABLETEMPLATE       = 16_00000020;
  CC_ENABLETEMPLATEHANDLE = 16_00000040;

TYPE
  LPFRHOOKPROC = <*APIENTRY*> PROCEDURE (a1: HWND;  a2: UINT;
                                         a3: WPARAM;  a4: LPARAM): UINT;

  LPFINDREPLACEA = UNTRACED REF FINDREPLACEA;
  FINDREPLACEA = RECORD
    lStructSize      : DWORD;        (*  size of this struct 16_20 *)
    hwndOwner        : HWND;         (*  handle to owner's window *)
    hInstance        : HINSTANCE;    (*  instance handle of.EXE that *)
                                     (*    contains cust. dlg. template *)
    Flags            : DWORD;        (*  one or more of the FR_?? *)
    lpstrFindWhat    : LPSTR;        (*  ptr. to search string *)
    lpstrReplaceWith : LPSTR;        (*  ptr. to replace string *)
    wFindWhatLen     : WORD;         (*  size of find buffer *)
    wReplaceWithLen  : WORD;         (*  size of replace buffer *)
    lCustData        : LPARAM;       (*  data passed to hook fn. *)
    lpfnHook         : LPFRHOOKPROC; (*  ptr. to hook fn. or NULL *)
    lpTemplateName   : LPCSTR;       (*  custom template name *)
  END;

  LPFINDREPLACEW = UNTRACED REF FINDREPLACEW;
  FINDREPLACEW = RECORD
    lStructSize      : DWORD;        (*  size of this struct 16_20 *)
    hwndOwner        : HWND;         (*  handle to owner's window *)
    hInstance        : HINSTANCE;    (*  instance handle of.EXE that *)
                                     (*    contains cust. dlg. template *)
    Flags            : DWORD;        (*  one or more of the FR_?? *)
    lpstrFindWhat    : LPWSTR;       (*  ptr. to search string *)
    lpstrReplaceWith : LPWSTR;       (*  ptr. to replace string *)
    wFindWhatLen     : WORD;         (*  size of find buffer *)
    wReplaceWithLen  : WORD;         (*  size of replace buffer *)
    lCustData        : LPARAM;       (*  data passed to hook fn. *)
    lpfnHook         : LPFRHOOKPROC; (*  ptr. to hook fn. or NULL *)
    lpTemplateName   : LPCWSTR;      (*  custom template name *)
  END;

  FINDREPLACE = FINDREPLACEA;
  LPFINDREPLACE = LPFINDREPLACEA;

CONST
  FR_DOWN                 = 16_00000001;
  FR_WHOLEWORD            = 16_00000002;
  FR_MATCHCASE            = 16_00000004;
  FR_FINDNEXT             = 16_00000008;
  FR_REPLACE              = 16_00000010;
  FR_REPLACEALL           = 16_00000020;
  FR_DIALOGTERM           = 16_00000040;
  FR_SHOWHELP             = 16_00000080;
  FR_ENABLEHOOK           = 16_00000100;
  FR_ENABLETEMPLATE       = 16_00000200;
  FR_NOUPDOWN             = 16_00000400;
  FR_NOMATCHCASE          = 16_00000800;
  FR_NOWHOLEWORD          = 16_00001000;
  FR_ENABLETEMPLATEHANDLE = 16_00002000;
  FR_HIDEUPDOWN           = 16_00004000;
  FR_HIDEMATCHCASE        = 16_00008000;
  FR_HIDEWHOLEWORD        = 16_00010000;

<*EXTERNAL FindTextA:APIENTRY*>
PROCEDURE FindTextA (a1: LPFINDREPLACEA): HWND;

<*EXTERNAL FindTextW:APIENTRY*>
PROCEDURE FindTextW (a1: LPFINDREPLACEW): HWND;

CONST FindText = FindTextA;

<*EXTERNAL ReplaceTextA:APIENTRY*>
PROCEDURE ReplaceTextA (a1: LPFINDREPLACEA): HWND;

<*EXTERNAL ReplaceTextW:APIENTRY*>
PROCEDURE ReplaceTextW (a1: LPFINDREPLACEW): HWND;

CONST ReplaceText = ReplaceTextA;

TYPE
  LPCFHOOKPROC = <*APIENTRY*> PROCEDURE (a1: HWND;  a2: UINT;
                                         a3: WPARAM;  a4: LPARAM): UINT;

  LPCHOOSEFONTA = UNTRACED REF CHOOSEFONTA;
  CHOOSEFONTA = RECORD
    lStructSize    : DWORD;
    hwndOwner      : HWND;         (*  caller's window handle *)
    hDC            : HDC;          (*  printer DC/IC or NULL *)
    lpLogFont      : LPLOGFONTA;   (*  ptr. to a LOGFONT struct *)
    iPointSize     : INT;          (*  10 * size in points of selected font *)
    Flags          : DWORD;        (*  enum. type flags *)
    rgbColors      : COLORREF;     (*  returned text color *)
    lCustData      : LPARAM;       (*  data passed to hook fn. *)
    lpfnHook       : LPCFHOOKPROC; (*  ptr. to hook function *)
    lpTemplateName : LPCSTR;       (*  custom template name *)
    hInstance      : HINSTANCE;    (*  instance handle of.EXE that *)
                                   (*    contains cust. dlg. template *)
    lpszStyle      : LPSTR;        (*  return the style field here *)
                                   (*  must be LF_FACESIZE or bigger *)
    nFontType      : WORD;         (*  same value reported to the EnumFonts *)
                                   (*    call back with the extra FONTTYPE_ *)
                                   (*    bits added *)
(*___*)MISSING_ALIGNMENT__ : WORD;
    nSizeMin       : INT;          (*  minimum pt size allowed & *)
    nSizeMax       : INT;          (*  max pt size allowed if *)
                                   (*    CF_LIMITSIZE is used *)
  END;

  LPCHOOSEFONTW = UNTRACED REF CHOOSEFONTW;
  CHOOSEFONTW = RECORD
    lStructSize    : DWORD;
    hwndOwner      : HWND;         (*  caller's window handle *)
    hDC            : HDC;          (*  printer DC/IC or NULL *)
    lpLogFont      : LPLOGFONTW;   (*  ptr. to a LOGFONT struct *)
    iPointSize     : INT;          (*  10 * size in points of selected font *)
    Flags          : DWORD;        (*  enum. type flags *)
    rgbColors      : COLORREF;     (*  returned text color *)
    lCustData      : LPARAM;       (*  data passed to hook fn. *)
    lpfnHook       : LPCFHOOKPROC; (*  ptr. to hook function *)
    lpTemplateName : LPCWSTR;      (*  custom template name *)
    hInstance      : HINSTANCE;    (*  instance handle of.EXE that *)
                                   (*  contains cust. dlg. template *)
    lpszStyle      : LPWSTR;       (*  return the style field here *)
                                   (*  must be LF_FACESIZE or bigger *)
    nFontType      : WORD;         (*  same value reported to the EnumFonts *)
                                   (*    call back with the extra FONTTYPE_ *)
                                   (*    bits added *)
(*___*)MISSING_ALIGNMENT__ : WORD;
    nSizeMin       : INT;          (*  minimum pt size allowed & *)
    nSizeMax       : INT;          (*  max pt size allowed if *)
                                   (*    CF_LIMITSIZE is used *)
  END;

  CHOOSEFONT = CHOOSEFONTA;
  LPCHOOSEFONT = LPCHOOSEFONTA;

<*EXTERNAL ChooseFontA:APIENTRY*>
PROCEDURE ChooseFontA (a1: LPCHOOSEFONTA): BOOL;

<*EXTERNAL ChooseFontW:APIENTRY*>
PROCEDURE ChooseFontW (a1: LPCHOOSEFONTW): BOOL;

CONST ChooseFont = ChooseFontA;

CONST
  CF_SCREENFONTS          = 16_00000001;
  CF_PRINTERFONTS         = 16_00000002;
  CF_BOTH                 = CF_SCREENFONTS + CF_PRINTERFONTS;
  CF_SHOWHELP             = 16_00000004;
  CF_ENABLEHOOK           = 16_00000008;
  CF_ENABLETEMPLATE       = 16_00000010;
  CF_ENABLETEMPLATEHANDLE = 16_00000020;
  CF_INITTOLOGFONTSTRUCT  = 16_00000040;
  CF_USESTYLE             = 16_00000080;
  CF_EFFECTS              = 16_00000100;
  CF_APPLY                = 16_00000200;
  CF_ANSIONLY             = 16_00000400;
  CF_NOVECTORFONTS        = 16_00000800;
  CF_NOOEMFONTS           = CF_NOVECTORFONTS;
  CF_NOSIMULATIONS        = 16_00001000;
  CF_LIMITSIZE            = 16_00002000;
  CF_FIXEDPITCHONLY       = 16_00004000;
  CF_WYSIWYG              = 16_00008000; (* must also have CF_SCREENFONTS
                                               & CF_PRINTERFONTS *)
  CF_FORCEFONTEXIST       = 16_00010000;
  CF_SCALABLEONLY         = 16_00020000;
  CF_TTONLY               = 16_00040000;
  CF_NOFACESEL            = 16_00080000;
  CF_NOSTYLESEL           = 16_00100000;
  CF_NOSIZESEL            = 16_00200000;

(*  these are extra nFontType bits that are added to what is returned to the *)
(*  EnumFonts callback routine *)

CONST
  SIMULATED_FONTTYPE    = 16_8000;
  PRINTER_FONTTYPE      = 16_4000;
  SCREEN_FONTTYPE       = 16_2000;
  BOLD_FONTTYPE         = 16_0100;
  ITALIC_FONTTYPE       = 16_0200;
  REGULAR_FONTTYPE      = 16_0400;

  WM_CHOOSEFONT_GETLOGFONT = (WM_USER + 1);

(*  strings used to obtain unique window message for communication *)
(*  between dialog and caller *)

CONST
  LBSELCHSTRINGA  = "commdlg_LBSelChangedNotify";
  SHAREVISTRINGA  = "commdlg_ShareViolation";
  FILEOKSTRINGA   = "commdlg_FileNameOK";
  COLOROKSTRINGA  = "commdlg_ColorOK";
  SETRGBSTRINGA   = "commdlg_SetRGBColor";
  HELPMSGSTRINGA  = "commdlg_help";
  FINDMSGSTRINGA  = "commdlg_FindReplace";

(**
  LBSELCHSTRINGW  = L"commdlg_LBSelChangedNotify";
  SHAREVISTRINGW  = L"commdlg_ShareViolation";
  FILEOKSTRINGW   = L"commdlg_FileNameOK";
  COLOROKSTRINGW  = L"commdlg_ColorOK";
  SETRGBSTRINGW   = L"commdlg_SetRGBColor";
  HELPMSGSTRINGW  = L"commdlg_help";
  FINDMSGSTRINGW  = L"commdlg_FindReplace";
**)

  LBSELCHSTRING  = LBSELCHSTRINGA;
  SHAREVISTRING  = SHAREVISTRINGA;
  FILEOKSTRING   = FILEOKSTRINGA;
  COLOROKSTRING  = COLOROKSTRINGA;
  SETRGBSTRING   = SETRGBSTRINGA;
  HELPMSGSTRING  = HELPMSGSTRINGA;
  FINDMSGSTRING  = FINDMSGSTRINGA;

(*  HIWORD values for lParam of commdlg_LBSelChangeNotify message *)

CONST
  CD_LBSELNOITEMS = -1;
  CD_LBSELCHANGE   = 0;
  CD_LBSELSUB      = 1;
  CD_LBSELADD      = 2;


TYPE
  LPPRINTHOOKPROC = <*APIENTRY*> PROCEDURE (a1: HWND;  a2: UINT;
                                           a3: WPARAM;  a4: LPARAM): UINT;

  LPSETUPHOOKPROC = <*APIENTRY*> PROCEDURE (a1: HWND;  a2: UINT;
                                            a3: WPARAM;  a4: LPARAM): UINT;

TYPE
  LPPRINTDLGA = UNTRACED REF PRINTDLGA;
  PRINTDLGA = RECORD
    lStructSize         : DWORD;
    hwndOwner           : HWND;
    hDevMode            : HGLOBAL;
    hDevNames           : HGLOBAL;
    hDC                 : HDC;
    Flags               : DWORD;
    nFromPage           : WORD;
    nToPage             : WORD;
    nMinPage            : WORD;
    nMaxPage            : WORD;
    nCopies             : WORD;
    hInstance           : HINSTANCE;
    lCustData           : LPARAM;
    lpfnPrintHook       : LPPRINTHOOKPROC;
    lpfnSetupHook       : LPSETUPHOOKPROC;
    lpPrintTemplateName : LPCSTR;
    lpSetupTemplateName : LPCSTR;
    hPrintTemplate      : HGLOBAL;
    hSetupTemplate      : HGLOBAL;
  END;

  LPPRINTDLGW = UNTRACED REF PRINTDLGW;
  PRINTDLGW = RECORD
    lStructSize         : DWORD;
    hwndOwner           : HWND;
    hDevMode            : HGLOBAL;
    hDevNames           : HGLOBAL;
    hDC                 : HDC;
    Flags               : DWORD;
    nFromPage           : WORD;
    nToPage             : WORD;
    nMinPage            : WORD;
    nMaxPage            : WORD;
    nCopies             : WORD;
    hInstance           : HINSTANCE;
    lCustData           : LPARAM;
    lpfnPrintHook       : LPPRINTHOOKPROC;
    lpfnSetupHook       : LPSETUPHOOKPROC;
    lpPrintTemplateName : LPCWSTR;
    lpSetupTemplateName : LPCWSTR;
    hPrintTemplate      : HGLOBAL;
    hSetupTemplate      : HGLOBAL;
  END;

  PRINTDLG   = PRINTDLGA;
  LPPRINTDLG = LPPRINTDLGA;

<*EXTERNAL PrintDlgA:APIENTRY*>
PROCEDURE PrintDlgA (a1: LPPRINTDLGA): BOOL;

<*EXTERNAL PrintDlgW:APIENTRY*>
PROCEDURE PrintDlgW (a1: LPPRINTDLGW): BOOL;

CONST PrintDlg = PrintDlgA;

CONST
  PD_ALLPAGES                  = 16_00000000;
  PD_SELECTION                 = 16_00000001;
  PD_PAGENUMS                  = 16_00000002;
  PD_NOSELECTION               = 16_00000004;
  PD_NOPAGENUMS                = 16_00000008;
  PD_COLLATE                   = 16_00000010;
  PD_PRINTTOFILE               = 16_00000020;
  PD_PRINTSETUP                = 16_00000040;
  PD_NOWARNING                 = 16_00000080;
  PD_RETURNDC                  = 16_00000100;
  PD_RETURNIC                  = 16_00000200;
  PD_RETURNDEFAULT             = 16_00000400;
  PD_SHOWHELP                  = 16_00000800;
  PD_ENABLEPRINTHOOK           = 16_00001000;
  PD_ENABLESETUPHOOK           = 16_00002000;
  PD_ENABLEPRINTTEMPLATE       = 16_00004000;
  PD_ENABLESETUPTEMPLATE       = 16_00008000;
  PD_ENABLEPRINTTEMPLATEHANDLE = 16_00010000;
  PD_ENABLESETUPTEMPLATEHANDLE = 16_00020000;
  PD_USEDEVMODECOPIES          = 16_00040000;
  PD_DISABLEPRINTTOFILE        = 16_00080000;
  PD_HIDEPRINTTOFILE           = 16_00100000;
  PD_NONETWORKBUTTON           = 16_00200000;

TYPE
  LPDEFNAMES = UNTRACED REF DEVNAMES;
  DEVNAMES = RECORD
    wDriverOffset : WORD;
    wDeviceOffset : WORD;
    wOutputOffset : WORD;
    wDefault      : WORD;
  END;

CONST DN_DEFAULTPRN = 16_0001;

<*EXTERNAL CommDlgExtendedError:APIENTRY*>
PROCEDURE CommDlgExtendedError (): DWORD;

END CommDlg.
