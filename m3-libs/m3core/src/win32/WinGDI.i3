(* Copyright (C) 1994, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* by Stephen Harrison                                       *)
(*                                                           *)
(* Last modified on Tue Oct  3 10:32:06 PDT 1995 by najork   *)
(*      modified on Wed Apr 12 15:00:57 PDT 1995 by kalsow   *)
(*      modified on Tue May 18 09:59:19 PDT 1993 by steveg   *)
(*      modified on Tue Mar 23 17:28:13 PST 1993 by harrison *)

(* This file corresponds to build version 0004 of "wingdi.h"
   See that file for details.

   Procedure declarations, constant definitions and macros for the GDI
   component. *)

INTERFACE WinGDI;

IMPORT Ctypes;

FROM Word IMPORT Or, Shift;
FROM WinDef IMPORT SHORT, BOOL, WORD, DWORD, UINT, LPDWORD, LPVOID, HDC,
                   LPARAM, POINT, HBRUSH, RECT, BYTE, LPBYTE, HBITMAP,
                   LPRECT, LPINT, HRGN, LPPOINT, INT, COLORREF, USHORT,
                   WFLOAT, HGDIOBJ, HMETAFILE, HMODULE, HFONT, HPEN,
                   HPALETTE, HGLOBAL, RECTL, SIZEL, PFLOAT, LPSIZE,
                   HENHMETAFILE, HGLRC;
FROM WinNT IMPORT WCHAR, LPSTR, LPWSTR, LPCSTR, LPCWSTR, LONG, PSTR;
FROM Ctypes IMPORT int, char, void_star, short;

(* Binary raster ops *)
CONST
  R2_BLACK       = 1;           (* 0 *)
  R2_NOTMERGEPEN = 2;           (* DPon *)
  R2_MASKNOTPEN  = 3;           (* DPna *)
  R2_NOTCOPYPEN  = 4;           (* PN *)
  R2_MASKPENNOT  = 5;           (* PDna *)
  R2_NOT         = 6;           (* Dn *)
  R2_XORPEN      = 7;           (* DPx *)
  R2_NOTMASKPEN  = 8;           (* DPan *)
  R2_MASKPEN     = 9;           (* DPa *)
  R2_NOTXORPEN   = 10;          (* DPxn *)
  R2_NOP         = 11;          (* D *)
  R2_MERGENOTPEN = 12;          (* DPno *)
  R2_COPYPEN     = 13;          (* P *)
  R2_MERGEPENNOT = 14;          (* PDno *)
  R2_MERGEPEN    = 15;          (* DPo *)
  R2_WHITE       = 16;          (* 1 *)
  R2_LAST        = 16;

(* Ternary raster operations *)
CONST
  SRCCOPY    : DWORD = 16_00CC0020; (* dest = source *)
  SRCPAINT   : DWORD = 16_00EE0086; (* dest = source OR dest *)
  SRCAND     : DWORD = 16_008800C6; (* dest = source AND dest *)
  SRCINVERT  : DWORD = 16_00660046; (* dest = source XOR dest *)
  SRCERASE   : DWORD = 16_00440328; (* dest = source AND (NOT dest ) *)
  NOTSRCCOPY : DWORD = 16_00330008; (* dest = (NOT source) *)
  NOTSRCERASE: DWORD = 16_001100A6; (* dest = (NOT src) AND (NOT dest) *)
  MERGECOPY  : DWORD = 16_00C000CA; (* dest = (source AND pattern) *)
  MERGEPAINT : DWORD = 16_00BB0226; (* dest = (NOT source) OR dest *)
  PATCOPY    : DWORD = 16_00F00021; (* dest = pattern *)
  PATPAINT   : DWORD = 16_00FB0A09; (* dest = DPSnoo *)
  PATINVERT  : DWORD = 16_005A0049; (* dest = pattern XOR dest *)
  DSTINVERT  : DWORD = 16_00550009; (* dest = (NOT dest) *)
  BLACKNESS  : DWORD = 16_00000042; (* dest = BLACK *)
  WHITENESS  : DWORD = 16_00FF0062; (* dest = WHITE *)

CONST GDI_ERROR: LONG = 16_FFFFFFFF;

(* Region Flags *)
CONST
  ERROR         = 0;
  NULLREGION    = 1;
  SIMPLEREGION  = 2;
  COMPLEXREGION = 3;
  RGN_ERROR     = ERROR;

(* CombineRgn() Styles *)
CONST
  RGN_AND  = 1;
  RGN_OR   = 2;
  RGN_XOR  = 3;
  RGN_DIFF = 4;
  RGN_COPY = 5;
  RGN_MIN  = RGN_AND;
  RGN_MAX  = RGN_COPY;

(* StretchBlt() Modes *)
CONST
  BLACKONWHITE      = 1;
  WHITEONBLACK      = 2;
  COLORONCOLOR      = 3;
  HALFTONE          = 4;
  MAXSTRETCHBLTMODE = 4;

(* PolyFill() Modes *)
CONST
  ALTERNATE     = 1;
  WINDING       = 2;
  POLYFILL_LAST = 2;

(* Text Alignment Options *)
CONST
  TA_NOUPDATECP = 0;
  TA_UPDATECP   = 1;

  TA_LEFT   = 0;
  TA_RIGHT  = 2;
  TA_CENTER = 6;

  TA_TOP      = 0;
  TA_BOTTOM   = 8;
  TA_BASELINE = 24;
  TA_MASK     = (TA_BASELINE + TA_CENTER + TA_UPDATECP);

  VTA_BASELINE = TA_BASELINE;
  VTA_LEFT     = TA_BOTTOM;
  VTA_RIGHT    = TA_TOP;
  VTA_CENTER   = TA_CENTER;
  VTA_BOTTOM   = TA_RIGHT;
  VTA_TOP      = TA_LEFT;

  ETO_GRAYED  = 1;
  ETO_OPAQUE  = 2;
  ETO_CLIPPED = 4;

  ASPECT_FILTERING = 16_0001;

(* Bounds Accumulation APIs *)
CONST
  DCB_RESET      = 16_0001;
  DCB_ACCUMULATE = 16_0002;
  DCB_DIRTY      = DCB_ACCUMULATE;
  DCB_SET        = Or(DCB_RESET, DCB_ACCUMULATE);
  DCB_ENABLE     = 16_0004;
  DCB_DISABLE    = 16_0008;

(* Metafile Functions *)
CONST
  META_SETBKCOLOR           = 16_0201;
  META_SETBKMODE            = 16_0102;
  META_SETMAPMODE           = 16_0103;
  META_SETROP2              = 16_0104;
  META_SETRELABS            = 16_0105;
  META_SETPOLYFILLMODE      = 16_0106;
  META_SETSTRETCHBLTMODE    = 16_0107;
  META_SETTEXTCHAREXTRA     = 16_0108;
  META_SETTEXTCOLOR         = 16_0209;
  META_SETTEXTJUSTIFICATION = 16_020A;
  META_SETWINDOWORG         = 16_020B;
  META_SETWINDOWEXT         = 16_020C;
  META_SETVIEWPORTORG       = 16_020D;
  META_SETVIEWPORTEXT       = 16_020E;
  META_OFFSETWINDOWORG      = 16_020F;
  META_SCALEWINDOWEXT       = 16_0410;
  META_OFFSETVIEWPORTORG    = 16_0211;
  META_SCALEVIEWPORTEXT     = 16_0412;
  META_LINETO               = 16_0213;
  META_MOVETO               = 16_0214;
  META_EXCLUDECLIPRECT      = 16_0415;
  META_INTERSECTCLIPRECT    = 16_0416;
  META_ARC                  = 16_0817;
  META_ELLIPSE              = 16_0418;
  META_FLOODFILL            = 16_0419;
  META_PIE                  = 16_081A;
  META_RECTANGLE            = 16_041B;
  META_ROUNDRECT            = 16_061C;
  META_PATBLT               = 16_061D;
  META_SAVEDC               = 16_001E;
  META_SETPIXEL             = 16_041F;
  META_OFFSETCLIPRGN        = 16_0220;
  META_TEXTOUT              = 16_0521;
  META_BITBLT               = 16_0922;
  META_STRETCHBLT           = 16_0B23;
  META_POLYGON              = 16_0324;
  META_POLYLINE             = 16_0325;
  META_ESCAPE               = 16_0626;
  META_RESTOREDC            = 16_0127;
  META_FILLREGION           = 16_0228;
  META_FRAMEREGION          = 16_0429;
  META_INVERTREGION         = 16_012A;
  META_PAINTREGION          = 16_012B;
  META_SELECTCLIPREGION     = 16_012C;
  META_SELECTOBJECT         = 16_012D;
  META_SETTEXTALIGN         = 16_012E;
  META_DRAWTEXT             = 16_062F;

  META_CHORD          = 16_0830;
  META_SETMAPPERFLAGS = 16_0231;
  META_EXTTEXTOUT     = 16_0a32;
  META_SETDIBTODEV    = 16_0d33;
  META_SELECTPALETTE  = 16_0234;
  META_REALIZEPALETTE = 16_0035;
  META_ANIMATEPALETTE = 16_0436;
  META_SETPALENTRIES  = 16_0037;
  META_POLYPOLYGON    = 16_0538;
  META_RESIZEPALETTE  = 16_0139;

  META_DIBBITBLT             = 16_0940;
  META_DIBSTRETCHBLT         = 16_0b41;
  META_DIBCREATEPATTERNBRUSH = 16_0142;
  META_STRETCHDIB            = 16_0f43;

  META_EXTFLOODFILL = 16_0548;

  (* #define META_RESETDC 16_014C *)
  (* #define META_STARTDOC 16_014D *)
  (* #define META_STARTPAGE 16_004F *)
  (* #define META_ENDPAGE 16_0050 *)
  (* #define META_ABORTDOC 16_0052 *)
  (* #define META_ENDDOC 16_004E *)

  META_DELETEOBJECT = 16_01f0;

  META_CREATEPALETTE = 16_00f7;
  (* #define META_CREATEBRUSH 16_00F8 *)
  META_CREATEPATTERNBRUSH  = 16_01F9;
  META_CREATEPENINDIRECT   = 16_02FA;
  META_CREATEFONTINDIRECT  = 16_02FB;
  META_CREATEBRUSHINDIRECT = 16_02FC;
  (* #define META_CREATEBITMAPINDIRECT 16_02FD *)
  (* #define META_CREATEBITMAP 16_06FE *)
  META_CREATEREGION = 16_06FF;

(* GDI Escapes *)
CONST
  NEWFRAME           = 1;
  ABORTDOC           = 2;
  NEXTBAND           = 3;
  SETCOLORTABLE      = 4;
  GETCOLORTABLE      = 5;
  FLUSHOUTPUT        = 6;
  DRAFTMODE          = 7;
  QUERYESCSUPPORT    = 8;
  SETABORTPROC       = 9;
  STARTDOC           = 10;
  ENDDOC             = 11;
  GETPHYSPAGESIZE    = 12;
  GETPRINTINGOFFSET  = 13;
  GETSCALINGFACTOR   = 14;
  MFCOMMENT          = 15;
  GETPENWIDTH        = 16;
  SETCOPYCOUNT       = 17;
  SELECTPAPERSOURCE  = 18;
  DEVICEDATA         = 19;
  PASSTHROUGH        = 19;
  GETTECHNOLGY       = 20;
  GETTECHNOLOGY      = 20;
  SETENDCAP          = 21;
  SETLINEJOIN        = 22;
  SETMITERLIMIT      = 23;
  BANDINFO           = 24;
  DRAWPATTERNRECT    = 25;
  GETVECTORPENSIZE   = 26;
  GETVECTORBRUSHSIZE = 27;
  ENABLEDUPLEX       = 28;
  GETSETPAPERBINS    = 29;
  GETSETPRINTORIENT  = 30;
  ENUMPAPERBINS      = 31;
  SETDIBSCALING      = 32;
  EPSPRINTING        = 33;
  ENUMPAPERMETRICS   = 34;
  GETSETPAPERMETRICS = 35;
  POSTSCRIPT_DATA    = 37;
  POSTSCRIPT_IGNORE  = 38;
  MOUSETRAILS        = 39;

  GETEXTENDEDTEXTMETRICS = 256;
  GETEXTENTTABLE         = 257;
  GETPAIRKERNTABLE       = 258;
  GETTRACKKERNTABLE      = 259;
  EXTTEXTOUT             = 512;
  ENABLERELATIVEWIDTHS   = 768;
  ENABLEPAIRKERNING      = 769;
  SETKERNTRACK           = 770;
  SETALLJUSTVALUES       = 771;
  SETCHARSET             = 772;

  STRETCHBLT           = 2048;
  GETSETSCREENPARAMS   = 3072;
  BEGIN_PATH           = 4096;
  CLIP_TO_PATH         = 4097;
  END_PATH             = 4098;
  EXT_DEVICE_CAPS      = 4099;
  RESTORE_CTM          = 4100;
  SAVE_CTM             = 4101;
  SET_ARC_DIRECTION    = 4102;
  SET_BACKGROUND_COLOR = 4103;
  SET_POLY_MODE        = 4104;
  SET_SCREEN_ANGLE     = 4105;
  SET_SPREAD           = 4106;
  TRANSFORM_CTM        = 4107;
  SET_CLIP_BOX         = 4108;
  SET_BOUNDS           = 4109;
  SET_MIRROR_MODE      = 4110;

(* Spooler Error Codes *)
CONST
  SP_NOTREPORTED = 16_4000;
  SP_ERROR       = (-1);
  SP_APPABORT    = (-2);
  SP_USERABORT   = (-3);
  SP_OUTOFDISK   = (-4);
  SP_OUTOFMEMORY = (-5);

  PR_JOBSTATUS = 16_0000;

(* Object Definitions for EnumObjects() *)
CONST
  OBJ_PEN         = 1;
  OBJ_BRUSH       = 2;
  OBJ_DC          = 3;
  OBJ_METADC      = 4;
  OBJ_PAL         = 5;
  OBJ_FONT        = 6;
  OBJ_BITMAP      = 7;
  OBJ_REGION      = 8;
  OBJ_METAFILE    = 9;
  OBJ_MEMDC       = 10;
  OBJ_EXTPEN      = 11;
  OBJ_ENHMETADC   = 12;
  OBJ_ENHMETAFILE = 13;

(* xform stuff *)
CONST
  MWT_IDENTITY      = 1;
  MWT_LEFTMULTIPLY  = 2;
  MWT_RIGHTMULTIPLY = 3;

  MWT_MIN = MWT_IDENTITY;
  MWT_MAX = MWT_RIGHTMULTIPLY;

TYPE
  PXFORM = UNTRACED REF XFORM;
  LPXFORM = UNTRACED REF XFORM;
  XFORM = RECORD
    eM11: WFLOAT;
    eM12: WFLOAT;
    eM21: WFLOAT;
    eM22: WFLOAT;
    eDx : WFLOAT;
    eDy : WFLOAT;
  END;

  (* Bitmap Header Definition *)
  PBITMAP = UNTRACED REF BITMAP;
  LPBITMAP = UNTRACED REF BITMAP;
  BITMAP = RECORD
    bmType      : LONG;
    bmWidth     : LONG;
    bmHeight    : LONG;
    bmWidthBytes: LONG;
    bmPlanes    : WORD;
    bmBitsPixel : WORD;
    bmBits      : LPVOID;
  END;

  RGBTRIPLE = RECORD
    rgbtBlue : BYTE;
    rgbtGreen: BYTE;
    rgbtRed  : BYTE;
  END;

  RGBQUAD = RECORD
    rgbBlue    : BYTE;
    rgbGreen   : BYTE;
    rgbRed     : BYTE;
    rgbReserved: BYTE;
  END;

  (* structures for defining DIBs *)
  PBITMAPCOREHEADER = UNTRACED REF PBITMAPCOREHEADER;
  LPBITMAPCOREHEADER = UNTRACED REF PBITMAPCOREHEADER;
  BITMAPCOREHEADER = RECORD
    bcSize    : DWORD;  (* used to get to color table *)
    bcWidth   : WORD;
    bcHeight  : WORD;
    bcPlanes  : WORD;
    bcBitCount: WORD;
  END;

  PBITMAPINFOHEADER = UNTRACED REF BITMAPINFOHEADER;
  LPBITMAPINFOHEADER = UNTRACED REF BITMAPINFOHEADER;
  BITMAPINFOHEADER = RECORD
    biSize         : DWORD;
    biWidth        : LONG;
    biHeight       : LONG;
    biPlanes       : WORD;
    biBitCount     : WORD;
    biCompression  : DWORD;
    biSizeImage    : DWORD;
    biXPelsPerMeter: LONG;
    biYPelsPerMeter: LONG;
    biClrUsed      : DWORD;
    biClrImportant : DWORD;
  END;

(* constants for the biCompression field *)
CONST
  BI_RGB      : LONG = 0;
  BI_RLE8     : LONG = 1;
  BI_RLE4     : LONG = 2;
  BI_BITFIELDS: LONG = 3;
  BI_TOPDOWN  : LONG = 4;   (*!!!  This should be deleted, I only leave *)
  (*!!!  it temp till Insignia changes there code !!!  remove by 10/1/92
   [patrickh] *)

TYPE
  PBITMAPINFO = UNTRACED REF BITMAPINFO;
  LPBITMAPINFO = UNTRACED REF BITMAPINFO;
  BITMAPINFO = RECORD
    bmiHeader: BITMAPINFOHEADER;
    bmiColors: ARRAY [0 .. 1 - 1] OF RGBQUAD;
  END;

  PBITMAPCOREINFO = UNTRACED REF BITMAPCOREINFO;
  LPBITMAPCOREINFO = UNTRACED REF BITMAPCOREINFO;
  BITMAPCOREINFO = RECORD
    bmciHeader: BITMAPCOREHEADER;
    bmciColors: ARRAY [0 .. 1 - 1] OF RGBTRIPLE;
  END;

  (*???  #pragma pack(2) *)
  PBITMAPFILEHEADER = UNTRACED REF BITMAPFILEHEADER;
  LPBITMAPFILEHEADER = UNTRACED REF BITMAPFILEHEADER;
  BITMAPFILEHEADER = RECORD
    bfType     : WORD;
    bfSize     : DWORD;
    bfReserved1: WORD;
    bfReserved2: WORD;
    bfOffBits  : DWORD;
  END;
  (*???  #pragma pack() *)

(*!  ???  #define MAKEPOINTS(l) (*((POINTS FAR *)&(l))) *)

(* Clipboard Metafile Picture Structure *)
TYPE
  HANDLETABLE = RECORD objectHandle: ARRAY [0 .. 1 - 1] OF HGDIOBJ;  END;
  PHANDLETABLE = UNTRACED REF HANDLETABLE;
  LPHANDLETABLE = UNTRACED REF HANDLETABLE;

  PMETARECORD = UNTRACED REF METARECORD;
  LPMETARECORD = UNTRACED REF METARECORD;
  METARECORD = RECORD
    rdSize    : DWORD;
    rdFunction: WORD;
    rdParm    : ARRAY [0 .. 1 - 1] OF WORD;
  END;

  LPMETAFILEPICT = UNTRACED REF METAFILEPICT;
  METAFILEPICT = RECORD
    mm  : LONG;
    xExt: LONG;
    yExt: LONG;
    hMF : HMETAFILE;
  END;

  (*???  #pragma pack(2) *)
  PMETAHEADER = UNTRACED REF METAHEADER;
  LPMETAHEADER = UNTRACED REF METAHEADER;
  METAHEADER = RECORD
    mtType        : WORD;
    mtHeaderSize  : WORD;
    mtVersion     : WORD;
    mtSize        : DWORD;
    mtNoObjects   : WORD;
    mtMaxRecord   : DWORD;
    mtNoParameters: WORD;
  END;

  (*???  #pragma pack() *)

  (* Enhanced Metafile structures *)
  PENHMETARECORD = UNTRACED REF ENHMETARECORD;
  LPENHMETARECORD = UNTRACED REF ENHMETARECORD;
  ENHMETARECORD = RECORD
    iType: DWORD;  (* Record type MR_XXX *)
    nSize: DWORD;  (* Record size in bytes *)
    dParm: ARRAY [0 .. 1 - 1] OF DWORD;  (* Parameters *)
  END;

  PENHMETAHEADER = UNTRACED REF ENHMETAHEADER;
  LPENHMETAHEADER = UNTRACED REF ENHMETAHEADER;
  ENHMETAHEADER =
    RECORD
    iType: DWORD;             (* Record type MR_METAFILE *)
    nSize: DWORD;             (* Record size in bytes.  This may be
                                   greater *)
      (* than the sizeof(ENHMETAHEADER). *)
    rclBounds: RECTL;         (* Inclusive-inclusive bounds in device
                                   units *)
    rclFrame: RECTL;          (* Inclusive-inclusive Picture Frame of
                                   metafile in .01 mm units *)
    dSignature: DWORD;        (* Signature.  Must be
                                   ENHMETA_SIGNATURE. *)
    nVersion: DWORD;          (* Version number *)
    nBytes  : DWORD;          (* Size of the metafile in bytes *)
    nRecords: DWORD;          (* Number of records in the metafile *)
    nHandles: WORD;           (* Number of handles in the handle table *)
      (* Handle index zero is reserved. *)
    sReserved: WORD;          (* Reserved.  Must be zero. *)
    nDescription: DWORD;      (* Number of chars in the unicode
                                   description string *)
      (* This is 0 if there is no description string *)
    offDescription: DWORD;    (* Offset to the metafile description
                                   record. *)
      (* This is 0 if there is no description string *)
    nPalEntries: DWORD;       (* Number of entries in the metafile
                                   palette. *)
    szlDevice: SIZEL;         (* Size of the reference device in pels *)
    szlMillimeters: SIZEL;    (* Size of the reference device in
                                   millimeters *)
  END;

(* tmPitchAntFamily flags *)
CONST
  TMPF_FIXED_PITCH = 16_01;
  TMPF_VECTOR      = 16_02;
  TMPF_DEVICE      = 16_08;
  TMPF_TRUETYPE    = 16_04;

TYPE
  PTEXTMETRICA = UNTRACED REF TEXTMETRICA;
  NPTEXTMETRICA = UNTRACED REF TEXTMETRICA;
  LPTEXTMETRICA = UNTRACED REF TEXTMETRICA;
  TEXTMETRICA = RECORD
    tmHeight          : LONG;
    tmAscent          : LONG;
    tmDescent         : LONG;
    tmInternalLeading : LONG;
    tmExternalLeading : LONG;
    tmAveCharWidth    : LONG;
    tmMaxCharWidth    : LONG;
    tmWeight          : LONG;
    tmOverhang        : LONG;
    tmDigitizedAspectX: LONG;
    tmDigitizedAspectY: LONG;
    tmFirstChar       : BYTE;
    tmLastChar        : BYTE;
    tmDefaultChar     : BYTE;
    tmBreakChar       : BYTE;
    tmItalic          : BYTE;
    tmUnderlined      : BYTE;
    tmStruckOut       : BYTE;
    tmPitchAndFamily  : BYTE;
    tmCharSet         : BYTE;
  END;

  PTEXTMETRICW = UNTRACED REF TEXTMETRICW;
  NPTEXTMETRICW = UNTRACED REF TEXTMETRICW;
  LPTEXTMETRICW = UNTRACED REF TEXTMETRICW;
  TEXTMETRICW = RECORD
    tmHeight          : LONG;
    tmAscent          : LONG;
    tmDescent         : LONG;
    tmInternalLeading : LONG;
    tmExternalLeading : LONG;
    tmAveCharWidth    : LONG;
    tmMaxCharWidth    : LONG;
    tmWeight          : LONG;
    tmOverhang        : LONG;
    tmDigitizedAspectX: LONG;
    tmDigitizedAspectY: LONG;
    tmFirstChar       : WCHAR;
    tmLastChar        : WCHAR;
    tmDefaultChar     : WCHAR;
    tmBreakChar       : WCHAR;
    tmItalic          : BYTE;
    tmUnderlined      : BYTE;
    tmStruckOut       : BYTE;
    tmPitchAndFamily  : BYTE;
    tmCharSet         : BYTE;
  END;

  TEXTMETRIC = TEXTMETRICA;
  PTEXTMETRIC = PTEXTMETRICA;
  NPTEXTMETRIC = NPTEXTMETRICA;
  LPTEXTMETRIC = LPTEXTMETRICA;

(* ntmFlags field flags *)
CONST
  NTM_REGULAR: LONG = 16_00000040;
  NTM_BOLD   : LONG = 16_00000020;
  NTM_ITALIC : LONG = 16_00000001;

TYPE
  PNEWTEXTMETRICA = UNTRACED REF NEWTEXTMETRICA;
  NPNEWTEXTMETRICA = UNTRACED REF NEWTEXTMETRICA;
  LPNEWTEXTMETRICA = UNTRACED REF NEWTEXTMETRICA;
  NEWTEXTMETRICA = RECORD
    tmHeight          : LONG;
    tmAscent          : LONG;
    tmDescent         : LONG;
    tmInternalLeading : LONG;
    tmExternalLeading : LONG;
    tmAveCharWidth    : LONG;
    tmMaxCharWidth    : LONG;
    tmWeight          : LONG;
    tmOverhang        : LONG;
    tmDigitizedAspectX: LONG;
    tmDigitizedAspectY: LONG;
    tmFirstChar       : BYTE;
    tmLastChar        : BYTE;
    tmDefaultChar     : BYTE;
    tmBreakChar       : BYTE;
    tmItalic          : BYTE;
    tmUnderlined      : BYTE;
    tmStruckOut       : BYTE;
    tmPitchAndFamily  : BYTE;
    tmCharSet         : BYTE;
    ntmFlags          : DWORD;
    ntmSizeEM         : UINT;
    ntmCellHeight     : UINT;
    ntmAvgWidth       : UINT;
  END;

  PNEWTEXTMETRICW = UNTRACED REF NEWTEXTMETRICW;
  NPNEWTEXTMETRICW = UNTRACED REF NEWTEXTMETRICW;
  LPNEWTEXTMETRICW = UNTRACED REF NEWTEXTMETRICW;
  NEWTEXTMETRICW = RECORD
    tmHeight          : LONG;
    tmAscent          : LONG;
    tmDescent         : LONG;
    tmInternalLeading : LONG;
    tmExternalLeading : LONG;
    tmAveCharWidth    : LONG;
    tmMaxCharWidth    : LONG;
    tmWeight          : LONG;
    tmOverhang        : LONG;
    tmDigitizedAspectX: LONG;
    tmDigitizedAspectY: LONG;
    tmFirstChar       : WCHAR;
    tmLastChar        : WCHAR;
    tmDefaultChar     : WCHAR;
    tmBreakChar       : WCHAR;
    tmItalic          : BYTE;
    tmUnderlined      : BYTE;
    tmStruckOut       : BYTE;
    tmPitchAndFamily  : BYTE;
    tmCharSet         : BYTE;
    ntmFlags          : DWORD;
    ntmSizeEM         : UINT;
    ntmCellHeight     : UINT;
    ntmAvgWidth       : UINT;
  END;

  NEWTEXTMETRIC = NEWTEXTMETRICA;
  PNEWTEXTMETRIC = PNEWTEXTMETRICA;
  NPNEWTEXTMETRIC = NPNEWTEXTMETRICA;
  LPNEWTEXTMETRIC = LPNEWTEXTMETRICA;

  (* GDI Logical Objects: *)

  (* Pel Array *)
  PPELARRAY = UNTRACED REF PELARRAY;
  NPPELARRAY = UNTRACED REF PELARRAY;
  LPPELARRAY = UNTRACED REF PELARRAY;
  PELARRAY = RECORD
    paXCount: LONG;
    paYCount: LONG;
    paXExt  : LONG;
    paYExt  : LONG;
    paRGBs  : BYTE;
  END;

  (* Logical Brush (or Pattern) *)
  PLOGBRUSH = UNTRACED REF LOGBRUSH;
  NPLOGBRUSH = UNTRACED REF LOGBRUSH;
  LPLOGBRUSH = UNTRACED REF LOGBRUSH;
  LOGBRUSH = RECORD
    lbStyle: UINT;
    lbColor: COLORREF;
    lbHatch: LONG;
  END;

  PATTERN = LOGBRUSH;
  PPATTERN = UNTRACED REF PATTERN;
  NPPATTERN = UNTRACED REF PATTERN;
  LPPATTERN = UNTRACED REF PATTERN;

  (* Logical Pen *)
  PLOGPEN = UNTRACED REF LOGPEN;
  NPLOGPEN = UNTRACED REF LOGPEN;
  LPLOGPEN = UNTRACED REF LOGPEN;
  LOGPEN = RECORD
    lopnStyle: UINT;
    lopnWidth: POINT;
    lopnColor: COLORREF;
  END;

  PEXTLOGPEN = UNTRACED REF EXTLOGPEN;
  NPEXTLOGPEN = UNTRACED REF EXTLOGPEN;
  LPEXTLOGPEN = UNTRACED REF EXTLOGPEN;
  EXTLOGPEN = RECORD
    elpPenStyle  : DWORD;
    elpWidth     : DWORD;
    elpBrushStyle: UINT;
    elpColor     : COLORREF;
    elpHatch     : LONG;
    elpNumEntries: DWORD;
    elpStyleEntry: ARRAY [0 .. 1 - 1] OF DWORD;
  END;

  PPALETTEENTRY = UNTRACED REF PALETTEENTRY;
  LPPALETTEENTRY = UNTRACED REF PALETTEENTRY;
  PALETTEENTRY = RECORD
    peRed  : BYTE;
    peGreen: BYTE;
    peBlue : BYTE;
    peFlags: BYTE;
  END;

  (* Logical Palette *)
  PLOGPALETTE = UNTRACED REF LOGPALETTE;
  NPLOGPALETTE = UNTRACED REF LOGPALETTE;
  LPLOGPALETTE = UNTRACED REF LOGPALETTE;
  LOGPALETTE = RECORD
    palVersion   : WORD;
    palNumEntries: WORD;
    palPalEntry  : ARRAY [0..100000] OF PALETTEENTRY;
  END;

  LOGPALETTEBASE= RECORD
    palVersion   : WORD;
    palNumEntries: WORD;
  END;

(* Logical Font *)
CONST LF_FACESIZE = 32;

TYPE
  PLOGFONTA = UNTRACED REF LOGFONTA;
  NPLOGFONTA = UNTRACED REF LOGFONTA;
  LPLOGFONTA = UNTRACED REF LOGFONTA;
  LOGFONTA = RECORD
    lfHeight        : LONG;
    lfWidth         : LONG;
    lfEscapement    : LONG;
    lfOrientation   : LONG;
    lfWeight        : LONG;
    lfItalic        : BYTE;
    lfUnderline     : BYTE;
    lfStrikeOut     : BYTE;
    lfCharSet       : BYTE;
    lfOutPrecision  : BYTE;
    lfClipPrecision : BYTE;
    lfQuality       : BYTE;
    lfPitchAndFamily: BYTE;
    lfFaceName      : ARRAY [0 .. LF_FACESIZE - 1] OF char;
  END;

  PLOGFONTW = UNTRACED REF LOGFONTW;
  NPLOGFONTW = UNTRACED REF LOGFONTW;
  LPLOGFONTW = UNTRACED REF LOGFONTW;
  LOGFONTW = RECORD
    lfHeight        : LONG;
    lfWidth         : LONG;
    lfEscapement    : LONG;
    lfOrientation   : LONG;
    lfWeight        : LONG;
    lfItalic        : BYTE;
    lfUnderline     : BYTE;
    lfStrikeOut     : BYTE;
    lfCharSet       : BYTE;
    lfOutPrecision  : BYTE;
    lfClipPrecision : BYTE;
    lfQuality       : BYTE;
    lfPitchAndFamily: BYTE;
    lfFaceName      : ARRAY [0 .. LF_FACESIZE - 1] OF WCHAR;
  END;

  LOGFONT = LOGFONTA;
  PLOGFONT = PLOGFONTA;
  NPLOGFONT = NPLOGFONTA;
  LPLOGFONT = LPLOGFONTA;

CONST LF_FULLFACESIZE = 64;

(* Structure passed to FONTENUMPROC *)
TYPE
  LPENUMLOGFONTA = UNTRACED REF ENUMLOGFONTA;
  ENUMLOGFONTA = RECORD
    elfLogFont : LOGFONTA;
    elfFullName: ARRAY [0 .. LF_FULLFACESIZE - 1] OF BYTE;
    elfStyle   : ARRAY [0 .. LF_FACESIZE - 1] OF BYTE;
  END;

  (* Structure passed to FONTENUMPROC *)
  LPENUMLOGFONTW = UNTRACED REF ENUMLOGFONTW;
  ENUMLOGFONTW = RECORD
    elfLogFont : LOGFONTW;
    elfFullName: ARRAY [0 .. LF_FULLFACESIZE - 1] OF WCHAR;
    elfStyle   : ARRAY [0 .. LF_FACESIZE - 1] OF WCHAR;
  END;

  ENUMLOGFONT = ENUMLOGFONTA;
  LPENUMLOGFONT = LPENUMLOGFONTA;

CONST
  OUT_DEFAULT_PRECIS   = 0;
  OUT_STRING_PRECIS    = 1;
  OUT_CHARACTER_PRECIS = 2;
  OUT_STROKE_PRECIS    = 3;
  OUT_TT_PRECIS        = 4;
  OUT_DEVICE_PRECIS    = 5;
  OUT_RASTER_PRECIS    = 6;
  OUT_TT_ONLY_PRECIS   = 7;
  OUT_OUTLINE_PRECIS   = 8;

  CLIP_DEFAULT_PRECIS   = 0;
  CLIP_CHARACTER_PRECIS = 1;
  CLIP_STROKE_PRECIS    = 2;
  CLIP_MASK             = 16_f;
  CLIP_LH_ANGLES        = Shift(1, 4);
  CLIP_TT_ALWAYS        = Shift(2, 4);
  CLIP_EMBEDDED         = Shift(8, 4);

  DEFAULT_QUALITY = 0;
  DRAFT_QUALITY   = 1;
  PROOF_QUALITY   = 2;

  DEFAULT_PITCH  = 0;
  FIXED_PITCH    = 1;
  VARIABLE_PITCH = 2;

  ANSI_CHARSET        = 0;
  UNICODE_CHARSET     = 1;
  SYMBOL_CHARSET      = 2;
  SHIFTJIS_CHARSET    = 128;
  HANGEUL_CHARSET     = 129;
  CHINESEBIG5_CHARSET = 136;
  OEM_CHARSET         = 255;

  (* Font Families *)
  FF_DONTCARE = Shift(0, 4) (* Don't care or don't know. *);
  FF_ROMAN    = Shift(1, 4) (* Variable stroke width, serifed. *);
  (* Times Roman, Century Schoolbook, etc. *)
  FF_SWISS = Shift(2, 4) (* Variable stroke width, sans-serifed. *);
  (* Helvetica, Swiss, etc. *)
  FF_MODERN = Shift(
                3, 4) (* Constant stroke width, serifed or sans-serifed. *);
  (* Pica, Elite, Courier, etc. *)
  FF_SCRIPT     = Shift(4, 4) (* Cursive, etc. *);
  FF_DECORATIVE = Shift(5, 4) (* Old English, etc. *);

  (* Font Weights *)
  FW_DONTCARE   = 0;
  FW_THIN       = 100;
  FW_EXTRALIGHT = 200;
  FW_LIGHT      = 300;
  FW_NORMAL     = 400;
  FW_MEDIUM     = 500;
  FW_SEMIBOLD   = 600;
  FW_BOLD       = 700;
  FW_EXTRABOLD  = 800;
  FW_HEAVY      = 900;

  FW_ULTRALIGHT = FW_EXTRALIGHT;
  FW_REGULAR    = FW_NORMAL;
  FW_DEMIBOLD   = FW_SEMIBOLD;
  FW_ULTRABOLD  = FW_EXTRABOLD;
  FW_BLACK      = FW_HEAVY;

(* !!![kirko] The CHARSET structure is sheduled to die *)
TYPE
  LPCHARSET = UNTRACED REF CHARSET;
  CHARSET = RECORD
    aflBlock: ARRAY [0 .. 3 - 1] OF DWORD;
    flLang  : DWORD;
  END;

CONST
  PANOSE_COUNT              = 10;
  PAN_FAMILYTYPE_INDEX      = 0;
  PAN_SERIFSTYLE_INDEX      = 1;
  PAN_WEIGHT_INDEX          = 2;
  PAN_PROPORTION_INDEX      = 3;
  PAN_CONTRAST_INDEX        = 4;
  PAN_STROKEVARIATION_INDEX = 5;
  PAN_ARMSTYLE_INDEX        = 6;
  PAN_LETTERFORM_INDEX      = 7;
  PAN_MIDLINE_INDEX         = 8;
  PAN_XHEIGHT_INDEX         = 9;

  PAN_CULTURE_LATIN = 0;

TYPE
  LPPANOSE = UNTRACED REF PANOSE;
  PANOSE = RECORD
    ulCulture: DWORD;  (* !!!  [kirko] this field will disappear *)
    bFamilyType     : BYTE;
    bSerifStyle     : BYTE;
    bWeight         : BYTE;
    bProportion     : BYTE;
    bContrast       : BYTE;
    bStrokeVariation: BYTE;
    bArmStyle       : BYTE;
    bLetterform     : BYTE;
    bMidline        : BYTE;
    bXHeight        : BYTE;
  END;

CONST
  PAN_ANY    = 0 (* Any *);
  PAN_NO_FIT = 1 (* No Fit *);

  PAN_FAMILY_TEXT_DISPLAY = 2 (* Text and Display *);
  PAN_FAMILY_SCRIPT       = 3 (* Script *);
  PAN_FAMILY_DECORATIVE   = 4 (* Decorative *);
  PAN_FAMILY_PICTORIAL    = 5 (* Pictorial *);

  PAN_SERIF_COVE               = 2 (* Cove *);
  PAN_SERIF_OBTUSE_COVE        = 3 (* Obtuse Cove *);
  PAN_SERIF_SQUARE_COVE        = 4 (* Square Cove *);
  PAN_SERIF_OBTUSE_SQUARE_COVE = 5 (* Obtuse Square Cove *);
  PAN_SERIF_SQUARE             = 6 (* Square *);
  PAN_SERIF_THIN               = 7 (* Thin *);
  PAN_SERIF_BONE               = 8 (* Bone *);
  PAN_SERIF_EXAGGERATED        = 9 (* Exaggerated *);
  PAN_SERIF_TRIANGLE           = 10 (* Triangle *);
  PAN_SERIF_NORMAL_SANS        = 11 (* Normal Sans *);
  PAN_SERIF_OBTUSE_SANS        = 12 (* Obtuse Sans *);
  PAN_SERIF_PERP_SANS          = 13 (* Prep Sans *);
  PAN_SERIF_FLARED             = 14 (* Flared *);
  PAN_SERIF_ROUNDED            = 15 (* Rounded *);

  PAN_WEIGHT_VERY_LIGHT = 2 (* Very Light *);
  PAN_WEIGHT_LIGHT      = 3 (* Light *);
  PAN_WEIGHT_THIN       = 4 (* Thin *);
  PAN_WEIGHT_BOOK       = 5 (* Book *);
  PAN_WEIGHT_MEDIUM     = 6 (* Medium *);
  PAN_WEIGHT_DEMI       = 7 (* Demi *);
  PAN_WEIGHT_BOLD       = 8 (* Bold *);
  PAN_WEIGHT_HEAVY      = 9 (* Heavy *);
  PAN_WEIGHT_BLACK      = 10 (* Black *);
  PAN_WEIGHT_NORD       = 11 (* Nord *);

  PAN_PROP_OLD_STYLE      = 2 (* Old Style *);
  PAN_PROP_MODERN         = 3 (* Modern *);
  PAN_PROP_EVEN_WIDTH     = 4 (* Even Width *);
  PAN_PROP_EXPANDED       = 5 (* Expanded *);
  PAN_PROP_CONDENSED      = 6 (* Condensed *);
  PAN_PROP_VERY_EXPANDED  = 7 (* Very Expanded *);
  PAN_PROP_VERY_CONDENSED = 8 (* Very Condensed *);
  PAN_PROP_MONOSPACED     = 9 (* Monospaced *);

  PAN_CONTRAST_NONE        = 2 (* None *);
  PAN_CONTRAST_VERY_LOW    = 3 (* Very Low *);
  PAN_CONTRAST_LOW         = 4 (* Low *);
  PAN_CONTRAST_MEDIUM_LOW  = 5 (* Medium Low *);
  PAN_CONTRAST_MEDIUM      = 6 (* Medium *);
  PAN_CONTRAST_MEDIUM_HIGH = 7 (* Mediim High *);
  PAN_CONTRAST_HIGH        = 8 (* High *);
  PAN_CONTRAST_VERY_HIGH   = 9 (* Very High *);

  PAN_STROKE_GRADUAL_DIAG = 2 (* Gradual/Diagonal *);
  PAN_STROKE_GRADUAL_TRAN = 3 (* Gradual/Transitional *);
  PAN_STROKE_GRADUAL_VERT = 4 (* Gradual/Vertical *);
  PAN_STROKE_GRADUAL_HORZ = 5 (* Gradual/Horizontal *);
  PAN_STROKE_RAPID_VERT   = 6 (* Rapid/Vertical *);
  PAN_STROKE_RAPID_HORZ   = 7 (* Rapid/Horizontal *);
  PAN_STROKE_INSTANT_VERT = 8 (* Instant/Vertical *);

  PAN_STRAIGHT_ARMS_HORZ         = 2 (* Straight Arms/Horizontal *);
  PAN_STRAIGHT_ARMS_WEDGE        = 3 (* Straight Arms/Wedge *);
  PAN_STRAIGHT_ARMS_VERT         = 4 (* Straight Arms/Vertical *);
  PAN_STRAIGHT_ARMS_SINGLE_SERIF = 5 (* Straight Arms/Single-Serif *);
  PAN_STRAIGHT_ARMS_DOUBLE_SERIF = 6 (* Straight Arms/Double-Serif *);
  PAN_BENT_ARMS_HORZ             = 7 (* Non-Straight Arms/Horizontal *);
  PAN_BENT_ARMS_WEDGE            = 8 (* Non-Straight Arms/Wedge *);
  PAN_BENT_ARMS_VERT             = 9 (* Non-Straight Arms/Vertical *);
  PAN_BENT_ARMS_SINGLE_SERIF     = 10 (* Non-Straight Arms/Single-Serif *);
  PAN_BENT_ARMS_DOUBLE_SERIF     = 11 (* Non-Straight Arms/Double-Serif *);

  PAN_LETT_NORMAL_CONTACT     = 2 (* Normal/Contact *);
  PAN_LETT_NORMAL_WEIGHTED    = 3 (* Normal/Weighted *);
  PAN_LETT_NORMAL_BOXED       = 4 (* Normal/Boxed *);
  PAN_LETT_NORMAL_FLATTENED   = 5 (* Normal/Flattened *);
  PAN_LETT_NORMAL_ROUNDED     = 6 (* Normal/Rounded *);
  PAN_LETT_NORMAL_OFF_CENTER  = 7 (* Normal/Off Center *);
  PAN_LETT_NORMAL_SQUARE      = 8 (* Normal/Square *);
  PAN_LETT_OBLIQUE_CONTACT    = 9 (* Oblique/Contact *);
  PAN_LETT_OBLIQUE_WEIGHTED   = 10 (* Oblique/Weighted *);
  PAN_LETT_OBLIQUE_BOXED      = 11 (* Oblique/Boxed *);
  PAN_LETT_OBLIQUE_FLATTENED  = 12 (* Oblique/Flattened *);
  PAN_LETT_OBLIQUE_ROUNDED    = 13 (* Oblique/Rounded *);
  PAN_LETT_OBLIQUE_OFF_CENTER = 14 (* Oblique/Off Center *);
  PAN_LETT_OBLIQUE_SQUARE     = 15 (* Oblique/Square *);

  PAN_MIDLINE_STANDARD_TRIMMED = 2 (* Standard/Trimmed *);
  PAN_MIDLINE_STANDARD_POINTED = 3 (* Standard/Pointed *);
  PAN_MIDLINE_STANDARD_SERIFED = 4 (* Standard/Serifed *);
  PAN_MIDLINE_HIGH_TRIMMED     = 5 (* High/Trimmed *);
  PAN_MIDLINE_HIGH_POINTED     = 6 (* High/Pointed *);
  PAN_MIDLINE_HIGH_SERIFED     = 7 (* High/Serifed *);
  PAN_MIDLINE_CONSTANT_TRIMMED = 8 (* Constant/Trimmed *);
  PAN_MIDLINE_CONSTANT_POINTED = 9 (* Constant/Pointed *);
  PAN_MIDLINE_CONSTANT_SERIFED = 10 (* Constant/Serifed *);
  PAN_MIDLINE_LOW_TRIMMED      = 11 (* Low/Trimmed *);
  PAN_MIDLINE_LOW_POINTED      = 12 (* Low/Pointed *);
  PAN_MIDLINE_LOW_SERIFED      = 13 (* Low/Serifed *);

  PAN_XHEIGHT_CONSTANT_SMALL = 2 (* Constant/Small *);
  PAN_XHEIGHT_CONSTANT_STD   = 3 (* Constant/Standard *);
  PAN_XHEIGHT_CONSTANT_LARGE = 4 (* Constant/Large *);
  PAN_XHEIGHT_DUCKING_SMALL  = 5 (* Ducking/Small *);
  PAN_XHEIGHT_DUCKING_STD    = 6 (* Ducking/Standard *);
  PAN_XHEIGHT_DUCKING_LARGE  = 7 (* Ducking/Large *);

  ELF_VENDOR_SIZE = 4;

(* The extended logical font *)
(* An extension of the ENUMLOGFONT *)
TYPE
  PEXTLOGFONTA = UNTRACED REF EXTLOGFONTA;
  NPEXTLOGFONTA = UNTRACED REF EXTLOGFONTA;
  LPEXTLOGFONTA = UNTRACED REF EXTLOGFONTA;
  EXTLOGFONTA = RECORD
    elfLogFont : LOGFONTA;
    elfFullName: ARRAY [0 .. LF_FULLFACESIZE - 1] OF BYTE;
    elfStyle   : ARRAY [0 .. LF_FACESIZE - 1] OF BYTE;
    elfVersion  : DWORD;  (* 0 for the first release of NT *)
    elfStyleSize: DWORD;
    elfMatch    : DWORD;
    elfReserved : DWORD;
    elfVendorId: ARRAY [0 .. ELF_VENDOR_SIZE - 1] OF BYTE;
    elfCulture: DWORD;   (* 0 for Latin *)
    elfPanose : PANOSE;
  END;

  PEXTLOGFONTW = UNTRACED REF EXTLOGFONTW;
  NPEXTLOGFONTW = UNTRACED REF EXTLOGFONTW;
  LPEXTLOGFONTW = UNTRACED REF EXTLOGFONTW;
  EXTLOGFONTW = RECORD
    elfLogFont : LOGFONTW;
    elfFullName: ARRAY [0 .. LF_FULLFACESIZE - 1] OF WCHAR;
    elfStyle   : ARRAY [0 .. LF_FACESIZE - 1] OF WCHAR;
    elfVersion  : DWORD;  (* 0 for the first release of NT *)
    elfStyleSize: DWORD;
    elfMatch    : DWORD;
    elfReserved : DWORD;
    elfVendorId: ARRAY [0 .. ELF_VENDOR_SIZE - 1] OF BYTE;
    elfCulture: DWORD;   (* 0 for Latin *)
    elfPanose : PANOSE;
  END;

  EXTLOGFONT = EXTLOGFONTA;
  PEXTLOGFONT = PEXTLOGFONTA;
  NPEXTLOGFONT = NPEXTLOGFONTA;
  LPEXTLOGFONT = LPEXTLOGFONTA;

CONST
  ELF_VERSION       = 0;
  ELF_CULTURE_LATIN = 0;

(* the complete set of font attribute distances *)

(* Allowed values for FMATCH::wType *)
CONST
  FMATCH_EXACT = 0;
  FMATCH_NEAR  = 1;
  FMATCH_FAR   = 2;
  FMATCH_ERROR = 3;

  PANOSE_RANGE = 16;

  FM_LOCATION_GDI = 1 (* location of font is in GDI *);

TYPE
  LPFMPENALTYSET = UNTRACED REF FMPENALTYSET;
  FMPENALTYSET = RECORD
    psSize  : DWORD;  (* size of this structure in BYTE's *)
    psHeight: DWORD;
    psWidth : DWORD;
    psEscapement    : DWORD;
    psOrientation   : DWORD;
    psWeight        : DWORD;
    psItalic        : DWORD;
    psUnderline     : DWORD;
    psStrikeOut     : DWORD;
    psOutPrecsion   : DWORD;
    psClipPrecision : DWORD;
    psQuality       : DWORD;
    psPitchAndFamily: DWORD;
    psFaceName      : DWORD;
    psFullName      : DWORD;
    psStyle         : DWORD;
    psPanose  : ARRAY [0 .. PANOSE_COUNT - 1] OF DWORD;
    psVendorId: DWORD;
    psLocation: DWORD;
  END;

  LPFMWEIGHTSET = UNTRACED REF FMWEIGHTSET;
  FMWEIGHTSET = RECORD
    wsSize  : DWORD;  (* size of this structure in BYTE's *)
    wsHeight: DWORD;
    wsWidth : DWORD;
    wsEscapement    : DWORD;
    wsOrientation   : DWORD;
    wsWeight        : DWORD;
    wsItalic        : DWORD;
    wsUnderline     : DWORD;
    wsStrikeOut     : DWORD;
    wsOutPrecsion   : DWORD;
    wsClipPrecision : DWORD;
    wsQuality       : DWORD;
    wsPitchAndFamily: DWORD;
    wsFaceName      : DWORD;
    wsFullName      : DWORD;
    wsStyle         : DWORD;
    wsPanose  : ARRAY [0 .. PANOSE_COUNT - 1] OF DWORD;
    wsVendorId: DWORD;
    wsLocation: DWORD;
  END;

  LPFMATCHA = UNTRACED REF FMATCHA;
  FMATCHA = RECORD
    fmSize: DWORD;    (* size of this structure in bytes *)
    fmTotalPenalty: DWORD;  (* total penalty of physical font *)
    fmPenaltySet: FMPENALTYSET;  (* penalties of physical font *)
    fmExtLogFont: EXTLOGFONTA;   (* describes physical font *)
  END;

  LPFMATCHW = UNTRACED REF FMATCHW;
  FMATCHW = RECORD
    fmSize: DWORD;    (* size of this structure in bytes *)
    fmTotalPenalty: DWORD;  (* total penalty of physical font *)
    fmPenaltySet: FMPENALTYSET;  (* penalties of physical font *)
    fmExtLogFont: EXTLOGFONTW;   (* describes physical font *)
  END;

  FMATCH = FMATCHA;
  LPFMATCH = LPFMATCHA;

(* indices to font mapping functions *)
CONST
  MAPPER_INDEX_TERMINATE = 0;

  MAPPER_INDEX_HEIGHT         = 1;
  MAPPER_INDEX_WIDTH          = 2;
  MAPPER_INDEX_ESCAPEMENT     = 3;
  MAPPER_INDEX_ORIENTATION    = 4;
  MAPPER_INDEX_WEIGHT         = 5;
  MAPPER_INDEX_ITALIC         = 6;
  MAPPER_INDEX_UNDERLINE      = 7;
  MAPPER_INDEX_STRIKEOUT      = 8;
  MAPPER_INDEX_CHARSET        = 9;
  MAPPER_INDEX_OUTPRECISION   = 10;
  MAPPER_INDEX_CLIPPRECISION  = 11;
  MAPPER_INDEX_QUALITY        = 12;
  MAPPER_INDEX_PITCHANDFAMILY = 13;
  MAPPER_INDEX_FACENAME       = 14;
  MAPPER_INDEX_FULLNAME       = 15;
  MAPPER_INDEX_STYLE          = 16;
  MAPPER_INDEX_PANOSE         = 17;
  MAPPER_INDEX_VENDORID       = 18;
  MAPPER_INDEX_ASPECT         = 19;
  MAPPER_INDEX_LOCATION       = 20;

  MAPPER_INDEX_LAST = MAPPER_INDEX_LOCATION;

  SIZEOFMAPORDER = BYTESIZE(UINT) * (MAPPER_INDEX_LAST + 1);

TYPE
  FMORDER = ARRAY [0 .. MAPPER_INDEX_LAST + 1 - 1] OF DWORD;
  LPFMORDER = UNTRACED REF FMORDER;

  LPFMCONTROLS = UNTRACED REF FMCONTROLS;
  FMCONTROLS = RECORD
    size         : DWORD;
    penaltySumMax: DWORD;
    fmpsMax      : FMPENALTYSET;
    fmws         : FMWEIGHTSET;
    fmorder      : FMORDER;
  END;

CONST SIZEOFFMCONTROLS = BYTESIZE(FMCONTROLS);

(* EnumFonts Masks *)
CONST
  RASTER_FONTTYPE   = 16_0001;
  DEVICE_FONTTYPE   = 16_002;
  TRUETYPE_FONTTYPE = 16_004;

PROCEDURE RGB (r, g, b: BYTE): COLORREF;
PROCEDURE PALETTERGB (r, g, b: BYTE): COLORREF;
PROCEDURE PALETTEINDEX (i: WORD): COLORREF;

(* palette entry flags *)
CONST
  PC_RESERVED   = 16_01 (* palette index used for animation *);
  PC_EXPLICIT   = 16_02 (* palette index is explicit to device *);
  PC_NOCOLLAPSE = 16_04 (* do not match color to system palette *);

PROCEDURE GetRValue (rgb: COLORREF): BYTE;
PROCEDURE GetGValue (rgb: COLORREF): BYTE;
PROCEDURE GetBValue (rgb: COLORREF): BYTE;

(* Background Modes *)
CONST
  TRANSPARENT = 1;
  OPAQUE      = 2;
  BKMODE_LAST = 2;

(* Graphics Modes *)
CONST
  GM_COMPATIBLE = 1;
  GM_ADVANCED = 2;
  GM_LAST = 2;

(* PolyDraw and GetPath point types *)
CONST
  PT_CLOSEFIGURE = 16_01;
  PT_LINETO      = 16_02;
  PT_BEZIERTO    = 16_04;
  PT_MOVETO      = 16_06;

(* Mapping Modes *)
CONST
  MM_TEXT        = 1;
  MM_LOMETRIC    = 2;
  MM_HIMETRIC    = 3;
  MM_LOENGLISH   = 4;
  MM_HIENGLISH   = 5;
  MM_TWIPS       = 6;
  MM_ISOTROPIC   = 7;
  MM_ANISOTROPIC = 8;

(* Min and Max Mapping Mode values *)
CONST
  MM_MIN            = MM_TEXT;
  MM_MAX            = MM_ANISOTROPIC;
  MM_MAX_FIXEDSCALE = MM_TWIPS;

(* Coordinate Modes *)
CONST
  ABSOLUTE = 1;
  RELATIVE = 2;

(* Stock Logical Objects *)
CONST
  WHITE_BRUSH         = 0;
  LTGRAY_BRUSH        = 1;
  GRAY_BRUSH          = 2;
  DKGRAY_BRUSH        = 3;
  BLACK_BRUSH         = 4;
  NULL_BRUSH          = 5;
  HOLLOW_BRUSH        = NULL_BRUSH;
  WHITE_PEN           = 6;
  BLACK_PEN           = 7;
  NULL_PEN            = 8;
  OEM_FIXED_FONT      = 10;
  ANSI_FIXED_FONT     = 11;
  ANSI_VAR_FONT       = 12;
  SYSTEM_FONT         = 13;
  DEVICE_DEFAULT_FONT = 14;
  DEFAULT_PALETTE     = 15;
  SYSTEM_FIXED_FONT   = 16;
  STOCK_LAST          = 16;

  CLR_INVALID = 16_FFFFFFFF;

(* Brush Styles *)
CONST
  BS_SOLID        = 0;
  BS_NULL         = 1;
  BS_HOLLOW       = BS_NULL;
  BS_HATCHED      = 2;
  BS_PATTERN      = 3;
  BS_INDEXED      = 4;
  BS_DIBPATTERN   = 5;
  BS_DIBPATTERNPT = 6;

(* Hatch Styles *)
CONST
  HS_HORIZONTAL = 0 (* ----- *);
  HS_VERTICAL   = 1 (* ||||| *);
  HS_FDIAGONAL  = 2 (* \\\\\ *);
  HS_BDIAGONAL  = 3 (* ///// *);
  HS_CROSS      = 4 (* +++++ *);
  HS_DIAGCROSS  = 5 (* xxxxx *);
  HS_FDIAGONAL1 = 6;
  HS_BDIAGONAL1 = 7;
  HS_SOLID      = 8;
  HS_DENSE1     = 9;
  HS_DENSE2     = 10;
  HS_DENSE3     = 11;
  HS_DENSE4     = 12;
  HS_DENSE5     = 13;
  HS_DENSE6     = 14;
  HS_DENSE7     = 15;
  HS_DENSE8     = 16;
  HS_NOSHADE    = 17;
  HS_HALFTONE   = 18;
  HS_API_MAX    = 19;

(* Pen Styles *)
CONST
  PS_SOLID       = 0;
  PS_DASH        = 1 (* ------- *);
  PS_DOT         = 2 (* ....... *);
  PS_DASHDOT     = 3 (* _._._._ *);
  PS_DASHDOTDOT  = 4 (* _.._.._ *);
  PS_NULL        = 5;
  PS_INSIDEFRAME = 6;
  PS_USERSTYLE   = 7;
  PS_ALTERNATE   = 8;
  PS_STYLE_MASK  = 16_0000000F;

CONST
  PS_ENDCAP_ROUND  = 16_00000000;
  PS_ENDCAP_SQUARE = 16_00000100;
  PS_ENDCAP_FLAT   = 16_00000200;
  PS_ENDCAP_MASK   = 16_00000F00;

  PS_JOIN_ROUND = 16_00000000;
  PS_JOIN_BEVEL = 16_00001000;
  PS_JOIN_MITER = 16_00002000;
  PS_JOIN_MASK  = 16_0000F000;

  PS_COSMETIC  = 16_00000000;
  PS_GEOMETRIC = 16_00010000;
  PS_TYPE_MASK = 16_000F0000;

  AD_COUNTERCLOCKWISE = 1;
  AD_CLOCKWISE        = 2;

(* Device Parameters for GetDeviceCaps() *)
CONST
  DRIVERVERSION = 0 (* Device driver version *);
  TECHNOLOGY    = 2 (* Device classification *);
  HORZSIZE      = 4 (* Horizontal size in millimeters *);
  VERTSIZE      = 6 (* Vertical size in millimeters *);
  HORZRES       = 8 (* Horizontal width in pixels *);
  VERTRES       = 10 (* Vertical width in pixels *);
  BITSPIXEL     = 12 (* Number of bits per pixel *);
  PLANES        = 14 (* Number of planes *);
  NUMBRUSHES    = 16 (* Number of brushes the device has *);
  NUMPENS       = 18 (* Number of pens the device has *);
  NUMMARKERS    = 20 (* Number of markers the device has *);
  NUMFONTS      = 22 (* Number of fonts the device has *);
  NUMCOLORS     = 24 (* Number of colors the device supports *);
  PDEVICESIZE   = 26 (* Size required for device descriptor *);
  CURVECAPS     = 28 (* Curve capabilities *);
  LINECAPS      = 30 (* Line capabilities *);
  POLYGONALCAPS = 32 (* Polygonal capabilities *);
  TEXTCAPS      = 34 (* Text capabilities *);
  CLIPCAPS      = 36 (* Clipping capabilities *);
  RASTERCAPS    = 38 (* Bitblt capabilities *);
  ASPECTX       = 40 (* Length of the X leg *);
  ASPECTY       = 42 (* Length of the Y leg *);
  ASPECTXY      = 44 (* Length of the hypotenuse *);

  LOGPIXELSX = 88 (* Logical pixels/inch in X *);
  LOGPIXELSY = 90 (* Logical pixels/inch in Y *);

  SIZEPALETTE = 104 (* Number of entries in physical palette *);
  NUMRESERVED = 106 (* Number of reserved entries in palette *);
  COLORRES    = 108 (* Actual color resolution *);

(* Printing related DeviceCaps.  These replace the appropriate Escapes *)
CONST
  PHYSICALWIDTH   = 110 (* Physical Width in device units *);
  PHYSICALHEIGHT  = 111 (* Physical Height in device units *);
  PHYSICALOFFSETX = 112 (* Physical Printable Area x margin *);
  PHYSICALOFFSETY = 113 (* Physical Printable Area y margin *);
  SCALINGFACTORX  = 114 (* Scaling factor x *);
  SCALINGFACTORY  = 115 (* Scaling factor y *);

(* Device Capability Masks: *)

(* Device Technologies *)
CONST
  DT_PLOTTER    = 0 (* Vector plotter *);
  DT_RASDISPLAY = 1 (* Raster display *);
  DT_RASPRINTER = 2 (* Raster printer *);
  DT_RASCAMERA  = 3 (* Raster camera *);
  DT_CHARSTREAM = 4 (* Character-stream, PLP *);
  DT_METAFILE   = 5 (* Metafile, VDM *);
  DT_DISPFILE   = 6 (* Display-file *);

(* Curve Capabilities *)
CONST
  CC_NONE       = 0 (* Curves not supported *);
  CC_CIRCLES    = 1 (* Can do circles *);
  CC_PIE        = 2 (* Can do pie wedges *);
  CC_CHORD      = 4 (* Can do chord arcs *);
  CC_ELLIPSES   = 8 (* Can do ellipese *);
  CC_WIDE       = 16 (* Can do wide lines *);
  CC_STYLED     = 32 (* Can do styled lines *);
  CC_WIDESTYLED = 64 (* Can do wide styled lines *);
  CC_INTERIORS  = 128 (* Can do interiors *);
  CC_ROUNDRECT  = 256 (* *);

(* Line Capabilities *)
CONST
  LC_NONE       = 0 (* Lines not supported *);
  LC_POLYLINE   = 2 (* Can do polylines *);
  LC_MARKER     = 4 (* Can do markers *);
  LC_POLYMARKER = 8 (* Can do polymarkers *);
  LC_WIDE       = 16 (* Can do wide lines *);
  LC_STYLED     = 32 (* Can do styled lines *);
  LC_WIDESTYLED = 64 (* Can do wide styled lines *);
  LC_INTERIORS  = 128 (* Can do interiors *);

(* Polygonal Capabilities *)
CONST
  PC_NONE        = 0 (* Polygonals not supported *);
  PC_POLYGON     = 1 (* Can do polygons *);
  PC_RECTANGLE   = 2 (* Can do rectangles *);
  PC_WINDPOLYGON = 4 (* Can do winding polygons *);
  PC_TRAPEZOID   = 4 (* Can do trapezoids *);
  PC_SCANLINE    = 8 (* Can do scanlines *);
  PC_WIDE        = 16 (* Can do wide borders *);
  PC_STYLED      = 32 (* Can do styled borders *);
  PC_WIDESTYLED  = 64 (* Can do wide styled borders *);
  PC_INTERIORS   = 128 (* Can do interiors *);

(* Polygonal Capabilities *)
CONST
  CP_NONE      = 0 (* No clipping of output *);
  CP_RECTANGLE = 1 (* Output clipped to rects *);
  CP_REGION    = 2 (* *);

(* Text Capabilities *)
CONST
  TC_OP_CHARACTER = 16_00000001 (* Can do OutputPrecision CHARACTER *);
  TC_OP_STROKE    = 16_00000002 (* Can do OutputPrecision STROKE *);
  TC_CP_STROKE    = 16_00000004 (* Can do ClipPrecision STROKE *);
  TC_CR_90        = 16_00000008 (* Can do CharRotAbility 90 *);
  TC_CR_ANY       = 16_00000010 (* Can do CharRotAbility ANY *);
  TC_SF_X_YINDEP  = 16_00000020 (* Can do ScaleFreedom X_YINDEPENDENT *);
  TC_SA_DOUBLE    = 16_00000040 (* Can do ScaleAbility DOUBLE *);
  TC_SA_INTEGER   = 16_00000080 (* Can do ScaleAbility INTEGER *);
  TC_SA_CONTIN    = 16_00000100 (* Can do ScaleAbility CONTINUOUS *);
  TC_EA_DOUBLE    = 16_00000200 (* Can do EmboldenAbility DOUBLE *);
  TC_IA_ABLE      = 16_00000400 (* Can do ItalisizeAbility ABLE *);
  TC_UA_ABLE      = 16_00000800 (* Can do UnderlineAbility ABLE *);
  TC_SO_ABLE      = 16_00001000 (* Can do StrikeOutAbility ABLE *);
  TC_RA_ABLE      = 16_00002000 (* Can do RasterFontAble ABLE *);
  TC_VA_ABLE      = 16_00004000 (* Can do VectorFontAble ABLE *);
  TC_RESERVED     = 16_00008000;
  TC_SCROLLBLT    = 16_00010000 (* do text scroll with blt *);

(* Raster Capabilities *)
CONST
  RC_BITBLT       = 1 (* Can do standard BLT. *);
  RC_BANDING      = 2 (* Device requires banding support *);
  RC_SCALING      = 4 (* Device requires scaling support *);
  RC_BITMAP64     = 8 (* Device can support >64K bitmap *);
  RC_GDI20_OUTPUT = 16_0010 (* has 2.0 output calls *);
  RC_GDI20_STATE  = 16_0020;
  RC_SAVEBITMAP   = 16_0040;
  RC_DI_BITMAP    = 16_0080 (* supports DIB to memory *);
  RC_PALETTE      = 16_0100 (* supports a palette *);
  RC_DIBTODEV     = 16_0200 (* supports DIBitsToDevice *);
  RC_BIGFONT      = 16_0400 (* supports >64K fonts *);
  RC_STRETCHBLT   = 16_0800 (* supports StretchBlt *);
  RC_FLOODFILL    = 16_1000 (* supports FloodFill *);
  RC_STRETCHDIB   = 16_2000 (* supports StretchDIBits *);
  RC_OP_DX_OUTPUT = 16_4000;
  RC_DEVBITS      = 16_8000;

(* DIB color table identifiers *)
CONST
  DIB_RGB_COLORS      = 0 (* color table in RGBs *);
  DIB_PAL_COLORS      = 1 (* color table in palette indices *);
  DIB_PAL_INDICES     = 2 (* No color table indices into surf palette *);
  DIB_PAL_PHYSINDICES = 2 (* No color table indices into surf palette *);
  DIB_PAL_LOGINDICES  = 4 (* No color table indices into DC palette *);

(* constants for Get/SetSystemPaletteUse() *)
CONST
  SYSPAL_ERROR    = 0;
  SYSPAL_STATIC   = 1;
  SYSPAL_NOSTATIC = 2;

(* constants for CreateDIBitmap *)
CONST
  CBM_CREATEDIB: LONG = 16_02 (* create DIB bitmap *);
  CBM_INIT     : LONG = 16_04 (* initialize bitmap *);

(* ExtFloodFill style flags *)
CONST
  FLOODFILLBORDER  = 0;
  FLOODFILLSURFACE = 1;

(* DEVMODE dmDisplayMode flags *)
CONST DM_GRAYSCALE = 1;

TYPE
  PDEVMODEA = UNTRACED REF DEVMODEA;
  NPDEVMODEA = UNTRACED REF DEVMODEA;
  LPDEVMODEA = UNTRACED REF DEVMODEA;
  DEVMODEA = RECORD
    dmDeviceName   : ARRAY [0 .. 32 - 1] OF BYTE;
    dmSpecVersion  : WORD;
    dmDriverVersion: WORD;
    dmSize         : WORD;
    dmDriverExtra  : WORD;
    dmFields       : DWORD;
    dmOrientation  : short;
    dmPaperSize    : short;
    dmPaperLength  : short;
    dmPaperWidth   : short;
    dmScale        : short;
    dmCopies       : short;
    dmDefaultSource: short;
    dmPrintQuality : short;
    dmColor        : short;
    dmDuplex       : short;
    dmYResolution  : short;
    dmTTOption     : short;
    dmCollate      : short;
    dmFormName     : ARRAY [0 .. 32 - 1] OF BYTE;
    dmBitsPerPel   : USHORT;
    dmPelsWidth    : DWORD;
    dmPelsHeight   : DWORD;
    dmDisplayMode  : DWORD;
  END;

  PDEVMODEW = UNTRACED REF DEVMODEW;
  LPDEVMODEW = UNTRACED REF DEVMODEW;
  NPDEVMODEW = UNTRACED REF DEVMODEW;
  DEVMODEW = RECORD
    dmDeviceName   : ARRAY [0 .. 32 - 1] OF WCHAR;
    dmSpecVersion  : WORD;
    dmDriverVersion: WORD;
    dmSize         : WORD;
    dmDriverExtra  : WORD;
    dmFields       : DWORD;
    dmOrientation  : short;
    dmPaperSize    : short;
    dmPaperLength  : short;
    dmPaperWidth   : short;
    dmScale        : short;
    dmCopies       : short;
    dmDefaultSource: short;
    dmPrintQuality : short;
    dmColor        : short;
    dmDuplex       : short;
    dmYResolution  : short;
    dmTTOption     : short;
    dmCollate      : short;
    dmFormName     : ARRAY [0 .. 32 - 1] OF WCHAR;
    dmBitsPerPel   : USHORT;
    dmPelsWidth    : DWORD;
    dmPelsHeight   : DWORD;
    dmDisplayMode  : DWORD;
  END;

  DEVMODE = DEVMODEA;
  PDEVMODE = PDEVMODEA;
  NPDEVMODE = NPDEVMODEA;
  LPDEVMODE = LPDEVMODEA;

(* GetRegionData/ExtCreateRegion *)
CONST RDH_RECTANGLES = 1;

TYPE
  PRGNDATAHEADER = RGNDATAHEADER;
  RGNDATAHEADER = RECORD
    dwSize  : DWORD;
    iType   : DWORD;
    nCount  : DWORD;
    nRgnSize: DWORD;
    rcBound : RECT;
  END;

  PRGNDATA = UNTRACED REF RGNDATA;
  NPRGNDATA = UNTRACED REF RGNDATA;
  LPRGNDATA = UNTRACED REF RGNDATA;
  RGNDATA = RECORD
    rdh   : RGNDATAHEADER;
    Buffer: ARRAY [0 .. 1 - 1] OF char;
  END;

  PABC = UNTRACED REF ABC;
  NPABC = UNTRACED REF ABC;
  LPABC = UNTRACED REF ABC;
  ABC = RECORD
    abcA: int;
    abcB: UINT;
    abcC: int;
  END;

  PABCFLOAT = UNTRACED REF ABCFLOAT;
  NPABCFLOAT = UNTRACED REF ABCFLOAT;
  LPABCFLOAT = UNTRACED REF ABCFLOAT;
  ABCFLOAT = RECORD
    abcfA: WFLOAT;
    abcfB: WFLOAT;
    abcfC: WFLOAT;
  END;

  POUTLINETEXTMETRICA = UNTRACED REF OUTLINETEXTMETRICA;
  NPOUTLINETEXTMETRICA = UNTRACED REF OUTLINETEXTMETRICA;
  LPOUTLINETEXTMETRICA = UNTRACED REF OUTLINETEXTMETRICA;
  OUTLINETEXTMETRICA = RECORD
    otmSize               : UINT;
    otmTextMetrics        : TEXTMETRICA;
    otmFiller             : BYTE;
    otmPanoseNumber       : PANOSE;
    otmfsSelection        : UINT;
    otmfsType             : UINT;
    otmsCharSlopeRise     : UINT;
    otmsCharSlopeRun      : UINT;
    otmItalicAngle        : UINT;
    otmEMSquare           : UINT;
    otmAscent             : UINT;
    otmDescent            : int;
    otmLineGap            : int;
    otmsCapEmHeight       : UINT;
    otmsXHeight           : UINT;
    otmrcFontBox          : RECT;
    otmMacAscent          : int;
    otmMacDescent         : int;
    otmMacLineGap         : UINT;
    otmusMinimumPPEM      : UINT;
    otmptSubscriptSize    : POINT;
    otmptSubscriptOffset  : POINT;
    otmptSuperscriptSize  : POINT;
    otmptSuperscriptOffset: POINT;
    otmsStrikeoutSize     : UINT;
    otmsStrikeoutPosition : int;
    otmsUnderscoreSize    : int;
    otmsUnderscorePosition: UINT;
    otmpFamilyName        : PSTR;
    otmpFaceName          : PSTR;
    otmpStyleName         : PSTR;
    otmpFullName          : PSTR;
  END;

  POUTLINETEXTMETRICW = UNTRACED REF OUTLINETEXTMETRICW;
  NPOUTLINETEXTMETRICW = UNTRACED REF OUTLINETEXTMETRICW;
  LPOUTLINETEXTMETRICW = UNTRACED REF OUTLINETEXTMETRICW;
  OUTLINETEXTMETRICW = RECORD
    otmSize               : UINT;
    otmTextMetrics        : TEXTMETRICW;
    otmFiller             : BYTE;
    otmPanoseNumber       : PANOSE;
    otmfsSelection        : UINT;
    otmfsType             : UINT;
    otmsCharSlopeRise     : UINT;
    otmsCharSlopeRun      : UINT;
    otmItalicAngle        : UINT;
    otmEMSquare           : UINT;
    otmAscent             : UINT;
    otmDescent            : int;
    otmLineGap            : int;
    otmsCapEmHeight       : UINT;
    otmsXHeight           : UINT;
    otmrcFontBox          : RECT;
    otmMacAscent          : int;
    otmMacDescent         : int;
    otmMacLineGap         : UINT;
    otmusMinimumPPEM      : UINT;
    otmptSubscriptSize    : POINT;
    otmptSubscriptOffset  : POINT;
    otmptSuperscriptSize  : POINT;
    otmptSuperscriptOffset: POINT;
    otmsStrikeoutSize     : UINT;
    otmsStrikeoutPosition : int;
    otmsUnderscoreSize    : int;
    otmsUnderscorePosition: UINT;
    otmpFamilyName        : PSTR;
    otmpFaceName          : PSTR;
    otmpStyleName         : PSTR;
    otmpFullName          : PSTR;
  END;

  OUTLINETEXTMETRIC = OUTLINETEXTMETRICA;
  POUTLINETEXTMETRIC = POUTLINETEXTMETRICA;
  NPOUTLINETEXTMETRIC = NPOUTLINETEXTMETRICA;
  LPOUTLINETEXTMETRIC = LPOUTLINETEXTMETRICA;

  POLYTEXTA = RECORD
    x      : int;
    y      : int;
    n      : UINT;
    lpwstr : UNTRACED REF BYTE;
    uiFlags: UINT;
    rcl    : RECT;
    pdx    : UNTRACED REF int;
  END;

  POLYTEXTW = RECORD
    x      : int;
    y      : int;
    n      : UINT;
    lpwstr : UNTRACED REF WCHAR;
    uiFlags: UINT;
    rcl    : RECT;
    pdx    : UNTRACED REF int;
  END;

  POLYTEXT = POLYTEXTA;

  FIXED = RECORD
    fract: WORD;
    value: short;
  END;

  LPMAT2 = UNTRACED REF MAT2;
  MAT2 = RECORD
    eM11: FIXED;
    eM12: FIXED;
    eM21: FIXED;
    eM22: FIXED;
  END;

  LPGLYPHMETRICS = UNTRACED REF GLYPHMETRICS;
  GLYPHMETRICS = RECORD
    gmBlackBoxX    : UINT;
    gmBlackBoxY    : UINT;
    gmptGlyphOrigin: POINT;
    gmCellIncX     : short;
    gmCellIncY     : short;
  END;

(* GetGlyphOutline constants *)

CONST
  GGO_NONE   = 0;
  GGO_BITMAP = 1;
  GGO_NATIVE = 2;

  TT_POLYGON_TYPE = 24;

  TT_PRIM_LINE    = 1;
  TT_PRIM_QSPLINE = 2;

TYPE
  LPPOINTFX = UNTRACED REF POINTFX;
  POINTFX = RECORD
    x: FIXED;
    y: FIXED;
  END;

  LPTTPOLYCURVE = UNTRACED REF TTPOLYCURVE;
  TTPOLYCURVE = RECORD
    wType: WORD;
    cpfx : WORD;
    apfx : ARRAY [0 .. 1 - 1] OF POINTFX;
  END;

  LPTTPOLYGONHEADER = UNTRACED REF TTPOLYGONHEADER;
  TTPOLYGONHEADER = RECORD
    cb      : DWORD;
    dwType  : DWORD;
    pfxStart: POINTFX;
  END;

  LPRASTERIZER_STATUS = UNTRACED REF RASTERIZER_STATUS;
  RASTERIZER_STATUS = RECORD
    nSize      : short;
    wFlags     : short;
    nLanguageID: short;
  END;

(* bits defined in wFlags of RASTERIZER_STATUS *)
CONST
  TT_AVAILABLE = 16_0001;
  TT_ENABLED   = 16_0002;

(* Pixel format descriptor *)
TYPE
  PIXELFORMATDESCRIPTOR = RECORD
    nSize          : WORD;
    nVersion       : WORD;
    dwFlags        : DWORD;
    iPixelType     : BYTE;
    cColorBits     : BYTE;
    cRedBits       : BYTE;
    cRedShift      : BYTE;
    cGreenBits     : BYTE;
    cGreenShift    : BYTE;
    cBlueBits      : BYTE;
    cBlueShift     : BYTE;
    cAlphaBits     : BYTE;
    cAlphaShift    : BYTE;
    cAccumBits     : BYTE;
    cAccumRedBits  : BYTE;
    cAccumGreenBits: BYTE;
    cAccumBlueBits : BYTE;
    cAccumAlphaBits: BYTE;
    cDepthBits     : BYTE;
    cStencilBits   : BYTE;
    cAuxBuffers    : BYTE;
    iLayerType     : BYTE;
    bReserved      : BYTE;
    dwLayerMask    : DWORD;
    dwVisibleMask  : DWORD;
    dwDamageMask   : DWORD;
  END;
  LPPIXELFORMATDESCRIPTOR = UNTRACED REF PIXELFORMATDESCRIPTOR;

CONST
  (* pixel types *)
  PFD_TYPE_RGBA       = 0;
  PFD_TYPE_COLORINDEX = 1;

  (* layer types *)
  PFD_MAIN_PLANE     =  0;
  PFD_OVERLAY_PLANE  =  1;
  PFD_UNDERLAY_PLANE = -1;

  (* PIXELFORMATDESCRIPTOR flags *)
  PFD_DOUBLEBUFFER        = 16_00000001;
  PFD_STEREO              = 16_00000002;
  PFD_DRAW_TO_WINDOW      = 16_00000004;
  PFD_DRAW_TO_BITMAP      = 16_00000008;
  PFD_SUPPORT_GDI         = 16_00000010;
  PFD_SUPPORT_OPENGL      = 16_00000020;
  PFD_GENERIC_FORMAT      = 16_00000040;
  PFD_NEED_PALETTE        = 16_00000080;
  PFD_NEED_SYSTEM_PALETTE = 16_00000100;
  PFD_GENERIC_ACCELERATED = 16_00001000;

  (* PIXELFORMATDESCRIPTOR flags for use in ChoosePixelFormat only *)
  PFD_DOUBLEBUFFER_DONTCARE = 16_40000000;
  PFD_STEREO_DONTCARE       = 16_80000000;

TYPE
  FONTENUMPROC = <*CALLBACK*> PROCEDURE (a1: UNTRACED REF LOGFONT;
                                         a2: UNTRACED REF TEXTMETRIC;
                                         a3: DWORD;
                                         a4: LPARAM                   ): int;

  GOBJENUMPROC = <*CALLBACK*> PROCEDURE (a1: LPVOID; a2: LPARAM): int;
  LINEDDAPROC = <*CALLBACK*> PROCEDURE (a1: int; a2: int; a3: LPARAM);

<*EXTERNAL AddFontResourceA:WINAPI*>
PROCEDURE AddFontResourceA (a1: LPCSTR): int;

<*EXTERNAL AddFontResourceW:WINAPI*>
PROCEDURE AddFontResourceW (a1: LPCWSTR): int;
CONST AddFontResource = AddFontResourceA;

<*EXTERNAL AddFontModule:WINAPI*>
PROCEDURE AddFontModule (a1: HMODULE): int;

<*EXTERNAL AnimatePalette:WINAPI*>
PROCEDURE AnimatePalette (a1: HPALETTE;
                            a2: UINT;
                            a3: UINT;
                            a4: UNTRACED REF PALETTEENTRY): BOOL;

<*EXTERNAL Arc:WINAPI*>
PROCEDURE Arc (a1: HDC; a2, a3, a4, a5, a6, a7, a8, a9: int): BOOL;

<*EXTERNAL BitBlt:WINAPI*>
PROCEDURE BitBlt (a1: HDC;  a2, a3, a4, a5: int;
                  a6: HDC;  a7, a8: int;  a9: DWORD): BOOL;

<*EXTERNAL CancelDC:WINAPI*>
PROCEDURE CancelDC (a1: HDC): BOOL;

<*EXTERNAL Chord:WINAPI*>
PROCEDURE Chord (a1: HDC; a2, a3, a4, a5, a6, a7, a8, a9: int): BOOL;

<*EXTERNAL ChoosePixelFormat:WINAPI*>
PROCEDURE ChoosePixelFormat(a1: HDC; a2: LPPIXELFORMATDESCRIPTOR): int;

<*EXTERNAL CloseMetaFile:WINAPI*>
PROCEDURE CloseMetaFile (a1: HDC): HMETAFILE;

<*EXTERNAL CombineRgn:WINAPI*>
PROCEDURE CombineRgn (a1: HRGN; a2: HRGN; a3: HRGN; a4: int): int;

<*EXTERNAL CopyMetaFileA:WINAPI*>
PROCEDURE CopyMetaFileA (a1: HMETAFILE; a2: LPSTR): HMETAFILE;

<*EXTERNAL CopyMetaFileW:WINAPI*>
PROCEDURE CopyMetaFileW (a1: HMETAFILE; a2: LPWSTR): HMETAFILE;
CONST CopyMetaFile = CopyMetaFileA;

<*EXTERNAL CreateBitmap:WINAPI*>
PROCEDURE CreateBitmap (a1, a2: int;  a3, a4: UINT;  a5: void_star): HBITMAP;

<*EXTERNAL CreateBitmapIndirect:WINAPI*>
PROCEDURE CreateBitmapIndirect (a1: LPBITMAP): HBITMAP;

<*EXTERNAL CreateBrushIndirect:WINAPI*>
PROCEDURE CreateBrushIndirect (a1: LPLOGBRUSH): HBRUSH;

<*EXTERNAL CreateCompatibleBitmap:WINAPI*>
PROCEDURE CreateCompatibleBitmap (a1: HDC; a2: int; a3: int): HBITMAP;

<*EXTERNAL CreateDiscardableBitmap:WINAPI*>
PROCEDURE CreateDiscardableBitmap (a1: HDC; a2: int; a3: int): HBITMAP;

<*EXTERNAL CreateCompatibleDC:WINAPI*>
PROCEDURE CreateCompatibleDC (a1: HDC): HDC;

<*EXTERNAL CreateDCA:WINAPI*>
PROCEDURE CreateDCA (a1, a2, a3: LPCSTR;  a4: UNTRACED REF DEVMODEA): HDC;

<*EXTERNAL CreateDCW:WINAPI*>
PROCEDURE CreateDCW (a1, a2, a3: LPCWSTR; a4: UNTRACED REF DEVMODEW): HDC;
CONST CreateDC = CreateDCA;

<*EXTERNAL CreateDIBitmap:WINAPI*>
PROCEDURE CreateDIBitmap (a1: HDC;
                            a2: LPBITMAPINFOHEADER;
                            a3: DWORD;
                            a4: UNTRACED REF BYTE;
                            a5: LPBITMAPINFO;
                            a6: UINT                ): HBITMAP;

<*EXTERNAL CreateDIBSection:WINAPI*>
PROCEDURE CreateDIBSection (a1: HDC;
                              a2: LPBITMAPINFO;
                              a3: DWORD;
                              a4: DWORD;
                              a5: UNTRACED REF LPBYTE): HBITMAP;

<*EXTERNAL CreateDIBPatternBrush:WINAPI*>
PROCEDURE CreateDIBPatternBrush (a1: HGLOBAL; a2: UINT): HBRUSH;

<*EXTERNAL CreateDIBPatternBrushPt:WINAPI*>
PROCEDURE CreateDIBPatternBrushPt (a1: LPVOID; a2: DWORD): HBRUSH;

<*EXTERNAL CreateEllipticRgn:WINAPI*>
PROCEDURE CreateEllipticRgn (a1: int; a2: int; a3: int; a4: int): HRGN;

<*EXTERNAL CreateEllipticRgnIndirect:WINAPI*>
PROCEDURE CreateEllipticRgnIndirect (a1: LPRECT): HRGN;

<*EXTERNAL CreateFontIndirectA:WINAPI*>
PROCEDURE CreateFontIndirectA (a1: UNTRACED REF LOGFONTA): HFONT;

<*EXTERNAL CreateFontIndirectW:WINAPI*>
PROCEDURE CreateFontIndirectW (a1: UNTRACED REF LOGFONTW): HFONT;
CONST CreateFontIndirect = CreateFontIndirectA;

<*EXTERNAL CreateFontA:WINAPI*>
PROCEDURE CreateFontA (a1, a2, a3, a4, a5: int;
                       a6, a7, a8, a9, a10, a11, a12, a13: DWORD;
                       a14: LPCSTR ): HFONT;

<*EXTERNAL CreateFontW:WINAPI*>
PROCEDURE CreateFontW (a1, a2, a3, a4, a5: int;
                       a6, a7, a8, a9, a10, a11, a12, a13: DWORD;
                       a14: LPCWSTR ): HFONT;

CONST CreateFont = CreateFontA;

<*EXTERNAL CreateHatchBrush:WINAPI*>
PROCEDURE CreateHatchBrush (a1: int; a2: COLORREF): HBRUSH;

<*EXTERNAL CreateICA:WINAPI*>
PROCEDURE CreateICA (a1, a2, a3: LPCSTR; a4: UNTRACED REF DEVMODEA): HDC;

<*EXTERNAL CreateICW:WINAPI*>
PROCEDURE CreateICW (a1, a2, a3: LPCWSTR; a4: UNTRACED REF DEVMODEW): HDC;
CONST CreateIC = CreateICA;

<*EXTERNAL CreateMetaFileA:WINAPI*>
PROCEDURE CreateMetaFileA (a1: LPCSTR): HDC;

<*EXTERNAL CreateMetaFileW:WINAPI*>
PROCEDURE CreateMetaFileW (a1: LPCWSTR): HDC;
CONST CreateMetaFile = CreateMetaFileA;

<*EXTERNAL CreatePalette:WINAPI*>
PROCEDURE CreatePalette (a1: UNTRACED REF LOGPALETTE): HPALETTE;

<*EXTERNAL CreatePen:WINAPI*>
PROCEDURE CreatePen (a1: int; a2: int; a3: COLORREF): HPEN;

<*EXTERNAL CreatePenIndirect:WINAPI*>
PROCEDURE CreatePenIndirect (a1: LPLOGPEN): HPEN;

<*EXTERNAL CreatePolyPolygonRgn:WINAPI*>
PROCEDURE CreatePolyPolygonRgn (a1: LPPOINT; a2: UNTRACED REF INT;
                                a3, a4: int): HRGN;

<*EXTERNAL CreatePatternBrush:WINAPI*>
PROCEDURE CreatePatternBrush (a1: HBITMAP): HBRUSH;

<*EXTERNAL CreateRectRgn:WINAPI*>
PROCEDURE CreateRectRgn (a1: int; a2: int; a3: int; a4: int): HRGN;

<*EXTERNAL CreateRectRgnIndirect:WINAPI*>
PROCEDURE CreateRectRgnIndirect (a1: LPRECT): HRGN;

<*EXTERNAL CreateRoundRectRgn:WINAPI*>
PROCEDURE CreateRoundRectRgn (a1, a2, a3, a4, a5, a6: int): HRGN;

<*EXTERNAL CreateScalableFontResourceA:WINAPI*>
PROCEDURE CreateScalableFontResourceA (a1: DWORD;
                                         a2: LPCSTR;
                                         a3: LPCSTR;
                                         a4: LPSTR   ): BOOL;

<*EXTERNAL CreateScalableFontResourceW:WINAPI*>
PROCEDURE CreateScalableFontResourceW (a1: DWORD;
                                         a2: LPCWSTR;
                                         a3: LPCWSTR;
                                         a4: LPWSTR   ): BOOL;
CONST CreateScalableFontResource = CreateScalableFontResourceA;

<*EXTERNAL CreateSolidBrush:WINAPI*>
PROCEDURE CreateSolidBrush (a1: COLORREF): HBRUSH;

<*EXTERNAL DeleteDC:WINAPI*>
PROCEDURE DeleteDC (a1: HDC): BOOL;

<*EXTERNAL DeleteMetaFile:WINAPI*>
PROCEDURE DeleteMetaFile (a1: HMETAFILE): BOOL;

<*EXTERNAL DeleteObject:WINAPI*>
PROCEDURE DeleteObject (a1: HGDIOBJ): BOOL;

<*EXTERNAL DescribePixelFormat:WINAPI*>
PROCEDURE DescribePixelFormat(a1: HDC; 
                              a2: int; 
                              a3: UINT; 
                              a4: LPPIXELFORMATDESCRIPTOR): int;

<*EXTERNAL DeviceCapabilitiesEx:WINAPI*>
PROCEDURE DeviceCapabilitiesEx (a1: LPSTR;
                                a2: LPSTR;
                                a3: LPSTR;
                                a4: int;
                                a5: LPSTR;
                                a6: LPDEVMODE): int;

<*EXTERNAL Ellipse:WINAPI*>
PROCEDURE Ellipse (a1: HDC; a2: int; a3: int; a4: int; a5: int): BOOL;

<*EXTERNAL EnumFontFamiliesA:WINAPI*>
PROCEDURE EnumFontFamiliesA (a1: HDC;
                               a2: LPCSTR;
                               a3: FONTENUMPROC;
                               a4: LPARAM        ): int;

<*EXTERNAL EnumFontFamiliesW:WINAPI*>
PROCEDURE EnumFontFamiliesW (a1: HDC;
                               a2: LPCWSTR;
                               a3: FONTENUMPROC;
                               a4: LPARAM        ): int;
CONST EnumFontFamilies = EnumFontFamiliesA;

<*EXTERNAL EnumFontsA:WINAPI*>
PROCEDURE EnumFontsA (a1: HDC; a2: LPCSTR; a3: FONTENUMPROC; a4: LPARAM): int;

<*EXTERNAL EnumFontsW:WINAPI*>
PROCEDURE EnumFontsW (a1: HDC;
                      a2: LPCWSTR;
                      a3: FONTENUMPROC;
                      a4: LPARAM        ): int;
CONST EnumFonts = EnumFontsA;

<*EXTERNAL EnumObjects:WINAPI*>
PROCEDURE EnumObjects (a1: HDC; a2: int; a3: GOBJENUMPROC; a4: LPARAM): int;

<*EXTERNAL EqualRgn:WINAPI*>
PROCEDURE EqualRgn (a1: HRGN; a2: HRGN): BOOL;

<*EXTERNAL Escape:WINAPI*>
PROCEDURE Escape (a1: HDC; a2: int; a3: int; a4: LPCSTR; a5: LPVOID): int;

<*EXTERNAL ExtEscape:WINAPI*>
PROCEDURE ExtEscape (a1: HDC;
                     a2: int;
                     a3: int;
                     a4: LPCSTR;
                     a5: int;
                     a6: LPSTR   ): int;

<*EXTERNAL DrawEscape:WINAPI*>
PROCEDURE DrawEscape (a1: HDC; a2: int; a3: int; a4: LPCSTR): int;

<*EXTERNAL ExcludeClipRect:WINAPI*>
PROCEDURE ExcludeClipRect (a1: HDC; a2: int; a3: int; a4: int; a5: int): int;

<*EXTERNAL ExtCreateRegion:WINAPI*>
PROCEDURE ExtCreateRegion (a1: LPXFORM; a2: DWORD; a3: LPRGNDATA): HRGN;

<*EXTERNAL ExtFloodFill:WINAPI*>
PROCEDURE ExtFloodFill (a1: HDC;
                        a2: int;
                        a3: int;
                        a4: COLORREF;
                        a5: UINT      ): BOOL;

<*EXTERNAL FillRgn:WINAPI*>
PROCEDURE FillRgn (a1: HDC; a2: HRGN; a3: HBRUSH): BOOL;

<*EXTERNAL FloodFill:WINAPI*>
PROCEDURE FloodFill (a1: HDC; a2: int; a3: int; a4: COLORREF): BOOL;

<*EXTERNAL FrameRgn:WINAPI*>
PROCEDURE FrameRgn (a1: HDC; a2: HRGN; a3: HBRUSH; a4: int; a5: int): BOOL;

<*EXTERNAL GetROP2:WINAPI*>
PROCEDURE GetROP2 (a1: HDC): int;

<*EXTERNAL GetAspectRatioFilterEx:WINAPI*>
PROCEDURE GetAspectRatioFilterEx (a1: HDC; a2: LPSIZE): BOOL;

<*EXTERNAL GetBkColor:WINAPI*>
PROCEDURE GetBkColor (a1: HDC): COLORREF;

<*EXTERNAL GetBkMode:WINAPI*>
PROCEDURE GetBkMode (a1: HDC): int;

<*EXTERNAL GetBitmapBits:WINAPI*>
PROCEDURE GetBitmapBits (a1: HBITMAP; a2: LONG; a3: LPVOID): LONG;

<*EXTERNAL GetBitmapDimensionEx:WINAPI*>
PROCEDURE GetBitmapDimensionEx (a1: HBITMAP; a2: LPSIZE): BOOL;

<*EXTERNAL GetBoundsRect:WINAPI*>
PROCEDURE GetBoundsRect (a1: HDC; a2: LPRECT; a3: UINT): UINT;

<*EXTERNAL GetBrushOrgEx:WINAPI*>
PROCEDURE GetBrushOrgEx (a1: HDC; a2: LPPOINT): BOOL;

<*EXTERNAL GetCharWidthA:WINAPI*>
PROCEDURE GetCharWidthA (a1: HDC; a2: UINT; a3: UINT; a4: LPINT): BOOL;

<*EXTERNAL GetCharWidthW:WINAPI*>
PROCEDURE GetCharWidthW (a1: HDC; a2: UINT; a3: UINT; a4: LPINT): BOOL;
CONST GetCharWidth = GetCharWidthA;

<*EXTERNAL GetCharWidth32A:WINAPI*>
PROCEDURE GetCharWidth32A (a1: HDC; a2: UINT; a3: UINT; a4: LPINT): BOOL;

<*EXTERNAL GetCharWidth32W:WINAPI*>
PROCEDURE GetCharWidth32W (a1: HDC; a2: UINT; a3: UINT; a4: LPINT): BOOL;
CONST GetCharWidth32 = GetCharWidth32A;

<*EXTERNAL GetCharWidthFloatA:WINAPI*>
PROCEDURE GetCharWidthFloatA (a1: HDC; a2: UINT; a3: UINT; a4: PFLOAT): BOOL;

<*EXTERNAL GetCharWidthFloatW:WINAPI*>
PROCEDURE GetCharWidthFloatW (a1: HDC; a2: UINT; a3: UINT; a4: PFLOAT): BOOL;
CONST GetCharWidthFloat = GetCharWidthFloatA;

<*EXTERNAL GetCharABCWidthsA:WINAPI*>
PROCEDURE GetCharABCWidthsA (a1: HDC; a2: UINT; a3: UINT; a4: LPABC): BOOL;

<*EXTERNAL GetCharABCWidthsW:WINAPI*>
PROCEDURE GetCharABCWidthsW (a1: HDC; a2: UINT; a3: UINT; a4: LPABC): BOOL;
CONST GetCharABCWidths = GetCharABCWidthsA;

<*EXTERNAL GetCharABCWidthsFloatA:WINAPI*>
PROCEDURE GetCharABCWidthsFloatA (a1: HDC;
                                    a2: UINT;
                                    a3: UINT;
                                    a4: LPABCFLOAT): BOOL;

<*EXTERNAL GetCharABCWidthsFloatW:WINAPI*>
PROCEDURE GetCharABCWidthsFloatW (a1: HDC;
                                    a2: UINT;
                                    a3: UINT;
                                    a4: LPABCFLOAT): BOOL;
CONST GetCharABCWidthsFloat = GetCharABCWidthsFloatA;

<*EXTERNAL GetClipBox:WINAPI*>
PROCEDURE GetClipBox (a1: HDC; a2: LPRECT): int;

<*EXTERNAL GetClipRgn:WINAPI*>
PROCEDURE GetClipRgn (a1: HDC; a2: HRGN): int;

<*EXTERNAL GetMetaRgn:WINAPI*>
PROCEDURE GetMetaRgn (a1: HDC; a2: HRGN): int;

<*EXTERNAL GetCurrentObject:WINAPI*>
PROCEDURE GetCurrentObject (a1: HDC; a2: UINT): HGDIOBJ;

<*EXTERNAL GetCurrentPositionEx:WINAPI*>
PROCEDURE GetCurrentPositionEx (a1: HDC; a2: LPPOINT): BOOL;

<*EXTERNAL GetDeviceCaps:WINAPI*>
PROCEDURE GetDeviceCaps (a1: HDC; a2: int): int;

<*EXTERNAL GetDIBits:WINAPI*>
PROCEDURE GetDIBits (a1: HDC;
                     a2: HBITMAP;
                     a3: UINT;
                     a4: UINT;
                     a5: LPVOID;
                     a6: LPBITMAPINFO;
                     a7: UINT          ): int;

<*EXTERNAL GetFontData:WINAPI*>
PROCEDURE GetFontData (a1: HDC;
                       a2: DWORD;
                       a3: DWORD;
                       a4: LPVOID;
                       a5: DWORD   ): DWORD;

<*EXTERNAL GetGlyphOutline:WINAPI*>
PROCEDURE GetGlyphOutline (a1: HDC;
                           a2: UINT;
                           a3: UINT;
                           a4: LPGLYPHMETRICS;
                           a5: DWORD;
                           a6: LPVOID;
                           a7: UNTRACED REF MAT2): DWORD;

<*EXTERNAL GetMapMode:WINAPI*>
PROCEDURE GetMapMode (a1: HDC): int;

<*EXTERNAL GetMetaFileBitsEx:WINAPI*>
PROCEDURE GetMetaFileBitsEx (a1: HMETAFILE; a2: UINT; a3: LPVOID): UINT;

<*EXTERNAL GetMetaFileA:WINAPI*>
PROCEDURE GetMetaFileA (a1: LPCSTR): HMETAFILE;

<*EXTERNAL GetMetaFileW:WINAPI*>
PROCEDURE GetMetaFileW (a1: LPCWSTR): HMETAFILE;
CONST GetMetaFile = GetMetaFileA;

<*EXTERNAL GetNearestColor:WINAPI*>
PROCEDURE GetNearestColor (a1: HDC; a2: COLORREF): COLORREF;

<*EXTERNAL GetNearestPaletteIndex:WINAPI*>
PROCEDURE GetNearestPaletteIndex (a1: HPALETTE; a2: COLORREF): UINT;

<*EXTERNAL GetObjectType:WINAPI*>
PROCEDURE GetObjectType (h: HGDIOBJ): DWORD;

<*EXTERNAL GetOutlineTextMetricsA:WINAPI*>
PROCEDURE GetOutlineTextMetricsA (a1: HDC;
                                    a2: UINT;
                                    a3: LPOUTLINETEXTMETRICA): UINT;

<*EXTERNAL GetOutlineTextMetricsW:WINAPI*>
PROCEDURE GetOutlineTextMetricsW (a1: HDC;
                                  a2: UINT;
                                  a3: LPOUTLINETEXTMETRICW): UINT;
CONST GetOutlineTextMetrics = GetOutlineTextMetricsA;

<*EXTERNAL GetPaletteEntries:WINAPI*>
PROCEDURE GetPaletteEntries (a1: HPALETTE;
                             a2: UINT;
                             a3: UINT;
                             a4: LPPALETTEENTRY): UINT;

<*EXTERNAL GetPixel:WINAPI*>
PROCEDURE GetPixel (a1: HDC; a2: int; a3: int): COLORREF;

<*EXTERNAL GetPixelFormat:WINAPI*>
PROCEDURE GetPixelFormat (a1: HDC): int;

<*EXTERNAL GetPolyFillMode:WINAPI*>
PROCEDURE GetPolyFillMode (a1: HDC): int;

<*EXTERNAL GetRasterizerCaps:WINAPI*>
PROCEDURE GetRasterizerCaps (a1: LPRASTERIZER_STATUS; a2: UINT): BOOL;

<*EXTERNAL GetRegionData:WINAPI*>
PROCEDURE GetRegionData (a1: HRGN; a2: DWORD; a3: LPRGNDATA): DWORD;

<*EXTERNAL GetRgnBox:WINAPI*>
PROCEDURE GetRgnBox (a1: HRGN; a2: LPRECT): int;

<*EXTERNAL GetStockObject:WINAPI*>
PROCEDURE GetStockObject (a1: int): HGDIOBJ;

<*EXTERNAL GetStretchBltMode:WINAPI*>
PROCEDURE GetStretchBltMode (a1: HDC): int;

<*EXTERNAL GetSystemPaletteEntries:WINAPI*>
PROCEDURE GetSystemPaletteEntries (a1: HDC;
                                     a2: UINT;
                                     a3: UINT;
                                     a4: LPPALETTEENTRY): UINT;

<*EXTERNAL GetSystemPaletteUse:WINAPI*>
PROCEDURE GetSystemPaletteUse (a1: HDC): UINT;

<*EXTERNAL GetTextCharacterExtra:WINAPI*>
PROCEDURE GetTextCharacterExtra (a1: HDC): int;

<*EXTERNAL GetTextAlign:WINAPI*>
PROCEDURE GetTextAlign (a1: HDC): UINT;

<*EXTERNAL GetTextColor:WINAPI*>
PROCEDURE GetTextColor (a1: HDC): COLORREF;

<*EXTERNAL GetTextExtentPointA:WINAPI*>
PROCEDURE GetTextExtentPointA (a1: HDC; a2: LPCSTR; a3: int; a4: LPSIZE): BOOL;

<*EXTERNAL GetTextExtentPointW:WINAPI*>
PROCEDURE GetTextExtentPointW (a1: HDC; a2: LPCWSTR; a3: int; a4: LPSIZE): BOOL;
CONST GetTextExtentPoint = GetTextExtentPointA;

<*EXTERNAL GetTextExtentPoint32A:WINAPI*>
PROCEDURE GetTextExtentPoint32A (a1: HDC; a2: LPCSTR; a3: int; a4: LPSIZE): BOOL;

<*EXTERNAL GetTextExtentPoint32W:WINAPI*>
PROCEDURE GetTextExtentPoint32W (a1: HDC; a2: LPCWSTR; a3: int; a4: LPSIZE): BOOL;
CONST GetTextExtentPoint32 = GetTextExtentPoint32A;

<*EXTERNAL GetTextExtentExPointA:WINAPI*>
PROCEDURE GetTextExtentExPointA (a1: HDC;
                                 a2: LPCSTR;
                                 a3: int;
                                 a4: int;
                                 a5: LPINT;
                                 a6: LPINT;
                                 a7: LPSIZE  ): BOOL;

<*EXTERNAL GetTextExtentExPointW:WINAPI*>
PROCEDURE GetTextExtentExPointW (a1: HDC;
                                 a2: LPCWSTR;
                                 a3: int;
                                 a4: int;
                                 a5: LPINT;
                                 a6: LPINT;
                                 a7: LPSIZE   ): BOOL;
CONST GetTextExtentExPoint = GetTextExtentExPointA;

<*EXTERNAL GetViewportExtEx:WINAPI*>
PROCEDURE GetViewportExtEx (a1: HDC; a2: LPSIZE): BOOL;

<*EXTERNAL GetViewportOrgEx:WINAPI*>
PROCEDURE GetViewportOrgEx (a1: HDC; a2: LPPOINT): BOOL;

<*EXTERNAL GetWindowExtEx:WINAPI*>
PROCEDURE GetWindowExtEx (a1: HDC; a2: LPSIZE): BOOL;

<*EXTERNAL GetWindowOrgEx:WINAPI*>
PROCEDURE GetWindowOrgEx (a1: HDC; a2: LPPOINT): BOOL;

<*EXTERNAL IntersectClipRect:WINAPI*>
PROCEDURE IntersectClipRect (a1: HDC; a2: int; a3: int; a4: int; a5: int): int;

<*EXTERNAL InvertRgn:WINAPI*>
PROCEDURE InvertRgn (a1: HDC; a2: HRGN): BOOL;

<*EXTERNAL LineDDA:WINAPI*>
PROCEDURE LineDDA (a1, a2, a3, a4: int; a5: LINEDDAPROC;  a6: LPARAM): BOOL;

<*EXTERNAL LineTo:WINAPI*>
PROCEDURE LineTo (a1: HDC; a2: int; a3: int): BOOL;

<*EXTERNAL MaskBlt:WINAPI*>
PROCEDURE MaskBlt (a1: HDC;  a2, a3, a4, a5: int;  a6: HDC;
                   a7, a8: int;  a9: HBITMAP;  a10, a11: int;
                   a12: DWORD): BOOL;

<*EXTERNAL PlgBlt:WINAPI*>
PROCEDURE PlgBlt (a1: HDC;  a2: LPPOINT;  a3: HDC;  a4, a5, a6, a7: int;
                  a8: HBITMAP;   a9, a10: int): BOOL;

<*EXTERNAL OffsetClipRgn:WINAPI*>
PROCEDURE OffsetClipRgn (a1: HDC; a2: int; a3: int): int;

<*EXTERNAL OffsetRgn:WINAPI*>
PROCEDURE OffsetRgn (a1: HRGN; a2: int; a3: int): int;

<*EXTERNAL PatBlt:WINAPI*>
PROCEDURE PatBlt (a1: HDC;  a2, a3, a4, a5: int;  a6: DWORD): BOOL;

<*EXTERNAL Pie:WINAPI*>
PROCEDURE Pie (a1: HDC; a2, a3, a4, a5, a6, a7, a8, a9: int): BOOL;

<*EXTERNAL PlayMetaFile:WINAPI*>
PROCEDURE PlayMetaFile (a1: HDC; a2: HMETAFILE): BOOL;

<*EXTERNAL PaintRgn:WINAPI*>
PROCEDURE PaintRgn (a1: HDC; a2: HRGN): BOOL;

<*EXTERNAL PolyPolygon:WINAPI*>
PROCEDURE PolyPolygon (a1: HDC;  a2: LPPOINT;  a3: LPINT;  a4: int): BOOL;

<*EXTERNAL PtInRegion:WINAPI*>
PROCEDURE PtInRegion (a1: HRGN; a2: int; a3: int): BOOL;

<*EXTERNAL PtVisible:WINAPI*>
PROCEDURE PtVisible (a1: HDC; a2: int; a3: int): BOOL;

<*EXTERNAL RectInRegion:WINAPI*>
PROCEDURE RectInRegion (a1: HRGN; a2: LPRECT): BOOL;

<*EXTERNAL RectVisible:WINAPI*>
PROCEDURE RectVisible (a1: HDC; a2: LPRECT): BOOL;

<*EXTERNAL Rectangle:WINAPI*>
PROCEDURE Rectangle (a1: HDC; a2: int; a3: int; a4: int; a5: int): BOOL;

<*EXTERNAL RestoreDC:WINAPI*>
PROCEDURE RestoreDC (a1: HDC; a2: int): BOOL;

<*EXTERNAL RealizePalette:WINAPI*>
PROCEDURE RealizePalette (a1: HDC): UINT;

<*EXTERNAL RemoveFontModule:WINAPI*>
PROCEDURE RemoveFontModule (a1: HMODULE): BOOL;

<*EXTERNAL RemoveFontResourceA:WINAPI*>
PROCEDURE RemoveFontResourceA (a1: LPSTR): BOOL;

<*EXTERNAL RemoveFontResourceW:WINAPI*>
PROCEDURE RemoveFontResourceW (a1: LPWSTR): BOOL;
CONST RemoveFontResource = RemoveFontResourceA;

<*EXTERNAL RoundRect:WINAPI*>
PROCEDURE RoundRect (a1: HDC;  a2, a3, a4, a5, a6, a7: int): BOOL;

<*EXTERNAL ResizePalette:WINAPI*>
PROCEDURE ResizePalette (a1: HPALETTE; a2: UINT): BOOL;

<*EXTERNAL SaveDC:WINAPI*>
PROCEDURE SaveDC (a1: HDC): int;

<*EXTERNAL SelectClipRgn:WINAPI*>
PROCEDURE SelectClipRgn (a1: HDC; a2: HRGN): int;

<*EXTERNAL ExtSelectClipRgn:WINAPI*>
PROCEDURE ExtSelectClipRgn (a1: HDC; a2: HRGN; a3: int): int;

<*EXTERNAL SetMetaRgn:WINAPI*>
PROCEDURE SetMetaRgn (a1: HDC): int;

<*EXTERNAL SelectObject:WINAPI*>
PROCEDURE SelectObject (a1: HDC; a2: HGDIOBJ): HGDIOBJ;

<*EXTERNAL SelectPalette:WINAPI*>
PROCEDURE SelectPalette (a1: HDC; a2: HPALETTE; a3: BOOL): HPALETTE;

<*EXTERNAL SetBkColor:WINAPI*>
PROCEDURE SetBkColor (a1: HDC; a2: COLORREF): COLORREF;

<*EXTERNAL SetBkMode:WINAPI*>
PROCEDURE SetBkMode (a1: HDC; a2: int): int;

<*EXTERNAL SetBitmapBits:WINAPI*>
PROCEDURE SetBitmapBits (a1: HBITMAP; a2: DWORD; a3: void_star): LONG;

<*EXTERNAL SetBoundsRect:WINAPI*>
PROCEDURE SetBoundsRect (a1: HDC; a2: LPRECT; a3: UINT): UINT;

<*EXTERNAL SetDIBits:WINAPI*>
PROCEDURE SetDIBits (a1: HDC;
                     a2: HBITMAP;
                     a3: UINT;
                     a4: UINT;
                     a5: void_star;
                     a6: LPBITMAPINFO;
                     a7: UINT          ): int;

<*EXTERNAL SetDIBitsToDevice:WINAPI*>
PROCEDURE SetDIBitsToDevice (a1 : HDC;
                             a2 : int;
                             a3 : int;
                             a4 : DWORD;
                             a5 : DWORD;
                             a6 : int;
                             a7 : int;
                             a8 : UINT;
                             a9 : UINT;
                             a10: LPVOID;
                             a11: LPBITMAPINFO;
                             a12: UINT          ): int;

<*EXTERNAL SetMapperFlags:WINAPI*>
PROCEDURE SetMapperFlags (a1: HDC; a2: DWORD): DWORD;

<*EXTERNAL SetGraphicsMode:WINAPI*>
PROCEDURE SetGraphicsMode(hdc: HDC; iMode: int): int;

<*EXTERNAL SetMapMode:WINAPI*>
PROCEDURE SetMapMode (a1: HDC; a2: int): int;

<*EXTERNAL SetMetaFileBitsEx:WINAPI*>
PROCEDURE SetMetaFileBitsEx (a1: UINT; a2: LPBYTE): HMETAFILE;

<*EXTERNAL SetPaletteEntries:WINAPI*>
PROCEDURE SetPaletteEntries (a1: HPALETTE;
                             a2: UINT;
                             a3: UINT;
                             a4: UNTRACED REF PALETTEENTRY): UINT;

<*EXTERNAL SetPixel:WINAPI*>
PROCEDURE SetPixel (a1: HDC; a2: int; a3: int; a4: COLORREF): COLORREF;

<*EXTERNAL SetPixelV:WINAPI*>
PROCEDURE SetPixelV (a1: HDC; a2: int; a3: int; a4: COLORREF): BOOL;

<*EXTERNAL SetPixelFormat:WINAPI*>
PROCEDURE SetPixelFormat (a1: HDC; a2: int; a3: LPPIXELFORMATDESCRIPTOR): BOOL;

<*EXTERNAL SetPolyFillMode:WINAPI*>
PROCEDURE SetPolyFillMode (a1: HDC; a2: int): int;

<*EXTERNAL StretchBlt:WINAPI*>
PROCEDURE StretchBlt (a1: HDC;  a2, a3, a4, a5: int;  a6: HDC;
                      a7, a8, a9, a10: int;  a11: DWORD): BOOL;

<*EXTERNAL SetRectRgn:WINAPI*>
PROCEDURE raw_SetRectRgn (a1: HRGN; a2: int; a3: int; a4: int; a5: int): BOOL;

PROCEDURE SetRectRgn (a1: HRGN; a2: int; a3: int; a4: int; a5: int): BOOL;

<*EXTERNAL StretchDIBits:WINAPI*>
PROCEDURE StretchDIBits (a1: HDC;  a2, a3, a4, a5, a6, a7, a8, a9: int;
                         a10: void_star;
                         a11: LPBITMAPINFO;
                         a12: UINT;
                         a13: DWORD         ): int;

<*EXTERNAL SetROP2:WINAPI*>
PROCEDURE SetROP2 (a1: HDC; a2: int): int;

<*EXTERNAL SetStretchBltMode:WINAPI*>
PROCEDURE SetStretchBltMode (a1: HDC; a2: int): int;

<*EXTERNAL SetSystemPaletteUse:WINAPI*>
PROCEDURE SetSystemPaletteUse (a1: HDC; a2: UINT): UINT;

<*EXTERNAL SetTextCharacterExtra:WINAPI*>
PROCEDURE SetTextCharacterExtra (a1: HDC; a2: int): int;

<*EXTERNAL SetTextColor:WINAPI*>
PROCEDURE SetTextColor (a1: HDC; a2: COLORREF): COLORREF;

<*EXTERNAL SetTextAlign:WINAPI*>
PROCEDURE SetTextAlign (a1: HDC; a2: UINT): UINT;

<*EXTERNAL SetTextJustification:WINAPI*>
PROCEDURE SetTextJustification (a1: HDC; a2: int; a3: int): BOOL;

<*EXTERNAL UpdateColors:WINAPI*>
PROCEDURE UpdateColors (a1: HDC): BOOL;

<*EXTERNAL PlayMetaFileRecord:WINAPI*>
PROCEDURE PlayMetaFileRecord (a1: HDC;
                              a2: LPHANDLETABLE;
                              a3: LPMETARECORD;
                              a4: UINT           ): BOOL;

TYPE
  MFENUMPROC = <*CALLBACK*> PROCEDURE (a1: HDC;
                                       a2: UNTRACED REF HANDLETABLE;
                                       a3: UNTRACED REF METARECORD;
                                       a4: int;
                                       a5: LPARAM                    ): int;

<*EXTERNAL EnumMetaFile:WINAPI*>
PROCEDURE EnumMetaFile (a1: HDC;
                        a2: HMETAFILE;
                        a3: MFENUMPROC;
                        a4: LPARAM      ): BOOL;

TYPE
  ENHMFENUMPROC = <*CALLBACK*> PROCEDURE (a1: HDC;
                                          a2: UNTRACED REF HANDLETABLE;
                                          a3: UNTRACED REF ENHMETARECORD;
                                          a4: int;
                                          a5: LPARAM                    ): int;

(* Enhanced Metafile Function Declarations *)

<*EXTERNAL CloseEnhMetaFile:WINAPI*>
PROCEDURE CloseEnhMetaFile (a1: HDC): HENHMETAFILE;

<*EXTERNAL CopyEnhMetaFileA:WINAPI*>
PROCEDURE CopyEnhMetaFileA (a1: HENHMETAFILE; a2: LPSTR): HENHMETAFILE;

<*EXTERNAL CopyEnhMetaFileW:WINAPI*>
PROCEDURE CopyEnhMetaFileW (a1: HENHMETAFILE; a2: LPWSTR): HENHMETAFILE;
CONST CopyEnhMetaFile = CopyEnhMetaFileA;

<*EXTERNAL CreateEnhMetaFileA:WINAPI*>
PROCEDURE CreateEnhMetaFileA (a1: HDC; a2: LPSTR; a3: LPRECT; a4: LPSTR): HDC;

<*EXTERNAL CreateEnhMetaFileW:WINAPI*>
PROCEDURE CreateEnhMetaFileW (a1: HDC; a2: LPWSTR; a3: LPRECT; a4: LPWSTR): HDC;
CONST CreateEnhMetaFile = CreateEnhMetaFileA;

<*EXTERNAL DeleteEnhMetaFile:WINAPI*>
PROCEDURE DeleteEnhMetaFile (a1: HENHMETAFILE): BOOL;

<*EXTERNAL EnumEnhMetaFile:WINAPI*>
PROCEDURE EnumEnhMetaFile (a1: HDC;
                             a2: HENHMETAFILE;
                             a3: ENHMFENUMPROC;
                             a4: LPVOID;
                             a5: LPRECT         ): BOOL;

<*EXTERNAL GetEnhMetaFileA:WINAPI*>
PROCEDURE GetEnhMetaFileA (a1: LPSTR): HENHMETAFILE;

<*EXTERNAL GetEnhMetaFileW:WINAPI*>
PROCEDURE GetEnhMetaFileW (a1: LPWSTR): HENHMETAFILE;
CONST GetEnhMetaFile = GetEnhMetaFileA;

<*EXTERNAL GetEnhMetaFileBits:WINAPI*>
PROCEDURE GetEnhMetaFileBits (a1: HENHMETAFILE; a2: UINT; a3: LPBYTE): UINT;

<*EXTERNAL GetEnhMetaFileDescriptionA:WINAPI*>
PROCEDURE GetEnhMetaFileDescriptionA (a1: HENHMETAFILE;
                                      a2: UINT;
                                      a3: LPSTR         ): UINT;

<*EXTERNAL GetEnhMetaFileDescriptionW:WINAPI*>
PROCEDURE GetEnhMetaFileDescriptionW (a1: HENHMETAFILE;
                                      a2: UINT;
                                      a3: LPWSTR        ): UINT;
CONST GetEnhMetaFileDescription = GetEnhMetaFileDescriptionA;

<*EXTERNAL GetEnhMetaFileHeader:WINAPI*>
PROCEDURE GetEnhMetaFileHeader (a1: HENHMETAFILE;
                                a2: UINT;
                                a3: LPENHMETAHEADER): UINT;

<*EXTERNAL GetEnhMetaFilePaletteEntries:WINAPI*>
PROCEDURE GetEnhMetaFilePaletteEntries (a1: HENHMETAFILE;
                                        a2: UINT;
                                        a3: LPPALETTEENTRY): UINT;

<*EXTERNAL GetWinMetaFileBits:WINAPI*>
PROCEDURE GetWinMetaFileBits (a1: HENHMETAFILE;
                              a2: UINT;
                              a3: LPBYTE;
                              a4: INT;
                              a5: HDC           ): UINT;

<*EXTERNAL PlayEnhMetaFile:WINAPI*>
PROCEDURE PlayEnhMetaFile (a1: HDC; a2: HENHMETAFILE; a3: LPRECT): BOOL;

<*EXTERNAL PlayEnhMetaFileRecord:WINAPI*>
PROCEDURE PlayEnhMetaFileRecord (a1: HDC;
                                 a2: LPHANDLETABLE;
                                 a3: LPENHMETARECORD;
                                 a4: UINT             ): BOOL;

<*EXTERNAL SetEnhMetaFileBits:WINAPI*>
PROCEDURE SetEnhMetaFileBits (a1: UINT; a2: LPBYTE): HENHMETAFILE;

<*EXTERNAL SetWinMetaFileBits:WINAPI*>
PROCEDURE SetWinMetaFileBits (a1: UINT;
                              a2: LPBYTE;
                              a3: HDC;
                              a4: LPMETAFILEPICT): HENHMETAFILE;

<*EXTERNAL GdiComment:WINAPI*>
PROCEDURE GdiComment (a1: HDC; a2: UINT; a3: LPBYTE): BOOL;

<*EXTERNAL GetTextMetricsA:WINAPI*>
PROCEDURE GetTextMetricsA (a1: HDC; a2: LPTEXTMETRICA): BOOL;

<*EXTERNAL GetTextMetricsW:WINAPI*>
PROCEDURE GetTextMetricsW (a1: HDC; a2: LPTEXTMETRICW): BOOL;
CONST GetTextMetrics = GetTextMetricsA;

(* new GDI *)

<*EXTERNAL AngleArc:WINAPI*>
PROCEDURE AngleArc (a1: HDC; a2, a3: int;  a4: DWORD;  a5, a6: WFLOAT): BOOL;

<*EXTERNAL PolyPolyline:WINAPI*>
PROCEDURE PolyPolyline (a1: HDC; a2: LPPOINT; a3: LPDWORD; a4: DWORD): BOOL;

<*EXTERNAL GetWorldTransform:WINAPI*>
PROCEDURE GetWorldTransform (a1: HDC; a2: LPXFORM): BOOL;

<*EXTERNAL SetWorldTransform:WINAPI*>
PROCEDURE SetWorldTransform (a1: HDC; a2: LPXFORM): BOOL;

<*EXTERNAL ModifyWorldTransform:WINAPI*>
PROCEDURE ModifyWorldTransform (a1: HDC; a2: LPXFORM; a3: DWORD): BOOL;

<*EXTERNAL CombineTransform:WINAPI*>
PROCEDURE CombineTransform (a1: LPXFORM; a2: LPXFORM; a3: LPXFORM): BOOL;

(* Flags value for COLORADJUSTMENT *)
CONST
  CA_NEGATIVE   = 16_0001;
  CA_LOG_FILTER = 16_0002;

(* IlluminantIndex values *)
CONST
  ILLUMINANT_EQUAL_ENERGY = 0;
  ILLUMINANT_A            = 1;
  ILLUMINANT_B            = 2;
  ILLUMINANT_C            = 3;
  ILLUMINANT_D50          = 4;
  ILLUMINANT_D55          = 5;
  ILLUMINANT_D65          = 6;
  ILLUMINANT_D75          = 7;
  ILLUMINANT_F2           = 8;
  ILLUMINANT_MAX_INDEX    = ILLUMINANT_F2;

  ILLUMINANT_TURNGSTEN   = ILLUMINANT_A;
  ILLUMINANT_DAYLIGHT    = ILLUMINANT_C;
  ILLUMINANT_FLUORESCENT = ILLUMINANT_F2;
  ILLUMINANT_NTSC        = ILLUMINANT_C;

(* Min and max for RedGamma, GreenGamma, BlueGamma *)
CONST
  RGB_GAMMA_MIN: WORD = 8_02500;
  RGB_GAMMA_MAX: WORD = 65000;

(* Min and max for ReferenceBlack and ReferenceWhite *)
CONST
  REFERENCE_WHITE_MIN: WORD = 6000;
  REFERENCE_WHITE_MAX: WORD = 10000;
  REFERENCE_BLACK_MIN: WORD = 0;
  REFERENCE_BLACK_MAX: WORD = 4000;

(* Min and max for Contrast, Brightness, Colorfulness, RedGreenTint *)
CONST
  COLOR_ADJ_MIN: SHORT = -100;
  COLOR_ADJ_MAX: SHORT = 100;

TYPE
  COLORADJUSTMENT = RECORD
    caSize           : WORD;
    caFlags          : WORD;
    caIlluminantIndex: WORD;
    caRedGamma       : WORD;
    caGreenGamma     : WORD;
    caBlueGamma      : WORD;
    caReferenceBlack : WORD;
    caReferenceWhite : WORD;
    caContrast       : SHORT;
    caBrightness     : SHORT;
    caColorfulness   : SHORT;
    caRedGreenTint   : SHORT;
  END;
  PCOLORADJUSTMENT = COLORADJUSTMENT;
  LPCOLORADJUSTMENT = COLORADJUSTMENT;

<*EXTERNAL SetColorAdjustment:WINAPI*>
PROCEDURE SetColorAdjustment (a1: HDC; a2: LPCOLORADJUSTMENT): BOOL;

<*EXTERNAL GetColorAdjustment:WINAPI*>
PROCEDURE GetColorAdjustment (a1: HDC; a2: LPCOLORADJUSTMENT): BOOL;

<*EXTERNAL CreateHalftonePalette:WINAPI*>
PROCEDURE CreateHalftonePalette (a1: HDC): HPALETTE;

TYPE
  ABORTPROC = <*CALLBACK*> PROCEDURE (a1: HDC; a2: int): BOOL;

  LPDOCINFOA = UNTRACED REF DOCINFOA;
  DOCINFOA = RECORD
    cbSize     : int;
    lpszDocName: LPSTR;
    lpszOutput : LPSTR;
  END;

  LPDOCINFOW = UNTRACED REF DOCINFOW;
  DOCINFOW = RECORD
    cbSize     : int;
    lpszDocName: LPWSTR;
    lpszOutput : LPWSTR;
  END;

  DOCINFO = DOCINFOA;
  LPDOCINFO = LPDOCINFOA;

<*EXTERNAL StartDocA:WINAPI*>
PROCEDURE StartDocA (a1: HDC; a2: LPDOCINFOA): int;

<*EXTERNAL StartDocW:WINAPI*>
PROCEDURE StartDocW (a1: HDC; a2: LPDOCINFOW): int;
CONST StartDoc = StartDocA;

<*EXTERNAL EndDoc:WINAPI*>
PROCEDURE EndDoc (a1: HDC): int;

<*EXTERNAL StartPage:WINAPI*>
PROCEDURE StartPage (a1: HDC): int;

<*EXTERNAL EndPage:WINAPI*>
PROCEDURE EndPage (a1: HDC): int;

<*EXTERNAL AbortDoc:WINAPI*>
PROCEDURE AbortDoc (a1: HDC): int;

<*EXTERNAL SetAbortProc:WINAPI*>
PROCEDURE SetAbortProc (a1: HDC; a2: ABORTPROC): int;

<*EXTERNAL GdiPlayJournal:WINAPI*>
PROCEDURE GdiPlayJournal (a1: HDC; a2: LPCSTR; a3: DWORD; a4: DWORD): BOOL;

<*EXTERNAL AbortPath:WINAPI*>
PROCEDURE AbortPath (a1: HDC): BOOL;

<*EXTERNAL ArcTo:WINAPI*>
PROCEDURE ArcTo (a1: HDC; a2, a3, a4, a5, a6, a7, a8, a9: int): BOOL;

<*EXTERNAL BeginPath:WINAPI*>
PROCEDURE BeginPath (a1: HDC): BOOL;

<*EXTERNAL CloseFigure:WINAPI*>
PROCEDURE CloseFigure (a1: HDC): BOOL;

<*EXTERNAL EndPath:WINAPI*>
PROCEDURE EndPath (a1: HDC): BOOL;

<*EXTERNAL FillPath:WINAPI*>
PROCEDURE FillPath (a1: HDC): BOOL;

<*EXTERNAL FlattenPath:WINAPI*>
PROCEDURE FlattenPath (a1: HDC): BOOL;

<*EXTERNAL GetPath:WINAPI*>
PROCEDURE GetPath (a1: HDC; a2: LPPOINT; a3: LPBYTE; a4: int): int;

<*EXTERNAL PathToRegion:WINAPI*>
PROCEDURE PathToRegion (a1: HDC): HRGN;

<*EXTERNAL PolyDraw:WINAPI*>
PROCEDURE PolyDraw (a1: HDC; a2: LPPOINT; a3: LPBYTE; a4: int): BOOL;

<*EXTERNAL SelectClipPath:WINAPI*>
PROCEDURE SelectClipPath (a1: HDC; a2: int): BOOL;

<*EXTERNAL SetArcDirection:WINAPI*>
PROCEDURE SetArcDirection (a1: HDC; a2: int): int;

<*EXTERNAL SetMiterLimit:WINAPI*>
PROCEDURE SetMiterLimit (a1: HDC; a2: WFLOAT; a3: PFLOAT): BOOL;

<*EXTERNAL StrokeAndFillPath:WINAPI*>
PROCEDURE StrokeAndFillPath (a1: HDC): BOOL;

<*EXTERNAL StrokePath:WINAPI*>
PROCEDURE StrokePath (a1: HDC): BOOL;

<*EXTERNAL WidenPath:WINAPI*>
PROCEDURE WidenPath (a1: HDC): BOOL;

<*EXTERNAL ExtCreatePen:WINAPI*>
PROCEDURE ExtCreatePen (a1: DWORD;
                        a2: DWORD;
                        a3: LPLOGBRUSH;
                        a4: DWORD;
                        a5: LPDWORD     ): HPEN;

<*EXTERNAL GetMiterLimit:WINAPI*>
PROCEDURE GetMiterLimit (a1: HDC; a2: PFLOAT): BOOL;

<*EXTERNAL GetArcDirection:WINAPI*>
PROCEDURE GetArcDirection (a1: HDC): int;

<*EXTERNAL GetObjectA:WINAPI*>
PROCEDURE GetObjectA (a1: HGDIOBJ; a2: int; a3: LPVOID): int;

<*EXTERNAL GetObjectW:WINAPI*>
PROCEDURE GetObjectW (a1: HGDIOBJ; a2: int; a3: LPVOID): int;
CONST GetObject = GetObjectA;

<*EXTERNAL MoveToEx:WINAPI*>
PROCEDURE MoveToEx (a1: HDC; a2: int; a3: int; a4: LPPOINT): BOOL;

<*EXTERNAL TextOutA:WINAPI*>
PROCEDURE TextOutA (a1: HDC; a2: int; a3: int; a4: LPCSTR; a5: int): BOOL;

<*EXTERNAL TextOutW:WINAPI*>
PROCEDURE TextOutW (a1: HDC; a2: int; a3: int; a4: LPCWSTR; a5: int): BOOL;
CONST TextOut = TextOutA;

<*EXTERNAL ExtTextOutA:WINAPI*>
PROCEDURE ExtTextOutA (a1: HDC;
                       a2: int;
                       a3: int;
                       a4: UINT;
                       a5: LPRECT;
                       a6: LPCSTR;
                       a7: UINT;
                       a8: LPINT              ): BOOL;

<*EXTERNAL ExtTextOutW:WINAPI*>
PROCEDURE ExtTextOutW (a1: HDC;
                       a2: int;
                       a3: int;
                       a4: UINT;
                       a5: LPRECT;
                       a6: LPCWSTR;
                       a7: UINT;
                       a8: LPINT              ): BOOL;
CONST ExtTextOut = ExtTextOutA;

<*EXTERNAL PolyTextOutA:WINAPI*>
PROCEDURE PolyTextOutA (a1: HDC; a2: UNTRACED REF POLYTEXTA; a3: int): BOOL;

<*EXTERNAL PolyTextOutW:WINAPI*>
PROCEDURE PolyTextOutW (a1: HDC; a2: UNTRACED REF POLYTEXTW; a3: int): BOOL;
CONST PolyTextOut = PolyTextOutA;

<*EXTERNAL CreatePolygonRgn:WINAPI*>
PROCEDURE CreatePolygonRgn (a1: LPPOINT; a2: int; a3: int): HRGN;

<*EXTERNAL DPtoLP:WINAPI*>
PROCEDURE DPtoLP (a1: HDC; a2: LPPOINT; a3: int): BOOL;

<*EXTERNAL LPtoDP:WINAPI*>
PROCEDURE LPtoDP (a1: HDC; a2: LPPOINT; a3: int): BOOL;

<*EXTERNAL Polygon:WINAPI*>
PROCEDURE Polygon (a1: HDC; a2: LPPOINT; a3: int): BOOL;

<*EXTERNAL Polyline:WINAPI*>
PROCEDURE Polyline (a1: HDC; a2: LPPOINT; a3: int): BOOL;

<*EXTERNAL PolyBezier:WINAPI*>
PROCEDURE PolyBezier (a1: HDC; a2: LPPOINT; a3: DWORD): BOOL;

<*EXTERNAL PolyBezierTo:WINAPI*>
PROCEDURE PolyBezierTo (a1: HDC; a2: LPPOINT; a3: DWORD): BOOL;

<*EXTERNAL PolylineTo:WINAPI*>
PROCEDURE PolylineTo (a1: HDC; a2: LPPOINT; a3: DWORD): BOOL;

<*EXTERNAL SetViewportExtEx:WINAPI*>
PROCEDURE SetViewportExtEx (a1: HDC; a2: int; a3: int; a4: LPSIZE): BOOL;

<*EXTERNAL SetViewportOrgEx:WINAPI*>
PROCEDURE SetViewportOrgEx (a1: HDC; a2: int; a3: int; a4: LPPOINT): BOOL;

<*EXTERNAL SetWindowExtEx:WINAPI*>
PROCEDURE SetWindowExtEx (a1: HDC; a2: int; a3: int; a4: LPSIZE): BOOL;

<*EXTERNAL SetWindowOrgEx:WINAPI*>
PROCEDURE SetWindowOrgEx (a1: HDC; a2: int; a3: int; a4: LPPOINT): BOOL;

<*EXTERNAL OffsetViewportOrgEx:WINAPI*>
PROCEDURE OffsetViewportOrgEx (a1: HDC; a2: int; a3: int; a4: LPPOINT): BOOL;

<*EXTERNAL OffsetWindowOrgEx:WINAPI*>
PROCEDURE OffsetWindowOrgEx (a1: HDC; a2: int; a3: int; a4: LPPOINT): BOOL;

<*EXTERNAL ScaleViewportExtEx:WINAPI*>
PROCEDURE ScaleViewportExtEx (a1: HDC;
                              a2: int;
                              a3: int;
                              a4: int;
                              a5: int;
                              a6: LPSIZE): BOOL;

<*EXTERNAL ScaleWindowExtEx:WINAPI*>
PROCEDURE ScaleWindowExtEx (a1: HDC;
                            a2: int;
                            a3: int;
                            a4: int;
                            a5: int;
                            a6: LPSIZE): BOOL;

<*EXTERNAL SetBitmapDimensionEx:WINAPI*>
PROCEDURE SetBitmapDimensionEx (a1: HBITMAP; a2: int; a3: int; a4: LPSIZE): BOOL;

<*EXTERNAL SetBrushOrgEx:WINAPI*>
PROCEDURE SetBrushOrgEx (a1: HDC; a2: int; a3: int; a4: LPPOINT): BOOL;

<*EXTERNAL GetTextFaceA:WINAPI*>
PROCEDURE GetTextFaceA (a1: HDC; a2: int; a3: LPSTR): int;

<*EXTERNAL GetTextFaceW:WINAPI*>
PROCEDURE GetTextFaceW (a1: HDC; a2: int; a3: LPWSTR): int;
CONST GetTextFace = GetTextFaceA;

CONST FONTMAPPER_MAX = 10;

<*EXTERNAL EnumNearestFontsA:WINAPI*>
PROCEDURE EnumNearestFontsA (a1: HDC;
                             a2: LPEXTLOGFONTA;
                             a3: DWORD;
                             a4: LPFMATCHA      ): DWORD;

<*EXTERNAL EnumNearestFontsW:WINAPI*>
PROCEDURE EnumNearestFontsW (a1: HDC;
                             a2: LPEXTLOGFONTW;
                             a3: DWORD;
                             a4: LPFMATCHW      ): DWORD;
CONST EnumNearestFonts = EnumNearestFontsA;

<*EXTERNAL SetFontMapperControls:WINAPI*>
PROCEDURE SetFontMapperControls (a1: LPFMCONTROLS): BOOL;

<*EXTERNAL GetFontMapperControls:WINAPI*>
PROCEDURE GetFontMapperControls (a1: LPFMCONTROLS; a2: DWORD): BOOL;

<*EXTERNAL ExtCreateFontIndirectA:WINAPI*>
PROCEDURE ExtCreateFontIndirectA (a1: LPEXTLOGFONTA): HFONT;

<*EXTERNAL ExtCreateFontIndirectW:WINAPI*>
PROCEDURE ExtCreateFontIndirectW (a1: LPEXTLOGFONTW): HFONT;

CONST ExtCreateFontIndirect = ExtCreateFontIndirectA;

TYPE
  LPKERNINGPAIR = UNTRACED REF KERNINGPAIR;
  KERNINGPAIR = RECORD
    wFirst     : WORD;
    wSecond    : WORD;
    iKernAmount: int;
  END;

<*EXTERNAL GetKerningPairs:WINAPI*>
PROCEDURE GetKerningPairs (a1: HDC; a2: DWORD; a3: LPKERNINGPAIR): DWORD;

<*EXTERNAL GetDCOrg:WINAPI*>
PROCEDURE GetDCOrg (a1: HDC): DWORD;

<*EXTERNAL FixBrushOrgEx:WINAPI*>
PROCEDURE FixBrushOrgEx (a1: HDC; a2: int; a3: int; a4: LPPOINT): BOOL;

<*EXTERNAL UnrealizeObject:WINAPI*>
PROCEDURE UnrealizeObject (a1: HGDIOBJ): BOOL;

<*EXTERNAL GdiFlush:WINAPI*>
PROCEDURE GdiFlush (): BOOL;

<*EXTERNAL GdiSetBatchLimit:WINAPI*>
PROCEDURE GdiSetBatchLimit (a1: DWORD): DWORD;

<*EXTERNAL GdiGetBatchLimit:WINAPI*>
PROCEDURE GdiGetBatchLimit (): DWORD;


(* OpenGL wgl prototypes *)

<*EXTERNAL wglCreateContext:WINAPI*>
PROCEDURE wglCreateContext (a1: HDC): HGLRC;

<*EXTERNAL wglDeleteContext:WINAPI*>
PROCEDURE wglDeleteContext (a1: HGLRC): BOOL;

<*EXTERNAL wglGetCurrentContext:WINAPI*>
PROCEDURE wglGetCurrentContext (): HGLRC;

<*EXTERNAL wglGetCurrentDC:WINAPI*>
PROCEDURE wglGetCurrentDC (): HDC;

<*EXTERNAL wglMakeCurrent:WINAPI*>
PROCEDURE wglMakeCurrent (a1: HDC; a2: HGLRC): BOOL;

<*EXTERNAL wglUseFontBitmapsA:WINAPI*>
PROCEDURE wglUseFontBitmapsA (a1: HDC; a2: DWORD; a3: DWORD; a4: DWORD): BOOL;

<*EXTERNAL wglUseFontBitmapsW:WINAPI*>
PROCEDURE wglUseFontBitmapsW (a1: HDC; a2: DWORD; a3: DWORD; a4: DWORD): BOOL;

CONST wglUseFontBitmaps = wglUseFontBitmapsA;

<*EXTERNAL SwapBuffers:WINAPI*>
PROCEDURE SwapBuffers (a1: HDC): BOOL;

END WinGDI.
