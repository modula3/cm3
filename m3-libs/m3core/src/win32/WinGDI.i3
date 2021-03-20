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

FROM Ctypes IMPORT char, int;
FROM Word IMPORT Or, Shift;
FROM WinDef IMPORT INT16, BOOL, UINT16, UINT32, PUINT32, PVOID, HDC,
                   LPARAM, POINT, HBRUSH, RECT, UINT8, PUINT8, HBITMAP, PRECT,
                   PINT32, HRGN, PPOINT, INT32, COLORREF, WFLOAT,
                   HGDIOBJ, HMETAFILE, HMODULE, HFONT, HPEN, HPALETTE, HGLOBAL,
                   RECTL, SIZEL, PFLOAT, PSIZE, HENHMETAFILE, HGLRC, WCHAR,
                   POINTS, PSTR, PWSTR, PCSTR, PCWSTR, SIZE_T;

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
  SRCCOPY    : UINT32 = 16_00CC0020; (* dest = source *)
  SRCPAINT   : UINT32 = 16_00EE0086; (* dest = source OR dest *)
  SRCAND     : UINT32 = 16_008800C6; (* dest = source AND dest *)
  SRCINVERT  : UINT32 = 16_00660046; (* dest = source XOR dest *)
  SRCERASE   : UINT32 = 16_00440328; (* dest = source AND (NOT dest ) *)
  NOTSRCCOPY : UINT32 = 16_00330008; (* dest = (NOT source) *)
  NOTSRCERASE: UINT32 = 16_001100A6; (* dest = (NOT src) AND (NOT dest) *)
  MERGECOPY  : UINT32 = 16_00C000CA; (* dest = (source AND pattern) *)
  MERGEPAINT : UINT32 = 16_00BB0226; (* dest = (NOT source) OR dest *)
  PATCOPY    : UINT32 = 16_00F00021; (* dest = pattern *)
  PATPAINT   : UINT32 = 16_00FB0A09; (* dest = DPSnoo *)
  PATINVERT  : UINT32 = 16_005A0049; (* dest = pattern XOR dest *)
  DSTINVERT  : UINT32 = 16_00550009; (* dest = (NOT dest) *)
  BLACKNESS  : UINT32 = 16_00000042; (* dest = BLACK *)
  WHITENESS  : UINT32 = 16_00FF0062; (* dest = WHITE *)
  NOMIRRORBITMAP = 16_80000000; (* Do not Mirror the bitmap in this call *)
  CAPTUREBLT = 16_40000000; (* Include layered windows *)

  GDI_ERROR: INT32 = -1;

(* Region Flags *)
  ERROR         = 0;
  NULLREGION    = 1;
  SIMPLEREGION  = 2;
  COMPLEXREGION = 3;
  RGN_ERROR     = ERROR;

(* CombineRgn() Styles *)
  RGN_AND  = 1;
  RGN_OR   = 2;
  RGN_XOR  = 3;
  RGN_DIFF = 4;
  RGN_COPY = 5;
  RGN_MIN  = RGN_AND;
  RGN_MAX  = RGN_COPY;

(* StretchBlt() Modes *)
  BLACKONWHITE      = 1;
  WHITEONBLACK      = 2;
  COLORONCOLOR      = 3;
  HALFTONE          = 4;
  MAXSTRETCHBLTMODE = 4;
  STRETCH_ANDSCANS = BLACKONWHITE;
  STRETCH_ORSCANS = WHITEONBLACK;
  STRETCH_DELETESCANS = COLORONCOLOR;
  STRETCH_HALFTONE = HALFTONE;

(* Layout Orientation Options *)

  LAYOUT_RTL = 16_00000001; (* Right to left *)
  LAYOUT_BTT = 16_00000002; (* Bottom to top *)
  LAYOUT_VBH = 16_00000004; (* Vertical before horizontal *)
  LAYOUT_ORIENTATIONMASK = (LAYOUT_RTL + LAYOUT_BTT + LAYOUT_VBH);
  LAYOUT_BITMAPORIENTATIONPRESERVED = 16_00000008;

(* PolyFill() Modes *)
  ALTERNATE     = 1;
  WINDING       = 2;
  POLYFILL_LAST = 2;

(* Text Alignment Options *)
  TA_NOUPDATECP = 0;
  TA_UPDATECP   = 1;

  TA_LEFT   = 0;
  TA_RIGHT  = 2;
  TA_CENTER = 6;

  TA_TOP      = 0;
  TA_BOTTOM   = 8;
  TA_BASELINE = 24;
  TA_RTLREADING = 256;
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
  ETO_GLYPH_INDEX = 16_0010;
  ETO_RTLREADING = 16_0080;
  ETO_NUMERICSLOCAL = 16_0400;
  ETO_NUMERICSLATIN = 16_0800;
  ETO_IGNORELANGUAGE = 16_1000;
  ETO_PDY = 16_2000;

  ASPECT_FILTERING = 16_0001;

(* Bounds Accumulation APIs *)
  DCB_RESET      = 16_0001;
  DCB_ACCUMULATE = 16_0002;
  DCB_DIRTY      = DCB_ACCUMULATE;
  DCB_SET        = Or(DCB_RESET, DCB_ACCUMULATE);
  DCB_ENABLE     = 16_0004;
  DCB_DISABLE    = 16_0008;

(* Metafile Functions *)
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
  (* META_DRAWTEXT          = 16_062F; *) (* not in headers *)

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
  META_SETLAYOUT = 16_0149;

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
  OPENCHANNEL = 4110;
  DOWNLOADHEADER = 4111;
  CLOSECHANNEL = 4112;
  POSTSCRIPT_PASSTHROUGH = 4115;
  ENCAPSULATED_POSTSCRIPT = 4116;
  POSTSCRIPT_IDENTIFY = 4117; (* new escape for NT5 pscript driver *)
  POSTSCRIPT_INJECTION = 4118; (* new escape for NT5 pscript driver *)
  CHECKJPEGFORMAT = 4119;
  CHECKPNGFORMAT = 4120;
  GET_PS_FEATURESETTING = 4121; (* new escape for NT5 pscript driver *)
  SPCLPASSTHROUGH2 = 4568; (* new escape for NT5 pscript driver *)

(* Spooler Error Codes *)
  SP_NOTREPORTED = 16_4000;
  SP_ERROR       = (-1);
  SP_APPABORT    = (-2);
  SP_USERABORT   = (-3);
  SP_OUTOFDISK   = (-4);
  SP_OUTOFMEMORY = (-5);

  PR_JOBSTATUS = 16_0000;

(* Object Definitions for EnumObjects() *)
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
  OBJ_COLORSPACE = 14;

(* xform stuff *)
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
    bmType      : INT32;
    bmWidth     : INT32;
    bmHeight    : INT32;
    bmWidthBytes: INT32;
    bmPlanes    : UINT16;
    bmBitsPixel : UINT16;
    bmBits      : PVOID;
  END;

  RGBTRIPLE = RECORD
    rgbtBlue : UINT8;
    rgbtGreen: UINT8;
    rgbtRed  : UINT8;
  END;

  RGBQUAD = RECORD
    rgbBlue    : UINT8;
    rgbGreen   : UINT8;
    rgbRed     : UINT8;
    rgbReserved: UINT8;
  END;

  (* structures for defining DIBs *)
  PBITMAPCOREHEADER = UNTRACED REF BITMAPCOREHEADER;
  LPBITMAPCOREHEADER = UNTRACED REF BITMAPCOREHEADER;
  BITMAPCOREHEADER = RECORD
    bcSize    : UINT32;  (* used to get to color table *)
    bcWidth   : UINT16;
    bcHeight  : UINT16;
    bcPlanes  : UINT16;
    bcBitCount: UINT16;
  END;

  PBITMAPINFOHEADER = UNTRACED REF BITMAPINFOHEADER;
  LPBITMAPINFOHEADER = UNTRACED REF BITMAPINFOHEADER;
  BITMAPINFOHEADER = RECORD
    biSize         : UINT32;
    biWidth        : INT32;
    biHeight       : INT32;
    biPlanes       : UINT16;
    biBitCount     : UINT16;
    biCompression  : UINT32;
    biSizeImage    : UINT32;
    biXPelsPerMeter: INT32;
    biYPelsPerMeter: INT32;
    biClrUsed      : UINT32;
    biClrImportant : UINT32;
  END;

(* constants for the biCompression field *)
CONST
  BI_RGB      : INT32 = 0;
  BI_RLE8     : INT32 = 1;
  BI_RLE4     : INT32 = 2;
  BI_BITFIELDS: INT32 = 3;
  BI_TOPDOWN  : INT32 = 4;   (*!!!  This should be deleted, I only leave *)
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
  BITMAPFILEHEADER = BITS (16_E * 8) FOR RECORD
    bfType     : UINT16;
    (*bfSize     : UINT32;*)
    unaligned_bfSize: ARRAY [0..1] OF UINT16;
    bfReserved1: UINT16;
    bfReserved2: UINT16;
    (*bfOffBits  : UINT32;*)
    unaligned_bfOffBits: ARRAY [0..1] OF UINT16;
  END;
  (*???  #pragma pack() *)

(* Replace macros with functions. *)
(* s in points is for signed 16bit short, not plural *)
<*EXTERNAL "WinGDI__MAKEPOINTS"*>   PROCEDURE MAKEPOINTS(lParam: LPARAM; points: UNTRACED REF POINTS);
<*EXTERNAL "WinGDI__GET_X_LPARAM"*> PROCEDURE GET_X_LPARAM(lParam: LPARAM): int;
<*EXTERNAL "WinGDI__GET_Y_LPARAM"*> PROCEDURE GET_Y_LPARAM(lParam: LPARAM): int;

(* Make up a new one. *)
<*EXTERNAL "WinGDI__PointsToLParam"*> PROCEDURE PointsToLParam(points: UNTRACED REF POINTS): LPARAM;

(* Clipboard Metafile Picture Structure *)
TYPE
  HANDLETABLE = RECORD objectHandle: ARRAY [0 .. 1 - 1] OF HGDIOBJ;  END;
  PHANDLETABLE = UNTRACED REF HANDLETABLE;
  LPHANDLETABLE = UNTRACED REF HANDLETABLE;

  PMETARECORD = UNTRACED REF METARECORD;
  LPMETARECORD = UNTRACED REF METARECORD;
  METARECORD = RECORD
    rdSize    : UINT32;
    rdFunction: UINT16;
    rdParm    : ARRAY [0 .. 1 - 1] OF UINT16;
  END;

  LPMETAFILEPICT = UNTRACED REF METAFILEPICT;
  METAFILEPICT = RECORD
    mm  : INT32;
    xExt: INT32;
    yExt: INT32;
    hMF : HMETAFILE;
  END;

  (*???  #pragma pack(2) *)
  PMETAHEADER = UNTRACED REF METAHEADER;
  LPMETAHEADER = UNTRACED REF METAHEADER;
  METAHEADER = BITS (16_12 * 8) FOR RECORD
    mtType        : UINT16;
    mtHeaderSize  : UINT16;
    mtVersion     : UINT16;
    (*mtSize        : UINT32;*)
    unaligned_mtSize  : ARRAY [0..1] OF UINT16;
    mtNoObjects   : UINT16;
    (*mtMaxRecord   : UINT32;*)
    (* This is aligned ok, but it blows up the alignment and size of the containing record. *)
    unaligned_mtMaxRecord  : ARRAY [0..1] OF UINT16;
    mtNoParameters: UINT16;
  END;

  (*???  #pragma pack() *)

  (* Enhanced Metafile structures *)
  PENHMETARECORD = UNTRACED REF ENHMETARECORD;
  LPENHMETARECORD = UNTRACED REF ENHMETARECORD;
  ENHMETARECORD = RECORD
    iType: UINT32;  (* Record type MR_XXX *)
    nSize: UINT32;  (* Record size in bytes *)
    dParm: ARRAY [0 .. 1 - 1] OF UINT32;  (* Parameters *)
  END;

  PENHMETAHEADER = UNTRACED REF ENHMETAHEADER;
  LPENHMETAHEADER = UNTRACED REF ENHMETAHEADER;
  ENHMETAHEADER =
    RECORD
    iType: UINT32;             (* Record type MR_METAFILE *)
    nSize: UINT32;             (* Record size in bytes.  This may be
                                   greater *)
      (* than the sizeof(ENHMETAHEADER). *)
    rclBounds: RECTL;         (* Inclusive-inclusive bounds in device
                                   units *)
    rclFrame: RECTL;          (* Inclusive-inclusive Picture Frame of
                                   metafile in .01 mm units *)
    dSignature: UINT32;        (* Signature.  Must be
                                   ENHMETA_SIGNATURE. *)
    nVersion: UINT32;          (* Version number *)
    nBytes  : UINT32;          (* Size of the metafile in bytes *)
    nRecords: UINT32;          (* Number of records in the metafile *)
    nHandles: UINT16;           (* Number of handles in the handle table *)
      (* Handle index zero is reserved. *)
    sReserved: UINT16;          (* Reserved.  Must be zero. *)
    nDescription: UINT32;      (* Number of chars in the unicode
                                   description string *)
      (* This is 0 if there is no description string *)
    offDescription: UINT32;    (* Offset to the metafile description
                                   record. *)
      (* This is 0 if there is no description string *)
    nPalEntries: UINT32;       (* Number of entries in the metafile
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
    tmHeight          : INT32;
    tmAscent          : INT32;
    tmDescent         : INT32;
    tmInternalLeading : INT32;
    tmExternalLeading : INT32;
    tmAveCharWidth    : INT32;
    tmMaxCharWidth    : INT32;
    tmWeight          : INT32;
    tmOverhang        : INT32;
    tmDigitizedAspectX: INT32;
    tmDigitizedAspectY: INT32;
    tmFirstChar       : UINT8;
    tmLastChar        : UINT8;
    tmDefaultChar     : UINT8;
    tmBreakChar       : UINT8;
    tmItalic          : UINT8;
    tmUnderlined      : UINT8;
    tmStruckOut       : UINT8;
    tmPitchAndFamily  : UINT8;
    tmCharSet         : UINT8;
  END;

  PTEXTMETRICW = UNTRACED REF TEXTMETRICW;
  NPTEXTMETRICW = UNTRACED REF TEXTMETRICW;
  LPTEXTMETRICW = UNTRACED REF TEXTMETRICW;
  TEXTMETRICW = RECORD
    tmHeight          : INT32;
    tmAscent          : INT32;
    tmDescent         : INT32;
    tmInternalLeading : INT32;
    tmExternalLeading : INT32;
    tmAveCharWidth    : INT32;
    tmMaxCharWidth    : INT32;
    tmWeight          : INT32;
    tmOverhang        : INT32;
    tmDigitizedAspectX: INT32;
    tmDigitizedAspectY: INT32;
    tmFirstChar       : WCHAR;
    tmLastChar        : WCHAR;
    tmDefaultChar     : WCHAR;
    tmBreakChar       : WCHAR;
    tmItalic          : UINT8;
    tmUnderlined      : UINT8;
    tmStruckOut       : UINT8;
    tmPitchAndFamily  : UINT8;
    tmCharSet         : UINT8;
  END;

  TEXTMETRIC = TEXTMETRICA;
  PTEXTMETRIC = PTEXTMETRICA;
  NPTEXTMETRIC = NPTEXTMETRICA;
  LPTEXTMETRIC = LPTEXTMETRICA;

(* ntmFlags field flags *)
CONST
  NTM_REGULAR: INT32 = 16_00000040;
  NTM_BOLD   : INT32 = 16_00000020;
  NTM_ITALIC : INT32 = 16_00000001;

TYPE
  PNEWTEXTMETRICA = UNTRACED REF NEWTEXTMETRICA;
  NPNEWTEXTMETRICA = UNTRACED REF NEWTEXTMETRICA;
  LPNEWTEXTMETRICA = UNTRACED REF NEWTEXTMETRICA;
  NEWTEXTMETRICA = RECORD
    tmHeight          : INT32;
    tmAscent          : INT32;
    tmDescent         : INT32;
    tmInternalLeading : INT32;
    tmExternalLeading : INT32;
    tmAveCharWidth    : INT32;
    tmMaxCharWidth    : INT32;
    tmWeight          : INT32;
    tmOverhang        : INT32;
    tmDigitizedAspectX: INT32;
    tmDigitizedAspectY: INT32;
    tmFirstChar       : UINT8;
    tmLastChar        : UINT8;
    tmDefaultChar     : UINT8;
    tmBreakChar       : UINT8;
    tmItalic          : UINT8;
    tmUnderlined      : UINT8;
    tmStruckOut       : UINT8;
    tmPitchAndFamily  : UINT8;
    tmCharSet         : UINT8;
    padding           : ARRAY [0..2] OF UINT8;
    ntmFlags          : UINT32;
    ntmSizeEM         : UINT32;
    ntmCellHeight     : UINT32;
    ntmAvgWidth       : UINT32;
  END;

  PNEWTEXTMETRICW = UNTRACED REF NEWTEXTMETRICW;
  NPNEWTEXTMETRICW = UNTRACED REF NEWTEXTMETRICW;
  LPNEWTEXTMETRICW = UNTRACED REF NEWTEXTMETRICW;
  NEWTEXTMETRICW = RECORD
    tmHeight          : INT32;
    tmAscent          : INT32;
    tmDescent         : INT32;
    tmInternalLeading : INT32;
    tmExternalLeading : INT32;
    tmAveCharWidth    : INT32;
    tmMaxCharWidth    : INT32;
    tmWeight          : INT32;
    tmOverhang        : INT32;
    tmDigitizedAspectX: INT32;
    tmDigitizedAspectY: INT32;
    tmFirstChar       : WCHAR;
    tmLastChar        : WCHAR;
    tmDefaultChar     : WCHAR;
    tmBreakChar       : WCHAR;
    tmItalic          : UINT8;
    tmUnderlined      : UINT8;
    tmStruckOut       : UINT8;
    tmPitchAndFamily  : UINT8;
    tmCharSet         : UINT8;
    padding           : ARRAY [0..2] OF UINT8;
    ntmFlags          : UINT32;
    ntmSizeEM         : UINT32;
    ntmCellHeight     : UINT32;
    ntmAvgWidth       : UINT32;
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
    paXCount: INT32;
    paYCount: INT32;
    paXExt  : INT32;
    paYExt  : INT32;
    paRGBs  : UINT8;
  END;

  (* Logical Brush (or Pattern) *)
  PLOGBRUSH = UNTRACED REF LOGBRUSH;
  NPLOGBRUSH = UNTRACED REF LOGBRUSH; (* remove this? *)
  LPLOGBRUSH = UNTRACED REF LOGBRUSH; (* remove this? *)
  LOGBRUSH = RECORD
    lbStyle: UINT32;
    lbColor: COLORREF;
    lbHatch: SIZE_T;
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
    lopnStyle: UINT32;
    lopnWidth: POINT;
    lopnColor: COLORREF;
  END;

  PEXTLOGPEN = UNTRACED REF EXTLOGPEN;
  NPEXTLOGPEN = UNTRACED REF EXTLOGPEN;
  LPEXTLOGPEN = UNTRACED REF EXTLOGPEN;
  EXTLOGPEN = RECORD
    elpPenStyle  : UINT32;
    elpWidth     : UINT32;
    elpBrushStyle: UINT32;
    elpColor     : COLORREF;
    elpHatch     : SIZE_T;
    elpNumEntries: UINT32;
    elpStyleEntry: ARRAY [0 .. 1 - 1] OF UINT32;
  END;

  PPALETTEENTRY = UNTRACED REF PALETTEENTRY;
  LPPALETTEENTRY = UNTRACED REF PALETTEENTRY;
  PALETTEENTRY = RECORD
    peRed  : UINT8;
    peGreen: UINT8;
    peBlue : UINT8;
    peFlags: UINT8;
  END;

  (* Logical Palette *)
  PLOGPALETTE = UNTRACED REF LOGPALETTE;
  NPLOGPALETTE = UNTRACED REF LOGPALETTE;
  LPLOGPALETTE = UNTRACED REF LOGPALETTE;
  LOGPALETTE = RECORD
    palVersion   : UINT16;
    palNumEntries: UINT16;
    palPalEntry  : ARRAY [0..100000] OF PALETTEENTRY;
  END;

  LOGPALETTEBASE= RECORD
    palVersion   : UINT16;
    palNumEntries: UINT16;
  END;

(* Logical Font *)
CONST LF_FACESIZE = 32;

TYPE
  PLOGFONTA = UNTRACED REF LOGFONTA;
  NPLOGFONTA = UNTRACED REF LOGFONTA;
  LPLOGFONTA = UNTRACED REF LOGFONTA;
  LOGFONTA = RECORD
    lfHeight        : INT32;
    lfWidth         : INT32;
    lfEscapement    : INT32;
    lfOrientation   : INT32;
    lfWeight        : INT32;
    lfItalic        : UINT8;
    lfUnderline     : UINT8;
    lfStrikeOut     : UINT8;
    lfCharSet       : UINT8;
    lfOutPrecision  : UINT8;
    lfClipPrecision : UINT8;
    lfQuality       : UINT8;
    lfPitchAndFamily: UINT8;
    lfFaceName      : ARRAY [0 .. LF_FACESIZE - 1] OF char;
  END;

  PLOGFONTW = UNTRACED REF LOGFONTW;
  NPLOGFONTW = UNTRACED REF LOGFONTW;
  LPLOGFONTW = UNTRACED REF LOGFONTW;
  LOGFONTW = RECORD
    lfHeight        : INT32;
    lfWidth         : INT32;
    lfEscapement    : INT32;
    lfOrientation   : INT32;
    lfWeight        : INT32;
    lfItalic        : UINT8;
    lfUnderline     : UINT8;
    lfStrikeOut     : UINT8;
    lfCharSet       : UINT8;
    lfOutPrecision  : UINT8;
    lfClipPrecision : UINT8;
    lfQuality       : UINT8;
    lfPitchAndFamily: UINT8;
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
    elfFullName: ARRAY [0 .. LF_FULLFACESIZE - 1] OF UINT8;
    elfStyle   : ARRAY [0 .. LF_FACESIZE - 1] OF UINT8;
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
    bFamilyType     : UINT8;
    bSerifStyle     : UINT8;
    bWeight         : UINT8;
    bProportion     : UINT8;
    bContrast       : UINT8;
    bStrokeVariation: UINT8;
    bArmStyle       : UINT8;
    bLetterform     : UINT8;
    bMidline        : UINT8;
    bXHeight        : UINT8;
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
    elfFullName: ARRAY [0 .. LF_FULLFACESIZE - 1] OF UINT8;
    elfStyle   : ARRAY [0 .. LF_FACESIZE - 1] OF UINT8;
    elfVersion  : UINT32;  (* 0 for the first release of NT *)
    elfStyleSize: UINT32;
    elfMatch    : UINT32;
    elfReserved : UINT32;
    elfVendorId: ARRAY [0 .. ELF_VENDOR_SIZE - 1] OF UINT8;
    elfCulture: UINT32;   (* 0 for Latin *)
    elfPanose : PANOSE;
  END;

  PEXTLOGFONTW = UNTRACED REF EXTLOGFONTW;
  NPEXTLOGFONTW = UNTRACED REF EXTLOGFONTW;
  LPEXTLOGFONTW = UNTRACED REF EXTLOGFONTW;
  EXTLOGFONTW = RECORD
    elfLogFont : LOGFONTW;
    elfFullName: ARRAY [0 .. LF_FULLFACESIZE - 1] OF WCHAR;
    elfStyle   : ARRAY [0 .. LF_FACESIZE - 1] OF WCHAR;
    elfVersion  : UINT32;  (* 0 for the first release of NT *)
    elfStyleSize: UINT32;
    elfMatch    : UINT32;
    elfReserved : UINT32;
    elfVendorId: ARRAY [0 .. ELF_VENDOR_SIZE - 1] OF UINT8;
    elfCulture: UINT32;   (* 0 for Latin *)
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
  FMATCH_EXACT = 0;
  FMATCH_NEAR  = 1;
  FMATCH_FAR   = 2;
  FMATCH_ERROR = 3;

  PANOSE_RANGE = 16;

  FM_LOCATION_GDI = 1 (* location of font is in GDI *);

TYPE
  LPFMPENALTYSET = UNTRACED REF FMPENALTYSET;
  FMPENALTYSET = RECORD
    psSize  : UINT32;  (* size of this structure in UINT8's *)
    psHeight: UINT32;
    psWidth : UINT32;
    psEscapement    : UINT32;
    psOrientation   : UINT32;
    psWeight        : UINT32;
    psItalic        : UINT32;
    psUnderline     : UINT32;
    psStrikeOut     : UINT32;
    psOutPrecsion   : UINT32;
    psClipPrecision : UINT32;
    psQuality       : UINT32;
    psPitchAndFamily: UINT32;
    psFaceName      : UINT32;
    psFullName      : UINT32;
    psStyle         : UINT32;
    psPanose  : ARRAY [0 .. PANOSE_COUNT - 1] OF UINT32;
    psVendorId: UINT32;
    psLocation: UINT32;
  END;

  LPFMWEIGHTSET = UNTRACED REF FMWEIGHTSET;
  FMWEIGHTSET = RECORD
    wsSize  : UINT32;  (* size of this structure in UINT8's *)
    wsHeight: UINT32;
    wsWidth : UINT32;
    wsEscapement    : UINT32;
    wsOrientation   : UINT32;
    wsWeight        : UINT32;
    wsItalic        : UINT32;
    wsUnderline     : UINT32;
    wsStrikeOut     : UINT32;
    wsOutPrecsion   : UINT32;
    wsClipPrecision : UINT32;
    wsQuality       : UINT32;
    wsPitchAndFamily: UINT32;
    wsFaceName      : UINT32;
    wsFullName      : UINT32;
    wsStyle         : UINT32;
    wsPanose  : ARRAY [0 .. PANOSE_COUNT - 1] OF UINT32;
    wsVendorId: UINT32;
    wsLocation: UINT32;
  END;

  LPFMATCHA = UNTRACED REF FMATCHA;
  FMATCHA = RECORD
    fmSize: UINT32;    (* size of this structure in bytes *)
    fmTotalPenalty: UINT32;  (* total penalty of physical font *)
    fmPenaltySet: FMPENALTYSET;  (* penalties of physical font *)
    fmExtLogFont: EXTLOGFONTA;   (* describes physical font *)
  END;

  LPFMATCHW = UNTRACED REF FMATCHW;
  FMATCHW = RECORD
    fmSize: UINT32;    (* size of this structure in bytes *)
    fmTotalPenalty: UINT32;  (* total penalty of physical font *)
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

  SIZEOFMAPORDER = BYTESIZE(UINT32) * (MAPPER_INDEX_LAST + 1);

TYPE
  FMORDER = ARRAY [0 .. MAPPER_INDEX_LAST + 1 - 1] OF UINT32;
  LPFMORDER = UNTRACED REF FMORDER;

  LPFMCONTROLS = UNTRACED REF FMCONTROLS;
  FMCONTROLS = RECORD
    size         : UINT32;
    penaltySumMax: UINT32;
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

PROCEDURE RGB (r, g, b: UINT8): COLORREF;
PROCEDURE PALETTERGB (r, g, b: UINT8): COLORREF;
PROCEDURE PALETTEINDEX (i: UINT16): COLORREF;

(* palette entry flags *)
CONST
  PC_RESERVED   = 16_01 (* palette index used for animation *);
  PC_EXPLICIT   = 16_02 (* palette index is explicit to device *);
  PC_NOCOLLAPSE = 16_04 (* do not match color to system palette *);

PROCEDURE GetRValue (rgb: COLORREF): UINT8;
PROCEDURE GetGValue (rgb: COLORREF): UINT8;
PROCEDURE GetBValue (rgb: COLORREF): UINT8;

(* Background Modes *)
CONST
  TRANSPARENT = 1;
  OPAQUE      = 2;
  BKMODE_LAST = 2;

(* Graphics Modes *)
  GM_COMPATIBLE = 1;
  GM_ADVANCED = 2;
  GM_LAST = 2;

(* PolyDraw and GetPath point types *)
  PT_CLOSEFIGURE = 16_01;
  PT_LINETO      = 16_02;
  PT_BEZIERTO    = 16_04;
  PT_MOVETO      = 16_06;

(* Mapping Modes *)
  MM_TEXT        = 1;
  MM_LOMETRIC    = 2;
  MM_HIMETRIC    = 3;
  MM_LOENGLISH   = 4;
  MM_HIENGLISH   = 5;
  MM_TWIPS       = 6;
  MM_ISOTROPIC   = 7;
  MM_ANISOTROPIC = 8;

(* Min and Max Mapping Mode values *)
  MM_MIN            = MM_TEXT;
  MM_MAX            = MM_ANISOTROPIC;
  MM_MAX_FIXEDSCALE = MM_TWIPS;

(* Coordinate Modes *)
  ABSOLUTE = 1;
  RELATIVE = 2;

(* Stock Logical Objects *)
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
  DEFAULT_GUI_FONT = 17;
  (* STOCK_LAST = 16; *) (* this varies *)

  CLR_INVALID = 16_FFFFFFFF;

(* Brush Styles *)
  BS_SOLID        = 0;
  BS_NULL         = 1;
  BS_HOLLOW       = BS_NULL;
  BS_HATCHED      = 2;
  BS_PATTERN      = 3;
  BS_INDEXED      = 4;
  BS_DIBPATTERN   = 5;
  BS_DIBPATTERNPT = 6;
  BS_PATTERN8X8 = 7;
  BS_DIBPATTERN8X8 = 8;
  BS_MONOPATTERN = 9;

(* Hatch Styles *)
  HS_HORIZONTAL = 0 (* ----- *);
  HS_VERTICAL   = 1 (* ||||| *);
  HS_FDIAGONAL  = 2 (* \\\\\ *);
  HS_BDIAGONAL  = 3 (* ///// *);
  HS_CROSS      = 4 (* +++++ *);
  HS_DIAGCROSS  = 5 (* xxxxx *);
  (* None of these are in current headers.
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
  *)

(* Pen Styles *)
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
  PHYSICALWIDTH   = 110 (* Physical Width in device units *);
  PHYSICALHEIGHT  = 111 (* Physical Height in device units *);
  PHYSICALOFFSETX = 112 (* Physical Printable Area x margin *);
  PHYSICALOFFSETY = 113 (* Physical Printable Area y margin *);
  SCALINGFACTORX  = 114 (* Scaling factor x *);
  SCALINGFACTORY  = 115 (* Scaling factor y *);

(* Device Capability Masks: *)

(* Device Technologies *)
  DT_PLOTTER    = 0 (* Vector plotter *);
  DT_RASDISPLAY = 1 (* Raster display *);
  DT_RASPRINTER = 2 (* Raster printer *);
  DT_RASCAMERA  = 3 (* Raster camera *);
  DT_CHARSTREAM = 4 (* Character-stream, PLP *);
  DT_METAFILE   = 5 (* Metafile, VDM *);
  DT_DISPFILE   = 6 (* Display-file *);

(* Curve Capabilities *)
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
  LC_NONE       = 0 (* Lines not supported *);
  LC_POLYLINE   = 2 (* Can do polylines *);
  LC_MARKER     = 4 (* Can do markers *);
  LC_POLYMARKER = 8 (* Can do polymarkers *);
  LC_WIDE       = 16 (* Can do wide lines *);
  LC_STYLED     = 32 (* Can do styled lines *);
  LC_WIDESTYLED = 64 (* Can do wide styled lines *);
  LC_INTERIORS  = 128 (* Can do interiors *);

(* Polygonal Capabilities *)
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
  CP_NONE      = 0 (* No clipping of output *);
  CP_RECTANGLE = 1 (* Output clipped to rects *);
  CP_REGION    = 2 (* *);

(* Text Capabilities *)
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
  DIB_RGB_COLORS      = 0 (* color table in RGBs *);
  DIB_PAL_COLORS      = 1 (* color table in palette indices *);
  DIB_PAL_INDICES     = 2 (* No color table indices into surf palette *);
  DIB_PAL_PHYSINDICES = 2 (* No color table indices into surf palette *);
  DIB_PAL_LOGINDICES  = 4 (* No color table indices into DC palette *);

(* constants for Get/SetSystemPaletteUse() *)
  SYSPAL_ERROR    = 0;
  SYSPAL_STATIC   = 1;
  SYSPAL_NOSTATIC = 2;

(* constants for CreateDIBitmap *)
  CBM_CREATEDIB: INT32 = 16_02 (* create DIB bitmap *);
  CBM_INIT     : INT32 = 16_04 (* initialize bitmap *);

(* ExtFloodFill style flags *)
  FLOODFILLBORDER  = 0;
  FLOODFILLSURFACE = 1;

(* DEVMODE dmDisplayMode flags *)
  DM_GRAYSCALE = 1;

TYPE
  PDEVMODEA = UNTRACED REF DEVMODEA;
  NPDEVMODEA = UNTRACED REF DEVMODEA;
  LPDEVMODEA = UNTRACED REF DEVMODEA;
  DEVMODEA = RECORD
    dmDeviceName   : ARRAY [0 .. 32 - 1] OF UINT8;
    dmSpecVersion  : UINT16;
    dmDriverVersion: UINT16;
    dmSize         : UINT16;
    dmDriverExtra  : UINT16;
    dmFields       : UINT32;
    dmOrientation  : INT16;
    dmPaperSize    : INT16;
    dmPaperLength  : INT16;
    dmPaperWidth   : INT16;
    dmScale        : INT16;
    dmCopies       : INT16;
    dmDefaultSource: INT16;
    dmPrintQuality : INT16;
    dmColor        : INT16;
    dmDuplex       : INT16;
    dmYResolution  : INT16;
    dmTTOption     : INT16;
    dmCollate      : INT16;
    dmFormName     : ARRAY [0 .. 32 - 1] OF UINT8;
    dmBitsPerPel   : UINT16;
    dmPelsWidth    : UINT32;
    dmPelsHeight   : UINT32;
    dmDisplayMode  : UINT32;
  END;

  PDEVMODEW = UNTRACED REF DEVMODEW;
  LPDEVMODEW = UNTRACED REF DEVMODEW;
  NPDEVMODEW = UNTRACED REF DEVMODEW;
  DEVMODEW = RECORD
    dmDeviceName   : ARRAY [0 .. 32 - 1] OF WCHAR;
    dmSpecVersion  : UINT16;
    dmDriverVersion: UINT16;
    dmSize         : UINT16;
    dmDriverExtra  : UINT16;
    dmFields       : UINT32;
    dmOrientation  : INT16;
    dmPaperSize    : INT16;
    dmPaperLength  : INT16;
    dmPaperWidth   : INT16;
    dmScale        : INT16;
    dmCopies       : INT16;
    dmDefaultSource: INT16;
    dmPrintQuality : INT16;
    dmColor        : INT16;
    dmDuplex       : INT16;
    dmYResolution  : INT16;
    dmTTOption     : INT16;
    dmCollate      : INT16;
    dmFormName     : ARRAY [0 .. 32 - 1] OF WCHAR;
    dmBitsPerPel   : UINT16;
    dmPelsWidth    : UINT32;
    dmPelsHeight   : UINT32;
    dmDisplayMode  : UINT32;
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
    dwSize  : UINT32;
    iType   : UINT32;
    nCount  : UINT32;
    nRgnSize: UINT32;
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
    abcA: INT32;
    abcB: UINT32;
    abcC: INT32;
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
    otmSize               : UINT32;
    otmTextMetrics        : TEXTMETRICA;
    otmFiller             : UINT8;
    otmPanoseNumber       : PANOSE;
    otmfsSelection        : UINT32;
    otmfsType             : UINT32;
    otmsCharSlopeRise     : UINT32;
    otmsCharSlopeRun      : UINT32;
    otmItalicAngle        : UINT32;
    otmEMSquare           : UINT32;
    otmAscent             : UINT32;
    otmDescent            : INT32;
    otmLineGap            : INT32;
    otmsCapEmHeight       : UINT32;
    otmsXHeight           : UINT32;
    otmrcFontBox          : RECT;
    otmMacAscent          : INT32;
    otmMacDescent         : INT32;
    otmMacLineGap         : UINT32;
    otmusMinimumPPEM      : UINT32;
    otmptSubscriptSize    : POINT;
    otmptSubscriptOffset  : POINT;
    otmptSuperscriptSize  : POINT;
    otmptSuperscriptOffset: POINT;
    otmsStrikeoutSize     : UINT32;
    otmsStrikeoutPosition : INT32;
    otmsUnderscoreSize    : INT32;
    otmsUnderscorePosition: UINT32;
    otmpFamilyName        : PSTR;
    otmpFaceName          : PSTR;
    otmpStyleName         : PSTR;
    otmpFullName          : PSTR;
  END;

  POUTLINETEXTMETRICW = UNTRACED REF OUTLINETEXTMETRICW;
  NPOUTLINETEXTMETRICW = UNTRACED REF OUTLINETEXTMETRICW;
  LPOUTLINETEXTMETRICW = UNTRACED REF OUTLINETEXTMETRICW;
  OUTLINETEXTMETRICW = RECORD
    otmSize               : UINT32;
    otmTextMetrics        : TEXTMETRICW;
    otmFiller             : UINT8;
    otmPanoseNumber       : PANOSE;
    otmfsSelection        : UINT32;
    otmfsType             : UINT32;
    otmsCharSlopeRise     : UINT32;
    otmsCharSlopeRun      : UINT32;
    otmItalicAngle        : UINT32;
    otmEMSquare           : UINT32;
    otmAscent             : UINT32;
    otmDescent            : INT32;
    otmLineGap            : INT32;
    otmsCapEmHeight       : UINT32;
    otmsXHeight           : UINT32;
    otmrcFontBox          : RECT;
    otmMacAscent          : INT32;
    otmMacDescent         : INT32;
    otmMacLineGap         : UINT32;
    otmusMinimumPPEM      : UINT32;
    otmptSubscriptSize    : POINT;
    otmptSubscriptOffset  : POINT;
    otmptSuperscriptSize  : POINT;
    otmptSuperscriptOffset: POINT;
    otmsStrikeoutSize     : UINT32;
    otmsStrikeoutPosition : INT32;
    otmsUnderscoreSize    : INT32;
    otmsUnderscorePosition: UINT32;
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
    x      : INT32;
    y      : INT32;
    n      : UINT32;
    lpwstr : UNTRACED REF UINT8;
    uiFlags: UINT32;
    rcl    : RECT;
    pdx    : UNTRACED REF INT32;
  END;

  POLYTEXTW = RECORD
    x      : INT32;
    y      : INT32;
    n      : UINT32;
    lpwstr : UNTRACED REF WCHAR;
    uiFlags: UINT32;
    rcl    : RECT;
    pdx    : UNTRACED REF INT32;
  END;

  POLYTEXT = POLYTEXTA;

  FIXED = RECORD
    fract: UINT16;
    value: INT16;
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
    gmBlackBoxX    : UINT32;
    gmBlackBoxY    : UINT32;
    gmptGlyphOrigin: POINT;
    gmCellIncX     : INT16;
    gmCellIncY     : INT16;
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
    wType: UINT16;
    cpfx : UINT16;
    apfx : ARRAY [0 .. 1 - 1] OF POINTFX;
  END;

  LPTTPOLYGONHEADER = UNTRACED REF TTPOLYGONHEADER;
  TTPOLYGONHEADER = RECORD
    cb      : UINT32;
    dwType  : UINT32;
    pfxStart: POINTFX;
  END;

  LPRASTERIZER_STATUS = UNTRACED REF RASTERIZER_STATUS;
  RASTERIZER_STATUS = RECORD
    nSize      : INT16;
    wFlags     : INT16;
    nLanguageID: INT16;
  END;

(* bits defined in wFlags of RASTERIZER_STATUS *)
CONST
  TT_AVAILABLE = 16_0001;
  TT_ENABLED   = 16_0002;

(* Pixel format descriptor *)
TYPE
  PIXELFORMATDESCRIPTOR = RECORD
    nSize          : UINT16;
    nVersion       : UINT16;
    dwFlags        : UINT32;
    iPixelType     : UINT8;
    cColorBits     : UINT8;
    cRedBits       : UINT8;
    cRedShift      : UINT8;
    cGreenBits     : UINT8;
    cGreenShift    : UINT8;
    cBlueBits      : UINT8;
    cBlueShift     : UINT8;
    cAlphaBits     : UINT8;
    cAlphaShift    : UINT8;
    cAccumBits     : UINT8;
    cAccumRedBits  : UINT8;
    cAccumGreenBits: UINT8;
    cAccumBlueBits : UINT8;
    cAccumAlphaBits: UINT8;
    cDepthBits     : UINT8;
    cStencilBits   : UINT8;
    cAuxBuffers    : UINT8;
    iLayerType     : UINT8;
    bReserved      : UINT8;
    dwLayerMask    : UINT32;
    dwVisibleMask  : UINT32;
    dwDamageMask   : UINT32;
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
                                         a3: UINT32;
                                         a4: LPARAM                   ): INT32;

  GOBJENUMPROC = <*CALLBACK*> PROCEDURE (a1: PVOID; a2: LPARAM): INT32;
  LINEDDAPROC = <*CALLBACK*> PROCEDURE (a1: INT32; a2: INT32; a3: LPARAM);

<*EXTERNAL AddFontResourceA:WINAPI*>
PROCEDURE AddFontResourceA (a1: PCSTR): INT32;

<*EXTERNAL AddFontResourceW:WINAPI*>
PROCEDURE AddFontResourceW (a1: PCWSTR): INT32;
CONST AddFontResource = AddFontResourceA;

<*EXTERNAL AddFontModule:WINAPI*>
PROCEDURE AddFontModule (a1: HMODULE): INT32;

<*EXTERNAL AnimatePalette:WINAPI*>
PROCEDURE AnimatePalette (a1: HPALETTE;
                            a2: UINT32;
                            a3: UINT32;
                            a4: UNTRACED REF PALETTEENTRY): BOOL;

<*EXTERNAL Arc:WINAPI*>
PROCEDURE Arc (a1: HDC; a2, a3, a4, a5, a6, a7, a8, a9: INT32): BOOL;

<*EXTERNAL BitBlt:WINAPI*>
PROCEDURE BitBlt (a1: HDC;  a2, a3, a4, a5: INT32;
                  a6: HDC;  a7, a8: INT32;  a9: UINT32): BOOL;

<*EXTERNAL CancelDC:WINAPI*>
PROCEDURE CancelDC (a1: HDC): BOOL;

<*EXTERNAL Chord:WINAPI*>
PROCEDURE Chord (a1: HDC; a2, a3, a4, a5, a6, a7, a8, a9: INT32): BOOL;

<*EXTERNAL ChoosePixelFormat:WINAPI*>
PROCEDURE ChoosePixelFormat(a1: HDC; a2: LPPIXELFORMATDESCRIPTOR): INT32;

<*EXTERNAL CloseMetaFile:WINAPI*>
PROCEDURE CloseMetaFile (a1: HDC): HMETAFILE;

<*EXTERNAL CombineRgn:WINAPI*>
PROCEDURE CombineRgn (a1: HRGN; a2: HRGN; a3: HRGN; a4: INT32): INT32;

<*EXTERNAL CopyMetaFileA:WINAPI*>
PROCEDURE CopyMetaFileA (a1: HMETAFILE; a2: PSTR): HMETAFILE;

<*EXTERNAL CopyMetaFileW:WINAPI*>
PROCEDURE CopyMetaFileW (a1: HMETAFILE; a2: PWSTR): HMETAFILE;
CONST CopyMetaFile = CopyMetaFileA;

<*EXTERNAL CreateBitmap:WINAPI*>
PROCEDURE CreateBitmap (a1, a2: INT32;  a3, a4: UINT32;  a5: PVOID): HBITMAP;

<*EXTERNAL CreateBitmapIndirect:WINAPI*>
PROCEDURE CreateBitmapIndirect (a1: LPBITMAP): HBITMAP;

<*EXTERNAL CreateBrushIndirect:WINAPI*>
PROCEDURE CreateBrushIndirect (a1: LPLOGBRUSH): HBRUSH;

<*EXTERNAL CreateCompatibleBitmap:WINAPI*>
PROCEDURE CreateCompatibleBitmap (a1: HDC; a2: INT32; a3: INT32): HBITMAP;

<*EXTERNAL CreateDiscardableBitmap:WINAPI*>
PROCEDURE CreateDiscardableBitmap (a1: HDC; a2: INT32; a3: INT32): HBITMAP;

<*EXTERNAL CreateCompatibleDC:WINAPI*>
PROCEDURE CreateCompatibleDC (a1: HDC): HDC;

<*EXTERNAL CreateDCA:WINAPI*>
PROCEDURE CreateDCA (a1, a2, a3: PCSTR;  a4: UNTRACED REF DEVMODEA): HDC;

<*EXTERNAL CreateDCW:WINAPI*>
PROCEDURE CreateDCW (a1, a2, a3: PCWSTR; a4: UNTRACED REF DEVMODEW): HDC;
CONST CreateDC = CreateDCA;

<*EXTERNAL CreateDIBitmap:WINAPI*>
PROCEDURE CreateDIBitmap (a1: HDC;
                            a2: LPBITMAPINFOHEADER;
                            a3: UINT32;
                            a4: UNTRACED REF UINT8;
                            a5: LPBITMAPINFO;
                            a6: UINT32                ): HBITMAP;

<*EXTERNAL CreateDIBSection:WINAPI*>
PROCEDURE CreateDIBSection (a1: HDC;
                              a2: LPBITMAPINFO;
                              a3: UINT32;
                              a4: UINT32;
                              a5: UNTRACED REF PUINT8): HBITMAP;

<*EXTERNAL CreateDIBPatternBrush:WINAPI*>
PROCEDURE CreateDIBPatternBrush (a1: HGLOBAL; a2: UINT32): HBRUSH;

<*EXTERNAL CreateDIBPatternBrushPt:WINAPI*>
PROCEDURE CreateDIBPatternBrushPt (a1: PVOID; a2: UINT32): HBRUSH;

<*EXTERNAL CreateEllipticRgn:WINAPI*>
PROCEDURE CreateEllipticRgn (a1: INT32; a2: INT32; a3: INT32; a4: INT32): HRGN;

<*EXTERNAL CreateEllipticRgnIndirect:WINAPI*>
PROCEDURE CreateEllipticRgnIndirect (a1: PRECT): HRGN;

<*EXTERNAL CreateFontIndirectA:WINAPI*>
PROCEDURE CreateFontIndirectA (a1: UNTRACED REF LOGFONTA): HFONT;

<*EXTERNAL CreateFontIndirectW:WINAPI*>
PROCEDURE CreateFontIndirectW (a1: UNTRACED REF LOGFONTW): HFONT;
CONST CreateFontIndirect = CreateFontIndirectA;

<*EXTERNAL CreateFontA:WINAPI*>
PROCEDURE CreateFontA (a1, a2, a3, a4, a5: INT32;
                       a6, a7, a8, a9, a10, a11, a12, a13: UINT32;
                       a14: PCSTR ): HFONT;

<*EXTERNAL CreateFontW:WINAPI*>
PROCEDURE CreateFontW (a1, a2, a3, a4, a5: INT32;
                       a6, a7, a8, a9, a10, a11, a12, a13: UINT32;
                       a14: PCWSTR ): HFONT;

CONST CreateFont = CreateFontA;

<*EXTERNAL CreateHatchBrush:WINAPI*>
PROCEDURE CreateHatchBrush (a1: INT32; a2: COLORREF): HBRUSH;

<*EXTERNAL CreateICA:WINAPI*>
PROCEDURE CreateICA (a1, a2, a3: PCSTR; a4: UNTRACED REF DEVMODEA): HDC;

<*EXTERNAL CreateICW:WINAPI*>
PROCEDURE CreateICW (a1, a2, a3: PCWSTR; a4: UNTRACED REF DEVMODEW): HDC;
CONST CreateIC = CreateICA;

<*EXTERNAL CreateMetaFileA:WINAPI*>
PROCEDURE CreateMetaFileA (a1: PCSTR): HDC;

<*EXTERNAL CreateMetaFileW:WINAPI*>
PROCEDURE CreateMetaFileW (a1: PCWSTR): HDC;
CONST CreateMetaFile = CreateMetaFileA;

<*EXTERNAL CreatePalette:WINAPI*>
PROCEDURE CreatePalette (a1: UNTRACED REF LOGPALETTE): HPALETTE;

<*EXTERNAL CreatePen:WINAPI*>
PROCEDURE CreatePen (a1: INT32; a2: INT32; a3: COLORREF): HPEN;

<*EXTERNAL CreatePenIndirect:WINAPI*>
PROCEDURE CreatePenIndirect (a1: LPLOGPEN): HPEN;

<*EXTERNAL CreatePolyPolygonRgn:WINAPI*>
PROCEDURE CreatePolyPolygonRgn (a1: PPOINT; a2: UNTRACED REF INT32;
                                a3, a4: INT32): HRGN;

<*EXTERNAL CreatePatternBrush:WINAPI*>
PROCEDURE CreatePatternBrush (a1: HBITMAP): HBRUSH;

<*EXTERNAL CreateRectRgn:WINAPI*>
PROCEDURE CreateRectRgn (a1: INT32; a2: INT32; a3: INT32; a4: INT32): HRGN;

<*EXTERNAL CreateRectRgnIndirect:WINAPI*>
PROCEDURE CreateRectRgnIndirect (a1: PRECT): HRGN;

<*EXTERNAL CreateRoundRectRgn:WINAPI*>
PROCEDURE CreateRoundRectRgn (a1, a2, a3, a4, a5, a6: INT32): HRGN;

<*EXTERNAL CreateScalableFontResourceA:WINAPI*>
PROCEDURE CreateScalableFontResourceA (a1: UINT32;
                                         a2: PCSTR;
                                         a3: PCSTR;
                                         a4: PSTR   ): BOOL;

<*EXTERNAL CreateScalableFontResourceW:WINAPI*>
PROCEDURE CreateScalableFontResourceW (a1: UINT32;
                                         a2: PCWSTR;
                                         a3: PCWSTR;
                                         a4: PWSTR   ): BOOL;
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
                              a2: INT32; 
                              a3: UINT32; 
                              a4: LPPIXELFORMATDESCRIPTOR): INT32;

<*EXTERNAL DeviceCapabilitiesEx:WINAPI*>
PROCEDURE DeviceCapabilitiesEx (a1: PSTR;
                                a2: PSTR;
                                a3: PSTR;
                                a4: INT32;
                                a5: PSTR;
                                a6: LPDEVMODE): INT32;

<*EXTERNAL Ellipse:WINAPI*>
PROCEDURE Ellipse (a1: HDC; a2: INT32; a3: INT32; a4: INT32; a5: INT32): BOOL;

<*EXTERNAL EnumFontFamiliesA:WINAPI*>
PROCEDURE EnumFontFamiliesA (a1: HDC;
                               a2: PCSTR;
                               a3: FONTENUMPROC;
                               a4: LPARAM        ): INT32;

<*EXTERNAL EnumFontFamiliesW:WINAPI*>
PROCEDURE EnumFontFamiliesW (a1: HDC;
                               a2: PCWSTR;
                               a3: FONTENUMPROC;
                               a4: LPARAM        ): INT32;
CONST EnumFontFamilies = EnumFontFamiliesA;

<*EXTERNAL EnumFontsA:WINAPI*>
PROCEDURE EnumFontsA (a1: HDC; a2: PCSTR; a3: FONTENUMPROC; a4: LPARAM): INT32;

<*EXTERNAL EnumFontsW:WINAPI*>
PROCEDURE EnumFontsW (a1: HDC;
                      a2: PCWSTR;
                      a3: FONTENUMPROC;
                      a4: LPARAM        ): INT32;
CONST EnumFonts = EnumFontsA;

<*EXTERNAL EnumObjects:WINAPI*>
PROCEDURE EnumObjects (a1: HDC; a2: INT32; a3: GOBJENUMPROC; a4: LPARAM): INT32;

<*EXTERNAL EqualRgn:WINAPI*>
PROCEDURE EqualRgn (a1: HRGN; a2: HRGN): BOOL;

<*EXTERNAL Escape:WINAPI*>
PROCEDURE Escape (a1: HDC; a2: INT32; a3: INT32; a4: PCSTR; a5: PVOID): INT32;

<*EXTERNAL ExtEscape:WINAPI*>
PROCEDURE ExtEscape (a1: HDC;
                     a2: INT32;
                     a3: INT32;
                     a4: PCSTR;
                     a5: INT32;
                     a6: PSTR   ): INT32;

<*EXTERNAL DrawEscape:WINAPI*>
PROCEDURE DrawEscape (a1: HDC; a2: INT32; a3: INT32; a4: PCSTR): INT32;

<*EXTERNAL ExcludeClipRect:WINAPI*>
PROCEDURE ExcludeClipRect (a1: HDC; a2: INT32; a3: INT32; a4: INT32; a5: INT32): INT32;

<*EXTERNAL ExtCreateRegion:WINAPI*>
PROCEDURE ExtCreateRegion (a1: LPXFORM; a2: UINT32; a3: LPRGNDATA): HRGN;

<*EXTERNAL ExtFloodFill:WINAPI*>
PROCEDURE ExtFloodFill (a1: HDC;
                        a2: INT32;
                        a3: INT32;
                        a4: COLORREF;
                        a5: UINT32      ): BOOL;

<*EXTERNAL FillRgn:WINAPI*>
PROCEDURE FillRgn (a1: HDC; a2: HRGN; a3: HBRUSH): BOOL;

<*EXTERNAL FloodFill:WINAPI*>
PROCEDURE FloodFill (a1: HDC; a2: INT32; a3: INT32; a4: COLORREF): BOOL;

<*EXTERNAL FrameRgn:WINAPI*>
PROCEDURE FrameRgn (a1: HDC; a2: HRGN; a3: HBRUSH; a4: INT32; a5: INT32): BOOL;

<*EXTERNAL GetROP2:WINAPI*>
PROCEDURE GetROP2 (a1: HDC): INT32;

<*EXTERNAL GetAspectRatioFilterEx:WINAPI*>
PROCEDURE GetAspectRatioFilterEx (a1: HDC; a2: PSIZE): BOOL;

<*EXTERNAL GetBkColor:WINAPI*>
PROCEDURE GetBkColor (a1: HDC): COLORREF;

<*EXTERNAL GetBkMode:WINAPI*>
PROCEDURE GetBkMode (a1: HDC): INT32;

<*EXTERNAL GetBitmapBits:WINAPI*>
PROCEDURE GetBitmapBits (a1: HBITMAP; a2: INT32; a3: PVOID): INT32;

<*EXTERNAL GetBitmapDimensionEx:WINAPI*>
PROCEDURE GetBitmapDimensionEx (a1: HBITMAP; a2: PSIZE): BOOL;

<*EXTERNAL GetBoundsRect:WINAPI*>
PROCEDURE GetBoundsRect (a1: HDC; a2: PRECT; a3: UINT32): UINT32;

<*EXTERNAL GetBrushOrgEx:WINAPI*>
PROCEDURE GetBrushOrgEx (a1: HDC; a2: PPOINT): BOOL;

<*EXTERNAL GetCharWidthA:WINAPI*>
PROCEDURE GetCharWidthA (a1: HDC; a2: UINT32; a3: UINT32; a4: PINT32): BOOL;

<*EXTERNAL GetCharWidthW:WINAPI*>
PROCEDURE GetCharWidthW (a1: HDC; a2: UINT32; a3: UINT32; a4: PINT32): BOOL;
CONST GetCharWidth = GetCharWidthA;

<*EXTERNAL GetCharWidth32A:WINAPI*>
PROCEDURE GetCharWidth32A (a1: HDC; a2: UINT32; a3: UINT32; a4: PINT32): BOOL;

<*EXTERNAL GetCharWidth32W:WINAPI*>
PROCEDURE GetCharWidth32W (a1: HDC; a2: UINT32; a3: UINT32; a4: PINT32): BOOL;
CONST GetCharWidth32 = GetCharWidth32A;

<*EXTERNAL GetCharWidthFloatA:WINAPI*>
PROCEDURE GetCharWidthFloatA (a1: HDC; a2: UINT32; a3: UINT32; a4: PFLOAT): BOOL;

<*EXTERNAL GetCharWidthFloatW:WINAPI*>
PROCEDURE GetCharWidthFloatW (a1: HDC; a2: UINT32; a3: UINT32; a4: PFLOAT): BOOL;
CONST GetCharWidthFloat = GetCharWidthFloatA;

<*EXTERNAL GetCharABCWidthsA:WINAPI*>
PROCEDURE GetCharABCWidthsA (a1: HDC; a2: UINT32; a3: UINT32; a4: LPABC): BOOL;

<*EXTERNAL GetCharABCWidthsW:WINAPI*>
PROCEDURE GetCharABCWidthsW (a1: HDC; a2: UINT32; a3: UINT32; a4: LPABC): BOOL;
CONST GetCharABCWidths = GetCharABCWidthsA;

<*EXTERNAL GetCharABCWidthsFloatA:WINAPI*>
PROCEDURE GetCharABCWidthsFloatA (a1: HDC;
                                    a2: UINT32;
                                    a3: UINT32;
                                    a4: LPABCFLOAT): BOOL;

<*EXTERNAL GetCharABCWidthsFloatW:WINAPI*>
PROCEDURE GetCharABCWidthsFloatW (a1: HDC;
                                    a2: UINT32;
                                    a3: UINT32;
                                    a4: LPABCFLOAT): BOOL;
CONST GetCharABCWidthsFloat = GetCharABCWidthsFloatA;

<*EXTERNAL GetClipBox:WINAPI*>
PROCEDURE GetClipBox (a1: HDC; a2: PRECT): INT32;

<*EXTERNAL GetClipRgn:WINAPI*>
PROCEDURE GetClipRgn (a1: HDC; a2: HRGN): INT32;

<*EXTERNAL GetMetaRgn:WINAPI*>
PROCEDURE GetMetaRgn (a1: HDC; a2: HRGN): INT32;

<*EXTERNAL GetCurrentObject:WINAPI*>
PROCEDURE GetCurrentObject (a1: HDC; a2: UINT32): HGDIOBJ;

<*EXTERNAL GetCurrentPositionEx:WINAPI*>
PROCEDURE GetCurrentPositionEx (a1: HDC; a2: PPOINT): BOOL;

<*EXTERNAL GetDeviceCaps:WINAPI*>
PROCEDURE GetDeviceCaps (a1: HDC; a2: INT32): INT32;

<*EXTERNAL GetDIBits:WINAPI*>
PROCEDURE GetDIBits (a1: HDC;
                     a2: HBITMAP;
                     a3: UINT32;
                     a4: UINT32;
                     a5: PVOID;
                     a6: LPBITMAPINFO;
                     a7: UINT32          ): INT32;

<*EXTERNAL GetFontData:WINAPI*>
PROCEDURE GetFontData (a1: HDC;
                       a2: UINT32;
                       a3: UINT32;
                       a4: PVOID;
                       a5: UINT32   ): UINT32;

<*EXTERNAL GetGlyphOutline:WINAPI*>
PROCEDURE GetGlyphOutline (a1: HDC;
                           a2: UINT32;
                           a3: UINT32;
                           a4: LPGLYPHMETRICS;
                           a5: UINT32;
                           a6: PVOID;
                           a7: UNTRACED REF MAT2): UINT32;

<*EXTERNAL GetMapMode:WINAPI*>
PROCEDURE GetMapMode (a1: HDC): INT32;

<*EXTERNAL GetMetaFileBitsEx:WINAPI*>
PROCEDURE GetMetaFileBitsEx (a1: HMETAFILE; a2: UINT32; a3: PVOID): UINT32;

<*EXTERNAL GetMetaFileA:WINAPI*>
PROCEDURE GetMetaFileA (a1: PCSTR): HMETAFILE;

<*EXTERNAL GetMetaFileW:WINAPI*>
PROCEDURE GetMetaFileW (a1: PCWSTR): HMETAFILE;
CONST GetMetaFile = GetMetaFileA;

<*EXTERNAL GetNearestColor:WINAPI*>
PROCEDURE GetNearestColor (a1: HDC; a2: COLORREF): COLORREF;

<*EXTERNAL GetNearestPaletteIndex:WINAPI*>
PROCEDURE GetNearestPaletteIndex (a1: HPALETTE; a2: COLORREF): UINT32;

<*EXTERNAL GetObjectType:WINAPI*>
PROCEDURE GetObjectType (h: HGDIOBJ): UINT32;

<*EXTERNAL GetOutlineTextMetricsA:WINAPI*>
PROCEDURE GetOutlineTextMetricsA (a1: HDC;
                                    a2: UINT32;
                                    a3: LPOUTLINETEXTMETRICA): UINT32;

<*EXTERNAL GetOutlineTextMetricsW:WINAPI*>
PROCEDURE GetOutlineTextMetricsW (a1: HDC;
                                  a2: UINT32;
                                  a3: LPOUTLINETEXTMETRICW): UINT32;
CONST GetOutlineTextMetrics = GetOutlineTextMetricsA;

<*EXTERNAL GetPaletteEntries:WINAPI*>
PROCEDURE GetPaletteEntries (a1: HPALETTE;
                             a2: UINT32;
                             a3: UINT32;
                             a4: LPPALETTEENTRY): UINT32;

<*EXTERNAL GetPixel:WINAPI*>
PROCEDURE GetPixel (a1: HDC; a2: INT32; a3: INT32): COLORREF;

<*EXTERNAL GetPixelFormat:WINAPI*>
PROCEDURE GetPixelFormat (a1: HDC): INT32;

<*EXTERNAL GetPolyFillMode:WINAPI*>
PROCEDURE GetPolyFillMode (a1: HDC): INT32;

<*EXTERNAL GetRasterizerCaps:WINAPI*>
PROCEDURE GetRasterizerCaps (a1: LPRASTERIZER_STATUS; a2: UINT32): BOOL;

<*EXTERNAL GetRegionData:WINAPI*>
PROCEDURE GetRegionData (a1: HRGN; a2: UINT32; a3: LPRGNDATA): UINT32;

<*EXTERNAL GetRgnBox:WINAPI*>
PROCEDURE GetRgnBox (a1: HRGN; a2: PRECT): INT32;

<*EXTERNAL GetStockObject:WINAPI*>
PROCEDURE GetStockObject (a1: INT32): HGDIOBJ;

<*EXTERNAL GetStretchBltMode:WINAPI*>
PROCEDURE GetStretchBltMode (a1: HDC): INT32;

<*EXTERNAL GetSystemPaletteEntries:WINAPI*>
PROCEDURE GetSystemPaletteEntries (a1: HDC;
                                     a2: UINT32;
                                     a3: UINT32;
                                     a4: LPPALETTEENTRY): UINT32;

<*EXTERNAL GetSystemPaletteUse:WINAPI*>
PROCEDURE GetSystemPaletteUse (a1: HDC): UINT32;

<*EXTERNAL GetTextCharacterExtra:WINAPI*>
PROCEDURE GetTextCharacterExtra (a1: HDC): INT32;

<*EXTERNAL GetTextAlign:WINAPI*>
PROCEDURE GetTextAlign (a1: HDC): UINT32;

<*EXTERNAL GetTextColor:WINAPI*>
PROCEDURE GetTextColor (a1: HDC): COLORREF;

<*EXTERNAL GetTextExtentPointA:WINAPI*>
PROCEDURE GetTextExtentPointA (a1: HDC; a2: PCSTR; a3: INT32; a4: PSIZE): BOOL;

<*EXTERNAL GetTextExtentPointW:WINAPI*>
PROCEDURE GetTextExtentPointW (a1: HDC; a2: PCWSTR; a3: INT32; a4: PSIZE): BOOL;
CONST GetTextExtentPoint = GetTextExtentPointA;

<*EXTERNAL GetTextExtentPoint32A:WINAPI*>
PROCEDURE GetTextExtentPoint32A (a1: HDC; a2: PCSTR; a3: INT32; a4: PSIZE): BOOL;

<*EXTERNAL GetTextExtentPoint32W:WINAPI*>
PROCEDURE GetTextExtentPoint32W (a1: HDC; a2: PCWSTR; a3: INT32; a4: PSIZE): BOOL;
CONST GetTextExtentPoint32 = GetTextExtentPoint32A;

<*EXTERNAL GetTextExtentExPointA:WINAPI*>
PROCEDURE GetTextExtentExPointA (a1: HDC;
                                 a2: PCSTR;
                                 a3: INT32;
                                 a4: INT32;
                                 a5: PINT32;
                                 a6: PINT32;
                                 a7: PSIZE  ): BOOL;

<*EXTERNAL GetTextExtentExPointW:WINAPI*>
PROCEDURE GetTextExtentExPointW (a1: HDC;
                                 a2: PCWSTR;
                                 a3: INT32;
                                 a4: INT32;
                                 a5: PINT32;
                                 a6: PINT32;
                                 a7: PSIZE   ): BOOL;
CONST GetTextExtentExPoint = GetTextExtentExPointA;

<*EXTERNAL GetViewportExtEx:WINAPI*>
PROCEDURE GetViewportExtEx (a1: HDC; a2: PSIZE): BOOL;

<*EXTERNAL GetViewportOrgEx:WINAPI*>
PROCEDURE GetViewportOrgEx (a1: HDC; a2: PPOINT): BOOL;

<*EXTERNAL GetWindowExtEx:WINAPI*>
PROCEDURE GetWindowExtEx (a1: HDC; a2: PSIZE): BOOL;

<*EXTERNAL GetWindowOrgEx:WINAPI*>
PROCEDURE GetWindowOrgEx (a1: HDC; a2: PPOINT): BOOL;

<*EXTERNAL IntersectClipRect:WINAPI*>
PROCEDURE IntersectClipRect (a1: HDC; a2: INT32; a3: INT32; a4: INT32; a5: INT32): INT32;

<*EXTERNAL InvertRgn:WINAPI*>
PROCEDURE InvertRgn (a1: HDC; a2: HRGN): BOOL;

<*EXTERNAL LineDDA:WINAPI*>
PROCEDURE LineDDA (a1, a2, a3, a4: INT32; a5: LINEDDAPROC;  a6: LPARAM): BOOL;

<*EXTERNAL LineTo:WINAPI*>
PROCEDURE LineTo (a1: HDC; a2: INT32; a3: INT32): BOOL;

<*EXTERNAL MaskBlt:WINAPI*>
PROCEDURE MaskBlt (a1: HDC;  a2, a3, a4, a5: INT32;  a6: HDC;
                   a7, a8: INT32;  a9: HBITMAP;  a10, a11: INT32;
                   a12: UINT32): BOOL;

<*EXTERNAL PlgBlt:WINAPI*>
PROCEDURE PlgBlt (a1: HDC;  a2: PPOINT;  a3: HDC;  a4, a5, a6, a7: INT32;
                  a8: HBITMAP;   a9, a10: INT32): BOOL;

<*EXTERNAL OffsetClipRgn:WINAPI*>
PROCEDURE OffsetClipRgn (a1: HDC; a2: INT32; a3: INT32): INT32;

<*EXTERNAL OffsetRgn:WINAPI*>
PROCEDURE OffsetRgn (a1: HRGN; a2: INT32; a3: INT32): INT32;

<*EXTERNAL PatBlt:WINAPI*>
PROCEDURE PatBlt (a1: HDC;  a2, a3, a4, a5: INT32;  a6: UINT32): BOOL;

<*EXTERNAL Pie:WINAPI*>
PROCEDURE Pie (a1: HDC; a2, a3, a4, a5, a6, a7, a8, a9: INT32): BOOL;

<*EXTERNAL PlayMetaFile:WINAPI*>
PROCEDURE PlayMetaFile (a1: HDC; a2: HMETAFILE): BOOL;

<*EXTERNAL PaintRgn:WINAPI*>
PROCEDURE PaintRgn (a1: HDC; a2: HRGN): BOOL;

<*EXTERNAL PolyPolygon:WINAPI*>
PROCEDURE PolyPolygon (a1: HDC;  a2: PPOINT;  a3: PINT32;  a4: INT32): BOOL;

<*EXTERNAL PtInRegion:WINAPI*>
PROCEDURE PtInRegion (a1: HRGN; a2: INT32; a3: INT32): BOOL;

<*EXTERNAL PtVisible:WINAPI*>
PROCEDURE PtVisible (a1: HDC; a2: INT32; a3: INT32): BOOL;

<*EXTERNAL RectInRegion:WINAPI*>
PROCEDURE RectInRegion (a1: HRGN; a2: PRECT): BOOL;

<*EXTERNAL RectVisible:WINAPI*>
PROCEDURE RectVisible (a1: HDC; a2: PRECT): BOOL;

<*EXTERNAL Rectangle:WINAPI*>
PROCEDURE Rectangle (a1: HDC; a2: INT32; a3: INT32; a4: INT32; a5: INT32): BOOL;

<*EXTERNAL RestoreDC:WINAPI*>
PROCEDURE RestoreDC (a1: HDC; a2: INT32): BOOL;

<*EXTERNAL RealizePalette:WINAPI*>
PROCEDURE RealizePalette (a1: HDC): UINT32;

<*EXTERNAL RemoveFontModule:WINAPI*>
PROCEDURE RemoveFontModule (a1: HMODULE): BOOL;

<*EXTERNAL RemoveFontResourceA:WINAPI*>
PROCEDURE RemoveFontResourceA (a1: PSTR): BOOL;

<*EXTERNAL RemoveFontResourceW:WINAPI*>
PROCEDURE RemoveFontResourceW (a1: PWSTR): BOOL;
CONST RemoveFontResource = RemoveFontResourceA;

<*EXTERNAL RoundRect:WINAPI*>
PROCEDURE RoundRect (a1: HDC;  a2, a3, a4, a5, a6, a7: INT32): BOOL;

<*EXTERNAL ResizePalette:WINAPI*>
PROCEDURE ResizePalette (a1: HPALETTE; a2: UINT32): BOOL;

<*EXTERNAL SaveDC:WINAPI*>
PROCEDURE SaveDC (a1: HDC): INT32;

<*EXTERNAL SelectClipRgn:WINAPI*>
PROCEDURE SelectClipRgn (a1: HDC; a2: HRGN): INT32;

<*EXTERNAL ExtSelectClipRgn:WINAPI*>
PROCEDURE ExtSelectClipRgn (a1: HDC; a2: HRGN; a3: INT32): INT32;

<*EXTERNAL SetMetaRgn:WINAPI*>
PROCEDURE SetMetaRgn (a1: HDC): INT32;

<*EXTERNAL SelectObject:WINAPI*>
PROCEDURE SelectObject (a1: HDC; a2: HGDIOBJ): HGDIOBJ;

<*EXTERNAL SelectPalette:WINAPI*>
PROCEDURE SelectPalette (a1: HDC; a2: HPALETTE; a3: BOOL): HPALETTE;

<*EXTERNAL SetBkColor:WINAPI*>
PROCEDURE SetBkColor (a1: HDC; a2: COLORREF): COLORREF;

<*EXTERNAL SetBkMode:WINAPI*>
PROCEDURE SetBkMode (a1: HDC; a2: INT32): INT32;

<*EXTERNAL SetBitmapBits:WINAPI*>
PROCEDURE SetBitmapBits (a1: HBITMAP; a2: UINT32; a3: PVOID): INT32;

<*EXTERNAL SetBoundsRect:WINAPI*>
PROCEDURE SetBoundsRect (a1: HDC; a2: PRECT; a3: UINT32): UINT32;

<*EXTERNAL SetDIBits:WINAPI*>
PROCEDURE SetDIBits (a1: HDC;
                     a2: HBITMAP;
                     a3: UINT32;
                     a4: UINT32;
                     a5: PVOID;
                     a6: LPBITMAPINFO;
                     a7: UINT32          ): INT32;

<*EXTERNAL SetDIBitsToDevice:WINAPI*>
PROCEDURE SetDIBitsToDevice (a1 : HDC;
                             a2 : INT32;
                             a3 : INT32;
                             a4 : UINT32;
                             a5 : UINT32;
                             a6 : INT32;
                             a7 : INT32;
                             a8 : UINT32;
                             a9 : UINT32;
                             a10: PVOID;
                             a11: LPBITMAPINFO;
                             a12: UINT32          ): INT32;

<*EXTERNAL SetMapperFlags:WINAPI*>
PROCEDURE SetMapperFlags (a1: HDC; a2: UINT32): UINT32;

<*EXTERNAL SetGraphicsMode:WINAPI*>
PROCEDURE SetGraphicsMode(hdc: HDC; iMode: INT32): INT32;

<*EXTERNAL SetMapMode:WINAPI*>
PROCEDURE SetMapMode (a1: HDC; a2: INT32): INT32;

<*EXTERNAL SetMetaFileBitsEx:WINAPI*>
PROCEDURE SetMetaFileBitsEx (a1: UINT32; a2: PUINT8): HMETAFILE;

<*EXTERNAL SetPaletteEntries:WINAPI*>
PROCEDURE SetPaletteEntries (a1: HPALETTE;
                             a2: UINT32;
                             a3: UINT32;
                             a4: UNTRACED REF PALETTEENTRY): UINT32;

<*EXTERNAL SetPixel:WINAPI*>
PROCEDURE SetPixel (a1: HDC; a2: INT32; a3: INT32; a4: COLORREF): COLORREF;

<*EXTERNAL SetPixelV:WINAPI*>
PROCEDURE SetPixelV (a1: HDC; a2: INT32; a3: INT32; a4: COLORREF): BOOL;

<*EXTERNAL SetPixelFormat:WINAPI*>
PROCEDURE SetPixelFormat (a1: HDC; a2: INT32; a3: LPPIXELFORMATDESCRIPTOR): BOOL;

<*EXTERNAL SetPolyFillMode:WINAPI*>
PROCEDURE SetPolyFillMode (a1: HDC; a2: INT32): INT32;

<*EXTERNAL StretchBlt:WINAPI*>
PROCEDURE StretchBlt (a1: HDC;  a2, a3, a4, a5: INT32;  a6: HDC;
                      a7, a8, a9, a10: INT32;  a11: UINT32): BOOL;

<*EXTERNAL SetRectRgn:WINAPI*>
PROCEDURE raw_SetRectRgn (a1: HRGN; a2: INT32; a3: INT32; a4: INT32; a5: INT32): BOOL;

PROCEDURE SetRectRgn (a1: HRGN; a2: INT32; a3: INT32; a4: INT32; a5: INT32): BOOL;

<*EXTERNAL StretchDIBits:WINAPI*>
PROCEDURE StretchDIBits (a1: HDC;  a2, a3, a4, a5, a6, a7, a8, a9: INT32;
                         a10: PVOID;
                         a11: LPBITMAPINFO;
                         a12: UINT32;
                         a13: UINT32         ): INT32;

<*EXTERNAL SetROP2:WINAPI*>
PROCEDURE SetROP2 (a1: HDC; a2: INT32): INT32;

<*EXTERNAL SetStretchBltMode:WINAPI*>
PROCEDURE SetStretchBltMode (a1: HDC; a2: INT32): INT32;

<*EXTERNAL SetSystemPaletteUse:WINAPI*>
PROCEDURE SetSystemPaletteUse (a1: HDC; a2: UINT32): UINT32;

<*EXTERNAL SetTextCharacterExtra:WINAPI*>
PROCEDURE SetTextCharacterExtra (a1: HDC; a2: INT32): INT32;

<*EXTERNAL SetTextColor:WINAPI*>
PROCEDURE SetTextColor (a1: HDC; a2: COLORREF): COLORREF;

<*EXTERNAL SetTextAlign:WINAPI*>
PROCEDURE SetTextAlign (a1: HDC; a2: UINT32): UINT32;

<*EXTERNAL SetTextJustification:WINAPI*>
PROCEDURE SetTextJustification (a1: HDC; a2: INT32; a3: INT32): BOOL;

<*EXTERNAL UpdateColors:WINAPI*>
PROCEDURE UpdateColors (a1: HDC): BOOL;

<*EXTERNAL PlayMetaFileRecord:WINAPI*>
PROCEDURE PlayMetaFileRecord (a1: HDC;
                              a2: LPHANDLETABLE;
                              a3: LPMETARECORD;
                              a4: UINT32           ): BOOL;

TYPE
  MFENUMPROC = <*CALLBACK*> PROCEDURE (a1: HDC;
                                       a2: UNTRACED REF HANDLETABLE;
                                       a3: UNTRACED REF METARECORD;
                                       a4: INT32;
                                       a5: LPARAM                    ): INT32;

<*EXTERNAL EnumMetaFile:WINAPI*>
PROCEDURE EnumMetaFile (a1: HDC;
                        a2: HMETAFILE;
                        a3: MFENUMPROC;
                        a4: LPARAM      ): BOOL;

TYPE
  ENHMFENUMPROC = <*CALLBACK*> PROCEDURE (a1: HDC;
                                          a2: UNTRACED REF HANDLETABLE;
                                          a3: UNTRACED REF ENHMETARECORD;
                                          a4: INT32;
                                          a5: LPARAM                    ): INT32;

(* Enhanced Metafile Function Declarations *)

<*EXTERNAL CloseEnhMetaFile:WINAPI*>
PROCEDURE CloseEnhMetaFile (a1: HDC): HENHMETAFILE;

<*EXTERNAL CopyEnhMetaFileA:WINAPI*>
PROCEDURE CopyEnhMetaFileA (a1: HENHMETAFILE; a2: PSTR): HENHMETAFILE;

<*EXTERNAL CopyEnhMetaFileW:WINAPI*>
PROCEDURE CopyEnhMetaFileW (a1: HENHMETAFILE; a2: PWSTR): HENHMETAFILE;
CONST CopyEnhMetaFile = CopyEnhMetaFileA;

<*EXTERNAL CreateEnhMetaFileA:WINAPI*>
PROCEDURE CreateEnhMetaFileA (a1: HDC; a2: PSTR; a3: PRECT; a4: PSTR): HDC;

<*EXTERNAL CreateEnhMetaFileW:WINAPI*>
PROCEDURE CreateEnhMetaFileW (a1: HDC; a2: PWSTR; a3: PRECT; a4: PWSTR): HDC;
CONST CreateEnhMetaFile = CreateEnhMetaFileA;

<*EXTERNAL DeleteEnhMetaFile:WINAPI*>
PROCEDURE DeleteEnhMetaFile (a1: HENHMETAFILE): BOOL;

<*EXTERNAL EnumEnhMetaFile:WINAPI*>
PROCEDURE EnumEnhMetaFile (a1: HDC;
                             a2: HENHMETAFILE;
                             a3: ENHMFENUMPROC;
                             a4: PVOID;
                             a5: PRECT         ): BOOL;

<*EXTERNAL GetEnhMetaFileA:WINAPI*>
PROCEDURE GetEnhMetaFileA (a1: PSTR): HENHMETAFILE;

<*EXTERNAL GetEnhMetaFileW:WINAPI*>
PROCEDURE GetEnhMetaFileW (a1: PWSTR): HENHMETAFILE;
CONST GetEnhMetaFile = GetEnhMetaFileA;

<*EXTERNAL GetEnhMetaFileBits:WINAPI*>
PROCEDURE GetEnhMetaFileBits (a1: HENHMETAFILE; a2: UINT32; a3: PUINT8): UINT32;

<*EXTERNAL GetEnhMetaFileDescriptionA:WINAPI*>
PROCEDURE GetEnhMetaFileDescriptionA (a1: HENHMETAFILE;
                                      a2: UINT32;
                                      a3: PSTR         ): UINT32;

<*EXTERNAL GetEnhMetaFileDescriptionW:WINAPI*>
PROCEDURE GetEnhMetaFileDescriptionW (a1: HENHMETAFILE;
                                      a2: UINT32;
                                      a3: PWSTR        ): UINT32;
CONST GetEnhMetaFileDescription = GetEnhMetaFileDescriptionA;

<*EXTERNAL GetEnhMetaFileHeader:WINAPI*>
PROCEDURE GetEnhMetaFileHeader (a1: HENHMETAFILE;
                                a2: UINT32;
                                a3: LPENHMETAHEADER): UINT32;

<*EXTERNAL GetEnhMetaFilePaletteEntries:WINAPI*>
PROCEDURE GetEnhMetaFilePaletteEntries (a1: HENHMETAFILE;
                                        a2: UINT32;
                                        a3: LPPALETTEENTRY): UINT32;

<*EXTERNAL GetWinMetaFileBits:WINAPI*>
PROCEDURE GetWinMetaFileBits (a1: HENHMETAFILE;
                              a2: UINT32;
                              a3: PUINT8;
                              a4: INT32;
                              a5: HDC           ): UINT32;

<*EXTERNAL PlayEnhMetaFile:WINAPI*>
PROCEDURE PlayEnhMetaFile (a1: HDC; a2: HENHMETAFILE; a3: PRECT): BOOL;

<*EXTERNAL PlayEnhMetaFileRecord:WINAPI*>
PROCEDURE PlayEnhMetaFileRecord (a1: HDC;
                                 a2: LPHANDLETABLE;
                                 a3: LPENHMETARECORD;
                                 a4: UINT32             ): BOOL;

<*EXTERNAL SetEnhMetaFileBits:WINAPI*>
PROCEDURE SetEnhMetaFileBits (a1: UINT32; a2: PUINT8): HENHMETAFILE;

<*EXTERNAL SetWinMetaFileBits:WINAPI*>
PROCEDURE SetWinMetaFileBits (a1: UINT32;
                              a2: PUINT8;
                              a3: HDC;
                              a4: LPMETAFILEPICT): HENHMETAFILE;

<*EXTERNAL GdiComment:WINAPI*>
PROCEDURE GdiComment (a1: HDC; a2: UINT32; a3: PUINT8): BOOL;

<*EXTERNAL GetTextMetricsA:WINAPI*>
PROCEDURE GetTextMetricsA (a1: HDC; a2: LPTEXTMETRICA): BOOL;

<*EXTERNAL GetTextMetricsW:WINAPI*>
PROCEDURE GetTextMetricsW (a1: HDC; a2: LPTEXTMETRICW): BOOL;
CONST GetTextMetrics = GetTextMetricsA;

(* new GDI *)

<*EXTERNAL AngleArc:WINAPI*>
PROCEDURE AngleArc (a1: HDC; a2, a3: INT32;  a4: UINT32;  a5, a6: WFLOAT): BOOL;

<*EXTERNAL PolyPolyline:WINAPI*>
PROCEDURE PolyPolyline (a1: HDC; a2: PPOINT; a3: PUINT32; a4: UINT32): BOOL;

<*EXTERNAL GetWorldTransform:WINAPI*>
PROCEDURE GetWorldTransform (a1: HDC; a2: LPXFORM): BOOL;

<*EXTERNAL SetWorldTransform:WINAPI*>
PROCEDURE SetWorldTransform (a1: HDC; a2: LPXFORM): BOOL;

<*EXTERNAL ModifyWorldTransform:WINAPI*>
PROCEDURE ModifyWorldTransform (a1: HDC; a2: LPXFORM; a3: UINT32): BOOL;

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
  RGB_GAMMA_MIN: UINT16 = 8_02500;
  RGB_GAMMA_MAX: UINT16 = 65000;

(* Min and max for ReferenceBlack and ReferenceWhite *)
CONST
  REFERENCE_WHITE_MIN: UINT16 = 6000;
  REFERENCE_WHITE_MAX: UINT16 = 10000;
  REFERENCE_BLACK_MIN: UINT16 = 0;
  REFERENCE_BLACK_MAX: UINT16 = 4000;

(* Min and max for Contrast, Brightness, Colorfulness, RedGreenTint *)
CONST
  COLOR_ADJ_MIN: INT16 = -100;
  COLOR_ADJ_MAX: INT16 = 100;

TYPE
  COLORADJUSTMENT = RECORD
    caSize           : UINT16;
    caFlags          : UINT16;
    caIlluminantIndex: UINT16;
    caRedGamma       : UINT16;
    caGreenGamma     : UINT16;
    caBlueGamma      : UINT16;
    caReferenceBlack : UINT16;
    caReferenceWhite : UINT16;
    caContrast       : INT16;
    caBrightness     : INT16;
    caColorfulness   : INT16;
    caRedGreenTint   : INT16;
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
  ABORTPROC = <*CALLBACK*> PROCEDURE (a1: HDC; a2: INT32): BOOL;

  LPDOCINFOA = UNTRACED REF DOCINFOA;
  DOCINFOA = RECORD
    cbSize     : INT32;
    lpszDocName: PSTR;
    lpszOutput : PSTR;
  END;

  LPDOCINFOW = UNTRACED REF DOCINFOW;
  DOCINFOW = RECORD
    cbSize     : INT32;
    lpszDocName: PWSTR;
    lpszOutput : PWSTR;
  END;

  DOCINFO = DOCINFOA;
  LPDOCINFO = LPDOCINFOA;

<*EXTERNAL StartDocA:WINAPI*>
PROCEDURE StartDocA (a1: HDC; a2: LPDOCINFOA): INT32;

<*EXTERNAL StartDocW:WINAPI*>
PROCEDURE StartDocW (a1: HDC; a2: LPDOCINFOW): INT32;
CONST StartDoc = StartDocA;

<*EXTERNAL EndDoc:WINAPI*>
PROCEDURE EndDoc (a1: HDC): INT32;

<*EXTERNAL StartPage:WINAPI*>
PROCEDURE StartPage (a1: HDC): INT32;

<*EXTERNAL EndPage:WINAPI*>
PROCEDURE EndPage (a1: HDC): INT32;

<*EXTERNAL AbortDoc:WINAPI*>
PROCEDURE AbortDoc (a1: HDC): INT32;

<*EXTERNAL SetAbortProc:WINAPI*>
PROCEDURE SetAbortProc (a1: HDC; a2: ABORTPROC): INT32;

<*EXTERNAL GdiPlayJournal:WINAPI*>
PROCEDURE GdiPlayJournal (a1: HDC; a2: PCSTR; a3: UINT32; a4: UINT32): BOOL;

<*EXTERNAL AbortPath:WINAPI*>
PROCEDURE AbortPath (a1: HDC): BOOL;

<*EXTERNAL ArcTo:WINAPI*>
PROCEDURE ArcTo (a1: HDC; a2, a3, a4, a5, a6, a7, a8, a9: INT32): BOOL;

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
PROCEDURE GetPath (a1: HDC; a2: PPOINT; a3: PUINT8; a4: INT32): INT32;

<*EXTERNAL PathToRegion:WINAPI*>
PROCEDURE PathToRegion (a1: HDC): HRGN;

<*EXTERNAL PolyDraw:WINAPI*>
PROCEDURE PolyDraw (a1: HDC; a2: PPOINT; a3: PUINT8; a4: INT32): BOOL;

<*EXTERNAL SelectClipPath:WINAPI*>
PROCEDURE SelectClipPath (a1: HDC; a2: INT32): BOOL;

<*EXTERNAL SetArcDirection:WINAPI*>
PROCEDURE SetArcDirection (a1: HDC; a2: INT32): INT32;

<*EXTERNAL SetMiterLimit:WINAPI*>
PROCEDURE SetMiterLimit (a1: HDC; a2: WFLOAT; a3: PFLOAT): BOOL;

<*EXTERNAL StrokeAndFillPath:WINAPI*>
PROCEDURE StrokeAndFillPath (a1: HDC): BOOL;

<*EXTERNAL StrokePath:WINAPI*>
PROCEDURE StrokePath (a1: HDC): BOOL;

<*EXTERNAL WidenPath:WINAPI*>
PROCEDURE WidenPath (a1: HDC): BOOL;

<*EXTERNAL ExtCreatePen:WINAPI*>
PROCEDURE ExtCreatePen (a1: UINT32;
                        a2: UINT32;
                        a3: LPLOGBRUSH;
                        a4: UINT32;
                        a5: PUINT32     ): HPEN;

<*EXTERNAL GetMiterLimit:WINAPI*>
PROCEDURE GetMiterLimit (a1: HDC; a2: PFLOAT): BOOL;

<*EXTERNAL GetArcDirection:WINAPI*>
PROCEDURE GetArcDirection (a1: HDC): INT32;

<*EXTERNAL GetObjectA:WINAPI*>
PROCEDURE GetObjectA (a1: HGDIOBJ; a2: INT32; a3: PVOID): INT32;

<*EXTERNAL GetObjectW:WINAPI*>
PROCEDURE GetObjectW (a1: HGDIOBJ; a2: INT32; a3: PVOID): INT32;
CONST GetObject = GetObjectA;

<*EXTERNAL MoveToEx:WINAPI*>
PROCEDURE MoveToEx (a1: HDC; a2: INT32; a3: INT32; a4: PPOINT): BOOL;

<*EXTERNAL TextOutA:WINAPI*>
PROCEDURE TextOutA (a1: HDC; a2: INT32; a3: INT32; a4: PCSTR; a5: INT32): BOOL;

<*EXTERNAL TextOutW:WINAPI*>
PROCEDURE TextOutW (a1: HDC; a2: INT32; a3: INT32; a4: PCWSTR; a5: INT32): BOOL;
CONST TextOut = TextOutA;

<*EXTERNAL ExtTextOutA:WINAPI*>
PROCEDURE ExtTextOutA (a1: HDC;
                       a2: INT32;
                       a3: INT32;
                       a4: UINT32;
                       a5: PRECT;
                       a6: PCSTR;
                       a7: UINT32;
                       a8: PINT32              ): BOOL;

<*EXTERNAL ExtTextOutW:WINAPI*>
PROCEDURE ExtTextOutW (a1: HDC;
                       a2: INT32;
                       a3: INT32;
                       a4: UINT32;
                       a5: PRECT;
                       a6: PCWSTR;
                       a7: UINT32;
                       a8: PINT32              ): BOOL;
CONST ExtTextOut = ExtTextOutA;

<*EXTERNAL PolyTextOutA:WINAPI*>
PROCEDURE PolyTextOutA (a1: HDC; a2: UNTRACED REF POLYTEXTA; a3: INT32): BOOL;

<*EXTERNAL PolyTextOutW:WINAPI*>
PROCEDURE PolyTextOutW (a1: HDC; a2: UNTRACED REF POLYTEXTW; a3: INT32): BOOL;
CONST PolyTextOut = PolyTextOutA;

<*EXTERNAL CreatePolygonRgn:WINAPI*>
PROCEDURE CreatePolygonRgn (a1: PPOINT; a2: INT32; a3: INT32): HRGN;

<*EXTERNAL DPtoLP:WINAPI*>
PROCEDURE DPtoLP (a1: HDC; a2: PPOINT; a3: INT32): BOOL;

<*EXTERNAL LPtoDP:WINAPI*>
PROCEDURE LPtoDP (a1: HDC; a2: PPOINT; a3: INT32): BOOL;

<*EXTERNAL Polygon:WINAPI*>
PROCEDURE Polygon (a1: HDC; a2: PPOINT; a3: INT32): BOOL;

<*EXTERNAL Polyline:WINAPI*>
PROCEDURE Polyline (a1: HDC; a2: PPOINT; a3: INT32): BOOL;

<*EXTERNAL PolyBezier:WINAPI*>
PROCEDURE PolyBezier (a1: HDC; a2: PPOINT; a3: UINT32): BOOL;

<*EXTERNAL PolyBezierTo:WINAPI*>
PROCEDURE PolyBezierTo (a1: HDC; a2: PPOINT; a3: UINT32): BOOL;

<*EXTERNAL PolylineTo:WINAPI*>
PROCEDURE PolylineTo (a1: HDC; a2: PPOINT; a3: UINT32): BOOL;

<*EXTERNAL SetViewportExtEx:WINAPI*>
PROCEDURE SetViewportExtEx (a1: HDC; a2: INT32; a3: INT32; a4: PSIZE): BOOL;

<*EXTERNAL SetViewportOrgEx:WINAPI*>
PROCEDURE SetViewportOrgEx (a1: HDC; a2: INT32; a3: INT32; a4: PPOINT): BOOL;

<*EXTERNAL SetWindowExtEx:WINAPI*>
PROCEDURE SetWindowExtEx (a1: HDC; a2: INT32; a3: INT32; a4: PSIZE): BOOL;

<*EXTERNAL SetWindowOrgEx:WINAPI*>
PROCEDURE SetWindowOrgEx (a1: HDC; a2: INT32; a3: INT32; a4: PPOINT): BOOL;

<*EXTERNAL OffsetViewportOrgEx:WINAPI*>
PROCEDURE OffsetViewportOrgEx (a1: HDC; a2: INT32; a3: INT32; a4: PPOINT): BOOL;

<*EXTERNAL OffsetWindowOrgEx:WINAPI*>
PROCEDURE OffsetWindowOrgEx (a1: HDC; a2: INT32; a3: INT32; a4: PPOINT): BOOL;

<*EXTERNAL ScaleViewportExtEx:WINAPI*>
PROCEDURE ScaleViewportExtEx (a1: HDC;
                              a2: INT32;
                              a3: INT32;
                              a4: INT32;
                              a5: INT32;
                              a6: PSIZE): BOOL;

<*EXTERNAL ScaleWindowExtEx:WINAPI*>
PROCEDURE ScaleWindowExtEx (a1: HDC;
                            a2: INT32;
                            a3: INT32;
                            a4: INT32;
                            a5: INT32;
                            a6: PSIZE): BOOL;

<*EXTERNAL SetBitmapDimensionEx:WINAPI*>
PROCEDURE SetBitmapDimensionEx (a1: HBITMAP; a2: INT32; a3: INT32; a4: PSIZE): BOOL;

<*EXTERNAL SetBrushOrgEx:WINAPI*>
PROCEDURE SetBrushOrgEx (a1: HDC; a2: INT32; a3: INT32; a4: PPOINT): BOOL;

<*EXTERNAL GetTextFaceA:WINAPI*>
PROCEDURE GetTextFaceA (a1: HDC; a2: INT32; a3: PSTR): INT32;

<*EXTERNAL GetTextFaceW:WINAPI*>
PROCEDURE GetTextFaceW (a1: HDC; a2: INT32; a3: PWSTR): INT32;
CONST GetTextFace = GetTextFaceA;

CONST FONTMAPPER_MAX = 10;

<*EXTERNAL EnumNearestFontsA:WINAPI*>
PROCEDURE EnumNearestFontsA (a1: HDC;
                             a2: LPEXTLOGFONTA;
                             a3: UINT32;
                             a4: LPFMATCHA      ): UINT32;

<*EXTERNAL EnumNearestFontsW:WINAPI*>
PROCEDURE EnumNearestFontsW (a1: HDC;
                             a2: LPEXTLOGFONTW;
                             a3: UINT32;
                             a4: LPFMATCHW      ): UINT32;
CONST EnumNearestFonts = EnumNearestFontsA;

<*EXTERNAL SetFontMapperControls:WINAPI*>
PROCEDURE SetFontMapperControls (a1: LPFMCONTROLS): BOOL;

<*EXTERNAL GetFontMapperControls:WINAPI*>
PROCEDURE GetFontMapperControls (a1: LPFMCONTROLS; a2: UINT32): BOOL;

<*EXTERNAL ExtCreateFontIndirectA:WINAPI*>
PROCEDURE ExtCreateFontIndirectA (a1: LPEXTLOGFONTA): HFONT;

<*EXTERNAL ExtCreateFontIndirectW:WINAPI*>
PROCEDURE ExtCreateFontIndirectW (a1: LPEXTLOGFONTW): HFONT;

CONST ExtCreateFontIndirect = ExtCreateFontIndirectA;

TYPE
  LPKERNINGPAIR = UNTRACED REF KERNINGPAIR;
  KERNINGPAIR = RECORD
    wFirst     : UINT16;
    wSecond    : UINT16;
    iKernAmount: INT32;
  END;

<*EXTERNAL GetKerningPairs:WINAPI*>
PROCEDURE GetKerningPairs (a1: HDC; a2: UINT32; a3: LPKERNINGPAIR): UINT32;

<*EXTERNAL GetDCOrg:WINAPI*>
PROCEDURE GetDCOrg (a1: HDC): UINT32;

<*EXTERNAL FixBrushOrgEx:WINAPI*>
PROCEDURE FixBrushOrgEx (a1: HDC; a2: INT32; a3: INT32; a4: PPOINT): BOOL;

<*EXTERNAL UnrealizeObject:WINAPI*>
PROCEDURE UnrealizeObject (a1: HGDIOBJ): BOOL;

<*EXTERNAL GdiFlush:WINAPI*>
PROCEDURE GdiFlush (): BOOL;

<*EXTERNAL GdiSetBatchLimit:WINAPI*>
PROCEDURE GdiSetBatchLimit (a1: UINT32): UINT32;

<*EXTERNAL GdiGetBatchLimit:WINAPI*>
PROCEDURE GdiGetBatchLimit (): UINT32;


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
PROCEDURE wglUseFontBitmapsA (a1: HDC; a2: UINT32; a3: UINT32; a4: UINT32): BOOL;

<*EXTERNAL wglUseFontBitmapsW:WINAPI*>
PROCEDURE wglUseFontBitmapsW (a1: HDC; a2: UINT32; a3: UINT32; a4: UINT32): BOOL;

CONST wglUseFontBitmaps = wglUseFontBitmapsA;

<*EXTERNAL SwapBuffers:WINAPI*>
PROCEDURE SwapBuffers (a1: HDC): BOOL;

END WinGDI.
