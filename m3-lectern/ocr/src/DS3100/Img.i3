(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(* Last modified on Sun May 29 13:56:00 PDT 1994 by mcjones    *)

(* "Img" provides direct access to [some of] the DECimage Application
   Services Image Services Library.  You should read this interface in
   conjunction with DECimage Application Services for ULTRIX Image
   Services Library Programmer's Reference Manual (Order Number
   AA-NH13B-TE).  See also
   /usr/include/img/Img{Def,Entry,StatusCodes}.h.  *)

INTERFACE Img;

FROM Ctypes IMPORT char_star, long, long_int, unsigned_char,
                   unsigned_char_star, unsigned_long,
                   unsigned_long_int, unsigned_long_star;

TYPE unsigned_long_int_star = UNTRACED REF unsigned_long_int;

(* From ImgDef.h: *)

(*                                                                          *)
(* item codes for Image Data Unit aggregate                                 *)
(*                                                                          *)
CONST PrivateCodingAttr = 101122087;
CONST PixelsPerLine = 100794408;
CONST NumberOfLines = 100794409;
CONST CompressionType = 101187626;
CONST CompressionParams = 101122091;
CONST DataOffset = 100794412;
CONST PixelStride = 100794413;
CONST ScanlineStride = 100794414;
CONST BitOrder = 101187631;
CONST PixelOrder = 101187631;
CONST PlaneBitsPerPixel = 100794416;
CONST BitsPerPixel = 100794416;
CONST PlaneData = 1174601777;
CONST ByteUnit = 101187634;
CONST ByteOrder = 100794419;
CONST DataType = 101187636;
(*                                                                          *)
(* more componant space attributes (new ones)                               *)
(*                                                                          *)
CONST PixelGroupSize = 84410421;
CONST PixelGroupOrder = 84017206;
CONST QuantLevelsPerComp = 621412407;
(*                                                                          *)
(* Item codes for Special Processing items                                  *)
(*                                                                          *)
CONST DataPlaneBase = 922877952;
CONST PlaneDataBase = 922877952;
CONST TotalBitsPerPixel = 922877953;
(*                                                                          *)
(* Sequence spacer, to preserve itemcode values from shifting up after      *)
(* having moved an itemcode def that was here somewhere else.               *)
(*                                                                          *)
CONST PxlAspectRatio = 654639107;
(*                                                                          *)
(* NOTE:    This attribute number used to be assigned to Img_DataPlaneSize, *)
(*	    but a V2 bug returned size in bits.  It was renamed to          *)
(*	    provide support for this behavior (and to not break V2 applications) *)
(*                                                                          *)
(*	    Img_DataPlaneSize is supported below with a new attribute    *)
(*	    number.                                                         *)
(*                                                                          *)
CONST DataPlaneBitSize = 922877956;
CONST LutCnt = 922877957;
CONST LookupTablesCnt = 922877957;
CONST IduCnt = 922877958;
CONST BpcListCnt = 922877959;
CONST BPCListCnt = 922877959;
CONST UdpPrsnt = 922877960;
CONST Udp = 1728184329;
CONST CdpPrsnt = 922877962;
CONST Cdp = 1728184331;
CONST UserLabelCnt = 922877964;
CONST UserLabelLen = 922877965;
CONST QuantBitsPerComp = 922877966;
CONST TotalQuantBitsPerPixel = 922877967;
CONST PixelAlignment = 922877968;
CONST ScanlineAlignment = 922877969;
CONST VirtualArsize = 922877970;
CONST Dtype = 922877971;
CONST DType = 922877971;
CONST DataPlaneSize = 922877972;


(*                                                                          *)
(* Itemcodes for ROI definition item list(s)                                *)
(*                                                                          *)
CONST RoiRectangle = 151126016;

(*                                                                          *)
(* Compression Typecode definitions                                         *)
(*                                                                          *)
CONST K_CtypeMin = 0;
CONST K_PrivateCompression = 1;
CONST K_PcmCompression = 2;
CONST K_G31dCompression = 3;
CONST K_G32dCompression = 4;
CONST K_G42dCompression = 5;
CONST K_MonoCompression = 6;
CONST K_DctCompression = 7;
CONST K_CtypeMax = 8;

(*                                                                          *)
(* Compression flags                                                        *)
(*                                                                          *)
CONST M_AlignGroup3 = 1;

(*                                                                          *)
(* Frame Block Spectral Typecode definitions                                *)
(*                                                                          *)
CONST K_ClassPrivate = 1;
CONST K_ClassBitonal = 2;
CONST K_ClassGreyscale = 3;
CONST K_ClassMultispect = 4;
CONST K_ClassGrayscale = 3;

(*                                                                          *)
(*	User level ROI definition structures                                *)
(*                                                                          *)
CONST RoiK_RectLength = 16;
TYPE ROI_RECT = RECORD
    Ulx: long_int;            (* Upper left X coordinate          *)
    Uly: long_int;            (* Upper left Y coordinate          *)
    Pxls: unsigned_long_int;  (* Pixels per scanline              *)
    Scnlns: unsigned_long_int;(* Scanline count                   *)
  END;

(**                                                                         *)
(*	Item list structure for GET operations                              *)
(**                                                                         *)
TYPE
  GET_ITMLST = RECORD
    Code: unsigned_long_int;
    Length: unsigned_long_int;
    Buffer: char_star;
    Retlen: unsigned_long_int_star;
    Index: unsigned_long_int;
  END;
  GET_ITMLST_star = UNTRACED REF GET_ITMLST;

(**                                                                         *)
(*	Generic item list structure for new 3.0 functions                   *)
(**                                                                         *)
TYPE
  ITMLST = RECORD
    Code: unsigned_long_int;
    Length: unsigned_long_int;
    Buffer: char_star;
    Retlen: unsigned_long_int_star;
    Index: unsigned_long_int;
  END;
  ITMLST_star = UNTRACED REF ITMLST;

(*                                                                          *)
(* Literals used for stream and file operations.                            *)
(*                                                                          *)
CONST K_ModeImport = 1;
CONST K_ModeExport = 2;
CONST K_ModeTransfer = 3;
(*                                                                          *)
(* Masks used for general processing flags                                  *)
(*                                                                          *)
CONST M_Abort = 4;

(*                                                                          *)
(* Scanline stride alignment constants.                                     *)
(*                                                                          *)
CONST K_AlignBit = 1;
CONST K_AlignByte = 8;
CONST K_AlignWord = 16;
CONST K_AlignLongword = 32;

(*                                                                          *)
(* Flag for use with memory management                                      *)
(*                                                                          *)
CONST M_InitMem = 1;                (* bit 1                            *)

(*                                                                          *)
(* More Frame utils flags                                                   *)
(*                                                                          *)
CONST M_NoDataPlaneAlloc = 1024;
CONST M_AutoDeallocate = 2048;
CONST M_NoStandardize = 4096;
CONST M_NoStructureVerify = 8192;
CONST M_NoAttrVerify = 16384;
CONST M_NoDataPlaneVerify = 32768;
CONST M_NonstandardVerify = 65536;
CONST M_NoChf = 131072;
CONST M_InPlace = 262144;
CONST M_VerifyOn = 524288;

(*                                                                          *)
(* File I/O constants and masks                                             *)
(*                                                                          *)
CONST K_FtypeDDIF = 1;
CONST K_Cda = 1;
CONST K_Qio = 2;
CONST K_RmsBlk = 3;
CONST K_RmsBio = 3;
CONST K_UltrixIo = 4;
CONST M_Asynchronous = 32;


(* From ImgEntry.h, DAS Programmer's Reference Manual: *)

<* EXTERNAL ImgAllocateFrame *> PROCEDURE AllocateFrame(
    data_class: unsigned_long;
    itmlst: ITMLST_star;
    srcfid: unsigned_long;
    flags: unsigned_long): unsigned_long;

<* EXTERNAL ImgAllocDataPlane *> PROCEDURE AllocDataPlane(
    size: unsigned_long;
    flags: unsigned_long;
    fill_pattern: unsigned_char): unsigned_char_star;

<* EXTERNAL ImgAttachDataPlane *> PROCEDURE AttachDataPlane(
    fid: unsigned_long;
    data_plane: unsigned_char_star;
    index: unsigned_long): unsigned_long;

<* EXTERNAL ImgCloseFile *> PROCEDURE CloseFile(
    ctx: unsigned_long;
    flags: unsigned_long)
  : unsigned_long;

<* EXTERNAL ImgCreateRoiDef *> PROCEDURE CreateRoiDef(
    itmlst: ITMLST_star;
    flags: unsigned_long)
  : unsigned_long;

<* EXTERNAL ImgDeallocateFrame *> PROCEDURE DeallocateFrame(
    fid: unsigned_long);

<* EXTERNAL ImgDecompressFrame *> PROCEDURE DecompressFrame(
    srcfid: unsigned_long;
    flags: unsigned_long;
    comp_params: ITMLST_star)
  : unsigned_long;

<* EXTERNAL ImgDeleteRoiDef *> PROCEDURE DeleteRoiDef(roi_id: unsigned_long);

<* EXTERNAL ImgExportDataPlane *> PROCEDURE ExportDataPlane(
    fid: unsigned_long;
    plane_idx: unsigned_long;
    bufadr: char_star;
    buflen: unsigned_long;
    flags: unsigned_long;
    action: unsigned_long_star; (* really procedure pointer *)
    actionprm: long)
  : unsigned_long;

<* EXTERNAL ImgExportFrame *> PROCEDURE ExportFrame(
    fid: unsigned_long;
    ctx: unsigned_long;
    flags: unsigned_long): unsigned_long;

<* EXTERNAL ImgGet *> PROCEDURE Get(
    srcfid: unsigned_long;
    itemcode: unsigned_long;
    bufadr: unsigned_long_star;
    buflen: unsigned_long;
    retlen: unsigned_long_star;
    index: unsigned_long)
  : unsigned_long;

<* EXTERNAL ImgGetFrameAttributes *> PROCEDURE GetFrameAttributes(
    srcfid: unsigned_long;
    itmlst: ITMLST_star)
  : unsigned_long;

<* EXTERNAL ImgImportFrame *> PROCEDURE ImportFrame(
    ctx: unsigned_long;
    flags: unsigned_long)
  : unsigned_long;

<* EXTERNAL ImgOpenFile *> PROCEDURE OpenFile(
    mode: unsigned_long;
    file_type: unsigned_long;
    filename_len: unsigned_long;
    filename_str: char_star;
    itmlst: GET_ITMLST_star;
    flags: unsigned_long)
  : unsigned_long;

<* EXTERNAL ImgVerifyFrame *> PROCEDURE VerifyFrame(
    srcfid: unsigned_long;
    flags: unsigned_long);

END Img.
