(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(* Last modified on Thu Jan 27 13:53:21 PST 1994 by mcjones    *)

(* "Irs" provides direct access to the DECimage Character Recognition
   Software library.  You should read this interface in conjunction
   with DECimage Character Recognition Software for ULTRIX Risc
   Programmer's Reference Manual (Order Number AA-PNNN0A-TE).  See
   also /usr/include/irs/Irs{Def,Entry}.h.

   See "DCRS" for an interface to the same facilities that is more in
   the Modula-3 style. *)

INTERFACE Irs;

FROM Ctypes IMPORT char_star, char_star_star, long_int, unsigned_char,
                   unsigned_long, unsigned_long_int,
                   unsigned_long_star;

TYPE unsigned_byte = unsigned_char;

(* From IrsDef.h: *)

(* flag to bypass automated segmentation                                    *)
CONST M_BypassSegmentation = 8;
(* flag for adding region borders to PS output                              *)
CONST M_RegionBorder = 16;
(* flag for positioning words in PostScript output smoothly,                *)
(* instead of positioning in exact pixel coordinates.                       *)
CONST M_RelativeWordPosition = 32;
(* flag for including image regions in DDIF and PostScript output           *)
CONST M_IncludeImageRegions = 64;
(* flag for producing ASCII output in WYSIWYG format, instead of decolumnized *)
CONST M_FormatWYSIWYG =  128;
(* flag for forcing monospaced recognition                                  *)
CONST M_ForceMonospaceRecog = 256;
(* DCRS V1.0 versions of the flag constants retained for compatibility      *)
CONST K_RegionBorder = 1;
CONST K_BypassSegmentation = 1;
(* constants for region types....                                           *)
CONST K_TextRegion = 1;
CONST K_NonTextRegion = 2;
(* following groups of masks are used when word type is FontChange          *)
(* constant masks for font families                                         *)
(* only one of this group may be set.                                       *)
CONST M_NoFont = 1;
CONST M_Courier = 2;
CONST M_Times = 4;
CONST M_Helvetica = 8;
(* constant masks for font styles                                           *)
(* any combination of these masks may be set.                               *)
CONST M_Italic = 16;
CONST M_Bold = 32;
CONST M_Underline = 64;
(* constants for word types.....                                            *)
CONST K_PrimaryWord = 1;
CONST K_FontChange = 4;
CONST K_BeginNewLine = 8;
(* constants for language types......                                       *)
CONST K_IsoLatin1 = 0;
CONST K_English = 1;
CONST K_Danish = 2;
CONST K_Dutch = 3;
CONST K_CanadianFrench = 4;
CONST K_Finnish = 5;
CONST K_French = 6;
CONST K_German = 7;
CONST K_Italian = 8;
CONST K_Norwegian = 9;
CONST K_Numeric = 10;
CONST K_Spanish = 11;
CONST K_Swedish = 12;
CONST K_Icelandic = 13;
CONST K_Custom = 99;

(* constants to specify primary dictionaries to IrsCreateDictContext() routine *)
CONST K_EnglishDict = 128;
(* symbols for permissible resolutions                                      *)
CONST K_Res200 = 200;
CONST K_Res300 = 300;
CONST K_Res400 = 400;
(* structure for region lists                                               *)
TYPE RegionList = RECORD
    RegNum: long_int;
    BlockNum: long_int;
    RegType: unsigned_long_int;
    BlockURX: unsigned_long_int;
    BlockURY: unsigned_long_int;
    BlockLLX: unsigned_long_int;
    BlockLLY: unsigned_long_int;
    RegFontStyle: unsigned_long_int;
    RegFontSize: unsigned_long_int;
  END;
  RegionList_star = UNTRACED REF RegionList;
(* structure for word lists                                                 *)
TYPE WordList = RECORD
    RegNum: long_int;
    BlockNum: long_int;
    WordType: unsigned_long_int;
    WordSize: unsigned_long_int;
    FontInfo: unsigned_long_int;
    WordURX: unsigned_long_int;
    WordURY: unsigned_long_int;
    WordLLX: unsigned_long_int;
    WordLLY: unsigned_long_int;
    CharString: char_star;
  END;
  WordList_star = UNTRACED REF WordList;

(* From IrsEntry.h: *)

<* EXTERNAL IrsCreateDictContext *> PROCEDURE CreateDictContext(
    primary_dict: unsigned_long;
    user_dict_list: unsigned_long_star;
    flags: unsigned_long)
  : unsigned_long;

<* EXTERNAL IrsDeleteBuffer *> PROCEDURE DeleteBuffer(buffer: unsigned_long);

<* EXTERNAL IrsDeleteRegion *> PROCEDURE DeleteRegion(
    struct: unsigned_long;
    region_number: unsigned_long);

<* EXTERNAL IrsDeleteStruct *> PROCEDURE DeleteStruct(struct: unsigned_long);

<* EXTERNAL IrsExportASCII *> PROCEDURE ExportASCII(
    recognize_struct: unsigned_long;
    reject_char: unsigned_byte;
    flags: unsigned_long;
    ASCII_buffer: char_star_star;
    ASCII_buffer_size: unsigned_long_star);

<* EXTERNAL IrsExportDDIF *> PROCEDURE ExportDDIF(
    recognize_struct: unsigned_long;
    reject_char: unsigned_byte;
    flags: unsigned_long;
    filename: char_star);

<* EXTERNAL IrsExportPS *> PROCEDURE ExportPS(
    recognize_struct: unsigned_long;
    reject_char: unsigned_byte;
    flags: unsigned_long;
    PS_buffer: char_star_star;
    PS_buffer_size: unsigned_long_star);

<* EXTERNAL IrsGetRegionList *> PROCEDURE GetRegionList(
    struct: unsigned_long;
    region_list: UNTRACED REF (*VAR OUT*) RegionList_star);

<* EXTERNAL IrsGetWordList *> PROCEDURE GetWordList(
    recognize_struct: unsigned_long;
    reject_char: unsigned_byte;
    region: unsigned_long;
    word_list: UNTRACED REF (*VAR OUT*) WordList_star;
    );

<* EXTERNAL IrsRecognizeText *> PROCEDURE RecognizeText(
    segmentation_structure: unsigned_long;
    language: unsigned_byte;
    user_charset: unsigned_long_star;
    flags: unsigned_long): unsigned_long;

<* EXTERNAL IrsSegmentRegion *> PROCEDURE SegmentRegion(
    fid: unsigned_long;
    roi: unsigned_long;
    resolution: unsigned_long;
    flags: unsigned_long)
  : unsigned_long;

(* From IrsStatusCodes.h (as supplied to my by Henry Tumblin): *)

(* IrsStatusCodes.h *)
(********************************************************************************************************************************)
(* Created  8-DEC-1992 10:13:46 by VAX SDL V3.2-12     Source:  8-DEC-1992 10:06:39 USER3$:[GUEST.IIS]IRSSTATUSCODES.SDL;1 *)
(********************************************************************************************************************************)
 
(*** MODULE $IRSDEF ***)
(*                                                                          *)
(* This SDL File Generated by VAX-11 Message V04-00 on  8-DEC-1992 10:06:40.22 *)
(*                                                                          *)
(*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
(* COPYRIGHT ) DIGITAL EQUIPMENT CORPORATION, 1991 ALL RIGHTS RESERVED.       ! *)
(* UNPUBLISHED RIGHTS RESERVED UNDER THE COPYRIGHT LAWS OF THE UNITED STATES. ! *)
(* THE SOFTWARE CONTAINED ON THIS MEDIA IS PROPRIETARY AND EMBODIES THE       ! *)
(* CONFIDENTIAL TECHNOLOGY OF DIGITAL EQUIPMENT CORPORATION. POSSESSION, USE, ! *)
(* DUPLICATION OR DISSEMINATION OF THE SOFTWARE AND MEDIA IS AUTHORIZED ONLY  ! *)
(* PURSUANT TO A VALID WRITTEN LICENSE FROM DIGITAL EQUIPMENT CORPORATION.    ! *)
(*                                                                            ! *)
(* RESTRICTED RIGHTS LEGEND: USE, DUPLICATION, OR DISCLOSURE BY THE U.S.      ! *)
(* GOVERNMENT IS SUBJECT TO RESTRICTIONS AS SET FORTH IN SUBPARAGRAPH         ! *)
(* (C)(1)(II) OF DFARS 252.227-7013, OR IN FAR 52.227-19, AS APPLICABLE.      ! *)
(*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
CONST FACILITY = 2814;
(*+                                                                         *)
(* GENERAL SUCCESS CODES                                                    *)
(*-                                                                         *)
CONST X_NORMAL = 184451073;
CONST X_SUCCESS = 184451081;
(*+                                                                         *)
(* INFORMATIONAL CODES.                                                     *)
(*-                                                                         *)
CONST X_EXPDDIFINFO = 184451875;
CONST X_IMGWNOREG = 184451883;
CONST X_IMGTOOCMPLX = 184451891;
CONST X_FMTFAIL = 184451899;
(* NEW ERROR MESSAGES FOR V1.1                                              *)
CONST X_INVFLGSPC = 184451907;
CONST X_INVDCTSPC = 184451915;
(*                                                                          *)
(* FATAL CODES (LIB$STOP ONLY)                                              *)
(*                                                                          *)
CONST X_EXPDDIFFAIL = 184454276;
CONST X_INVALDFID = 184454284;
CONST X_INVARGCNT = 184454292;
CONST X_INVRESOL = 184454300;
CONST X_INVROI = 184454308;
CONST X_INVSTRTYP = 184454316;
CONST X_UNSCMPTYP = 184454324;
CONST X_UNSCSPORG = 184454332;
CONST X_UNSOPTION = 184454340;
CONST X_UNSSPCTYP = 184454348;
(* NEW ERROR MESSAGES FOR DCRS V1.1                                         *)
CONST X_BUFNOTSPC = 184454356;
CONST X_INVLANGID = 184454364;
CONST X_NOCHARSET = 184454372;
CONST X_NOREGLIST = 184454380;
CONST X_NOWORDLIST = 184454388;
CONST X_LICNOTREG = 184454396;
(* ERROR MESSAGES FOR THE DCRS WIDGET AND APPLICATION V1.1                  *)
CONST X_AILFAIL = 184454404;
CONST X_CLPBRDNOREG = 184454412;
CONST X_EMPRECSTR = 184454420;
CONST X_EMPSEGSTR = 184454428;
CONST X_INSVIRMEM = 184454436;
CONST X_INVARGVAL = 184454444;
CONST X_INVFILSPC = 184454452;
CONST X_INVIMGCOO = 184454460;
CONST X_INVINTLOG = 184454468;
CONST X_INVROTANG = 184454476;
CONST X_INVREJCHR = 184454484;
CONST X_INVROTVAL = 184454492;
CONST X_INVWIN = 184454500;
CONST X_NOACTUNDO = 184454508;
CONST X_NOFETMAIN = 184454516;
CONST X_NOFETWIDG = 184454524;
CONST X_NOFETLIT = 184454532;
CONST X_NOIMGFID = 184454540;
CONST X_NOOPNHIER = 184454548;
CONST X_NOREGCLAS = 184454556;
CONST X_OPNFILASC = 184454564;
CONST X_OPNFILDDIF = 184454572;
CONST X_OPNFILPS = 184454580;
CONST X_OPNFILSAV = 184454588;
CONST X_OPNFILRES = 184454596;
CONST X_REASETFIL = 184454604;
CONST X_VERSETFIL = 184454612;
CONST X_WRTPRTFIL = 184454620;
CONST X_WRTSETFIL = 184454628;

END Irs.
