(* Copyright (C) 1996, Critcal Mass, Inc.  All rights reserved.   *)
(*                                                                *)
(* derived from Microsoft's WINNLS.H by Bill Kalsow               *)

(**************************************************************************
*                                                                         *
* winnls.h -- NLS procedure declarations, constant definitions and macros *
*                                                                         *
**************************************************************************)

INTERFACE WinNLS;

FROM WinDef  IMPORT BOOL, INT32, UINT32, UINT8, PUINT16, PBOOL;
FROM WinNT   IMPORT PSTR, PCSTR, PWSTR, PCWSTR, LCID, LANGID;
FROM WinBase IMPORT PSYSTEMTIME;

(***************************************************************************
* Constants
*
* Define all constants for the NLS component here.
****************************************************************************)


(*
 *  String Length Maximums.
 *)
CONST
  MAX_LEADBYTES   = 12;             (* 5 ranges, 2 bytes ea., 0 term. *)
  MAX_DEFAULTCHAR = 2;              (* single or double byte *)


(*
 *  MBCS and Unicode Translation Flags.
 *)
CONST
  MB_PRECOMPOSED       = 16_00000001;     (* use precomposed chars *)
  MB_COMPOSITE         = 16_00000002;     (* use composite chars *)
  MB_USEGLYPHCHARS     = 16_00000004;     (* use glyph chars, not ctrl chars *)
  MB_ERR_INVALID_CHARS = 16_00000008;     (* error for invalid chars *)

  WC_DEFAULTCHECK      = 16_00000100;     (* check for default char *)
  WC_COMPOSITECHECK    = 16_00000200;     (* convert composite to precomposed *)
  WC_DISCARDNS         = 16_00000010;     (* discard non-spacing chars *)
  WC_SEPCHARS          = 16_00000020;     (* generate separate chars *)
  WC_DEFAULTCHAR       = 16_00000040;     (* replace w/ default char *)



(*
 *  Character Type Flags.
 *)
CONST
  CT_CTYPE1 = 16_00000001;     (* ctype 1 information *)
  CT_CTYPE2 = 16_00000002;     (* ctype 2 information *)
  CT_CTYPE3 = 16_00000004;     (* ctype 3 information *)

(*
 *  CType 1 Flag Bits.
 *)
CONST
  C1_UPPER  = 16_0001;         (* upper case *)
  C1_LOWER  = 16_0002;         (* lower case *)
  C1_DIGIT  = 16_0004;         (* decimal digits *)
  C1_SPACE  = 16_0008;         (* spacing characters *)
  C1_PUNCT  = 16_0010;         (* punctuation characters *)
  C1_CNTRL  = 16_0020;         (* control characters *)
  C1_BLANK  = 16_0040;         (* blank characters *)
  C1_XDIGIT = 16_0080;         (* other digits *)
  C1_ALPHA  = 16_0100;         (* any linguistic character *)

(*
 *  CType 2 Flag Bits.
 *)
CONST
  C2_LEFTTORIGHT      = 16_0001;         (* left to right *)
  C2_RIGHTTOLEFT      = 16_0002;         (* right to left *)

  C2_EUROPENUMBER     = 16_0003;         (* European number, digit *)
  C2_EUROPESEPARATOR  = 16_0004;         (* European numeric separator *)
  C2_EUROPETERMINATOR = 16_0005;         (* European numeric terminator *)
  C2_ARABICNUMBER     = 16_0006;         (* Arabic number *)
  C2_COMMONSEPARATOR  = 16_0007;         (* common numeric separator *)

  C2_BLOCKSEPARATOR   = 16_0008;         (* block separator *)
  C2_SEGMENTSEPARATOR = 16_0009;         (* segment separator *)
  C2_WHITESPACE       = 16_000A;         (* white space *)
  C2_OTHERNEUTRAL     = 16_000B;         (* other neutrals *)

  C2_NOTAPPLICABLE    = 16_0000;         (* no implicit directionality *)

(*
 *  CType 3 Flag Bits.
 *)
CONST
  C3_NONSPACING    = 16_0001;         (* nonspacing character *)
  C3_DIACRITIC     = 16_0002;         (* diacritic mark *)
  C3_VOWELMARK     = 16_0004;         (* vowel mark *)
  C3_SYMBOL        = 16_0008;         (* symbols *)

  C3_KATAKANA      = 16_0010;         (* katakana character *)
  C3_HIRAGANA      = 16_0020;         (* hiragana character *)
  C3_HALFWIDTH     = 16_0040;         (* half width character *)
  C3_FULLWIDTH     = 16_0080;         (* full width character *)
  C3_IDEOGRAPH     = 16_0100;         (* ideographic character *)
  C3_KASHIDA       = 16_0200;         (* Arabic kashida character *)
  C3_LEXICAL       = 16_0400;         (* lexical character *)

  C3_ALPHA         = 16_8000;         (* any linguistic char (C1_ALPHA) *)

  C3_NOTAPPLICABLE = 16_0000;         (* ctype 3 is not applicable *)


(*
 *  String Flags.
 *)
CONST
  NORM_IGNORECASE     = 16_00000001;  (* ignore case *)
  NORM_IGNORENONSPACE = 16_00000002;  (* ignore nonspacing chars *)
  NORM_IGNORESYMBOLS  = 16_00000004;  (* ignore symbols *)

  NORM_IGNOREKANATYPE = 16_00010000;  (* ignore kanatype *)
  NORM_IGNOREWIDTH    = 16_00020000;  (* ignore width *)


(*
 *  Locale Independent Mapping Flags.
 *)
CONST
  MAP_FOLDCZONE   = 16_00000010;  (* fold compatibility zone chars *)
  MAP_PRECOMPOSED = 16_00000020;  (* convert to precomposed chars *)
  MAP_COMPOSITE   = 16_00000040;  (* convert to composite chars *)
  MAP_FOLDDIGITS  = 16_00000080;  (* all digits to ASCII 0-9 *)


(*
 *  Locale Dependent Mapping Flags.
 *)
CONST
  LCMAP_LOWERCASE = 16_00000100;  (* lower case letters *)
  LCMAP_UPPERCASE = 16_00000200;  (* upper case letters *)
  LCMAP_SORTKEY   = 16_00000400;  (* WC sort key (normalize) *)
  LCMAP_BYTEREV   = 16_00000800;  (* byte reversal *)

  LCMAP_HIRAGANA  = 16_00100000;  (* map katakana to hiragana *)
  LCMAP_KATAKANA  = 16_00200000;  (* map hiragana to katakana *)
  LCMAP_HALFWIDTH = 16_00400000;  (* map double byte to single byte *)
  LCMAP_FULLWIDTH = 16_00800000;  (* map single byte to double byte *)



(*
 *  Locale Enumeration Flags.
 *)
CONST
  LCID_INSTALLED = 16_00000001;  (* installed locale ids *)
  LCID_SUPPORTED = 16_00000002;  (* supported locale ids *)

(*
 *  Code Page Enumeration Flags.
 *)
CONST
  CP_INSTALLED = 16_00000001;  (* installed code page ids *)
  CP_SUPPORTED = 16_00000002;  (* supported code page ids *)


(*
 *  Sorting Flags.
 *
 *    UNT16 Sort:   culturally correct sort
 *                  hyphen and apostrophe are special cased
 *                  example: "coop" and "co-op" will sort together in a list
 *
 *                        co_op     <-------  underscore (symbol)
 *                        coat
 *                        comb
 *                        coop
 *                        co-op     <-------  hyphen (punctuation)
 *                        cork
 *                        went
 *                        were
 *                        we're     <-------  apostrophe (punctuation)
 *
 *
 *    STRING Sort:  hyphen and apostrophe will sort with all other symbols
 *
 *                        co-op     <-------  hyphen (punctuation)
 *                        co_op     <-------  underscore (symbol)
 *                        coat
 *                        comb
 *                        coop
 *                        cork
 *                        we're     <-------  apostrophe (punctuation)
 *                        went
 *                        were
 *)
CONST
  SORT_STRINGSORT = 16_00001000;  (* use string sort method *)


(*
 *  Code Page Default Values.
 *)
CONST
  CP_ACP   = 0;              (* default to ANSI code page *)
  CP_OEMCP = 1;              (* default to OEM  code page *)
  CP_MACCP = 2;              (* default to MAC  code page *)

  CP_UTF8 = 65001;           (* GLib native codepage. *)


(*
 *  Country Codes.
 *)
CONST
  CTRY_DEFAULT        = 0;
  CTRY_AUSTRALIA      = 61;      (* Australia *)
  CTRY_AUSTRIA        = 43;      (* Austria *)
  CTRY_BELGIUM        = 32;      (* Belgium *)
  CTRY_BRAZIL         = 55;      (* Brazil *)
  CTRY_BULGARIA       = 359;     (* Bulgaria *)
  CTRY_CANADA         = 2;       (* Canada *)
  CTRY_CROATIA        = 385;     (* Croatia *)
  CTRY_CZECH          = 42;      (* Czech Republic *)
  CTRY_DENMARK        = 45;      (* Denmark *)
  CTRY_FINLAND        = 358;     (* Finland *)
  CTRY_FRANCE         = 33;      (* France *)
  CTRY_GERMANY        = 49;      (* Germany *)
  CTRY_GREECE         = 30;      (* Greece *)
  CTRY_HONG_KONG      = 852;     (* Hong Kong *)
  CTRY_HUNGARY        = 36;      (* Hungary *)
  CTRY_ICELAND        = 354;     (* Iceland *)
  CTRY_IRELAND        = 353;     (* Ireland *)
  CTRY_ITALY          = 39;      (* Italy *)
  CTRY_JAPAN          = 81;      (* Japan *)
  CTRY_MEXICO         = 52;      (* Mexico *)
  CTRY_NETHERLANDS    = 31;      (* Netherlands *)
  CTRY_NEW_ZEALAND    = 64;      (* New Zealand *)
  CTRY_NORWAY         = 47;      (* Norway *)
  CTRY_POLAND         = 48;      (* Poland *)
  CTRY_PORTUGAL       = 351;     (* Portugal *)
  CTRY_PRCHINA        = 86;      (* Peoples' Republic of China *)
  CTRY_ROMANIA        = 40;      (* Romania *)
  CTRY_RUSSIA         = 7;       (* Russia *)
  CTRY_SINGAPORE      = 65;      (* Singapore *)
  CTRY_SLOVAK         = 42;      (* Slovak Republic *)
  CTRY_SLOVENIA       = 386;     (* Slovenia *)
  CTRY_SOUTH_KOREA    = 82;      (* South Korea *)
  CTRY_SPAIN          = 34;      (* Spain *)
  CTRY_SWEDEN         = 46;      (* Sweden *)
  CTRY_SWITZERLAND    = 41;      (* Switzerland *)
  CTRY_TAIWAN         = 886;     (* Taiwan *)
  CTRY_TURKEY         = 90;      (* Turkey *)
  CTRY_UNITED_KINGDOM = 44;      (* United Kingdom *)
  CTRY_UNITED_STATES  = 1;       (* United States *)


(*
 *  Locale Types.
 *
 *  These types are used for the GetLocaleInfoW NLS API routine.
 *
 *  LOCALE_NOUSEROVERRIDE is also used in GetTimeFormatW and GetDateFormatW.
 *
 *  LOCALE_USE_CP_ACP is used in many of the A (Ansi) apis that need to do 
 *  string translation.
 *)
CONST
  LOCALE_NOUSEROVERRIDE       = 16_80000000;   (* do not use user overrides *)
  LOCALE_USE_CP_ACP           = 16_40000000;   (* use the system ACP *)

  LOCALE_ILANGUAGE            = 16_00000001;   (* language id *)
  LOCALE_SLANGUAGE            = 16_00000002;   (* localized name of language *)
  LOCALE_SENGLANGUAGE         = 16_00001001;   (* English name of language *)
  LOCALE_SABBREVLANGNAME      = 16_00000003;   (* abbreviated language name *)
  LOCALE_SNATIVELANGNAME      = 16_00000004;   (* native name of language *)
  LOCALE_ICOUNTRY             = 16_00000005;   (* country code *)
  LOCALE_SCOUNTRY             = 16_00000006;   (* localized name of country *)
  LOCALE_SENGCOUNTRY          = 16_00001002;   (* English name of country *)
  LOCALE_SABBREVCTRYNAME      = 16_00000007;   (* abbreviated country name *)
  LOCALE_SNATIVECTRYNAME      = 16_00000008;   (* native name of country *)
  LOCALE_IDEFAULTLANGUAGE     = 16_00000009;   (* default language id *)
  LOCALE_IDEFAULTCOUNTRY      = 16_0000000A;   (* default country code *)
  LOCALE_IDEFAULTCODEPAGE     = 16_0000000B;   (* default oem code page *)
  LOCALE_IDEFAULTANSICODEPAGE = 16_00001004;   (* default ansi code page *)

  LOCALE_SLIST                = 16_0000000C;   (* list item separator *)
  LOCALE_IMEASURE             = 16_0000000D;   (* 0 = metric, 1 = US *)

  LOCALE_SDECIMAL             = 16_0000000E;   (* decimal separator *)
  LOCALE_STHOUSAND            = 16_0000000F;   (* thousand separator *)
  LOCALE_SGROUPING            = 16_00000010;   (* digit grouping *)
  LOCALE_IDIGITS              = 16_00000011;   (* number of fractional digits *)
  LOCALE_ILZERO               = 16_00000012;   (* leading zeros for decimal *)
  LOCALE_INEGNUMBER           = 16_00001010;   (* negative number mode *)
  LOCALE_SNATIVEDIGITS        = 16_00000013;   (* native ascii 0-9 *)

  LOCALE_SCURRENCY            = 16_00000014;   (* local monetary symbol *)
  LOCALE_SINTLSYMBOL          = 16_00000015;   (* intl monetary symbol *)
  LOCALE_SMONDECIMALSEP       = 16_00000016;   (* monetary decimal separator *)
  LOCALE_SMONTHOUSANDSEP      = 16_00000017;   (* monetary thousand separator *)
  LOCALE_SMONGROUPING         = 16_00000018;   (* monetary grouping *)
  LOCALE_ICURRDIGITS          = 16_00000019;   (* # local monetary digits *)
  LOCALE_IINTLCURRDIGITS      = 16_0000001A;   (* # intl monetary digits *)
  LOCALE_ICURRENCY            = 16_0000001B;   (* positive currency mode *)
  LOCALE_INEGCURR             = 16_0000001C;   (* negative currency mode *)

  LOCALE_SDATE                = 16_0000001D;   (* date separator *)
  LOCALE_STIME                = 16_0000001E;   (* time separator *)
  LOCALE_SSHORTDATE           = 16_0000001F;   (* short date format string *)
  LOCALE_SLONGDATE            = 16_00000020;   (* long date format string *)
  LOCALE_STIMEFORMAT          = 16_00001003;   (* time format string *)
  LOCALE_IDATE                = 16_00000021;   (* short date format ordering *)
  LOCALE_ILDATE               = 16_00000022;   (* long date format ordering *)
  LOCALE_ITIME                = 16_00000023;   (* time format specifier *)
  LOCALE_ITIMEMARKPOSN        = 16_00001005;   (* time marker position *)
  LOCALE_ICENTURY             = 16_00000024;   (* century format specifier (short date) *)
  LOCALE_ITLZERO              = 16_00000025;   (* leading zeros in time field *)
  LOCALE_IDAYLZERO            = 16_00000026;   (* leading zeros in day field (short date) *)
  LOCALE_IMONLZERO            = 16_00000027;   (* leading zeros in month field (short date) *)
  LOCALE_S1159                = 16_00000028;   (* AM designator *)
  LOCALE_S2359                = 16_00000029;   (* PM designator *)

  LOCALE_ICALENDARTYPE        = 16_00001009;   (* type of calendar specifier *)
  LOCALE_IOPTIONALCALENDAR    = 16_0000100B;   (* additional calendar types specifier *)
  LOCALE_IFIRSTDAYOFWEEK      = 16_0000100C;   (* first day of week specifier *)
  LOCALE_IFIRSTWEEKOFYEAR     = 16_0000100D;   (* first week of year specifier *)

  LOCALE_SDAYNAME1            = 16_0000002A;   (* long name for Monday *)
  LOCALE_SDAYNAME2            = 16_0000002B;   (* long name for Tuesday *)
  LOCALE_SDAYNAME3            = 16_0000002C;   (* long name for Wednesday *)
  LOCALE_SDAYNAME4            = 16_0000002D;   (* long name for Thursday *)
  LOCALE_SDAYNAME5            = 16_0000002E;   (* long name for Friday *)
  LOCALE_SDAYNAME6            = 16_0000002F;   (* long name for Saturday *)
  LOCALE_SDAYNAME7            = 16_00000030;   (* long name for Sunday *)
  LOCALE_SABBREVDAYNAME1      = 16_00000031;   (* abbreviated name for Monday *)
  LOCALE_SABBREVDAYNAME2      = 16_00000032;   (* abbreviated name for Tuesday *)
  LOCALE_SABBREVDAYNAME3      = 16_00000033;   (* abbreviated name for Wednesday *)
  LOCALE_SABBREVDAYNAME4      = 16_00000034;   (* abbreviated name for Thursday *)
  LOCALE_SABBREVDAYNAME5      = 16_00000035;   (* abbreviated name for Friday *)
  LOCALE_SABBREVDAYNAME6      = 16_00000036;   (* abbreviated name for Saturday *)
  LOCALE_SABBREVDAYNAME7      = 16_00000037;   (* abbreviated name for Sunday *)
  LOCALE_SMONTHNAME1          = 16_00000038;   (* long name for January *)
  LOCALE_SMONTHNAME2          = 16_00000039;   (* long name for February *)
  LOCALE_SMONTHNAME3          = 16_0000003A;   (* long name for March *)
  LOCALE_SMONTHNAME4          = 16_0000003B;   (* long name for April *)
  LOCALE_SMONTHNAME5          = 16_0000003C;   (* long name for May *)
  LOCALE_SMONTHNAME6          = 16_0000003D;   (* long name for June *)
  LOCALE_SMONTHNAME7          = 16_0000003E;   (* long name for July *)
  LOCALE_SMONTHNAME8          = 16_0000003F;   (* long name for August *)
  LOCALE_SMONTHNAME9          = 16_00000040;   (* long name for September *)
  LOCALE_SMONTHNAME10         = 16_00000041;   (* long name for October *)
  LOCALE_SMONTHNAME11         = 16_00000042;   (* long name for November *)
  LOCALE_SMONTHNAME12         = 16_00000043;   (* long name for December *)
  LOCALE_SMONTHNAME13         = 16_0000100E;   (* long name for 13th month (if exists) *)
  LOCALE_SABBREVMONTHNAME1    = 16_00000044;   (* abbreviated name for January *)
  LOCALE_SABBREVMONTHNAME2    = 16_00000045;   (* abbreviated name for February *)
  LOCALE_SABBREVMONTHNAME3    = 16_00000046;   (* abbreviated name for March *)
  LOCALE_SABBREVMONTHNAME4    = 16_00000047;   (* abbreviated name for April *)
  LOCALE_SABBREVMONTHNAME5    = 16_00000048;   (* abbreviated name for May *)
  LOCALE_SABBREVMONTHNAME6    = 16_00000049;   (* abbreviated name for June *)
  LOCALE_SABBREVMONTHNAME7    = 16_0000004A;   (* abbreviated name for July *)
  LOCALE_SABBREVMONTHNAME8    = 16_0000004B;   (* abbreviated name for August *)
  LOCALE_SABBREVMONTHNAME9    = 16_0000004C;   (* abbreviated name for September *)
  LOCALE_SABBREVMONTHNAME10   = 16_0000004D;   (* abbreviated name for October *)
  LOCALE_SABBREVMONTHNAME11   = 16_0000004E;   (* abbreviated name for November *)
  LOCALE_SABBREVMONTHNAME12   = 16_0000004F;   (* abbreviated name for December *)
  LOCALE_SABBREVMONTHNAME13   = 16_0000100F;   (* abbreviated name for 13th month (if exists) *)

  LOCALE_SPOSITIVESIGN        = 16_00000050;   (* positive sign *)
  LOCALE_SNEGATIVESIGN        = 16_00000051;   (* negative sign *)
  LOCALE_IPOSSIGNPOSN         = 16_00000052;   (* positive sign position *)
  LOCALE_INEGSIGNPOSN         = 16_00000053;   (* negative sign position *)
  LOCALE_IPOSSYMPRECEDES      = 16_00000054;   (* mon sym precedes pos amt *)
  LOCALE_IPOSSEPBYSPACE       = 16_00000055;   (* mon sym sep by space from pos amt *)
  LOCALE_INEGSYMPRECEDES      = 16_00000056;   (* mon sym precedes neg amt *)
  LOCALE_INEGSEPBYSPACE       = 16_00000057;   (* mon sym sep by space from neg amt *)

  LOCALE_FONTSIGNATURE        = 16_00000058;   (* font signature *)

(*
 *  Time Flags for GetTimeFormatW.
 *)
CONST
  TIME_NOMINUTESORSECONDS    = 16_00000001;  (* do not use minutes or seconds *)
  TIME_NOSECONDS             = 16_00000002;  (* do not use seconds *)
  TIME_NOTIMEMARKER          = 16_00000004;  (* do not use time marker *)
  TIME_FORCE24HOURFORMAT     = 16_00000008;  (* always use 24 hour format *)


(*
 *  Date Flags for GetDateFormatW.
 *)
CONST
  DATE_SHORTDATE             = 16_00000001;  (* use short date picture *)
  DATE_LONGDATE              = 16_00000002;  (* use long date picture *)
  DATE_USE_ALT_CALENDAR      = 16_00000004;  (* use alternate calendar (if any) *)


(*
 *  Calendar Types.
 *
 *  These types are used for the GetALTCalendarInfoW NLS API routine.
 *)
CONST
  CAL_ICALINTVALUE           = 16_00000001;   (* calendar type *)
  CAL_SCALNAME               = 16_00000002;   (* native name of calendar *)
  CAL_IYEAROFFSETRANGE       = 16_00000003;   (* starting years of eras *)
  CAL_SERASTRING             = 16_00000004;   (* era name for IYearOffsetRanges *)
  CAL_SSHORTDATE             = 16_00000005;   (* short date format string *)
  CAL_SLONGDATE              = 16_00000006;   (* long date format string *)
  CAL_SDAYNAME1              = 16_00000007;   (* native name for Monday *)
  CAL_SDAYNAME2              = 16_00000008;   (* native name for Tuesday *)
  CAL_SDAYNAME3              = 16_00000009;   (* native name for Wednesday *)
  CAL_SDAYNAME4              = 16_0000000a;   (* native name for Thursday *)
  CAL_SDAYNAME5              = 16_0000000b;   (* native name for Friday *)
  CAL_SDAYNAME6              = 16_0000000c;   (* native name for Saturday *)
  CAL_SDAYNAME7              = 16_0000000d;   (* native name for Sunday *)
  CAL_SABBREVDAYNAME1        = 16_0000000e;   (* abbreviated name for Monday *)
  CAL_SABBREVDAYNAME2        = 16_0000000f;   (* abbreviated name for Tuesday *)
  CAL_SABBREVDAYNAME3        = 16_00000010;   (* abbreviated name for Wednesday *)
  CAL_SABBREVDAYNAME4        = 16_00000011;   (* abbreviated name for Thursday *)
  CAL_SABBREVDAYNAME5        = 16_00000012;   (* abbreviated name for Friday *)
  CAL_SABBREVDAYNAME6        = 16_00000013;   (* abbreviated name for Saturday *)
  CAL_SABBREVDAYNAME7        = 16_00000014;   (* abbreviated name for Sunday *)
  CAL_SMONTHNAME1            = 16_00000015;   (* native name for January *)
  CAL_SMONTHNAME2            = 16_00000016;   (* native name for February *)
  CAL_SMONTHNAME3            = 16_00000017;   (* native name for March *)
  CAL_SMONTHNAME4            = 16_00000018;   (* native name for April *)
  CAL_SMONTHNAME5            = 16_00000019;   (* native name for May *)
  CAL_SMONTHNAME6            = 16_0000001a;   (* native name for June *)
  CAL_SMONTHNAME7            = 16_0000001b;   (* native name for July *)
  CAL_SMONTHNAME8            = 16_0000001c;   (* native name for August *)
  CAL_SMONTHNAME9            = 16_0000001d;   (* native name for September *)
  CAL_SMONTHNAME10           = 16_0000001e;   (* native name for October *)
  CAL_SMONTHNAME11           = 16_0000001f;   (* native name for November *)
  CAL_SMONTHNAME12           = 16_00000020;   (* native name for December *)
  CAL_SMONTHNAME13           = 16_00000021;   (* native name for 13th month (if any) *)
  CAL_SABBREVMONTHNAME1      = 16_00000022;   (* abbreviated name for January *)
  CAL_SABBREVMONTHNAME2      = 16_00000023;   (* abbreviated name for February *)
  CAL_SABBREVMONTHNAME3      = 16_00000024;   (* abbreviated name for March *)
  CAL_SABBREVMONTHNAME4      = 16_00000025;   (* abbreviated name for April *)
  CAL_SABBREVMONTHNAME5      = 16_00000026;   (* abbreviated name for May *)
  CAL_SABBREVMONTHNAME6      = 16_00000027;   (* abbreviated name for June *)
  CAL_SABBREVMONTHNAME7      = 16_00000028;   (* abbreviated name for July *)
  CAL_SABBREVMONTHNAME8      = 16_00000029;   (* abbreviated name for August *)
  CAL_SABBREVMONTHNAME9      = 16_0000002a;   (* abbreviated name for September *)
  CAL_SABBREVMONTHNAME10     = 16_0000002b;   (* abbreviated name for October *)
  CAL_SABBREVMONTHNAME11     = 16_0000002c;   (* abbreviated name for November *)
  CAL_SABBREVMONTHNAME12     = 16_0000002d;   (* abbreviated name for December *)
  CAL_SABBREVMONTHNAME13     = 16_0000002e;   (* abbreviated name for 13th month (if any) *)


(*
 *  Calendar Enumeration Value.
 *)
CONST
  ENUM_ALL_CALENDARS = 16_ffffffff;   (* enumerate all calendars *)


(*
 *  Calendar ID Values.
 *)
CONST
  CAL_GREGORIAN    = 1;         (* Gregorian (localized) calendar *)
  CAL_GREGORIAN_US = 2;         (* Gregorian (U.S.) calendar *)
  CAL_JAPAN        = 3;         (* Japanese Emperor Era calendar *)
  CAL_TAIWAN       = 4;         (* Republic of China Era calendar *)
  CAL_KOREA        = 5;         (* Korean Tangun Era calendar *)



(****************************************************************************
* Typedefs
*
* Define all types for the NLS component here.
****************************************************************************)

TYPE
  LCTYPE  = UINT32;  (*  Locale type constant. *)
  CALTYPE = UINT32;  (* Calendar type constant. *)
  CALID   = UINT32;  (*  Calendar ID.  *)

(*
 *  CP Info.
 *)
TYPE
  LPCPINFO = UNTRACED REF CPINFO;
  CPINFO = RECORD
    MaxCharSize : UINT32;  (* max length (bytes) of a char *)
    DefaultChar : ARRAY [0..MAX_DEFAULTCHAR-1] OF UINT8;   (* default character *)
    LeadByte    : ARRAY [0..MAX_LEADBYTES-1] OF UINT8;     (* lead byte ranges *)
  END;


(*
 *  Number format.
 *)
TYPE
  LPNUMBERFMTA = UNTRACED REF NUMBERFMTA;
  NUMBERFMTA = RECORD
    NumDigits     : UINT32;    (* number of decimal digits *)
    LeadingZero   : UINT32;    (* if leading zero in decimal fields *)
    Grouping      : UINT32;    (* group size left of decimal *)
    lpDecimalSep  : PSTR;   (* ptr to decimal separator string *)
    lpThousandSep : PSTR;   (* ptr to thousand separator string *)
    NegativeOrder : UINT32;    (* negative number ordering *)
  END;

  LPNUMBERFMTW = UNTRACED REF NUMBERFMTW;
  NUMBERFMTW = RECORD
    NumDigits     : UINT32;    (* number of decimal digits *)
    LeadingZero   : UINT32;    (* if leading zero in decimal fields *)
    Grouping      : UINT32;    (* group size left of decimal *)
    lpDecimalSep  : PWSTR;  (* ptr to decimal separator string *)
    lpThousandSep : PWSTR;  (* ptr to thousand separator string *)
    NegativeOrder : UINT32;    (* negative number ordering *)
  END;

  NUMBERFMT   = NUMBERFMTA;
  LPNUMBERFMT = LPNUMBERFMTA;
  
(*
 *  Currency format.
 *)
TYPE
  LPCURRENCYFMTA = UNTRACED REF CURRENCYFMTA;
  CURRENCYFMTA = RECORD
    NumDigits        : UINT32;    (* number of decimal digits *)
    LeadingZero      : UINT32;    (* if leading zero in decimal fields *)
    Grouping         : UINT32;    (* group size left of decimal *)
    lpDecimalSep     : PSTR;   (* ptr to decimal separator string *)
    lpThousandSep    : PSTR;   (* ptr to thousand separator string *)
    NegativeOrder    : UINT32;    (* negative currency ordering *)
    PositiveOrder    : UINT32;    (* positive currency ordering *)
    lpCurrencySymbol : PSTR;   (* ptr to currency symbol string *)
  END;

  LPCURRENCYFMTW = UNTRACED REF CURRENCYFMTW;
  CURRENCYFMTW = RECORD
    NumDigits        : UINT32;    (* number of decimal digits *)
    LeadingZero      : UINT32;    (* if leading zero in decimal fields *)
    Grouping         : UINT32;    (* group size left of decimal *)
    lpDecimalSep     : PWSTR;  (* ptr to decimal separator string *)
    lpThousandSep    : PWSTR;  (* ptr to thousand separator string *)
    NegativeOrder    : UINT32;    (* negative currency ordering *)
    PositiveOrder    : UINT32;    (* positive currency ordering *)
    lpCurrencySymbol : PWSTR;  (* ptr to currency symbol string *)
  END;

  CURRENCYFMT   = CURRENCYFMTA;
  LPCURRENCYFMT = LPCURRENCYFMTA;

(*
 *  Enumeration function constants.
 *)
TYPE
  LOCALE_ENUMPROCA   = <*CALLBACK*> PROCEDURE (str: PSTR): BOOL;
  CODEPAGE_ENUMPROCA = <*CALLBACK*> PROCEDURE (str: PSTR): BOOL;
  DATEFMT_ENUMPROCA  = <*CALLBACK*> PROCEDURE (str: PSTR): BOOL;
  TIMEFMT_ENUMPROCA  = <*CALLBACK*> PROCEDURE (str: PSTR): BOOL;
  CALINFO_ENUMPROCA  = <*CALLBACK*> PROCEDURE (str: PSTR): BOOL;

  LOCALE_ENUMPROCW   = <*CALLBACK*> PROCEDURE (str: PWSTR): BOOL;
  CODEPAGE_ENUMPROCW = <*CALLBACK*> PROCEDURE (str: PWSTR): BOOL;
  DATEFMT_ENUMPROCW  = <*CALLBACK*> PROCEDURE (str: PWSTR): BOOL;
  TIMEFMT_ENUMPROCW  = <*CALLBACK*> PROCEDURE (str: PWSTR): BOOL;
  CALINFO_ENUMPROCW  = <*CALLBACK*> PROCEDURE (str: PWSTR): BOOL;

  LOCALE_ENUMPROC    = LOCALE_ENUMPROCA;
  CODEPAGE_ENUMPROC  = CODEPAGE_ENUMPROCA;
  DATEFMT_ENUMPROC   = DATEFMT_ENUMPROCA;
  TIMEFMT_ENUMPROC   = TIMEFMT_ENUMPROCA;
  CALINFO_ENUMPROC   = CALINFO_ENUMPROCA;


(****************************************************************************
* Macros
*
* Define all macros for the NLS component here.
****************************************************************************)



(****************************************************************************
* Function Prototypes
*
* Only prototypes for the NLS APIs should go here.
****************************************************************************)

(*
 *  Code Page Dependent APIs.
 *)

<*EXTERNAL IsValidCodePage:WINAPI*>
PROCEDURE IsValidCodePage (CodePage: UINT32): BOOL;

<*EXTERNAL GetACP:WINAPI*>
PROCEDURE GetACP (): UINT32;

<*EXTERNAL GetOEMCP:WINAPI*>
PROCEDURE GetOEMCP (): UINT32;

<*EXTERNAL GetCPInfo:WINAPI*>
PROCEDURE GetCPInfo (CodePage: UINT32;  lpCPInfo: LPCPINFO): BOOL;

<*EXTERNAL IsDBCSLeadByte:WINAPI*>
PROCEDURE IsDBCSLeadByte (TestChar: UINT8): BOOL;

<*EXTERNAL IsDBCSLeadByteEx:WINAPI*>
PROCEDURE IsDBCSLeadByteEx (CodePage: UINT32;  TestChar: UINT8): BOOL;

<*EXTERNAL MultiByteToWideChar:WINAPI*>
PROCEDURE MultiByteToWideChar (CodePage       : UINT32;
                               dwFlags        : UINT32;
                               lpMultiByteStr : PCSTR;
                               cchMultiByte   : INT32;
                               lpWideCharStr  : PWSTR;
                               cchWideChar    : INT32): INT32;

<*EXTERNAL WideCharToMultiByte:WINAPI*>
PROCEDURE WideCharToMultiByte (CodePage          : UINT32;
                               dwFlags           : UINT32;
                               lpWideCharStr     : PCWSTR;
                               cchWideChar       : INT32;
                               lpMultiByteStr    : PSTR;
                               cchMultiByte      : INT32;
                               lpDefaultChar     : PCSTR;
                               lpUsedDefaultChar : PBOOL): INT32;

(*
 *  Locale Dependent APIs.
 *)

<*EXTERNAL CompareStringA:WINAPI*>
PROCEDURE CompareStringA (Locale     : LCID;
                          dwCmpFlags : UINT32;
                          lpString1  : PCSTR;
                          cchCount1  : INT32;
                          lpString2  : PCSTR;
                          cchCount2  : INT32): INT32;

<*EXTERNAL CompareStringW:WINAPI*>
PROCEDURE CompareStringW (Locale     : LCID;
                          dwCmpFlags : UINT32;
                          lpString1  : PCWSTR;
                          cchCount1  : INT32;
                          lpString2  : PCWSTR;
                          cchCount2  : INT32): INT32;

CONST CompareString = CompareStringA;

<*EXTERNAL LCMapStringA:WINAPI*>
PROCEDURE LCMapStringA (Locale     : LCID;
                        dwMapFlags : UINT32;
                        lpSrcStr   : PCSTR;
                        cchSrc     : INT32;
                        lpDestStr  : PSTR;
                        cchDest    : INT32): INT32;

<*EXTERNAL LCMapStringW:WINAPI*>
PROCEDURE LCMapStringW (Locale     : LCID;
                        dwMapFlags : UINT32;
                        lpSrcStr   : PCWSTR;
                        cchSrc     : INT32;
                        lpDestStr  : PWSTR;
                        cchDest    : INT32): INT32;

CONST LCMapString = LCMapStringA;

<*EXTERNAL GetLocaleInfoA:WINAPI*>
PROCEDURE GetLocaleInfoA (Locale   : LCID;
                          LCType   : LCTYPE;
                          lpLCData : PSTR;
                          cchData  : INT32): INT32;

<*EXTERNAL GetLocaleInfoW:WINAPI*>
PROCEDURE GetLocaleInfoW (Locale   : LCID;
                          LCType   : LCTYPE;
                          lpLCData : PWSTR;
                          cchData  : INT32): INT32;

CONST GetLocaleInfo = GetLocaleInfoA;

<*EXTERNAL SetLocaleInfoA:WINAPI*>
PROCEDURE SetLocaleInfoA (Locale   : LCID;
                          LCType   : LCTYPE;
                          lpLCData : PCSTR): BOOL;

<*EXTERNAL SetLocaleInfoW:WINAPI*>
PROCEDURE SetLocaleInfoW (Locale   : LCID;
                          LCType   : LCTYPE;
                          lpLCData : PCWSTR): BOOL;

CONST SetLocaleInfo = SetLocaleInfoA;

<*EXTERNAL GetTimeFormatA:WINAPI*>
PROCEDURE GetTimeFormatA (Locale    : LCID;
                          dwFlags   : UINT32;
                          lpTime    : PSYSTEMTIME;
                          lpFormat  : PCSTR;
                          lpTimeStr : PSTR;
                          cchTime   : INT32): INT32;

<*EXTERNAL GetTimeFormatW:WINAPI*>
PROCEDURE GetTimeFormatW (Locale    : LCID;
                          dwFlags   : UINT32;
                          lpTime    : PSYSTEMTIME;
                          lpFormat  : PCWSTR;
                          lpTimeStr : PWSTR;
                          cchTime   : INT32): INT32;

CONST GetTimeFormat = GetTimeFormatA;

<*EXTERNAL GetDateFormatA:WINAPI*>
PROCEDURE GetDateFormatA (Locale    : LCID;
                          dwFlags   : UINT32;
                          lpDate    : PSYSTEMTIME;
                          lpFormat  : PCSTR;
                          lpDateStr : PSTR;
                          cchDate   : INT32): INT32;

<*EXTERNAL GetDateFormatW:WINAPI*>
PROCEDURE GetDateFormatW (Locale    : LCID;
                          dwFlags   : UINT32;
                          lpDate    : PSYSTEMTIME;
                          lpFormat  : PCWSTR;
                          lpDateStr : PWSTR;
                          cchDate   : INT32): INT32;

CONST GetDateFormat = GetDateFormatA;

<*EXTERNAL GetNumberFormatA:WINAPI*>
PROCEDURE GetNumberFormatA (Locale      : LCID;
                            dwFlags     : UINT32;
                            lpValue     : PCSTR;
                            lpFormat    : LPNUMBERFMTA;
                            lpNumberStr : PSTR;
                            cchNumber   : INT32): INT32;

<*EXTERNAL GetNumberFormatW:WINAPI*>
PROCEDURE GetNumberFormatW (Locale      : LCID;
                            dwFlags     : UINT32;
                            lpValue     : PCWSTR;
                            lpFormat    : LPNUMBERFMTW;
                            lpNumberStr : PWSTR;
                            cchNumber   : INT32): INT32;

CONST GetNumberFormat = GetNumberFormatA;

<*EXTERNAL GetCurrencyFormatA:WINAPI*>
PROCEDURE GetCurrencyFormatA (Locale        : LCID;
                              dwFlags       : UINT32;
                              lpValue       : PCSTR;
                              lpFormat      : LPCURRENCYFMTA;
                              lpCurrencyStr : PSTR;
                              cchCurrency   : INT32): INT32;

<*EXTERNAL GetCurrencyFormatW:WINAPI*>
PROCEDURE GetCurrencyFormatW (Locale        : LCID;
                              dwFlags       : UINT32;
                              lpValue       : PCWSTR;
                              lpFormat      : LPCURRENCYFMTW;
                              lpCurrencyStr : PWSTR;
                              cchCurrency   : INT32): INT32;

CONST GetCurrencyFormat = GetCurrencyFormatA;

<*EXTERNAL EnumCalendarInfoA:WINAPI*>
PROCEDURE EnumCalendarInfoA (lpCalInfoEnumProc : CALINFO_ENUMPROCA;
                             Locale            : LCID;
                             Calendar          : CALID;
                             CalType           : CALTYPE): BOOL;

<*EXTERNAL EnumCalendarInfoW:WINAPI*>
PROCEDURE EnumCalendarInfoW (lpCalInfoEnumProc : CALINFO_ENUMPROCW;
                             Locale            : LCID;
                             Calendar          : CALID;
                             CalType           : CALTYPE): BOOL;

CONST EnumCalendarInfo = EnumCalendarInfoA;

<*EXTERNAL EnumTimeFormatsA:WINAPI*>
PROCEDURE EnumTimeFormatsA (lpTimeFmtEnumProc : TIMEFMT_ENUMPROCA;
                            Locale            : LCID;
                            dwFlags           : UINT32): BOOL;

<*EXTERNAL EnumTimeFormatsW:WINAPI*>
PROCEDURE EnumTimeFormatsW (lpTimeFmtEnumProc : TIMEFMT_ENUMPROCW;
                            Locale            : LCID;
                            dwFlags           : UINT32): BOOL;

CONST EnumTimeFormats = EnumTimeFormatsA;

<*EXTERNAL EnumDateFormatsA:WINAPI*>
PROCEDURE EnumDateFormatsA (lpDateFmtEnumProc : DATEFMT_ENUMPROCA;
                            Locale            : LCID;
                            dwFlags           : UINT32): BOOL;

<*EXTERNAL EnumDateFormatsW:WINAPI*>
PROCEDURE EnumDateFormatsW (lpDateFmtEnumProc : DATEFMT_ENUMPROCW;
                            Locale            : LCID;
                            dwFlags           : UINT32): BOOL;

CONST EnumDateFormats = EnumDateFormatsA;

<*EXTERNAL IsValidLocale:WINAPI*>
PROCEDURE IsValidLocale (Locale: LCID;  dwFlags: UINT32): BOOL;

<*EXTERNAL ConvertDefaultLocale:WINAPI*>
PROCEDURE ConvertDefaultLocale (Locale: LCID): LCID;

<*EXTERNAL GetThreadLocale:WINAPI*>
PROCEDURE GetThreadLocale (): LCID;

<*EXTERNAL SetThreadLocale:WINAPI*>
PROCEDURE SetThreadLocale (Locale: LCID): BOOL;

<*EXTERNAL GetSystemDefaultLangID:WINAPI*>
PROCEDURE GetSystemDefaultLangID (): LANGID;

<*EXTERNAL GetUserDefaultLangID:WINAPI*>
PROCEDURE GetUserDefaultLangID (): LANGID;

<*EXTERNAL GetSystemDefaultLCID:WINAPI*>
PROCEDURE GetSystemDefaultLCID (): LCID;

<*EXTERNAL GetUserDefaultLCID:WINAPI*>
PROCEDURE GetUserDefaultLCID (): LCID;

(*
 *  Locale Independent APIs.
 *)

<*EXTERNAL GetStringTypeExA:WINAPI*>
PROCEDURE GetStringTypeExA (Locale     : LCID;
                            dwInfoType : UINT32;
                            lpSrcStr   : PCSTR;
                            cchSrc     : INT32;
                            lpCharType : PUINT16): BOOL;

<*EXTERNAL GetStringTypeExW:WINAPI*>
PROCEDURE GetStringTypeExW (Locale     : LCID;
                            dwInfoType : UINT32;
                            lpSrcStr   : PCWSTR;
                            cchSrc     : INT32;
                            lpCharType : PUINT16): BOOL;

CONST GetStringTypeEx = GetStringTypeExA;

(*
 *  NOTE: The parameters for GetStringTypeA and GetStringTypeW are
 *        NOT the same.  The W version was shipped in NT 3.1.  The
 *        A version was then shipped in 16-bit OLE with the wrong
 *        parameters (ported from Win95).  To be compatible, we
 *        must break the relationship between the A and W versions
 *        of GetStringType.  There will be NO function call for the
 *        generic GetStringType.
 *
 *        GetStringTypeEx (above) should be used instead.
 *)

<*EXTERNAL GetStringTypeA:WINAPI*>
PROCEDURE GetStringTypeA (Locale     : LCID;
                          dwInfoType : UINT32;
                          lpSrcStr   : PCSTR;
                          cchSrc     : INT32;
                          lpCharType : PUINT16): BOOL;

<*EXTERNAL GetStringTypeW:WINAPI*>
PROCEDURE GetStringTypeW (dwInfoType : UINT32;
                          lpSrcStr   : PCWSTR;
                          cchSrc     : INT32;
                          lpCharType : PUINT16): BOOL;

<*EXTERNAL FoldStringA:WINAPI*>
PROCEDURE FoldStringA (dwMapFlags : UINT32;
                       lpSrcStr   : PCSTR;
                       cchSrc     : INT32;
                       lpDestStr  : PSTR;
                       cchDest    : INT32): INT32;

<*EXTERNAL FoldStringW:WINAPI*>
PROCEDURE FoldStringW (dwMapFlags : UINT32;
                       lpSrcStr   : PCWSTR;
                       cchSrc     : INT32;
                       lpDestStr  : PWSTR;
                       cchDest    : INT32): INT32;

CONST FoldString = FoldStringA;

<*EXTERNAL EnumSystemLocalesA:WINAPI*>
PROCEDURE EnumSystemLocalesA (lpLocaleEnumProc : LOCALE_ENUMPROCA;
                              dwFlags          : UINT32): BOOL;

<*EXTERNAL EnumSystemLocalesW:WINAPI*>
PROCEDURE EnumSystemLocalesW (lpLocaleEnumProc : LOCALE_ENUMPROCW;
                              dwFlags          : UINT32): BOOL;

CONST EnumSystemLocales = EnumSystemLocalesA;

<*EXTERNAL EnumSystemCodePagesA:WINAPI*>
PROCEDURE EnumSystemCodePagesA (lpCodePageEnumProc : CODEPAGE_ENUMPROCA;
                                dwFlags            : UINT32): BOOL;

<*EXTERNAL EnumSystemCodePagesW:WINAPI*>
PROCEDURE EnumSystemCodePagesW (lpCodePageEnumProc : CODEPAGE_ENUMPROCW;
                                dwFlags            : UINT32): BOOL;

CONST EnumSystemCodePages = EnumSystemCodePagesA;

END WinNLS.
