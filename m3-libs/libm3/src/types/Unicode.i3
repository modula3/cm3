(*---------------------------------------------------------------------------*\
 *  Copyright (c) 2003-2004 by Dirk Muysers.
 *  For details see file "COPYRIGHT"
 *
 *  INTERFACE: Unicode.i3
 *  PURPOSE:   Unicode character set
 *  HISTORY:
 *    DATE          PROGRAMMER       DESCRIPTION
 *    30-Nov-2003   Dirk Muysers     Initial creation
 *
 *----------------------------------------------------------------------------
 *  $Id$
\*---------------------------------------------------------------------------*)
INTERFACE Unicode;

(*---------------------------------------------------------------------------*\
 * DESCRIPTION:
 *
 * "Unicode" deals with individual characters that belong to the WIDECHAR type.
 * It includes constant definitions for the character codes of a few exotic 
 * characters, such as "Unicode.NL" for new-line.  It classifies
 * characters into groups, like digits or punctuation and provides
 * functions that map lower-case letters into upper-case and the like.
 *
 * Please note that this library is surrogate-blind. It considers
 * surrogates as ordinary --although illegal-- characters und does not
 * check their correct pairing.
 * 
\*---------------------------------------------------------------------------*)

CONST
  NUL  = W'\x0000'; SOH  = W'\x0001'; STX  = W'\x0002';  ETX  = W'\x0003';
  EOT  = W'\x0004'; ENQ  = W'\x0005'; ACK  = W'\x0006';  BEL  = W'\x0007';
  BS   = W'\x0008'; HT   = W'\x0009'; NL   = W'\x000A';  VT   = W'\x000B';
  NP   = W'\x000C'; CR   = W'\x000D'; SO   = W'\x000E';  SI   = W'\x000F';
  DLE  = W'\x0010'; DC1  = W'\x0011'; DC2  = W'\x0012';  DC3  = W'\x0013';
  DC4  = W'\x0014'; NAK  = W'\x0015'; SYN  = W'\x0016';  ETB  = W'\x0017';
  CAN  = W'\x0018'; EM   = W'\x0019'; SUB  = W'\x001A';  ESC  = W'\x001B';
  FS   = W'\x001C'; GS   = W'\x001D'; RS   = W'\x001E';  US   = W'\x001F';
  SP   = W'\x0020'; DEL  = W'\x007F'; 
  PAD  = W'\x0080'; HOP  = W'\x0081'; BPH  = W'\x0082';  NBH  = W'\x0083';
  IND  = W'\x0084'; NEL  = W'\x0085'; SSA  = W'\x0086';  ESA  = W'\x0087';
  HTS  = W'\x0088'; HTJ  = W'\x0089'; VTS  = W'\x008A';  PLD  = W'\x008B';
  PLU  = W'\x008C'; RI   = W'\x008D'; SS2  = W'\x008E';  SS3  = W'\x008F';
  DCS  = W'\x0090'; PU1  = W'\x0091'; PU2  = W'\x0092';  STS  = W'\x0093';
  CCH  = W'\x0094'; MW   = W'\x0095'; SPA  = W'\x0096';  EPA  = W'\x0097';
  SOS  = W'\x0098'; SGCI = W'\x0099'; SCI  = W'\x009A';  CSI  = W'\x009B';
  ST   = W'\x009C'; OSC  = W'\x009D'; PM   = W'\x009E';  APC  = W'\x009F';
  NBSP = W'\x00A0';

TYPE T = WIDECHAR;

(*---------------------------------------------------------------------------*\
 * Predicates
\*---------------------------------------------------------------------------*)

PROCEDURE IsDefined (t: T): BOOLEAN;
(* Returns "TRUE" if "t" has a defined meaning in Unicode. *)

PROCEDURE IsASCII (t: T): BOOLEAN;
(* Returns "TRUE" if "t" corresponds to an ASCII character. *)

PROCEDURE IsSpace (t: T): BOOLEAN;
(* Returns "TRUE" if "t" is a Unicode white space character. *)

PROCEDURE IsLetter (t: T): BOOLEAN;
(* Returns "TRUE" if "t" is a Unicode letter. *)

PROCEDURE IsDigit (t: T): BOOLEAN;
(* Returns "TRUE" if "t" is a Unicode digit. *)

PROCEDURE IsLetterOrDigit (t: T): BOOLEAN;
(* == "IsLetter(t) OR IsDigit(t)". *)

PROCEDURE IsUpperCase (t: T): BOOLEAN;
(* Returns "TRUE" if "t" is an upper-case Unicode letter. *)

PROCEDURE IsLowerCase (t: T): BOOLEAN;
(* Returns "TRUE" if "t" is a lower-case Unicode letter. *)

PROCEDURE IsTitleCase (t: T): BOOLEAN;
(* Returns "TRUE" if "t" is defined as a Unicode title-case letter. *)

(*---------------------------------------------------------------------------*\
 * Translations
\*---------------------------------------------------------------------------*)

PROCEDURE ToUpperCase (t: T): T;
(* Return the upper-case equivalent of "t" if it has one.
   Otherwise, return "t". *)

PROCEDURE ToLowerCase (t: T): T;
(* Return the upper-case equivalent of "t" if it has one.
   Otherwise, return "t". *)

PROCEDURE ToTitleCase (t: T): T;
(* Return the title-case equivalent of "t" if it has one.
   Otherwise, return "t". *)

PROCEDURE ToDigitValue (t: T): CARDINAL;
(* Returns the numeric value of the digit "t" if "IsDigit(t)" is "TRUE".
   Otherwise, returns "LAST (CARDINAL)". *)

END Unicode.
(* vi: set ai sw=2 ts=2 tw=80: *)
