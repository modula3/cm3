(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

INTERFACE Unicode;

(* Unicode Characters

   "Unicode" deals with individual characters.  It includes constant
   definitions for the character codes of a few exotic characters,
   such as "Unicode.NL" for new-line.  It classifies characters into
   groups, like digits or punctuation and provides functions that map
   lower-case letters into upper-case and the like.

   Index: characters; punctuation; case, converting characters;
          characters, case conversion; upper-case, converting to lower;
          lower-case, converting to upper
*)


CONST
  NUL = W'\000000';    SOH = W'\000001';    STX = W'\000002';    ETX = W'\000003';
  EOT = W'\000004';    ENQ = W'\000005';    ACK = W'\000006';    BEL = W'\000007';
  BS  = W'\000010';    HT  = W'\000011';    NL  = W'\000012';    VT  = W'\000013';
  NP  = W'\000014';    CR  = W'\000015';    SO  = W'\000016';    SI  = W'\000017';
  DLE = W'\000020';    DC1 = W'\000021';    DC2 = W'\000022';    DC3 = W'\000023';
  DC4 = W'\000024';    NAK = W'\000025';    SYN = W'\000026';    ETB = W'\000027';
  CAN = W'\000030';    EM  = W'\000031';    SUB = W'\000032';    ESC = W'\000033';
  FS  = W'\000034';    GS  = W'\000035';    RS  = W'\000036';    US  = W'\000037';
  SP  = W'\000040';    DEL = W'\000177';

(*--------------------------------------------------------- predicates ---*)

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

PROCEDURE IsLetterOrDigit (t): BOOLEAN;
(* == "IsLetter(t) OR IsDigit(t)". *)

PROCEDURE IsUpperCase (t: T): BOOLEAN;
(* Returns "TRUE" if "t" is an upper-case Unicode letter. *)

PROCEDURE IsLowerCase (t: T): BOOLEAN;
(* Returns "TRUE" if "t" is a lower-case Unicode letter. *)

PROCEDURE IsTitleCase (t: T): BOOLEAN;
(* Returns "TRUE" if "t" is defined as a Unicode title-case letter. *)

(*------------------------------------------------------- translations ---*)

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
