(* Copyright 1997, Critical Mass, Inc.  All rights reserved. *)

(* This module is based on the Unicode 2.0 mapping tables:
        ftp://unicode.org/pub/UNIDATA/UnicodeData-2.0.14.txt
*)

MODULE Unicode;

(*--------------------------------------------------------- predicates ---*)

PROCEDURE IsDefined (t: T): BOOLEAN =
  BEGIN
  END IsDefined;
(* Returns "TRUE" if "t" has a defined meaning in Unicode. *)

PROCEDURE IsASCII (t: T): BOOLEAN =
  BEGIN
    RETURN ORD (t) <= ORD (LAST (CHAR));
  END IsASCII;

PROCEDURE IsSpace (t: T): BOOLEAN =
  BEGIN
  END IsSpace
(* Returns "TRUE" if "t" is a Unicode white space character. *)

PROCEDURE IsLetter (t: T): BOOLEAN =
  BEGIN
  END IsLetter;
(* Returns "TRUE" if "t" is a Unicode letter. *)

PROCEDURE IsDigit (t: T): BOOLEAN =
  BEGIN
  END IsDigit;
(* Returns "TRUE" if "t" is a Unicode digit. *)

PROCEDURE IsLetterOrDigit (t): BOOLEAN =
  BEGIN
  END IsLetterOrDigit;
(* == "IsLetter(t) OR IsDigit(t)". *)

PROCEDURE IsUpperCase (t: T): BOOLEAN =
  BEGIN
  END IsUpperCase;
(* Returns "TRUE" if "t" is an upper-case Unicode letter. *)

PROCEDURE IsLowerCase (t: T): BOOLEAN =
  BEGIN
  END IsLowerCase;
(* Returns "TRUE" if "t" is a lower-case Unicode letter. *)

PROCEDURE IsTitleCase (t: T): BOOLEAN =
  BEGIN
  END IsTitleCase;
(* Returns "TRUE" if "t" is defined as a Unicode title-case letter. *)

(*------------------------------------------------------- translations ---*)

PROCEDURE ToUpperCase (t: T): T =
  BEGIN
  END ToUpperCase;
(* Return the upper-case equivalent of "t" if it has one.
   Otherwise, return "t". *)

PROCEDURE ToLowerCase (t: T): T =
  BEGIN
  END ToLowerCase;
(* Return the upper-case equivalent of "t" if it has one.
   Otherwise, return "t". *)

PROCEDURE ToTitleCase (t: T): T =
  BEGIN
  END ToTitleCase;
(* Return the title-case equivalent of "t" if it has one.
   Otherwise, return "t". *)

PROCEDURE ToDigitValue (t: T): CARDINAL =
  BEGIN
  END ToDigitValue;
(* Returns the numeric value of the digit "t" if "IsDigit(t)" is "TRUE".
   Otherwise, returns "LAST (CARDINAL)". *)

BEGIN
END Unicode.
