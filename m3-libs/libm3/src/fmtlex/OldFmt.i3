(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Fri Feb 18 13:14:53 PST 1994 by kalsow     *)
(*      modified on Fri Jun 21 20:57:07 1991 by muller         *)

INTERFACE OldFmt;

IMPORT Text;

TYPE
  Align = {Left, Right};
  Base = [2..16];

  Style = {Flo, AltFlo, Sci, AltSci, Mix};
       
    (* Formatting styles for REALs.  The Sci and AltSci formats are
       "<digit>.<digits>[E|D]<exponent>"; "E" is used for REALs, "D" is
       used for LONGREALs.  The Flo and AltFlo formats are simply
       "<digits>.<digits>".  In the Alt formats, trailing zeros are
       suppressed in the decimal part; in both formats, a decimal point is
       always printed.  The Mix format is AltFlo unless AltSci is shorter;
       if AltFlo is selected and there are no zeros after the decimal
       point, the decimal point is suppressed. *)

<*OBSOLETE*>       
PROCEDURE Bool (b: BOOLEAN): Text.T;
(* Format b as "TRUE" or "FALSE". *)

<*OBSOLETE*>       
PROCEDURE Int (n: INTEGER;  base : Base := 10): Text.T;
(* Format n in the given base. *)

<*OBSOLETE*>       
PROCEDURE Unsigned (n: INTEGER;  base : Base := 16): Text.T;
(* Format n in the given base. *)

<*OBSOLETE*>       
PROCEDURE Addr (n: ADDRESS;  base : Base := 16): Text.T;
(* Format n in the given base.  Return "NIL" if n = NIL. *)

<*OBSOLETE*>       
PROCEDURE Ref (r: REFANY;  base : Base := 16): Text.T;
(* Format r in the given base.  Return "NIL" if n = NIL. *)

<*OBSOLETE*>       
PROCEDURE Real (x: REAL;  precision: CARDINAL:= 6; style := Style.Mix): Text.T;
(* Format x in the given style.  The precision is the number of digits 
   after the decimal point, or the maximum number for the Alt formats.  *)

<*OBSOLETE*>       
PROCEDURE LongReal (x: LONGREAL;  precision: CARDINAL:= 6;
                    style := Style.Mix): Text.T;
(* Format x in the given style.  The precision is the number of digits
   after the decimal point, or the maximum number for the Alt formats.  *)

<*OBSOLETE*>       
PROCEDURE Char (c: CHAR): Text.T;
(* Return a text containing the character c. *)

<*OBSOLETE*>       
PROCEDURE Pad (text: Text.T;  length: CARDINAL;
               padChar: CHAR := ' ';  align : Align := Align.Right): Text.T;
(* If Text.Length(text) >= length, then text is returned unchanged.
   Otherwise, text is padded with padChar until it has the given length.
   The text goes to the right or left, according to align. *)

<*OBSOLETE*>       
PROCEDURE F(fmt: Text.T; t1, t2, t3, t4, t5: Text.T := NIL): Text.T RAISES {};
(* Uses 'fmt' as a format string. The result is a copy of 'fmt' in which all
   format specifiers have been replaced, in order, by the text arguments 't1',
   't2' etc.

   A format specifier contains a field width, alignment and one of two padding
   characters. 'F' evaluates the specifier and replaces it by the corresponding
   text argument padded as it would be by a call to 'Pad' with the specified
   field width, padding character and alignment.

   The syntax of a format specifier is:
     %[-]{0-9}s
   i.e. a percent character followed by an optional '-', an optional number
   and a compulsory terminating 's'.

   If the '-' is present the alignment is 'Align.Left' otherwise it is
   'Align.Right'. The alignment corresponds to the 'align' argument to 'Pad'.

   The number specifies the field width (this corresponds to the 'length'
   argument to 'Pad'). If the number is omitted it defaults to zero.

   If the number is present and starts with the digit '0' the padding character
   is '0'; otherwise it is the space character. The padding character
   corresponds to the 'padChar' argument to 'Pad'.

   Format specifiers seem complicated but are fairly simple to use in practice.
   See the end of this comment for some examples.

   It is a checked runtime error if 'fmt' is NIL or the number of format
   specifiers in 'fmt' is not equal to the number of non NIL arguments to 'F'.

   Non NIL arguments to 'F' must precede any NIL arguments; it is a checked
   runtime error if they do not. e.g. if 't5' is non NIL then 't1' to 't4' must
   be non NIL also.

   If 't1' to 't5' are all NIL and 'fmt' contains no format specifiers the
   the result is 'fmt'

   Examples:
     F("%s %s\n", "Hello", "World") returns "Hello World\n".
     F("%s", Int(3))                returns "3"
     F("%2s", Int(3))               returns " 3"
     F("%-2s", Int(3))              returns "3 "
     F("%02s", Int(3))              returns "03"
     F("%-02s", Int(3))             returns "30"
     F("%s", "%s")                  returns "%s"
     F("%s% tax", Int(3))           returns "3% tax"

   The following examples are legal but pointless:
     F("%-s", Int(3))               returns "3"
     F("%0s", Int(3))               returns "3"
     F("%-0s", Int(3))              returns "3"
*)

<*OBSOLETE*>       
PROCEDURE FN(fmt: Text.T; READONLY texts: ARRAY OF Text.T): Text.T RAISES {};
(* Similar to 'F' but accepts an array of text arguments. It is a checked
   runtime error if the number of format specifiers in 'fmt' is not equal to
   'NUMBER(texts)' or if any element of 'texts' is NIL.

   If 'NUMBER(texts) = 0' and 'fmt' contains no format specifiers the result is
   'fmt'.

   Example:
     FN("%s %s %s %s %s %s %s",
         Text.Array{"Too", "many", "arguments", "for", "F", "to", "handle"})
   returns "Too many arguments for F to handle"
*)

END OldFmt.
