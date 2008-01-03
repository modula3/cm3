(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(* Last modified on Thu Jan  5 13:51:02 PST 1995 by detlefs    *)
(*      modified on Tue Mar 15 12:56:39 PST 1994 by heydon     *)
(*      modified on Fri Feb 18 13:12:30 PST 1994 by kalsow     *)
(*      modified on Tue Nov  9 08:37:38 PST 1993 by mcjones    *)
(*      modified on Thu Apr 29 16:32:36 PDT 1993 by muller     *)
(*      modified on Mon Feb 15 15:18:41 PST 1993 by ramshaw    *)

(* The "Fmt" interface provides procedures for formatting numbers and
   other data as text.
   \index{writing formatted data}
   \index{formatted data!writing}
*)

INTERFACE Fmt;

IMPORT Word, Real AS R, LongReal AS LR, Extended AS ER;

PROCEDURE Bool(b: BOOLEAN): TEXT;
(* Format "b" as {\tt \char'42TRUE\char'42} or  {\tt \char'42FALSE\char'42}. *)

PROCEDURE Char(c: CHAR): TEXT;
(* Return a text containing the character "c". *)

TYPE Base = [2..16];

PROCEDURE Int(n: INTEGER; base: Base := 10): TEXT;
PROCEDURE Unsigned(n: Word.T; base: Base := 16): TEXT;
(* Format the signed or unsigned number "n" in the specified base. *)

(* The value returned by "Int" or "Unsigned" never contains upper-case
   letters, and it never starts with an explicit base and underscore.
   For example, to render an unsigned number "N" in hexadecimal as a
   legal Modula-3 literal, you must write something like:
| "16_" & Fmt.Unsigned(N, 16)
*)

TYPE Style = {Sci, Fix, Auto};

PROCEDURE Real(
    x: REAL;
    style := Style.Auto;
    prec: CARDINAL := R.MaxSignifDigits - 1;
    literal := FALSE)
  : TEXT;
PROCEDURE LongReal(
    x: LONGREAL;
    style := Style.Auto;
    prec: CARDINAL := LR.MaxSignifDigits - 1;
    literal := FALSE)
  : TEXT;
PROCEDURE Extended(
    x: EXTENDED;
    style := Style.Auto;
    prec: CARDINAL := ER.MaxSignifDigits - 1;
    literal := FALSE)
  : TEXT;
(* Format the floating-point number "x". *)

(*

\paragraph*{Overview.}

   "Style.Sci" gives scientific notation with fields padded to fixed
   widths, suitable for making a table.  The parameter "prec"
   specifies the number of digits after the decimal point---that is,
   the relative precision.
   \index{scientific notation}

   "Style.Fix" gives fixed point, with "prec" once again specifying
   the number of digits after the decimal point---in this case, the
   absolute precision.  The results of "Style.Fix" have varying
   widths, but they will form a table if they are right-aligned (using
   "Fmt.Pad") in a sufficiently wide field.
   \index{fixed-point notation}

   "Style.Auto" is not intended for tables.  It gives scientific
   notation with at most "prec" digits after the decimal point for
   numbers that are very big or very small.  There may be fewer than
   "prec" digits after the decimal point because trailing zeros are
   suppressed.  For numbers that are neither too big nor too small, it
   formats the same significant digits---at most "prec+1" of them---in
   fixed point, for greater legibility.

   All styles omit the decimal point unless it is followed by at least
   one digit.

   Setting "literal" to "TRUE" alters all styles as necessary to make
   the result a legal Modula-3 literal of the appropriate type.


\paragraph*{Accuracy.}

   As discussed in the "Float" interface, the call "ToDecimal(x)"
   converts "x" to a floating-decimal number with automatic precision
   control~\cite{Steele,Gay}: Just enough digits are retained to
   distinguish "x" from other values of type "T", which implies that
   at most "T.MaxSignifDigits" are retained.  The "Real", "LongReal",
   and "Extended" procedures format those digits as an appropriate
   string of characters.  If the precision requested by "prec" is
   higher than the automatic precision provided by "ToDecimal(x)",
   they append trailing zeros.  If the precision requested by "prec"
   is lower, they round "ToDecimal(x)" as necessary, obeying the
   current rounding mode.  Because they exploit the "errorSign" field
   of the record "ToDecimal(x)" in doing this rounding, they get the
   same result that rounding "x" itself would give.

   As a consequence, setting "prec" higher than "T.MaxSignifDigits-1"
   in "Style.Sci" isn't very useful: The trailing digits of all of the
   resulting numbers will be zero.  Setting "prec" higher than
   "T.MaxSignifDigits-1" in "Style.Auto" actually has no effect at
   all, since trailing zeros are suppressed.


\paragraph*{Details.}

   We restrict ourselves at first to those cases where "Class(x)" is
   either "Normal" or "Denormal".

   In those cases, "Style.Sci" returns: a minus sign or blank, the
   leading nonzero digit of "x", a decimal point, "prec" more digits
   of "x", a character "'e'", a minus sign or plus sign, and
   "T.MaxExpDigits" of exponent (with leading zeros as necessary).
   When "prec" is zero, the decimal point is omitted.

   "Style.Fix" returns: a minus sign if necessary, one or more digits,
   a decimal point, and "prec" more digits---never any blanks.  When
   "prec" is zero, the decimal point is omitted.

   "Style.Auto" first formats "x" as in "Style.Sci", using scientific
   notation with "prec" digits after the decimal point.  Call this
   intermediate result "R".

   If the exponent of "R" is at least "6" in magnitude, "Style.Auto"
   leaves "R" in scientific notation, but condenses it by omitting all
   blanks, plus signs, trailing zero digits, and leading zeros in the
   exponent.  If this leaves no digits after the decimal point, the
   decimal point itself is omitted.

   If the exponent of "R" is at most "5" in magnitude, "Style.Auto"
   reformats the digits of "R" in fixed point, first deleting any
   trailing zeros and then adding leading or trailing zeros as
   necessary to bridge the gap from the digits of "R" to the unit's
   place.

   For example, assuming the current rounding mode is "NearestElseEven":

| Fmt.Real(1.287e6,  Style.Auto, prec := 2) = "1.29e6"
| Fmt.Real(1.297e6,  Style.Auto, prec := 2) = "1.3e6"
| Fmt.Real(1.297e5,  Style.Auto, prec := 2) = "130000"
| Fmt.Real(1.297e-5, Style.Auto, prec := 2) = "0.000013"
| Fmt.Real(1.297e-6, Style.Auto, prec := 2) = "1.3e-6"
| Fmt.Real(9.997e5,  Style.Auto, prec := 2) = "1e6"
| Fmt.Real(9.997e-6, Style.Auto, prec := 2) = "0.00001"

   "Style.Sci" handles zero by replacing the entire exponent field by
   blanks, for example: {\tt \char'042\char'040 0.00\char'040
   \char'040 \char'040 \char'040 \char'042}.  "Style.Fix" renders zero
   with all digits zero; for example, "\char'042 0.00\char'042".
   "Style.Auto" renders zero as "\char'042 0\char'042".  On IEEE
   implementations, the value minus zero is rendered as a negative
   number.

   Also on IEEE implementations, "Style.Sci" formats infinities or
   NaN's with a minus sign or blank, the string "\char'042
   Infinity\char'042" or "\char'042 NaN\char'042", and enough trailing
   blanks to get the correct overall width.  "Style.Fix" and
   "Style.Auto" omit the blanks.  In "Style.Sci", if "\char'042
   Infinity\char'042" doesn't fit, "\char'042 Inf\char'042" is used
   instead.

   Setting "literal" to "TRUE" alters things as follows: Numbers that
   are rendered without a decimal point when "literal" is "FALSE" have
   a decimal point and one trailing zero appended to their digits.
   For the routines "Fmt.LongReal" and "Fmt.Extended", an exponent
   field of "d0" or "x0" is appended to numbers in fixed point and
   "'d'" or "'x'" is used, rather than "'e'", to introduce the
   exponents of numbers in scientific notation.  On IEEE
   implementations, the string "\char'042 Infinity\char'042" is
   replaced by "\char'042 1.0/0.0\char'042", "\char'042
   1.0d0/0.0d0\char'042", or "\char'042 1.0x0/0.0x0\char'042" as
   appropriate, and "\char'042 NaN\char'042" is similarly replaced by
   a representation of the quotient "0/0".  (Unfortunately, these
   quotient strings are so long that they may ruin the formatting of
   "Style.Sci" tables when "prec" is small and "literal" is "TRUE".)

*)

TYPE Align = {Left, Right};

PROCEDURE Pad(
    text: TEXT;
    length: CARDINAL;
    padChar: CHAR := ' ';
    align: Align := Align.Right): TEXT;
(* If "Text.Length(text) >= length", then "text" is returned
   unchanged.  Otherwise, "text" is padded with "padChar" until it has
   the given "length".  The text goes to the right or left, according
   to "align". *)

PROCEDURE F(fmt: TEXT; t1, t2, t3, t4, t5: TEXT := NIL)
  : TEXT;
(* Uses "fmt" as a format string. The result is a copy of "fmt" in
   which all format specifiers have been replaced, in order, by the
   text arguments "t1", "t2", etc. *)

(* A format specifier contains a field width, alignment and one of two
   padding characters. The procedure "F" evaluates the specifier and
   replaces it by the corresponding text argument padded as it would
   be by a call to "Pad" with the specified field width, padding
   character and alignment.

   The syntax of a format specifier is:
| %[-]{0-9}s
   that is, a percent character followed by an optional minus sign, an
   optional number and a compulsory terminating "s".

   If the minus sign is present the alignment is "Align.Left",
   otherwise it is "Align.Right". The alignment corresponds to the
   "align" argument to "Pad".

   The number specifies the field width (this corresponds to the
   "length" argument to "Pad"). If the number is omitted it defaults
   to zero.

   If the number is present and starts with the digit "0" the padding character
   is "'0'"; otherwise it is the space character. The padding character
   corresponds to the "padChar" argument to "Pad".

   It is a checked runtime error if "fmt" is "NIL" or the number of
   format specifiers in "fmt" is not equal to the number of non-nil
   arguments to "F".

   Non-nil arguments to "F" must precede any "NIL" arguments; it is a
   checked runtime error if they do not.

   If "t1" to "t5" are all "NIL" and "fmt" contains no format
   specifiers, the result is "fmt".

   Examples:
| F("%s %s\n", "Hello", "World") `returns` "Hello World\n".
| F("%s", Int(3))                `returns` "3"
| F("%2s", Int(3))               `returns` " 3"
| F("%-2s", Int(3))              `returns` "3 "
| F("%02s", Int(3))              `returns` "03"
| F("%-02s", Int(3))             `returns` "30"
| F("%s", "%s")                  `returns` "%s"
| F("%s% tax", Int(3))           `returns` "3% tax"

   The following examples are legal but pointless:
| F("%-s", Int(3))               `returns` "3"
| F("%0s", Int(3))               `returns` "3"
| F("%-0s", Int(3))              `returns` "3"
*)

PROCEDURE FN(fmt: TEXT; READONLY texts: ARRAY OF TEXT)
  : TEXT;
(* Similar to "F" but accepts an array of text arguments. It is a
   checked runtime error if the number of format specifiers in "fmt"
   is not equal to "NUMBER(texts)" or if any element of "texts" is
   "NIL". If "NUMBER(texts) = 0" and "fmt" contains no format
   specifiers the result is "fmt". *)

(* Example:

| FN("%s %s %s %s %s %s %s",
|   ARRAY OF TEXT{"Too", "many", "arguments",
|     "for", "F", "to", "handle"})

   returns {\tt \char'42Too many arguments for F to handle\char'42}.
*)

END Fmt.

<*PRAGMA SPEC *>

<*SPEC Bool(b)                           ENSURES RES # NIL *>
<*SPEC Char(c)                           ENSURES RES # NIL *>
<*SPEC Int(n, base)                      ENSURES RES # NIL *>
<*SPEC Unsigned(n, base)                 ENSURES RES # NIL *>
<*SPEC Real(x, style, prec, literal)     ENSURES RES # NIL *>
<*SPEC LongReal(x, style, prec, literal) ENSURES RES # NIL *>
<*SPEC Extended(x, style, prec, literal) ENSURES RES # NIL *>

