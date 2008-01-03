(* Copyright (C) 1994, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(* Last modified on Fri Feb 25 14:22:27 PST 1994 by kalsow    *)
(*      modified on Tue Nov 30 23:33:48 PST 1993 by heydon    *)
(*      modified on Wed Oct  6 09:09:31 PDT 1993 by mcjones   *)
(*      modified on Mon Feb 15 14:17:17 PST 1993 by ramshaw   *)
(*      modified on Sun Jun  3 14:25:23 PST 1991 by luca      *)

(* The "Lex" interface provides procedures for reading strings,
   booleans, integers, and floating-point numbers from an input
   stream.  Similar functionality on text strings is available
   from the "Scan" interface. *)

INTERFACE Lex;

IMPORT FloatMode, Rd, Word;
FROM Thread IMPORT Alerted;

EXCEPTION Error;

CONST
   Blanks = SET OF CHAR{
     ' ', '\t', '\n', '\r', '\013' (* vertical tab *), '\f'};
   NonBlanks = SET OF CHAR{'!' .. '~'};

(* Each of the procedures in this interface reads a specified prefix
   of the characters in the reader passed to the procedure, and leaves
   the reader positioned immediately after that prefix, perhaps at
   end-of-file.  Each procedure may call "Rd.UngetChar" after its
   final call on "Rd.GetChar". *)

PROCEDURE Scan(
    rd: Rd.T; READONLY cs: SET OF CHAR := NonBlanks): TEXT 
  RAISES {Rd.Failure, Alerted};
(* Read the longest prefix of "rd" composed of characters in "cs" and
   return that prefix as a "TEXT".  *)

PROCEDURE Skip(
    rd: Rd.T; READONLY cs: SET OF CHAR := Blanks)
  RAISES {Rd.Failure, Alerted};
(* Read the longest prefix of "rd" composed of characters in "cs" and
   discard it.  *)

(* Whenever a specification of one of the procedures mentions skipping
   blanks, this is equivalent to performing the call "Skip(rd, Blanks)". *)

PROCEDURE Match(rd: Rd.T; t: TEXT)
  RAISES {Error, Rd.Failure, Alerted};
(* Read the longest prefix of "rd" that is also a prefix of "t".
   Raise "Error" if that prefix is not, in fact, equal to all of "t". *)

PROCEDURE Bool(rd: Rd.T): BOOLEAN RAISES {Error, Rd.Failure, Alerted};
(* Read a boolean from "rd" and return its value. *)

(* "Bool" skips blanks, then reads the longest prefix of "rd" that is
   a prefix of a "Boolean" in the following grammar:

| Boolean = "F" "A" "L" "S" "E" | "T" "R" "U" "E".

   The case of letters in a "Boolean" is not significant.  If the
   prefix read from "rd" is an entire "Boolean", "Bool" returns that
   boolean; else it raises "Error".  *)

PROCEDURE Int(rd: Rd.T; defaultBase: [2..16] := 10)
  : INTEGER RAISES {Error, FloatMode.Trap, Rd.Failure, Alerted};
PROCEDURE Unsigned(rd: Rd.T; defaultBase: [2..16] := 16)
  : Word.T RAISES {Error, FloatMode.Trap, Rd.Failure, Alerted};
(* Read a number from "rd" and return its value. *)

(* Each procedure skips blanks, then reads the longest prefix of "rd"
   that is a prefix of a "Number" as defined by the grammar below.  If
   "defaultBase" exceeds 10, then the procedure scans for a
   "BigBaseNum"; otherwise it scans for a "SmallBaseNum".  The effect
   of this rule is that the letters 'a' through 'f' and 'A' through
   'F' stop the scan unless either the "defaultBase" or the explicitly
   provided base exceeds 10.  "Unsigned" omits the scan for a "Sign".

| Number       = [Sign] (SmallBaseNum | BigBaseNum).
| SmallBaseNum = DecVal | BasedInt.
| BigBaseNum   = HexVal | BasedInt.
| BasedInt     = SmallBase "_" DecVal | BigBase "_" HexVal.
| DecVal       = Digit {Digit}.
| HexVal       = HexDigit {HexDigit}.
| Sign         = "+" | "-".
| SmallBase    = "2" | "3" | ... | "10".
| BigBase      = "11" | "12" | ... | "16".
| Digit        = "0" | "1" | ... | "9".
| HexDigit     = Digit | "A" | "B" | "C" | "D" | "E" | "F"
|                      | "a" | "b" | "c" | "d" | "e" | "f".

   If the prefix read from "rd" is an entire "Number" (as described
   above), the corresponding number is returned; else "Error" is
   raised.

   If an explicit base is given with an underscore, it is interpreted
   in decimal.  In this case, the digits in "DecVal" or "HexVal" are
   interpreted in the explicit base, else they are interpreted in the
   "defaultBase".

   Both procedures may raise "FloatMode.Trap(IntOverflow)".  They
   raise "Error" if some digit in the value part is not a legal digit
   in the chosen base. *)

PROCEDURE Real(rd: Rd.T): REAL
  RAISES {Error, FloatMode.Trap, Rd.Failure, Alerted};
PROCEDURE LongReal(rd: Rd.T): LONGREAL
  RAISES {Error, FloatMode.Trap, Rd.Failure, Alerted};
PROCEDURE Extended(rd: Rd.T): EXTENDED 
  RAISES {Error, FloatMode.Trap, Rd.Failure, Alerted};
(* Read a real number from "rd" and return its value. *)

(* Each procedure skips blanks, then reads the longest prefix of "rd"
   that is a prefix of a floating-decimal number "Float" in the
   grammar:

| Float  = [Sign] FloVal [Exp].
| FloVal = {Digit} (Digit | Digit "." | "." Digit) {Digit}.
| Exp    = Marker [Sign] Digit {Digit}.
| Marker = ("E" | "e" | "D" | "d" | "X" | "x").

   where "Sign" and "Digit" are as defined above.  If the prefix read
   from "rd" is an entire "Float", that "Float" is converted to a
   "REAL", "LONGREAL", or "EXTENDED" using the routine "FromDecimal"
   in the appropriate instance of the "Float" generic interface; else
   "Error" is raised.  Note that the exponent of "Float" can be
   introduced with any of the six characters "'e'", "'E'", "'d'",
   "'D'", "'x'", or "'X'", independent of the target type of the
   conversion.

   On IEEE implementations, the syntax for "Float" is extended as
   follows:

| Float   = [Sign] FloVal [Exp] | [Sign] IEEEVal.
| IEEEVal = "I" "N" "F" "I" "N" "I" "T" "Y" | "I" "N" F"
|         | "N" "A" "N".

   The case of letters in an "IEEEVal" is not significant. The
   "FloatMode.Trap" exception may be raised with any of the arguments
   "Overflow", "Underflow", or "Inexact".

*)

END Lex.
