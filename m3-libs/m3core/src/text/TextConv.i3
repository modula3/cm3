(* Copyright (C) 1993 Digital Equipment Corporation.         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* Last modified on Wed Nov 24 09:40:22 PST 1993 by kalsow   *)
(*      modified on Fri Jul 16 19:42:47 1993 by luca         *)

INTERFACE TextConv;

(* A collection of text routines for the handling of escape sequences
   and separators. *)

TYPE CharSet = SET OF CHAR;

CONST
  Escape = '\\';
  NonPrinting = CharSet{'\000'..'\037', '\177'};
  Quotes = CharSet{'\'', '\"'};

(* Given the constants above, we have the following mapping for
   special characters that may appear in a text:

  encoded   decoded
    \\        \
    \'        '
    \"        "
    \n        LF
    \r        CR
    \t        HT
    \f        FF
    \0xx      0xx        where 0xx is an octal 000..077.
    \1xx      1xx        where 1xx is an octal 100..177.
    \x        x          where x is any other character.
*)

(* Summary.

  The Encode routine converts text with special characters into text 
  with excape sequences that encode those special characters. 
  The Decode routine act as a semi-inverse.

  The Explode routine segments texts in base of the occurrence of 
  delimiter characters (such as blanks). The Implode routine acts
  as a semi-inverse).

*)

EXCEPTION Fail;

(* ==== ENCODING ==== *)

PROCEDURE Encode(textIn: TEXT; quoted: BOOLEAN:=TRUE): TEXT;
(* Return a text that is the encoding of textIn. If quoted is TRUE,
   a double quote is added at the beginning an one at the end. *)

PROCEDURE EncodedCharSize(charIn: CHAR): INTEGER;
(* Return the size of the encoding of charIn; either 1, 2, or 4. *)

PROCEDURE EncodeChar(
    charIn: CHAR; 
    VAR (*out*)charsOut: ARRAY[0..3] OF CHAR)
    : INTEGER;
(* Write the encoding of charIn to charsOut; the result is the number of 
   significant elements of charsOut, either 1, 2, or 4.  *)

PROCEDURE EncodedCharsSize(
    READONLY charsIn: ARRAY OF CHAR): INTEGER;
(* Return the sum of the sizes of the encodings of all the characters
   in charIn. *)

PROCEDURE EncodeChars(
    READONLY charsIn: ARRAY OF CHAR; 
    VAR (*out*)charsOut: ARRAY OF CHAR)
    : INTEGER;
(* Write the encoding of charsIn to charsOut; charsOut must be at least of
   EncodedCharsSize(charsIn) size, which is also given as result. *)

(* ==== DECODING ==== *)

PROCEDURE Decode(textIn: TEXT; quoted: BOOLEAN:=TRUE): TEXT RAISES {Fail};
(* Return a text that is the decoding of textIn. If quoted is TRUE, it
   strips the first and last character of textIn before decoding the rest.
   Fails if the escape sequences in textIn are ill-formed (e.g. if the last 
   char of textIn is \ ), or, when quoted is TRUE, if Length(textIn)<2 or 
   if the first and last characters are not double quotes. *)

PROCEDURE DecodeChar(
    READONLY charsIn: ARRAY[0..3] OF CHAR; availIn: INTEGER;
    VAR (*out*)charOut: CHAR)
    : INTEGER RAISES {Fail};
(* Decode the sequence charsIn (of which availIn are provided) as a character,
   and write it to charOut. Fail if not enough characters are available in 
   charsIn for the decoding of a single character, or if octal encodings are
   ill-formed. Return the number of characters of charsIn actually used. *)

PROCEDURE DecodedCharsSize(
    READONLY charsIn: ARRAY OF CHAR)
    : INTEGER RAISES {Fail};
(* Apply DecodeChar repeatedly to charsIn until exhausted, and return the number
   of calls to DecodeChar. Fail if any of the DecodeChar calls fail. *)

PROCEDURE DecodeChars(
    READONLY charsIn: ARRAY OF CHAR; 
    VAR (*out*)charsOut: ARRAY OF CHAR)
    : INTEGER RAISES {Fail};
(* Apply DecodeChar repeatedly to charsIn until exhausted, and place the results
   in charsOut; charsOut must be at least of DecodedCharsSize(charsIn) size, 
   which is also given as result. Fail if any of the calls to DecodeChar fail. *)

(* ==== EXPLODING ==== *)

PROCEDURE Explode(text: TEXT; VAR(*out*) array: ARRAY OF TEXT; 
    READONLY sep: SET OF CHAR);
(* Split an input text into a similarly ordered array of texts, each a 
   maximal subsequence of the input not containing sep chars. The empty
   text is exploded as a singleton array of the empty text.
   Each sep char in the input produces a break, so the size of the result 
   is 1 + the number of sep chars in the text.
   Implode(Explode(text,{ch}),ch) is the identity.
*)

PROCEDURE ExplodedSize(text: TEXT; READONLY sep: SET OF CHAR): INTEGER;
(* Return the length of the array needed by Explode on the same input;
   always > 0. *)

(* ==== IMPLODING ==== *)

PROCEDURE Implode(READONLY array: ARRAY OF TEXT; sep: CHAR): TEXT;
(* Concatenate an array of texts into a single text, separating the pieces 
   by a single sep char.  A zero-length array is imploded as an empty text.
   Explode(Implode(array,ch),{ch}) is the identity provided that
   array has positive size and sep does not occur in array. *)

END TextConv.
