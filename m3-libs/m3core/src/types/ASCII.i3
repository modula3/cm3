(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Tue May 11 16:59:03 PDT 1993 by swart          *)
(*      modified on Thu Apr 22 09:56:55 PDT 1993 by mcjones        *)
(*      modified on Thu Nov  2 21:55:29 1989 by muller         *)
(*      modified on Fri Sep 29 15:46:46 1989 by kalsow         *)
(*      modified on Fri Jan 20 10:02:29 PST 1989 by glassman   *)
(*      modified on Wed May 27 23:11:56 1987 by mbrown         *)
(*      modified Mon May 13 20:11:50 1985 by Ellis             *)

INTERFACE ASCII;

(* Ascii Characters

   Char deals with individual characters.  It includes constant
   definitions for the character codes of exotic characters, such
   as Char.NL for new-line.  It classifies characters into groups,
   like digits or punctuation; each group is represented as a set
   of characters.  Finally, it provides mapping tables that translate
   lower-case letters into upper-case and the like.

   For systems with Unicode CHARs this interface can be used for
   classifying the subset whose values are in the range 0 to 255.

   Index: characters; punctuation; case, converting characters;
          characters, case conversion; upper-case, converting to lower;
          lower-case, converting to upper
*)


CONST
  NUL = '\000';    SOH = '\001';    STX = '\002';    ETX = '\003';
  EOT = '\004';    ENQ = '\005';    ACK = '\006';    BEL = '\007';
  BS  = '\010';    HT  = '\011';    NL  = '\012';    VT  = '\013';
  NP  = '\014';    CR  = '\015';    SO  = '\016';    SI  = '\017';
  DLE = '\020';    DC1 = '\021';    DC2 = '\022';    DC3 = '\023';
  DC4 = '\024';    NAK = '\025';    SYN = '\026';    ETB = '\027';
  CAN = '\030';    EM  = '\031';    SUB = '\032';    ESC = '\033';
  FS  = '\034';    GS  = '\035';    RS  = '\036';    US  = '\037';
  SP  = '\040';    DEL = '\177';

TYPE
  Range = ['\000'..'\377'];
    (* Characters which are representable in both 8 bit and unicode
       characters. *)

  Set = SET OF Range;

CONST
  All           = Set{FIRST(Range).. LAST(Range)};
  Asciis        = Set{'\000'.. '\177'};
  Controls      = Set{'\000'.. '\037', '\177'};
  Spaces        = Set{' ', '\t', '\n', '\r', '\f'};
  Digits        = Set{'0'.. '9'};
  Uppers        = Set{'A'.. 'Z'};
  Lowers        = Set{'a'.. 'z'};
  Letters       = Uppers + Lowers;
  AlphaNumerics = Letters + Digits;
  Graphics      = Asciis - Controls;
  Punctuation   = Graphics - AlphaNumerics;

VAR
  Upper   : ARRAY Range OF Range;
  Lower   : ARRAY Range OF Range;
  Control : ARRAY Range OF Range;
  (* These constant arrays implement character conversions (mappings):

         Upper[c]   = the upper-case equivalent of c if c is a letter, o.w. c
         Lower[c]   = the lower-case equivalent of c if c is a letter, o.w. c
         Control[c] = the control-shifted equivalent of c if c is in Graphics
                       (i.e. BitAnd( c, 037B )), o.w. c
    *)

END ASCII.
