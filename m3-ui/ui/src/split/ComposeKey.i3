(* Copyright (C) 1993, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Jim Meehan and Mark Manasse                                            *)
(* Last modified on Thu Mar  4 21:15:32 PST 1993 by msm                      *)
(*      modified on Mon Feb 15 13:14:01 PST 1993 by meehan                   *)

INTERFACE ComposeKey;

(* Software support for composing non-standard characters *)

IMPORT VBT;

TYPE
  T <: Public;
  Public =
    OBJECT
    METHODS
      filter  (cd: VBT.KeyRec): VBT.KeyRec;
      compose (k1, k2: VBT.KeySym): VBT.KeySym;
      feedback (composing: BOOLEAN)
    END;

END ComposeKey.

(* On many keyboards, there is a key labeled "Compose" or "Compose
   Character", which is used for typing non-standard characters such
   as an accented "e" or a copyright symbol. The two characters that
   are typed after the "Compose" key is pressed and released are
   combined to form one character.  For example, the key-sequence
   "Compose c comma" produces a lower-case c with a cedilla.

   Here is an example showing the intended use of this interface.
   Assume that "TextEditingVBT" is a subtype of "VBT" used for typing
   text, such as "TypeinVBT.T" or "TextPort.T".  A client would
   override the "key" method in order to filter the keys delivered to
   the supertype's "key" method.

   Note that in some editing models, several keys can be generated from a
   single keystroke.  To support this, applications should keep invoking the
   filter method until the returned key is VBT.NoKey.  Subsequent calls
   to the method should use VBT.NoKey as the depressed key.  Here's an
   example of the use of this type:

| TYPE
|   MyTextEditor =
|     TextEditingVBT.T OBJECT
|         comp: ComposeKey.T 
|       OVERRIDES
|         key := Key 
|       END;
| 
| PROCEDURE Key (v: MyTextEditor; READONLY inputCd; VBT.KeyRec) =
|   VAR cd := inputCd; BEGIN
|     IF inputCd.wentDown AND cd.whatChanged # VBT.NoKey THEN
|       LOOP
|         cd := v.comp.filter(cd);
|         IF cd.whatChanged = VBT.NoKey THEN EXIT END;
|         TextEditingVBT.T.key(v, cd);
|         cd.whatChanged := VBT.NoKey
|       END
|     END
|   END Key;
| 
| VAR editor :=
|   NEW (MyTextEditor, comp := NEW (ComposeKey.T));

   A "ComposeKey.T" maintains an internal state that keep track of the
   sequence of "VBT.KeyRec"s that are passed to the "filter" method.
   In its normal, initial state, the filter merely returns it
   argument.  For the first two keys in a composition-sequence, it
   returns a "VBT.KeyRec" whose "cd.whatChanged" is "VBT.NoKey", and
   for the last key in the sequence, it returns a "VBT.KeyRec" whose
   "cd.whatChanged" represents the non-standard character.

   Using the previous example, three consecutive calls to "filter"
   with "Keyrec"s containing "Compose" ("KeyboardKey.Multi_key"), "c",
   and "comma" will produce three "KeyRec"s. In the first two,
   "cd.whatChanged" will be "VBT.NoKey"; in the last one,
   "cd.whatChanged" will be "Latin1Key.ccedilla".

   During the third call, "filter" passes the "KeySym"s for "c" and
   "comma" to "compose" to produce the non-standard character.  By
   default, the "compose" method uses VT220-style composition of ISO
   Latin-1 characters.  A client can override the "compose" method to
   produce some other style of composition.
  
   The default "compose" method ignores case where there is no
   ambiguity.  For example, "c" and "o" can be combined to produce the
   copyright symbol; so can "C" and "O", "c" and "O", or "C" and "o".
   By contrast, "e" and "`" can be combined to produce a lower-case
   "e" with a grave accent; "E" and "`", however, produce an
   upper-case "E" with a grave accent.

   Unless both of the characters are alphanumeric, they can be
   combined in either order.  So "`" and "e" have the same effect as
   "e" and "`", but "o" and "c" do {\em not} combine to form the
   copyright symbol.

   If there is no known combination for the two characters, then
   "compose" returns "VBT.NoKey".

   The "ComposeKey" object is in one of two states; composing or not
   composing. When "filter" detects a state-change, it calls the
   "feedback" method with a flag indicating the state.  The default
   method is a no-op; a client may wish to override that in order to
   provide a visual cue to the user that key-composition is in effect
   (e.g., changing the cursor).  Otherwise, the user might not
   understand why typed character are not being ``echoed''.

   Here is the complete ISO Latin-1 combination-table (courtesy of
   Henri Gouraud of PRL).  The alternate keystroke sequences are ones
   where Bell Labs Plan 9 or the X consortium differ:

|  ++    NUMBER SIGN
|  '<SP> APOSTROPHE
|  AA    AT SIGN
|  ((    LEFT BRACKET
|  //    BACKSLASH [also /<]
|  ))    RIGHT BRACKET
|  ^<SP> CIRCUMFLEX [also ><SP>]
|  `<SP> GRAVE
|  (-    LEFT BRACE
|  /^    BAR [also vl]
|  )-    RIGHT BRACE
|  ~<SP> TILDE [also -<SP>]
| <SP><SP>   NO-BREAK SPACE
|  !!    INVERTED EXCLAMATION MARK
|  c/    CENT SIGN [also c$, c|]
|  L-    POUND SIGN [also l$, l=]
|  XO    CURRENCY SIGN [also g$]
|  Y-    YEN SIGN [also y$, y=]
|  ||    BROKEN BAR [also vb, |^]
|  SO    SECTION SIGN [also SS (not ss!), s!] 
|  ""    DIAERESIS
|  co    COPYRIGHT SIGN
|  a_    FEMININE ORDINAL INDICATOR [also sa]
|  <<    LEFT ANGLE QUOTATION MARK
|  -,    NOT SIGN [also no]
|  --    HYPHEN
|  RO    REGISTERED TRADE MARK SIGN
|  -^    MACRON [also __, _^]
|  0^    RING ABOVE, DEGREE SIGN [also de, 0*]
|  +-    PLUS-MINUS SIGN
|  2^    SUPERSCRIPT TWO [also s2]
|  3^    SUPERSCRIPT THREE [also s3]
|  ''    ACUTE ACCENT
|  /u    GREEK SMALL LETTER MU, MICRO SIGN [also *m] [BUT NOT mu!]
|  P!    PILCROW SIGN, PARAGRAPH [also pg]
|  .^    MIDDLE DOT [also ..]
|  ,,    CEDILLA
|  1^    SUPERSCRIPT ONE [also s1]
|  o_    MASCULINE ORDINAL INDICATOR [also s0, (sup 0)]
|  >>    RIGHT ANGLE QUOTATION MARK
|  14    VULGAR FRACTION ONE QUARTER
|  12    VULGAR FRACTION ONE HALF
|  34    VULGAR FRACTION THREE QUARTERS
|  ??    INVERTED QUESTION MARK
|  A`    LATIN CAPITAL LETTER A WITH GRAVE ACCENT
|  A'    LATIN CAPITAL LETTER A WITH ACUTE ACCENT
|  A^    LATIN CAPITAL LETTER A WITH CIRCUMFLEX ACCENT [also A>]
|  A~    LATIN CAPITAL LETTER A WITH TILDE [also A-]
|  A"    LATIN CAPITAL LETTER A WITH DIAERESIS
|  A*    LATIN CAPITAL LETTER A WITH RING ABOVE [also oA]
|  AE    CAPITAL DIPHTHONG AE
|  C,    LATIN CAPITAL LETTER C WITH CEDILLA
|  E`    LATIN CAPITAL LETTER E WITH GRAVE ACCENT 
|  E'    LATIN CAPITAL LETTER E WITH ACUTE ACCENT 
|  E^    LATIN CAPITAL LETTER E WITH CIRCUMFLEX ACCENT [also E>]
|  E"    LATIN CAPITAL LETTER E WITH DIAERESIS
|  I`    LATIN CAPITAL LETTER I WITH GRAVE ACCENT 
|  I'    LATIN CAPITAL LETTER I WITH ACUTE ACCENT 
|  I^    LATIN CAPITAL LETTER I WITH CIRCUMFLEX ACCENT [also I>]
|  I"    LATIN CAPITAL LETTER I WITH DIAERESIS
|  D-    CAPITAL ICELANDIC LETTER ETH
|  N~    LATIN CAPITAL LETTER N WITH TILDE [also N-]
|  O`    LATIN CAPITAL LETTER O WITH GRAVE ACCENT 
|  O'    LATIN CAPITAL LETTER O WITH ACUTE ACCENT 
|  O^    LATIN CAPITAL LETTER O WITH CIRCUMFLEX ACCENT [also O>]
|  O~    LATIN CAPITAL LETTER O WITH TILDE [also O-]
|  O"    LATIN CAPITAL LETTER O WITH DIAERESIS
|  xx    MULTIPLICATION SIGN [also mu]
|  O/    LATIN CAPITAL LETTER O WITH OBLIQUE STROKE
|  U`    LATIN CAPITAL LETTER U WITH GRAVE ACCENT 
|  U'    LATIN CAPITAL LETTER U WITH ACUTE ACCENT 
|  U^    LATIN CAPITAL LETTER U WITH CIRCUMFLEX [also U>]
|  U"    LATIN CAPITAL LETTER U WITH DIAERESIS
|  Y'    LATIN CAPITAL LETTER Y WITH ACUTE ACCEN
|  TH    CAPITAL ICELANDIC LETTER THORN [also |P]
|  ss    SMALL GERMAN LETTER SHARP s
|  a`    LATIN SMALL LETTER a WITH GRAVE ACCENT
|  a'    LATIN SMALL LETTER a WITH ACUTE ACCENT
|  a^    LATIN SMALL LETTER a WITH CIRCUMFLEX ACCENT [also a>]
|  a~    LATIN SMALL LETTER a WITH TILDE [also a-]
|  a"    LATIN SMALL LETTER a WITH DIAERESIS
|  a*    LATIN SMALL LETTER a WITH RING ABOVE [also oa]
|  ae    SMALL DIPHTHONG ae
|  c,    LATIN SMALL LETTER c WITH CEDILLA
|  e`    LATIN SMALL LETTER e WITH GRAVE ACCENT
|  e'    LATIN SMALL LETTER e WITH ACUTE ACCENT
|  e^    LATIN SMALL LETTER e WITH CIRCUMFLEX ACCENT [also e>]
|  e"    LATIN SMALL LETTER e WITH DIAERESIS
|  i`    LATIN SMALL LETTER i WITH GRAVE ACCENT
|  i'    LATIN SMALL LETTER i WITH ACUTE ACCENT
|  i^    LATIN SMALL LETTER i WITH CIRCUMFLEX ACCENT [also i>]
|  i"    LATIN SMALL LETTER i WITH DIAERESIS
|  d-    SMALL ICELANDIC LETTER ETH
|  n~    LATIN SMALL LETTER n WITH TILDE [also n-]
|  o`    LATIN SMALL LETTER o WITH GRAVE ACCENT
|  o'    LATIN SMALL LETTER o WITH ACUTE ACCENT
|  o^    LATIN SMALL LETTER o WITH CIRCUMFLEX ACCENT [also o>]
|  o~    LATIN SMALL LETTER o WITH TILDE [also o-]
|  o"    LATIN SMALL LETTER o WITH DIAERESIS
|  -:    DIVISION SIGN
|  o/    LATIN SMALL LETTER o WITH OBLIQUE STROKE
|  u`    LATIN SMALL LETTER u WITH GRAVE ACCENT
|  u'    LATIN SMALL LETTER u WITH ACUTE ACCENT
|  u^    LATIN SMALL LETTER u WITH CIRCUMFLEX ACCENT [also u>]
|  u"    LATIN SMALL LETTER u WITH DIAERESIS
|  y'    LATIN SMALL LETTER y WITH ACUTE ACCENT
|  th    SMALL ICELANDIC LETTER THORN [also |p]
|  y"    LATIN SMALL LETTER y WITH DIAERESIS


*)
