(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman, Mark Manasse and Greg Nelson           *)
(* Last modified on Fri Apr 30 13:47:12 PDT 1993 by mjordan  *)
(*      modified on Fri Apr  2 10:25:51 PST 1993 by steveg   *)
(*      modified on Fri Jan 29 12:21:33 PST 1993 by msm      *)
(*      modified on Mon Feb 24 13:58:03 PST 1992 by muller   *)
(*      modified on Sat Dec 21 16:36:41 PST 1991 by gnelson  *)
<*PRAGMA LL*>

(* A "ScrnFont.T" is a handle on a typeface that is valid for some
   particular screentype, called the {\it owner} of the handle.  All
   handles have names, which are highly conventionalized strings
   encoding the size, style, and other properties of the typeface. *)

INTERFACE ScrnFont;

IMPORT ScrnPixmap, Rect, TrestleComm, Font, Fingerprint;

EXCEPTION Failure;

(* \subsubsection{Obtaining handles from the oracle} *)

TYPE
  Oracle = Private OBJECT
    METHODS
      <* LL.sup <= VBT.mu *>
      list(pat: TEXT; maxResults := 1):
        REF ARRAY OF TEXT RAISES {TrestleComm.Failure};
      match(
        family: TEXT;
        pointSize: INTEGER := 120;
        slant: Slant := Slant.Roman;
        maxResults: CARDINAL := 1;
        weightName: TEXT := AnyMatch;
        version: TEXT := "";
        foundry: TEXT := AnyMatch;
        width: TEXT := AnyMatch;
        pixelsize: INTEGER := AnyValue;
        hres, vres: INTEGER := ScreenTypeResolution;
        spacing: Spacing := Spacing.Any;
        averageWidth: INTEGER := AnyValue;
        charsetRegistry: TEXT := "ISO8859";
        charsetEncoding: TEXT := "1")
      : REF ARRAY OF TEXT RAISES {TrestleComm.Failure};
      lookup(name: TEXT; useXft : BOOLEAN := TRUE): T
        RAISES {Failure, TrestleComm.Failure};
      builtIn(f: Font.Predefined): T;
    END;
    Private <: ROOT;

(* For a screentype "st", the field "st.font" is an "Oracle" that
   produces font handles owned by "st".

   The method call

| st.font.list(pat, maxResults)

   returns the names of all fonts owned by "st" that match the pattern
   "pat".  The list of results may be truncated to length "maxResults".
   A "*" matches any number of characters and a "?" matches a single
   character.

   The arguments to the "match" method specify various font attributes,
   as explained below.  The method call

| st.font.match(...)

   returns the names of all font handles owned by "st" that match the
   specifications.  The list of results may be truncated to the length
   "maxResults".  If no fonts match the specifications, the result will
   be either "NIL" or an empty array.  Passing "AnyMatch" for a text
   attribute, or "AnyValue" for an integer attribute, allows any value
   for that attribute.  For text attributes, partial text matches are
   also possible: a "*" matches any number of characters and "?" matches
   a single character.

   The method call

| st.font.lookup(name)

   returns the font handle owned by "st" with the given name.  Generally
   "name" should be one of the names returned by the "list" or
   "match" method.

   The method call

| st.font.builtIn(f)

   returns the screen-dependent font valid for "st" that corresponds
   to the predefined screen-independent font "Font.T{f}".

   The locking level for all methods is "LL.sup <= VBT.mu". *)

(* \subsubsection{Font attributes} *)

(* The arguments to a font oracle list method specify font attributes
   whose full specifications are the ``X Logical Font Description
   Conventions Version 1.3'', an MIT X Consortium Standard which can
   be found in Part IV of {\it X Window System} by Scheifler and Gettys
   \cite{XSpec}.  Here they are described in brief.

   The argument "family" specifies the family of the typeface.  To find
   out what fonts your X server has, run the "xlsfonts" program.  Most
   servers support the families "Courier", "Helvetica", and "Times",
   among others.

   The argument "pointsize" is ten times the font's size in
   points; e.g., 120 for a standard 12-point font.

   The argument "slant" is an element of the following enumeration
   type: *)

TYPE
  Slant = {Roman, Italic, Oblique, ReverseItalic,
    ReverseOblique, Other, Any};

(* whose elements have the following interpretations:

\medskip\nobulletitem
"Roman": Upright letters in a roman style.

\medskip\nobulletitem
"Italic": Clockwise slanted letters in an italic style.

\medskip\nobulletitem
"Oblique": Clockwise slanted letters in a roman style.

\medskip\nobulletitem
"ReverseItalic":  Counter clockwise slanted letters in an italic style.

\medskip\nobulletitem
"ReverseOblique": Counter clockwise slanted letters in a roman style.

\medskip\nobulletitem
"Other": None of the above

\medskip\nobulletitem
"Any": Any of the above (including "Other").

\medskip

The argument "weightName" is the foundry's name for the font's weight;
e.g.,  "Bold", "DemiBold", or "Medium".

The argument "version" specifies the version of the {\it X Logical
Font Description Conventions} that describes the format of a font's
name.  If the argument is omitted, Version 1.3 is assumed.  (Version
1.3 is the only version as these words are written.)

The argument "foundry" specifies the X registered name for the font's
foundry, e.g., "Adobe", "B&H", "Bitstream", "DEC".

The argument "width" specifies the foundry's name for the font's width; e.g., "Normal" or "Condensed".

The argument "pixelsize" specifies the size of the font in pixels.
The size in points depends on the vertical resolution of the device:
A pixelsize of 20 could represent a 20-point font at 75 pixels per
inch or a 10-point font at 150 pixels per inch.

The arguments "hres" and "vres" specify the horizontal and vertical
screen resolution for which the font is designed, in pixels per inch.

The argument "spacing" is an element of the following enumeration:
*)

TYPE Spacing =
  {Proportional, Monospaced, CharCell, Any};

(* whose elements have the following meaning:

\medskip\nobulletitem "Proportional": Character widths vary.

\medskip\nobulletitem "Monospaced": Character widths are constant.

\medskip\nobulletitem "CharCell": Font is self-clearing, as defined in
the "VBT" interface.

\medskip\nobulletitem "Any": Any of the above.

\medskip The argument "averageWidth" specifies the un-weighted
arithmetic mean of the widths of all glyphs in the font, measured in
tenths of a pixel.

The arguments "charsetRegistry" and "charsetEncoding" are the X names of the
font's character set and encoding scheme;  e.g., "ISO8859" and "1" for
ISO Latin-1 fonts.  See Appendix G of \cite{XSpec}. *)

CONST
  AnyMatch = "*";
  AnyValue = -1;
  ScreenTypeResolution = -2;

(* Passing "AnyMatch" as an argument to the "list" method matches any
   text value for the corresponding attribute, and "AnyValue" matches
   any integer value.  Passing "ScreenTypeResolution" for "hres" or
   "vres" matches fonts whose horizontal and vertical resolutions agree
   with the screentype that owns the font.  *)

(* \subsubsection{Registering fonts} *)

(* Some screentypes allow the client to register fonts.  The client registers
   the font's strike (bits) and metrics (description) with the "StrikeOracle".
   The name of the font is implied by the attributes in the metrics, so
   the "list" and "lookup" methods will find client-registered fonts.  *)

TYPE
  StrikeOracle = Oracle OBJECT
    METHODS
      <* LL.sup <= VBT.mu *>
      load(strike: Strike; metrics: Metrics): T
        RAISES {Failure, TrestleComm.Failure};
    END;

(* The method call "st.font.load(strike, metrics)" creates a font
   owned by "st" with the given strike and metrics and returns a handle
   to it.

   The "metrics" argument must define all of the initial fields of the
   font metrics record: "family", "pointSize", ..., "isAscii", and
   "defaultChar".  The values "minBounds" and "maxBounds" must be provided
   if "charMetrics" is "NIL"; otherwise if "printWidth" is "AnyValue", the
   "load" method will compute them from "charMetrics".  If any of
   the remaining fields have the value "AnyValue", the "load" method
   will compute them. *)

(* \subsubsection{The handle object}  *)

TYPE
  T <: Public;
  Public = OBJECT (*CONST*)
    id: INTEGER;
    metrics: Metrics;
    xftFont : ADDRESS;
  END;

TYPE StrikeFont = T OBJECT
  METHODS <* LL.sup <= VBT.mu *>
    strike(): Strike RAISES {TrestleComm.Failure}
  END;

TYPE Strike = OBJECT
  METHODS <* LL.sup <= VBT.mu *>
    glyph(ch: INTEGER): ScrnPixmap.T;
  END;

(* If "f" is a "ScrnFont.T", then "f.id" is an identifier whose
   interpretation depends on the screentype that owns "f" and
   "f.metrics" are the metrics for "f".  If in addition "f" is a
   "StrikeFont", then "f.strike()" returns "f"'s strike.  The screentype
   of the strike's pixmaps will be the screentype that owns "f".

   If "str" is a "Strike", then "str.glyph(ch)" is the pixmap for
   the character "ch".  This will be empty except for characters in
   the range "[m.firstChar..m.lastChar]", where "m" is the metrics (see
   below) for the font of which "str" is the strike.  *)

PROCEDURE BoundingBox(txt: TEXT; fnt: T): Rect.T;
<* LL arbitrary *>
(* Return the smallest rectangle that contains the bounding boxes
   of the characters of "txt" if "txt" were painted in the font "fnt" with
   "txt"'s reference point at the origin. *)

PROCEDURE BoundingBoxSub(
  READONLY txt: ARRAY OF CHAR;
  fnt: T): Rect.T;
<* LL arbitrary *>
(* Like "BoundingBox" but takes an array instead of a "TEXT". *)

PROCEDURE BoundingBoxSubValid(
  READONLY txt: ARRAY OF CHAR;
  fnt: T; VAR (*OUT*) valid: BOOLEAN): Rect.T;
<* LL arbitrary *>
(* Like "BoundingBoxSub" but indicates if all characters in "txt"
   are valid.  "valid" may be set to "FALSE" even if all characters
   are valid, if the text wasn't checked. *)

PROCEDURE TextWidth(txt: TEXT; fnt: T): INTEGER;
<* LL arbitrary *>
(* Return the sum of the printing widths of the characters in "txt"
   in the font "fnt". *)

(* \subsubsection{The raw representation} *)

TYPE
  CharMetric = RECORD
    printWidth: INTEGER;
    boundingBox: Rect.T;
  END;
  CharMetrics = REF ARRAY OF CharMetric;

(* The "printWidth" of a character is the displacement to the next
   character's reference point.

   The "boundingBox" of a character is the smallest rectangle with sides
   parallel to the axes that contains the glyph of the character placed
   with its reference point at (0,0).  *)

TYPE
  Metrics = OBJECT (*CONST*)
    family: TEXT;
    pointSize: INTEGER;
    slant: Slant;
    weightName: TEXT;
    version: TEXT;
    foundry: TEXT;
    width: TEXT;
    pixelsize: INTEGER;
    hres, vres: INTEGER;
    spacing: Spacing;
    averageWidth: INTEGER;
    charsetRegistry: TEXT;
    charsetEncoding: TEXT;
    firstChar, lastChar: INTEGER;
    charMetrics: CharMetrics;
    selfClearing: BOOLEAN;
    rightKerning, leftKerning: BOOLEAN;
    isAscii: BOOLEAN;
    defaultChar: INTEGER;
    minBounds, maxBounds: CharMetric;
    ascent, descent := 0;
    fprint := Fingerprint.Zero;
  METHODS <* LL arbitrary *>
    intProp(name: TEXT; ch: INTEGER := -1): INTEGER
      RAISES {Failure};
    textProp(name: TEXT; ch: INTEGER := -1): TEXT
      RAISES {Failure};
  END;

(* The fields from "family" to "charSetEncoding" in the "Metrics" object
   specify the attributes that were defined for the "lookup" method.
   A value of "*" or "Any" in one of these fields means that the
   corresponding attribute is unknown.

   The integers "firstChar" and "lastChar" are the indices of the
   first and last characters defined in the font.

   The array "charMetrics" specifies the metrics of the
   individual characters.  The metrics for character "ch"
   are in "charMetrics[ch-firstChar]".  If all characters have
   the same "printWidth" and "boundingBox", then these values
   are stored in "minBounds" and "maxBounds" and the "charMetrics"
   field is "NIL".

   The flag "selfClearing" indicates whether the font is self-clearing,
   as defined in the "VBT" interface, and the two kerning flags indicate
   the present of right and left kerning in the font.

   The flag "isAscii" indicates that character codes 32-126 (base 10)
   have their normal ASCII meanings.

   The integer "defaultChar" is the code for the recommended character
   to display in the place of a character that isn't defined for the
   font.

   The rectangles "minBounds.boundingBox" and "maxBounds.boundingBox"
   contain the meet and join, respectively, of the bounding boxes of
   all characters in the font when they are positioned with their
   reference points at (0, 0). The values "minBounds.printWidth" and
   "maxBounds.printWidth" are the minimum and maximum printing widths
   for all characters in the font.

   The method call "m.intProp(nm)" returns the integer value of the
   font attribute named "nm", or raises "Failure" if this attribute is
   not defined for "m".  The method call "m.intProp(nm, ORD(ch))"
   returns the integer value of the font attribute named "nm" for the
   character "ch", or raises "Failure" if this attribute is not defined
   for "(m, ch)".  The "textProp" method is similar.

   The set of attributes returned by the metrics methods depend on the
   font.  Fonts that are owned by X screentypes support the attributes
   defined in Part IV of {\it X Window System} ({\it op. cit.}); we
   recommend that other fonts support them too.  (To read an X font
   attribute whose type is an X atom, use the "textProp" method, which
   returns the name of the atom.)

   The fprint field, if non-zero, holds a fingerprint for the metrics,
   which can be used to quickly determine if two Metrics are the same.

   The ascent and descent fields contain the recommended spacing in pixels
   above and below the baseline; the sum is the recommended base-line skip.
*)

END ScrnFont.
