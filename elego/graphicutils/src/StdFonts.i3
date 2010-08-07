(*--------------------------------------------------------------------------*)
INTERFACE StdFonts;

IMPORT TextTextTbl, TextIntTbl;

VAR (* CONST, initialized in implementation module according to current
       platform.
    *)

  DefaultProportionalSpacedFamily : TEXT;
  DefaultMonoSpacedFamily         : TEXT;

  DefaultPointSizeTiny   : CARDINAL;
  DefaultPointSizeSmall  : CARDINAL;
  DefaultPointSizeMedium : CARDINAL;
  DefaultPointSizeLarge  : CARDINAL;
  DefaultPointSizeHuge   : CARDINAL;

  TinyFont          : TEXT;
  SmallFont         : TEXT;
  MediumFont        : TEXT;
  LargeFont         : TEXT;
  HugeFont          : TEXT;

  TinyItalicFont    : TEXT;
  SmallItalicFont   : TEXT;
  MediumItalicFont  : TEXT;
  LargeItalicFont   : TEXT;
  HugeItalicFont    : TEXT;

  TinyBoldFont      : TEXT;
  SmallBoldFont     : TEXT;
  MediumBoldFont    : TEXT;
  LargeBoldFont     : TEXT;
  HugeBoldFont      : TEXT;

  TinyMonoSpacedFont          : TEXT;
  SmallMonoSpacedFont         : TEXT;
  MediumMonoSpacedFont        : TEXT;
  LargeMonoSpacedFont         : TEXT;
  HugeMonoSpacedFont          : TEXT;

  TinyItalicMonoSpacedFont    : TEXT;
  SmallItalicMonoSpacedFont   : TEXT;
  MediumItalicMonoSpacedFont  : TEXT;
  LargeItalicMonoSpacedFont   : TEXT;
  HugeItalicMonoSpacedFont    : TEXT;

  TinyBoldMonoSpacedFont      : TEXT;
  SmallBoldMonoSpacedFont     : TEXT;
  MediumBoldMonoSpacedFont    : TEXT;
  LargeBoldMonoSpacedFont     : TEXT;
  HugeBoldMonoSpacedFont      : TEXT;

  FontTbl : TextTextTbl.T;
  SizeTbl : TextIntTbl.T;
END StdFonts.
