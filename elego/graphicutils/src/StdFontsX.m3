(*--------------------------------------------------------------------------*)
MODULE StdFontsX EXPORTS StdFonts;

IMPORT TextTextTbl, TextIntTbl;

BEGIN
  DefaultProportionalSpacedFamily := "helvetica";
  DefaultMonoSpacedFamily         := "courier";

  DefaultPointSizeTiny   := 10;
  DefaultPointSizeSmall  := 12;
  DefaultPointSizeMedium := 14;
  DefaultPointSizeLarge  := 18;
  DefaultPointSizeHuge   := 24;

  TinyFont          := "-*-helvetica-medium-r-normal-*-10-*-*-*-*-*-iso8859-1";
  SmallFont         := "-*-helvetica-medium-r-normal-*-12-*-*-*-*-*-iso8859-1";
  MediumFont        := "-*-helvetica-medium-r-normal-*-14-*-*-*-*-*-iso8859-1";
  LargeFont         := "-*-helvetica-medium-r-normal-*-18-*-*-*-*-*-iso8859-1";
  HugeFont          := "-*-helvetica-medium-r-normal-*-24-*-*-*-*-*-iso8859-1";

  TinyItalicFont    := "-*-helvetica-medium-i-normal-*-10-*-*-*-*-*-iso8859-1";
  SmallItalicFont   := "-*-helvetica-medium-i-normal-*-12-*-*-*-*-*-iso8859-1";
  MediumItalicFont  := "-*-helvetica-medium-i-normal-*-14-*-*-*-*-*-iso8859-1";
  LargeItalicFont   := "-*-helvetica-medium-i-normal-*-18-*-*-*-*-*-iso8859-1";
  HugeItalicFont    := "-*-helvetica-medium-i-normal-*-24-*-*-*-*-*-iso8859-1";

  TinyBoldFont      := "-*-helvetica-bold-r-normal-*-10-*-*-*-*-*-iso8859-1";
  SmallBoldFont     := "-*-helvetica-bold-r-normal-*-12-*-*-*-*-*-iso8859-1";
  MediumBoldFont    := "-*-helvetica-bold-r-normal-*-14-*-*-*-*-*-iso8859-1";
  LargeBoldFont     := "-*-helvetica-bold-r-normal-*-18-*-*-*-*-*-iso8859-1";
  HugeBoldFont      := "-*-helvetica-bold-r-normal-*-24-*-*-*-*-*-iso8859-1";

  TinyMonoSpacedFont          := "-*-courier-medium-r-normal-*-10-*-*-*-*-*-iso8859-1";
  SmallMonoSpacedFont         := "-*-courier-medium-r-normal-*-12-*-*-*-*-*-iso8859-1";
  MediumMonoSpacedFont        := "-*-courier-medium-r-normal-*-14-*-*-*-*-*-iso8859-1";
  LargeMonoSpacedFont         := "-*-courier-medium-r-normal-*-18-*-*-*-*-*-iso8859-1";
  HugeMonoSpacedFont          := "-*-courier-medium-r-normal-*-24-*-*-*-*-*-iso8859-1";

  TinyItalicMonoSpacedFont    := "-*-courier-medium-i-normal-*-10-*-*-*-*-*-iso8859-1";
  SmallItalicMonoSpacedFont   := "-*-courier-medium-i-normal-*-12-*-*-*-*-*-iso8859-1";
  MediumItalicMonoSpacedFont  := "-*-courier-medium-i-normal-*-14-*-*-*-*-*-iso8859-1";
  LargeItalicMonoSpacedFont   := "-*-courier-medium-i-normal-*-18-*-*-*-*-*-iso8859-1";
  HugeItalicMonoSpacedFont    := "-*-courier-medium-i-normal-*-24-*-*-*-*-*-iso8859-1";

  TinyBoldMonoSpacedFont      := "-*-courier-bold-i-normal-*-10-*-*-*-*-*-iso8859-1";
  SmallBoldMonoSpacedFont     := "-*-courier-bold-i-normal-*-12-*-*-*-*-*-iso8859-1";
  MediumBoldMonoSpacedFont    := "-*-courier-bold-i-normal-*-14-*-*-*-*-*-iso8859-1";
  LargeBoldMonoSpacedFont     := "-*-courier-bold-i-normal-*-18-*-*-*-*-*-iso8859-1";
  HugeBoldMonoSpacedFont      := "-*-courier-bold-i-normal-*-24-*-*-*-*-*-iso8859-1";

  FontTbl := NEW(TextTextTbl.Default).init();
  SizeTbl := NEW(TextIntTbl.Default).init();

  EVAL FontTbl.put("DefaultProportionalSpacedFamily", 
                   DefaultProportionalSpacedFamily);
  EVAL FontTbl.put("DefaultMonoSpacedFamily", DefaultMonoSpacedFamily);

  EVAL FontTbl.put("TinyFont", TinyFont);
  EVAL FontTbl.put("SmallFont", SmallFont);
  EVAL FontTbl.put("MediumFont", MediumFont);
  EVAL FontTbl.put("LargeFont", LargeFont);
  EVAL FontTbl.put("HugeFont", HugeFont);

  EVAL FontTbl.put("TinyItalicFont", TinyItalicFont);
  EVAL FontTbl.put("SmallItalicFont", SmallItalicFont);
  EVAL FontTbl.put("MediumItalicFont", MediumItalicFont);
  EVAL FontTbl.put("LargeItalicFont", LargeItalicFont);
  EVAL FontTbl.put("HugeItalicFont", HugeItalicFont);

  EVAL FontTbl.put("TinyBoldFont", TinyBoldFont);
  EVAL FontTbl.put("SmallBoldFont", SmallBoldFont);
  EVAL FontTbl.put("MediumBoldFont", MediumBoldFont);
  EVAL FontTbl.put("LargeBoldFont", LargeBoldFont);
  EVAL FontTbl.put("HugeBoldFont", HugeBoldFont);

  EVAL FontTbl.put("TinyMonoSpacedFont", TinyMonoSpacedFont);
  EVAL FontTbl.put("SmallMonoSpacedFont", SmallMonoSpacedFont);
  EVAL FontTbl.put("MediumMonoSpacedFont", MediumMonoSpacedFont);
  EVAL FontTbl.put("LargeMonoSpacedFont", LargeMonoSpacedFont);
  EVAL FontTbl.put("HugeMonoSpacedFont", HugeMonoSpacedFont);

  EVAL FontTbl.put("TinyItalicMonoSpacedFont", TinyItalicMonoSpacedFont);
  EVAL FontTbl.put("SmallItalicMonoSpacedFont", SmallItalicMonoSpacedFont);
  EVAL FontTbl.put("MediumItalicMonoSpacedFont", MediumItalicMonoSpacedFont);
  EVAL FontTbl.put("LargeItalicMonoSpacedFont", LargeItalicMonoSpacedFont);
  EVAL FontTbl.put("HugeItalicMonoSpacedFont", HugeItalicMonoSpacedFont);

  EVAL FontTbl.put("TinyBoldMonoSpacedFont", TinyBoldMonoSpacedFont);
  EVAL FontTbl.put("SmallBoldMonoSpacedFont", SmallBoldMonoSpacedFont);
  EVAL FontTbl.put("MediumBoldMonoSpacedFont", MediumBoldMonoSpacedFont);
  EVAL FontTbl.put("LargeBoldMonoSpacedFont", LargeBoldMonoSpacedFont);
  EVAL FontTbl.put("HugeBoldMonoSpacedFont", HugeBoldMonoSpacedFont);

  EVAL SizeTbl.put("DefaultPointSizeTiny", DefaultPointSizeTiny);
  EVAL SizeTbl.put("DefaultPointSizeSmall", DefaultPointSizeSmall);
  EVAL SizeTbl.put("DefaultPointSizeMedium", DefaultPointSizeMedium);
  EVAL SizeTbl.put("DefaultPointSizeLarge", DefaultPointSizeLarge);
  EVAL SizeTbl.put("DefaultPointSizeHuge", DefaultPointSizeHuge);
END StdFontsX.
