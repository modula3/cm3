(*--------------------------------------------------------------------------*)
MODULE StdFontsWin32 EXPORTS StdFonts;

IMPORT TextTextTbl, TextIntTbl;

BEGIN
  DefaultProportionalSpacedFamily := "Lucida Console";
  DefaultMonoSpacedFamily         := "Lucida Console";

  DefaultPointSizeTiny   := 10;
  DefaultPointSizeSmall  := 11;
  DefaultPointSizeMedium := 12;
  DefaultPointSizeLarge  := 13;
  DefaultPointSizeHuge   := 20;

  TinyFont          := "-*-Lucida Console-Normal-R-*-*-*-90-*-*-P-6-iso8859-ANSI";
  SmallFont         := "-*-Lucida Console-Normal-R-*-*-*-110-*-*-P-6-iso8859-ANSI";
  MediumFont        := "-*-Lucida Console-Normal-R-*-*-*-120-*-*-P-7-iso8859-ANSI";
  LargeFont         := "-*-Lucida Console-Normal-R-*-*-*-130-*-*-P-9-iso8859-ANSI";
  HugeFont          := "-*-Lucida Console-Normal-R-*-*-*-200-*-*-P-10-iso8859-ANSI";

  TinyItalicFont    := "-*-Lucida Console-Normal-I-*-*-*-90-*-*-P-6-iso8859-ANSI";
  SmallItalicFont   := "-*-Lucida Console-Normal-I-*-*-*-110-*-*-P-6-iso8859-ANSI";
  MediumItalicFont  := "-*-Lucida Console-Normal-I-*-*-*-120-*-*-P-7-iso8859-ANSI";
  LargeItalicFont   := "-*-Lucida Console-Normal-I-*-*-*-130-*-*-P-9-iso8859-ANSI";
  HugeItalicFont    := "-*-Lucida Console-Normal-I-*-*-*-200-*-*-P-10-iso8859-ANSI";

  TinyBoldFont      := "-*-Lucida Console-Bold-R-*-*-*-90-*-*-P-6-iso8859-ANSI";
  SmallBoldFont     := "-*-Lucida Console-Bold-R-*-*-*-110-*-*-P-6-iso8859-ANSI";
  MediumBoldFont    := "-*-Lucida Console-Bold-R-*-*-*-120-*-*-P-7-iso8859-ANSI";
  LargeBoldFont     := "-*-Lucida Console-Bold-R-*-*-*-130-*-*-P-9-iso8859-ANSI";
  HugeBoldFont      := "-*-Lucida Console-Bold-R-*-*-*-200-*-*-P-10-iso8859-ANSI";

  TinyMonoSpacedFont          := "-*-Lucida Console-Normal-R-*-*-*-90-*-*-M-6-iso8859-ANSI";
  SmallMonoSpacedFont         := "-*-Lucida Console-Normal-R-*-*-*-110-*-*-M-8-iso8859-ANSI";
  MediumMonoSpacedFont        := "-*-Lucida Console-Normal-R-*-*-*-120-*-*-M-9-iso8859-ANSI";
  LargeMonoSpacedFont         := "-*-Lucida Console-Normal-R-*-*-*-130-*-*-M-13-iso8859-ANSI";
  HugeMonoSpacedFont          := "-*-Lucida Console-Normal-R-*-*-*-200-*-*-M-15-iso8859-ANSI";

  TinyItalicMonoSpacedFont    := "-*-Lucida Console-Normal-I-*-*-*-90-*-*-M-6-iso8859-ANSI";
  SmallItalicMonoSpacedFont   := "-*-Lucida Console-Normal-I-*-*-*-110-*-*-M-8-iso8859-ANSI";
  MediumItalicMonoSpacedFont  := "-*-Lucida Console-Normal-I-*-*-*-120-*-*-M-9-iso8859-ANSI";
  LargeItalicMonoSpacedFont   := "-*-Lucida Console-Normal-I-*-*-*-130-*-*-M-13-iso8859-ANSI";
  HugeItalicMonoSpacedFont    := "-*-Lucida Console-Normal-I-*-*-*-200-*-*-M-15-iso8859-ANSI";

  TinyBoldMonoSpacedFont      := "-*-Lucida Console-Bold-R-*-*-*-90-*-*-M-6-iso8859-ANSI";
  SmallBoldMonoSpacedFont     := "-*-Lucida Console-Bold-R-*-*-*-110-*-*-M-8-iso8859-ANSI";
  MediumBoldMonoSpacedFont    := "-*-Lucida Console-Bold-R-*-*-*-120-*-*-M-9-iso8859-ANSI";
  LargeBoldMonoSpacedFont     := "-*-Lucida Console-Bold-R-*-*-*-130-*-*-M-13-iso8859-ANSI";
  HugeBoldMonoSpacedFont      := "-*-Lucida Console-Bold-R-*-*-*-200-*-*-M-15-iso8859-ANSI";

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
END StdFontsWin32.
