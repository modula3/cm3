(* Copyright (C) 1995, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Wed Jan 17 14:37:02 PST 1996 by najork                   *)
(*      modified on Wed Oct 18 10:01:43 PDT 1995 by mhb                      *)
<* PRAGMA LL *>

(* An "HTMLVBTG" is a VBT class for diplaying HTML using a tree of VBTs. *)

INTERFACE HTMLVBTG;

IMPORT HTML, HTMLVBT, RefList, Thread, Web, Font, PaintOp, Pixmap, PixmapVBT,
       TextVBT, VBT;

TYPE
  Info = OBJECT
           url: TEXT;
         METHODS
           <* LL<VBT.mu *>
           load (page: Web.Page) RAISES {Thread.Alerted};
         END;

  ImageInfo <: PublicImageInfo;
  PublicImageInfo = Info OBJECT align: HTML.Alignment;  END;

  ObletInfo <: PublicObletInfo;
  PublicObletInfo = Info OBJECT END;

  RigidTextVBT <: TextVBT.T;

  RigidPixmapVBT <: PixmapVBT.T;

  T <: Private;

  Private <: Public;

  Public = HTMLVBT.T OBJECT
           METHODS
             <* LL <= VBT.mu *>
             init (    html      : HTML.T;
                       useAlt    : BOOLEAN;
                       useZippers: BOOLEAN;
                   VAR list      : RefList.T;
                       scrollBar : BOOLEAN): T;
           END;

PROCEDURE EnterHMode (v: T; parent: VBT.Split);

(************************************************)
(**********  Look and feel parameters  **********)
(************************************************)

(* All sizes are specified in points. *)

(********** colors **********)

CONST
  BackgroundColor = "LightGray";
  RegularColor    = "Black";
  AnchorColor     = "Blue";
  HighlightColor  = "Red";
  FollowedColor   = "Purple";
  IsIndexBgColor  = "Pink";
  ErrorColor      = "Red";
 
  
(********** spacing **********)

CONST
  ShadowAmt     = 0.5;
  PageMarginAmt = 10.0;
  ParSkipAmt    = 10.0;
  IndentAmt     = 15.0;


(********** fonts **********)

TYPE 
  FontFamily = {Normal, Fixed}; 
  FontWeight = {Normal, Bold};
  FontSlant  = {Normal, Slanted};
  FontSize   = {Huge, LARGE, Large, Normal, Small, Tiny};
  FontStyle  = {Plain, Bold, Slanted, BoldSlanted};
  
CONST
  NormalFontNames = ARRAY FontStyle OF TEXT{
                            "times_roman", 
                            "times_bold", 
                            "times_italic",
                            "times_bolditalic"};

  NormalFontRegularSizes = ARRAY FontSize OF TEXT {"24", "18", "14", "12", "10", "8"};
  NormalFontBigSizes     = ARRAY FontSize OF TEXT {"36", "24", "18", "18", "14", "12"};
  NormalFontSizes        = NormalFontRegularSizes;


  FixedFontNames = ARRAY FontStyle OF TEXT{
                            "courier", 
                            "courier_bold", 
                            "courier_oblique",
                            "courier_boldoblique"};

  FixedFontRegularSizes = ARRAY FontSize OF TEXT {"24", "18", "14", "12", "10", "8"};
  FixedFontBigSizes     = ARRAY FontSize OF TEXT {"36", "24", "18", "18", "14", "12"};
  FixedFontSizes        = FixedFontRegularSizes;


(********** horizontal rules **********)

CONST
  HRAmt         = 1.0;
  HRPreSkipAmt  = 3.0;
  HRPostSkipAmt = 3.0;


(********** headings **********)

TYPE 
  HeadingInfo = RECORD 
    preGlue: REAL;
    postGlue: REAL; 
    fontSize: FontSize;
  END;

  State = RECORD
    family: FontFamily;
    size  : FontSize;
    weight: FontWeight;
    slant : FontSlant;
    font  : Font.T;
    bgFg  : PaintOp.ColorQuad;
  END;

CONST 
  headingInfo = ARRAY[1..6] OF HeadingInfo {
    (* H1 *) HeadingInfo{10.0, 10.0, FontSize.Huge},
    (* H2 *) HeadingInfo{ 8.0,  8.0, FontSize.LARGE},
    (* H3 *) HeadingInfo{ 6.0,  6.0, FontSize.Large},
    (* H4 *) HeadingInfo{ 4.0,  4.0, FontSize.Normal},
    (* H5 *) HeadingInfo{ 4.0,  4.0, FontSize.Small},
    (* H6 *) HeadingInfo{ 2.0,  2.0, FontSize.Tiny}};

VAR (* CONST *)
  EmptyImage: Pixmap.T;
  ErrorImage: Pixmap.T;
  RegularColors  : PaintOp.ColorQuad;
  RegularBgColors: PaintOp.ColorQuad;
  AnchorColors   : PaintOp.ColorQuad;
  ErrorColors    : PaintOp.ColorQuad;

END HTMLVBTG.



