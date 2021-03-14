UNSAFE INTERFACE Xft;

(*==============================================================*)
(* The X11 R4 Interface for Modula 3                            *)
(*                                                              *)
(* contains: /usr/include/X11/Xft/Xft.h                         *)
(* /usr/include/X11/Xft/XftCompat.h ??                          *)
(*==============================================================*)


FROM Ctypes IMPORT char, char_star, int, int_star, short,
                   unsigned_short, unsigned_char, unsigned_int,
                   unsigned_long;

FROM X IMPORT DisplayStar, Drawable, Pixmap, VisualStar, Colormap, Region,
              XRectangleStar, XID;

TYPE
  Int = int;
  Short = short;
  Char = char;
  String = char_star;

CONST
  XFT_CORE             = "core";
  XFT_RENDER           = "render";
  XFT_XLFD             = "xlfd";
  XFT_MAX_GLYPH_MEMORY = "maxglyphmemory";
  XFT_MAX_UNREF_FONTS  = "maxunreffonts";

CONST
(* from fontcofig.h for spacing metric *)
  FC_PROPORTIONAL  =           0;
  FC_DUAL          =          90;
  FC_MONO          =          100;
  FC_CHARCELL      =          110;

CONST
(* from fontcofig.h for slant metric *)
  FC_SLANT_ROMAN   =          0;
  FC_SLANT_ITALIC  =          100;
  FC_SLANT_OBLIQUE =          110;

TYPE Picture = XID;

TYPE
  (* from X11/extensions/Xrender.h *)
  XRenderColor = RECORD red, green, blue, alpha: unsigned_short;  END;

  RenderColorStar = REF XRenderColor;

  XGlyphInfo = RECORD
                 width : unsigned_short;
                 height: unsigned_short;
                 x     : short;
                 y     : short;
                 xOff  : short;
                 yOff  : short;
               END;

  XGlyphInfoStar = REF XGlyphInfo;

TYPE
  XftFont = RECORD
              ascent           : Int;
              descent          : Int;
              height           : Int;
              max_advance_width: Int;
              charset          : ADDRESS;
              pattern          : FcPatternStar;
            END;

  XftFontStar = REF XftFont;

  XftColor = RECORD
               pixel: unsigned_long;
               color: XRenderColor;
             END;

  XftColorStar = REF XftColor;

TYPE
  (* Fontconfig constants *)
  FcChar8 = unsigned_char;
  FcChar16 = unsigned_short;
  FcChar32 = unsigned_int;

  FcEndian = {Big, Little};

  (* Freetype constants *)
  FT_UInt = unsigned_int;

  XftCharSpec = RECORD
                  ucs4: FcChar32;
                  x   : Short;
                  y   : Short;
                END;

  XftCharSpecStar = REF XftCharSpec;

  XftCharFontSpec = RECORD
                      font: XftFontStar;
                      ucs4: FcChar32;
                      x   : Short;
                      y   : Short;
                    END;

  XftCharFontSpecStar = REF XftCharFontSpec;

  XftGlyphSpec = RECORD
                   glyph: FT_UInt;
                   x    : Short;
                   y    : Short;
                 END;

  XftGlyphSpecStar = REF XftGlyphSpec;

  XftGlyphFontSpec = RECORD
                       font : XftFontStar;
                       glyph: FT_UInt;
                       x    : Short;
                       y    : Short;
                     END;

  XftGlyphFontSpecStar = REF XftGlyphFontSpec;

(* xftcolor.c *)

<* EXTERNAL XftColorAllocName *>
PROCEDURE ColorAllocName (dpy   : DisplayStar;
                          visual: VisualStar;
                          cmap  : Colormap;
                          name  : String;
                          result: XftColorStar ): BOOLEAN;


<* EXTERNAL XftColorAllocValue *>
PROCEDURE ColorAllocValue (dpy   : DisplayStar;
                           visual: VisualStar;
                           cmap  : Colormap;
                           color : RenderColorStar;
                           result: XftColorStar     ): BOOLEAN;

<* EXTERNAL XftColorFree *>
PROCEDURE ColorFree (dpy   : DisplayStar;
                     visual: VisualStar;
                     cmap  : Colormap;
                     color : XftColorStar );

(* xftdpy.c *)

TYPE

  FcPatternStar = ADDRESS;

<* EXTERNAL XftDefaultHasRender *>
PROCEDURE DefaultHasRender (dpy: DisplayStar): BOOLEAN;

<* EXTERNAL XftDefaultSet *>
PROCEDURE DefaultSet (dpy: DisplayStar; defaults: FcPatternStar): BOOLEAN;

<* EXTERNAL XftDefaultSubstitute *>
PROCEDURE DefaultSubstitute
  (dpy: DisplayStar; screen: Int; pattern: FcPatternStar);

(* xftdraw.c *)

TYPE
  DrawStar = ADDRESS;

<* EXTERNAL XftDrawCreate *>
PROCEDURE DrawCreate (dpy     : DisplayStar;
                      drawable: Drawable;
                      visual  : VisualStar;
                      colormap: Colormap     ): DrawStar;

<* EXTERNAL XftDrawCreateBitmap *>
PROCEDURE DrawCreateBitmap (dpy: DisplayStar; bitmap: Pixmap): DrawStar;

<* EXTERNAL XftDrawCreateAlpha *>
PROCEDURE DrawCreateAlpha (dpy: DisplayStar; pixmap: Pixmap; depth: Int):
  DrawStar;

<* EXTERNAL XftDrawChange *>
PROCEDURE DrawChange (draw: DrawStar; drawable: Drawable);

<* EXTERNAL XftDrawDisplay *>
PROCEDURE DrawDisplay (draw: DrawStar): DisplayStar;

<* EXTERNAL XftDrawDrawable *>
PROCEDURE DrawDrawable (draw: DrawStar): Drawable;

<* EXTERNAL XftDrawColormap *>
PROCEDURE DrawColormap (draw: DrawStar): Colormap;

<* EXTERNAL XftDrawVisual *>
PROCEDURE DrawVisual (draw: DrawStar): VisualStar;

<* EXTERNAL XftDrawDestroy *>
PROCEDURE DrawDestroy (draw: DrawStar);

<* EXTERNAL XftDrawPicture *>
PROCEDURE DrawPicture (draw: DrawStar): Picture;

<* EXTERNAL XftDrawSrcPicture *>
PROCEDURE DrawSrcPicture (draw: DrawStar; color: XftColorStar): Picture;

TYPE

  FT_UIntStar = ADDRESS;
  FcChar8Star = ADDRESS;
  FcChar16Star = ADDRESS;
  FcChar32Star = ADDRESS;

<* EXTERNAL XftDrawGlyphs *>
PROCEDURE DrawGlyphs (draw   : DrawStar;
                      color  : XftColorStar;
                      pub    : XftFontStar;
                      x      : Int;
                      y      : Int;
                      glyphs : FT_UIntStar;
                      nglyphs: Int           );

<* EXTERNAL XftDrawString8 *>
PROCEDURE DrawString8 (draw  : DrawStar;
                       color : XftColorStar;
                       pub   : XftFontStar;
                       x     : Int;
                       y     : Int;
                       string: FcChar8Star;
                       len   : Int           );

<* EXTERNAL XftDrawString16 *>
PROCEDURE DrawString16 (draw  : DrawStar;
                        color : XftColorStar;
                        pub   : XftFontStar;
                        x     : Int;
                        y     : Int;
                        string: FcChar16Star;
                        len   : Int           );

<* EXTERNAL XftDrawString32 *>
PROCEDURE DrawString32 (draw  : DrawStar;
                        color : XftColorStar;
                        pub   : XftFontStar;
                        x     : Int;
                        y     : Int;
                        string: FcChar32Star;
                        len   : Int           );

<* EXTERNAL XftDrawStringUtf8 *>
PROCEDURE DrawStringUtf8 (draw  : DrawStar;
                          color : XftColorStar;
                          pub   : XftFontStar;
                          x     : Int;
                          y     : Int;
                          string: FcChar8Star;
                          len   : Int           );

<* EXTERNAL XftDrawStringUtf16 *>
PROCEDURE DrawStringUtf16 (draw  : DrawStar;
                           color : XftColorStar;
                           pub   : XftFontStar;
                           x     : Int;
                           y     : Int;
                           string: FcChar8Star;
                           len   : Int           );

<* EXTERNAL XftDrawCharSpec *>
PROCEDURE DrawCharSpec (draw : DrawStar;
                        color: XftColorStar;
                        pub  : XftFontStar;
                        chars: XftCharSpecStar;
                        len  : Int              );

<* EXTERNAL XftDrawCharFontSpec *>
PROCEDURE DrawCharFontSpec (draw : DrawStar;
                            color: XftColorStar;
                            chars: XftCharFontSpecStar;
                            len  : Int                  );

<* EXTERNAL XftDrawGlyphSpec *>
PROCEDURE DrawGlyphSpec (draw  : DrawStar;
                         color : XftColorStar;
                         pub   : XftFontStar;
                         glyphs: XftGlyphSpecStar;
                         len   : Int               );

<* EXTERNAL XftDrawGlyphFontSpec *>
PROCEDURE DrawGlyphFontSpec (draw  : DrawStar;
                             color : XftColorStar;
                             glyphs: XftGlyphFontSpecStar;
                             len   : Int                   );

<* EXTERNAL XftDrawRect *>
PROCEDURE DrawRect (draw  : DrawStar;
                    color : XftColorStar;
                    x     : Int;
                    y     : Int;
                    width : unsigned_int;
                    height: unsigned_int  );

<* EXTERNAL XftDrawSetClip *>
PROCEDURE DrawSetClip (draw: DrawStar; r: Region): BOOLEAN;

<* EXTERNAL XftDrawSetClipRectangles *>
PROCEDURE DrawSetClipRectangles (draw   : DrawStar;
                                 xOrigin: Int;
                                 yOrigin: Int;
                                 rects  : XRectangleStar;
                                 n      : Int             );

<* EXTERNAL XftDrawSetSubwindowMode *>
PROCEDURE DrawSetSubwindowMode (draw: DrawStar; mode: Int);

(* xftextent.c *)

<* EXTERNAL XftGlyphExtents *>
PROCEDURE GlyphExtents (dpy    : DisplayStar;
                        pub    : XftFontStar;
                        VAR glyphs : FT_UInt;
                        nglyphs: Int;
                        extents: XGlyphInfoStar);

<* EXTERNAL XftGlyphExtents8 *>
PROCEDURE GlyphExtents8 (dpy    : DisplayStar;
                         pub    : XftFontStar;
                         string : FcChar8Star;
                         len    : Int;
                         extents: XGlyphInfoStar);

<* EXTERNAL XftGlyphExtents16 *>
PROCEDURE GlyphExtents16 (dpy    : DisplayStar;
                          pub    : XftFontStar;
                          string : FcChar16Star;
                          len    : Int;
                          extents: XGlyphInfoStar);

<* EXTERNAL XftGlyphExtents32 *>
PROCEDURE GlyphExtents32 (dpy    : DisplayStar;
                          pub    : XftFontStar;
                          string : FcChar32Star;
                          len    : Int;
                          extents: XGlyphInfoStar);

<* EXTERNAL XftTextExtentsUtf8 *>
PROCEDURE TextExtentsUtf8 (dpy    : DisplayStar;
                           pub    : XftFontStar;
                           string : FcChar8Star;
                           len    : Int;
                           extents: XGlyphInfoStar);

<* EXTERNAL XftTextExtentsUtf16 *>
PROCEDURE TextExtentsUtf16 (dpy    : DisplayStar;
                            pub    : XftFontStar;
                            string : FcChar8Star;
                            endian : FcEndian;
                            len    : Int;
                            extents: XGlyphInfoStar);

(* xftfont.c *)

TYPE
  FcResult = {FcResultMatch, FcResultNoMatch, FcResultTypeMismatch,
              FcResultNoId, FcResultOutOfMemory};

  FcResultStar = REF FcResult;

<* EXTERNAL XftFontMatch *>
PROCEDURE FontMatch (dpy    : DisplayStar;
                     screen : Int;
                     pattern: FcPatternStar;
                     result : FcResultStar   ): FcPatternStar;

(*
There is no valist version of this function so we cannot
map it.
XftFont *
XftFontOpen (Display *dpy, int screen, ...) _X_SENTINEL(0);
*)

<* EXTERNAL XftFontOpenName *>
PROCEDURE FontOpenName (dpy: DisplayStar; screen: Int; name: String):
  XftFontStar;

<* EXTERNAL XftFontOpenXlfd *>
PROCEDURE FontOpenXlfd (dpy: DisplayStar; screen: Int; xlfd: String):
  XftFontStar;

(* xftfreetype.c *)

TYPE FT_Face = ADDRESS;

<* EXTERNAL XftLockFace *>
PROCEDURE LockFace (pub: XftFontStar): FT_Face;

<* EXTERNAL XftUnlockFace *>
PROCEDURE UnlockFace (pub: XftFontStar);

TYPE
  XftFontInfo = ADDRESS;

<* EXTERNAL XftFontInfoCreate *>
PROCEDURE FontInfoCreate (dpy: DisplayStar; pattern: FcPatternStar):
  XftFontInfo;

<* EXTERNAL XftFontInfoDestroy *>
PROCEDURE FontInfoDestroy (dpy: DisplayStar; fi: XftFontInfo);

<* EXTERNAL XftFontInfoHash *>
PROCEDURE FontInfoHash (fi: XftFontInfo): FcChar32;


<* EXTERNAL XftFontInfoEqual *>
PROCEDURE FontInfoEqual (a: XftFontInfo; b: XftFontInfo): BOOLEAN;

<* EXTERNAL XftFontOpenInfo *>
PROCEDURE FontOpenInfo
  (dpy: DisplayStar; pattern: FcPatternStar; fi: XftFontInfo): XftFontStar;

<* EXTERNAL XftFontOpenPattern *>
PROCEDURE FontOpenPattern (dpy: DisplayStar; pattern: FcPatternStar):
  XftFontStar;

<* EXTERNAL XftFontCopy *>
PROCEDURE FontCopy (dpy: DisplayStar; pub: XftFontStar): XftFontStar;

<* EXTERNAL XftFontClose *>
PROCEDURE FontClose (dpy: DisplayStar; pub: XftFontStar);

<* EXTERNAL XftInitFtLibrary *>
PROCEDURE InitFtLibrary (): BOOLEAN;


(* xftglyphs.c *)

<* EXTERNAL XftFontLoadGlyphs *>
PROCEDURE FontLoadGlyphs (dpy         : DisplayStar;
                          pub         : XftFontStar;
                          need_bitmaps: BOOLEAN;
                          glyphs      : FT_UIntStar;
                          nglyph      : Int          );

<* EXTERNAL XftFontUnloadGlyphs *>
PROCEDURE FontUnloadGlyphs
  (dpy: DisplayStar; pub: XftFontStar; glyphs: FT_UIntStar; nglyph: Int);


CONST XFT_NMISSING = 256;

<* EXTERNAL XftFontCheckGlyph *>
PROCEDURE FontCheckGlyph (dpy         : DisplayStar;
                          pub         : XftFontStar;
                          need_bitmaps: BOOLEAN;
                          glyph       : FT_UInt;
                          missing     : FT_UIntStar;
                          nmissing    : int_star     ): BOOLEAN;


<* EXTERNAL XftCharExists *>
PROCEDURE CharExists (dpy: DisplayStar; pub: XftFontStar; ucs4: FcChar32):
  BOOLEAN;

<* EXTERNAL XftCharIndex *>
PROCEDURE CharIndex (dpy: DisplayStar; pub: XftFontStar; ucs4: FcChar32):
  FT_UInt;

(* xftinit.c *)

<* EXTERNAL XftInit *>
PROCEDURE Init (config: String): BOOLEAN;

<* EXTERNAL XftGetVersion *>
PROCEDURE GetVersion (): Int;

(* xftlist.c *)

(*
No valist version
FcFontSet *
XftListFonts (Display	*dpy,
              int	screen,
              ...) _X_SENTINEL(0);
*)

(* xftname.c *)

<* EXTERNAL XftNameParse *>
PROCEDURE NameParse (name: String): FcPatternStar;

(* xftrender.c *)

<* EXTERNAL XftGlyphRender *>
PROCEDURE GlyphRender (dpy    : DisplayStar;
                       op     : Int;
                       src    : Picture;
                       pub    : XftFontStar;
                       dst    : Picture;
                       srcx   : Int;
                       srcy   : Int;
                       x      : Int;
                       y      : Int;
                       glyphs : FT_UIntStar;
                       nglyphs: Int          );

<* EXTERNAL XftGlyphSpecRender *>
PROCEDURE GlyphSpecRender (dpy    : DisplayStar;
                           op     : Int;
                           src    : Picture;
                           pub    : XftFontStar;
                           dst    : Picture;
                           srcx   : Int;
                           srcy   : Int;
                           glyphs : FT_UIntStar;
                           nglyphs: Int          );

<* EXTERNAL XftCharSpecRender *>
PROCEDURE CharSpecRender (dpy  : DisplayStar;
                          op   : Int;
                          src  : Picture;
                          pub  : XftFontStar;
                          dst  : Picture;
                          srcx : Int;
                          srcy : Int;
                          chars: XftCharSpecStar;
                          len  : Int              );

<* EXTERNAL XftGlyphFontSpecRender *>
PROCEDURE GlyphFontSpecRender (dpy    : DisplayStar;
                               op     : Int;
                               src    : Picture;
                               dst    : Picture;
                               srcx   : Int;
                               srcy   : Int;
                               glyphs : XftGlyphFontSpecStar;
                               nglyphs: Int                   );

<* EXTERNAL XftCharFontSpecRender *>
PROCEDURE CharFontSpecRender (dpy  : DisplayStar;
                              op   : Int;
                              src  : Picture;
                              dst  : Picture;
                              srcx : Int;
                              srcy : Int;
                              chars: XftCharFontSpecStar;
                              len  : Int                  );

<* EXTERNAL XftTextRender8 *>
PROCEDURE TextRender8 (dpy   : DisplayStar;
                       op    : Int;
                       src   : Picture;
                       pub   : XftFontStar;
                       dst   : Picture;
                       srcx  : Int;
                       srcy  : Int;
                       x     : Int;
                       y     : Int;
                       string: FcChar8Star;
                       len   : Int          );

<* EXTERNAL XftTextRender16 *>
PROCEDURE TextRender16 (dpy   : DisplayStar;
                        op    : Int;
                        src   : Picture;
                        pub   : XftFontStar;
                        dst   : Picture;
                        srcx  : Int;
                        srcy  : Int;
                        x     : Int;
                        y     : Int;
                        string: FcChar16Star;
                        len   : Int           );

<* EXTERNAL XftTextRender16BE *>
PROCEDURE TextRender16BE (dpy   : DisplayStar;
                          op    : Int;
                          src   : Picture;
                          pub   : XftFontStar;
                          dst   : Picture;
                          srcx  : Int;
                          srcy  : Int;
                          x     : Int;
                          y     : Int;
                          string: FcChar8Star;
                          len   : Int          );

<* EXTERNAL XftTextRender16LE *>
PROCEDURE TextRender16LE (dpy   : DisplayStar;
                          op    : Int;
                          src   : Picture;
                          pub   : XftFontStar;
                          dst   : Picture;
                          srcx  : Int;
                          srcy  : Int;
                          x     : Int;
                          y     : Int;
                          string: FcChar8Star;
                          len   : Int          );

<* EXTERNAL XftTextRender32 *>
PROCEDURE TextRender32 (dpy   : DisplayStar;
                        op    : Int;
                        src   : Picture;
                        pub   : XftFontStar;
                        dst   : Picture;
                        srcx  : Int;
                        srcy  : Int;
                        x     : Int;
                        y     : Int;
                        string: FcChar32Star;
                        len   : Int           );

<* EXTERNAL XftTextRender32BE *>
PROCEDURE TextRender32BE (dpy   : DisplayStar;
                          op    : Int;
                          src   : Picture;
                          pub   : XftFontStar;
                          dst   : Picture;
                          srcx  : Int;
                          srcy  : Int;
                          x     : Int;
                          y     : Int;
                          string: FcChar8Star;
                          len   : Int          );

<* EXTERNAL XftTextRender32LE *>
PROCEDURE TextRender32LE (dpy   : DisplayStar;
                          op    : Int;
                          src   : Picture;
                          pub   : XftFontStar;
                          dst   : Picture;
                          srcx  : Int;
                          srcy  : Int;
                          x     : Int;
                          y     : Int;
                          string: FcChar8Star;
                          len   : Int          );

<* EXTERNAL XftTextRenderUtf8 *>
PROCEDURE TextRenderUtf8 (dpy   : DisplayStar;
                          op    : Int;
                          src   : Picture;
                          pub   : XftFontStar;
                          dst   : Picture;
                          srcx  : Int;
                          srcy  : Int;
                          x     : Int;
                          y     : Int;
                          string: FcChar8Star;
                          len   : Int          );

<* EXTERNAL XftTextRenderUtf16 *>
PROCEDURE TextRenderUtf16 (dpy   : DisplayStar;
                           op    : Int;
                           src   : Picture;
                           pub   : XftFontStar;
                           dst   : Picture;
                           srcx  : Int;
                           srcy  : Int;
                           x     : Int;
                           y     : Int;
                           string: FcChar8Star;
                           endian: FcResult;
                           len   : Int          );


(* xftxlfd.c *)

<* EXTERNAL XftXlfdParse *>
PROCEDURE XlfdParse
  (xlfd_orig: String; ignore_scalable: BOOLEAN; complete: BOOLEAN):
  FcPatternStar;

(* FontConfig.h - we need a couple of functions to get font attributes *)

<* EXTERNAL FcPatternGet *>
PROCEDURE FcPatternGet (p : FcPatternStar;
                        object  : String;
                        id : INTEGER;
                        v : REFANY) : FcResult;

<* EXTERNAL FcPatternGetInteger *>
PROCEDURE FcGetInteger (p : FcPatternStar;
                        object : String;
                        n : INTEGER;
                        VAR i : INTEGER) : FcResult;

<* EXTERNAL FcPatternGetDouble *>
PROCEDURE FcGetLongReal (p : FcPatternStar;
                        object : String;
                        n : INTEGER;
                        VAR d : LONGREAL) : FcResult;

<* EXTERNAL FcPatternGetString *>
PROCEDURE FcGetString (p : FcPatternStar;
                       object : String;
                       n : INTEGER;
                       VAR s : String) : FcResult;

<* EXTERNAL FcPatternGetBool *>
PROCEDURE FcGetBool (p : FcPatternStar;
                       object : String;
                       n : INTEGER;
                       VAR b : BOOLEAN) : FcResult;

CONST
  FC_FAMILY =          "family";            (* String *)
  FC_STYLE  =          "style";             (* String *)
  FC_SLANT  =          "slant";             (* Int *)
  FC_WEIGHT =          "weight";            (* Int *)
  FC_SIZE   =          "size";              (* Range (double) *)
  FC_ASPECT =          "aspect";            (* Double *)
  FC_PIXEL_SIZE =      "pixelsize";         (* Double *)
  FC_SPACING    =      "spacing";           (* Int *)
  FC_FOUNDRY    =      "foundry";           (* String *)
  FC_ANTIALIAS  =      "antialias";         (* Bool (depends) *)
  FC_HINTING    =      "hinting";           (* Bool (true) *)
  FC_HINT_STYLE =      "hintstyle";         (* Int *)
  FC_VERTICAL_LAYOUT=  "verticallayout";    (* Bool (false) *)
  FC_AUTOHINT   =      "autohint";          (* Bool (false) *)

  FC_WIDTH      =      "width";             (* Int *)
  FC_FILE       =      "file";              (* String *)
  FC_INDEX      =      "index";             (* Int *)
  FC_FT_FACE    =      "ftface";            (* FT_Face *)
  FC_RASTERIZER =      "rasterizer";        (* String (deprecated) *)
  FC_OUTLINE    =      "outline";           (* Bool *)
  FC_SCALABLE   =      "scalable";          (* Bool *)
  FC_COLOR      =      "color";             (* Bool *)
  FC_VARIABLE   =      "variable";          (* Bool *)
  FC_SCALE      =      "scale";             (* double (deprecated) *)
  FC_SYMBOL     =      "symbol";            (* Bool *)
  FC_DPI        =      "dpi";               (* double *)
  FC_RGBA       =      "rgba";              (* Int *)
  FC_MINSPACE   =      "minspace";          (* Bool use minimum line spacing *)
  FC_SOURCE     =      "source";            (* String (deprecated) *)
  FC_CHARSET    =      "charset";           (* CharSet *)
  FC_LANG       =      "lang";              (* String RFC 3066 langs *)
  FC_FONTVERSION=      "fontversion";       (* Int from 'head' table *)
  FC_FULLNAME   =      "fullname";          (* String *)
  FC_FAMILYLANG =      "familylang";        (* String RFC 3066 langs *)
  FC_STYLELANG  =      "stylelang";         (* String RFC 3066 langs *)
  FC_FULLNAMELANG =    "fullnamelang";      (* String RFC 3066 langs *)
  FC_CAPABILITY =      "capability";        (* String *)
  FC_FONTFORMAT =      "fontformat";        (* String *)
  FC_EMBOLDEN   =      "embolden";          (* Bool - true if emboldening needed*)
  FC_EMBEDDED_BITMAP=  "embeddedbitmap";    (* Bool - true to enable embedded bitmaps *)
  FC_DECORATIVE =      "decorative";        (* Bool - true if style is a decorative variant *)
  FC_LCD_FILTER =      "lcdfilter";         (* Int *)
  FC_FONT_FEATURES =   "fontfeatures";      (* String *)
  FC_FONT_VARIATIONS=  "fontvariations";    (* String *)
  FC_NAMELANG   =      "namelang";          (* String RFC 3866 langs *)
  FC_PRGNAME    =      "prgname";           (* String *)
  FC_HASH       =      "hash";              (* String (deprecated) *)
  FC_POSTSCRIPT_NAME=  "postscriptname";    (* String *)

  FC_CACHE_VERSION  = "7";
  FC_CACHE_SUFFIX =            ".cache-"  & FC_CACHE_VERSION;
  FC_DIR_CACHE_FILE =           "fonts.cache-"  & FC_CACHE_VERSION;
  FC_USER_CACHE_FILE =         ".fonts.cache-"  & FC_CACHE_VERSION;

  (* Adjust outline rasterizer *)
  FC_CHARWIDTH =       "charwidth"; (* Int *)
  FC_CHAR_WIDTH =      FC_CHARWIDTH;
  FC_CHAR_HEIGHT =     "charheight"; (* Int *)
  FC_MATRIX =          "matrix";     (* FcMatrix *)

END Xft.
