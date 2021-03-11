(* Copyright Peter McKinna 2019 *)

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
              charset : ADDRESS;
              pattern : FcPatternStar;
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
                        glyphs : FT_UIntStar;
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

END Xft.
