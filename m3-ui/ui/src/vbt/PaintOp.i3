(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman, Mark Manasse and Greg Nelson           *)
(* Last modified on Mon Feb 24 13:57:20 PST 1992 by muller   *)
(*      modified on Sat Dec 21 17:23:48 PST 1991 by gnelson  *)
(*      modified on Thu Apr 12 10:37:26 PDT 1990 by steveg   *)
<*PRAGMA LL*>

(* A "PaintOp.T" is a screen-independent painting operation. 

A painting operation "op" takes a source pixel "s" and a destination
pixel "d" and produces a new value "op(d, s)" for the destination pixel.

A painting operation that ignores the source pixel is called a {\it tint}.  
If "op" is a tint, we just write "op(d)" instead of "op(d, s)".
If the effect of a tint is to set the destination pixel to
some fixed value independent of its initial value, then the 
tint is said to be {\it opaque}. 

The locking level is "LL.sup <= VBT.mu" for all of the procedures
in this interface. *)

INTERFACE PaintOp;

TYPE
  T = RECORD op:INTEGER END; Predefined = [0..16];
  
CONST
  Bg = T{0};                        
  Fg = T{1};                        
  Transparent = T{2};               
  Swap = T{3};

  Copy = T{4};

(* "Bg", "Fg", "Transparent", and "Swap" are Trestle's four basic tints.
   
   "Bg" sets the destination pixel to the screen's background color;
   "Fg" sets it to the screen's foreground color; "Transparent" is the
   identity function; "Swap" is a self-inverting operation that
   exchanges the foreground and background pixels.  More precisely,
   consider a particular screentype and let "bgpix" and "fgpix" be the
   foreground and background pixel for that screentype.  Then for any
   pixel "d",

| Bg(d) = bgpix
| Fg(d) = fgpix

| Transparent(d) = d

| Swap(bgpix) = fgpix
| Swap(fgpix) = bgpix
| Swap(Swap(d)) = d
| Swap(d) # d

The operation "Copy" copies source to destination:

| Copy(d, s) = s

"Copy" is not a tint, and should be used only when the source pixels
are of the same screentype as the destination pixels (for example,
with "VBT.Scroll", or when painting a pixmap of the same type as
the screen).  *)

CONST
  BgBg = Bg;
  BgFg = T{5};                      
  BgTransparent = T{6}; 
  BgSwap = T{7};    

  FgFg = Fg;
  FgBg = T{8};
  FgTransparent = T{9};
  FgSwap = T{10};

  TransparentTransparent = Transparent;
  TransparentBg = T{11};
  TransparentFg = T{12};
  TransparentSwap = T{13};

  SwapSwap = Swap;
  SwapBg = T{14};
  SwapFg = T{15};
  SwapTransparent = T{16};
  
(* The sixteen operations above all have names of the form "XY",
   where "X" and "Y" are one of the four basic tints.
   They are defined by the rule:

| XY(dest, source) = 
|   IF source = 0 THEN X(dest) ELSE Y(dest) END

   For example, "BgFg" can be used to paint a one bit deep source 
   interpreting zeros as background and ones as foreground. 

   Obviously these sixteen painting operations should be used only with
   one-bit deep sources.  However, not all one-bit deep sources are
   of the same screentype: for example, different screentypes might
   have different rules for representing bitmaps.  To accomodate this
   unfortunate fact of life, we associate with every screentype "st"
   another screentype "st.bits", which is the type of bitmap sources
   appropriate for "st". The depth of "st.bits" is always one.  If the
   depth of "st" is one, then it is possible (but not certain) that
   "st.bits = st". When using one of sixteen operations above on a "VBT"
   with screentype "st", the source must have type "st.bits".  You will
   be happy to recall that this will be taken care of automatically
   if you use screen-independent bitmaps and fonts.
   
   Next there is a procedure for generating colored painting operations. *)

TYPE 
  Mode = {Stable, Normal, Accurate};
  BW = {UseBg, UseFg, UseIntensity};

PROCEDURE FromRGB(
   r, g, b: REAL; 
   mode := Mode.Normal;
   gray := -1.0;
   bw := BW.UseIntensity): T;
(* Return a tint that will set a pixel to the color "(r,g,b)". *)

(* The values "r", "g", and "b" should be in the range "0.0" to "1.0";
   they represent the fractions of red, green, and blue in the
   desired color.

   The "gray" argument controls what the tint will do on a gray-scale
   display.  If "gray" is between zero and one, it specifies the
   intensity of the tint.  If "gray" is defaulted to "-1", then the
   tint will use the intensity of the color "(r,g,b)".

   The "bw" argument controls what the tint will be on a monochrome
   display.  If "bw" is "UseBg" or "UseFg", then the tint will be "Bg"
   or "Fg", respectively.  If "bw" is "UseIntensity", then the tint
   will be "Fg" if "r", "g", and "b" are all zero (that is, if the color
   is black), and "Bg" otherwise.

   The "mode" argument is relevant on color and gray-scale displays.
   When the total number of pixel colors desired by all of the
   applications that are running exceeds the number of available colors,
   then some applications' colors will change (usually in an
   unpleasantly random way).  
   
   To reduce the likelihood that your color will change randomly (at
   the cost of fidelity), set "mode" to "Stable".  To increase the
   fidelity of the pixel to the specified intensities (at the cost of
   increased danger of random change), set "mode" to "Accurate".  For
   example, an icon window should use stable colors; a color editor
   should use accurate colors.  *)

PROCEDURE Pair(op0, op1: T): T;
(* Return an operation "op" such that "op(d,0) = op0(d)" and
   "op(d,1) = op1(d)". *)
   
(* For example, 

| Pair(FromRGB(1.0,1.0,1.0), FromRGB(1.0,0.0,0.0))

   will paint a bitmap with zeros as white and ones as red. *)
   
PROCEDURE SwapPair(op0, op1: T): T;
(* Return an operation that swaps the pixels painted by "op0" and "op1". *)

(* "SwapPair" requires that "op0" and "op1" be opaque, that is,
   they must set the destination to particular pixels (say, "pix0" 
   and "pix1"). Then the tint "op" returned by "SwapPair" satisfies:

| op(pix0) = pix1
| op(pix1) = pix0
| op(op(p)) = p `for any pixel` p

For example, "Swap = SwapPair(Bg, Fg)". *)

(* Sometimes it is handy to collect several related painting 
   operations into a single object: *)

TYPE 
  ColorQuad = OBJECT 
    bg, fg, bgFg, transparentFg: T 
  END;

PROCEDURE MakeColorQuad(bg, fg: T): ColorQuad;
(* Return "ColorQuad{bg,fg,Pair(bg,fg),Pair(Transparent,fg)}". *)

TYPE
  ColorScheme = ColorQuad OBJECT 
    swap, bgTransparent, bgSwap, fgBg, fgTransparent,
      fgSwap, transparentBg, transparentSwap,
      swapBg, swapFg, swapTransparent: T;
  END;

PROCEDURE MakeColorScheme(bg, fg: T): ColorScheme;
(* Return the fifteen painting operations other than "Transparent" that
    can be made by combining "bg", "fg", and "Transparent", using
    "SwapPair" and "Pair".  *)
    
(* In "MakeColorQuad" and "MakeColorScheme", "bg" and "fg" should be 
   tints. *)

VAR (*CONST*) bgFg: ColorScheme; 

(* This ``variable'' is really a constant for "MakeColorScheme(Bg, Fg)". *)

END PaintOp.
