(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Jun 13 15:49:48 PDT 1995 by najork                   *)
(*       Created on Thu Feb 17 18:50:39 PST 1994 by najork                   *)

(******************************************************************************

This interface contains PEX definitions, derived from C header files. 
The following C header files went into the translation:

     PEX.h
     PEXproto.h
     PEXprotost.h
     MPEX.h
     MPEXproto.h
     MPEXprotostr.h
     PEXlib.h
     PEXlibprotos.h
     PEXocbuf.h
     MPEXlib.h

The translation was done by hand. Not every definition in the C header files 
is translated; and probably there are a few (lots of?) bugs.  Send a message 
to najork@src.dec.com if you need any definitions that are not yet translated,
or if you trip over any bugs; I will patch things up on demand. 

Here are the copyright notices from the original C header files:



Copyright notice of PEX.h, PEXproto.h, PEXprotost.h:
----------------------------------------------------

Copyright 1989, 1990, 1991 by Sun Microsystems, Inc. and the X Consortium.

                            All Rights Reserved
    
    Permission to use, copy, modify, and distribute this software and its 
    documentation for any purpose and without fee is hereby granted, 
    provided that the above copyright notice appear in all copies and that
    both that copyright notice and this permission notice appear in 
    supporting documentation, and that the names of Sun Microsystems,
    the X Consortium, and MIT not be used in advertising or publicity 
    pertaining to distribution of the software without specific, written 
    prior permission.  
    
    SUN MICROSYSTEMS DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, 
    INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO 
    EVENT SHALL SUN MICROSYSTEMS BE LIABLE FOR ANY SPECIAL, INDIRECT OR 
    CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF 
    USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR 
    OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR 
    PERFORMANCE OF THIS SOFTWARE.


Copyright notice of MPEX.h, MPEXproto.h, MPEXprotostr.h:
--------------------------------------------------------

    Copyright 1989 by Digital Equipment Corporation, Maynard, Massachusetts,
    and the Massachusetts Institute of Technology, Cambridge, Massachusetts.
    
                            All Rights Reserved
    
    Permission to use, copy, modify, and distribute this software and its
    documentation for any purpose and without fee is hereby granted,
    provided that the above copyright notice appear in all copies and that
    both that copyright notice and this permission notice appear in
    supporting documentation, and that the names of Digital or MIT not be
    used in advertising or publicity pertaining to distribution of the
    software without specific, written prior permission.
    
    DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
    ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
    DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
    ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
    WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
    ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
    SOFTWARE.


Copyright notice of PEXlib.h, PEXlibprotos.h, PEXocbuf.h, MPEXlib.h:
--------------------------------------------------------------------

    COPYRIGHT (c) 1988,1989,1990,1991     
    by DIGITAL Equipment Corporation, Maynard, Mass.                        
                                                                            
    This software is furnished under a license and may be used and  copied  
    only  in  accordance  with  the  terms  of  such  license and with the  
    inclusion of the above copyright notice.  This software or  any  other  
    copies  thereof may not be provided or otherwise made available to any  
    other person.  No title to and ownership of  the  software  is  hereby  
    transferred.                                                            
                                                                            
    The information in this software is subject to change  without  notice  
    and  should  not  be  construed  as  a commitment by DIGITAL Equipment  
    Corporation.                                                            
                                                                            
    DIGITAL assumes no responsibility for the use or  reliability  of  its  
    software on equipment which is not supplied by DIGITAL.                 

******************************************************************************)

UNSAFE INTERFACE PEX;

IMPORT Ctypes, Word, X;

(*****************************************************************************)
(* Misc.                                                                     *)
(*****************************************************************************)

TYPE
  XIDStar         = UNTRACED REF X.XID;
  DisplayStarStar = UNTRACED REF X.DisplayStar;

  Drawable = CARD32;  
  (* Note that X.Drawable = X.XID = Ctypes.unsigned_long
     So under AOSF, BYTESIZE (X.Drawable) = 8, and BYTESIZE (Drawable) = 4
    
     This appears strange, but is consistent with 
     /usr/include/PEX/PEXproto.h (Revision 1.1.2.4 1993/03/23) and 
     /usr/include/PEX/MPEXproto.h (Revision 1.1.1.6 1993/06/03)
     from the AOSF Open3D kit.

     Also note that the pxlPhigsWksInfo record and all procedures
     use X.Drawable, not drawable. This is consistent with the definition
     of Drawable in /usr/include/PEX/PEXlib.h and 
     /usr/include/PEX/PEXlibprotos.h from the AOSF Open3D kit. *)

(*****************************************************************************)
(* Based on Xmd.h                                                            *)
(*                                                                           *)
(* Performs only the definitions relevant for PEX.                           *)
(*****************************************************************************)

TYPE  
  INT8   = Ctypes.char;                       (*  8 bits *)
  INT16  = Ctypes.short;                      (* 16 bits *)
  INT32  = Ctypes.int;                        (* 32 bits *)
  CARD8  = Ctypes.unsigned_char;              (*  8 bits *)
  CARD16 = Ctypes.unsigned_short;             (* 16 bits *)
  CARD32 = Ctypes.unsigned_int;               (* 32 bits *)
  BYTE   = Ctypes.unsigned_char;              (*  8 bits *)

(*****************************************************************************)
(* Based on PEX.h  v 5.1 91/02/16 09:47:00 rws                               *)
(*                                                                           *)
(* Should be complete!                                                       *)
(*****************************************************************************)

CONST 

  (* Matches revision 5.0P *)

  PEX_PROTO_MAJOR = 5;	(* current protocol version *)
  PEX_PROTO_MINOR = 0;	(* current minor version *)

(* Subsets *)

  PEXCompleteImplementation  = 0;
  PEXImmediateModeOnly	     = 1;
  PEXPhigsWksOnly            = 2;

(* Resources *)

  PEXAlreadyFreed = 1;

(* Asf Attributes *)

(* Masks for setting Asf's *)

  PEXMarkerTypeAsf           = Word.LeftShift(1,0);
  PEXMarkerScaleAsf          = Word.LeftShift(1,1);
  PEXMarkerColourAsf         = Word.LeftShift(1,2);
  PEXTextFontIndexAsf        = Word.LeftShift(1,3);
  PEXTextPrecAsf             = Word.LeftShift(1,4);
  PEXCharExpansionAsf        = Word.LeftShift(1,5);
  PEXCharSpacingAsf          = Word.LeftShift(1,6);
  PEXTextColourAsf           = Word.LeftShift(1,7);
  PEXLineTypeAsf             = Word.LeftShift(1,8);
  PEXLineWidthAsf            = Word.LeftShift(1,9);
  PEXLineColourAsf           = Word.LeftShift(1,10);
  PEXCurveApproxAsf          = Word.LeftShift(1,11);
  PEXPolylineInterpAsf       = Word.LeftShift(1,12);
  PEXInteriorStyleAsf        = Word.LeftShift(1,13);
  PEXInteriorStyleIndexAsf   = Word.LeftShift(1,14);
  PEXSurfaceColourAsf        = Word.LeftShift(1,15);
  PEXSurfaceInterpAsf        = Word.LeftShift(1,16);
  PEXReflectionModelAsf      = Word.LeftShift(1,17);
  PEXReflectionAttrAsf       = Word.LeftShift(1,18);
  PEXBfInteriorStyleAsf      = Word.LeftShift(1,19);
  PEXBfInteriorStyleIndexAsf = Word.LeftShift(1,20);
  PEXBfSurfaceColourAsf      = Word.LeftShift(1,21);
  PEXBfSurfaceInterpAsf      = Word.LeftShift(1,22);
  PEXBfReflectionModelAsf    = Word.LeftShift(1,23);
  PEXBfReflectionAttrAsf     = Word.LeftShift(1,24);
  PEXSurfaceApproxAsf        = Word.LeftShift(1,25);
  PEXSurfaceEdgesAsf         = Word.LeftShift(1,26);
  PEXSurfaceEdgeTypeAsf      = Word.LeftShift(1,27);
  PEXSurfaceEdgeWidthAsf     = Word.LeftShift(1,28);
  PEXSurfaceEdgeColourAsf    = Word.LeftShift(1,29);

  PEXMaxAsfShift = 29;

(* Asf Values *)

  PEXBundled    = 0;
  PEXIndividual = 1;

(* Composition *)

  PEXPreConcatenate  = 0;
  PEXPostConcatenate = 1;
  PEXReplace         = 2;

(* Cull mode *)

  (* 0 None *)
  PEXBackFaces  = 1;
  PEXFrontFaces	= 2;

(* Curve Type  and Surface Type *)

  PEXRational    = 0;
  PEXNonRational = 1;

(* Edit Mode *)

  PEXStructureInsert  = 0;
  PEXStructureReplace = 1;

(* Whence values *)

  PEXBeginning = 0;
  PEXCurrent   = 1;
  PEXEnd       = 2;

(* Element Search *)

  PEXNotFound = 1;
  PEXFound    = 2;

(* GetEnumeratedType return format *)

  PEXETIndex    = 1;
  PEXETMnemonic = 2;
  PEXETBoth     = 3;

(* Enum Types *)

  PEXETMarkerType 		=  1;
  PEXETATextStyle 		=  2;
  PEXETInteriorStyle 		=  3;
  PEXETHatchStyle 		=  4;
  PEXETLineType 		=  5;
  PEXETSurfaceEdgeType 		=  6;
  PEXETPickDeviceType		=  7;
  PEXETPolylineInterpMethod 	=  8;
  PEXETCurveApproxMethod 	=  9;
  PEXETReflectionModel 		= 10;
  PEXETSurfaceInterpMethod 	= 11;
  PEXETSurfaceApproxMethod 	= 12;
  PEXETModelClipOperator 	= 13;
  PEXETLightType 		= 14;
  PEXETColourType 		= 15;
  PEXETFloatFormat 		= 16;
  PEXETHlhsrMode 		= 17;
  PEXETPromptEchoType 		= 18;
  PEXETDisplayUpdateMode 	= 19;
  PEXETColourApproxType 	= 20;
  PEXETColourApproxModel 	= 21;
  PEXETGDP 			= 22;
  PEXETGDP3 			= 23;
  PEXETGSE 			= 24;
  PEXETTrimCurveApproxMethod 	= 25;
  PEXETRenderingColourModel 	= 26;
  PEXETParaSurfCharacteristics 	= 27;

(* Renderer state *)

  PEXIdle      = 0;
  PEXRendering = 1;

(* Flags (e.g., Switches, Visibility, and Edges) *)

  PEXOff = 0;
  PEXOn  = 1;

(* Shape hints *)
(* Complex, Nonconvex, Convex, are defined  as 0, 1, 2 in X.h *)

  PEXComplex      = 0;
  PEXNonconvex    = 1;
  PEXConvex       = 2;
  PEXUnknownShape = 3;

(* Contour hints *)

  PEXDisjoint       = 0;
  PEXNested         = 1;
  PEXIntersecting   = 2;
  PEXUnknownContour = 3;

(* Table Type *)

  PEXLineBundleLUT     =  1;
  PEXMarkerBundleLUT   =  2;
  PEXTextBundleLUT     =  3;
  PEXInteriorBundleLUT =  4;
  PEXEdgeBundleLUT     =  5;
  PEXPatternLUT        =  6;
  PEXTextFontLUT       =  7;
  PEXColourLUT         =  8;
  PEXViewLUT           =  9;
  PEXLightLUT          = 10;
  PEXDepthCueLUT       = 11;
  PEXColourApproxLUT   = 12;

  PEXMaxTableType      = 12;

(* Status in GetTableEntry *)

  PEXDefaultEntry = 0;
  PEXDefinedEntry = 1;

(* ValueType in GetTableEntr{y|ies} *)

  PEXSetValue      = 0;
  PEXRealizedValue = 1;

(* Constants for Path and Vertical and Horizontal alignment *)

  PEXPathRight    = 0;
  PEXPathLeft     = 1;
  PEXPathUp       = 2;
  PEXPathDown     = 3;
  PEXValignNormal = 0;
  PEXValignTop    = 1;
  PEXValignCap    = 2;
  PEXValignHalf   = 3;
  PEXValignBase   = 4;
  PEXValignBottom = 5;
  PEXHalignNormal = 0;
  PEXHalignLeft   = 1;
  PEXHalignCenter = 2;
  PEXHalignRight  = 3;

(* Text precision *)

  PEXStringPrecision = 0;
  PEXCharPrecision   = 1;
  PEXStrokePrecision = 2;

(* Character Set Widths *)

  PEXCSByte  = 0;
  PEXCSShort = 1;
  PEXCSLong  = 2;

(* Update State *)

  PEXNotPending = 0;
  PEXPending    = 1;

(* Visual State *)

  PEXCorrect   = 0;
  PEXDeferred  = 1;
  PEXSimulated = 2;

(* Display State *)

  PEXEmpty    = 0;
  PEXNotEmpty = 1;

(* Buffer Mode *)

  PEXSingleBuffered = 0;
  PEXDoubleBuffered = 1;

(* Dynamic types *)

  PEXIMM = 0;
  PEXIRG = 1;
  PEXCBS = 2;

(* Geometric attributes (Vertex, Facet) *)

  PEXGAColour = 16_0001;
  PEXGANormal = 16_0002;
  PEXGAEdges  = 16_0004;

(* Pick Status *)

  PEXNoPick = 0;
  PEXOk     = 1;

(* Pick Echo Switch *)

  PEXNoEcho = 0;
  PEXEcho   = 1;

(* Pick Path Order *)

  PEXTopFirst    = 0;
  PEXBottomFirst = 1;

(* Items for GetStructureInfo *)

  PEXElementPtr      = 16_0001;
  PEXNumElements     = 16_0002;
  PEXLengthStructure = 16_0004;
  PEXHasRefs         = 16_0008;
  PEXEditMode        = 16_0010;

(* Flags for GetStructuresInNetwork *)

  PEXAll     = 0;
  PEXOrphans = 1;

(* Path part for GetAncestors *)

  PEXTopPart    = 0;
  PEXBottomPart = 1;

(* Direction for ElementSearch *)

  PEXForward  = 0;
  PEXBackward = 1;

(* Nameset changes *)

  PEXNSAdd     = 0;
  PEXNSRemove  = 1;
  PEXNSReplace = 2;

(* Priorities *)

  PEXHigher = 0;
  PEXLower  = 1;

(* Enumerated Type Descriptors *)

  (* Marker Type *)

  PEXMarkerDot      = 1;
  PEXMarkerCross    = 2;
  PEXMarkerAsterisk = 3;
  PEXMarkerCircle   = 4;
  PEXMarkerX        = 5;

  (* ATextStyle *)

  PEXATextNotConnected = 1;
  PEXATextConnected    = 2;

  (* InteriorStyle *)

  PEXInteriorStyleHollow  = 1;
  PEXInteriorStyleSolid   = 2;
  PEXInteriorStylePattern = 3;
  PEXInteriorStyleHatch   = 4;
  PEXInteriorStyleEmpty   = 5;

  (* HatchStyle *)
  (* LineType *)

  PEXLineTypeSolid   = 1;
  PEXLineTypeDashed  = 2;
  PEXLineTypeDotted  = 3;
  PEXLineTypeDashDot = 4;

  (* SurfaceEdgeType *)

  PEXSurfaceEdgeSolid   = 1;
  PEXSurfaceEdgeDashed  = 2;
  PEXSurfaceEdgeDotted  = 3;
  PEXSurfaceEdgeDashDot = 4;

  (* PickDeviceType *)

  PEXPickDeviceDC_HitBox     = 1;
  PEXPickDeviceNPC_HitVolume = 2;

  (* PolylineInterpMethod *)

  PEXPolylineInterpNone   = 1;
  PEXPolylineInterpColour = 2;

  (* Curve(and Surface)(and Trim Curve)ApproxMethods *)

  PEXApproxImpDep               =  1;
  PEXApproxConstantBetweenKnots =  2;
  PEXApproxWcsChordalSize       =  3;
  PEXApproxNpcChordalSize       =  4;
  PEXApproxDcChordalSize        =  5;
  PEXCurveApproxWcsChordalDev   =  6;
  PEXCurveApproxNpcChordalDev   =  7;
  PEXCurveApproxDcChordalDev    =  8;
  PEXSurfaceApproxWcsPlanarDev  =  6;
  PEXSurfaceApproxNpcPlanarDev  =  7;
  PEXSurfaceApproxDcPlanarDev   =  8;
  PEXApproxWcsRelative          =  9;
  PEXApproxNpcRelative          = 10;
  PEXApproxDcRelative           = 11;

  (* ReflectionModel *)

  PEXReflectionNoShading = 1;
  PEXReflectionAmbient   = 2;
  PEXReflectionDiffuse   = 3;
  PEXReflectionSpecular  = 4;

  (* SurfaceInterpMethod *)

  PEXSurfaceInterpNone       = 1;
  PEXSurfaceInterpColour     = 2;
  PEXSurfaceInterpDotProduct = 3;
  PEXSurfaceInterpNormal     = 4;

  (* ModelClipOperator *)

  PEXModelClipReplace      = 1;
  PEXModelClipIntersection = 2;

  (* LightType *)

  PEXLightAmbient   = 1;
  PEXLightWcsVector = 2;
  PEXLightWcsPoint  = 3;
  PEXLightWcsSpot   = 4;

  (* ColourType *)

  PEXIndexedColour  = 0;
  PEXRgbFloatColour = 1;
  PEXCieFloatColour = 2;
  PEXHsvFloatColour = 3;
  PEXHlsFloatColour = 4;
  PEXRgb8Colour     = 5;
  PEXRgb16Colour    = 6;
  PEXMaxColour      = 6;

  (* FloatFormat *)

  PEXIeee_754_32    = 1;
  PEXDEC_F_Floating = 2;
  PEXIeee_754_64    = 3;
  PEXDEC_D_Floating = 4;

  (* HlhsrMode *)

  PEXHlhsrOff            = 1;
  PEXHlhsrZBuffer        = 2;
  PEXHlhsrPainters       = 3;
  PEXHlhsrScanline       = 4;
  PEXHlhsrHiddenLineOnly = 5;

  (* PromptEchoType *)

  PEXEchoPrimitive = 1;
  PEXEchoStructure = 2;
  PEXEchoNetwork   = 3;

  (* DisplayUpdateMethod *)

  PEXVisualizeEach     = 1;
  PEXVisualizeEasy     = 2;
  PEXVisualizeNone     = 3;
  PEXSimulateSome      = 4;
  PEXVisualizeWhenever = 5;

  (* ColourApproxType *)

  PEXColourSpace = 1;
  PEXColourRange = 2;

  (* ColourApproxMethod *)

  PEXColourApproxRGB = 1;
  PEXColourApproxCIE = 2;
  PEXColourApproxHSV = 3;
  PEXColourApproxHLS = 4;
  PEXColourApproxYIQ = 5;

  (* RenderingColourModel *)

  PEXRdrColourModelImpDep = 0;
  PEXRdrColourModelRGB    = 1;
  PEXRdrColourModelCIE    = 2;
  PEXRdrColourModelHSV    = 3;
  PEXRdrColourModelHLS    = 4;

  (* ParametricSurfaceCharacteristics *)

  PEXPSCNone          = 1;
  PEXPSCImpDep        = 2;
  PEXPSCIsoCurves     = 3;
  PEXPSCMcLevelCurves = 4;
  PEXPSCWcLevelCurves = 5;

  (* Isoparametric Curves *)

  PEXICUniformPlacement    = 0;
  PEXICNonuniformPlacement = 1;

(* Clipping *)

  PEXClipXY    = 16_0001;
  PEXClipBack  = 16_0002;
  PEXClipFront = 16_0004;

  PEXClip   = 0;
  PEXNoClip = 1;

(* Implementation Dependent Constant Names *)

  PEXIDDitheringSupported       =  1;
  PEXIDMaxEdgeWidth             =  2;
  PEXIDMaxLineWidth             =  3;
  PEXIDMaxMarkerSize            =  4;
  PEXIDMaxModelClipPlanes       =  5;
  PEXIDMaxNameSetNames          =  6;
  PEXIDMaxNonAmbientLights      =  7;
  PEXIDMaxNURBOrder             =  8;
  PEXIDMaxTrimCurveOrder        =  9;
  PEXIDMinEdgeWidth             = 10;
  PEXIDMinLineWidth             = 11;
  PEXIDMinMarkerSize            = 12;
  PEXIDNominalEdgeWidth         = 13;
  PEXIDNominalLineWidth         = 14;
  PEXIDNominalMarkerSize        = 15;
  PEXIDNumSupportedEdgeWidths   = 16;
  PEXIDNumSupportedLineWidths   = 17;
  PEXIDNumSupportedMarkerSizes  = 18;
  PEXIDBestColourApproximation  = 19;
  PEXIDTransparencySupported    = 20;
  PEXIDDoubleBufferingSupported = 21;
  PEXIDChromaticityRedU         = 22;
  PEXIDChromaticityRedV         = 23;
  PEXIDLuminanceRed             = 24;
  PEXIDChromaticityGreenU       = 25;
  PEXIDChromaticityGreenV       = 26;
  PEXIDLuminanceGreen           = 27;
  PEXIDChromaticityBlueU        = 28;
  PEXIDChromaticityBlueV        = 29;
  PEXIDLuminanceBlue            = 30;
  PEXIDChromaticityWhiteU       = 31;
  PEXIDChromaticityWhiteV       = 32;
  PEXIDLuminanceWhite           = 33;

(* Constants for IDRgbBestApproximation *)

  PEXColourApproxAnyValues = 0;
  PEXColourApproxPowersOf2 = 1;

(*****************************************************************************)
(* The following procedures mimic a set of (#define) C macros in PEX.h.      *)
(* C Macros are a tricky thing: They do not contain type information, and    *)
(* can thereby realize ad-hoc polymorphism which Modula-3 cannot reproduce,  *)
(* and worse, they have an Algol-like call-by-name semantics, which can be   *)
(* nasty. Example: given                                                     *)
(*    #define FOO(x) ((x)+(x))                                               *)
(* after executing                                                           *)
(*    i = 3; j = FOO(i++);                                                   *)
(* j is not 6, but 7, and i is not 4, but 5.                                 *)
(* I recommend to look at your code very carefully when you replace one of   *)
(* these C macros with a Modula-3 function!                                  *)
(*                                                                           *)
(* CHECK_BITMASK_ARRAY is not a function, but just a stub of code            *)
(* (an if-statement with the condition, but where the then- and else         *)
(* part are missing). I cannot be translated.                                *)
(*****************************************************************************)

(** To convert a bit index to a mask number and a mask value, assuming
 ** 32 bit wide words.  For example, a bitIndex of 5 will return 
 ** maskNum == 0 and maskValue == (1 << 5) = 32, while a bitIndex of 39
 ** will return maskNum == 1 and maskValue == (1 << 7) == 128 
 **)
PROCEDURE PEX_BITNUM_TO_BITMASK (bitIndex : Ctypes.int; 
                                 VAR maskNum : Ctypes.int; 
                                 VAR maskValue : Ctypes.int);
PROCEDURE PEX_BITMASK (i : Ctypes.int) : Ctypes.int;

PROCEDURE PEX_MASKIDX (i : Ctypes.int) : Ctypes.int;

PROCEDURE PEX_MASKWORD (READONLY buf : ARRAY OF Ctypes.int ; 
                        i : Ctypes.int) : Ctypes.int;
PROCEDURE PEX_BITSET (VAR buf : ARRAY OF Ctypes.int ; 
                      i : Ctypes.int);
PROCEDURE PEX_BITCLEAR (VAR buf : ARRAY OF Ctypes.int ; 
                        i : Ctypes.int);
PROCEDURE PEX_GETBIT (READONLY buf : ARRAY OF Ctypes.int ; 
                      i : Ctypes.int) : Ctypes.int;



CONST

  PEXMSGetWksInfo = 2;
  PEXMSPipeline   = 3;

(* Pipeline Context *)

  PEXPCMarkerType 		=  0;
  PEXPCMarkerScale 		=  1;
  PEXPCMarkerColour 		=  2;
  PEXPCMarkerBundleIndex 	=  3;
  PEXPCTextFont 		=  4;
  PEXPCTextPrecision 		=  5;
  PEXPCCharExpansion 		=  6;
  PEXPCCharSpacing 		=  7;
  PEXPCTextColour 		=  8;
  PEXPCCharHeight 		=  9;
  PEXPCCharUpVector 		= 10;
  PEXPCTextPath 		= 11;
  PEXPCTextAlignment 		= 12;
  PEXPCAtextHeight 		= 13;
  PEXPCAtextUpVector 		= 14;
  PEXPCAtextPath 		= 15;
  PEXPCAtextAlignment 		= 16;
  PEXPCAtextStyle 		= 17;
  PEXPCTextBundleIndex 		= 18;
  PEXPCLineType 		= 19;
  PEXPCLineWidth 		= 20;
  PEXPCLineColour 		= 21;
  PEXPCCurveApproximation 	= 22;
  PEXPCPolylineInterp 		= 23;
  PEXPCLineBundleIndex 		= 24;
  PEXPCInteriorStyle 		= 25;
  PEXPCInteriorStyleIndex 	= 26;
  PEXPCSurfaceColour 		= 27;
  PEXPCSurfaceReflAttr		= 28;
  PEXPCSurfaceReflModel		= 29;
  PEXPCSurfaceInterp 		= 30;
  PEXPCBfInteriorStyle 		= 31;

  PEXPCBfInteriorStyleIndex 	= 32;
  PEXPCBfSurfaceColour 		= 33;
  PEXPCBfSurfaceReflAttr 	= 34;
  PEXPCBfSurfaceReflModel 	= 35;
  PEXPCBfSurfaceInterp 		= 36;
  PEXPCSurfaceApproximation 	= 37;
  PEXPCCullingMode 		= 38;
  PEXPCDistinguishFlag 		= 39;
  PEXPCPatternSize 		= 40;
  PEXPCPatternRefPt 		= 41;
  PEXPCPatternRefVec1 		= 42;
  PEXPCPatternRefVec2 		= 43;
  PEXPCInteriorBundleIndex 	= 44;
  PEXPCSurfaceEdgeFlag 		= 45;
  PEXPCSurfaceEdgeType 		= 46;
  PEXPCSurfaceEdgeWidth 	= 47;
  PEXPCSurfaceEdgeColour 	= 48;
  PEXPCEdgeBundleIndex 		= 49;
  PEXPCLocalTransform 		= 50;
  PEXPCGlobalTransform 		= 51;
  PEXPCModelClip 		= 52;
  PEXPCModelClipVolume 		= 53;
  PEXPCViewIndex 		= 54;
  PEXPCLightState 		= 55;
  PEXPCDepthCueIndex 		= 56;
  PEXPCSetAsfValues 		= 57;
  PEXPCPickId 			= 58;
  PEXPCHlhsrIdentifier 		= 59;
  PEXPCNameSet 			= 60;
  PEXPCColourApproxIndex 	= 61;
  PEXPCRenderingColourModel 	= 62;
  PEXPCParaSurfCharacteristics 	= 63;
  PEXMaxPCIndex 		= 63;

(* Renderer Bitmasks *)

  PEXRDPipelineContext 	 = Word.LeftShift(1,0);
  PEXRDCurrentPath 	 = Word.LeftShift(1,1);
  PEXRDMarkerBundle 	 = Word.LeftShift(1,2);
  PEXRDTextBundle 	 = Word.LeftShift(1,3);
  PEXRDLineBundle 	 = Word.LeftShift(1,4);
  PEXRDInteriorBundle 	 = Word.LeftShift(1,5);
  PEXRDEdgeBundle 	 = Word.LeftShift(1,6);
  PEXRDViewTable 	 = Word.LeftShift(1,7);
  PEXRDColourTable 	 = Word.LeftShift(1,8);
  PEXRDDepthCueTable 	 = Word.LeftShift(1,9);
  PEXRDLightTable 	 = Word.LeftShift(1,10);
  PEXRDColourApproxTable = Word.LeftShift(1,11);
  PEXRDPatternTable 	 = Word.LeftShift(1,12);
  PEXRDTextFontTable 	 = Word.LeftShift(1,13);
  PEXRDHighlightIncl 	 = Word.LeftShift(1,14);
  PEXRDHighlightExcl 	 = Word.LeftShift(1,15);
  PEXRDInvisibilityIncl  = Word.LeftShift(1,16);
  PEXRDInvisibilityExcl  = Word.LeftShift(1,17);
  PEXRDRendererState 	 = Word.LeftShift(1,18);
  PEXRDHlhsrMode 	 = Word.LeftShift(1,19);
  PEXRDNpcSubvolume 	 = Word.LeftShift(1,20);
  PEXRDViewport 	 = Word.LeftShift(1,21);
  PEXRDClipList 	 = Word.LeftShift(1,22);
  PEXMaxRDShift 	 = 22;

(* Renderer Dynamics Bitmasks *)

  (* tables *)

  PEXDynMarkerBundle 		= Word.LeftShift(1,0);
  PEXDynTextBundle 		= Word.LeftShift(1,1);
  PEXDynLineBundle 		= Word.LeftShift(1,2);
  PEXDynInteriorBundle 		= Word.LeftShift(1,3);
  PEXDynEdgeBundle 		= Word.LeftShift(1,4);
  PEXDynViewTable 		= Word.LeftShift(1,5);
  PEXDynColourTable 		= Word.LeftShift(1,6);
  PEXDynDepthCueTable 		= Word.LeftShift(1,7);
  PEXDynLightTable 		= Word.LeftShift(1,8);
  PEXDynColourApproxTable 	= Word.LeftShift(1,9);
  PEXDynPatternTable 		= Word.LeftShift(1,10);
  PEXDynTextFontTable 		= Word.LeftShift(1,11);
  PEXDynMarkerBundleContents 	= Word.LeftShift(1,16);
  PEXDynTextBundleContents 	= Word.LeftShift(1,17);
  PEXDynLineBundleContents 	= Word.LeftShift(1,18);
  PEXDynInteriorBundleContents 	= Word.LeftShift(1,19);
  PEXDynEdgeBundleContents 	= Word.LeftShift(1,20);
  PEXDynViewTableContents 	= Word.LeftShift(1,21);
  PEXDynColourTableContents 	= Word.LeftShift(1,22);
  PEXDynDepthCueTableContents 	= Word.LeftShift(1,23);
  PEXDynLightTableContents 	= Word.LeftShift(1,24);
  PEXDynColourApproxContents 	= Word.LeftShift(1,25);
  PEXDynPatternTableContents 	= Word.LeftShift(1,26);
  PEXDynTextFontTableContents 	= Word.LeftShift(1,27);

  (* namesets *)

  PEXDynHighlightNameset            = Word.LeftShift(1,0);
  PEXDynInvisibilityNameset         = Word.LeftShift(1,1);
  PEXDynHighlightNamesetContents    = Word.LeftShift(1,12);
  PEXDynInvisibilityNamesetContents = Word.LeftShift(1,13);

  (* attributes *)

  PEXDynHlhsrMode    = Word.LeftShift(1,0);
  PEXDynNpcSubvolume = Word.LeftShift(1,1);
  PEXDynViewport     = Word.LeftShift(1,2);
  PEXDynClipList     = Word.LeftShift(1,3);

  PEXElementType = Word.LeftShift(1,0);
  PEXElementSize = Word.LeftShift(1,1);
  PEXElementData = Word.LeftShift(1,2);

(* Search Context Bitmasks *)

  PEXSCPosition      = Word.LeftShift(1,0);
  PEXSCDistance      = Word.LeftShift(1,1);
  PEXSCCeiling 	     = Word.LeftShift(1,2);
  PEXSCModelClipFlag = Word.LeftShift(1,3);
  PEXSCStartPath     = Word.LeftShift(1,4);
  PEXSCNormalList    = Word.LeftShift(1,5);
  PEXSCInvertedList  = Word.LeftShift(1,6);

(* Phigs Workstation Attribute Bitmasks *)

  PEXPWDisplayUpdate 	 = 0;
  PEXPWVisualState 	 = 1;
  PEXPWDisplaySurface 	 = 2;
  PEXPWViewUpdate 	 = 3;
  PEXPWDefinedViews 	 = 4;
  PEXPWWksUpdate 	 = 5;
  PEXPWReqNpcSubvolume 	 = 6;
  PEXPWCurNpcSubvolume 	 = 7;
  PEXPWReqWksViewport 	 = 8;
  PEXPWCurWksViewport 	 = 9;
  PEXPWHlhsrUpdate 	 = 10;
  PEXPWReqHlhsrMode 	 = 11;
  PEXPWCurHlhsrMode 	 = 12;
  PEXPWDrawable 	 = 13;
  PEXPWMarkerBundle 	 = 14;
  PEXPWTextBundle 	 = 15;
  PEXPWLineBundle 	 = 16;
  PEXPWInteriorBundle 	 = 17;
  PEXPWEdgeBundle 	 = 18;
  PEXPWColourTable 	 = 19;
  PEXPWDepthCueTable 	 = 20;
  PEXPWLightTable 	 = 21;
  PEXPWColourApproxTable = 22;
  PEXPWPatternTable 	 = 23;
  PEXPWTextFontTable 	 = 24;
  PEXPWHighlightIncl 	 = 25;
  PEXPWHighlightExcl 	 = 26;
  PEXPWInvisibilityIncl  = 27;
  PEXPWInvisibilityExcl  = 28;
  PEXPWPostedStructures  = 29;
  PEXPWNumPriorities 	 = 30;
  PEXPWBufferUpdate 	 = 31;
  PEXPWReqBufferMode 	 = 32;
  PEXPWCurBufferMode 	 = 33;

(* Indices for GetDynamics *)

  PEXPWDViewRep 	   = 	 0;
  PEXPWDMarkerBundle 	   = 	 1;
  PEXPWDTextBundle 	   = 	 2;
  PEXPWDLineBundle 	   = 	 3;
  PEXPWDInteriorBundle 	   = 	 4;
  PEXPWDEdgeBundle 	   = 	 5;
  PEXPWDColourTable 	   = 	 6;
  PEXPWDPatternTable 	   = 	 7;
  PEXPWDWksTransform 	   = 	 8;
  PEXPWDHighlightFilter    = 	 9;
  PEXPWDInvisibilityFilter =  10;
  PEXPWDHlhsrMode 	   = 	11;
  PEXPWDStructureModify    = 	12;
  PEXPWDPostStructure 	   = 	13;
  PEXPWDUnpostStructure    = 	14;
  PEXPWDDeleteStructure    = 	15;
  PEXPWDReferenceModify    = 	16;
  PEXPWDBufferModify 	   = 	17;
  PEXPWDLightTable 	   = 	18;
  PEXPWDDepthCueTable 	   = 	19;
  PEXPWDColourApproxTable  = 	20;

(* Pick Device Bitmasks *)

  PEXPDPickStatus 	  = Word.LeftShift(1,0);
  PEXPDPickPath 	  = Word.LeftShift(1,1);
  PEXPDPickPathOrder 	  = Word.LeftShift(1,2);
  PEXPDPickIncl 	  = Word.LeftShift(1,3);
  PEXPDPickExcl 	  = Word.LeftShift(1,4);
  PEXPDPickDataRec 	  = Word.LeftShift(1,5);
  PEXPDPickPromptEchoType = Word.LeftShift(1,6);
  PEXPDPickEchoVolume 	  = Word.LeftShift(1,7);
  PEXPDPickEchoSwitch 	  = Word.LeftShift(1,8);

(* Pick Measure Bitmasks *)

  PEXPMStatus = Word.LeftShift(1,0);
  PEXPMPath   = Word.LeftShift(1,1);

(* Errors *)

  PEXColourTypeError 		=  0;
  PEXRendererStateError 	=  1;
  PEXFloatingPointFormatError 	=  2;
  PEXLabelError 		=  3;
  PEXLookupTableError 		=  4;
  PEXNameSetError 		=  5;
  PEXPathError 			=  6;
  PEXFontError 			=  7;
  PEXPhigsWksError 		=  8;
  PEXPickMeasureError 		=  9;
  PEXPipelineContextError 	= 10;
  PEXRendererError 		= 11;
  PEXSearchContextError 	= 12;
  PEXStructureError 		= 13;
  PEXOutputCommandError 	= 14;
  PEXMaxError 			= 14;

(* Requests *)

  PEX_GetExtensionInfo		=  1;
  PEX_GetEnumeratedTypeInfo	=  2;
  PEX_GetImpDepConstants	=  3;
  PEX_CreateLookupTable		=  4;
  PEX_CopyLookupTable		=  5;
  PEX_FreeLookupTable		=  6;
  PEX_GetTableInfo		=  7;
  PEX_GetPredefinedEntries	=  8;
  PEX_GetDefinedIndices		=  9;
  PEX_GetTableEntry		= 10;
  PEX_GetTableEntries		= 11;
  PEX_SetTableEntries		= 12;
  PEX_DeleteTableEntries	= 13;
  PEX_CreatePipelineContext	= 14;
  PEX_CopyPipelineContext	= 15;
  PEX_FreePipelineContext	= 16;
  PEX_GetPipelineContext	= 17;
  PEX_ChangePipelineContext	= 18;
  PEX_CreateRenderer		= 19;
  PEX_FreeRenderer		= 20;
  PEX_ChangeRenderer		= 21;
  PEX_GetRendererAttributes	= 22;
  PEX_GetRendererDynamics	= 23;
  PEX_BeginRendering		= 24;
  PEX_EndRendering		= 25;
  PEX_BeginStructure		= 26;
  PEX_EndStructure		= 27;
  PEX_RenderOutputCommands	= 28;
  PEX_RenderNetwork		= 29;
  PEX_CreateStructure		= 30;
  PEX_CopyStructure		= 31;
  PEX_DestroyStructures		= 32;
  PEX_GetStructureInfo		= 33;
  PEX_GetElementInfo		= 34;
  PEX_GetStructuresInNetwork	= 35;
  PEX_GetAncestors		= 36;
  PEX_GetDescendants		= 37;
  PEX_FetchElements		= 38;
  PEX_SetEditingMode		= 39;
  PEX_SetElementPointer		= 40;
  PEX_SetElementPointerAtLabel	= 41;
  PEX_ElementSearch		= 42;
  PEX_StoreElements		= 43;
  PEX_DeleteElements		= 44;
  PEX_DeleteElementsToLabel	= 45;
  PEX_DeleteBetweenLabels	= 46;
  PEX_CopyElements		= 47;
  PEX_ChangeStructureRefs	= 48;
  PEX_CreateNameSet		= 49;
  PEX_CopyNameSet		= 50;
  PEX_FreeNameSet		= 51;
  PEX_GetNameSet		= 52;
  PEX_ChangeNameSet		= 53;
  PEX_CreateSearchContext	= 54;
  PEX_CopySearchContext		= 55;
  PEX_FreeSearchContext		= 56;
  PEX_GetSearchContext		= 57;
  PEX_ChangeSearchContext	= 58;
  PEX_SearchNetwork		= 59;
  PEX_CreatePhigsWks		= 60;
  PEX_FreePhigsWks		= 61;
  PEX_GetWksInfo		= 62;
  PEX_GetDynamics		= 63;
  PEX_GetViewRep		= 64;
  PEX_RedrawAllStructures	= 65;
  PEX_UpdateWorkstation		= 66;
  PEX_RedrawClipRegion		= 67;
  PEX_ExecuteDeferredActions	= 68;
  PEX_SetViewPriority		= 69;
  PEX_SetDisplayUpdateMode	= 70;
  PEX_MapDCtoWC			= 71;
  PEX_MapWCtoDC			= 72;
  PEX_SetViewRep		= 73;
  PEX_SetWksWindow		= 74;
  PEX_SetWksViewport		= 75;
  PEX_SetHlhsrMode		= 76;
  PEX_SetWksBufferMode		= 77;
  PEX_PostStructure		= 78;
  PEX_UnpostStructure		= 79;
  PEX_UnpostAllStructures	= 80;
  PEX_GetWksPostings		= 81;
  PEX_GetPickDevice		= 82;
  PEX_ChangePickDevice		= 83;
  PEX_CreatePickMeasure		= 84;
  PEX_FreePickMeasure		= 85;
  PEX_GetPickMeasure		= 86;
  PEX_UpdatePickMeasure		= 87;
  PEX_OpenFont			= 88;
  PEX_CloseFont			= 89;
  PEX_QueryFont			= 90;
  PEX_ListFonts			= 91;
  PEX_ListFontsWithInfo		= 92;
  PEX_QueryTextExtents 		= 93;
  PEXMaxRequest			= 93;

(* Output Commands *)

  PEXOCAll			= 0;
  PEXOCMarkerType		= 1;
  PEXOCMarkerScale		= 2;
  PEXOCMarkerColourIndex	= 3;
  PEXOCMarkerColour		= 4;
  PEXOCMarkerBundleIndex	= 5;
  PEXOCTextFontIndex		= 6;
  PEXOCTextPrecision		= 7;
  PEXOCCharExpansion		= 8;
  PEXOCCharSpacing		= 9;
  PEXOCTextColourIndex		= 10;
  PEXOCTextColour		= 11;
  PEXOCCharHeight		= 12;
  PEXOCCharUpVector		= 13;
  PEXOCTextPath			= 14;
  PEXOCTextAlignment		= 15;
  PEXOCAtextHeight		= 16;
  PEXOCAtextUpVector		= 17;
  PEXOCAtextPath		= 18;
  PEXOCAtextAlignment		= 19;
  PEXOCAtextStyle		= 20;
  PEXOCTextBundleIndex		= 21;
  PEXOCLineType			= 22;
  PEXOCLineWidth		= 23;
  PEXOCLineColourIndex		= 24;
  PEXOCLineColour		= 25;
  PEXOCCurveApproximation	= 26;
  PEXOCPolylineInterp		= 27;
  PEXOCLineBundleIndex		= 28;
  PEXOCInteriorStyle		= 29;
  PEXOCInteriorStyleIndex	= 30;
  PEXOCSurfaceColourIndex	= 31;
  PEXOCSurfaceColour		= 32;
  PEXOCSurfaceReflAttr		= 33;
  PEXOCSurfaceReflModel		= 34;
  PEXOCSurfaceInterp		= 35;
  PEXOCBfInteriorStyle		= 36;
  PEXOCBfInteriorStyleIndex	= 37;
  PEXOCBfSurfaceColourIndex	= 38;
  PEXOCBfSurfaceColour		= 39;
  PEXOCBfSurfaceReflAttr	= 40;
  PEXOCBfSurfaceReflModel	= 41;
  PEXOCBfSurfaceInterp		= 42;
  PEXOCSurfaceApproximation	= 43;
  PEXOCCullingMode		= 44;
  PEXOCDistinguishFlag		= 45;
  PEXOCPatternSize		= 46;
  PEXOCPatternRefPt		= 47;
  PEXOCPatternAttr		= 48;
  PEXOCInteriorBundleIndex	= 49;
  PEXOCSurfaceEdgeFlag		= 50;
  PEXOCSurfaceEdgeType		= 51;
  PEXOCSurfaceEdgeWidth		= 52;
  PEXOCSurfaceEdgeColourIndex	= 53;
  PEXOCSurfaceEdgeColour	= 54;
  PEXOCEdgeBundleIndex		= 55;
  PEXOCSetAsfValues		= 56;
  PEXOCLocalTransform		= 57;
  PEXOCLocalTransform2D		= 58;
  PEXOCGlobalTransform		= 59;
  PEXOCGlobalTransform2D	= 60;
  PEXOCModelClip		= 61;
  PEXOCModelClipVolume		= 62;
  PEXOCModelClipVolume2D	= 63;
  PEXOCRestoreModelClip		= 64;
  PEXOCViewIndex		= 65;
  PEXOCLightState		= 66;
  PEXOCDepthCueIndex		= 67;
  PEXOCPickId			= 68;
  PEXOCHlhsrIdentifier		= 69;
  PEXOCColourApproxIndex	= 70;
  PEXOCRenderingColourModel	= 71;
  PEXOCParaSurfCharacteristics	= 72;
  PEXOCAddToNameSet		= 73;
  PEXOCRemoveFromNameSet	= 74;
  PEXOCExecuteStructure		= 75;
  PEXOCLabel			= 76;
  PEXOCApplicationData		= 77;
  PEXOCGse			= 78;
  PEXOCMarker			= 79;
  PEXOCMarker2D			= 80;
  PEXOCText			= 81;
  PEXOCText2D			= 82;
  PEXOCAnnotationText		= 83;
  PEXOCAnnotationText2D		= 84;
  PEXOCPolyline			= 85;
  PEXOCPolyline2D		= 86;
  PEXOCPolylineSet		= 87;
  PEXOCNurbCurve		= 88;
  PEXOCFillArea			= 89;
  PEXOCFillArea2D		= 90;
  PEXOCExtFillArea		= 91;
  PEXOCFillAreaSet		= 92;
  PEXOCFillAreaSet2D		= 93;
  PEXOCExtFillAreaSet		= 94;
  PEXOCTriangleStrip		= 95;
  PEXOCQuadrilateralMesh	= 96;
  PEXOCSOFAS			= 97;
  PEXOCNurbSurface		= 98;
  PEXOCCellArray		= 99;
  PEXOCCellArray2D		= 100;
  PEXOCExtCellArray		= 101;
  PEXOCGdp			= 102;
  PEXOCGdp2D			= 103;
  PEXMaxOC			= 103;

  PEXOCNil			= Word.Not(0);


(*****************************************************************************)
(* Based on PEXproto.h  5.1 91/02/12                                         *)
(*                                                                           *)
(* Should be complete!                                                       *)
(*****************************************************************************)

(* Definitions for the PEX used by server and c bindings *)

(*
 * This packet-construction scheme makes the following assumptions:
 *
 * 1. The compiler is able to generate code which addresses one- and two-byte
 * quantities.  In the worst case, this would be done with bit-fields.  If 
 * bit-fields are used it may be necessary to reorder the request fields in
 * this file, depending on the order in which the machine assigns bit fields
 * to machine words.  There may also be a problem with sign extension, as K+R 
 * specify that bitfields are always unsigned.
 *
 * 2. 2- and 4-byte fields in packet structures must be ordered by hand such 
 * that they are naturally-aligned, so that no compiler will ever insert 
 * padding bytes.
 *
 * 3. All packets are hand-padded to a multiple of 4 bytes, for the same 
 * reason.
 *)

(* In the following typedefs, comments appear that say 
 * "LISTof Foo( numItems )", "CLIST of Foo()", and "SINGLE Foo()".   
 * These are used when the protocol specifies that a request or reply 
 * contains a variable length list of (possibly variable types of) objects.
 *
 * A LISTof list is one for which we have already been given the length.
 * The items in the list are of type "Foo". The number of items in the list
 * appears parenthetically after the type.  ("numItems" in our example.)
 * Any other information needed to parse the list is also passed in the
 * parentheses. (E.g., "tableType" in a list of table entries.)
 *
 * A CLISTof list is the same, except that the first 4 bytes of the list
 * indicate the number of items in the list.  The length may need to be
 * byte-swapped.
 *
 * A SINGLE item of an indeterminate length is indicated in the same
 * manner.  (E.g., a "SINGLE TableEntry()".) Any other information
 * needed to parse the item is also passed in the parentheses.
 * (E.g., "itemMask" in a set of pipeline context attributes.)
 *
 * If no information is given in the parentheses, then the size is
 * implicit.
 *
 * Variable length padding is noted with a comment, with the number
 * of bytes of padding required as calculated from the value in
 * the parentheses.  (number of bytes of padding = n?(3-((n-1)%4):0 , where
 * n is the parenthetical value.)
 *)

(* Matches revision 5.0P *)

(****************************************************************
 *  		REPLIES 					*
 ****************************************************************)

TYPE

  pexGetExtensionInfoReply = RECORD
    type           : BYTE;			(* X_Reply *)
    what           : CARD8;			(* unused *)
    sequenceNumber : CARD16;
    length         : CARD32;			(* not 0 *)
    majorVersion   : CARD16;
    minorVersion   : CARD16;
    release        : CARD32;
    lengthName     : CARD32;
    subsetInfo     : CARD32;
    pad            : ARRAY [0 .. 7] OF BYTE;
    (* LISTof CARD8 follows -- Don't swap *)
    (* pad *)
  END; (* pexGetExtensionInfoReply *)

  pexGetEnumeratedTypeInfoReply = RECORD
    type           : BYTE;	(* X_Reply *)
    what           : CARD8;	(* unused *)
    sequenceNumber : CARD16;
    length         : CARD32;	(* NOT 0; this is an extra-large reply*)
    numLists       : CARD32;
    pad            : ARRAY [0 .. 19] OF BYTE;	
				(* lists of lists begin afterwards *)
    (* LISTof CLISTof pexEnumTypeDesc( numLists ) *)
    (* pad *)
  END; (* pexGetEnumeratedTypeInfoReply *)

  pexGetImpDepConstantsReply = RECORD
    type           : BYTE;			(* X_Reply *)
    what           : CARD8;			(* unused *)
    sequenceNumber : CARD16;
    length         : CARD32;			(* not 0 *)
    pad            : ARRAY [0 .. 23] OF BYTE;
    (* LISTof VALUE() *)
  END; (* pexGetImpDepConstantsReply *)

  pexGetTableInfoReply = RECORD
    type             : BYTE;			(* X_Reply *)
    what             : CARD8;			(* unused *)
    sequenceNumber   : CARD16;
    length           : CARD32;			(* 0 *)
    unused           : CARD16;
    definableEntries : CARD16;
    numPredefined    : CARD16;
    predefinedMin    : CARD16;
    predefinedMax    : CARD16;
    pad              : ARRAY [0 .. 13] OF BYTE;
  END; (* pexGetTableInfoReply *)

  pexGetPredefinedEntriesReply = RECORD
    type           : BYTE;			(* X_Reply *)
    what           : CARD8;			(* unused *)
    sequenceNumber : CARD16;
    length         : CARD32;			(* not 0 *)
    unused         : CARD32;
    numEntries     : CARD32;
    pad            : ARRAY [0 .. 15] OF BYTE;
    (* LISTof TableEntry( numEntries, tableType ) *)
  END; (* pexGetPredefinedEntriesReply *)

  pexGetDefinedIndicesReply = RECORD
    type           : BYTE;			(* X_Reply *)
    what           : CARD8;			(* unused *)
    sequenceNumber : CARD16;
    length         : CARD32;			(* not 0 *)
    numIndices     : CARD32;
    pad            : ARRAY [0 .. 19] OF BYTE;
    (* LISTof pexTableIndex( numIndices ) *)
    (* pad( numIndices ) *)
  END; (* pexGetDefinedIndicesReply *)

  pexGetTableEntryReply = RECORD
    type           : BYTE;			(* X_Reply *)
    what           : CARD8;			(* unused *)
    sequenceNumber : CARD16;
    length         : CARD32;			(* not 0 *)
    status         : CARD16;
    tableType      : CARD16;
    pad            : ARRAY [0 .. 19] OF BYTE;
    (* SINGLE TableEntry( tableType )  *)
  END; (* pexGetTableEntryReply *)

  pexGetTableEntriesReply = RECORD
    type           : BYTE;			(* X_Reply *)
    what           : CARD8;			(* unused *)
    sequenceNumber : CARD16;
    length         : CARD32;			(* not 0 *)
    tableType      : CARD16;
    unused         : CARD16;
    numEntries     : CARD32;
    pad            : ARRAY [0 .. 15] OF BYTE;
    (* LISTof TableEntry( numEntries, tableType ) *)
  END; (* pexGetTableEntriesReply *)

  pexGetPipelineContextReply = RECORD
    type           : BYTE;			(* X_Reply *)
    what           : CARD8;			(* unused *)
    sequenceNumber : CARD16;
    length         : CARD32;			(* not 0 *)
    pad            : ARRAY [0 .. 23] OF BYTE;
    (* SINGLE PipelineContextAttributes( itemMask )  *)
  END; (* pexGetPipelineContextReply *)

  pexGetRendererAttributesReply = RECORD
    type           : BYTE;			(* X_Reply *)
    what           : CARD8;			(* unused *)
    sequenceNumber : CARD16;
    length         : CARD32;			(* not 0 *)
    pad            : ARRAY [0 .. 23] OF BYTE;
    (* SINGLE RendererAttributes( itemMask ) *)
  END; (* pexGetRendererAttributesReply *)

  pexGetRendererDynamicsReply = RECORD
    type           : BYTE;			(* X_Reply *)
    what           : CARD8;			(* unused *)
    sequenceNumber : CARD16;
    length         : CARD32;			(* 0 *)
    tables         : pexBitmask;
    namesets       : pexBitmask;
    attributes     : pexBitmask;
    pad            : ARRAY [0 .. 11] OF BYTE;
  END; (* pexGetRendererDynamicsReply *)

  pexGetStructureInfoReply = RECORD
    type            : BYTE;			(* X_Reply *)
    what            : CARD8;			(* unused *)
    sequenceNumber  : CARD16;
    length          : CARD32;			(* 0 *)
    editMode        : CARD16;
    unused          : CARD16;
    elementPtr      : CARD32;
    numElements     : CARD32;
    lengthStructure : CARD32;
    hasRefs         : CARD16;
    pad             : ARRAY [0 .. 5] OF BYTE;
  END; (* pexGetStructureInfoReply *)

  pexGetElementInfoReply = RECORD
    type           : BYTE;			(* X_Reply *)
    what           : CARD8;			(* unused *)
    sequenceNumber : CARD16;
    length         : CARD32;			(* not 0 *)
    numInfo        : CARD32;
    pad            : ARRAY [0 .. 19] OF BYTE;
    (* LISTof pexElementInfo( numInfo ) *)
  END; (* pexGetElementInfoReply *)

  pexGetStructuresInNetworkReply = RECORD
    type           : BYTE;			(* X_Reply *)
    what           : CARD8;			(* unused *)
    sequenceNumber : CARD16;
    length         : CARD32;			(* not 0 *)
    unused         : ARRAY [0 .. 7] OF CARD8;
    numStructures  : CARD32;
    pad            : ARRAY [0 .. 11] OF BYTE;
    (* LISTof pexStructure( numStructures )  *)
  END; (* pexGetStructuresInNetworkReply *)

  pexGetAncestorsReply = RECORD
    type           : BYTE;			(* X_Reply *)
    what           : CARD8;			(* unused *)
    sequenceNumber : CARD16;
    length         : CARD32;			(* not 0 *)
    unused         : ARRAY [0 .. 11] OF CARD8;
    numPaths       : CARD32;
    pad            : ARRAY [0 .. 7] OF BYTE;
    (* LISTof CLISTof pexElementRef( numPaths ) *)
  END; (* pexGetAncestorsReply *)

  pexGetDescendantsReply = pexGetAncestorsReply;

  pexFetchElementsReply = RECORD
    type           : BYTE;			(* X_Reply *)
    what           : CARD8;			(* unused *)
    sequenceNumber : CARD16;
    length         : CARD32;			(* not 0 *)
    numElements    : CARD32;
    pad            : ARRAY [0 .. 19] OF BYTE;
    (* LISTof OutputCommand( numElements ) *)
  END; (* pexFetchElementsReply *)

  pexElementSearchReply = RECORD
    type           : BYTE;			(* X_Reply *)
    what           : CARD8;			(* unused *)
    sequenceNumber : CARD16;
    length         : CARD32;			(* 0 *)
    status         : CARD16;
    unused         : CARD16;
    foundOffset    : CARD32;
    pad            : ARRAY [0 .. 15] OF BYTE;
  END; (* pexElementSearchReply *)

  pexGetNameSetReply = RECORD
    type           : BYTE;			(* X_Reply *)
    what           : CARD8;			(* unused *)
    sequenceNumber : CARD16;
    length         : CARD32;			(* not 0 *)
    numNames       : CARD32;
    pad            : ARRAY [0 .. 19] OF BYTE;
    (* LISTof pexName( numNames ) *)
  END; (* pexGetNameSetReply *)

  pexGetSearchContextReply = RECORD
    type           : BYTE;			(* X_Reply *)
    what           : CARD8;			(* unused *)
    sequenceNumber : CARD16;
    length         : CARD32;			(* not 0 *)
    pad            : ARRAY [0 .. 23] OF BYTE;
    (* SINGLE SearchContextAttributes( itemMask ) *)
  END; (* pexGetSearchContextReply *)

  pexSearchNetworkReply = RECORD
    type           : BYTE;			(* X_Reply *)
    what           : CARD8;			(* unused *)
    sequenceNumber : CARD16;
    length         : CARD32;			(* not 0 *)
    unused         : CARD32;
    numItems       : CARD32;
    pad            : ARRAY [0 .. 15] OF BYTE;
    (* LISTof pexElementRef( numItems ) *)
  END; (* pexSearchNetworkReply *)

  pexGetWksInfoReply = RECORD
    type           : BYTE;			(* X_Reply *)
    what           : CARD8;			(* unused *)
    sequenceNumber : CARD16;
    length         : CARD32;			(* not 0 *)
    pad            : ARRAY [0 .. 23] OF BYTE;
    (* SINGLE WksInfo( itemMask ) *)
  END; (* pexGetWksInfoReply *)

  pexGetDynamicsReply = RECORD
    type               : BYTE;			(* X_Reply *)
    what               : CARD8;			(* unused *)
    sequenceNumber     : CARD16;
    length             : CARD32;		(* 0 *)
    viewRep            : pexDynamicType;
    markerBundle       : pexDynamicType;
    textBundle         : pexDynamicType;
    lineBundle         : pexDynamicType;
    interiorBundle     : pexDynamicType;
    edgeBundle         : pexDynamicType;
    colourTable        : pexDynamicType;
    patternTable       : pexDynamicType;
    wksTransform       : pexDynamicType;
    highlightFilter    : pexDynamicType;
    invisibilityFilter : pexDynamicType;
    HlhsrMode          : pexDynamicType;
    structureModify    : pexDynamicType;
    postStructure      : pexDynamicType;
    unpostStructure    : pexDynamicType;
    deleteStructure    : pexDynamicType;
    referenceModify    : pexDynamicType;
    bufferModify       : pexDynamicType;
    lightTable         : pexDynamicType;
    depthCueTable      : pexDynamicType;
    colourApproxTable  : pexDynamicType;
    pad                : ARRAY [0 .. 2] OF CARD8;
  END; (* pexGetDynamicsReply *)

  pexGetViewRepReply = RECORD
    type           : BYTE;			(* X_Reply *)
    what           : CARD8;			(* unused *)
    sequenceNumber : CARD16;
    length         : CARD32;			(* 4 + 76*fp/4 *)
    viewUpdate     : CARD16;			(* Pending, NotPending *) 
    pad            : ARRAY [0 .. 21] OF BYTE;
    (* SINGLE pexViewRep() 	requested *)
    (* SINGLE pexViewRep() 	current *)
  END; (* pexGetViewRepReply *)

  pexMapDCtoWCReply = RECORD
    type           : BYTE;			(* X_Reply *)
    what           : CARD8;			(* unused *)
    sequenceNumber : CARD16;
    length         : CARD32;			(* not 0 *)
    viewIndex      : CARD16;
    unused         : CARD16;
    numCoords      : CARD32;
    pad            : ARRAY [0 .. 15] OF BYTE;
    (* LISTof pexCoord3D( numCoords ) *)
  END; (* pexMapDCtoWCReply *)

  pexMapWCtoDCReply = RECORD
    type           : BYTE;			(* X_Reply *)
    what           : CARD8;			(* unused *)
    sequenceNumber : CARD16;
    length         : CARD32;			(* not 0 *)
    unused         : CARD32;
    numCoords      : CARD32;
    pad            : ARRAY [0 .. 15] OF BYTE;
    (* LISTof pexDeviceCoord( numCoords ) *)
  END; (* pexMapWCtoDCReply *)

  pexGetWksPostingsReply = RECORD
    type           : BYTE;			(* X_Reply *)
    what           : CARD8;			(* unused *)
    sequenceNumber : CARD16;
    length         : CARD32;			(* not 0 *)
    pad            : ARRAY [0 .. 23] OF BYTE;
    (* LISTof pexPhigsWksID() *)
  END; (* pexGetWksPostingsReply *)

  pexGetPickDeviceReply = RECORD
    type           : BYTE;			(* X_Reply *)
    what           : CARD8;			(* unused *)
    sequenceNumber : CARD16;
    length         : CARD32;			(* not 0 *)
    pad            : ARRAY [0 .. 23] OF BYTE;
    (* SINGLE PickDeviceAttributes( itemMask ) *)
  END; (* pexGetPickDeviceReply *)

  pexGetPickMeasureReply = RECORD
    type           : BYTE;			(* X_Reply *)
    what           : CARD8;			(* unused *)
    sequenceNumber : CARD16;
    length         : CARD32;			(* not 0 *)
    pad            : ARRAY [0 .. 23] OF BYTE;
    (* SINGLE pexPickMeasureAttributes( itemMask ) *)
  END; (* pexGetPickMeasureReply *)

  pexQueryFontReply = RECORD
    type           : BYTE;			(* X_Reply *)
    what           : CARD8;			(* unused *)
    sequenceNumber : CARD16;
    length         : CARD32;			(* not 0 *)
    lengthFontInfo : CARD32;
    pad            : ARRAY [0 .. 19] OF CARD8;
    (* SINGLE pexFontInfo() *)
  END; (* pexQueryFontReply *)

  pexListFontsReply = RECORD
    type           : BYTE;			(* X_Reply *)
    what           : CARD8;			(* unused *)
    sequenceNumber : CARD16;
    length         : CARD32;			(* not 0 *)
    numStrings     : CARD32;
    pad            : ARRAY [0 .. 19] OF BYTE;
    (* LISTof pexString( numStrings ) *)
  END; (* pexListFontsReply *)

  pexListFontsWithInfoReply = RECORD
    type           : BYTE;			(* X_Reply *)
    what           : CARD8;			(* unused *)
    sequenceNumber : CARD16;
    length         : CARD32;			(* not 0 *)
    numStrings     : CARD32;
    pad            : ARRAY [0 .. 19] OF BYTE;
    (* LISTof pexString( numStrings ) *)
    (* CLISTof pexFontInfo() *)
  END; (* pexListFontsWithInfoReply *)

  pexQueryTextExtentsReply = RECORD
    type           : BYTE;			(* X_Reply *)
    what           : CARD8;			(* unused *)
    sequenceNumber : CARD16;
    length         : CARD32;			(* not 0 *)
    pad            : ARRAY [0 .. 23] OF BYTE;
    (* LISTof ExtentInfo() *)
  END; (* pexQueryTextExtentsReply *)

(****************************************************************
 *  		REQUESTS 					*
 ****************************************************************)

(* Request structure *)

  pexReq = RECORD
    reqType : CARD8;
    opcode 	: CARD8;	(* meaning depends on request type *)
    length 	: CARD16;       (* length in 4 bytes quantities of *)
                                (* whole request, including this header *)
  END; (* pexReq *)

(*****************************************************************
 *  structures that follow request.
 *****************************************************************)

(* ResourceReq is used for any request which has a resource ID
   ( or Atom or Time ) as its one and only argument.  *)

  pexResourceReq = RECORD
    reqType : CARD8;
    opcode  : CARD8;
    length  : CARD16;		(* 2 *)
    id      : CARD32;		(* a Structure, Renderer, Font, LUT, etc. *)
  END; (* pexResourceReq *)


(*****************************************************************
 *  Specific Requests 
 *****************************************************************)

  pexGetExtensionInfoReq = RECORD
    reqType             : CARD8;
    opcode              : CARD8;
    length              : CARD16;		(* 2 *)
    clientProtocolMajor : CARD16;
    clientProtocolMinor : CARD16;
  END; (* pexGetExtensionInfoReq *)

  pexGetEnumeratedTypeInfoReq = RECORD
    reqType  : CARD8;
    opcode   : CARD8;
    length   : CARD16;
    drawable : Drawable;
    itemMask : pexBitmask;
    numEnums : CARD32;
    (* LISTof CARD16( numEnums ) *)
    (* pad( numEnums*2 ) *)
  END; (* pexGetEnumeratedTypeInfoReq *)

  pexGetImpDepConstantsReq = RECORD
    reqType  : CARD8;
    opcode   : CARD8;
    length   : CARD16;
    fpFormat : pexEnumTypeIndex;
    unused   : CARD16;
    drawable : Drawable;
    numNames : CARD32;
    (* LISTof pexImpDepConstantNames ( numNames )  *)
    (* pad *)
  END; (* pexGetImpDepConstantsReq *)

  pexCreateLookupTableReq = RECORD
    reqType         : CARD8;
    opcode          : CARD8;
    length          : CARD16;		(* 4 *)
    drawableExample : Drawable;
    lut             : pexLookupTable;
    tableType       : pexTableType;
    unused          : CARD16;
  END; (* pexCreateLookupTableReq *)

  pexCopyLookupTableReq = RECORD
    reqType : CARD8;
    opcode  : CARD8;
    length  : CARD16;			(* 3 *)
    src     : pexLookupTable;
    dst     : pexLookupTable;
  END; (* pexCopyLookupTableReq *)

  pexFreeLookupTableReq = pexResourceReq;

  pexGetTableInfoReq = RECORD
    reqType         : CARD8;
    opcode          : CARD8;
    length          : CARD16;		(* 3 *)
    drawableExample : Drawable;
    tableType       : pexTableType;
    unused          : CARD16;
  END; (* pexGetTableInfoReq *)


  pexGetPredefinedEntriesReq = RECORD
    reqType         : CARD8;
    opcode          : CARD8;
    length          : CARD16;		(* 5 *)
    fpFormat        : pexEnumTypeIndex;
    unused          : CARD16;
    drawableExample : Drawable;
    tableType       : pexTableType;
    start           : pexTableIndex;
    count           : CARD16;
    pad             : CARD16;
  END; (* pexGetPredefinedEntriesReq *)

  pexGetDefinedIndicesReq = pexResourceReq;

  pexGetTableEntryReq = RECORD
    reqType   : CARD8;
    opcode    : CARD8;
    length    : CARD16;			(* 4 *)
    fpFormat  : pexEnumTypeIndex;
    valueType : CARD16;
    lut       : pexLookupTable;
    index     : pexTableIndex;
    pad       : CARD16;
  END; (* pexGetTableEntryReq *)

  pexGetTableEntriesReq = RECORD
    reqType   : CARD8;
    opcode    : CARD8;
    length    : CARD16;			(* 4 *)
    fpFormat  : pexEnumTypeIndex;
    valueType : CARD16;
    lut       : pexLookupTable;
    start     : pexTableIndex;
    count     : CARD16;
  END; (* pexGetTableEntriesReq *)

  pexSetTableEntriesReq = RECORD
    reqType  : CARD8;
    opcode   : CARD8;
    length   : CARD16;
    fpFormat : pexEnumTypeIndex;
    unused   : CARD16;
    lut      : pexLookupTable;
    start    : pexTableIndex;
    count    : CARD16;
    (*    LISTof TableEntry( count ) *)
  END; (* pexSetTableEntriesReq *)

  pexDeleteTableEntriesReq = RECORD
    reqType : CARD8;
    opcode  : CARD8;
    length  : CARD16;			(* 3 *)
    lut     : pexLookupTable;
    start   : pexTableIndex;
    count   : CARD16;
  END; (* pexDeleteTableEntriesReq *)

  pexCreatePipelineContextReq = RECORD
    reqType  : CARD8;
    opcode   : CARD8;
    length   : CARD16;			(* 6 *)
    fpFormat : pexEnumTypeIndex;
    unused   : CARD16;
    pc       : pexPC;
    itemMask : ARRAY [0 .. 2 ] OF pexBitmask; (* pexBitmask Array *)
    (* SINGLE PipelineContextAttributes( itemMask ) *)
  END; (* pexCreatePipelineContextReq *)

  pexCopyPipelineContextReq = RECORD
    reqType  : CARD8;
    opcode   : CARD8;
    length   : CARD16;			(* 6 *)
    src      : pexPC;
    dst      : pexPC;
    itemMask : ARRAY [0 .. 2 ] OF pexBitmask; (* pexBitmask Array *)
  END; (* pexCopyPipelineContextReq *)

  pexFreePipelineContextReq = pexResourceReq;

  pexGetPipelineContextReq = RECORD
    reqType  : CARD8;
    opcode   : CARD8;
    length   : CARD16;			(* 6 *)
    fpFormat : pexEnumTypeIndex;
    unused   : CARD16;
    pc       : pexPC;
    itemMask : ARRAY [0 .. 2 ] OF pexBitmask; (* pexBitmask Array *)
  END; (* pexGetPipelineContextReq *)

  pexChangePipelineContextReq = RECORD
    reqType  : CARD8;
    opcode   : CARD8;
    length   : CARD16;
    fpFormat : pexEnumTypeIndex;
    unused   : CARD16;
    pc       : pexPC;
    itemMask : ARRAY [0 .. 2 ] OF pexBitmask; (* pexBitmask Array *)
    (* SINGLE PipelineContextAttributes( itemMask ) *)
  END; (* pexChangePipelineContextReq *)

  pexCreateRendererReq = RECORD
    reqType  : CARD8;
    opcode   : CARD8;
    length   : CARD16;
    fpFormat : pexEnumTypeIndex;
    unused   : CARD16;
    rdr      : pexRenderer;
    drawable : Drawable;
    itemMask : pexBitmask;
    (* SINGLE RendererAttributes( itemMask ) *)
  END; (* pexCreateRendererReq *)

  pexFreeRendererReq = pexResourceReq;

  pexChangeRendererReq = RECORD
    reqType  : CARD8;
    opcode   : CARD8;
    length   : CARD16;
    fpFormat : pexEnumTypeIndex;
    unused   : CARD16;
    rdr      : pexRenderer;
    itemMask : pexBitmask;
    (* SINGLE RendererAttributes( itemMask ) *)
  END; (* pexChangeRendererReq *)

  pexGetRendererAttributesReq = RECORD
    reqType  : CARD8;
    opcode   : CARD8;
    length   : CARD16;			(* 4 *)
    fpFormat : pexEnumTypeIndex;
    unused   : CARD16;
    rdr      : pexRenderer;
    itemMask : pexBitmask;
  END; (* pexGetRendererAttributesReq *)

  pexGetRendererDynamicsReq = pexResourceReq;

  pexBeginRenderingReq = RECORD
    reqType  : CARD8;
    opcode   : CARD8;
    length   : CARD16;			(* 3 *)
    rdr      : pexRenderer;
    drawable : Drawable;
  END; (* pexBeginRenderingReq *)

  pexEndRenderingReq = RECORD
    reqType   : CARD8;
    opcode    : CARD8;
    length    : CARD16;			(* 3 *)
    rdr       : pexRenderer;
    flushFlag : pexSwitch;
    pad       : ARRAY [0 .. 2] OF BYTE;
  END; (* pexEndRenderingReq *)

  pexBeginStructureReq = RECORD
    reqType : CARD8;
    opcode  : CARD8;
    length  : CARD16;			(* 3 *)
    rdr     : pexRenderer;
    sid     : pexStructure;
  END; (* pexBeginStructureReq *)

  pexEndStructureReq = pexResourceReq;

  pexRenderOutputCommandsReq = RECORD
    reqType     : CARD8;
    opcode      : CARD8;
    length      : CARD16;
    fpFormat    : pexEnumTypeIndex;
    unused      : CARD16;
    rdr         : pexRenderer;
    numCommands : CARD32;
    (* LISTof OutputCommand( numCommands ) *)
  END; (* pexRenderOutputCommandsReq *)

(* individual output commands may be found in the section "Output Commands" *)


  pexRenderNetworkReq = RECORD
    reqType  : CARD8;
    opcode   : CARD8;
    length   : CARD16;			(* 4 *)
    rdr      : pexRenderer;
    drawable : Drawable;
    sid      : pexStructure;
  END; (* pexRenderNetworkReq *)

  pexCreateStructureReq = pexResourceReq;

  pexCopyStructureReq = RECORD
    reqType : CARD8;
    opcode  : CARD8;
    length  : CARD16;			(* 3 *)
    src     : pexStructure;
    dst     : pexStructure;
  END; (* pexCopyStructureReq *)

  pexDestroyStructuresReq = RECORD
    reqType       : CARD8;
    opcode        : CARD8;
    length        : CARD16;
    numStructures : CARD32;
    (* LISTof pexStructure( numStructures ) *)
  END; (* pexDestroyStructuresReq *)

  pexGetStructureInfoReq = RECORD
    reqType  : CARD8;
    opcode   : CARD8;
    length   : CARD16;			(* 3 *)
    fpFormat : pexEnumTypeIndex;
    itemMask : CARD16;
    sid      : pexStructure;
  END; (* pexGetStructureInfoReq *)

  pexGetElementInfoReq = RECORD
    reqType  : CARD8;
    opcode   : CARD8;
    length   : CARD16;			(* 7 *)
    fpFormat : pexEnumTypeIndex;
    unused   : CARD16;
    sid      : pexStructure;
    range    : BITS BITSIZE (pexElementRange) FOR pexElementRange;
  END; (* pexGetElementInfoReq *)

  pexGetStructuresInNetworkReq = RECORD
    reqType : CARD8;
    opcode  : CARD8;
    length  : CARD16;			(* 3 *)
    sid     : pexStructure;
    which   : CARD16;
    pad     : CARD16;
  END; (* pexGetStructuresInNetworkReq *)

  pexGetAncestorsReq = RECORD
    reqType   : CARD8;
    opcode    : CARD8;
    length    : CARD16;			(* 4 *)
    sid       : pexStructure;
    pathOrder : CARD16;
    unused    : CARD16;
    pathDepth : CARD32;
  END; (* pexGetAncestorsReq *)

  pexGetDescendantsReq = pexGetAncestorsReq;

  pexFetchElementsReq = RECORD
    reqType  : CARD8;
    opcode   : CARD8;
    length   : CARD16;			(* 7 *)
    fpFormat : pexEnumTypeIndex;
    unused   : CARD16;
    sid      : pexStructure;
    range    : BITS BITSIZE (pexElementRange) FOR pexElementRange;
  END; (* pexFetchElementsReq *)

  pexSetEditingModeReq = RECORD
    reqType : CARD8;
    opcode  : CARD8;
    length  : CARD16;			(* 3 *)
    sid     : pexStructure;
    mode    : CARD16;
    pad     : CARD16;
  END; (* pexSetEditingModeReq *)

  pexSetElementPointerReq = RECORD
    reqType  : CARD8;
    opcode   : CARD8;
    length   : CARD16;			(* 4 *)
    sid      : pexStructure;
    position : pexElementPos;
  END; (* pexSetElementPointerReq *)

  pexSetElementPointerAtLabelReq = RECORD
    reqType : CARD8;
    opcode  : CARD8;
    length  : CARD16;			(* 4 *)
    sid     : pexStructure;
    label   : INT32;
    offset  : INT32;
  END; (* pexSetElementPointerAtLabelReq *)

  pexElementSearchReq = RECORD
    reqType   : CARD8;
    opcode    : CARD8;
    length    : CARD16;
    sid       : pexStructure;
    position  : BITS BITSIZE (pexElementPos) FOR pexElementPos;
    direction : CARD32;
    numIncls  : CARD32;
    numExcls  : CARD32;
    (* LISTof CARD16( numIncls ) *)
    (* pad( numIncls*2 ) *)
    (* LISTof CARD16( numExcls ) *)
    (* pad( numExcls*2 ) *)
  END; (* pexElementSearchReq *)

  pexStoreElementsReq = RECORD
    reqType     : CARD8;
    opcode      : CARD8;
    length      : CARD16;
    fpFormat    : pexEnumTypeIndex;
    unused      : CARD16;
    sid         : pexStructure;
    numCommands : CARD32;
    (* LISTof OutputCommand( numCommands ) *)
  END; (* pexStoreElementsReq *)

  pexDeleteElementsReq = RECORD
    reqType : CARD8;
    opcode  : CARD8;
    length  : CARD16;			(* 6 *)
    sid     : pexStructure;
    range   : pexElementRange;
  END; (* pexDeleteElementsReq *)

  pexDeleteElementsToLabelReq = RECORD
    reqType  : CARD8;
    opcode   : CARD8;
    length   : CARD16;			(* 5 *)
    sid      : BITS BITSIZE (pexStructure)  FOR pexStructure;
    position : BITS BITSIZE (pexElementPos) FOR pexElementPos;
    label    : INT32;
  END; (* pexDeleteElementsToLabelReq *)

  pexDeleteBetweenLabelsReq = RECORD
    reqType : CARD8;
    opcode  : CARD8;
    length  : CARD16;			(* 4 *)
    sid     : pexStructure;
    label1  : INT32;
    label2  : INT32;
  END; (* pexDeleteBetweenLabelsReq *)

  pexCopyElementsReq = RECORD
    reqType     : CARD8;
    opcode      : CARD8;
    length      : CARD16;		(* 9 *)
    src         : BITS BITSIZE (pexStructure)    FOR pexStructure;
    srcRange    : BITS BITSIZE (pexElementRange) FOR pexElementRange;
    dst         : BITS BITSIZE (pexStructure)    FOR pexStructure;
    dstPosition : BITS BITSIZE (pexElementPos)   FOR pexElementPos;
  END; (* pexCopyElementsReq *)

  pexChangeStructureRefsReq = RECORD
    reqType  : CARD8;
    opcode   : CARD8;
    length   : CARD16;			(* 3 *)
    old_id   : pexStructure;
    new_id   : pexStructure;
  END; (* pexChangeStructureRefsReq *)

  pexCreateNameSetReq = pexResourceReq;

  pexCopyNameSetReq = RECORD
    reqType : CARD8;
    opcode  : CARD8;
    length  : CARD16;			(* 3 *)
    src     : pexNameSet;
    dst     : pexNameSet;
  END; (* pexCopyNameSetReq *)

  pexFreeNameSetReq = pexResourceReq;

  pexGetNameSetReq = pexResourceReq;

  pexChangeNameSetReq = RECORD
    reqType : CARD8;
    opcode  : CARD8;
    length  : CARD16;
    ns      : pexNameSet;
    action  : CARD16;
    unused  : CARD16;
    (* LISTof pexName() *)
  END; (* pexChangeNameSetReq *)

  pexCreateSearchContextReq = RECORD
    reqType  : CARD8;
    opcode   : CARD8;
    length   : CARD16;
    fpFormat : pexEnumTypeIndex;
    unused   : CARD16;
    sc       : pexSC;
    itemMask : pexBitmask;
    (* SINGLE SearchContextAttributes( itemMask ) *)
  END; (* pexCreateSearchContextReq *)

  pexCopySearchContextReq = RECORD
    reqType  : CARD8;
    opcode   : CARD8;
    length   : CARD16;			(* 4 *)
    src      : pexSC;
    dst      : pexSC;
    itemMask : pexBitmask;
  END; (* pexCopySearchContextReq *)

  pexFreeSearchContextReq = pexResourceReq;

  pexGetSearchContextReq = RECORD
    reqType  : CARD8;
    opcode   : CARD8;
    length   : CARD16;			(* 4 *)
    fpFormat : pexEnumTypeIndex;
    unused   : CARD16;
    sc       : pexSC;
    itemMask : pexBitmask;
  END; (* pexGetSearchContextReq *)

  pexChangeSearchContextReq = RECORD
    reqType  : CARD8;
    opcode   : CARD8;
    length   : CARD16;
    fpFormat : pexEnumTypeIndex;
    unused   : CARD16;
    sc       : pexSC;
    itemMask : pexBitmask;
    (* SINGLE SearchContextAttributes( itemMask ) *)
  END; (* pexChangeSearchContextReq *)

  pexSearchNetworkReq = pexResourceReq;

  pexCreatePhigsWksReq = RECORD
    reqType           : CARD8;
    opcode            : CARD8;
    length            : CARD16;		(* 19 *)
    wks               : pexPhigsWks;
    drawable          : Drawable;
    markerBundle      : pexLookupTable;
    textBundle        : pexLookupTable;
    lineBundle        : pexLookupTable;
    interiorBundle    : pexLookupTable;
    edgeBundle        : pexLookupTable;
    colourTable       : pexLookupTable;
    depthCueTable     : pexLookupTable;
    lightTable        : pexLookupTable;
    colourApproxTable : pexLookupTable;
    patternTable      : pexLookupTable;
    textFontTable     : pexLookupTable;
    highlightIncl     : pexNameSet;
    highlightExcl     : pexNameSet;
    invisIncl         : pexNameSet;
    invisExcl         : pexNameSet;
    bufferMode        : CARD16;
    pad               : CARD16;
  END; (* pexCreatePhigsWksReq *)

  pexFreePhigsWksReq = pexResourceReq;

  pexGetWksInfoReq = RECORD
    reqType  : CARD8;
    opcode   : CARD8;
    length   : CARD16;			(* 5 *)
    fpFormat : pexEnumTypeIndex;
    unused   : CARD16;
    wks      : pexPhigsWks;
    itemMask : ARRAY [0 .. 1] OF pexBitmask;
  END; (* pexGetWksInfoReq *)

  pexGetDynamicsReq = RECORD
    reqType  : CARD8;
    opcode   : CARD8;
    length   : CARD16;			(* 2 *)
    drawable : Drawable;
  END; (* pexGetDynamicsReq *)

  pexGetViewRepReq = RECORD
    reqType  : CARD8;
    opcode   : CARD8;
    length   : CARD16;			(* 3 *)
    fpFormat : pexEnumTypeIndex;
    index    : pexTableIndex;
    wks      : pexPhigsWks;
  END; (* pexGetViewRepReq *)

  pexRedrawAllStructuresReq = pexResourceReq;	

  pexUpdateWorkstationReq = pexResourceReq;

  pexRedrawClipRegionReq = RECORD
    reqType  : CARD8;
    opcode   : CARD8;
    length   : CARD16;
    wks      : pexPhigsWks;
    numRects : CARD32;
    (* LISTof pexDeviceRect( numRects ) *)
  END; (* pexRedrawClipRegionReq *)

  pexExecuteDeferredActionsReq = pexResourceReq;

  pexSetViewPriorityReq = RECORD
    reqType  : CARD8;
    opcode   : CARD8;
    length   : CARD16;			(* 4 *)
    wks      : pexPhigsWks;
    index1   : pexTableIndex;
    index2   : pexTableIndex;
    priority : CARD16;
    pad      : CARD16;
  END; (* pexSetViewPriorityReq *)

  pexSetDisplayUpdateModeReq = RECORD
    reqType       : CARD8;
    opcode        : CARD8;
    length        : CARD16;		(* 3 *)
    wks           : pexPhigsWks;
    displayUpdate : pexEnumTypeIndex;
    pad           : CARD16;
  END; (* pexSetDisplayUpdateModeReq *)

  pexMapDCtoWCReq = RECORD
    reqType   : CARD8;
    opcode    : CARD8;
    length    : CARD16;
    fpFormat  : pexEnumTypeIndex;
    unused    : CARD16;
    wks       : pexPhigsWks;
    numCoords : CARD32;
    (* LISTof pexDeviceCoord( numCoords ) *)
  END; (* pexMapDCtoWCReq *)

  pexMapWCtoDCReq = RECORD
    reqType   : CARD8;
    opcode    : CARD8;
    length    : CARD16;
    fpFormat  : pexEnumTypeIndex;
    index     : CARD16;
    wks       : pexPhigsWks;
    numCoords : CARD32;
    (* LISTof pexCoord3D( numCoords ) *)
  END; (* pexMapWCtoDCReq *)

  pexSetViewRepReq = RECORD
    reqType  : CARD8;
    opcode   : CARD8;
    length   : CARD16;			(* 43 *)
    fpFormat : pexEnumTypeIndex;
    unused   : CARD16;
    wks      : pexPhigsWks;
    viewRep  : BITS BITSIZE (pexViewRep) FOR pexViewRep;
  END; (* pexSetViewRepReq *)

  pexSetWksWindowReq = RECORD
    reqType      : CARD8;
    opcode       : CARD8;
    length       : CARD16;		(* 9 *)
    fpFormat     : pexEnumTypeIndex;
    unused       : CARD16;
    wks          : pexPhigsWks;
    npcSubvolume : BITS BITSIZE (pexNpcSubvolume) FOR pexNpcSubvolume;
  END; (* pexSetWksWindowReq *)

  pexSetWksViewportReq = RECORD
    reqType  : CARD8;
    opcode   : CARD8;
    length   : CARD16;			(* 8 *)
    fpFormat : pexEnumTypeIndex;
    unused   : CARD16;
    wks      : pexPhigsWks;
    viewport : pexViewport;
  END; (* pexSetWksViewportReq *)

  pexSetHlhsrModeReq = RECORD
    reqType : CARD8;
    opcode  : CARD8;
    length  : CARD16;			(* 3 *)
    wks     : pexPhigsWks;
    mode    : pexEnumTypeIndex;
    pad     : CARD16;
  END; (* pexSetHlhsrModeReq *)

  pexSetWksBufferModeReq = RECORD
    reqType    : CARD8;
    opcode     : CARD8;
    length     : CARD16;		(* 3 *)
    wks        : pexPhigsWks;
    bufferMode : CARD16;
    pad        : CARD16;
  END; (* pexSetWksBufferModeReq *)

  pexPostStructureReq = RECORD
    reqType  : CARD8;
    opcode   : CARD8;
    length   : CARD16;			(* 5 *)
    fpFormat : pexEnumTypeIndex;
    unused   : CARD16;
    wks      : pexPhigsWks;
    sid      : pexStructure;
    priority : Ctypes.float;
  END; (* pexPostStructureReq *)

  pexUnpostStructureReq = RECORD
    reqType : CARD8;
    opcode  : CARD8;
    length  : CARD16;			(* 3 *)
    wks     : pexPhigsWks;
    sid     : pexStructure;
  END; (* pexUnpostStructureReq *)

  pexUnpostAllStructuresReq = pexResourceReq;

  pexGetWksPostingsReq = pexResourceReq;

  pexGetPickDeviceReq = RECORD
    reqType  : CARD8;
    opcode   : CARD8;
    length   : CARD16;			(* 4 *)
    fpFormat : pexEnumTypeIndex;
    devType  : pexEnumTypeIndex;
    wks      : pexPhigsWks;
    itemMask : pexBitmask;
  END; (* pexGetPickDeviceReq *)

  pexChangePickDeviceReq = RECORD
    reqType  : CARD8;
    opcode   : CARD8;
    length   : CARD16;
    fpFormat : pexEnumTypeIndex;
    unused   : CARD16;
    wks      : pexPhigsWks;
    devType  : pexEnumTypeIndex;
    unused2  : CARD16;
    itemMask : pexBitmask;
    (* SINGLE PickDeviceAttributes( itemMask ) *)
  END; (* pexChangePickDeviceReq *)

  pexCreatePickMeasureReq = RECORD
    reqType : CARD8;
    opcode  : CARD8;
    length  : CARD16;			(* 4 *)
    wks     : pexPhigsWks;
    pm      : pexPickMeasure;
    devType : pexEnumTypeIndex;
    pad     : CARD16;
  END; (* pexCreatePickMeasureReq *)

  pexFreePickMeasureReq = pexResourceReq;

  pexGetPickMeasureReq = RECORD
    reqType  : CARD8;
    opcode   : CARD8;
    length   : CARD16;			(* 3 *)
    pm       : pexPickMeasure;
    itemMask : pexBitmask;
  END; (* pexGetPickMeasureReq *)

  pexUpdatePickMeasureReq = RECORD
    reqType  : CARD8;
    opcode   : CARD8;
    length   : CARD16;
    pm       : pexPickMeasure;
    numBytes : CARD32;
    (* LISTof CARD8( numBytes ) -- don't swap *)
    (* pad( numBytes ) *)
  END; (* pexUpdatePickMeasureReq *)

  pexOpenFontReq = RECORD
    reqType  : CARD8;
    opcode   : CARD8;
    length   : CARD16;
    font     : pexFont;
    numBytes : CARD32;
    (* LISTof CARD8( numBytes ) -- don't swap *)
    (* pad( numBytes ) *)
  END; (* pexOpenFontReq *)

  pexCloseFontReq = pexResourceReq;

  pexQueryFontReq = RECORD
    reqType : CARD8;
    opcode  : CARD8;
    length  : CARD16;
    font    : pexFont;
  END; (* pexQueryFontReq *)

  pexListFontsReq = RECORD
    reqType  : CARD8;
    opcode   : CARD8;
    length   : CARD16;
    maxNames : CARD16;
    numChars : CARD16;
    (* LISTof CARD8( numChars ) -- don't swap *)
    (* pad( numBytes ) *)
  END; (* pexListFontsReq *)

  pexListFontsWithInfoReq = RECORD
    reqType  : CARD8;
    opcode   : CARD8;
    length   : CARD16;
    unused   : CARD16;
    maxNames : CARD16;
    numChars : CARD16;
    pad      : CARD16;
    (* LISTof CARD8( numChars )  *)
    (* pad( numBytes ) *)
  END; (* pexListFontsWithInfoReq *)

  pexQueryTextExtentsReq = RECORD
    reqType        : CARD8;
    opcode         : CARD8;
    length         : CARD16;
    fpFormat       : pexEnumTypeIndex;
    textPath       : CARD16;
    fontGroupIndex : pexTableIndex;
    unused         : CARD16;
    id             : CARD32;  (* was XID -- renderer, wks, or text font lut *)
    charExpansion  : Ctypes.float;
    charSpacing    : Ctypes.float;
    charHeight     : Ctypes.float;
    textAlignment  : pexTextAlignmentData;
    numStrings     : CARD32;
    (* LISTof LISTof MONO_ENCODINGS() *)
    (* pad() *)
  END; (*  pexQueryTextExtentsReq *)

(*****************************************************************
 * Output Commands 
 *****************************************************************)

  pexMarkerType = RECORD
    head       : pexElementInfo;
    markerType : pexEnumTypeIndex;
    pad        : CARD16;
  END; (* pexMarkerType *)

  pexMarkerScale = RECORD
    head  : pexElementInfo;
    scale : Ctypes.float;
  END; (* pexMarkerScale *)

  pexMarkerBundleIndex = RECORD
    head  : pexElementInfo;
    index : pexTableIndex;
    pad   : CARD16;
  END; (* pexMarkerBundleIndex *)

  pexMarkerColourIndex      = pexMarkerBundleIndex;
  pexTextColourIndex        = pexMarkerBundleIndex;
  pexLineColourIndex        = pexMarkerBundleIndex;
  pexSurfaceColourIndex     = pexMarkerBundleIndex;
  pexBfSurfaceColourIndex   = pexMarkerBundleIndex;
  pexSurfaceEdgeColourIndex = pexMarkerBundleIndex;
  pexTextFontIndex          = pexMarkerBundleIndex;

  pexMarkerColour = RECORD
    head       : pexElementInfo;
    colourSpec : pexColourSpecifier;
    (* SINGLE COLOUR() *)
  END; (* pexMarkerColour *)

  pexTextColour        = pexMarkerColour;
  pexLineColour        = pexMarkerColour;
  pexSurfaceColour     = pexMarkerColour;
  pexBfSurfaceColour   = pexMarkerColour;
  pexSurfaceEdgeColour = pexMarkerColour;

  pexAtextStyle = RECORD
    head  : pexElementInfo;
    style : pexEnumTypeIndex;
    pad   : CARD16;
  END; (* pexAtextStyle *)

  pexTextBundleIndex      = pexMarkerBundleIndex;
  pexLineBundleIndex      = pexMarkerBundleIndex;
  pexInteriorBundleIndex  = pexMarkerBundleIndex;
  pexInteriorStyleIndex   = pexMarkerBundleIndex;
  pexBfInteriorStyleIndex = pexMarkerBundleIndex;
  pexEdgeBundleIndex      = pexMarkerBundleIndex;
  pexViewIndex            = pexMarkerBundleIndex;
  pexDepthCueIndex        = pexMarkerBundleIndex;
  pexColourApproxIndex    = pexMarkerBundleIndex;

  pexTextPrecision = RECORD
    head      : pexElementInfo;
    precision : CARD16;
    pad       : CARD16;
  END; (* pexTextPrecision *)

  pexCharExpansion = RECORD
    head      : pexElementInfo;
    expansion : Ctypes.float;
  END; (* pexCharExpansion *)

  pexCharSpacing = RECORD
    head    : pexElementInfo;
    spacing : Ctypes.float;
  END; (* pexCharSpacing *)

  pexCharHeight = RECORD
    head   : pexElementInfo;
    height : Ctypes.float;
  END; (* pexCharHeight *)
  pexAtextHeight = pexCharHeight;

  pexCharUpVector = RECORD
    head : pexElementInfo;
    up   : BITS BITSIZE (pexVector2D) FOR pexVector2D;
  END; (* pexCharUpVector *)
  pexAtextUpVector = pexCharUpVector;

  pexTextPath = RECORD
    head : pexElementInfo;
    path : CARD16;
    pad  : CARD16;
  END; (* pexTextPath *)
  pexAtextPath = pexTextPath;

  pexTextAlignment = RECORD
    head      : pexElementInfo;
    alignment : pexTextAlignmentData;
  END; (* pexTextAlignment *)
  pexAtextAlignment = pexTextAlignment;

  pexLineType = RECORD
    head     : pexElementInfo;
    lineType : pexEnumTypeIndex;
    pad      : CARD16;
  END; (* pexLineType *)

  pexLineWidth = RECORD
    head  : pexElementInfo;
    width : Ctypes.float;
  END; (* pexLineWidth *)
  pexSurfaceEdgeWidth = pexLineWidth;

  pexCurveApproximation = RECORD
    head   : pexElementInfo;
    approx : BITS BITSIZE (pexCurveApprox) FOR pexCurveApprox;
  END; (* pexCurveApproximation *)

  pexPolylineInterp = RECORD
    head           : pexElementInfo;
    polylineInterp : pexEnumTypeIndex;
    pad            : CARD16;
  END; (* pexPolylineInterp *)

  pexInteriorStyle = RECORD
    head          : pexElementInfo;
    interiorStyle : pexEnumTypeIndex;
    pad           : CARD16;
  END; (* pexInteriorStyle *)
  pexBfInteriorStyle = pexInteriorStyle;

  pexSurfaceReflAttr = RECORD
    head           : pexElementInfo;
    reflectionAttr : BITS BITSIZE (pexReflectionAttr) FOR pexReflectionAttr;
  END; (* pexSurfaceReflAttr *)
  pexBfSurfaceReflAttr = pexSurfaceReflAttr;

  pexSurfaceReflModel = RECORD
    head            : pexElementInfo;
    reflectionModel : pexEnumTypeIndex;
    pad             : CARD16;
  END; (* pexSurfaceReflModel *)
  pexBfSurfaceReflModel = pexSurfaceReflModel;

  pexSurfaceInterp = RECORD
    head          : pexElementInfo;
    surfaceInterp : pexEnumTypeIndex;
    pad           : CARD16;
  END; (* pexSurfaceInterp *)
  pexBfSurfaceInterp = pexSurfaceInterp;

  pexSurfaceApproximation = RECORD
    head   : pexElementInfo;
    approx : pexSurfaceApprox;
  END; (* pexSurfaceApproximation *)

  pexCullingMode = RECORD
    head     : pexElementInfo;
    cullMode : pexCullMode;
    pad      : CARD16;
  END; (* pexCullingMode *)

  pexDistinguishFlag = RECORD
    head        : pexElementInfo;
    distinguish : pexSwitch;
    pad         : ARRAY [0 .. 2] OF BYTE;
  END; (* pexDistinguishFlag *)

  pexPatternSize = RECORD
    head : pexElementInfo;
    size : BITS BITSIZE (pexVector2D) FOR pexVector2D;
  END; (* pexPatternSize *)

  pexPatternRefPt = RECORD
    head  : pexElementInfo;
    point :  BITS BITSIZE (pexCoord2D) FOR pexCoord2D;
  END; (* pexPatternRefPt *)

  pexPatternAttr = RECORD
    head    : pexElementInfo;
    refPt   : pexCoord3D;
    vector1 : pexVector3D;
    vector2 : pexVector3D;
  END; (* pexPatternAttr *)

  pexSurfaceEdgeFlag = RECORD
    head  : pexElementInfo;
    onoff : pexSwitch;
    pad   : ARRAY [0 .. 2] OF BYTE;
  END; (* pexSurfaceEdgeFlag *)

  pexSurfaceEdgeType = RECORD
    head     : pexElementInfo;
    edgeType : pexEnumTypeIndex;
    pad      : CARD16;
  END; (* pexSurfaceEdgeType *)

  pexSetAsfValues = RECORD
    head      : pexElementInfo;
    attribute : pexAsfAttribute;
    source    : pexAsfValue;
    pad       : ARRAY [0 .. 2] OF BYTE;
  END; (* pexSetAsfValues *)

  pexLocalTransform = RECORD
    head     : pexElementInfo;
    compType : pexComposition;
    unused   : CARD16;
    matrix   : pexMatrix;
  END; (* pexLocalTransform *)

  pexLocalTransform2D = RECORD
    head      : pexElementInfo;
    compType  : pexComposition;
    unused    : CARD16;
    matrix3X3 : pexMatrix3X3;
  END; (* pexLocalTransform2D *)

  pexGlobalTransform = RECORD
    head   : pexElementInfo;
    matrix : pexMatrix;
  END; (* pexGlobalTransform *)

  pexGlobalTransform2D = RECORD
    head      : pexElementInfo;
    matrix3X3 : pexMatrix3X3;
  END; (* pexGlobalTransform2D *)

  pexModelClip = RECORD
    head  : pexElementInfo;
    onoff : pexSwitch;
    pad   : ARRAY [0 .. 2] OF BYTE;
  END; (* pexModelClip *)

  pexModelClipVolume = RECORD
    head              : pexElementInfo;
    modelClipOperator : pexEnumTypeIndex;
    numHalfSpaces     : CARD16;
    (* LISTof pexHalfSpace( numHalfSpaces ) *)
  END; (* pexModelClipVolume *)

  pexModelClipVolume2D = RECORD
    head              : pexElementInfo;
    modelClipOperator : pexEnumTypeIndex;
    numHalfSpaces     : CARD16;
    (* LISTof pexHalfSpace2D( numHalfSpaces ) *)
  END; (* pexModelClipVolume2D *)

  pexRestoreModelClip = RECORD
    head : pexElementInfo;
  END; (* pexRestoreModelClip *)

  pexLightState = RECORD
    head       : pexElementInfo;
    numEnable  : CARD16;
    numDisable : CARD16;
    (* LISTof pexTableIndex( numEnable ) *)
    (* pad( ( numEnable )*2 ) *)
    (* LISTof pexTableIndex( numDisable ) *)
    (* pad( ( numDisable )*2 ) *)
  END; (* pexLightState *)

  pexPickId = RECORD
    head   : pexElementInfo;
    pickId : CARD32;
  END; (* pexPickId *)

  pexHlhsrIdentifier = RECORD
    head    : pexElementInfo;
    hlhsrID : CARD32;
  END; (* pexHlhsrIdentifier *)

  pexRenderingColourModel = RECORD
    head  : pexElementInfo;
    model : pexEnumTypeIndex;
    pad   : CARD16;
  END; (* pexRenderingColourModel *)

  pexParaSurfCharacteristics = RECORD
    head            : pexElementInfo;
    characteristics : pexEnumTypeIndex;
    length          : CARD16;
    (* SINGLEof PARAMETRIC_SURFACE_CHARACTERISTICS *)
  END; (* pexParaSurfCharacteristics *)

  pexAddToNameSet = RECORD
    head : pexElementInfo;
    (* LISTof pexName() *)
  END; (* pexAddToNameSet *)
  pexRemoveFromNameSet = pexAddToNameSet;

  pexExecuteStructure = RECORD
    head : pexElementInfo;
    id   : pexStructure;
  END; (* pexExecuteStructure *)

  pexLabel = RECORD
    head  : pexElementInfo;
    label : INT32;
  END; (* pexLabel *)

  pexApplicationData = RECORD
    head        : pexElementInfo;
    numElements : CARD16;
    unused      : CARD16;
    (* LISTof CARD8( numElements ) -- don't swap *)
    (* pad( numElements ) *)
  END; (* pexApplicationData *)
    
  pexGse = RECORD
    head        : pexElementInfo;
    id          : CARD32;
    numElements : CARD16;
    unused      : CARD16;
    (* LISTof CARD8( numElements ) -- don't swap *)
    (* pad( numElements ) *)
  END; (* pexGse *)

  pexMarker = RECORD
    head : pexElementInfo;
    (* LISTof pexCoord3D() *)
  END; (* pexMarker; *)

  pexMarker2D = RECORD
    head : pexElementInfo;
    (* LISTof pexCoord2D() *)
  END; (* pexMarker2D; *)

  pexText = RECORD
    head         : pexElementInfo;
    origin       : pexCoord3D;
    vector1      : pexVector3D;
    vector2      : pexVector3D;
    numEncodings : CARD16;
    unused       : CARD16;
    (* LISTof pexMonoEncoding( numEncodings ) *)
  END; (* pexText *)

  pexText2D = RECORD
    head         : pexElementInfo;
    origin       : BITS BITSIZE (pexCoord2D) FOR pexCoord2D;
    numEncodings : CARD16;
    unused       : CARD16;
    (* LISTof pexMonoEncoding( numEncodings ) *)
  END; (* pexText2D *)

  pexAnnotationText = RECORD
    head         : pexElementInfo;
    origin       : pexCoord3D;
    offset       : pexCoord3D;
    numEncodings : CARD16;
    unused       : CARD16;
    (* LISTof pexMonoEncoding( numEncodings ) *)
  END; (* pexAnnotationText *)

  pexAnnotationText2D = RECORD
    head         : pexElementInfo;
    origin       : BITS BITSIZE (pexCoord2D) FOR pexCoord2D;
    offset       : BITS BITSIZE (pexCoord2D) FOR pexCoord2D;
    numEncodings : CARD16;
    unused       : CARD16;
    (* LISTof pexMonoEncoding( numEncodings ) *)
  END; (* pexAnnotationText2D *)

  pexPolyline = RECORD
    head : pexElementInfo;
    (* LISTof pexCoord3D() *)
  END; (* pexPolyline *)

  pexPolyline2D = RECORD
    head : pexElementInfo;
    (* LISTof pexCoord2D() *)
  END; (* pexPolyline2D *)

  pexPolylineSet = RECORD
    head          : pexElementInfo;
    colourType    : pexColourType;
    vertexAttribs : pexBitmaskShort;
    numLists      : CARD32;
    (* LISTof CLISTof pexVertex( numLists, vertexAttribs, colourType ) *)
  END; (* pexPolylineSet *)

  pexNurbCurve = RECORD
    head       : pexElementInfo;
    curveOrder : CARD16;
    coordType  : pexCoordType;
    tmin       : Ctypes.float;
    tmax       : Ctypes.float;
    numKnots   : CARD32;
    numPoints  : CARD32;
    (* LISTof FLOAT( numKnots ) *)
    (* LISTof {pexCoord3D|pexCoord4D}( numPoints, coordType ) *)
  END; (* pexNurbCurve *)

  pexFillArea = RECORD
    head        : pexElementInfo;
    shape       : CARD16;
    ignoreEdges : pexSwitch;
    pad         : CARD8;
    (* LISTof pexCoord3D() *)
  END; (* pexFillArea *)

  pexFillArea2D = RECORD
    head        : pexElementInfo;
    shape       : CARD16;
    ignoreEdges : pexSwitch;
    unused      : CARD8;
    (* LISTof pexCoord2D() *)
  END; (* pexFillArea2D *)

  pexExtFillArea = RECORD
    head          : pexElementInfo;
    shape         : CARD16;
    ignoreEdges   : pexSwitch;
    unused        : CARD8;
    colourType    : pexColourType;
    facetAttribs  : pexBitmaskShort;
    vertexAttribs : pexBitmaskShort;
    unused2       : CARD16;
    (* SINGLE Facet( facetAttribs, vertexAttribs, colourType ) *)
  END; (* pexExtFillArea *)

  pexFillAreaSet = RECORD
    head        : pexElementInfo;
    shape       : CARD16;
    ignoreEdges : pexSwitch;
    contourHint : CARD8;
    numLists    : CARD32;
    (* LISTof CLISTof Coord3D( numLists ) *)
  END; (* pexFillAreaSet *)

  pexFillAreaSet2D = RECORD
    head        : pexElementInfo;
    shape       : CARD16;
    ignoreEdges : pexSwitch;
    contourHint : CARD8;
    numLists    : CARD32;
    (* LISTof CLISTof Coord2D( numLists ) *)
  END; (* pexFillAreaSet2D *)


  pexExtFillAreaSet = RECORD
    head          : pexElementInfo;
    shape         : CARD16;
    ignoreEdges   : pexSwitch;
    contourHint   : CARD8;
    colourType    : pexColourType;
    facetAttribs  : pexBitmaskShort;
    vertexAttribs : pexBitmaskShort;
    unused2       : CARD16;
    numLists      : CARD32;
    (* pexOptData( facetAttribs ) *)
    (* LISTof CLISTof  pexVertex( numLists, vertexAttribs, colourType ) *)
  END; (* pexExtFillAreaSet *)

  pexTriangleStrip = RECORD
    head          : pexElementInfo;
    colourType    : pexColourType;
    facetAttribs  : pexBitmaskShort;
    vertexAttribs : pexBitmaskShort;
    unused        : CARD16;
    numVertices   : CARD32;
    (* number of OptData is numVert - 2 *)
    (* LISTof pexOptData( facetAttribs, colourType ) *)
    (* LISTof pexVertex( numVertices, vertexAttribs, colourType ) *)
  END; (* pexTriangleStrip *)

  pexQuadrilateralMesh = RECORD
    head          : pexElementInfo;
    colourType    : pexColourType;
    mPts          : CARD16;
    nPts          : CARD16;
    facetAttribs  : pexBitmaskShort;
    vertexAttribs : pexBitmaskShort;
    shape         : CARD16;
    (* actually, there are (mPts-1)*(nPts-1) opt data entries *)
    (* LISTof pexOptData( facetAttribs, colourType ) *)
    (* LISTof pexVertex( mPts, nPts, vertexAttribs, colourType ) *)
  END; (* pexQuadrilateralMesh *)

  pexSOFAS = RECORD
    head              : pexElementInfo;
    shape             : CARD16;
    colourType        : pexColourType;
    FAS_Attributes    : CARD16;
    vertexAttributes  : CARD16;
    edgeAttributes    : CARD16;
    contourHint       : CARD8;
    contourCountsFlag : pexSwitch;
    numFAS            : CARD16;
    numVertices       : CARD16;
    numEdges          : CARD16;
    numContours       : CARD16;
    (* LISTof OPT_DATA( numFAS ) *)
    (* LISTof pexVertex( numVertices ) *)
    (* LISTof CARD8( numEdges ) *)
    (* pad( numEdges ) *)
    (* LISTof CLISTof CLISTof CARD16( numFAS, numContours, numEdges ) *)
    (* pad *)
  END; (* pexSOFAS *)

  pexNurbSurface = RECORD
    head      : pexElementInfo;
    type      : pexCoordType;
    uOrder    : CARD16;
    vOrder    : CARD16;
    unused    : CARD16;
    numUknots : CARD32;
    numVknots : CARD32;
    mPts      : CARD16;
    nPts      : CARD16;
    numLists  : CARD32;
    (* LISTof FLOAT( numUknots ) *)
    (* LISTof FLOAT( numVKnots ) *)
    (* LISTof {pexCoord3D|pexCoord4D}( mPts, nPts, surfaceType ) *)
    (* LISTof CLISTof pexTrimCurve( numLists ) *)
  END; (* pexNurbSurface *)
  pexNurbSurfaceStar = UNTRACED REF pexNurbSurface;

  pexCellArray = RECORD
    head   : pexElementInfo;
    point1 : pexCoord3D;
    point2 : pexCoord3D;
    point3 : pexCoord3D;
    dx     : CARD32;
    dy     : CARD32;
    (* LISTof pexTableIndex( dx, dy ) *)
    (* pad(  2*dx*dy ) *)
  END; (* pexCellArray *)

  pexCellArray2D = RECORD
    head   : pexElementInfo;
    point1 : BITS BITSIZE (pexCoord2D) FOR pexCoord2D;
    point2 : BITS BITSIZE (pexCoord2D) FOR pexCoord2D;
    dx     : CARD32;
    dy     : CARD32;
    (* LISTof pexTableIndex( dx, dy ) *)
    (* pad( 2*dx*dy ) *)
  END; (* pexCellArray2D *)

  pexExtCellArray = RECORD
    head       : pexElementInfo;
    colourType : pexColourType;
    unused     : CARD16;
    point1     : pexCoord3D;
    point2     : pexCoord3D;
    point3     : pexCoord3D;
    dx         : CARD32;
    dy         : CARD32;
    (* LISTof pexColourSpecifier( dx, dy ) *)
  END; (* pexExtCellArray *)

  pexGdp = RECORD
    head      : pexElementInfo;
    gdpId     : INT32;
    numPoints : CARD32;
    numBytes  : CARD32;
    (* LISTof pexCoord3D( numPoints ) *)
    (* LISTof CARD8( numBytes ) -- don't swap *)
    (* pad( numBytes ) *)
  END; (* pexGdp *)

  pexGdp2D = RECORD
    head      : pexElementInfo;
    gdpId     : INT32;
    numPoints : CARD32;
    numBytes  : CARD32;
    (* LISTof pexCoord2D( numPoints ) *)
    (* LISTof CARD8( numBytes ) -- don't swap *)
    (* pad( numBytes ) *)
  END; (* pexGdp2D *)


(*****************************************************************************)
(* Based on PEXprotost.h  5.1 91/02/12                                       *)
(*                                                                           *)
(* Should be complete!                                                       *)
(*****************************************************************************)

(* Matches revision 5.0P *)

TYPE 

  pexAsfAttribute      = CARD32;
  pexAsfValue          = CARD8;
  pexBitmask           = CARD32;
  pexBitmaskShort      = CARD16;
  pexCoordType         = CARD16;                   (* rational, nonrational *)
  pexComposition       = CARD16;
  pexCullMode          = CARD16;
  pexDynamicType       = BYTE;
  pexEnumTypeIndex     = INT16;
  pexLookupTable       = CARD32;                   (* used to be XID *)
  pexName              = CARD32;
  pexNameStar          = UNTRACED REF pexName;
  pexNameStarStar      = UNTRACED REF pexNameStar;
  pexNameSet           = CARD32;                   (* used to be XID *)
  pexPC                = CARD32;                   (* used to be XID *)
  pexFont              = CARD32;                   (* used to be XID *)
  pexFontStar          = UNTRACED REF pexFont;
  pexMatrix            = ARRAY [0 .. 3],[0 .. 3] OF Ctypes.float;
  pexMatrixStar        = UNTRACED REF pexMatrix;
  pexMatrix3X3         = ARRAY [0 .. 2],[0 .. 2] OF Ctypes.float;
  pexPhigsWks          = CARD32;                   (* used to be XID *)
  pexPickMeasure       = CARD32;                   (* used to be XID *)
  pexRenderer          = CARD32;                   (* used to be XID *)
  pexSC                = CARD32;                   (* used to be XID *)
  pexStructure         = CARD32;                   (* used to be XID *)
  pexStructureStar     = UNTRACED REF pexStructure;
  pexStructureStarStar = UNTRACED REF pexStructureStar;
  pexSwitch            = CARD8;
  pexTableIndex        = CARD16;
  pexTableIndexStar    = UNTRACED REF pexTableIndex;
  pexTableType         = CARD16;       (* could be smaller if it ever helps *)
  pexTextHAlignment    = CARD16;
  pexTextVAlignment    = CARD16;
  pexTypeOrTableIndex  = CARD16;
  pexColourType        = pexEnumTypeIndex;                    (* ColourType *)

(* included in others *)

  pexString = RECORD
    length : CARD16;
    (* list of CARD8 -- don't swap *)
  END; (* pexString *)

  pexStructureInfo = RECORD
    sid      : pexStructure;
    priority : Ctypes.float;
  END; (* pexStructureInfo *)
  pexStructureInfoStar = UNTRACED REF pexStructureInfo;

  pexVector2D = RECORD
    x : Ctypes.float;
    y : Ctypes.float;
  END; (* pexVector2D *)

  pexVector3D = RECORD
    x : Ctypes.float;
    y : Ctypes.float;
    z : Ctypes.float;
  END; (* pexVector3D *)
  pexVector3DStar = UNTRACED REF pexVector3D;

(* Coord structures *)

  pexCoord2D = RECORD
    x : Ctypes.float;
    y : Ctypes.float;
  END; (* pexCoord2D *)
  pexCoord2DStar = UNTRACED REF pexCoord2D;

  pexCoord3D = RECORD
    x : Ctypes.float;
    y : Ctypes.float;
    z : Ctypes.float;
  END; (* pexCoord3D *)
  pexCoord3DStar = UNTRACED REF pexCoord3D;

  pexCoord4D = RECORD
    x : Ctypes.float;
    y : Ctypes.float;
    z : Ctypes.float;
    w : Ctypes.float;
  END; (* pexCoord4D *)
  pexCoord4DStar = UNTRACED REF pexCoord4D;

(* Colour structures *)

  pexRgbFloatColour = RECORD
    red   : Ctypes.float;   
    green : Ctypes.float;
    blue  : Ctypes.float;
  END; (* pexRgbFloatColour *)
  pexRgbFloatColourStar = UNTRACED REF pexRgbFloatColour;

  pexHsvColour = RECORD
    hue        : Ctypes.float;
    saturation : Ctypes.float;
    value      : Ctypes.float;
  END; (* pexHsvColour *)

  pexHlsColour = RECORD
    hue        : Ctypes.float;
    lightness  : Ctypes.float;
    saturation : Ctypes.float;
  END; (* pexHlsColour *)

  pexCieColour = RECORD
    x : Ctypes.float;
    y : Ctypes.float;
    z : Ctypes.float;
  END; (* pexCieColour *)

  pexRgb8Colour = RECORD
    red   : CARD8;
    green : CARD8;
    blue  : CARD8;
    pad   : CARD8;
  END; (* pexRgb8Colour *)

  pexRgb16Colour = RECORD
    red   : CARD16;
    green : CARD16;
    blue  : CARD16;
    pad   : CARD16;
  END; (* pexRgb16Colour *)

  pexIndexedColour = RECORD
    index : pexTableIndex;
    pad   : CARD16;
  END; (* pexIndexedColour *)
  pexIndexedColourStar = UNTRACED REF pexIndexedColour;

(* Recall that Modula-3 does not have variant records, *)
(* or anything resembling a C union type!              *)

CONST
  auxPexColourSize =
    MAX (BYTESIZE (pexIndexedColour),
    MAX (BYTESIZE (pexRgb8Colour),
    MAX (BYTESIZE (pexRgb16Colour),
    MAX (BYTESIZE (pexRgbFloatColour),
    MAX (BYTESIZE (pexHsvColour),
    MAX (BYTESIZE (pexHlsColour),
         BYTESIZE (pexCieColour)))))));

TYPE
  pexColour = RECORD
    format : ARRAY [1 .. auxPexColourSize] OF BYTE;
  END; (* pexColour *)
  pexColourStar = UNTRACED REF pexColour;

  pexFloatColour = RECORD
    first  : Ctypes.float;
    second : Ctypes.float;
    third  : Ctypes.float;
  END; (* pexFloatColour *)

  pexColourSpecifier = RECORD
    colourType : pexColourType;        (* ColourType enumerated type *)
    unused     : CARD16;
    (* SINGLE COLOUR(colourType) *)
  END; (* pexColourSpecifier *)

  pexCurveApprox = RECORD
    approxMethod : pexEnumTypeIndex;
    unused       : CARD16;
    tolerance    : Ctypes.float;
  END; (* pexCurveApprox *)

  pexDeviceCoord = RECORD
    x : INT16;
    y : INT16;
    z : Ctypes.float;
  END; (* pexDeviceCoord *)

  pexDeviceCoord2D = RECORD 
    x : INT16;
    y : INT16;
  END; (* pexDeviceCoord2D *)

  pexDeviceRect = RECORD 
    xmin : INT16;
    ymin : INT16;
    xmax : INT16;
    ymax : INT16;
  END; (* pexDeviceRect *)
  pexDeviceRectStar = UNTRACED REF pexDeviceRect;

  pexElementInfo = RECORD
    elementType : CARD16;
    length      : CARD16;
  END; (* pexElementInfo *)
  pexElementInfoStar     = UNTRACED REF pexElementInfo;
  pexElementInfoStarStar = UNTRACED REF pexElementInfoStar;

  pexElementPos = RECORD
    whence : CARD16;
    unused : CARD16;
    offset : INT32;
  END; (* pexElementPos *)

  pexElementRange = RECORD
    position1 : pexElementPos;
    position2 : pexElementPos;
  END; (* pexElementRange *)

  pexElementRef = RECORD
    structure : pexStructure;
    offset    : CARD32;
  END; (* pexElementRef *)
  pexElementRefStar = UNTRACED REF pexElementRef;

  pexExtentInfo = RECORD
    lowerLeft   : pexCoord2D;
    upperRight  : pexCoord2D;
    concatpoint : pexCoord2D;
  END; (* pexExtentInfo *)

  pexEnumTypeDesc = RECORD
    index      : pexEnumTypeIndex;
    descriptor : pexString;
  END; (* pexEnumTypeDesc *)

  pexHalfSpace = RECORD
    point  : pexCoord3D;
    vector : pexVector3D;
  END; (* pexHalfSpace *)
  pexHalfSpaceStar = UNTRACED REF pexHalfSpace;

  pexNameSetPair = RECORD
    incl : pexNameSet;
    excl : pexNameSet;
  END; (* pexNameSetPair *)
  pexNameSetPairStar = UNTRACED REF pexNameSetPair;

  pexHalfSpace2D = RECORD
    point  : pexCoord2D;
    vector : pexVector2D;
  END; (* pexHalfSpace2D *)
  pexHalfSpace2DStar = UNTRACED REF pexHalfSpace2D;

  pexLocalTransform3DData = RECORD
    composition : CARD16;
    unused      : CARD16;
    matrix      : pexMatrix;
  END; (* pexLocalTransform3DData *)
  pexLocalTransform3DDataStar = UNTRACED REF pexLocalTransform3DData;

  pexLocalTransform2DData = RECORD
    composition : CARD16;
    unused      : CARD16;
    matrix      : pexMatrix3X3;
  END; (* pexLocalTransform2DData *)
  pexLocalTransform2DDataStar = UNTRACED REF pexLocalTransform2DData;

  pexNpcSubvolume = RECORD
    minval : pexCoord3D;
    maxval : pexCoord3D;
  END; (* pexNpcSubvolume *)

(*  an OPT_DATA  structure cannot be defined because it has variable content
 *  and size.  An union structure could be used to define a template for
 *  the data. However, since unions pad to a fixed amount of space and the
 *  protocol uses variable lengths, this is not appropriate for protocol
 *  data types.  The most correct way of defining this data is to define
 *  one data structure for every possible combination of color, normal and
 *  edge data that could be given with a vertex or facet.
 *)

  pexPickPath = RECORD
    sid : pexStructure;
    offset : CARD32;
    pickid : CARD32;
  END; (* pexPickPath *)
  pexPickPathStar     = UNTRACED REF pexPickPath;
  pexPickPathStarStar = UNTRACED REF pexPickPathStar;

  pexTextAlignmentData = RECORD
    vertical   : pexTextVAlignment;
    horizontal : pexTextHAlignment;
  END; (* pexTextAlignmentData *)

  pexTrimCurve = RECORD
    visibility   : pexSwitch;
    unused       : CARD8;
    order        : CARD16;
    type         : pexCoordType;
    approxMethod : INT16;
    tolerance    : Ctypes.float;
    tMin         : Ctypes.float;
    tMax         : Ctypes.float;
    numKnots     : CARD32;
    numCoord     : CARD32;
    (* LISTof FLOAT(numKnots) -- length = order + number of coords *)
    (* LISTof {pexCoord3D|pexCoord4D}(numCoord) *)
  END; (* pexTrimCurve *)

  pexReflectionAttr = RECORD
    ambient        : Ctypes.float;
    diffuse        : Ctypes.float;
    specular       : Ctypes.float;
    specularConc   : Ctypes.float;
    transmission   : Ctypes.float;  (* 0.0 = opaque, 1.0 = transparent *)
    specularColour : pexColourSpecifier;
    (* SINGLE COLOUR() *)
  END; (* pexReflectionAttr *)

  pexSurfaceApprox = RECORD
    approxMethod : pexEnumTypeIndex;
    unused       : CARD16;
    uTolerance   : Ctypes.float;
    vTolerance   : Ctypes.float;
  END; (* pexSurfaceApprox *)

  pexVertex = RECORD
    point : pexCoord3D;
    (* SINGLE OPT_DATA() *)
  END; (* pexVertex *)

  pexViewport = RECORD
    minval      : BITS BITSIZE (pexDeviceCoord) FOR pexDeviceCoord;
    maxval      : BITS BITSIZE (pexDeviceCoord) FOR pexDeviceCoord;
    useDrawable : pexSwitch;
    pad         : ARRAY [0 .. 2] OF BYTE;
  END; (* pexViewport *)

  pexViewEntry = RECORD
    clipFlags   : CARD16;
    unused      : CARD16;
    clipLimits  : BITS BITSIZE (pexNpcSubvolume) FOR pexNpcSubvolume;
    orientation : pexMatrix;
    mapping     : pexMatrix;
  END; (* pexViewEntry *)

  pexViewRep = RECORD
    index  : pexTableIndex;
    unused : CARD16;
    view   : pexViewEntry;
  END; (* pexViewRep *)

(*
 * typedefs for lookup tables
 *)

  pexTableInfo = RECORD
    definableEntries : CARD16;
    numPredefined    : CARD16;
    predefinedMin    : CARD16;
    predefinedMax    : CARD16;
  END; (* pexTableInfo *)
  pexTableInfoStar = UNTRACED REF pexTableInfo;
  pexTableInfoStarStar = UNTRACED REF pexTableInfoStar;

  pexLineBundleEntry = RECORD 
    lineType       : pexEnumTypeIndex;
    polylineInterp : pexEnumTypeIndex;
    curveApprox    : BITS BITSIZE (pexCurveApprox) FOR pexCurveApprox;
    lineWidth      : Ctypes.float;
    lineColour     : pexColourSpecifier;
    (* SINGLE COLOUR() *)
  END; (* pexLineBundleEntry *)

  pexMarkerBundleEntry = RECORD 
    markerType   : pexEnumTypeIndex;
    unused       : INT16;
    markerScale  : Ctypes.float;
    markerColour : pexColourSpecifier;
    (* SINGLE COLOUR() *)
  END; (* pexMarkerBundleEntry *)

  pexTextBundleEntry = RECORD 
    textFontIndex : CARD16;
    textPrecision : CARD16;
    charExpansion : Ctypes.float;
    charSpacing   : Ctypes.float;
    textColour    : pexColourSpecifier;
    (* SINGLE COLOUR() *)
  END; (* pexTextBundleEntry *)


(*
    Note that since an InteriorBundleEntry contains 4 embedded instances of 
    pexColourSpecifier, a variable-sized item, a data structure cannot be
    defined for it.
*)
  pexInteriorBundleEntry = RECORD
    interiorStyle        : pexEnumTypeIndex;
    interiorStyleIndex   : INT16;
    reflectionModel      : pexEnumTypeIndex;
    surfaceInterp        : pexEnumTypeIndex;
    bfInteriorStyle      : pexEnumTypeIndex;
    bfInteriorStyleIndex : INT16;
    bfReflectionModel    : pexEnumTypeIndex;
    bfSurfaceInterp      : pexEnumTypeIndex;
    surfaceApprox        : pexSurfaceApprox;
    (* SINGLE pexColourSpecifier		surfaceColour    *)
    (* SINGLE pexReflectionAttr			reflectionAttr   *)
    (* SINGLE pexColourSpecifier		bfSurfaceColour  *)
    (* SINGLE pexReflectionAttr			bfReflectionAttr *)
  END; (* pexInteriorBundleEntry *)

  pexEdgeBundleEntry = RECORD
    edges      : pexSwitch;
    unused     : CARD8;
    edgeType   : pexEnumTypeIndex;
    edgeWidth  : Ctypes.float;
    edgeColour : pexColourSpecifier;
    (* SINGLE COLOUR() *)
  END; (* pexEdgeBundleEntry *)

  pexPatternEntry = RECORD
    colourType : pexColourType;
    numx       : CARD16;
    numy       : CARD16;
    unused     : CARD16;
    (* LISTof Colour(numx, numy) 2D array of colours *)
  END; (* pexPatternEntry *)

(* a pexColourEntry is just a pexColourSpecifier *)

  pexTextFontEntry = RECORD
    numFonts : CARD32;
    (* LISTof pexFont( numFonts ) *)
  END; (* pexTextFontEntry *)

(* a pexViewEntry is defined above *)

  pexLightEntry = RECORD
    lightType     : pexEnumTypeIndex;
    unused        : INT16;
    direction     : pexVector3D;
    point         : pexCoord3D;
    concentration : Ctypes.float;
    spreadAngle   : Ctypes.float;
    attenuation1  : Ctypes.float;
    attenuation2  : Ctypes.float;
    lightColour   : pexColourSpecifier;
    (* SINGLE COLOUR() *)
  END; (* pexLightEntry *)

  pexDepthCueEntry = RECORD
    mode           : pexSwitch;
    unused         : CARD8;
    unused2        : CARD16;
    frontPlane     : Ctypes.float;
    backPlane      : Ctypes.float;
    frontScaling   : Ctypes.float;
    backScaling    : Ctypes.float;
    depthCueColour : pexColourSpecifier;
    (* SINGLE COLOUR() *)
  END; (* pexDepthCueEntry *)

  pexColourApproxEntry = RECORD
    approxType  : INT16;
    approxModel : INT16;
    max1        : CARD16;
    max2        : CARD16;
    max3        : CARD16;
    dither      : CARD8;
    unused      : CARD8;
    mult1       : CARD32;
    mult2       : CARD32;
    mult3       : CARD32;
    weight1     : Ctypes.float;
    weight2     : Ctypes.float;
    weight3     : Ctypes.float;
    basePixel   : CARD32;
  END; (* pexColourApproxEntry *)


(*  Font structures *)

  pexFontProp = RECORD
    name  : X.Atom;
    value : CARD32;
  END; (* pexFontProp *)

  pexFontInfo = RECORD
    firstGlyph   : CARD32;
    lastGlyph    : CARD32;
    defaultGlyph : CARD32;
    allExist     : pexSwitch;
    strokeFont   : pexSwitch;
    unused       : CARD16;
    numProps     : CARD32;
    (* LISTof pexFontProp(numProps) *)
  END; (* pexFontInfo *)
  pexFontInfoStar = UNTRACED REF pexFontInfo;
  pexFontInfoStarStar = UNTRACED REF pexFontInfoStar;


(* Text Structures *)

  pexMonoEncoding = RECORD
    characterSet      : INT16;
    characterSetWidth : CARD8;
    encodingState     : CARD8;
    unused            : CARD16;
    numChars          : CARD16;
    (* LISTof CHARACTER( numChars ) *)
    (* pad *)
  END; (* pexMonoEncoding *)

(* CHARACTER is either a CARD8, a CARD16, or a CARD32 *)


(* Parametric Surface Characteristics types *)

(* type 1 None *)

(* type 2 Implementation Dependent *)

  pexPSC_IsoparametricCurves = RECORD
    placementType : CARD16;
    unused        : CARD16;
    numUcurves    : CARD16;
    numVcurves    : CARD16;
  END; (* pexPSC_IsoparametricCurves *)                        (* type 3 *)

  pexPSC_LevelCurves = RECORD
    origin              : pexCoord3D;
    direction           : pexVector3D;
    numberIntersections : CARD16;
    pad                 : CARD16;
    (* LISTof pexCoord3D( numIntersections ) *)
  END; (* pexPSC_LevelCurves *)	              (*  type 4: MC, type 5: WC *)

(* Pick Device data records *)

  pexPD_DC_HitBox = RECORD
    position : pexDeviceCoord2D;
    distance : Ctypes.float;
  END; (* pexPD_DC_HitBox *)				(* pick device 1 *)

  pexPD_NPC_HitVolume = pexNpcSubvolume;                (* pick device 2 *)

(* Output Command errors *)

  pexOutputCommandError = RECORD
    type           : CARD8;	(*  0 *)
    errorCode      : CARD8;	(* 14 *)
    sequenceNumber : CARD16;
    resourceId     : CARD32;	(* renderer or structure *)
    minorCode      : CARD16;
    majorCode      : CARD8;
    unused         : CARD8;
    opcode         : CARD16;	(* opcode of failed output command *)
    numCommands    : CARD16;    (* number successfully done before error *)
    pad            : ARRAY [0 .. 15] OF BYTE;
  END; (* pexOutputCommandError *)


(*****************************************************************************)
(* Based on PEXlib.h                                                         *)
(*                                                                           *)
(* Not yet complete!                                                         *)
(*****************************************************************************)

(*
 * PEXlib constants  
 *)

(* for PEXRotationMatrix *)

CONST
  pxlXAxis = 1;
  pxlYAxis = 2;
  pxlZAxis = 3;

(* output command buffer types *)

  pxlRenderImmediate = PEX_RenderOutputCommands;
  pxlAddToStructure  = PEX_StoreElements;

(* A pxlInt is a 32-bit integeger value *)

TYPE
  pxlInt          = INT32;
  unsigned_pxlInt = CARD32;

(*
 * Following are the typedefs used in the PEXlib interface. pxlFooBar
 * definitions are always supersets of the pexFooBar protocol definitions. 
 * Note that most pxlFooBar definitions are equivalent to the pexFooBar
 * protocol definitions.  However some of the definitions do have extra
 * fields. The extra fields are used to access data that would normally 
 * follow a pexFooBar in the protocol.  pxlFooBar definitions should
 * *never* contain fields which are not required in the protocol.  This
 * means pexlib can copy pxlFooBar data into the protocol stream.  Sometimes 
 * the data which follows a pexFooBar is variable length.  For example 
 *
 *   	typedef struct pxlColourSpecifier
 *	{
 *    	    pxlColourType       colourType;
 *  	    CARD16              unused;
 *    	    pxlColour           colour;
 *	} pxlColourSpecifier;
 *
 *	typedef struct pexColourSpecifier
 *	{
 *    	    pexColourType       colourType;
 *    	    CARD16              unused;
 *    	    ( SINGLE COLOUR(colourType) ) 
 *	} pexColourSpecifier;
 *
 * The pxlColorSpecifier has an extra field for accessing the colour data.
 * Note that the size of 'colour' depends on the colour type.  (ie: pxlColour 
 * and pexColour are defined as a union of all possible colour types)
 * Using a pxlColourSpecifier provides the application easy access to the 
 * colour data.  However PEXlib must pack the colour data before sending it 
 * down to the pex server.
 *
 *)

(*
 * PEXlib definitions used globally 
 *)

TYPE
  pxlAsfAttribute 	      = pexAsfAttribute;
  pxlAsfValue 		      = pexAsfValue;
  pxlBitmask 		      = pexBitmask;
  pxlBitmaskShort 	      = pexBitmaskShort;
  pxlCoordType 		      = pexCoordType;
  pxlComposition 	      = pexComposition;
  pxlCullMode 		      = pexCullMode;
  pxlDynamicType 	      = pexDynamicType;
  pxlEnumTypeIndex	      = pexEnumTypeIndex;
  pxlLookupTable	      = X.XID;               (* was pexLookupTable *)
  pxlName 		      = pexName;
  pxlNameStar 		      = pexNameStar;
  pxlNameStarStar  	      = pexNameStarStar;
  pxlNameSet		      = X.XID;               (* was pexNameSet *)
  pxlPipelineContext 	      = pexPC;
  pxlFont 		      = pexFont;
  pxlMatrix 		      = pexMatrix;
  pxlMatrixStar 	      = pexMatrixStar;
  pxlMatrix3X3 		      = pexMatrix3X3;
  pxlRenderer 		      = pexRenderer;
  pxlStructure 		      = pexStructure;
  pxlStructureStar            = pexStructureStar;
  pxlStructureStarStar        = pexStructureStarStar;
  pxlSwitch 		      = pexSwitch;
  pxlTableIndex 	      = pexTableIndex;
  pxlTableIndexStar           = pexTableIndexStar;
  pxlTableType 		      = pexTableType;
  pxlTextHAlignment 	      = pexTextHAlignment;
  pxlTextVAlignment 	      = pexTextVAlignment;
  pxlTypeOrTableIndex 	      = pexTypeOrTableIndex;
  pxlColourType 	      = pexColourType;

  pxlString 		      = pexString;
  pxlStructureInfo 	      = pexStructureInfo;
  pxlStructureInfoStar 	      = pexStructureInfoStar;
  pxlVector2D 		      = pexVector2D;
  pxlVector3D 		      = pexVector3D;
  pxlVector3DStar             = pexVector3DStar;

  pxlCurveApprox 	      = pexCurveApprox;
  pxlDeviceRect 	      = pexDeviceRect;
  pxlDeviceRectStar 	      = pexDeviceRectStar;
  pxlElementInfo 	      = pexElementInfo;
  pxlElementInfoStar          = pexElementInfoStar;
  pxlElementInfoStarStar      = pexElementInfoStarStar;
  pxlElementPos 	      = pexElementPos;
  pxlElementRange 	      = pexElementRange;
  pxlElementRef 	      = pexElementRef;
  pxlElementRefStar 	      = pexElementRefStar;
  pxlExtentInfo 	      = pexExtentInfo;
  pxlEnumTypeDesc 	      = pexEnumTypeDesc;
  pxlHalfSpace 		      = pexHalfSpace;
  pxlHalfSpaceStar	      = pexHalfSpaceStar;
  pxlNameSetPair 	      = pexNameSetPair;
  pxlNameSetPairStar 	      = pexNameSetPairStar;
  pxlHalfSpace2D 	      = pexHalfSpace2D;
  pxlHalfSpace2DStar 	      = pexHalfSpace2DStar;
  pxlLocalTransform3DData     = pexLocalTransform3DData;
  pxlLocalTransform3DDataStar = pexLocalTransform3DDataStar;
  pxlLocalTransform2DData     = pexLocalTransform2DData;
  pxlLocalTransform2DDataStar = pexLocalTransform2DDataStar;
  pxlNpcSubvolume             = pexNpcSubvolume;
  pxlPickPath 		      = pexPickPath;
  pxlPickPathStar             = pexPickPathStar;
  pxlPickPathStarStar         = pexPickPathStarStar;
  pxlTextAlignmentData 	      = pexTextAlignmentData;
  pxlTrimCurve 		      = pexTrimCurve;
  pxlSurfaceApprox 	      = pexSurfaceApprox;
  pxlVertex 		      = pexVertex;
  pxlViewport                 = pexViewport;
  pxlViewRep 		      = pexViewRep;
  pxlMonoEncoding 	      = pexMonoEncoding;
  pxlPSC_IsoparametricCurves  = pexPSC_IsoparametricCurves;
  pxlPSC_LevelCurves	      = pexPSC_LevelCurves;

  pxlSearchContext 	      = X.XID;
  pxlPhigsWks 		      = pexPhigsWks;

(* 
 * Coordinate definitions 
 *)

  pxlCoord2D 	   = pexCoord2D;
  pxlCoord2DStar   = pexCoord2DStar;
  pxlCoord3D 	   = pexCoord3D;
  pxlCoord3DStar   = pexCoord3DStar;
  pxlCoord4D 	   = pexCoord4D;
  pxlCoord4DStar   = pexCoord4DStar;
  pxlDeviceCoord   = pexDeviceCoord;
  pxlDeviceCoord2D = pexDeviceCoord2D;

(*
 * Colour definitions
 *)

  pxlRgbFloatColour    = pexRgbFloatColour;
  pxlHsvColour         = pexHsvColour;
  pxlHlsColour         = pexHlsColour;
  pxlCieColour         = pexCieColour;
  pxlRgb8Colour        = pexRgb8Colour;
  pxlRgb16Colour       = pexRgb16Colour;
  pxlIndexedColour     = pexIndexedColour;
  pxlIndexedColourStar = pexIndexedColourStar;
  pxlColour            = pexColour;
  pxlColourStar        = pexColourStar;
  pxlFloatColour       = pexFloatColour;

  pxlColourSpecifier = RECORD
    colourType : pxlColourType;
    unused     : CARD16;
    colour     : pxlColour;
  END; (* pxlColourSpecifier *)

(* 
 * reflection attributes 
 *)

  pxlReflectionAttr = RECORD
    ambient        : Ctypes.float;
    diffuse        : Ctypes.float;
    specular       : Ctypes.float;
    specularConc   : Ctypes.float;
    transmission   : Ctypes.float;
    specularColour : BITS BITSIZE (pxlColourSpecifier) FOR pxlColourSpecifier;
  END; (* pxlReflectionAttr *)
  pxlReflectionAttrStar = UNTRACED REF pxlReflectionAttr;


(*
 * Definitions used for output commands
 *)

  pxlMarkerType 		= pexMarkerType;
  pxlMarkerScale 		= pexMarkerScale;
  pxlMarkerColourIndex 		= pexMarkerColourIndex;
  pxlMarkerColour 		= pexMarkerColour;
  pxlMarkerBundleIndex 		= pexMarkerBundleIndex;
  pxlTextFontIndex 		= pexTextFontIndex;
  pxlTextPrecision 		= pexTextPrecision;
  pxlCharExpansion 		= pexCharExpansion;
  pxlCharSpacing 		= pexCharSpacing;
  pxlTextColourIndex 		= pexTextColourIndex;
  pxlTextColour 		= pexTextColour;
  pxlCharHeight 		= pexCharHeight;
  pxlCharUpVector 		= pexCharUpVector;
  pxlTextPath 			= pexTextPath;
  pxlTextAlignment 		= pexTextAlignment;
  pxlAtextHeight 		= pexAtextHeight;
  pxlAtextUpVector 		= pexAtextUpVector;
  pxlAtextPath 			= pexAtextPath;
  pxlAtextAlignment 		= pexAtextAlignment;
  pxlAtextStyle 		= pexAtextStyle;
  pxlTextBundleIndex 		= pexTextBundleIndex;
  pxlLineType 			= pexLineType;
  pxlLineWidth 			= pexLineWidth;
  pxlLineColourIndex 		= pexLineColourIndex;
  pxlLineColour 		= pexLineColour;
  pxlCurveApproximation 	= pexCurveApproximation;
  pxlPolylineInterp 		= pexPolylineInterp;
  pxlLineBundleIndex 		= pexLineBundleIndex;
  pxlInteriorStyle 		= pexInteriorStyle;
  pxlInteriorStyleIndex 	= pexInteriorStyleIndex;
  pxlSurfaceColourIndex 	= pexSurfaceColourIndex;
  pxlSurfaceColour 		= pexSurfaceColour;
  pxlSurfaceReflAttr 		= pexSurfaceReflAttr;
  pxlSurfaceReflModel 		= pexSurfaceReflModel;
  pxlSurfaceInterp 		= pexSurfaceInterp;
  pxlBfInteriorStyle 		= pexBfInteriorStyle;
  pxlBfInteriorStyleIndex 	= pexBfInteriorStyleIndex;
  pxlBfSurfaceColourIndex 	= pexBfSurfaceColourIndex;
  pxlBfSurfaceColour 		= pexBfSurfaceColour;
  pxlBfSurfaceReflAttr 		= pexBfSurfaceReflAttr;
  pxlBfSurfaceReflModel 	= pexBfSurfaceReflModel;
  pxlBfSurfaceInterp 		= pexBfSurfaceInterp;
  pxlSurfaceApproximation 	= pexSurfaceApproximation;
  pxlCullingMode 		= pexCullingMode;
  pxlDistinguishFlag 		= pexDistinguishFlag;
  pxlPatternSize 		= pexPatternSize;
  pxlPatternRefPt 		= pexPatternRefPt;
  pxlPatternAttr 		= pexPatternAttr;
  pxlInteriorBundleIndex 	= pexInteriorBundleIndex;
  pxlSurfaceEdgeFlag 		= pexSurfaceEdgeFlag;
  pxlSurfaceEdgeType 		= pexSurfaceEdgeType;
  pxlSurfaceEdgeWidth 		= pexSurfaceEdgeWidth;
  pxlSurfaceEdgeColourIndex 	= pexSurfaceEdgeColourIndex;
  pxlSurfaceEdgeColour 		= pexSurfaceEdgeColour;
  pxlEdgeBundleIndex 		= pexEdgeBundleIndex;
  pxlSetAsfValues 		= pexSetAsfValues;
  pxlLocalTransform 		= pexLocalTransform;
  pxlLocalTransform2D 		= pexLocalTransform2D;
  pxlGlobalTransform 		= pexGlobalTransform;
  pxlGlobalTransform2D 		= pexGlobalTransform2D;
  pxlModelClip 			= pexModelClip;
  pxlModelClipVolume 		= pexModelClipVolume;
  pxlModelClipVolume2D 		= pexModelClipVolume2D;
  pxlRestoreModelClip 		= pexRestoreModelClip;
  pxlViewIndex 			= pexViewIndex;
  pxlLightState 		= pexLightState;
  pxlDepthCueIndex 		= pexDepthCueIndex;
  pxlPickId 			= pexPickId;
  pxlHlhsrIdentifier 		= pexHlhsrIdentifier;
  pxlColourApproxIndex 		= pexColourApproxIndex;
  pxlRenderingColourModel 	= pexRenderingColourModel;
  pxlParaSurfCharacteristics 	= pexParaSurfCharacteristics;
  pxlAddToNameSet 		= pexAddToNameSet;
  pxlRemoveFromNameSet 		= pexRemoveFromNameSet;
  pxlExecuteStructure 		= pexExecuteStructure;
  pxlLabel 			= pexLabel;
  pxlApplicationData 		= pexApplicationData;
  pxlGse 			= pexGse;
  pxlMarker 			= pexMarker;
  pxlMarker2D 			= pexMarker2D;
  pxlText 			= pexText;
  pxlText2D 			= pexText2D;
  pxlAnnotationText 		= pexAnnotationText;
  pxlAnnotationText2D 		= pexAnnotationText2D;
  pxlPolyline 			= pexPolyline;
  pxlPolyline2D 		= pexPolyline2D;
  pxlPolylineSet 		= pexPolylineSet;
  pxlNurbCurve 			= pexNurbCurve;
  pxlFillArea 			= pexFillArea;
  pxlFillArea2D 		= pexFillArea2D;
  pxlExtFillArea 		= pexExtFillArea;
  pxlFillAreaSet 		= pexFillAreaSet;
  pxlFillAreaSet2D 		= pexFillAreaSet2D;
  pxlExtFillAreaSet 		= pexExtFillAreaSet;
  pxlTriangleStrip 		= pexTriangleStrip;
  pxlQuadrilateralMesh 		= pexQuadrilateralMesh;
  pxlSOFAS 			= pexSOFAS;
  pxlNurbSurface 		= pexNurbSurface;
  pxlNurbSurfaceStar            = pexNurbSurfaceStar;
  pxlCellArray 			= pexCellArray;
  pxlCellArray2D 		= pexCellArray2D;
  pxlExtCellArray 		= pexExtCellArray;
  pxlGdp 			= pexGdp;
  pxlGdp2D 			= pexGdp2D;

  pxlAsfData = RECORD
    attribute : unsigned_pxlInt;            (* was unsigned long *)
    value     : Ctypes.unsigned_char;
    pad       : ARRAY [0 .. 2] OF Ctypes.unsigned_char;
  END; (* pxlAsfData *)

  pxlStringData = RECORD
    numChars : Ctypes.int;
    chars : Ctypes.char_star;
  END; (* pxlStringData *)
  pxlStringDataStar = UNTRACED REF pxlStringData;
  pxlStringDataStarStar = UNTRACED REF pxlStringDataStar;

  pxlMonoEncodedTextData = RECORD
    characterSet : INT16;
    characterSetWidth : CARD8;
    encodingState : CARD8;
    unused : CARD16;
    numChars : CARD16;
    chars : Ctypes.char_star;
  END; (* pxlMonoEncodedTextData *)
  pxlMonoEncodedTextDataStar = UNTRACED REF pxlMonoEncodedTextData;

  pxlPolylineData = RECORD
    numPoints : Ctypes.int;
    points    : Ctypes.char_star;
  END; (* pxlPolylineData *)
  pxlPolylineDataStar = UNTRACED REF pxlPolylineData;

  pxlPolygonData = RECORD
    facetData : Ctypes.char_star;
    numPoints : Ctypes.int;
    points    : Ctypes.char_star;
  END; (* pxlPolygonData *)
  pxlPolygonDataStar = UNTRACED REF pxlPolygonData;

  pxlContourData = RECORD
    numPoints : Ctypes.int;
    points    : Ctypes.char_star;
  END; (* pxlContourData *)
  pxlContourDataStar = UNTRACED REF pxlContourData;

  pxlComplexPolygonData = RECORD
    facetData   : Ctypes.char_star;
    numPolygons : Ctypes.int;
    polygons    : pxlContourDataStar;
  END; (* pxlComplexPolygonData *)
  pxlComplexPolygonDataStar = UNTRACED REF pxlComplexPolygonData;

  pxlTrimCurveData = RECORD
    visibility   : Ctypes.int;
    order        : Ctypes.int;
    type         : Ctypes.int;
    approxMethod : Ctypes.int;
    tolerance    : Ctypes.float;
    tmin         : Ctypes.float;
    tmax         : Ctypes.float;
    numPoints    : Ctypes.int;
    knots        : Ctypes.float_star;
    points       : Ctypes.char_star;
  END; (* pxlTrimCurveData *)
  pxlTrimCurveDataStar = UNTRACED REF pxlTrimCurveData;

  pxlTrimLoopData = RECORD
    numTrimCurves    : Ctypes.int;
    trimCurves       : pxlTrimCurveDataStar;
  END; (* pxlTrimLoopData *)
  pxlTrimLoopDataStar = UNTRACED REF pxlTrimLoopData;

  pxlNurbSurfaceData = RECORD
    type         : Ctypes.unsigned_short;
    uorder       : Ctypes.unsigned_short;
    vorder       : Ctypes.unsigned_short;
    numTrimLoops : Ctypes.unsigned_short;
    numMPoints   : Ctypes.unsigned_short;
    numNPoints   : Ctypes.unsigned_short;
    uknots       : Ctypes.float_star;
    vknots       : Ctypes.float_star;
    points       : Ctypes.char_star;
    trimLoop     : pxlTrimLoopDataStar;
  END; (* pxlNurbSurfaceData *)
  pxlNurbSurfaceDataStar = UNTRACED REF pxlNurbSurfaceData;

  pxlOutputCommandError = pexOutputCommandError;


(*
 * Definitions used for output commands
 * Note that these MUST match the equivalent DEC PHIGS structures 
 *)

  pxlPhigsPointList23 = RECORD
    number : Ctypes.int;
    type   : Ctypes.int;
    pts23  : Ctypes.char_star;
  END; (* pxlPhigsPointList23 *)

  pxlPhigsFloatList = RECORD
    number : Ctypes.int;
    floats : Ctypes.float_star;
  END; (* pxlPhigsFloatList *)

  pxlPhigsTrimCurve = RECORD
    visflag  : Ctypes.int;
    order    : Ctypes.int;
    trimKnot : pxlPhigsFloatList;
    ctlpts   : pxlPhigsPointList23;
    tmin     : Ctypes.float;
    tmax     : Ctypes.float;
  END; (* pxlPhigsTrimCurve *)
  pxlPhigsTrimCurveStar = UNTRACED REF pxlPhigsTrimCurve;

  pxlPhigsTrimList = RECORD
    number : Ctypes.int;
    trim   : pxlPhigsTrimCurveStar;
  END; (* pxlPhigsTrimList *)
  pxlPhigsTrimListStar = UNTRACED REF pxlPhigsTrimList;

(*
 * Definitions for lookup tables 
 *)

  pxlTableInfo = pexTableInfo;
  pxlTableInfoStar = pexTableInfoStar;
  pxlTableInfoStarStar = pexTableInfoStarStar;

  pxlLineBundleEntry = RECORD
    lineType       : pxlEnumTypeIndex;
    polylineInterp : pxlEnumTypeIndex;
    curveApprox    : BITS BITSIZE (pxlCurveApprox) FOR pxlCurveApprox;
    lineWidth      : Ctypes.float;
    lineColour     : pxlColourSpecifier;
  END; (* pxlLineBundleEntry *)

  pxlMarkerBundleEntry = RECORD
    markerType   : pxlEnumTypeIndex;
    unused       : INT16;
    markerScale  : Ctypes.float;
    markerColour : pxlColourSpecifier;
  END; (* pxlMarkerBundleEntry *)

  pxlTextBundleEntry = RECORD
    textFontIndex : CARD16;
    textPrecision : CARD16;
    charExpansion : Ctypes.float;
    charSpacing   : Ctypes.float;
    textColour    : BITS BITSIZE (pxlColourSpecifier) FOR pxlColourSpecifier;
  END; (* pxlTextBundleEntry *)

  pxlInteriorBundleEntry = RECORD
    interiorStyle        : pxlEnumTypeIndex;
    interiorStyleIndex   : INT16;
    reflectionModel      : pxlEnumTypeIndex;
    surfaceInterp        : pxlEnumTypeIndex;
    bfInteriorStyle      : pxlEnumTypeIndex;
    bfInteriorStyleIndex : INT16;
    bfReflectionModel    : pxlEnumTypeIndex;
    bfSurfaceInterp      : pxlEnumTypeIndex;
    surfaceApprox        : pxlSurfaceApprox;
    surfaceColour        : BITS BITSIZE (pxlColourSpecifier) FOR pxlColourSpecifier;
    reflectionAttr       : pxlReflectionAttr;
    bfSurfaceColour      : BITS BITSIZE (pxlColourSpecifier) FOR pxlColourSpecifier;
    bfReflectionAttr     : pxlReflectionAttr;
  END; (* pxlInteriorBundleEntry *)

  pxlEdgeBundleEntry = RECORD
    edges      : pxlSwitch;
    unused     : CARD8;
    edgeType   : pxlEnumTypeIndex;
    edgeWidth  : Ctypes.float;
    edgeColour : pxlColourSpecifier;
  END; (* pxlEdgeBundleEntry *)

  pxlPatternEntry = RECORD
    colourType : pxlColourType;
    numx       : Ctypes.unsigned_short;
    numy       : Ctypes.unsigned_short;
    unused     : CARD16;
    colours    : Ctypes.char_star; (* ptr to 2D array of colours of type: *)
				  (* pxlRgbFloatColour,
				     pxlHsvColour,
				     pxlHlsColour,
				     pxlCieColour,
				     pxlRgb8Colour,
 				     pxlRgb16Colour,
				     or short (for PEXIndexedColour colours) *)
  END; (* pxlPatternEntry *)

  pxlColourEntry = RECORD
    colour : pxlColourSpecifier;
  END; (* pxlColourEntry *)

  pxlTextFontEntry = RECORD
    numFonts : CARD32;
    fonts : pexFontStar;
  END; (* pxlTextFontEntry *)

  pxlViewEntry = pexViewEntry;

  pxlLightEntry = RECORD
    lightType     : pxlEnumTypeIndex;
    unused        : INT16;
    direction     : pxlVector3D;
    point         : pxlCoord3D;
    concentration : Ctypes.float;
    spreadAngle   : Ctypes.float;
    attenuation1  : Ctypes.float;
    attenuation2  : Ctypes.float;
    lightColour   : BITS BITSIZE (pxlColourSpecifier) FOR pxlColourSpecifier;
  END; (* pxlLightEntry *)

  pxlDepthCueEntry = RECORD
    mode           : pxlSwitch;
    unused         : CARD8;
    unused2        : CARD16;
    frontPlane     : Ctypes.float;
    backPlane      : Ctypes.float;
    frontScaling   : Ctypes.float;
    backScaling    : Ctypes.float;
    depthCueColour : BITS BITSIZE (pxlColourSpecifier) FOR pxlColourSpecifier;
  END; (* pxlDepthCueEntry *)

  pxlColourApproxEntry = pexColourApproxEntry;

(*  
 * Definitions for fonts
 *)

  pxlFontProp = pexFontProp;

  pxlFontInfo = pexFontInfo;
  pxlFontInfoStar = pexFontInfoStar;
  pxlFontInfoStarStar = pexFontInfoStarStar;

(* 
 * Defintions for PEX extension info 
 *)

  pxlInfo = RECORD
    majorVersion : Ctypes.unsigned_short;
    minorVersion : Ctypes.unsigned_short;
    release      : unsigned_pxlInt;                     (* was unsigned long *)
    subsetInfo   : unsigned_pxlInt;                     (* was unsigned long *)
    vendorName   : Ctypes.char_star;
  END; (* pxlInfo *)
  pxlInfoStar     = UNTRACED REF pxlInfo;
  pxlInfoStarStar = UNTRACED REF pxlInfoStar;


(* 
 * Definitions for enumerated type descriptions 
 *)

  pxlEnumTypeDescList = RECORD
    enumVal : Ctypes.short;
    string  : Ctypes.char_star;
  END; (* pxlEnumTypeDescList *)
  pxlEnumTypeDescListStar     = UNTRACED REF pxlEnumTypeDescList;
  pxlEnumTypeDescListStarStar = UNTRACED REF pxlEnumTypeDescListStar;

(* 
 * Definitions for pipeline context.
 *)

  pxlLightList = RECORD
    numLights  : unsigned_pxlInt;                 (* was unsigned long *)
    lightIndex : pxlTableIndexStar;
  END; (* pxlLightList *)

  pxlModelClipVolumeList = RECORD
    numHalfSpaces : Ctypes.int;
    halfSpace     : pxlHalfSpaceStar;
  END; (* pxlModelClipVolumeList *)

  pxlPSCData = RECORD
    characteristics : Ctypes.short;
    length          : Ctypes.short;
    pscData         : Ctypes.char_star;
  END; (* pxlPSCData *)

  pxlPCAttributes = RECORD
    markerType           : Ctypes.short;
    markerScale          : Ctypes.float;
    markerColour         : pxlColourSpecifier;
    markerBundleIndex    : Ctypes.unsigned_short;
    textFont             : Ctypes.unsigned_short;
    textPrecision        : Ctypes.unsigned_short;
    charExpansion        : Ctypes.float;
    charSpacing          : Ctypes.float;
    textColour           : pxlColourSpecifier;
    charHeight           : Ctypes.float;
    charUpVector         : BITS BITSIZE (pxlVector2D) FOR pxlVector2D;
    textPath             : Ctypes.unsigned_short;
    textAlignment        : BITS BITSIZE (pxlTextAlignmentData) 
                                     FOR pxlTextAlignmentData;
    atextHeight          : Ctypes.float;
    atextUpVector        : pxlVector2D;
    atextPath            : Ctypes.unsigned_short;
    atextAlignment       : BITS BITSIZE (pxlTextAlignmentData) 
                                     FOR pxlTextAlignmentData;
    atextStyle           : Ctypes.short;
    textBundleIndex      : Ctypes.unsigned_short;
    lineType             : Ctypes.short;
    lineWidth            : Ctypes.float;
    lineColour           : pxlColourSpecifier;
    curveApprox          : pxlCurveApprox;
    polylineInterp       : Ctypes.short;
    lineBundleIndex      : Ctypes.unsigned_short;
    interiorStyle        : Ctypes.short;
    interiorStyleIndex   : Ctypes.unsigned_short;
    surfaceColour        : pxlColourSpecifier;
    reflectionAttr       : pxlReflectionAttr;
    reflectionModel      : Ctypes.short;
    surfaceInterp        : Ctypes.short;
    bfInteriorStyle      : Ctypes.short;
    bfInteriorStyleIndex : Ctypes.unsigned_short;
    bfSurfaceColour      : BITS BITSIZE (pxlColourSpecifier) 
                                     FOR pxlColourSpecifier;
    bfReflectionAttr     : pxlReflectionAttr;
    bfReflectionModel    : Ctypes.short;
    bfSurfaceInterp      : Ctypes.short;
    surfaceApprox        : pxlSurfaceApprox;
    cullingMode          : Ctypes.unsigned_short;
    distinguish          : Ctypes.unsigned_char;
    pad                  : BYTE;            (* Not present in C source! *)
    patternSize          : BITS BITSIZE (pxlCoord2D) FOR pxlCoord2D;
    patternRefPt         : pxlCoord3D;
    patternRefVec1       : pxlVector3D;
    patternRefVec2       : pxlVector3D;
    interiorBundleIndex  : Ctypes.unsigned_short;
    surfaceEdges         : Ctypes.unsigned_short;
    surfaceEdgeType      : Ctypes.short;
    surfaceEdgeWidth     : Ctypes.float;
    surfaceEdgeColour    : BITS BITSIZE (pxlColourSpecifier) 
                                     FOR pxlColourSpecifier;
    edgeBundleIndex      : Ctypes.unsigned_short;
    localTransform       : pxlMatrix;
    globalTransform      : pxlMatrix;
    modelClip            : Ctypes.unsigned_short;
    modelClipVolume      : pxlModelClipVolumeList;
    viewIndex            : Ctypes.unsigned_short;
    lightState           : pxlLightList;
    depthCueIndex        : Ctypes.unsigned_short;
    asfValues            : ARRAY [0 .. 1] OF unsigned_pxlInt;
                           (* first word is mask, second is values *)
    pickId               : pxlInt;
    HlhsrIdentifier      : unsigned_pxlInt;
    nameSet              : pxlNameSet;
    colourApproxIndex    : Ctypes.unsigned_short;
    rdrColourModel       : Ctypes.short;
    psc                  : pxlPSCData;
  END; (* pxlPCAttributes *)
  pxlPCAttributesStar     = UNTRACED REF pxlPCAttributes;
  pxlPCAttributesStarStar = UNTRACED REF pxlPCAttributesStar;

(* macro for setting bits in a PC value mask *)

(* UNSAFE C MACRO TRANSLATION! *)
PROCEDURE PEX_SetPCAttrMaskBit(VAR mask : ARRAY [0 .. 1] OF Ctypes.int; 
                               attrNum : Ctypes.int);

(* 
 * Definitions for structures 
 *)

TYPE
  pxlStructureInformation = RECORD
    numElements : unsigned_pxlInt;
    length      : unsigned_pxlInt;
    numRefs     : unsigned_pxlInt;
    editMode    : Ctypes.unsigned_short;
    elementPtr  : unsigned_pxlInt;
  END; (* pxlStructureInformation *)
  pxlStructureInformationStar     = UNTRACED REF pxlStructureInformation;
  pxlStructureInformationStarStar = UNTRACED REF pxlStructureInformationStar;

  pxlStructurePath = RECORD
    pathLength  : Ctypes.int;
    elementRefs : pxlElementRefStar;
  END; (* pxlStructurePath *)
  pxlStructurePathStar     = UNTRACED REF pxlStructurePath;
  pxlStructurePathStarStar = UNTRACED REF pxlStructurePathStar;

  pxlNameSetPairList = RECORD
    numPairs : Ctypes.int;
    pairs    : pxlNameSetPairStar;
  END; (* pxlNameSetPairList *)

(* typedefs for search context attributes. defines in PEX.h *)

CONST
  pxlMaxSCShift	= 6;

TYPE
  pxlSCAttributes = RECORD
    position     : pxlCoord3D;
    distance     : Ctypes.float;
    ceiling      : Ctypes.unsigned_short;
    startPath    : pxlStructurePath;
    normalList   : pxlNameSetPairList;
    invertedList : pxlNameSetPairList;
  END; (* pxlSCAttributes *)

(* typedefs for PHIGS workstation info attributes. defines in PEX.h *)

  pxlViewList = RECORD
    numViews : unsigned_pxlInt;
    views    : Ctypes.unsigned_short_star;
  END; (* pxlViewList *)

  pxlStructureInfoList = RECORD
    numStructureInfo : unsigned_pxlInt;
    info             : pxlStructureInfoStar;
  END; (* pxlStructureInfoList *)

  pxlPhigsWksInfo = RECORD
    drawableUpdate   : Ctypes.short;
    visualState      : Ctypes.unsigned_short;
    drawableSurface  : Ctypes.unsigned_short;
    viewUpdate       : Ctypes.unsigned_short;
    definedViews     : pxlViewList;
    wksUpdate        : Ctypes.unsigned_short;
    reqNpcSubvolume  : pxlNpcSubvolume;
    curNpcSubvolume  : pxlNpcSubvolume;
    reqWksViewport   : pxlViewport;
    curWksViewport   : pxlViewport;
    hlhsrUpdate      : Ctypes.unsigned_short;
    reqHlhsrMode     : Ctypes.unsigned_short;
    curHlhsrMode     : Ctypes.unsigned_short;
    drawable         : X.Drawable;
    markerBundle     : pxlLookupTable;
    textBundle       : pxlLookupTable;
    lineBundle       : pxlLookupTable;
    interiorBundle   : pxlLookupTable;
    edgeBundle       : pxlLookupTable;
    colorTable       : pxlLookupTable;
    depthCueTable    : pxlLookupTable;
    lightTable       : pxlLookupTable;
    colorApproxTable : pxlLookupTable;
    patternTable     : pxlLookupTable;
    textFontTable    : pxlLookupTable;
    highlightIncl    : pxlNameSet;
    highlightExcl    : pxlNameSet;
    invisibilityIncl : pxlNameSet;
    invisibilityExcl : pxlNameSet;
    postedStructures : pxlStructureInfoList;
    numPriorities    : unsigned_pxlInt;
  END; (* pxlPhigsWksInfo *)


(* typedefs for PHIGS workstation dynamics. defines in PEX.h *)

  pxlPhigsWksDynamics = RECORD
    viewRep            : Ctypes.unsigned_char;
    markerBundle       : Ctypes.unsigned_char;
    textBundle         : Ctypes.unsigned_char;
    lineBundle         : Ctypes.unsigned_char;
    interiorBundle     : Ctypes.unsigned_char;
    edgeBundle         : Ctypes.unsigned_char;
    colorTable         : Ctypes.unsigned_char;
    patternTable       : Ctypes.unsigned_char;
    wksTransform       : Ctypes.unsigned_char;
    highlightFilter    : Ctypes.unsigned_char;
    invisibilityFilter : Ctypes.unsigned_char;
    hlhsrMode          : Ctypes.unsigned_char;
    structureModify    : Ctypes.unsigned_char;
    postStructure      : Ctypes.unsigned_char;
    unpostStructure    : Ctypes.unsigned_char;
    deleteStructure    : Ctypes.unsigned_char;
    referenceModify    : Ctypes.unsigned_char;
  END; (* pxlPhigsWksDynamics *)

(* 
 * Definitions for renderer 
 *)

  pxlClipList = RECORD
    numDeviceRects : unsigned_pxlInt;
    deviceRect     : pxlDeviceRectStar;
  END; (* pxlClipList *)

  pxlRendererAttributes = RECORD
    pipelineContext   : pxlPipelineContext;
    currentPath       : pxlStructurePath;
    markerBundle      : pxlLookupTable;
    textBundle        : pxlLookupTable;
    lineBundle        : pxlLookupTable;
    interiorBundle    : pxlLookupTable;
    edgeBundle        : pxlLookupTable;
    viewTable         : pxlLookupTable;
    colourTable       : pxlLookupTable;
    depthCueTable     : pxlLookupTable;
    lightTable        : pxlLookupTable;
    colourApproxTable : pxlLookupTable;
    patternTable      : pxlLookupTable;
    textFontTable     : pxlLookupTable;
    highlightIncl     : pxlNameSet;
    highlightExcl     : pxlNameSet;
    invisibilityIncl  : pxlNameSet;
    invisibilityExcl  : pxlNameSet;
    rendererState     : Ctypes.unsigned_short;
    hlhsrMode         : Ctypes.unsigned_short;
    npcSubvolume      : BITS BITSIZE (pxlNpcSubvolume) FOR pxlNpcSubvolume;
    viewport          : pxlViewport;
    clipList          : pxlClipList;
  (*** rest is new in the AOSF version ***)
    pickInclusion     : pexNameSet;
    pickExclusion     : pexNameSet;
    pickStartPath     : pxlStructurePath;
    backgroundColour  : pxlColourSpecifier;
    clearI            : Ctypes.unsigned_char;
    clearZ            : Ctypes.unsigned_char;
    echoMode          : Ctypes.unsigned_char;
  END; (* pxlRendererAttributes *)
  pxlRendererAttributesStar = UNTRACED REF pxlRendererAttributes;
  pxlRendererAttributesStarStar = UNTRACED REF pxlRendererAttributesStar;

(*
 * Generic definition of pex OC buffers
 *)

  pxlOCBuf = RECORD
    initLengthRequested : Ctypes.int;   (* hint for size of oc request       *)
    initLength          : Ctypes.int;
    display             : X. DisplayStar;
    extOpcode           : Ctypes.char;  (* opcode for pex extension          *)
    pexOpcode           : Ctypes.char;  (* opcode for ROC or StoreElements   *)
    retainedOCBuffer    : Ctypes.short; (* for transient, 1 for retained     *)
    target              : Ctypes.unsigned_long; 
                                    (* renderer id or structure id           *)
    basePtr             : Ctypes.unsigned_int_star; 
                                    (* points to start of current oc request *)
    curPtr              : Ctypes.unsigned_int_star; 
                                      (* where to store next oc              *)
    bufMax              : Ctypes.unsigned_int_star; 
                                      (* end of buffer containing oc request *)
    ocWordsLeft         : Ctypes.int; (* words left to copy to oc buffer,    *)
	  			      (* words allocated via PEXInitOC       *)
    lrTotalNum          : Ctypes.int; (* number of packets for large request *)
    lrTotalLength       : Ctypes.int; (* size of data for large request      *)
    lrSequenceNum       : Ctypes.int; (* # of current large request packet   *)
    lrBufferLength      : Ctypes.int; (* # words avail in a lr packet        *)
    lrBufferLeft        : Ctypes.int; (* # of words left in current lr packet*)
    OCError             : PROCEDURE ();
    FreeOCBuf           : PROCEDURE ();
    ClearOCBuf          : PROCEDURE ();
    SetOCBufType        : PROCEDURE ();
    SetOCBufTarget      : PROCEDURE ();
    GetOCBufType        : PROCEDURE ();
    GetOCBufFreeSpace   : PROCEDURE () : Ctypes.int;
    GetMaxOCChunkSize   : PROCEDURE () : Ctypes.int;
(* DIFFERS FROM ORIGINAL C SOURCE !! *)
    FlushOCBuf          : (PROCEDURE (ocbuf : pxlOCBufStar)); 
(*    FlushOCBuf          : PROCEDURE (); *)
    SendOCBuf           : (PROCEDURE (ocbuf : pxlOCBufStar));
(*    SendOCBuf           : PROCEDURE (); *)
    StartOC             : PROCEDURE () : Ctypes.int;
    StartLargeRequest   : PROCEDURE () : Ctypes.int;
    FinishOC            : PROCEDURE ();
    GetOCWords          : PROCEDURE () : Ctypes.unsigned_int_star;
    CopyOCBytes         : PROCEDURE ();
    StoreOCList         : PROCEDURE ();
  END; (* pxlOCBuf *)
  pxlOCBufStar     = UNTRACED REF pxlOCBuf;
  pxlOCBufStarStar = UNTRACED REF pxlOCBufStar;

(*****************************************************************************)
(* Should be in pexlibprotos.h, but is missing for some reason:              *)
(*****************************************************************************)

<*EXTERNAL*> PROCEDURE PEXSetColourApproxIndex (
		ocbuf : pxlOCBufStar;
		index : Ctypes.int);

(*****************************************************************************)
(* Based on pexlibprotos.h Version 0.1                                       *)
(*                                                                           *)
(* Not yet complete!                                                         *)
(*****************************************************************************)

(*
 *
 *  File Name:
 *     pexlibprotos.h
 *
 *  Subsystem:
 *     pex
 *
 *  Version: 
 *     0.1
 *
 *  Abstract:
 *     Define prototypes for all external pexlib routines
 *
 *  Keywords:
 *     pex, pexlib
 *
 *  Environment:
 *     included by pexlib.h if PROTOS is defined
 *
 *  Author:
 *     Andrew F. Vesper, Worksystems Graphics Software, PEXlib
 *
 *  Creation Date:
 *     13-Mar-1991
 *
 *  Modification History:
 *	xx-Apr-91   P?W	    Updated for PEX protocol V5
 *	29-Apr-91   AFV	    Fixed some prototype definitions.
 *	 6-May-91   AFV	    Change names of PEXPHIGS... routines
 *	11-Jul-91   STC	    Add Ellipse stuff
 *       9-Sep-91   SB      Added 2 new OC's MPEXGridRectangular MPEXGridRadial
 *       13-Sep-91  SB      Modified MPEXGridRectangular to take an Origin as 
 *			    input.
 *      25-Sep-91   SB      Added a new prototype MPEXReferToStructure
 *      10-Oct-91   DM      Added the new pipeline context requests
 *                          MPEXCopyNewPipelineContext,
 *                          MPEXGetNewPipelineContext,
 *                          MPEXChangeNewPipelineContext. Also added new text
 *                          skewed text attribute oc MPEXSetAnnotationTextSkew,
 *                          MPEXSetTextSkew.
 *)

(*
 * ErrorFunctionType is used in 
 *     PEXAllocateRetainedOCBuffer 
 *     PEXAllocateTransientOCBuffer 
 *     PEXAllocateOCBuffer
 * It is defined as "PROCEDURE ()" in the C source, but the above procedures
 * take PEXDefaultOCError as an argument, which has type
 *     PROCEDURE (ocbuf : pxlOCBufStar; value : Ctypes.int)
 *)

TYPE 
  ErrorFunctionType = PROCEDURE (ocbuf : pxlOCBufStar ; value : Ctypes.int);

(* 
 * Routines defined in pl_oc_buff.c
 *)

<*EXTERNAL*> PROCEDURE PEXAllocateRetainedOCBuffer (
		(* INPUT  *) display       : X.DisplayStar;
		(* INPUT  *) type          : Ctypes.int;
		(* INPUT  *) target        : X.XID;
		(* INPUT  *) errorFunction : ErrorFunctionType;
		(* INPUT  *) initSize      : Ctypes.unsigned_int)
	     : pxlOCBufStar;

<*EXTERNAL*> PROCEDURE PEXAllocateTransientOCBuffer (
		(* INPUT  *) display       : X.DisplayStar;
		(* INPUT  *) type          : Ctypes.int;
		(* INPUT  *) target        : X.XID;
		(* INPUT  *) errorFunction : ErrorFunctionType;
		(* INPUT  *) initSize      : Ctypes.int)
	     : pxlOCBufStar;

<*EXTERNAL*> PROCEDURE PEXGetDefaultOCBufferType (
		(* IN-OUT *) ocbuf         : pxlOCBufStar;
		(* OUTPUT *) displayReturn : DisplayStarStar;
		(* OUTPUT *) typeReturn    : Ctypes.int_star;
		(* OUTPUT *) targetReturn  : XIDStar);

<*EXTERNAL*> PROCEDURE PEXGetWordsError (
		(* IN-OUT *) ocbuf  : pxlOCBufStar;
		(* INPUT  *) nWords : Ctypes.int)
	     : Ctypes.unsigned_int_star;

<*EXTERNAL*> PROCEDURE PEXCopyBytesError (
		(* IN-OUT *) ocbuf  : pxlOCBufStar;
		(* INPUT  *) nBytes : Ctypes.int;
		(* INPUT  *) buffer : Ctypes.char_star);
			
<*EXTERNAL*> PROCEDURE PEXInitOC (
		(* IN-OUT *) ocbuf          : pxlOCBufStar;
		(* INPUT  *) ocType         : Ctypes.int;
		(* INPUT  *) ocHeaderLength : Ctypes.int;
		(* INPUT  *) ocDataLength   : Ctypes.int;
		(* INPUT  *) pReq           : Ctypes.char_star_star);

<*EXTERNAL*> PROCEDURE PEXCopyBytesToOC (
		(* IN-OUT *) ocbuf  : pxlOCBufStar;
		(* INPUT  *) nBytes : Ctypes.int;
		(* INPUT  *) OCData : Ctypes.char_star);

<*EXTERNAL*> PROCEDURE PEXCopyWordsToOC (
		(* IN-OUT *) ocbuf  : pxlOCBufStar;
		(* INPUT  *) nWords : Ctypes.int;
		(* INPUT  *) OCData : Ctypes.char_star);

<*EXTERNAL*> PROCEDURE PEXGetOCWords (
		(* IN-OUT *) ocbuf  : pxlOCBufStar;
		(* INPUT  *) nWords : Ctypes.int)
	     : Ctypes.unsigned_int_star;

<*EXTERNAL*> PROCEDURE PEXAddOC (
		(* IN-OUT *) ocbuf  : pxlOCBufStar;
		(* INPUT  *) ocType : Ctypes.unsigned_int;
		(* INPUT  *) size   : Ctypes.int;
		(* INPUT  *) OCData : Ctypes.char_star);

<*EXTERNAL*> PROCEDURE PEXAddListOC (
		(* IN-OUT *) ocbuf       : pxlOCBufStar;
		(* INPUT  *) ocType      : Ctypes.unsigned_int;
		(* INPUT  *) countNeeded : Ctypes.int;
		(* INPUT  *) count       : Ctypes.int;
		(* INPUT  *) elementSize : Ctypes.int;
		(* INPUT  *) elementList : Ctypes.char_star);

<*EXTERNAL*> PROCEDURE PEXDefaultStoreOCList (
		(* IN-OUT *) ocbuf       : pxlOCBufStar;
		(* INPUT  *) numElements : Ctypes.int;
		(* INPUT  *) ocList      : pxlElementInfoStar) ;

<*EXTERNAL*> PROCEDURE PEXDefaultTransientOCBuffer () : Ctypes.int;

<*EXTERNAL*> PROCEDURE PEXDefaultRetainedOCBuffer () : Ctypes.int;

<*EXTERNAL*> PROCEDURE PEXDefaultOCError (
		(* IN-OUT *) ocbuf : pxlOCBufStar;
		(* INPUT  *) value : Ctypes.int);

<*EXTERNAL*> PROCEDURE PEXSetOCOverflow (
		(* INPUT  *) value : Ctypes.int);

<*EXTERNAL*> PROCEDURE PEXGetOCOverflow () : Ctypes.int;

<*EXTERNAL*> PROCEDURE PEXClearOCOverflow ();

(* 
 * Routines defined in pl_oc_attr.c
 *)	

<*EXTERNAL*> PROCEDURE PEXSetMarkerType (
		(* IN-OUT *) ocbuf : pxlOCBufStar;
		(* INPUT  *) type  : Ctypes.int);

<*EXTERNAL*> PROCEDURE PEXSetMarkerScale (
		(* IN-OUT *) ocbuf : pxlOCBufStar;
		(* INPUT  *) scale : Ctypes.double);

<*EXTERNAL*> PROCEDURE PEXSetMarkerColourIndex (
		(* IN-OUT *) ocbuf : pxlOCBufStar;
		(* INPUT  *) index : Ctypes.int);

<*EXTERNAL*> PROCEDURE PEXSetMarkerColour (
		(* IN-OUT *) ocbuf  : pxlOCBufStar;
		(* INPUT  *) colour : pxlColourStar);
	
<*EXTERNAL*> PROCEDURE PEXSetMarkerBundleIndex (
		(* IN-OUT *) ocbuf : pxlOCBufStar;
		(* INPUT  *) index : Ctypes.int);

<*EXTERNAL*> PROCEDURE PEXSetTextFontIndex (
		(* IN-OUT *) ocbuf : pxlOCBufStar;
		(* INPUT  *) index : Ctypes.int);

<*EXTERNAL*> PROCEDURE PEXSetTextPrecision (
		(* IN-OUT *) ocbuf     : pxlOCBufStar;
		(* INPUT  *) precision : Ctypes.int);
	
<*EXTERNAL*> PROCEDURE PEXSetCharExpansion (
		(* IN-OUT *) ocbuf     : pxlOCBufStar;
		(* INPUT  *) expansion : Ctypes.double);

<*EXTERNAL*> PROCEDURE PEXSetCharSpacing (
		(* IN-OUT *) ocbuf   : pxlOCBufStar;
		(* INPUT  *) spacing : Ctypes.double);

<*EXTERNAL*> PROCEDURE PEXSetTextColourIndex (
		(* IN-OUT *) ocbuf : pxlOCBufStar;
		(* INPUT  *) index : Ctypes.int);

<*EXTERNAL*> PROCEDURE PEXSetTextColour (
		(* IN-OUT *) ocbuf  : pxlOCBufStar;
		(* INPUT  *) colour : pxlColourStar);

<*EXTERNAL*> PROCEDURE PEXSetCharHeight (
		(* IN-OUT *) ocbuf  : pxlOCBufStar;
		(* INPUT  *) height : Ctypes.double);

<*EXTERNAL*> PROCEDURE PEXSetCharUpVector (
		(* IN-OUT *) ocbuf : pxlOCBufStar;
		(* INPUT  *) x     : Ctypes.double;
		(* INPUT  *) y     : Ctypes.double);

<*EXTERNAL*> PROCEDURE PEXSetTextPath (
		(* IN-OUT *) ocbuf : pxlOCBufStar;
		(* INPUT  *) path  : Ctypes.int);

<*EXTERNAL*> PROCEDURE PEXSetTextAlignment (
		(* IN-OUT *) ocbuf      : pxlOCBufStar;
		(* INPUT  *) halignment : Ctypes.int;
		(* INPUT  *) valignment : Ctypes.int);

<*EXTERNAL*> PROCEDURE PEXSetATextHeight (
		(* IN-OUT *) ocbuf  : pxlOCBufStar;
		(* INPUT  *) height : Ctypes.double);

<*EXTERNAL*> PROCEDURE PEXSetATextUpVector (
		(* IN-OUT *) ocbuf : pxlOCBufStar;
		(* INPUT  *) x     : Ctypes.double;
		(* INPUT  *) y     : Ctypes.double);

<*EXTERNAL*> PROCEDURE PEXSetATextPath (
		(* IN-OUT *) ocbuf : pxlOCBufStar;
		(* INPUT  *) path  : Ctypes.int);

<*EXTERNAL*> PROCEDURE PEXSetATextAlignment (
		(* IN-OUT *) ocbuf      : pxlOCBufStar;
		(* INPUT  *) halignment : Ctypes.int;
		(* INPUT  *) valignment : Ctypes.int);

<*EXTERNAL*> PROCEDURE PEXSetATextStyle (
		(* IN-OUT *) ocbuf : pxlOCBufStar;
		(* INPUT  *) style : Ctypes.int);

<*EXTERNAL*> PROCEDURE PEXSetTextBundleIndex (
		(* IN-OUT *) ocbuf : pxlOCBufStar;
		(* INPUT  *) index : Ctypes.int);

<*EXTERNAL*> PROCEDURE PEXSetLineType (
		(* IN-OUT *) ocbuf : pxlOCBufStar;
		(* INPUT  *) type  : Ctypes.int);

<*EXTERNAL*> PROCEDURE PEXSetLineWidth (
		(* IN-OUT *) ocbuf : pxlOCBufStar;
		(* INPUT  *) width : Ctypes.double);

<*EXTERNAL*> PROCEDURE PEXSetLineColourIndex (
		(* IN-OUT *) ocbuf : pxlOCBufStar;
		(* INPUT  *) index : Ctypes.int);

<*EXTERNAL*> PROCEDURE PEXSetLineColour (
		(* IN-OUT *) ocbuf  : pxlOCBufStar;
		(* INPUT  *) colour : pxlColourStar);

<*EXTERNAL*> PROCEDURE PEXSetCurveApproxMethod (
		(* IN-OUT *) ocbuf        : pxlOCBufStar;
		(* INPUT  *) approxMethod : Ctypes.int;
		(* INPUT  *) tolerance    : Ctypes.double);

<*EXTERNAL*> PROCEDURE PEXSetPolylineInterpMethod (
		(* IN-OUT *) ocbuf  : pxlOCBufStar;
		(* INPUT  *) method : Ctypes.int);

<*EXTERNAL*> PROCEDURE PEXSetLineBundleIndex (
		(* IN-OUT *) ocbuf : pxlOCBufStar;
		(* INPUT  *) index : Ctypes.int);

<*EXTERNAL*> PROCEDURE PEXSetInteriorStyle (
		(* IN-OUT *) ocbuf : pxlOCBufStar;
		(* INPUT  *) style : Ctypes.int);

<*EXTERNAL*> PROCEDURE PEXSetInteriorStyleIndex (
		(* IN-OUT *) ocbuf : pxlOCBufStar;
		(* INPUT  *) index : Ctypes.int);

<*EXTERNAL*> PROCEDURE PEXSetSurfaceColourIndex (
		(* IN-OUT *) ocbuf : pxlOCBufStar;
		(* INPUT  *) index : Ctypes.int);

<*EXTERNAL*> PROCEDURE PEXSetSurfaceColour (
		(* IN-OUT *) ocbuf  : pxlOCBufStar;
		(* INPUT  *) colour : pxlColourStar);

<*EXTERNAL*> PROCEDURE PEXSetReflectionAttributes (
		(* IN-OUT *) ocbuf          : pxlOCBufStar;
		(* INPUT  *) reflectionAttr : pxlReflectionAttrStar);

<*EXTERNAL*> PROCEDURE PEXSetReflectionModel (
		(* IN-OUT *) ocbuf : pxlOCBufStar;
		(* INPUT  *) model : Ctypes.int);

<*EXTERNAL*> PROCEDURE PEXSetSurfaceInterpMethod (
		(* IN-OUT *) ocbuf  : pxlOCBufStar;
		(* INPUT  *) method : Ctypes.int);

<*EXTERNAL*> PROCEDURE PEXSetBFInteriorStyle (
		(* IN-OUT *) ocbuf : pxlOCBufStar;
		(* INPUT  *) style : Ctypes.int);

<*EXTERNAL*> PROCEDURE PEXSetBFInteriorStyleIndex (
		(* IN-OUT *) ocbuf : pxlOCBufStar;
		(* INPUT  *) index : Ctypes.int);

<*EXTERNAL*> PROCEDURE PEXSetBFSurfaceColourIndex (
		(* IN-OUT *) ocbuf : pxlOCBufStar;
		(* INPUT  *) index : Ctypes.int);

<*EXTERNAL*> PROCEDURE PEXSetBFSurfaceColour (
		(* IN-OUT *) ocbuf  : pxlOCBufStar;
		(* INPUT  *) colour : pxlColourStar);

<*EXTERNAL*> PROCEDURE PEXSetBFReflectionAttributes (
		(* IN-OUT *) ocbuf          : pxlOCBufStar;
		(* INPUT  *) reflectionAttr : pxlReflectionAttrStar);

<*EXTERNAL*> PROCEDURE PEXSetBFReflectionModel (
		(* IN-OUT *) ocbuf : pxlOCBufStar;
		(* INPUT  *) model : Ctypes.int);

<*EXTERNAL*> PROCEDURE PEXSetBFSurfaceInterpMethod (
		(* IN-OUT *) ocbuf  : pxlOCBufStar;
		(* INPUT  *) method : Ctypes.int);

<*EXTERNAL*> PROCEDURE PEXSetSurfaceApproxMethod (
		(* IN-OUT *) ocbuf        : pxlOCBufStar;
		(* INPUT  *) approxMethod : Ctypes.int;
		(* INPUT  *) sTolerance   : Ctypes.double;
		(* INPUT  *) tTolerance   : Ctypes.double);

<*EXTERNAL*> PROCEDURE PEXSetFacetCullingMode (
		(* IN-OUT *) ocbuf : pxlOCBufStar;
		(* INPUT  *) mode  : Ctypes.int);

<*EXTERNAL*> PROCEDURE PEXSetFacetDistinguishFlag (
		(* IN-OUT *) ocbuf : pxlOCBufStar;
		(* INPUT  *) flag  : Ctypes.int);

<*EXTERNAL*> PROCEDURE PEXSetPatternSize (
		(* IN-OUT *) ocbuf  : pxlOCBufStar;
		(* INPUT  *) width  : Ctypes.double;
		(* INPUT  *) height : Ctypes.double);

<*EXTERNAL*> PROCEDURE PEXSetPatternRefPt (
		(* IN-OUT *) ocbuf : pxlOCBufStar;
		(* INPUT  *) x     : Ctypes.double;
		(* INPUT  *) y     : Ctypes.double);

<*EXTERNAL*> PROCEDURE PEXSetPatternAttributes (
		(* IN-OUT *) ocbuf : pxlOCBufStar;
		(* INPUT  *) refPt : pxlCoord3DStar;
		(* INPUT  *) vec1  : pxlVector3DStar;
		(* INPUT  *) vec2  : pxlVector3DStar);

<*EXTERNAL*> PROCEDURE PEXSetInteriorBundleIndex (
		(* IN-OUT *) ocbuf : pxlOCBufStar;
		(* INPUT  *) index : Ctypes.int);

<*EXTERNAL*> PROCEDURE PEXSetSurfaceEdgeFlag (
		(* IN-OUT *) ocbuf : pxlOCBufStar;
		(* INPUT  *) flag  : Ctypes.int);

<*EXTERNAL*> PROCEDURE PEXSetSurfaceEdgeType (
		(* IN-OUT *) ocbuf : pxlOCBufStar;
		(* INPUT  *) type  : Ctypes.int);

<*EXTERNAL*> PROCEDURE PEXSetSurfaceEdgeWidth (
		(* IN-OUT *) ocbuf : pxlOCBufStar;
		(* INPUT  *) width : Ctypes.double);

<*EXTERNAL*> PROCEDURE PEXSetSurfaceEdgeColourIndex (
		(* IN-OUT *) ocbuf : pxlOCBufStar;
		(* INPUT  *) index : Ctypes.int);

<*EXTERNAL*> PROCEDURE PEXSetSurfaceEdgeColour (
		(* IN-OUT *) ocbuf  : pxlOCBufStar;
		(* INPUT  *) colour : pxlColourStar);

<*EXTERNAL*> PROCEDURE PEXSetEdgeBundleIndex (
		(* IN-OUT *) ocbuf : pxlOCBufStar;
		(* INPUT  *) index : Ctypes.int);

<*EXTERNAL*> PROCEDURE PEXSetIndividualASF (
		(* IN-OUT *) ocbuf     : pxlOCBufStar;
		(* INPUT  *) attribute : Ctypes.int;
		(* INPUT  *) value     : Ctypes.int);

<*EXTERNAL*> PROCEDURE PEXSetLocalTransform (
		(* IN-OUT *) ocbuf     : pxlOCBufStar;
		(* INPUT  *) transform : pxlLocalTransform3DDataStar);

<*EXTERNAL*> PROCEDURE PEXSetLocalTransform2D (
		(* IN-OUT *) ocbuf     : pxlOCBufStar;
		(* INPUT  *) transform : pxlLocalTransform2DDataStar);

<*EXTERNAL*> PROCEDURE PEXSetGlobalTransform (
		(* IN-OUT *) ocbuf     : pxlOCBufStar;
		(* INPUT  *) transform : pxlMatrixStar);
                (* was pxlMatrix (C vs Modula-3 Array passing problem) *)

<*EXTERNAL*> PROCEDURE PEXSetGlobalTransform2D (
		(* IN-OUT *) ocbuf     : pxlOCBufStar;
		(* INPUT  *) transform : pxlMatrix3X3);

<*EXTERNAL*> PROCEDURE PEXSetModelClipFlag (
		(* IN-OUT *) ocbuf : pxlOCBufStar;
		(* INPUT  *) flag  : Ctypes.int);

<*EXTERNAL*> PROCEDURE PEXSetModelClipVolume (
		(* IN-OUT *) ocbuf         : pxlOCBufStar;
		(* INPUT  *) operator      : Ctypes.int;
		(* INPUT  *) halfSpaces    : pxlHalfSpaceStar;
		(* INPUT  *) numHalfSpaces : Ctypes.int);

<*EXTERNAL*> PROCEDURE PEXSetModelClipVolume2D (
		(* IN-OUT *) ocbuf         : pxlOCBufStar;
		(* INPUT  *) operator      : Ctypes.int;
		(* INPUT  *) halfSpaces    : pxlHalfSpace2DStar;
		(* INPUT  *) numHalfSpaces : Ctypes.int);

<*EXTERNAL*> PROCEDURE PEXRestoreModelClipVolume (
		(* IN-OUT *) ocbuf : pxlOCBufStar);

<*EXTERNAL*> PROCEDURE PEXSetViewIndex (
		(* IN-OUT *) ocbuf : pxlOCBufStar;
		(* INPUT  *) index : Ctypes.int);

<*EXTERNAL*> PROCEDURE PEXSetLightSourceState (
		(* IN-OUT *) ocbuf      : pxlOCBufStar;
		(* INPUT  *) enable     : pxlTableIndexStar;
		(* INPUT  *) numEnable  : Ctypes.int;
		(* INPUT  *) disable    : pxlTableIndexStar;
		(* INPUT  *) numDisable : Ctypes.int);

<*EXTERNAL*> PROCEDURE PEXSetDepthCueIndex (
		(* IN-OUT *) ocbuf : pxlOCBufStar;
		(* INPUT  *) index : Ctypes.int);

<*EXTERNAL*> PROCEDURE PEXSetRenderingColourModel (
		(* IN-OUT *) ocbuf : pxlOCBufStar;
		(* INPUT  *) model : Ctypes.int);

<*EXTERNAL*> PROCEDURE PEXSetParaSurfCharacteristics (
		(* IN-OUT *) ocbuf   : pxlOCBufStar;
		(* INPUT  *) pscType : Ctypes.int;
		(* INPUT  *) pscData : Ctypes.char_star);

<*EXTERNAL*> PROCEDURE PEXSetPickID (
		(* IN-OUT *) ocbuf : pxlOCBufStar;
		(* INPUT  *) id    : Ctypes.unsigned_long);

<*EXTERNAL*> PROCEDURE PEXSetHlhsrID (
		(* IN-OUT *) ocbuf : pxlOCBufStar;
		(* INPUT  *) id    : Ctypes.unsigned_long);

<*EXTERNAL*> PROCEDURE PEXAddToNameSet (
		(* IN-OUT *) ocbuf    : pxlOCBufStar;
		(* INPUT  *) names    : pxlNameStar;
		(* INPUT  *) numNames : Ctypes.int);

<*EXTERNAL*> PROCEDURE PEXRemoveFromNameSet (
		(* IN-OUT *) ocbuf    : pxlOCBufStar;
		(* INPUT  *) names    : pxlNameStar;
		(* INPUT  *) numNames : Ctypes.int);
(* 
 * Routines defined in pl_oc_prim.c
 *)

<*EXTERNAL*> PROCEDURE PEXMarkers (
		(* IN-OUT *) ocbuf     : pxlOCBufStar;
		(* INPUT  *) points    : pxlCoord3DStar;
		(* INPUT  *) numPoints : Ctypes.int);

<*EXTERNAL*> PROCEDURE PEXMarkers2D (
		(* IN-OUT *) ocbuf  : pxlOCBufStar;
		(* INPUT  *) pts    : pxlCoord2DStar;
		(* INPUT  *) numPts : Ctypes.int);

<*EXTERNAL*> PROCEDURE PEXText (
		(* IN-OUT *) ocbuf  : pxlOCBufStar;
		(* INPUT  *) origin : pxlCoord3DStar;
		(* INPUT  *) vec1   : pxlVector3DStar;
		(* INPUT  *) vec2   : pxlVector3DStar;
		(* INPUT  *) string : Ctypes.char_star;
		(* INPUT  *) count  : Ctypes.int);

<*EXTERNAL*> PROCEDURE PEXText2D (
		(* IN-OUT *) ocbuf  : pxlOCBufStar;
		(* INPUT  *) origin : pxlCoord2DStar;
		(* INPUT  *) string : Ctypes.char_star;
		(* INPUT  *) count  : Ctypes.int);

<*EXTERNAL*> PROCEDURE PEXAnnotationText (
		(* IN-OUT *) ocbuf  : pxlOCBufStar;
		(* INPUT  *) origin : pxlCoord3DStar;
		(* INPUT  *) offset : pxlCoord3DStar;
		(* INPUT  *) string : Ctypes.char_star;
		(* INPUT  *) count  : Ctypes.int);

<*EXTERNAL*> PROCEDURE PEXAnnotationText2D (
		(* IN-OUT *) ocbuf  : pxlOCBufStar;
		(* INPUT  *) origin : pxlCoord2DStar;
		(* INPUT  *) offset : pxlCoord2DStar;
		(* INPUT  *) string : Ctypes.char_star;
		(* INPUT  *) count  : Ctypes.int);

<*EXTERNAL*> PROCEDURE PEXMonoEncodedText (
		(* INPUT  *) ocbuf           : pxlOCBufStar;
		(* INPUT  *) origin          : pxlCoord3DStar;
		(* INPUT  *) vec1            : pxlVector3DStar;
		(* INPUT  *) vec2            : pxlVector3DStar;
		(* INPUT  *) numEncodings    : Ctypes.int;
		(* INPUT  *) encodedTextList : pxlMonoEncodedTextDataStar);

<*EXTERNAL*> PROCEDURE PEXMonoEncodedText2D (
		(* INPUT  *) ocbuf           : pxlOCBufStar;
		(* INPUT  *) origin          : pxlCoord2DStar;
		(* INPUT  *) numEncodings    : Ctypes.int;
		(* INPUT  *) encodedTextList : pxlMonoEncodedTextDataStar);

<*EXTERNAL*> PROCEDURE PEXMonoEncodedAnnoText (
		(* INPUT  *) ocbuf           : pxlOCBufStar;
		(* INPUT  *) origin          : pxlCoord3DStar;
		(* INPUT  *) offset          : pxlCoord3DStar;
		(* INPUT  *) numEncodings    : Ctypes.int;
		(* INPUT  *) encodedTextList : pxlMonoEncodedTextDataStar);

<*EXTERNAL*> PROCEDURE PEXMonoEncodedAnnoText2D (
		(* INPUT  *) ocbuf           : pxlOCBufStar;
		(* INPUT  *) origin          : pxlCoord2DStar;
		(* INPUT  *) offset          : pxlCoord2DStar;
		(* INPUT  *) numEncodings    : Ctypes.int;
		(* INPUT  *) encodedTextList : pxlMonoEncodedTextDataStar);

<*EXTERNAL*> PROCEDURE PEXPolyline (
		(* IN-OUT *) ocbuf       : pxlOCBufStar;
		(* INPUT  *) vertices    : pxlCoord3DStar;
		(* INPUT  *) numVertices : Ctypes.int);

<*EXTERNAL*> PROCEDURE PEXPolyline2D (
		(* IN-OUT *) ocbuf       : pxlOCBufStar;
		(* INPUT  *) vertices    : pxlCoord2DStar;
		(* INPUT  *) numVertices : Ctypes.int);

<*EXTERNAL*> PROCEDURE PEXPolylineSet (
		(* IN-OUT *) ocbuf            : pxlOCBufStar;
		(* INPUT  *) vertexAttributes : Ctypes.unsigned_long;
		(* INPUT  *) polylines        : pxlPolylineDataStar;
		(* INPUT  *) numPolylines     : Ctypes.int);

<*EXTERNAL*> PROCEDURE PEXNurbCurve (
		(* IN-OUT *) ocbuf     : pxlOCBufStar;
		(* INPUT  *) order     : Ctypes.int;
		(* INPUT  *) type      : Ctypes.int;
		(* INPUT  *) tmin      : Ctypes.double;
		(* INPUT  *) tmax      : Ctypes.double;
		(* INPUT  *) knots     : Ctypes.float_star;
		(* INPUT  *) points    : Ctypes.char_star;
		(* INPUT  *) numPoints : Ctypes.int);

<*EXTERNAL*> PROCEDURE PEXFillArea (
		(* IN-OUT *) ocbuf       : pxlOCBufStar;
		(* INPUT  *) shape       : Ctypes.int;
		(* INPUT  *) ignoreEdges : Ctypes.int;
		(* INPUT  *) points      : pxlCoord3DStar;
		(* INPUT  *) numPoints   : Ctypes.int);

<*EXTERNAL*> PROCEDURE PEXFillArea2D (
		(* IN-OUT *) ocbuf       : pxlOCBufStar;
		(* INPUT  *) shape       : Ctypes.int;
		(* INPUT  *) ignoreEdges : Ctypes.int;
		(* INPUT  *) points      : pxlCoord2DStar;
		(* INPUT  *) numPoints   : Ctypes.int);

<*EXTERNAL*> PROCEDURE PEXMultiFillAreaData (
		(* IN-OUT *) ocbuf            : pxlOCBufStar;
		(* INPUT  *) shape            : Ctypes.int;
		(* INPUT  *) ignoreEdges      : Ctypes.int;
		(* INPUT  *) facetAttributes  : Ctypes.unsigned_long;
		(* INPUT  *) vertexAttributes : Ctypes.unsigned_long;
		(* INPUT  *) polygons         : pxlPolygonDataStar;
		(* INPUT  *) numPolygons      : Ctypes.int);

<*EXTERNAL*> PROCEDURE PEXFillAreaSet (
		(* IN-OUT *) ocbuf       : pxlOCBufStar;
		(* INPUT  *) shape       : Ctypes.int;
		(* INPUT  *) ignoreEdges : Ctypes.int;
		(* INPUT  *) contourHint : Ctypes.int;
		(* INPUT  *) polygons    : pxlPolygonDataStar;
		(* INPUT  *) numPolygons : Ctypes.int);

<*EXTERNAL*> PROCEDURE PEXFillAreaSet2D (
		(* IN-OUT *) ocbuf       : pxlOCBufStar;
		(* INPUT  *) shape       : Ctypes.int;
		(* INPUT  *) ignoreEdges : Ctypes.int;
		(* INPUT  *) contourHint : Ctypes.int;
		(* INPUT  *) polygons    : pxlPolygonDataStar;
		(* INPUT  *) numPolygons : Ctypes.int);

<*EXTERNAL*> PROCEDURE PEXMultiFillAreaSetData (
		(* IN-OUT *) ocbuf              : pxlOCBufStar;
		(* INPUT  *) shape              : Ctypes.int;
		(* INPUT  *) ignoreEdges        : Ctypes.int;
		(* INPUT  *) contourHint        : Ctypes.int;
		(* INPUT  *) facetAttributes    : Ctypes.unsigned_long;
		(* INPUT  *) vertexAttributes   : Ctypes.unsigned_long;
		(* INPUT  *) complexPolygons    : pxlComplexPolygonDataStar;
		(* INPUT  *) numComplexPolygons : Ctypes.int);

<*EXTERNAL*> PROCEDURE PEXSetOfFillAreaSets (
		(* IN-OUT *) ocbuf            : pxlOCBufStar;
		(* INPUT  *) shape            : Ctypes.int;
		(* INPUT  *) contourHint      : Ctypes.int;
		(* INPUT  *) contoursAllOne   : Ctypes.int;
		(* INPUT  *) FASAttributes    : Ctypes.unsigned_long;
		(* INPUT  *) vertexAttributes : Ctypes.unsigned_long;
		(* INPUT  *) edgeAttributes   : Ctypes.unsigned_long;
		(* INPUT  *) numFAS           : Ctypes.int;
		(* INPUT  *) numVertices      : Ctypes.int;
		(* INPUT  *) numEdges         : Ctypes.int;
		(* INPUT  *) numContours      : Ctypes.int;
		(* INPUT  *) FASOptDataList   : Ctypes.char_star;
		(* INPUT  *) verticeList      : Ctypes.char_star;
		(* INPUT  *) edgeList         : Ctypes.char_star;
		(* INPUT  *) FASList          : Ctypes.char_star);

<*EXTERNAL*> PROCEDURE PEXTriangleStrip (
		(* IN-OUT *) ocbuf            : pxlOCBufStar;
		(* INPUT  *) facetAttributes  : Ctypes.unsigned_long;
		(* INPUT  *) vertexAttributes : Ctypes.unsigned_long;
		(* INPUT  *) facetData        : Ctypes.char_star;
		(* INPUT  *) points           : Ctypes.char_star;
		(* INPUT  *) numPoints        : Ctypes.int);

<*EXTERNAL*> PROCEDURE PEXQuadMesh (
		(* IN-OUT *) ocbuf            : pxlOCBufStar;
		(* INPUT  *) shape            : Ctypes.int;
		(* INPUT  *) facetAttributes  : Ctypes.unsigned_long;
		(* INPUT  *) vertexAttributes : Ctypes.unsigned_long;
		(* INPUT  *) facetData        : Ctypes.char_star;
		(* INPUT  *) points           : Ctypes.char_star;
		(* INPUT  *) numPointsM       : Ctypes.int;
		(* INPUT  *) numPointsN       : Ctypes.int);

<*EXTERNAL*> PROCEDURE PEXNurbSurface (
		(* IN-OUT *) ocbuf : pxlOCBufStar;
		(* INPUT  *) nurb  : pxlNurbSurfaceStar);

<*EXTERNAL*> PROCEDURE PEXCellArray (
		(* IN-OUT *) ocbuf    : pxlOCBufStar;
		(* INPUT  *) pt1      : pxlCoord3DStar;
		(* INPUT  *) pt2      : pxlCoord3DStar;
		(* INPUT  *) pt3      : pxlCoord3DStar;
		(* INPUT  *) dx       : Ctypes.int;
		(* INPUT  *) dy       : Ctypes.int;
		(* INPUT  *) icolours : pxlIndexedColourStar);

<*EXTERNAL*> PROCEDURE PEXCellArray2D (
		(* IN-OUT *) ocbuf    : pxlOCBufStar;
		(* INPUT  *) pt1      : pxlCoord2DStar;
		(* INPUT  *) pt2      : pxlCoord2DStar;
		(* INPUT  *) dx       : Ctypes.int;
		(* INPUT  *) dy       : Ctypes.int;
		(* INPUT  *) icolours : pxlIndexedColourStar);

<*EXTERNAL*> PROCEDURE PEXExtendedCellArray (
		(* IN-OUT *) ocbuf   : pxlOCBufStar;
		(* INPUT  *) pt1     : pxlCoord3DStar;
		(* INPUT  *) pt2     : pxlCoord3DStar;
		(* INPUT  *) pt3     : pxlCoord3DStar;
		(* INPUT  *) dx      : Ctypes.int;
		(* INPUT  *) dy      : Ctypes.int;
		(* INPUT  *) colours : Ctypes.char_star);

<*EXTERNAL*> PROCEDURE PEXGdp (
		(* IN-OUT *) ocbuf     : pxlOCBufStar;
		(* INPUT  *) id        : Ctypes.int;
		(* INPUT  *) points    : pxlCoord3DStar;
		(* INPUT  *) numPoints : Ctypes.int;
		(* INPUT  *) data      : Ctypes.char_star;
		(* INPUT  *) numBytes  : Ctypes.int);

<*EXTERNAL*> PROCEDURE PEXGdp2D (
		(* IN-OUT *) ocbuf     : pxlOCBufStar;
		(* INPUT  *) id        : Ctypes.int;
		(* INPUT  *) points    : pxlCoord2DStar;
		(* INPUT  *) numPoints : Ctypes.int;
		(* INPUT  *) data      : Ctypes.char_star;
		(* INPUT  *) numBytes  : Ctypes.int);
(*
 * Routines defined in pl_oc_phigs.c
 *)

<*EXTERNAL*> PROCEDURE PEXPolylineSetUnpacked  (
 		(* IN-OUT *) ocbuf            : pxlOCBufStar;
 		(* INPUT  *) vertexAttributes : Ctypes.unsigned_long;
 		(* INPUT  *) colourType       : Ctypes.int;
 		(* INPUT  *) points           : pxlCoord3DStar;
 		(* INPUT  *) colours          : Ctypes.int_star;
 		(* INPUT  *) numPolylines     : Ctypes.int;
 		(* INPUT  *) sizes            : Ctypes.int_star);
 
<*EXTERNAL*> PROCEDURE PEXEncodedTextUnpacked (
 		(* IN-OUT *) ocbuf             : pxlOCBufStar;
 		(* INPUT  *) origin            : pxlCoord3DStar;
 		(* INPUT  *) vec1              : pxlVector3DStar;
 		(* INPUT  *) vec2              : pxlVector3DStar;
 		(* INPUT  *) numCharStrings    : Ctypes.int;
 		(* INPUT  *) charSetList       : Ctypes.int_star;
 		(* INPUT  *) charWidthList     : Ctypes.int_star;
 		(* INPUT  *) encodingStateList : Ctypes.int_star;
 		(* INPUT  *) charStringsList   : pxlStringDataStar;
 		(* INPUT  *) highByteFirst     : Ctypes.int);
 
<*EXTERNAL*> PROCEDURE PEXEncodedText2DUnpacked (
		(* IN-OUT *) ocbuf             : pxlOCBufStar;
		(* INPUT  *) origin            : pxlCoord2DStar;
		(* INPUT  *) numCharStrings    : Ctypes.int;
		(* INPUT  *) charSetList       : Ctypes.int_star;
		(* INPUT  *) charWidthList     : Ctypes.int_star;
		(* INPUT  *) encodingStateList : Ctypes.int_star;
		(* INPUT  *) charStringsList   : pxlStringDataStar;
		(* INPUT  *) highByteFirst     : Ctypes.int);

<*EXTERNAL*> PROCEDURE PEXEncodedAnnoTextUnpacked (
		(* IN-OUT *) ocbuf             : pxlOCBufStar;
		(* INPUT  *) origin            : pxlCoord3DStar;
		(* INPUT  *) offset            : pxlCoord3DStar;
		(* INPUT  *) numCharStrings    : Ctypes.int;
		(* INPUT  *) charSetList       : Ctypes.int_star;
		(* INPUT  *) charWidthList     : Ctypes.int_star;
		(* INPUT  *) encodingStateList : Ctypes.int_star;
		(* INPUT  *) charStringsList   : pxlStringDataStar;
		(* INPUT  *) highByteFirst     : Ctypes.int);

<*EXTERNAL*> PROCEDURE PEXEncodedAnnoText2DUnpacked (
		(* IN-OUT *) ocbuf             : pxlOCBufStar;
		(* INPUT  *) origin            : pxlCoord2DStar;
		(* INPUT  *) offset            : pxlCoord2DStar;
		(* INPUT  *) numCharStrings    : Ctypes.int;
		(* INPUT  *) charSetList       : Ctypes.int_star;
		(* INPUT  *) charWidthList     : Ctypes.int_star;
		(* INPUT  *) encodingStateList : Ctypes.int_star;
		(* INPUT  *) charStringsList   : pxlStringDataStar;
		(* INPUT  *) highByteFirst     : Ctypes.int);

<*EXTERNAL*> PROCEDURE PEXFillAreaDataUnpacked (
		(* INPUT  *) ocbuf            : pxlOCBufStar;
		(* INPUT  *) shape            : Ctypes.int;
		(* INPUT  *) ignoreEdges      : Ctypes.int;
		(* INPUT  *) facetAttributes  : Ctypes.unsigned_long;
		(* INPUT  *) vertexAttributes : Ctypes.unsigned_long;
		(* INPUT  *) colourType       : Ctypes.int;
		(* INPUT  *) facetColour      : Ctypes.int_star;
		(* INPUT  *) facetNormal      : pxlVector3DStar;
		(* INPUT  *) points           : pxlCoord3DStar;
		(* INPUT  *) colours          : Ctypes.int_star;
		(* INPUT  *) normals          : pxlVector3DStar;
		(* INPUT  *) numPoints        : Ctypes.int);

<*EXTERNAL*> PROCEDURE PEXFillAreaSetUnpacked (
		(* INPUT  *) ocbuf            : pxlOCBufStar;
		(* INPUT  *) shape            : Ctypes.int;
		(* INPUT  *) ignoreEdges      : Ctypes.int;
		(* INPUT  *) contourHint      : Ctypes.int;
		(* INPUT  *) points           : pxlCoord3DStar;
		(* INPUT  *) numContours      : Ctypes.int;
		(* INPUT  *) sizes            : Ctypes.int_star);

<*EXTERNAL*> PROCEDURE PEXFillAreaSet2DUnpacked (
		(* INPUT  *) ocbuf            : pxlOCBufStar;
		(* INPUT  *) shape            : Ctypes.int;
		(* INPUT  *) ignoreEdges      : Ctypes.int;
		(* INPUT  *) contourHint      : Ctypes.int;
		(* INPUT  *) points           : pxlCoord2DStar;
		(* INPUT  *) numContours      : Ctypes.int;
		(* INPUT  *) sizes            : Ctypes.int_star);

<*EXTERNAL*> PROCEDURE PEXFillAreaSetWithDataUnpacked (
		(* INPUT  *) ocbuf            : pxlOCBufStar;
		(* INPUT  *) shape            : Ctypes.int;
		(* INPUT  *) ignoreEdges      : Ctypes.int;
		(* INPUT  *) contourHint      : Ctypes.int;
		(* INPUT  *) facetAttributes  : Ctypes.unsigned_long;
		(* INPUT  *) vertexAttributes : Ctypes.unsigned_long;
		(* INPUT  *) colourType       : Ctypes.int;
		(* INPUT  *) facetColour      : Ctypes.int_star;
		(* INPUT  *) facetNormal      : pxlVector3DStar;
		(* INPUT  *) points           : pxlCoord3DStar;
		(* INPUT  *) colours          : Ctypes.int_star;
		(* INPUT  *) normals          : pxlVector3DStar;
		(* INPUT  *) edges            : Ctypes.int_star;
		(* INPUT  *) numContours      : Ctypes.int;
		(* INPUT  *) sizes            : Ctypes.int_star);

<*EXTERNAL*> PROCEDURE PEXTriangleStripUnpacked (
		(* INPUT  *) ocbuf            : pxlOCBufStar;
		(* INPUT  *) facetAttributes  : Ctypes.unsigned_long;
		(* INPUT  *) vertexAttributes : Ctypes.unsigned_long;
		(* INPUT  *) colourType       : Ctypes.int;
		(* INPUT  *) facetColours     : Ctypes.int_star;
		(* INPUT  *) facetNormals     : pxlVector3DStar;
		(* INPUT  *) points           : pxlCoord3DStar;
		(* INPUT  *) colours          : Ctypes.int_star;
		(* INPUT  *) normals          : pxlVector3DStar;
		(* INPUT  *) numPoints        : Ctypes.int);

<*EXTERNAL*> PROCEDURE PEXQuadMeshUnpacked (
		(* INPUT  *) ocbuf            : pxlOCBufStar;
		(* INPUT  *) shape            : Ctypes.int;
		(* INPUT  *) facetAttributes  : Ctypes.unsigned_long;
		(* INPUT  *) vertexAttributes : Ctypes.unsigned_long;
		(* INPUT  *) colourType       : Ctypes.int;
		(* INPUT  *) facetColours     : Ctypes.int_star;
		(* INPUT  *) facetNormals     : pxlVector3DStar;
		(* INPUT  *) points           : pxlCoord3DStar;
		(* INPUT  *) colours          : Ctypes.int_star;
		(* INPUT  *) normals          : pxlVector3DStar;
		(* INPUT  *) numPointsM       : Ctypes.int;
		(* INPUT  *) numPointsN       : Ctypes.int);

<*EXTERNAL*> PROCEDURE PEXIndexedPolygonsToSOFAS (
		(* INPUT  *) ocbuf            : pxlOCBufStar;
		(* INPUT  *) shape            : Ctypes.int;
		(* INPUT  *) facetAttributes  : Ctypes.unsigned_long;
		(* INPUT  *) edgeAttributes   : Ctypes.unsigned_long;
		(* INPUT  *) vertexAttributes : Ctypes.unsigned_long;
		(* INPUT  *) colourType       : Ctypes.int;
		(* INPUT  *) facetColours     : Ctypes.int_star;
		(* INPUT  *) facetNormals     : pxlVector3DStar;
		(* INPUT  *) numFacets        : Ctypes.int;
		(* INPUT  *) edges            : Ctypes.int_star;
		(* INPUT  *) edgeVisFlags     : Ctypes.int_star;
		(* INPUT  *) points           : pxlCoord3DStar;
		(* INPUT  *) colours          : Ctypes.int_star;
		(* INPUT  *) normals          : pxlVector3DStar;
		(* INPUT  *) numVertices      : Ctypes.int;
		(* INPUT  *) facetCounts      : Ctypes.int_star);

<*EXTERNAL*> PROCEDURE MPEXIndexedPolygonsUnpacked (
		(* INPUT  *) ocbuf            : pxlOCBufStar;
		(* INPUT  *) shape            : Ctypes.int;
		(* INPUT  *) facetAttributes  : Ctypes.unsigned_long;
		(* INPUT  *) edgeAttributes   : Ctypes.unsigned_long;
		(* INPUT  *) vertexAttributes : Ctypes.unsigned_long;
		(* INPUT  *) colourType       : Ctypes.int;
		(* INPUT  *) facetColours     : Ctypes.int_star;
		(* INPUT  *) facetNormals     : pxlVector3DStar;
		(* INPUT  *) numFacets        : Ctypes.int;
		(* INPUT  *) edges            : Ctypes.int_star;
		(* INPUT  *) edgeVisFlags     : Ctypes.int_star;
		(* INPUT  *) points           : pxlCoord3DStar;
		(* INPUT  *) colours          : Ctypes.int_star;
		(* INPUT  *) normals          : pxlVector3DStar;
		(* INPUT  *) numVertices      : Ctypes.int;
		(* INPUT  *) facetCounts      : Ctypes.int_star);

<*EXTERNAL*> PROCEDURE PEXNurbSurfaceUnpacked (
		(* INPUT  *) ocbuf        : pxlOCBufStar;
		(* INPUT  *) uOrder       : Ctypes.int;
		(* INPUT  *) vOrder       : Ctypes.int;
		(* INPUT  *) uKnots       : Ctypes.float_star;
		(* INPUT  *) numUKnots    : Ctypes.int;
		(* INPUT  *) vKnots       : Ctypes.float_star;
		(* INPUT  *) numVKnots    : Ctypes.int;
		(* INPUT  *) surfaceType  : Ctypes.int;
		(* INPUT  *) points       : Ctypes.char_star;
		(* INPUT  *) numMPoints   : Ctypes.int;
		(* INPUT  *) numNPoints   : Ctypes.int;
		(* INPUT  *) trimList     : pxlPhigsTrimListStar;
		(* INPUT  *) numTrimLoops : Ctypes.int);
(* 
 * Routines declared in pl_font.c
 *)

<*EXTERNAL*> PROCEDURE PEXOpenFont  (
		(* INPUT  *) display  : X.DisplayStar;
		(* INPUT  *) fontname : Ctypes.char_star) : pxlFont;

<*EXTERNAL*> PROCEDURE PEXCloseFont  (
		(* INPUT  *) display : X.DisplayStar;
		(* INPUT  *) font    : pxlFont);

<*EXTERNAL*> PROCEDURE PEXQueryFont  (
		(* INPUT  *) display        : X.DisplayStar;
		(* INPUT  *) font           : pxlFont;
		(* OUTPUT *) fontInfoReturn : pxlFontInfoStarStar) : X.Status;

<*EXTERNAL*> PROCEDURE PEXListFonts  (
		(* INPUT  *) display     : X.DisplayStar;
		(* INPUT  *) pattern     : Ctypes.char_star;
		(* INPUT  *) maxNames    : Ctypes.int;
		(* OUTPUT *) countReturn : Ctypes.int_star;
		(* OUTPUT *) namesReturn : pxlStringDataStarStar) : X.Status;

<*EXTERNAL*> PROCEDURE PEXListFontsWithInfo  (
		(* INPUT  *) display           : X.DisplayStar;
		(* INPUT  *) pattern           : Ctypes.char_star;
		(* INPUT  *) maxNames          : Ctypes.int;
		(* OUTPUT *) numStringsReturn  : Ctypes.int_star;
		(* OUTPUT *) numFontInfoReturn : Ctypes.int_star;
		(* OUTPUT *) namesReturn       : pxlStringDataStarStar;
		(* OUTPUT *) infoReturn        : pxlFontInfoStarStar) : X.Status;

<*EXTERNAL*> PROCEDURE PEXQueryTextExtents  (
		(* INPUT  *) display          : X.DisplayStar;
		(* INPUT  *) id               : X.XID;
		(* INPUT  *) fontGroup        : Ctypes.int;
		(* INPUT  *) path             : Ctypes.int;
		(* INPUT  *) expansion        : Ctypes.double;
		(* INPUT  *) spacing          : Ctypes.double;
		(* INPUT  *) height           : Ctypes.double;
		(* INPUT  *) halign           : Ctypes.int;
		(* INPUT  *) valign           : Ctypes.int;
		(* INPUT  *) string           : Ctypes.char_star;
		(* INPUT  *) count            : Ctypes.int;
		(* OUTPUT *) lowerLeftReturn  : pxlCoord2DStar;
		(* OUTPUT *) upperRightReturn : pxlCoord2DStar;
		(* OUTPUT *) concatPtReturn   : pxlCoord2DStar) : X.Status;

(* 
 * Routines defined in pl_free.c
 *)

<*EXTERNAL*> PROCEDURE PEXFree (
		(* INPUT  *) pch : Ctypes.char_star);

(* 
 * Routines defined in pl_lut.c
 *)

<*EXTERNAL*> PROCEDURE PEXCreateLookupTable (
		(* INPUT  *) display : X.DisplayStar;
		(* INPUT  *) d       : X.Drawable;
		(* INPUT  *) type    : Ctypes.int) 
	     : pxlLookupTable;

<*EXTERNAL*> PROCEDURE PEXFreeLookupTable (
		(* INPUT  *) display : X.DisplayStar;
		(* INPUT  *) lut     : pxlLookupTable);

<*EXTERNAL*> PROCEDURE PEXCopyLookupTable (
		(* INPUT  *) display : X.DisplayStar;
		(* INPUT  *) srcLut  : pxlLookupTable;
		(* INPUT  *) destLut : pxlLookupTable);

<*EXTERNAL*> PROCEDURE PEXGetTableInfo  (
		(* INPUT  *) display         : X.DisplayStar;
		(* INPUT  *) d               : X.Drawable;
		(* INPUT  *) type            : Ctypes.int;
		(* OUTPUT *) tableInfoReturn : pxlTableInfoStarStar) : X.Status;

<*EXTERNAL*> PROCEDURE PEXGetPredefinedEntries  (
		(* INPUT  *) display          : X.DisplayStar;
		(* INPUT  *) d                : X.Drawable;
		(* INPUT  *) type             : Ctypes.int;
		(* INPUT  *) start            : Ctypes.int;
		(* INPUT  *) count            : Ctypes.int;
		(* OUTPUT *) entriesReturn    : Ctypes.char_star_star;
		(* OUTPUT *) numEntriesReturn : Ctypes.int_star) : X.Status;

<*EXTERNAL*> PROCEDURE PEXGetDefinedIndices  (
		(* INPUT  *) display          : X.DisplayStar;
		(* INPUT  *) lut              : pxlLookupTable;
		(* OUTPUT *) indicesReturn    : UNTRACED REF Ctypes.unsigned_short_star;
		(* OUTPUT *) numIndicesReturn : Ctypes.int_star) : X.Status;

<*EXTERNAL*> PROCEDURE PEXGetTableEntry  (
		(* INPUT  *) display      : X.DisplayStar;
		(* INPUT  *) lut          : pxlLookupTable;
		(* INPUT  *) index        : Ctypes.int;
		(* INPUT  *) valueType    : Ctypes.int;
		(* OUTPUT *) statusReturn : Ctypes.int_star;
		(* OUTPUT *) entryReturn  : Ctypes.char_star_star) : X.Status;

<*EXTERNAL*> PROCEDURE PEXGetTableEntries  (
		(* INPUT  *) display       : X.DisplayStar;
		(* INPUT  *) lut           : pxlLookupTable;
		(* INPUT  *) start         : Ctypes.int;
		(* INPUT  *) count         : Ctypes.int;
		(* INPUT  *) valueType     : Ctypes.int;
		(* OUTPUT *) entriesReturn : Ctypes.char_star_star) : X.Status;

<*EXTERNAL*> PROCEDURE PEXSetTableEntries (
		(* INPUT  *) display : X.DisplayStar;
		(* INPUT  *) lut     : pxlLookupTable;
		(* INPUT  *) type    : Ctypes.int;
		(* INPUT  *) start   : Ctypes.int;
		(* INPUT  *) count   : Ctypes.int;
		(* INPUT  *) entries : Ctypes.char_star);

<*EXTERNAL*> PROCEDURE PEXDeleteTableEntries  (
		(* INPUT  *) display : X.DisplayStar;
		(* INPUT  *) lut     : pxlLookupTable;
		(* INPUT  *) start   : Ctypes.int;
		(* INPUT  *) count   : Ctypes.int);

(* 
 * Routines defined in pl_nameset.c
 *)

<*EXTERNAL*> PROCEDURE PEXCreateNameSet  (
		(* INPUT  *) display : X.DisplayStar) : pxlNameSet;

<*EXTERNAL*> PROCEDURE PEXFreeNameSet  (
		(* INPUT  *) display : X.DisplayStar;
		(* INPUT  *) ns      : pxlNameSet);

<*EXTERNAL*> PROCEDURE PEXCopyNameSet  (
		(* INPUT  *) display : X.DisplayStar;
		(* INPUT  *) srcNs   : pxlNameSet;
		(* INPUT  *) destNs  : pxlNameSet);

<*EXTERNAL*> PROCEDURE PEXGetNameSet  (
		(* INPUT  *) display        : X.DisplayStar;
		(* INPUT  *) ns             : pxlNameSet;
		(* OUTPUT *) namesReturn    : pxlNameStarStar;
		(* OUTPUT *) numNamesReturn : Ctypes.int_star) : X.Status;

<*EXTERNAL*> PROCEDURE PEXChangeNameSet  (
		(* INPUT  *) display   : X.DisplayStar;
		(* INPUT  *) ns        : pxlNameSet;
		(* INPUT  *) action    : Ctypes.int;
		(* INPUT  *) values    : pxlNameStar;
		(* INPUT  *) numValues : Ctypes.int);

(* 
 * Routines defined in pl_oc_struct.c
 *)

<*EXTERNAL*> PROCEDURE PEXExecuteStructure (
		(* IN-OUT *) ocbuf     : pxlOCBufStar;
		(* INPUT  *) structure : pxlStructure);

<*EXTERNAL*> PROCEDURE PEXLabel  (
	(* IN-OUT *) ocbuf : pxlOCBufStar;
	(* INPUT  *) label : Ctypes.long);

<*EXTERNAL*> PROCEDURE PEXApplicationData  (
	(* IN-OUT *) ocbuf    : pxlOCBufStar;
	(* INPUT  *) data     : Ctypes.char_star;
	(* INPUT  *) numBytes : Ctypes.int);

<*EXTERNAL*> PROCEDURE PEXGse  (
	(* IN-OUT *) ocbuf    : pxlOCBufStar;
	(* INPUT  *) id       : Ctypes.unsigned_long;
	(* INPUT  *) data     : Ctypes.char_star;
	(* INPUT  *) numBytes : Ctypes.int);

(* 
 * Routines defined in pl_pc.c
 *)

<*EXTERNAL*> PROCEDURE PEXCreatePipelineContext  (
		(* INPUT  *) display   : X.DisplayStar;
		(* INPUT  *) valueMask : Ctypes.unsigned_long_star;
                             (* C version : INPUT unsigned long valueMask[2] *)
		(* INPUT  *) values    : pxlPCAttributesStar)
	     : pxlPipelineContext;

<*EXTERNAL*> PROCEDURE PEXFreePipelineContext (
		(* INPUT  *) display : X.DisplayStar;
		(* INPUT  *) pc      : pxlPipelineContext);

<*EXTERNAL*> PROCEDURE PEXCopyPipelineContext  (
		(* INPUT  *) display      : X.DisplayStar;
		(* INPUT  *) valueMask    : Ctypes.unsigned_long_star;
                             (* C version : INPUT unsigned long valueMask[2] *)
		(* INPUT  *) srcPc        : pxlPipelineContext;
		(* INPUT  *) destPc       : pxlPipelineContext);

<*EXTERNAL*> PROCEDURE PEXGetPipelineContext (
		(* INPUT  *) display            : X.DisplayStar;
		(* INPUT  *) pc                 : pxlPipelineContext;
		(* INPUT  *) valueMask          : Ctypes.unsigned_long_star;
    	                     (* version : INPUT unsigned long valueMask[2] *)
		(* OUTPUT *) pcAttributesReturn : pxlPCAttributesStarStar) 
             : X.Status;

<*EXTERNAL*> PROCEDURE PEXChangePipelineContext (
		(* INPUT  *) display      : X.DisplayStar;
		(* INPUT  *) pc           : pxlPipelineContext;
		(* INPUT  *) valueMask    : Ctypes.unsigned_long_star;
                             (* C version : INPUT unsigned long valueMask[2] *)
		(* INPUT  *) pcAttributes : pxlPCAttributesStar);

(* 
 * Routines defined in pl_rdr.c
 *)

<*EXTERNAL*> PROCEDURE PEXCreateRenderer (
		(* INPUT  *) display : X.DisplayStar;
		(* INPUT  *) d : X.Drawable;
		(* INPUT  *) valueMask : Ctypes.unsigned_long;
		(* INPUT  *) values : pxlRendererAttributesStar) 
	     : pxlRenderer;

<*EXTERNAL*> PROCEDURE PEXFreeRenderer (
		(* INPUT  *) display  : X.DisplayStar;
		(* INPUT  *) renderer : pxlRenderer);

<*EXTERNAL*> PROCEDURE PEXGetRendererAttributes (
		(* INPUT  *) display      : X.DisplayStar;
		(* INPUT  *) renderer     : pxlRenderer;
		(* INPUT  *) valueMask    : Ctypes.unsigned_long;
		(* OUTPUT *) valuesReturn : pxlRendererAttributesStarStar) 
             : X.Status;

<*EXTERNAL*> PROCEDURE PEXGetRendererDynamics (
		(* INPUT  *) display          : X.DisplayStar;
		(* INPUT  *) renderer         : pxlRenderer;
		(* OUTPUT *) tablesReturn     : Ctypes.unsigned_long_star;
		(* OUTPUT *) namesetsReturn   : Ctypes.unsigned_long_star;
		(* OUTPUT *) attributesReturn : Ctypes.unsigned_long_star) 
             : X.Status;

<*EXTERNAL*> PROCEDURE PEXChangeRenderer (
		(* INPUT  *) display   : X.DisplayStar;
		(* INPUT  *) renderer  : pxlRenderer;
		(* INPUT  *) valueMask : Ctypes.unsigned_long;
		(* INPUT  *) values    : pxlRendererAttributesStar);

<*EXTERNAL*> PROCEDURE PEXBeginRendering (
		(* INPUT  *) display  : X.DisplayStar;
		(* INPUT  *) d        : X.Drawable;
		(* INPUT  *) renderer : pxlRenderer);

<*EXTERNAL*> PROCEDURE PEXEndRendering (
		(* INPUT  *) display  : X.DisplayStar;
		(* INPUT  *) renderer : pxlRenderer;
		(* INPUT  *) flush    : X.Bool);

<*EXTERNAL*> PROCEDURE PEXBeginStructure (
		(* INPUT  *) display  : X.DisplayStar;
		(* INPUT  *) renderer : pxlRenderer;
		(* INPUT  *) id       : Ctypes.unsigned_long);

<*EXTERNAL*> PROCEDURE PEXEndStructure (
		(* INPUT  *) display  : X.DisplayStar;
		(* INPUT  *) renderer : pxlRenderer);

<*EXTERNAL*> PROCEDURE PEXRenderNetwork (
		(* INPUT  *) display   : X.DisplayStar;
		(* INPUT  *) d         : X.Drawable;
		(* INPUT  *) renderer  : pxlRenderer;
		(* INPUT  *) structure : pxlStructure);

<*EXTERNAL*> PROCEDURE MPEXBeginTransparencyRendering (
		(* INPUT  *) display  : X.DisplayStar;
		(* INPUT  *) d        : X.Drawable;
		(* INPUT  *) renderer : pxlRenderer;
		(* INPUT  *) clear    : Ctypes.int);

<*EXTERNAL*> PROCEDURE MPEXEndTransparencyRendering (
		(* INPUT  *) display  : X.DisplayStar;
		(* INPUT  *) renderer : pxlRenderer;
		(* INPUT  *) flush    : X.Bool) : Ctypes.int;

(* 
 * Routines defined in pl_startup.c
 *)

<*EXTERNAL*> PROCEDURE PEXInitialize (
		(* INPUT  *) display : X.DisplayStar;
		(* OUTPUT *) pexinfo : pxlInfoStarStar) : X.Status;

<*EXTERNAL*> PROCEDURE PEXTerminate (
		(* INPUT  *) display : X.DisplayStar;
		(* INPUT  *) codes   : X.XExtCodesStar) : X.Status;

<*EXTERNAL*> PROCEDURE PEXGetEnumTypeInfo  (
		(* INPUT  *) display        : X.DisplayStar;
		(* INPUT  *) drawable       : X.Drawable;
		(* INPUT  *) enumType       : Ctypes.int;
		(* INPUT  *) returnMask     : Ctypes.unsigned_long;
		(* OUTPUT *) enumInfoReturn : pxlEnumTypeDescListStarStar;
		(* OUTPUT *) countReturn    : Ctypes.int_star) : X.Status;

<*EXTERNAL*> PROCEDURE PEXSetFloatingPointFormat  (
		(* INPUT  *) fpFormat : Ctypes.int);

<*EXTERNAL*> PROCEDURE PEXGetFloatingPointFormat () : Ctypes.int;

<*EXTERNAL*> PROCEDURE PEXSetDirectColourFormat  (
		(* INPUT  *) colourFormat : Ctypes.int);

<*EXTERNAL*> PROCEDURE PEXGetDirectColourFormat () : Ctypes.int;

<*EXTERNAL*> PROCEDURE PEXSetColourType (
		(* INPUT  *) colourType : Ctypes.int);

<*EXTERNAL*> PROCEDURE PEXGetColourType () : Ctypes.int;

<*EXTERNAL*> PROCEDURE PEXGetImpDepConstants  (
	(* INPUT  *) display      : X.DisplayStar;
	(* INPUT  *) drawable     : X.Drawable;
	(* INPUT  *) names        : Ctypes.short_star;
	(* INPUT  *) count        : Ctypes.int;
	(* OUTPUT *) valuesReturn : Ctypes.unsigned_long_star) : X.Status;

(* 
 * Routines defined in pl_struct.c
 *)

<*EXTERNAL*> PROCEDURE PEXCreateStructure (
		(* INPUT  *) display : X.DisplayStar) : pxlStructure;

<*EXTERNAL*> PROCEDURE PEXDestroyStructures  (
		(* INPUT  *) display       : X.DisplayStar;
		(* INPUT  *) structures    : pxlStructureStar;
		(* INPUT  *) numStructures : Ctypes.int);

<*EXTERNAL*> PROCEDURE PEXCopyStructure  (
		(* INPUT  *) display       : X.DisplayStar;
		(* INPUT  *) srcStructure  : pxlStructure;
		(* INPUT  *) destStructure : pxlStructure);

<*EXTERNAL*> PROCEDURE PEXGetStructureInfo  (
		(* INPUT  *) display             : X.DisplayStar;
		(* INPUT  *) structure           : pxlStructure;
		(* OUTPUT *) structureInfoReturn : pxlStructureInformationStarStar
		) : X.Status;

<*EXTERNAL*> PROCEDURE PEXGetElementInfo  (
		(* INPUT  *) display              : X.DisplayStar;
		(* INPUT  *) structure            : pxlStructure;
		(* INPUT  *) whence1              : Ctypes.int;
		(* INPUT  *) offset1              : Ctypes.long;
		(* INPUT  *) whence2              : Ctypes.int;
		(* INPUT  *) offset2              : Ctypes.long;
		(* OUTPUT *) elementInfoReturn    : pxlElementInfoStarStar;
		(* OUTPUT *) numElementInfoReturn : Ctypes.int_star) : X.Status;

<*EXTERNAL*> PROCEDURE PEXGetStructuresInNetwork  (
		(* INPUT  *) display             : X.DisplayStar;
		(* INPUT  *) structure           : pxlStructure;
		(* INPUT  *) which               : Ctypes.int;
		(* OUTPUT *) structuresReturn    : pxlStructureStarStar;
		(* OUTPUT *) numStructuresReturn : Ctypes.int_star) : X.Status;

<*EXTERNAL*> PROCEDURE PEXGetAncestors  (
		(* INPUT  *) display        : X.DisplayStar;
		(* INPUT  *) structure      : pxlStructure;
		(* INPUT  *) pathOrder      : Ctypes.int;
		(* INPUT  *) pathDepth      : Ctypes.int;
		(* OUTPUT *) pathsReturn    : pxlStructurePathStarStar;
		(* OUTPUT *) numPathsReturn : Ctypes.int_star) : X.Status;

<*EXTERNAL*> PROCEDURE PEXGetDescendants  (
		(* INPUT  *) display        : X.DisplayStar;
		(* INPUT  *) structure      : pxlStructure;
		(* INPUT  *) pathOrder      : Ctypes.int;
		(* INPUT  *) pathDepth      : Ctypes.int;
		(* OUTPUT *) pathsReturn    : pxlStructurePathStarStar;
		(* OUTPUT *) numPathsReturn : Ctypes.int_star) : X.Status;

<*EXTERNAL*> PROCEDURE PEXFetchElements  (
		(* INPUT  *) display     : X.DisplayStar;
		(* INPUT  *) structure   : pxlStructure;
		(* INPUT  *) whence1     : Ctypes.int;
		(* INPUT  *) offset1     : Ctypes.long;
		(* INPUT  *) whence2     : Ctypes.int;
		(* INPUT  *) offset2     : Ctypes.long;
		(* OUTPUT *) ocbufReturn : pxlOCBufStarStar) : X.Status;

<*EXTERNAL*> PROCEDURE PEXSetEditingMode (
		(* INPUT  *) display   : X.DisplayStar;
		(* INPUT  *) structure : pxlStructure;
		(* INPUT  *) mode      : Ctypes.int);

<*EXTERNAL*> PROCEDURE PEXSetElementPtr (
		(* INPUT  *) display   : X.DisplayStar;
		(* INPUT  *) structure : pxlStructure;
		(* INPUT  *) whence    : Ctypes.int;
		(* INPUT  *) offset    : Ctypes.long);

<*EXTERNAL*> PROCEDURE PEXSetElementPtrAtLabel  (
		(* INPUT  *) display   : X.DisplayStar;
		(* INPUT  *) structure : pxlStructure;
		(* INPUT  *) label     : Ctypes.long;
		(* INPUT  *) offset    : Ctypes.long);

<*EXTERNAL*> PROCEDURE PEXElementSearch  (
		(* INPUT  *) display      : X.DisplayStar;
		(* INPUT  *) structure    : pxlStructure;
		(* INPUT  *) whence       : Ctypes.int;
		(* INPUT  *) offset       : Ctypes.long;
		(* INPUT  *) direction    : Ctypes.int;
		(* INPUT  *) inclList     : Ctypes.short_star;
		(* INPUT  *) numIncl      : Ctypes.int;
		(* INPUT  *) exclList     : Ctypes.short_star;
		(* INPUT  *) numExcl      : Ctypes.int;
		(* OUTPUT *) statusReturn : Ctypes.int_star;
		(* OUTPUT *) offsetReturn : Ctypes.unsigned_long_star) : X.Status;

<*EXTERNAL*> PROCEDURE PEXDeleteElements (
		(* INPUT  *) display   : X.DisplayStar;
		(* INPUT  *) structure : pxlStructure;
		(* INPUT  *) whence1   : Ctypes.int;
		(* INPUT  *) offset1   : Ctypes.long;
		(* INPUT  *) whence2   : Ctypes.int;
		(* INPUT  *) offset2   : Ctypes.long);

<*EXTERNAL*> PROCEDURE PEXDeleteToLabel  (
		(* INPUT  *) display   : X.DisplayStar;
		(* INPUT  *) structure : pxlStructure;
		(* INPUT  *) whence    : Ctypes.int;
		(* INPUT  *) offset    : Ctypes.long;
		(* INPUT  *) label     : Ctypes.long);

<*EXTERNAL*> PROCEDURE PEXDeleteBetweenLabels  (
		(* INPUT  *) display   : X.DisplayStar;
		(* INPUT  *) structure : pxlStructure;
		(* INPUT  *) label1    : Ctypes.long;
		(* INPUT  *) label2    : Ctypes.long);

<*EXTERNAL*> PROCEDURE PEXCopyElements  (
		(* INPUT  *) display       : X.DisplayStar;
		(* INPUT  *) srcStructure  : pxlStructure;
		(* INPUT  *) srcWhence1    : Ctypes.int;
		(* INPUT  *) srcOffset1    : Ctypes.long;
		(* INPUT  *) srcWhence2    : Ctypes.int;
		(* INPUT  *) srcOffset2    : Ctypes.long;
		(* INPUT  *) destStructure : pxlStructure;
		(* INPUT  *) destWhence    : Ctypes.int;
		(* INPUT  *) destOffset    : Ctypes.long);

<*EXTERNAL*> PROCEDURE PEXChangeStructureRefs  (
		(* INPUT  *) display      : X.DisplayStar;
		(* INPUT  *) oldStructure : pxlStructure;
		(* INPUT  *) newStructure : pxlStructure);

(* 
 * Routines defined in pl_utl.c
 *)

<*EXTERNAL*> PROCEDURE PEXRotationMatrix (
		(* INPUT  *) axis         : Ctypes.int;
		(* INPUT  *) angle        : Ctypes.double;
		(* OUTPUT *) matrixReturn : pxlMatrixStar) 
                (* was pxlMatrix (C vs Modula-3 Array passing problem) *)
	     : X.Status;

<*EXTERNAL*> PROCEDURE PEXArbRotationMatrix (
		(* INPUT  *) pt1          : pxlCoord3DStar;
		(* INPUT  *) pt2          : pxlCoord3DStar;
		(* INPUT  *) angle        : Ctypes.double;
		(* OUTPUT *) matrixReturn : pxlMatrixStar);
                (* was pxlMatrix (C vs Modula-3 Array passing problem) *)

<*EXTERNAL*> PROCEDURE PEXScalingMatrix (
		(* INPUT  *) sx           : Ctypes.double;
		(* INPUT  *) sy           : Ctypes.double;
		(* INPUT  *) sz           : Ctypes.double;
		(* OUTPUT *) matrixReturn : pxlMatrixStar);
                (* was pxlMatrix (C vs Modula-3 Array passing problem) *)

<*EXTERNAL*> PROCEDURE PEXTranslationMatrix (
		(* INPUT  *) tx           : Ctypes.double;
		(* INPUT  *) ty           : Ctypes.double;
		(* INPUT  *) tz           : Ctypes.double;
		(* OUTPUT *) matrixReturn : pxlMatrixStar);
                (* was pxlMatrix (C vs Modula-3 Array passing problem) *)

<*EXTERNAL*> PROCEDURE PEXCopyMatrix (
		(* INPUT  *) matrix       : pxlMatrixStar;
		(* OUTPUT *) matrixReturn : pxlMatrixStar);
                (* was pxlMatrix (C vs Modula-3 Array passing problem) *)

<*EXTERNAL*> PROCEDURE PEXMultiplyMatrices (
		(* INPUT  *) mat1         : pxlMatrixStar;
		(* INPUT  *) mat2         : pxlMatrixStar;
		(* OUTPUT *) matrixReturn : pxlMatrixStar);
                (* was pxlMatrix (C vs Modula-3 Array passing problem) *)

<*EXTERNAL*> PROCEDURE PEXIdentityMatrix (
		(* OUTPUT *) matrixReturn : pxlMatrixStar);
                (* was pxlMatrix (C vs Modula-3 Array passing problem) *)

<*EXTERNAL*> PROCEDURE PEXInvertMatrix (
		(* INPUT  *) matrix        : pxlMatrixStar;
		(* OUTPUT *) inverseReturn : pxlMatrixStar) 
                (* was pxlMatrix (C vs Modula-3 Array passing problem) *)
             : X.Status;

<*EXTERNAL*> PROCEDURE PEXTransform3dPoints (
		(* INPUT  *) mat       : pxlMatrixStar;
                (* was pxlMatrix (C vs Modula-3 Array passing problem) *)
		(* INPUT  *) pts       : pxlCoord3DStar;
		(* INPUT  *) numPts    : Ctypes.int;
		(* OUTPUT *) ptsReturn : pxlCoord3DStar) 
             : X.Status;

<*EXTERNAL*> PROCEDURE PEXTransform4dPoints (
		(* INPUT  *) mat       : pxlMatrixStar;
                (* was pxlMatrix (C vs Modula-3 Array passing problem) *)
		(* INPUT  *) pts       : pxlCoord4DStar;
		(* INPUT  *) numPts    : Ctypes.int;
		(* OUTPUT *) ptsReturn : pxlCoord4DStar);

<*EXTERNAL*> PROCEDURE PEXLookatViewMatrix (
		(* INPUT  *) from      : pxlCoord3DStar;
		(* INPUT  *) to        : pxlCoord3DStar;
		(* INPUT  *) up        : pxlVector3DStar;
		(* OUTPUT *) matReturn : pxlMatrixStar)
                (* was pxlMatrix (C vs Modula-3 Array passing problem) *)
	     : X.Status;

<*EXTERNAL*> PROCEDURE PEXPolarViewMatrix (
		(* INPUT  *) from      : pxlCoord3DStar;
		(* INPUT  *) distance  : Ctypes.double;
		(* INPUT  *) azimuth   : Ctypes.double;
		(* INPUT  *) altitude  : Ctypes.double;
		(* INPUT  *) twist     : Ctypes.double;
		(* OUTPUT *) matReturn : pxlMatrixStar) 
                (* was pxlMatrix (C vs Modula-3 Array passing problem) *)
	     : X.Status;

<*EXTERNAL*> PROCEDURE PEXOrthoProjMatrix (
		(* INPUT  *) height    : Ctypes.double;
		(* INPUT  *) aspect    : Ctypes.double;
		(* INPUT  *) near      : Ctypes.double;
		(* INPUT  *) far       : Ctypes.double;
		(* OUTPUT *) matReturn : pxlMatrixStar) 
                (* was pxlMatrix (C vs Modula-3 Array passing problem) *)
	     : X.Status;

<*EXTERNAL*> PROCEDURE PEXPerspProjMatrix (
		(* INPUT  *) fovy      : Ctypes.double;
		(* INPUT  *) distance  : Ctypes.double;
		(* INPUT  *) aspect    : Ctypes.double;
		(* INPUT  *) near      : Ctypes.double;
		(* INPUT  *) far       : Ctypes.double;
		(* OUTPUT *) matReturn : pxlMatrixStar) 
                (* was pxlMatrix (C vs Modula-3 Array passing problem) *)
	     : X.Status;

<*EXTERNAL*> PROCEDURE PEXComputeNormals (
		(* INPUT  *) method     : Ctypes.int;
		(* INPUT  *) primType   : Ctypes.int;
		(* INPUT  *) prim       : Ctypes.char_star;
		(* OUTPUT *) primReturn : Ctypes.char_star);

(*************** Translated up to here! ***************)

(*
 * Routines from pl_mpex.c
 *)

<*EXTERNAL*> PROCEDURE MPEXChangeNewRenderer  (
		(* INPUT  *) display   : X.DisplayStar;
		(* INPUT  *) renderer  : pxlRenderer;
		(* INPUT  *) valueMask : Ctypes.unsigned_long;
		(* INPUT  *) values    : mpxlRendererAttributesStar);

<*EXTERNAL*> PROCEDURE MPEXGetNewRendererAttributes (
		(* INPUT  *) display      : X.DisplayStar;
		(* INPUT  *) renderer     : pxlRenderer;
		(* INPUT  *) valueMask    : Ctypes.unsigned_long;
		(* OUTPUT *) valuesReturn : mpxlRendererAttributesStarStar
             ) : X.Status;

<*EXTERNAL*> PROCEDURE MPEXRenderElements  (
		(* INPUT  *) display   : X.DisplayStar;
		(* INPUT  *) renderer  : pxlRenderer;
		(* INPUT  *) structure : pxlStructure;
		(* INPUT  *) whence1   : Ctypes.int;
		(* INPUT  *) offset1   : Ctypes.long;
		(* INPUT  *) whence2   : Ctypes.int;
		(* INPUT  *) offset2   : Ctypes.long);

<*EXTERNAL*> PROCEDURE MPEXAccumulateState  (
		(* INPUT  *) display     : X.DisplayStar;
		(* INPUT  *) renderer    : pxlRenderer;
		(* INPUT  *) elements    : pxlElementRefStar;
		(* INPUT  *) numElements : Ctypes.int);

<*EXTERNAL*> PROCEDURE MPEXBeginPickOne (
		(* INPUT  *) display  : X.DisplayStar;
		(* INPUT  *) renderer : pxlRenderer;
		(* INPUT  *) d        : X.Drawable;
		(* INPUT  *) id       : Ctypes.unsigned_long;
		(* INPUT  *) which    : Ctypes.int);

<*EXTERNAL*> PROCEDURE MPEXEndPickOne  (
		(* INPUT  *) display           : X.DisplayStar;
		(* INPUT  *) renderer          : pxlRenderer;
		(* OUTPUT *) elementsReturn    : pxlPickPathStarStar;
		(* OUTPUT *) numElementsReturn : Ctypes.int_star) : X.Status;

<*EXTERNAL*> PROCEDURE MPEXPickOne (
		(* INPUT  *) display           : X.DisplayStar;
		(* INPUT  *) renderer          : pxlRenderer;
		(* INPUT  *) d                 : X.Drawable;
		(* INPUT  *) structure         : pxlStructure;
		(* INPUT  *) which             : Ctypes.int;
		(* OUTPUT *) elementsReturn    : pxlPickPathStarStar;
		(* OUTPUT *) numElementsReturn : Ctypes.int_star) : X.Status;

<*EXTERNAL*> PROCEDURE MPEXBeginPickAll  (
		(* INPUT  *) display   : X.DisplayStar;
		(* INPUT  *) renderer  : pxlRenderer;
		(* INPUT  *) d         : X.Drawable;
		(* INPUT  *) id        : Ctypes.unsigned_long;
		(* INPUT  *) sendEvent : X.Bool);

<*EXTERNAL*> PROCEDURE MPEXEndPickAll  (
		(* INPUT  *) display        : X.DisplayStar;
		(* INPUT  *) renderer       : pxlRenderer;
		(* OUTPUT *) moreReturn     : Ctypes.int_star;
		(* OUTPUT *) pathsReturn    : mpxlPickAllPathStarStar;
		(* OUTPUT *) numPathsReturn : Ctypes.int_star) : X.Status;

<*EXTERNAL*> PROCEDURE MPEXPickAll  (
		(* INPUT  *) display        : X.DisplayStar;
		(* INPUT  *) renderer       : pxlRenderer;
		(* INPUT  *) d              : X.Drawable;
		(* OUTPUT *) moreReturn     : Ctypes.int_star;
		(* OUTPUT *) pathsReturn    : mpxlPickAllPathStarStar;
		(* OUTPUT *) numPathsReturn : Ctypes.int_star) : X.Status;

<*EXTERNAL*> PROCEDURE MPEXCopyNewPipelineContext (
		(* INPUT  *) display   : X.DisplayStar;
		(* INPUT  *) valueMask : Ctypes.unsigned_long;
		(* INPUT  *) srcPc     : pxlPipelineContext;
		(* INPUT  *) destPc    : pxlPipelineContext);

<*EXTERNAL*> PROCEDURE MPEXGetNewPipelineContext (
		(* INPUT  *) display             : X.DisplayStar;
		(* INPUT  *) pc                  : pxlPipelineContext;
		(* INPUT  *) valueMask           : Ctypes.unsigned_long;
		(* OUTPUT *) npcAttributesReturn : mpxlPCAttributesStarStar
             ) : X.Status;

<*EXTERNAL*> PROCEDURE MPEXChangeNewPipelineContext (
		(* INPUT  *) display       : X.DisplayStar;
		(* INPUT  *) pc            : pxlPipelineContext;
		(* INPUT  *) valueMask     : Ctypes.unsigned_long;
		(* INPUT  *) npcAttributes : mpxlPCAttributesStar);

<*EXTERNAL*> PROCEDURE MPEXSetHighlightIndex  (
		(* INPUT  *) ocbuf : pxlOCBufStar;
		(* INPUT  *) index : Ctypes.int);

<*EXTERNAL*> PROCEDURE MPEXSetEchoIndex  (
		(* INPUT  *) ocbuf : pxlOCBufStar;
		(* INPUT  *) index : Ctypes.int);

<*EXTERNAL*> PROCEDURE MPEXSetElementPtrAtPickID  (
		(* INPUT  *) display   : X.DisplayStar;
		(* INPUT  *) structure : pxlStructure;
		(* INPUT  *) id        : Ctypes.unsigned_long;
		(* INPUT  *) offset    : Ctypes.long);

<*EXTERNAL*> PROCEDURE MPEXNoop  (
		(* INPUT  *) ocbuf : pxlOCBufStar);

<*EXTERNAL*> PROCEDURE MPEXSetAnnotationTextSkew (
		(* INPUT  *) ocbuf : pxlOCBufStar;
		(* INPUT  *) skew  : Ctypes.float);

<*EXTERNAL*> PROCEDURE MPEXSetTextSkew (
		(* INPUT  *) ocbuf : pxlOCBufStar;
		(* INPUT  *) skew  : Ctypes.float);


<*EXTERNAL*> PROCEDURE MPEXCircle (
		(* INPUT  *) ocbuf  : pxlOCBufStar;
		(* INPUT  *) center : pxlCoord3DStar;
		(* INPUT  *) radius : Ctypes.double);

<*EXTERNAL*> PROCEDURE MPEXArc (
		(* INPUT  *) ocbuf      : pxlOCBufStar;
		(* INPUT  *) center     : pxlCoord3DStar;
		(* INPUT  *) radius     : Ctypes.double;
		(* INPUT  *) startAngle : Ctypes.double;
		(* INPUT  *) endAngle   : Ctypes.double);

<*EXTERNAL*> PROCEDURE MPEXEllipticalArc2D (
		(* INPUT  *) ocbuf      : pxlOCBufStar;
		(* INPUT  *) center     : pxlCoord2DStar;
		(* INPUT  *) majorAxis  : pxlCoord2DStar;
		(* INPUT  *) minorAxis  : pxlCoord2DStar;
		(* INPUT  *) startAngle : Ctypes.double;
		(* INPUT  *) endAngle   : Ctypes.double);


<*EXTERNAL*> PROCEDURE MPEXEllipticalArc3D (
		(* INPUT  *) ocbuf      : pxlOCBufStar;
		(* INPUT  *) center     : pxlCoord3DStar;
		(* INPUT  *) majorAxis  : pxlCoord3DStar;
		(* INPUT  *) minorAxis  : pxlCoord3DStar;
		(* INPUT  *) startAngle : Ctypes.double;
		(* INPUT  *) endAngle   : Ctypes.double);

<*EXTERNAL*> PROCEDURE MPEXEllipse2D (
		(* INPUT  *) ocbuf     : pxlOCBufStar;
		(* INPUT  *) center    : pxlCoord2DStar;
		(* INPUT  *) majorAxis : pxlCoord2DStar;
		(* INPUT  *) minorAxis : pxlCoord2DStar);

<*EXTERNAL*> PROCEDURE MPEXEllipse3D (
		(* INPUT  *) ocbuf     : pxlOCBufStar;
		(* INPUT  *) center    : pxlCoord3DStar;
		(* INPUT  *) majorAxis : pxlCoord3DStar;
		(* INPUT  *) minorAxis : pxlCoord3DStar);

<*EXTERNAL*> PROCEDURE MPEXIndexedPolygons  (
		(* IN-OUT *) ocbuf            : pxlOCBufStar;
		(* INPUT  *) shape            : Ctypes.int;
		(* INPUT  *) facetAttributes  : Ctypes.unsigned_long;
		(* INPUT  *) edgeAttributes   : Ctypes.unsigned_long;
		(* INPUT  *) vertexAttributes : Ctypes.unsigned_long;
		(* INPUT  *) numFacets        : Ctypes.int;
		(* INPUT  *) numEdges         : Ctypes.int;
		(* INPUT  *) numVertices      : Ctypes.int;
		(* INPUT  *) facetCounts      : Ctypes.unsigned_short_star;
		(* INPUT  *) facetOptData     : Ctypes.unsigned_char_star;
		(* INPUT  *) edges            : Ctypes.unsigned_char_star;
		(* INPUT  *) vertices         : Ctypes.unsigned_char_star);

<*EXTERNAL*> PROCEDURE MPEXGridRectangular (
		(* IN-OUT *) ocbuf : pxlOCBufStar;
		(* INPUT  *) origin : pxlCoord3DStar;
		(* INPUT  *) xdist : Ctypes.float;
		(* INPUT  *) ydist : Ctypes.float);

<*EXTERNAL*> PROCEDURE MPEXGridRadial (
		(* IN-OUT *) ocbuf      : pxlOCBufStar;
		(* INPUT  *) center     : pxlCoord3DStar;
		(* INPUT  *) startAngle : Ctypes.float;
		(* INPUT  *) endAngle   : Ctypes.float;
		(* INPUT  *) angDist    : Ctypes.float;
		(* INPUT  *) radDist    : Ctypes.float);

<*EXTERNAL*> PROCEDURE MPEXReferToStructure (
		(* INPUT  *) ocbuf     : pxlOCBufStar;
		(* INPUT  *) structure : pxlStructure);

<*EXTERNAL*> PROCEDURE MPEXAnnotationPixmap  (
		(* INPUT  *) ocbuf  : pxlOCBufStar;
		(* INPUT  *) origin : pxlCoord3DStar;
		(* INPUT  *) index  : Ctypes.int);

<*EXTERNAL*> PROCEDURE MPEXSetDashPattern  (
		(* INPUT  *) display       : X.DisplayStar;
		(* INPUT  *) renderer      : pxlRenderer;
		(* INPUT  *) patternNumber : Ctypes.int;
		(* INPUT  *) pattern       : Ctypes.unsigned_long);

<*EXTERNAL*> PROCEDURE MPEXGetDashPattern (
		(* INPUT  *) display       : X.DisplayStar;
		(* INPUT  *) renderer      : pxlRenderer;
		(* INPUT  *) patternNumber : Ctypes.int;
		(* OUTPUT *) pattern       : Ctypes.unsigned_long_star
             ) : X.Status;

<*EXTERNAL*> PROCEDURE MPEXSetMarkerGlyph  (
		(* INPUT  *) display     : X.DisplayStar;
		(* INPUT  *) renderer    : pxlRenderer;
		(* INPUT  *) glyphNumber : Ctypes.int;
		(* INPUT  *) glyph       : Ctypes.unsigned_short_star;
		    (* in C: unsigned short glyph[16] *)
		(* INPUT  *) xOffset     : Ctypes.int;
		(* INPUT  *) yOffset     : Ctypes.int);

<*EXTERNAL*> PROCEDURE MPEXGetMarkerGlyph (
		(* INPUT  *) display     : X.DisplayStar;
		(* INPUT  *) renderer    : pxlRenderer;
		(* INPUT  *) glyphNumber : Ctypes.int;
		(* OUTPUT *) glyph       : Ctypes.unsigned_short_star;
		    (* in C: unsigned short glyph[16] *)
		(* OUTPUT *) xOffset     : Ctypes.int_star;
		(* OUTPUT *) yOffset     : Ctypes.int_star) : X.Status;

(* 
 * Routines from pl_oc_parse.c
 *)

(* 
 * <*EXTERNAL*> PROCEDURE PEXDumpOCBufHeader  (
 * 	(* IN-OUT *) oc          : pxlOCBufStar;
 * 	(* IN-OUT *) output_file : UNTRACED REF Cstdio.iobuf);
 *               (* was in C:   FILE * output_file   *)
 *)

<*EXTERNAL*> PROCEDURE PEXGetOCListsFromOCBuf  (
		(* IN-OUT *) oc      : pxlOCBufStar;
		(* INPUT  *) routine : PROCEDURE ();
		(* INPUT  *) arg     : Ctypes.unsigned_long);

(* 
 * Routines defined in pl_sc.c
 *)

(* <*EXTERNAL*> PROCEDURE PEXCreateSearchContext (
 * 	/* INPUT  */ X.DisplayStar		display,
 * 	/* INPUT  */ Ctypes.unsigned_long	valueMask,
 * 	/* INPUT  */ pxlSCAttributesStar	values
 * 	) : pxlSearchContext;
 * 
 * <*EXTERNAL*> PROCEDURE PEXFreeSearchContext (
 * 	/* INPUT  */ X.DisplayStar		display,
 * 	/* INPUT  */ pxlSearchContext	sc
 * 	);
 * 
 * <*EXTERNAL*> PROCEDURE PEXCopySearchContext (
 * 	/* INPUT  */ X.DisplayStar		display,
 * 	/* INPUT  */ Ctypes.unsigned_long	valueMask,
 * 	/* INPUT  */ pxlSearchContext	srcSc,
 * 	/* INPUT  */ pxlSearchContext	destSc
 * 	);
 * 
 * <*EXTERNAL*> PROCEDURE PEXGetSearchContext (
 * 	/* INPUT  */ X.DisplayStar		display,
 * 	/* INPUT  */ pxlSearchContext	sc,
 * 	/* INPUT  */ Ctypes.unsigned_long	valueMask,
 * 	/* OUTPUT */ pxlSCAttributesStarStar	scAttributesReturn
 * 	) : X.Status;
 * 
 * <*EXTERNAL*> PROCEDURE PEXChangeSearchContext (
 * 	/* INPUT  */ X.DisplayStar		display,
 * 	/* INPUT  */ pxlSearchContext	sc,
 * 	/* INPUT  */ Ctypes.unsigned_long	valueMask,
 * 	/* OUTPUT */ pxlSCAttributesStar	values
 * 	);
 * 
 * <*EXTERNAL*> PROCEDURE PEXSearchNetwork (
 * 	/* INPUT  */ X.DisplayStar		display,
 * 	/* INPUT  */ pxlSearchContext	sc,
 * 	/* OUTPUT */ pxlStructurePathStarStar	pathReturn
 * 	) : X.Status;
 * 
 * /* 
 *  * Routined defined in pl_wks.c
 *  */
 * 
 * <*EXTERNAL*> PROCEDURE PEXCreatePhigsWks (
 * 	/* INPUT  */ X.DisplayStar		display,
 * 	/* INPUT  */ Drawable		d,
 * 	/* INPUT  */ pxlLookupTable	lineBundle,
 * 	/* INPUT  */ pxlLookupTable	markerBundle,
 * 	/* INPUT  */ pxlLookupTable	textBundle,
 * 	/* INPUT  */ pxlLookupTable	interiorBundle,
 * 	/* INPUT  */ pxlLookupTable	edgeBundle,
 * 	/* INPUT  */ pxlLookupTable	colourTable,
 * 	/* INPUT  */ pxlLookupTable	patternTable,
 * 	/* INPUT  */ pxlLookupTable	textFontTable,
 * 	/* INPUT  */ pxlLookupTable	depthCueTable,
 * 	/* INPUT  */ pxlLookupTable	lightTable,
 * 	/* INPUT  */ pxlLookupTable	colourApproxTable,
 * 	/* INPUT  */ pxlNameSet	highlightIncl,
 * 	/* INPUT  */ pxlNameSet	highlightExcl,
 * 	/* INPUT  */ pxlNameSet	invisibilityIncl,
 * 	/* INPUT  */ pxlNameSet	invisibilityExcl
 * 	) : pxlPhigsWks;
 * 
 * <*EXTERNAL*> PROCEDURE PEXFreePhigsWks (
 * 	/* INPUT  */ X.DisplayStar		display,
 * 	/* INPUT  */ pxlPhigsWks	wks
 * 	);
 * 
 * <*EXTERNAL*> PROCEDURE PEXGetWksInfo (
 * 	/* INPUT  */ X.DisplayStar		display,
 * 	/* INPUT  */ pxlPhigsWks	wks,
 * 	/* INPUT  */ Ctypes.unsigned_long	valueMask,
 * 	/* OUTPUT */ pxlPhigsWksInfoStarStar	wksAttributesReturn
 * 	) : X.Status;
 * 
 * <*EXTERNAL*> PROCEDURE PEXGetWksDynamics (
 * 	/* INPUT  */ X.DisplayStar			display,
 * 	/* INPUT  */ Drawable			d,
 * 	/* OUTPUT */ pxlPhigsWksDynamicsStarStar	wksDynamicsReturn
 * 	) : X.Status;
 * 
 * <*EXTERNAL*> PROCEDURE PEXGetWksViewRep (
 * 	/* INPUT  */ X.DisplayStar		display,
 * 	/* INPUT  */ pxlPhigsWks	wks,
 * 	/* INPUT  */ Ctypes.UnsignedInt	index,
 * 	/* OUTPUT */ Ctypes.int_star		viewUpdateReturn,
 * 	/* OUTPUT */ pxlViewEntryStarStar	reqViewReturn,
 * 	/* OUTPUT */ pxlViewEntryStarStar	curViewReturn
 * 	) : X.Status;
 * 
 * <*EXTERNAL*> PROCEDURE PEXGetWksPostings (
 * 	/* INPUT  */ X.DisplayStar		display,
 * 	/* INPUT  */ pxlStructure	structure,
 * 	/* OUTPUT */ pxlPhigsWksStarStar	wksReturn
 * 	) : X.Status;
 * 
 * <*EXTERNAL*> PROCEDURE PEXSetWksViewPriority (
 * 	/* INPUT  */ X.DisplayStar		display,
 * 	/* INPUT  */ pxlPhigsWks	wks,
 * 	/* INPUT  */ Ctypes.UnsignedInt	index1,
 * 	/* INPUT  */ Ctypes.UnsignedInt	index2,
 * 	/* INPUT  */ Ctypes.int		priority
 * 	);
 * 
 * <*EXTERNAL*> PROCEDURE PEXSetWksDrawableUpdate (
 * 	/* INPUT  */ X.DisplayStar		display,
 * 	/* INPUT  */ pxlPhigsWks	wks,
 * 	/* INPUT  */ Ctypes.int		drawableUpdate
 * 	);
 * 
 * <*EXTERNAL*> PROCEDURE PEXSetWksViewRep (
 * 	/* INPUT  */ X.DisplayStar		display,
 * 	/* INPUT  */ pxlPhigsWks	wks,
 * 	/* INPUT  */ Ctypes.UnsignedInt	viewIndex,
 * 	/* INPUT  */ pxlViewEntryStar	viewRep
 * 	);
 * 
 * <*EXTERNAL*> PROCEDURE PEXSetWksNpcSubvolume (
 * 	/* INPUT  */ X.DisplayStar		display,
 * 	/* INPUT  */ pxlPhigsWks	wks,
 * 	/* INPUT  */ pxlNpcSubvolumeStar	npcSubvolume
 * 	);
 * 
 * <*EXTERNAL*> PROCEDURE PEXSetWksViewport (
 * 	/* INPUT  */ X.DisplayStar		display,
 * 	/* INPUT  */ pxlPhigsWks	wks,
 * 	/* INPUT  */ pxlViewportStar	viewport
 * 	);
 * 
 * <*EXTERNAL*> PROCEDURE PEXSetWksHlhsrMode (
 * 	/* INPUT  */ X.DisplayStar		display,
 * 	/* INPUT  */ pxlPhigsWks		wks,
 * 	/* INPUT  */ Ctypes.UnsignedInt		hlhsrMode
 * 	);
 * 
 * <*EXTERNAL*> PROCEDURE PEXRedrawStructures (
 * 	/* INPUT  */ X.DisplayStar		display,
 * 	/* INPUT  */ pxlPhigsWks		wks
 * 	);
 * 
 * <*EXTERNAL*> PROCEDURE PEXUpdatePhigsWks (
 * 	/* INPUT  */ X.DisplayStar		display,
 * 	/* INPUT  */ pxlPhigsWks		wks
 * 	);
 * 
 * <*EXTERNAL*> PROCEDURE PEXExecuteDeferredActions (
 * 	/* INPUT  */ X.DisplayStar		display,
 * 	/* INPUT  */ pxlPhigsWks		wks
 * 	);
 * 
 * <*EXTERNAL*> PROCEDURE PEXMapDCtoWC (
 * 	/* INPUT  */ X.DisplayStar		display,
 * 	/* INPUT  */ pxlPhigsWks		wks,
 * 	/* INPUT  */ pxlDeviceCoordStar		dcPoints,
 * 	/* INPUT  */ Ctypes.int			numPoints,
 * 	/* OUTPUT */ pxlCoord3DStar		wcPointsReturn,
 * 	/* OUTPUT */ Ctypes.unsigned_int_star	viewIndexReturn
 * 	);
 * 
 * <*EXTERNAL*> PROCEDURE PEXMapWCtoDC (
 * 	/* INPUT  */ X.DisplayStar		display,
 * 	/* INPUT  */ pxlPhigsWks		wks,
 * 	/* INPUT  */ pxlCoord3DStar		wcPoints,
 * 	/* INPUT  */ Ctypes.int			numPoints,
 * 	/* INPUT  */ Ctypes.UnsignedInt		viewIndex,
 * 	/* OUTPUT */ pxlDeviceCoordStar		dcPointsReturn
 * 	);
 * 
 * <*EXTERNAL*> PROCEDURE PEXPostStructure (
 * 	/* INPUT  */ X.DisplayStar		display,
 * 	/* INPUT  */ pxlPhigsWks		wks,
 * 	/* INPUT  */ pxlStructure		structure,
 * 	/* INPUT  */ Ctypes.unsigned_long	priority
 * 	);
 * 
 * <*EXTERNAL*> PROCEDURE PEXUnpostStructure (
 * 	/* INPUT  */ X.DisplayStar		display,
 * 	/* INPUT  */ pxlPhigsWks		wks,
 * 	/* INPUT  */ pxlStructure		structure
 * 	);
 * 
 * <*EXTERNAL*> PROCEDURE PEXUnpostAllStructures (
 * 	/* INPUT  */ X.DisplayStar		display,
 * 	/* INPUT  */ pxlPhigsWks		wks
 * 	);
 *)

(*****************************************************************************)
(* Based on PEXocbuf.h                                                       *)
(*                                                                           *)
(* Everything included, but not everything translated yet!                   *)
(*****************************************************************************)

(*
 * typedefs for referencing fields in oc requests 
 *)

(* typedef struct pexRequestHeader
 * {
 *     unsigned char       extOpcode;
 *     unsigned char       pexOpcode;
 *     unsigned short      reqLength;
 * } pexRequestHeader;
 * 
 * typedef struct pexOCRequestHeader
 * {
 *     unsigned char       extOpcode;
 *     unsigned char       pexOpcode;
 *     unsigned short      reqLength;
 *     unsigned short      fpFormat;
 *     unsigned short      pad;
 *     unsigned long       target;
 *     unsigned long       numCommands;
 * } pexOCRequestHeader;
 * 
 * typedef pexElementInfo pexOCListHeader;
 * 
 * typedef struct pexOCcListHeader
 * {
 *     pexElementInfo      head;
 *     unsigned short      length;
 *     unsigned short      pad;
 * } pexOCcListHeader;
 * 
 * 
 * #define STORE_ELEMENT_INFO(_reqPtr_,_ocType_,_ocLength_) \
 * { \
 *     ((pexElementInfo * )(_reqPtr_))->elementType = (_ocType_); \
 *     ((pexElementInfo * )(_reqPtr_))->length = (_ocLength_); \
 * }
 *)

PROCEDURE PEXAllocateOCBuffer(display : X.DisplayStar;
                              type    : Ctypes.int;
                              target  : X.XID;
                              bufTyp  : PROCEDURE () : Ctypes.int;
                              errorFn : ErrorFunctionType;
                              initSize: Ctypes.int) 
          : pxlOCBufStar;

(*
 * NAME:
 *      PEXDeallocateOCBuffer
 *
 * FORMAT:
 *      void PEXDeallocateOCBuffer( ocbuf)
 *      pxlOCBuf 	*ocbuf;
 *
 * ARGUMENTS:
 *      ocbuf           The OC buffer to be deallocated.
 *
 * RETURNS:
 *      None
 *
 * DESCRIPTION:
 *      This routine frees all memory for the oc buffer
 *      specified by `ocbuf'.
 *
 *      NOTE: This is the only proper way to deallocate an OC buffer.
 *      Do not use the `PEXFree" command to deallocate OC buffers.
 *
 * ERRORS:
 *      None
 *
 * SEE ALSO:
 *      `PEXAllocateRetainedOCBuffer", `PEXAllocateTransientOCBuffer"
 *)

(*
 * #define PEXDeallocateOCBuffer(_ocbuf_) ( *(_ocbuf_)->FreeOCBuf)(_ocbuf_)
 *)


(*
 * NAME:
 *      PEXClearOCBuffer
 *
 * FORMAT:
 *      void PEXClearOCBuffer( ocbuf)
 *      pxlOCBuf 	*ocbuf;
 *
 * ARGUMENTS:
 *      ocbuf           The OC buffer to be emptied 
 *
 * RETURNS:
 *      None
 *
 * DESCRIPTION:
 *      This routine empties all ocs in the oc buffer.  If the oc
 *	buffer is retained then all the ocs currently stored in the buffer
 *	will be deleted.  If the oc buffer is transient then the next
 *	oc will start a new request in the transport buffer. 
 *
 * ERRORS:
 *      None
 *
 * SEE ALSO:
 *      `PEXAllocateRetainedOCBuffer", `PEXAllocateTransientOCBuffer"
 *)

(*
 * #define PEXClearOCBuffer(_ocbuf_) ( *(_ocbuf_)->ClearOCBuf)(_ocbuf_)
 *)



(*
 * NAME:
 *      PEXSetOCBufferType
 *
 * FORMAT:
 *      void PEXSetOCBufferType (display, ocbuf, type, target)
 *      Display         *display;
 *      pxlOCBuf        *ocbuf;
 *      int             type;
 *      XID             target;
 *
 * ARGUMENTS:
 *      display         A pointer to a display structure
 *                      returned by a successful `XOpenDisplay" call.
 *
 *      ocbuf           The OC buffer whose attributes are
 *                      to be modified.
 *
 *      type            The type of OC buffer.
 *
 *      target          The resource identifier of the renderer or structure
 *                      used as a target for the output commands
 *
 * RETURNS:
 *      None
 *
 * DESCRIPTION:
 *      Change the display, target and type associated with the oc buffer.
 *
 *      If `type' is `pxlRenderImmediate', requests that pass output commands to
 *      a renderer are created.
 *      If `type' is `pxlAddToStructure', requests that pass output commands to
 *      a structure are created.
 *      The `PEXlib.h' include file contains definitions of these constants.
 *
 *      `Target' defines the destination for the output commands.
 *      If the buffer type is `pxlRenderImmediate', `target' is the resource
 *      identifier of the renderer to which the output commands will be sent.
 *      If the buffer type is `pxlAddToStructure', `target' is the resource
 *      identifier of the structure to which the output commands will be sent.
 *
 *      This routine will flush the current contents of `ocbuf'.
 *
 * ERRORS:
 *      None
 *
 * SEE ALSO:
 *      `PEXAllocateRetainedOCBuffer", `PEXAllocateTransientOCBuffer",
 *      `PEXGetOCBufferType",  `PEXCreateRenderer", `PEXCreateStructure"
 *
 *)

(*
 * #define PEXSetOCBufferType(_display_, _ocbuf_, _type_, _target_) \
 * 	( *(_ocbuf_)->SetOCBufType)(_display_,_ocbuf_,_type_,_target_)
 *)



(* 
 * NAME:
 *      PEXSetOCBufferTarget
 *
 * FORMAT:
 *      void PEXSetOCBufferTarget (ocbuf, target)
 *      pxlOCBuf        *ocbuf;
 *      XID             target;
 *
 * ARGUMENTS:
 *      ocbuf           The OC buffer whose attributes are
 *                      to be modified.
 *
 *      target          The resource identifier of the renderer or structure
 *                      used as a target for the output commands
 *
 * RETURNS:
 *      None
 *
 * DESCRIPTION:
 *      Change the target associated with the oc buffer.
 *
 *      `Target' defines the destination for the output commands.
 *      If the buffer type is `pxlRenderImmediate', `target' is the resource
 *      identifier of the renderer to which the output commands will be sent.
 *      If the buffer type is `pxlAddToStructure', `target' is the resource
 *      identifier of the structure to which the output commands will be sent.
 *
 *      For retained oc buffers this routine will modify the target 
 *      associated with the output commands currently stored in the oc
 *      buffer.  For transient oc buffers this routine will modify the
 *	target of subsequent output commands.  This routine will `not` 
 *	flush the current contents of `ocbuf'.
 *
 * ERRORS:
 *      None
 *
 * SEE ALSO:
 *      `PEXAllocateRetainedOCBuffer", `PEXAllocateTransientOCBuffer",
 *      `PEXGetOCBufferType",  `PEXCreateRenderer", `PEXCreateStructure"
 *
 *)

(* 
 * #define PEXSetOCBufferTarget(_ocbuf_, _target_) \
 * 	( *(_ocbuf_)->SetOCBufTarget)(_ocbuf_,_target_)
 *)

(*
 * NAME:
 * 	PEXGetOCBufferType
 *
 * FORMAT:
 * 	void PEXGetOCBufferType (ocbuf, displayReturn, typeReturn, targetReturn)
 *	pxlOCBuf	*ocbuf;
 *	Display		**displayReturn;
 *	int		*typeReturn;
 *	XID		*targetReturn;
 *
 * ARGUMENTS:
 *	ocbuf		The OC buffer to be queried.
 *
 *	displayReturn 	Returns a pointer to the display structure used
 *			when the OC buffer is flushed or sent.
 *
 *	typeReturn	Returns the type of the OC buffer.
 *
 *	targetReturn 	Returns the resource identifier of the OC buffer target
 *			renderer or structure
 *
 * RETURNS:
 *	None
 *
 * DESCRIPTION:
 *	This routine returns attributes of `ocbuf' that can be altered.
 *
 *	`DisplayReturn' returns a pointer to information
 *	about the connection over which the output commands are sent.
 *
 *	`typeReturn' returns the OC buffer type.
 *
 *	`targetReturn' returns the resource identifier of the target resource
 *	for the output commands in `ocbuf'.   The resource identifier is
 *	either a renderer or a structure resource identifier, depending on
 *	the OC buffer type.
 *
 * ERRORS:
 *	None
 *
 * SEE ALSO:
 *      `PEXAllocateRetainedOCBuffer", `PEXAllocateTransientOCBuffer",
 *	`PEXSetOCBufferType",  `PEXCreateRenderer", `PEXCreateStructure"
 *
 *)

(*
 * #define PEXGetOCBufferType(_ocbuf_,_displayReturn_,_typeReturn_,_targetReturn_)\
 *   ( *(_ocbuf_)->GetOCBufType)(_ocbuf_,_displayReturn_,_typeReturn_,_targetReturn_)
 *)

(* 
 * NAME:
 * 	PEXGetOCBufferFreeSpace
 *
 * FORMAT:
 *	int PEXGetOCBufferFreeSpace (ocbuf)
 *	pxlOCBuf		*ocbuf;
 *
 * ARGUMENTS:
 *	ocbuf		The OC buffer to be queried.
 *
 * RETURNS:
 *	See description.
 *
 * DESCRIPTION:
 *	This routine returns the free space (in words) remaining in the output
 *	command buffer pointed to by `ocbuf'.
 *
 * ERRORS:
 *	None
 *
 * SEE ALSO:
 *	`PEXAllocateRetainedOCBuffer", `PEXAllocateTransientOCBuffer"
 *
 *)

(*
 * #define PEXGetOCBufferFreeSpace(_ocbuf_) \
 *     ( *(_ocbuf_)->GetOCBufFreeSpace)(_ocbuf_)
 *)

(*
 * NAME:
 * 	PEXGetMaxOCChunkSize
 *
 * FORMAT:
 *	int PEXGetMaxOCChunkSize (ocbuf)
 *	pxlOCBuf		*ocbuf;
 *
 * ARGUMENTS:
 *	ocbuf		The OC buffer to be queried.
 *
 * RETURNS:
 *	See description.
 *
 * DESCRIPTION:
 *      This routine returns the maximum number of words which can be
 *      reserved via PEXGetOCWords.  Note that the size may change if the
 *	display is changed (ie: if `PEXSetOCBufferType" is called)
 *
 * ERRORS:
 *	None
 *
 * SEE ALSO:
 *	`PEXAllocateRetainedOCBuffer", `PEXAllocateTransientOCBuffer"
 *	`PEXGetOCWords", `PEXSetOCBufferType"
 *
 *)

(*
 * #define PEXGetMaxOCChunkSize(_ocbuf_) ( *(_ocbuf_)->GetMaxOCChunkSize)(_ocbuf_)
 *)

PROCEDURE PEXFlushOCBuffer (ocbuf : pxlOCBufStar);

PROCEDURE PEXSendOCBuffer (ocbuf : pxlOCBufStar);

(* /*+
 *  * NAME:
 *  *      INIT_OC
 *  *
 *  * DESCRIPTION:
 *  *	Macro to start an oc. Calls PEXInitOC. 
 *  */
 * #define INIT_OC(_ocbuf_,_ocType_,_ocDataLength_,_pReq_) \
 *     PEXInitOC(_ocbuf_,OC/**/_ocType_,LENOF( pex/**/_ocType_),_ocDataLength_,\
 * 	&(_pReq_))
 * 
 * /*+
 *  * NAME:
 *  *      PEXFinishOC
 *  *
 *  * FORMAT:
 *  *	void PEXFinishOC (ocbuf)
 *  *	pxlOCBuf        *ocbuf;
 *  *
 *  * ARGUMENTS:
 *  *      ocbuf           The OC buffer to which the oc was sent 
 *  *
 *  * RETURNS:
 *  *      None
 *  *
 *  * DESCRIPTION:
 *  *	Called after all the data for the oc has been sent.  
 *  *	`PEXCopyBytesToOC", `PEXCopyWordsToOC" or `PEXGetOCWords" may be 
 *  *	called between `PEXInitOC" and `PEXFinishOC" pairs.  Note that
 *  *	calling `PEXFinishOC" without first calling `PEXInitOC" successfully
 *  *	can have severe consequences since pexlib does not check for this.
 *  *
 *  * ERRORS:
 *  * 	None	
 *  *
 *  * SEE ALSO:
 *  *      `PEXGetOCWords",`PEXCopyWordsToOC",`PEXCopyBytesToOC"
 *  *      `PEXInitOC",`PEXSendOCBuffer",`PEXFlushOCBuffer"
 *  */
 * #define PEXFinishOC(_ocbuf_) ( *(_ocbuf_)->FinishOC)( _ocbuf_)
 * 
 * /*+
 *  * NAME:
 *  *      PEXStoreOCList
 *  *
 *  * FORMAT:
 *  *	void PEXStoreOCList (ocbuf, numElements, ocList)
 *  *	pxlOCBuf        *ocbuf;
 *  *      int		numElements;
 *  *      pxlElementInfo  *ocList;
 *  *
 *  * ARGUMENTS:
 *  *      ocbuf           The OC buffer to store the ocs in 
 *  *	numElements	The number of ocs to store
 *  *	ocList		list of ocs to store
 *  *
 *  * RETURNS:
 *  *      None
 *  *
 *  * DESCRIPTION:
 *  *	Copies the list of output commands to the specified oc buffer. 
 *  *	The registered error function will be called if a memory alloc 
 *  *	error occurs.
 *  *
 *  * ERRORS:
 *  *	None
 *  *
 *  * SEE ALSO:
 *  *	`PEXAllocateRetainedOCBuffer", `PEXAllocateTransientOCBuffer",
 *  *      `PEXSendOCBuffer",`PEXFlushOCBuffer" 
 *  */
 * #define PEXStoreOCList(_ocbuf_,_numElements_,_ocList_) 			\
 * 	( *(_ocbuf_)->StoreOCList)( _ocbuf_,_numElements_,_ocList_)
 * 
 * #ifndef DEBUG
 * /*+
 *  * NAME:
 *  *      PEXGetOCWords
 *  *
 *  * FORMAT:
 *  *	unsigned int *PEXGetOCWords (ocbuf, nWords)
 *  *	pxlOCBuf        *ocbuf;
 *  *	int		nWords;
 *  *
 *  * ARGUMENTS:
 *  *      ocbuf           The OC buffer to allocate words from.
 *  *	nWords		The number of words to reserve
 *  *
 *  * RETURNS:
 *  *      Pointer to 'nWords' of data 
 *  *
 *  * DESCRIPTION:
 *  *	Return a pointer to 'nWords' of data located in the output command 
 *  *	which is currently in progress.  'PEXInitOC" must be called prior 
 *  *	to this.   NULL will be returned if an attempt is made to grab more 
 *  *	words than were reserved when `PEXInitOC" was called or if an attempt 
 *  *	is made to grab more words than specified in 'PEXGetMaxOCChunkSize". 
 *  *
 *  *      The code below is implemented as a macro since it will be called
 *  *	often and it is critical that it performs well.  The basic 
 *  *	algorithm is 
 *  *
 *  *    	    if ( (A && B) || C )
 *  *        	return ( (curPtr += nWords) - nWords);
 *  *    	    else
 *  *	  	return NULL;
 *  *
 *  * where A =
 *  * 	(the words requested dont overflow the request which is being built)
 *  *
 *  * B = 
 *  *  	(there is room in the current Xlib buffer) OR 
 *  * 	(we can get room in the current Xlib buffer)
 *  *
 *  * C =
 *  *  	(the current request is a large request) AND 
 *  *  	(the words requested dont overflow the large request) AND 
 *  *  	(a new large request packet was successfully started )
 *  *	 
 *  *
 *  * ERRORS:
 *  * 	None	
 *  *
 *  * SEE ALSO:
 *  *      `PEXInitOC", `PEXFinishOC"
 *  *
 *  */
 * #define RoomInThisRequest(_oc_,_nWords_)			\
 *  ( ((_oc_)->ocWordsLeft -= (_nWords_)) >= 0 )
 * 
 * #define RoomInXBuffer(_oc_,_nWords_) 				\
 *  ( ((_nWords_) <= ((_oc_)->bufMax - (_oc_)->curPtr)) ||		\
 *    (( *(_oc_)->GetOCWords)((_oc_), (_nWords_))) )
 * 
 * #define NewLargeRequestPacket(_oc_,_nWords_)			\
 *  ( ((_oc_)->lrSequenceNum) &&  					\
 *    (((_oc_)->lrBufferLeft -= (_nWords_)) >= 0) && 		\
 *    (((_oc_)->ocWordsLeft= ( *(_oc_)->StartLargeRequest)(_oc_)-(_nWords_)) >= 0) )
 * 
 * #define PEXGetOCWords(_oc_,_nWords_) 				\
 *  ( (( RoomInThisRequest(_oc_,_nWords_) && RoomInXBuffer(_oc_,_nWords_) ) || \
 *     NewLargeRequestPacket(_oc_,_nWords_)) ?			\
 * 	 (((_oc_)->curPtr += (_nWords_)) - (_nWords_)) 		\
 *   : 								\
 *          (NULL) )
 * #endif /* DEBUG not defined */
 * 
 * #ifndef DEBUG
 * /*+
 *  * NAME:
 *  * 	PEXAddOC
 *  *
 *  * FORMAT:
 *  *	void PEXAddOC (ocbuf, ocType, size, data)
 *  *	pxlOCBuf	*ocbuf;
 *  *	unsigned int	ocType;
 *  *	int		size;
 *  *	char		*data;
 *  *
 *  * ARGUMENTS:
 *  *	ocbuf		The OC buffer to which the output command
 *  *			is to be added.
 *  *
 *  *	ocType		The type of output command to be added
 *  *			to the OC buffer.
 *  *
 *  *	size		The size (in bytes) of the output command.
 *  *
 *  *	data		A pointer to the data for the output command.
 *  *
 *  * RETURNS:
 *  *	None
 *  *
 *  * DESCRIPTION:
 *  *	This routine adds an output command to `ocbuf'.  The `ocType'
 *  *	argument indicates the type of output command being passed.
 *  *	The `PEX.h' include file contains possible values for `octype',
 *  *	for example, `PEXOCMarkerType' and  `PEXOCNurbCurve'.
 *  *
 *  *	`Data' points to the actual data for the output command.
 *  *
 *  *	`Size' specifies the length of `data' (in bytes).
 *  *
 *  *	Client programs should call the individual output
 *  *	command functions or use the output command macros
 *  *	defined in `PEXoc.h' or `MPEXoc.h' instead of calling this routine
 *  *	directly.
 *  *
 *  * ERRORS:
 *  *	Renderer		Specified renderer resource identifier is
 *  *				invalid.
 *  *
 *  *	Structure		Specified structure resource identifier is
 *  *				invalid.
 *  *
 *  *	FloatingPointFormat	Device does not support the specified floating
 *  *				point format.
 *  *
 *  *	OutputCommand		A value in an output command is illegal,
 *  *				out of range, or otherwise inappropriate.
 *  *
 *  * SEE ALSO:
 *  *	`PEXAllocateRetainedOCBuffer", `PEXAllocateTransientOCBuffer"
 *  *
 *  */
 * 
 * #define PEXAddOC(_ocbuf_,_ocType_,_size_,_OCData_)		\
 * {								\
 *     int _ocLength = ((_size_) + Pad(_size_) + sizeof(pexElementInfo))>>2;\
 *     if ( ( *(_ocbuf_)->StartOC)((_ocbuf_),_ocLength) == Success )\
 *     {								\
 *         STORE_ELEMENT_INFO((_ocbuf_)->curPtr, _ocType_, _ocLength);\
 *         (_ocbuf_)->curPtr += LENOF(pexElementInfo);		\
 *         if (_size_ > 0) COPY_AREA(_OCData_, (_ocbuf_)->curPtr, _size_);\
 *         (_ocbuf_)->curPtr += NUMWORDS(_size_);			\
 *         ( *(_ocbuf_)->FinishOC)((_ocbuf_));			\
 *    }								\
 *     else							\
 *         ( *(_ocbuf_)->OCError)((_ocbuf_), _ocLength);		\
 * }
 * #endif /* DEBUG not defined */
 * 
 * #ifndef DEBUG
 * /*+ 
 *  * NAME:
 *  *	PEXAddListOC
 *  *
 *  * FORMAT:
 *  *	void PEXAddListOC (ocbuf, ocType, countNeeded, count, elementSize, 
 *  *			   elementList)
 *  *	pxlOCBuf	*ocbuf;
 *  *	unsigned int	ocType;
 *  *	int		countNeeded;
 *  *	int		count;
 *  *	int		elementSize;
 *  *	char		*elementList;
 *  *
 *  * ARGUMENTS:
 *  *	ocbuf		The OC buffer to which the output command is to be
 *  *			added.
 *  *
 *  *	ocType		The type of output command to be added to the OC 
 *  *			buffer.
 *  *
 *  *	countNeeded     True if the list to be added requires a count to
 *  *			precede it; False otherwise 
 *  *
 *  *	count		The number of items in the list to be placed
 *  *			in the output command.
 *  *
 *  *	elementSize	The size (in bytes) of each item in the list.
 *  *
 *  *	elementList	A pointer to the data for the list.
 *  *
 *  * RETURNS:
 *  *	None
 *  *
 *  * DESCRIPTION:
 *  *	This routine adds an output command of type `ocType' to the oc
 *  *	buffer.  The `PEX.h' include file contains possible values of `ocType'.
 *  *
 *  *	The last part of the command is a list of `count' items.  Each item is
 *  *	`elementSize' bytes long.  The list is padded if necessary.  If 
 *  *	`countNeeded`  is True the list created in `ocbuf' is preceded by a 
 *  *	count.  `elementList' points to the data for the list.  
 *  *
 *  * ERRORS:
 *  *	Renderer		Specified renderer resource identifier is
 *  *				invalid.
 *  *
 *  *	Structure		Specified structure resource identifier is
 *  *				invalid.
 *  *
 *  *	FloatingPointFormat	Device does not support the specified floating
 *  *				point format.
 *  *
 *  *	OutputCommand		A value in an output command is illegal,
 *  *				out of range, or otherwise inappropriate.
 *  *
 *  * SEE ALSO:
 *  *	`PEXAllocateRetainedOCBuffer", `PEXAllocateTransientOCBuffer",
 *  *	`PEXAddOC"
 *  *
 *  */
 * #define PEXAddListOC(_ocbuf_,_ocType_,_countNeeded_,_count_,_elSize_,_elList_)\
 * {							\
 *     int         _ocListSize = (_count_) * (_elSize_);	\
 *     char        *_pReq;					\
 *     PEXInitOC(_ocbuf_,_ocType_,				\
 *                 (_countNeeded_) ? LENOF( pexOCcListHeader) :\
 *                                 LENOF( pexOCListHeader),\
 *                 NUMWORDS( _ocListSize), &_pReq);	\
 *     if (_pReq == NULL) return;				\
 *     if (_countNeeded_) ((pexOCcListHeader * )_pReq)->length = (_count_);\
 *     PEXCopyBytesToOC(_ocbuf_, _ocListSize, _elList_);	\
 *     PEXFinishOC(_ocbuf_);				\
 * }
 * 
 *)

(*****************************************************************************)
(* Based on MPEX.h                                                           *)
(*****************************************************************************)

(*
 * /* Definitions for Digital Pick/Echo/Z PEX extension likely to be used by
 * applications */
 * 
 * #define MPEX_REQ	0x00000080 /* reqType is CARD8 */
 * #define MPEX_EVENT	0x00000000 /* offset from base event num from XInitExt */
 * #define MPEX_OC		0x00008000 /* elementType is CARD16 */
 * #define MPEX_ENUM	0x00008000 /* enum types are CARD16 */
 * #define MPEX_TABLE	0x00008000 /* pexTableType is CARD16 */
 * 
 * /*** Enumerated Types ***/
 * #define MPEXETHighlightMethod	(MPEX_ENUM+0)
 * #define MPEXETEchoMethod		(MPEX_ENUM+1)
 * #define MPEXETOverlapMode		(MPEX_ENUM+2)
 * #define MPEXETLogicalOp		(MPEX_ENUM+3)
 * 
 * #define FirstMPEXEnumType	(MPEX_ENUM+0)
 * #define LastMPEXEnumType	(MPEX_ENUM+3)
 * 
 * /*** MPEXHighlightLUT and Echo Methods ***/
 * #define MPEXChangeAllColors		0
 * #define MPEXChangeEdgeColor		1
 * #define MPEXChangeInteriorColor	2
 * 
 * /*** additional Hlhsr mode ***/
 * #define MPEXHlhsrZBufferId		-2
 * 
 * /*** Overlap Mode Values ***/
 * #define MPEXAnyPrim		0
 * #define MPEXFirstPrim	1
 * #define MPEXLastPrim	2
 * 
 * /*** Logical Operations ***/
 * #define MPEXOpClear		0x0
 * #define MPEXOpAnd		0x1
 * #define MPEXOpAndReverse	0x2
 * #define MPEXOpCopy		0x3
 * #define MPEXOpAndInverted	0x4
 * #define MPEXOpNoop		0x5
 * #define MPEXOpXor		0x6
 * #define MPEXOpOr		0x7
 * #define MPEXOpNor		0x8
 * #define MPEXOpEquiv		0x9
 * #define MPEXOpInvert	0xA
 * #define MPEXOpOrReverse	0xB
 * #define MPEXOpCopyInverted	0xC
 * #define MPEXOpOrInverted	0xD
 * #define MPEXOpNand		0xE
 * #define MPEXOpSet		0xF
 * 
 * /*** Lookup Table Types ***/
 * #define MPEXHighlightLUT	(MPEX_TABLE+0)
 * #define MPEXEchoLUT	(MPEX_TABLE+1)
 * #define MPEXPixmapLUT	(MPEX_TABLE+2)
 * #define MPEXTextureLUT	(MPEX_TABLE+3)
 * 
 * #define maxMPEXTableType	(MPEX_TABLE+4)
 * #define FirstMPEXTableType	(MPEX_TABLE+0)
 * #define LastMPEXTableType	(MPEX_TABLE+3)
 * 
 * /*** Renderer State ***/
 * #define MPEXPicking			2
 * #define	MPEXTransparencyOpaque	3
 * #define	MPEXTransparency		4
 * 
 * /*** Which Element to Pick ***/
 * #define MPEXPickClosest	0
 * #define MPEXPickLast	1
 * 
 * /*** Values for "more picks" ***/
 * #define MPEXNoMorePicks		0
 * #define MPEXMorePicks		1
 * #define MPEXMaybeMorePicks		2
 * 
 * /*** user-settable Dash Patterns ***/
 * #define MPEXDashPattern1	-1
 * #define MPEXDashPattern2	-2
 * #define MPEXDashPattern3	-3
 * #define MPEXDashPattern4	-4
 * #define MPEXDashPattern5	-5
 * #define MPEXDashPattern6	-6
 * #define MPEXDashPattern7	-7
 * #define MPEXDashPattern8	-8
 * #define MPEXDashPattern9	-9
 * #define MPEXDashPattern10	-10
 * #define MPEXDashPattern11	-11
 * #define MPEXDashPattern12	-12
 * #define MPEXDashPattern13	-13
 * #define MPEXDashPattern14	-14
 * #define MPEXDashPattern15	-15
 * #define MPEXDashPattern16	-16
 * 
 * /*** user-settable Marker Glyphs ***/
 * #define MPEXMarkerGlyph1	-1
 * #define MPEXMarkerGlyph2	-2
 * #define MPEXMarkerGlyph3	-3
 * #define MPEXMarkerGlyph4	-4
 * #define MPEXMarkerGlyph5	-5
 * #define MPEXMarkerGlyph6	-6
 * #define MPEXMarkerGlyph7	-7
 * #define MPEXMarkerGlyph8	-8
 * #define MPEXMarkerGlyph9	-9
 * #define MPEXMarkerGlyph10	-10
 * #define MPEXMarkerGlyph11	-11
 * #define MPEXMarkerGlyph12	-12
 * #define MPEXMarkerGlyph13	-13
 * #define MPEXMarkerGlyph14	-14
 * #define MPEXMarkerGlyph15	-15
 * #define MPEXMarkerGlyph16	-16
 * 
 * /*** Bit Numbers for Change/Get/CopyNewPipelineContext ***/
 * #define MPEXNPCHighlightIndex		0
 * #define MPEXNPCEchoIndex		1
 * #define MPEXNPCTextSkew			2
 * #define MPEXNPCAnnotationTextSkew	3
 * #define MPEXMaxNPCIndex			3
 *)

(*** Bit Numbers for Change/GetNewRendererAttributes ***)

CONST
  MPEXNRALogicalOpBit       =  0;
  MPEXNRAPlaneMaskBit       =  1;
  MPEXNRABackgroundPixelBit =  2;
  MPEXNRAClearIBit          =  3;
  MPEXNRAClearZBit          =  4;
  MPEXNRAOverlapModeBit     =  5;
  MPEXNRAAaSwitchBit        =  6;
  MPEXNRAHighlightTableBit  =  7;
  MPEXNRAEchoTableBit       =  8;
  MPEXNRAEchoSwitchBit      =  9;
  MPEXNRAPickPositionBit    = 10;
  MPEXNRAPickDistanceBit    = 11;
  MPEXNRAPickInclusionBit   = 12;
  MPEXNRAPickExclusionBit   = 13;
  MPEXNRAPickMaxHitsBit     = 14;
  MPEXNRAPickStartPathBit   = 15;
  MPEXNRACullSwitchBit      = 16;
  MPEXNRAPixmapTableBit     = 17;
  MPEXNRAErrorCheckBit      = 18;
  MPEXNRATextureTableBit    = 19;
  MaxNRAShift               = 20;
  FirstNRABit               =  0;
  LastNRABit                = 19;

(*** Bit masks for Change/GetNewRendererAttributes ***)

  MPEXNRALogicalOp       = Word.LeftShift(1,MPEXNRALogicalOpBit);
  MPEXNRAPlaneMask       = Word.LeftShift(1,MPEXNRAPlaneMaskBit);
  MPEXNRABackgroundPixel = Word.LeftShift(1,MPEXNRABackgroundPixelBit);
  MPEXNRAClearI          = Word.LeftShift(1,MPEXNRAClearIBit);
  MPEXNRAClearZ          = Word.LeftShift(1,MPEXNRAClearZBit);
  MPEXNRAOverlapMode     = Word.LeftShift(1,MPEXNRAOverlapModeBit);
  MPEXNRAAaSwitch        = Word.LeftShift(1,MPEXNRAAaSwitchBit);
  MPEXNRAHighlightTable  = Word.LeftShift(1,MPEXNRAHighlightTableBit);
  MPEXNRAEchoTable       = Word.LeftShift(1,MPEXNRAEchoTableBit);
  MPEXNRAEchoSwitch      = Word.LeftShift(1,MPEXNRAEchoSwitchBit);
  MPEXNRAPickPosition    = Word.LeftShift(1,MPEXNRAPickPositionBit);
  MPEXNRAPickDistance    = Word.LeftShift(1,MPEXNRAPickDistanceBit);
  MPEXNRAPickInclusion   = Word.LeftShift(1,MPEXNRAPickInclusionBit);
  MPEXNRAPickExclusion   = Word.LeftShift(1,MPEXNRAPickExclusionBit);
  MPEXNRAPickMaxHits     = Word.LeftShift(1,MPEXNRAPickMaxHitsBit);
  MPEXNRAPickStartPath   = Word.LeftShift(1,MPEXNRAPickStartPathBit);
  MPEXNRACullSwitch      = Word.LeftShift(1,MPEXNRACullSwitchBit);
  MPEXNRAPixmapTable     = Word.LeftShift(1,MPEXNRAPixmapTableBit);
  MPEXNRAErrorCheck      = Word.LeftShift(1,MPEXNRAErrorCheckBit);
  MPEXNRATextureTable    = Word.LeftShift(1,MPEXNRATextureTableBit);

(* /*** Output Command Opcodes ***/
 * #define MPEXOCHighlightIndex		(MPEX_OC+0)
 * #define MPEXOCEchoIndex			(MPEX_OC+1)
 * #define MPEXOCNoop			(MPEX_OC+2)
 * #define MPEXOCAnnoPixmap		(MPEX_OC+3)
 * #define MPEXOCCircle			(MPEX_OC+4)
 * #define MPEXOCArc			(MPEX_OC+5)
 * #define MPEXOCIndexedPolygons		(MPEX_OC+6)
 * #define MPEXOCEllipticalArc2D		(MPEX_OC+7)
 * #define MPEXOCEllipticalArc3D		(MPEX_OC+8)
 * #define MPEXOCEllipse2D			(MPEX_OC+9)
 * #define MPEXOCEllipse3D			(MPEX_OC+10)
 * #define MPEXOCGridRectangular           (MPEX_OC+11)
 * #define MPEXOCGridRadial                (MPEX_OC+12)
 * #define MPEXOCTextSkew			(MPEX_OC+13)
 * #define MPEXOCAnnotationTextSkew	(MPEX_OC+14)
 * #define MPEXOCReferToStructure          (MPEX_OC+15)
 * #define MPEXOCFFTextureMapIndex         (MPEX_OC+16)
 * #define MPEXOCBFTextureMapIndex         (MPEX_OC+17)
 * 
 * #define MaxMPEXOC			(MPEX_OC+18)
 * #define FirstMPEXOC			(MPEX_OC+0)
 * #define LastMPEXOC			(MPEX_OC+17)
 * 
 * /*** Events ***/
 * #define MPEXMaxHitsReached	        (MPEX_EVENT+0)
 * 
 * /*** Requests ***/
 * #define MPEX_ChangeNewRenderer		(MPEX_REQ+0)
 * #define MPEX_GetNewRendererAttributes	(MPEX_REQ+1)
 * #define MPEX_RenderElements		(MPEX_REQ+2)
 * #define MPEX_AccumulateState		(MPEX_REQ+3)
 * #define MPEX_BeginPickOne		(MPEX_REQ+4)
 * #define MPEX_EndPickOne			(MPEX_REQ+5)
 * #define MPEX_PickOne			(MPEX_REQ+6)
 * #define MPEX_BeginPickAll		(MPEX_REQ+7)
 * #define MPEX_EndPickAll			(MPEX_REQ+8)
 * #define MPEX_PickAll			(MPEX_REQ+9)
 * #define MPEX_SetElementPointerAtPickId	(MPEX_REQ+10)
 * #define MPEX_SetDashPattern		(MPEX_REQ+11)
 * #define MPEX_GetDashPattern		(MPEX_REQ+12)
 * #define MPEX_SetMarkerGlyph		(MPEX_REQ+13)
 * #define MPEX_GetMarkerGlyph		(MPEX_REQ+14)
 * #define MPEX_LargeRequest		(MPEX_REQ+15)
 * #define MPEX_BeginTransparencyRendering	(MPEX_REQ+16)
 * #define MPEX_EndTransparencyRendering	(MPEX_REQ+17)
 * #define MPEX_CopyNewPipelineContext	(MPEX_REQ+18)
 * #define MPEX_GetNewPipelineContext	(MPEX_REQ+19)
 * #define MPEX_ChangeNewPipelineContext	(MPEX_REQ+20)
 * 
 * #define MaxMPEXCommand		(MPEX_REQ+21)
 * #define FirstMPEXCommand	(MPEX_REQ+0)
 * #define LastMPEXCommand		(MPEX_REQ+20)
 * 
 * /* dummied in because Pete doesn't know any better (used in pl_startup.c) */
 * 
 * #define	PEXMinError	0
 * 
 * #define Texture Information */
 * 
 * #define MPEXGATexture    0x0008
 * #define MPEXInteriorStyleTexture 6
 *  
 * /* Texture Mapping Definition */
 * 
 * #define MPEXTextureMapWrap      0
 * #define MPEXTextureMapClamp     1
 * #define MPEXTextureMapNone      2
 * 
 * #endif /* MPEX_H */
 *)

(*****************************************************************************)
(* Based on MPEXlib.h                                                        *)
(*****************************************************************************)

(*
 * #ifndef MPEX_H
 * #  include "MPEX.h"
 * #endif
 * #ifndef MPEXPROTO_H
 * #  include "MPEXproto.h"
 * #endif
 * #ifndef MPEXPROTOSTR_H
 * #  include "MPEXprotostr.h"
 * #endif
 *)

(*
 * Following are the typedefs used in the PEXlib interface. mpxlFooBar
 * definitions are always supersets of the mpexFooBar protocol definitions.
 * Note that most mpxlFooBar definitions are equivalent to the mpexFooBar
 * protocol definitions.  However, some of the definitions do have extra
 * fields. See documentation in PEXlib.h for more details. 
 *)

(*
 *  Definitions for output commands
 *)

TYPE
  mpxlAnnoPixmap         = mpexAnnoPixmap;
  mpxlArc                = mpexArc;
  mpxlCircle             = mpexCircle;
  mpxlIndexedPolygon     = mpexIndexedPolygon;
  mpxlEllipticalArc2D    = mpexEllipticalArc2D;
  mpxlEllipticalArc3D    = mpexEllipticalArc3D;
  mpxlEllipse2D          = mpexEllipse2D;
  mpxlEllipse3D          = mpexEllipse3D;
  mpxlGridRectangular    = mpexGridRectangular;
  mpxlGridRadial         = mpexGridRadial;
  mpxlEchoIndex          = mpexEchoIndex;
  mpxlHighlightIndex     = mpexHighlightIndex;
  mpxlNoop               = mpexNoop;


(*
 *  Definitions for lookup table entries
 *)

TYPE
  mpxlHighlightEntry = RECORD
    method : pxlEnumTypeIndex;
    pad    : CARD16;
    colour : BITS BITSIZE (pxlColourSpecifier) FOR pxlColourSpecifier;
  END;

  mpxlEchoEntry = RECORD
    method : pxlEnumTypeIndex;
    pad    : CARD16;
    colour : BITS BITSIZE (pxlColourSpecifier) FOR pxlColourSpecifier;
  END;

  mpxlPixmapEntry = mpexPixmapEntry;

(* 
 * pick path returned by pick all 
 *)

  mpxlPickAllPath = RECORD
    pathLength  : Ctypes.int;
    elementRefs : pxlPickPathStar;
  END;
  mpxlPickAllPathStar     = UNTRACED REF mpxlPickAllPath;
  mpxlPickAllPathStarStar = UNTRACED REF mpxlPickAllPathStar;

(* 
 * Definition for MPEX Pipeline Context Attributes.  Defines are in MPEX.h. 
 *)

  mpxlPCAttributes = RECORD
    highlightIndex     : Ctypes.unsigned_short;
    echoIndex          : Ctypes.unsigned_short;
    textSkew           : Ctypes.float;
    annotationTextSkew : Ctypes.float;
    textureMapIndex    : Ctypes.unsigned_short;   (* Only in AOSF version *)
    textureMapState    : Ctypes.unsigned_short;   (* Only in AOSF version *)
  END;
  mpxlPCAttributesStar     = UNTRACED REF mpxlPCAttributes;
  mpxlPCAttributesStarStar = UNTRACED REF mpxlPCAttributesStar;

(*** macro for setting bits in a new PC value mask ***)

(* 
 * #define MPEX_SetPCAttrMaskBit(mask, attrNum) \
 *     mask |= 1L << ( ((attrNum)) & 0x1F)
 *) 

(* 
 * Definition for MPEX Renderer Attributes.  Defines are in MPEX.h. 
 *)

TYPE 
  mpxlRendererAttributes = RECORD
    logicalOp       : Ctypes.short;
    overlapMode     : Ctypes.short;
    planeMask       : Ctypes.unsigned_long;
    backgroundPixel : Ctypes.unsigned_long;
    clearI          : Ctypes.unsigned_char;
    clearZ          : Ctypes.unsigned_char;
    errorCheck      : Ctypes.unsigned_char;
    aaSwitch        : Ctypes.unsigned_char;
    echoSwitch      : Ctypes.unsigned_char;
    cullSwitch      : Ctypes.unsigned_char;
    highlightTable  : pxlLookupTable;
    echoTable       : pxlLookupTable;
    pixmapTable     : pxlLookupTable;
    textureTable    : pxlLookupTable;
    pickPosition    : pxlDeviceCoord2D;
    pickDistance    : Ctypes.float;
    pickInclusion   : pxlNameSet;
    pickExclusion   : pxlNameSet;
    pickMaxHits     : Ctypes.unsigned_long;
    pickStartPath   : pxlStructurePath;
  END; (* mpxlRendererAttributes *)
  mpxlRendererAttributesStar     = UNTRACED REF mpxlRendererAttributes;
  mpxlRendererAttributesStarStar = UNTRACED REF mpxlRendererAttributesStar;

(*** Event Structures ***)

(* The first several items in each event structure must be as they are or
   toolkits will break *)

(* typedef struct mpxlMaxHitsReachedEvent
 * {
 *     int 		type;		/* type of event, MPEXMaxHitsReached */
 *     unsigned long	serial;		/* # of last req processed by server */
 *     Bool		send_event;	/* True if came from SendEvent req */
 *     Display		*display;	/* Display the event was read from */
 *     pxlRenderer		rdr;		/* renderer doing the pick */
 * } mpxlMaxHitsReachedEvent;
 *)


(*
 * From MPEXproto.h
 *)

(* Definitions for Digital Pick/PEXEcho/Z PEX extension used by the server and
   C bindings *)

(****************************************************************
 * 		REPLIES						*
 ****************************************************************)

(*
 * typedef struct
 * {
 *     BYTE		type;	/* X_Reply */
 *     CARD8		what;
 *     CARD16		sequenceNumber B16;
 *     CARD32		length B32;	/* not 0 */
 *     BYTE		pad[24];
 *     /* SINGLE NewRendererAttributes(itemMask) */
 * } mpexGetNewRendererAttributesReply;
 * 
 * typedef struct
 * {
 *     BYTE		type;	/* X_Reply */
 *     CARD8		what;
 *     CARD16		sequenceNumber B16;
 *     CARD32		length B32;	/* not 0 */
 *     CARD32		numRefs B32;
 *     BYTE		pad[20];
 *     /* LISTof PickElementRef(numRefs) */
 * } mpexEndPickOneReply;
 * 
 * typedef mpexEndPickOneReply mpexPickOneReply;
 * 
 * typedef struct
 * {
 *     BYTE		type;	/* X_Reply */
 *     CARD8		what;
 *     CARD16		sequenceNumber B16;
 *     CARD32		length B32;	/* not 0 */
 *     CARD32		numPicks B32;
 *     CARD8		morePicks;
 *     BYTE		pad[19];
 *     /* LISTof CLISTof PickElementRef(numPicks) */
 * } mpexEndPickAllReply;
 * 
 * typedef mpexEndPickAllReply mpexPickAllReply;
 * 
 * typedef struct
 * {
 *     BYTE		type;	/* X_Reply */
 *     CARD8		what;
 *     CARD16		sequenceNumber B16;
 *     CARD32		length B32;	/* 0 */
 *     CARD32		pattern B32;
 *     BYTE		pad[20];
 * } mpexGetDashPatternReply;
 * 
 * typedef struct
 * {
 *     BYTE		type;	/* X_Reply */
 *     CARD8		what;
 *     CARD16		sequenceNumber B16;
 *     CARD32		length B32;	/* not 0 */
 *     INT16		xOffset B16;
 *     INT16		yOffset B16;
 *     BYTE		pad[20];
 *     CARD16		glyph[16];
 * } mpexGetMarkerGlyphReply;
 * 
 * 
 * typedef struct
 * {
 *     BYTE		type;	/* X_Reply */
 *     CARD8		what;
 *     CARD16		sequenceNumber B16;
 *     CARD32		length B32;	/* 0 */
 *     BYTE		requireAnotherPass;
 *     BYTE		pad[23];
 * } mpexEndTransparencyRenderingReply;
 * 
 * 
 * /****************************************************************
 *  *		EVENTS						*
 *  ****************************************************************/
 * 
 * typedef struct
 * {
 *     BYTE		type;
 *     BYTE		detail;
 *     CARD16		sequenceNumber B16;
 *     pexRenderer		rdr B32;
 *     BYTE		pad[24];
 * } mpexMaxHitsReachedEvent;
 * 
 * /****************************************************************
 *  *		REQUESTS					*
 *  ****************************************************************/
 * 
 * typedef struct
 * {
 *     CARD8		reqType;
 *     CARD8		opcode;
 *     CARD16		length B16;
 *     pexEnumTypeIndex	fpFormat B16;
 *     CARD16		pad B16;
 *     pexRenderer		rdr B32;
 *     pexBitmask		itemMask B32;
 *     /* SINGLE NewRendererAttributes(itemMask) */
 * } mpexChangeNewRendererReq;
 * 
 * typedef struct
 * {
 *     CARD8		reqType;
 *     CARD8		opcode;
 *     CARD16		length B16;
 *     pexEnumTypeIndex	fpFormat B16;
 *     CARD16		pad B16;
 *     pexRenderer		rdr B32;
 *     pexBitmask		itemMask B32;
 * } mpexGetNewRendererAttributesReq;
 * 
 * typedef struct
 * {
 *     CARD8		reqType;
 *     CARD8		opcode;
 *     CARD16		length B16;
 *     pexRenderer		rdr B32;
 *     pexStructure	sid B32;
 *     pexElementRange	range;
 * } mpexRenderElementsReq;
 * 
 * typedef struct
 * {
 *     CARD8		reqType;
 *     CARD8		opcode;
 *     CARD16		length B16;
 *     pexRenderer		rdr B32;
 *     CARD32		numRefs B32;
 *     /* LISTof ElementRef(numRefs) */
 * } mpexAccumulateStateReq;
 * 
 * typedef struct
 * {
 *     CARD8		reqType;
 *     CARD8		opcode;
 *     CARD16		length B16;
 *     pexRenderer		rdr B32;
 *     Drawable		drawable B32;
 *     CARD32		id B32;
 *     CARD8		which;
 *     BYTE		pad[3];
 * } mpexBeginPickOneReq;
 * 
 * typedef pexResourceReq mpexEndPickOneReq;
 * 
 * typedef struct
 * {
 *     CARD8		reqType;
 *     CARD8		opcode;
 *     CARD16		length B16;
 *     pexRenderer		rdr B32;
 *     Drawable		drawable B32;
 *     pexStructure	sid B32;
 *     CARD8		which;
 *     BYTE		pad[3];
 * } mpexPickOneReq;
 * 
 * typedef struct
 * {
 *     CARD8		reqType;
 *     CARD8		opcode;
 *     CARD16		length B16;
 *     pexRenderer		rdr B32;
 *     Drawable		drawable B32;
 *     unsigned long	id B32;
 *     CARD8		sendEvent;
 *     BYTE		pad[3];
 * } mpexBeginPickAllReq;
 * 
 * typedef pexResourceReq mpexEndPickAllReq;
 * 
 * typedef struct
 * {
 *     CARD8		reqType;
 *     CARD8		opcode;
 *     CARD16		length B16;
 *     pexRenderer		rdr B32;
 *     Drawable		drawable B32;
 * } mpexPickAllReq;
 * 
 * typedef struct
 * {
 *     CARD8		reqType;
 *     CARD8		opcode;
 *     CARD16		length B16;
 *     pexStructure	str B32;
 *     CARD32		pickid B32;
 *     INT32		offset B32;
 * } mpexSetElementPointerAtPickIdReq;
 * 
 * typedef struct
 * {
 *     CARD8		reqType;
 *     CARD8		opcode;
 *     CARD16		length B16;
 *     pexRenderer		rdr B32;
 *     INT16		patternNum B16;
 *     CARD16		pad B16;
 *     CARD32		pattern B32;
 * } mpexSetDashPatternReq;
 * 
 * typedef struct
 * {
 *     CARD8		reqType;
 *     CARD8		opcode;
 *     CARD16		length B16;
 *     pexRenderer		rdr B32;
 *     INT16		patternNum B16;
 *     CARD16		pad B16;
 * } mpexGetDashPatternReq;
 * 
 * typedef struct
 * {
 *     CARD8		reqType;
 *     CARD8		opcode;
 *     CARD16		length B16;
 *     pexRenderer		rdr B32;
 *     INT16		glyphNum B16;
 *     CARD16		pad B16;
 *     INT16		xOffset B16;
 *     INT16		yOffset B16;
 *     CARD16		glyph[16];
 * } mpexSetMarkerGlyphReq;
 * 
 * typedef struct
 * {
 *     CARD8		reqType;
 *     CARD8		opcode;
 *     CARD16		length B16;
 *     pexRenderer		rdr B32;
 *     INT16		glyphNum B16;
 *     CARD16		pad B16;
 * } mpexGetMarkerGlyphReq;
 * 
 * typedef struct
 * {
 *     CARD8		reqType;
 *     CARD8		opcode;
 *     CARD16		length B16;
 *     CARD16		sequenceNum B16;
 *     CARD16		totalNum B16;
 *     CARD32		totalLength B32;
 * } mpexLargeRequestReq;
 * 
 * typedef struct
 * {
 *     CARD8	reqType;
 *     CARD8	opcode;
 *     CARD16	length B16;
 *     pexRenderer	rdr B32;
 *     Drawable	drawable B32;
 *     CARD8	firstPass;
 *     BYTE	pad[3];
 * } mpexBeginTransparencyRenderingReq;
 * 
 * typedef struct
 * {
 *     CARD8	reqType;
 *     CARD8	opcode;
 *     CARD16	length B16;
 *     pexRenderer rdr B32;
 *     pexSwitch	flushFlag;
 *     BYTE	pad[3];
 * } mpexEndTransparencyRenderingReq;
 *)

(*****************************************************************
 * Output Commands
 *****************************************************************)
 
TYPE
  mpexAnnoPixmap = RECORD
    head   : pexElementInfo;
    origin : pexCoord3D;
    index  : pexTableIndex;
    pad    : CARD16;
  END;

  mpexArc = RECORD
    head       : pexElementInfo;
    center     : pexCoord3D;
    radius     : Ctypes.float;
    startAngle : Ctypes.float;
    endAngle   : Ctypes.float;
  END;

  mpexCircle = RECORD
    head   : pexElementInfo;
    center : pexCoord3D;
    radius : Ctypes.float;
  END;

  mpexEllipticalArc2D = RECORD
    head       : pexElementInfo;
    center     : BITS BITSIZE (pexCoord2D) FOR pexCoord2D;
    majorAxis  : BITS BITSIZE (pexCoord2D) FOR pexCoord2D;
    minorAxis  : BITS BITSIZE (pexCoord2D) FOR pexCoord2D;
    startAngle : Ctypes.float;
    endAngle   : Ctypes.float;
  END;

  mpexEllipticalArc3D = RECORD
    head       : pexElementInfo;
    center     : pexCoord3D;
    majorAxis  : pexCoord3D;
    minorAxis  : pexCoord3D;
    startAngle : Ctypes.float;
    endAngle   : Ctypes.float;
  END;

  mpexEllipse2D = RECORD
    head      : pexElementInfo;
    center    : BITS BITSIZE (pexCoord2D) FOR pexCoord2D;
    majorAxis : BITS BITSIZE (pexCoord2D) FOR pexCoord2D;
    minorAxis : BITS BITSIZE (pexCoord2D) FOR pexCoord2D;
   END;

   mpexEllipse3D = RECORD
    head      : pexElementInfo;
    center    : pexCoord3D;
    majorAxis : pexCoord3D;
    minorAxis : pexCoord3D;
   END;

   mpexIndexedPolygon = RECORD
     head          : pexElementInfo;
     shape         : CARD16;
     colourType    : pexColourType;
     facetAttribs  : pexBitmaskShort;
     vertexAttribs : pexBitmaskShort;
     edgeAttribs   : CARD16;
     numFacets     : CARD16;
     numEdges      : CARD16;
     numVertices   : CARD16;
     (*  LISTof CARD16(numFacets)                              *)
     (*  LISTof OptData(numFacets, facetAttribs, colourType)   *)
     (*  LISTof VertexIndex(numEdges, edgeAttribs)             *)
     (*  LISTof Vertex(numVertices, vertexAttribs, colourType) *)
   END;

  mpexGridRectangular = RECORD
    head   : pexElementInfo;
    origin : pexCoord3D;
    xdist  : Ctypes.float;
    ydist  : Ctypes.float;
  END;

  mpexGridRadial = RECORD
    head       : pexElementInfo;
    center     : pexCoord3D;
    startAngle : Ctypes.float;
    endAngle   : Ctypes.float;
    angDist    : Ctypes.float;
    radDist    : Ctypes.float;
  END;

  mpexEchoIndex = RECORD
    head  : pexElementInfo;
    index : pexTableIndex;
    pad   : CARD16;
  END;

  mpexHighlightIndex = RECORD
    head  : pexElementInfo;
    index : pexTableIndex;
    pad   : CARD16;
  END;

  mpexNoop = RECORD
    head : pexElementInfo;
  END;



(*
 * From MPEXprotostr.h
 *)

(* Definitions for Digital Pick/Echo/Z PEX extension likely to be used by 
   applications *)

(* Typedefs for lookup tables *)

TYPE
  mpexHighlightEntry = RECORD
    method : pexEnumTypeIndex;
    pad    : CARD16;
    colour : pexColourSpecifier;
  END;

  mpexEchoEntry = RECORD
    method : pexEnumTypeIndex;
    pad    : CARD16;
    colour : pexColourSpecifier;
  END;
    
  mpexPixmapEntry = RECORD
    pixmapId : CARD32;               (* used to be X.Pixmap *)
    xOffset  : INT16;
    yOffset  : INT16;
  END;


END PEX.
