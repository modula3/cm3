(* Copyright (C) 1992, Digital Equipment Corporation                     *)
(* All rights reserved.                                                  *)
(* See the file COPYRIGHT for a full description.                        *)
(*                                                                       *)
(* Last modified on Wed Nov  1 12:36:19 PST 1995 by mhb                  *)
(*      modified on Thu Jun  3 17:33:30 PDT 1993 by meehan               *)
(*      modified on Tue Jun 16 21:55:42 PDT 1992 by muller               *)

INTERFACE FVRuntime;

(* This is the (private) interface for FormsVBT. *)

IMPORT Atom, Axis, ButtonVBT, ChoiceVBT, Color, FlexVBT, Font,
       FormsVBT, FVTypes, HVSplit, ListVBT, PaintOp,
       Pixmap, Rd, RefList, Rsrc, Shadow, Sx, VBT, ZChildVBT, ZSplit;
IMPORT StubImageVBT AS ImageVBT;

TYPE
  Private <: SemiPublic;
  SemiPublic =
    FormsVBT.Public OBJECT
      path     : Rsrc.Path;
      baseURL  : TEXT := NIL; (* if non-NIL, use this rather than Rsrc path *)
      formstack: RefList.T   := NIL
      (* The current (parse-time) sub-form.  For reporting errors. *)
    END;
  Millimeters = REAL;
  Points = REAL;

CONST
  EMPTYSHAPE = FlexVBT.Shape {
                 FlexVBT.SizeRange {0.0, 0.0, 0.0},
                 FlexVBT.SizeRange {0.0, 0.0, 0.0}};
  DefaultShadowSizePts: Points = 1.5;
  DefaultShadowSize: Millimeters = DefaultShadowSizePts * 25.4 / 72.0;

PROCEDURE SetFirstFocus (widget: VBT.T); 
PROCEDURE SetPopTarget (source: ButtonVBT.T; target: ZChildVBT.T);
PROCEDURE SetLinkTarget (source: ButtonVBT.T; target: VBT.T);
PROCEDURE SetPageTarget (source: ButtonVBT.T; target: FVTypes.FVTSplit);


PROCEDURE SetVBT (fv: FormsVBT.T; name: TEXT; vbt: VBT.T)
  RAISES {FormsVBT.Error};


PROCEDURE Open (name: TEXT; path: Rsrc.Path; baseURL: TEXT): Rd.T
  RAISES {FormsVBT.Error};
(* If baseURL # NIL, then try to open "name" as a URL, relative
   to baseURL. If not Otherwise, open "name" as a Rsrc, passing in 
   path. *)


TYPE
  State = RECORD
            (* The inherited properties: *)
            bgOp, fgOp, darkOp, lightOp: PaintOp.T;

            bgRGB    := Color.T {0.8, 0.8, 0.8};
            fgRGB    := Color.T {0.0, 0.0, 0.0};
            darkRGB  := Color.T {0.333, 0.333, 0.333};
            lightRGB := Color.T {1.0, 1.0, 1.0};

            fontName, labelFontName      : TEXT;
            font, labelFont              : Font.T;
            fontMetrics, labelFontMetrics: RefList.T;

            shadow  : Shadow.T;
            shadowSz: Millimeters := DefaultShadowSize;

            (* The various "scoping" properties: *)
            glueAxis: Axis.T            := Axis.T.Hor;
            hvsplit : HVSplit.T         := NIL;
            macros  : RefList.T            := NIL;
            menubar : VBT.T             := NIL;
            radio   : FVTypes.FVRadio   := NIL;
            tsplit  : FVTypes.FVTSplit  := NIL;
            zchild  : ZChildVBT.T       := NIL;
            zsplit  : ZSplit.T          := NIL;

            (* Per component info: *)
            name: TEXT := NIL;
          END;

VAR DefaultFontMetrics, DefaultLabelFontMetrics: RefList.T; (* CONST *)

PROCEDURE InitParser ();

PROCEDURE Parse (         t          : FormsVBT.T;
                          description: Sx.T;
                 READONLY state      : State           ): VBT.T
  RAISES {FormsVBT.Error};

PROCEDURE NamedVBTs (t: FormsVBT.T): RefList.T;
(* Returns an alist of names and VBTs, sorted by name. *)

PROCEDURE GetAttachments (fv: FormsVBT.T): RefList.T;
PROCEDURE SetAttachments (fv: FormsVBT.T; alist: RefList.T)
  RAISES {FormsVBT.Error};

(* These routines get and set the procedures that are attached to "fv"
   via Attach.  "alist" is an association list of names and [internal]
   refs that contain the actual procedures.  This is used by
   FormsEditVBT to copy the attachments from one FormsVBT to its
   replacement.

   SetAttachments will raise Error if an attachment fails, probably because
   "fv" does not contain a named VBT for every name in the alist. *)

PROCEDURE FindFont (fontname: TEXT): Font.T;
 (* This maintains a cache of fonts, indexed by names. *)

PROCEDURE MetricsToName (metrics: RefList.T): TEXT;
(* Convert a metrics-list into a font-name. *)

REVEAL FVTypes.FVImage <: PrivateImage;
TYPE 
  PrivateImage = ImageVBT.T OBJECT
    op: PaintOp.T; (* to paint the image *)
    bg: PaintOp.T; (* to paint empty space in VBT *)
    gamma: REAL; (* gamma correction *)
    rd: Rd.T; (* reader on the ppm file *)
  END;

PROCEDURE GetPixmap (name: TEXT; path: Rsrc.Path; baseURL: TEXT): Pixmap.T
   RAISES {FormsVBT.Error};

REVEAL FVTypes.UniSelector <: PrivateUniSelector;
TYPE
  PrivateUniSelector = ListVBT.UniSelector OBJECT
                         quick  : BOOLEAN;
                         browser: FVTypes.FVBrowser
                       END;

REVEAL FVTypes.MultiSelector <: PrivateMultiSelector;
TYPE
  PrivateMultiSelector = ListVBT.MultiSelector OBJECT
                           quick  : BOOLEAN;
                           browser: FVTypes.FVMultiBrowser
                         END;

REVEAL FVTypes.FVCloseButton <: PrivateCloseButton;
TYPE
  PrivateCloseButton =
    FVTypes.PublicCloseButton OBJECT target: ZChildVBT.T END;

REVEAL FVTypes.FVChoice <: PrivateChoice;
TYPE
  PrivateChoice = ChoiceVBT.T OBJECT
                    radio: FVTypes.FVRadio;
                    name : TEXT
                  END;
                  
VAR
  FVSyntax: Sx.Syntax; (* The syntax used by the parser. *)

  qBOA, qName, qValue, qBackquote, qComma, qCommaAtsign, qQuote: Atom.T;
  (* Symbols needed by the parser and FormsEdit. *)

PROCEDURE ToText (x        : REFANY;
                  maxDepth : CARDINAL := LAST (CARDINAL);
                  maxLength: CARDINAL := LAST (CARDINAL)  ): TEXT;

END FVRuntime.
