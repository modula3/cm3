(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* Last modified on Sat Sep 30 12:06:45 PDT 1995 by mhb      *)
(*      modified on Sun Oct 24 13:50:54 PDT 1993 by sfreeman *)
(* modified on Mon Mar 15 12:59:16 PST 1993 by meehan *)
(* modified on Tue Jun 16 21:55:41 PDT 1992 by muller *)

(* This interface declares a type for each component in the
   language.  A client wishing to subclass the VBT used by a
   component should be sure that the VBT returned by the
   overrideVBT method is a subtype of type listed here. *)

INTERFACE FVTypes;

IMPORT AudioVBT, AnchorSplit, AnchorHelpSplit, BooleanVBT, BorderedVBT, ChoiceVBT,
       FileBrowserVBT, Filter, FlexVBT, Font, FormsVBT,
       GuardedBtnVBT, HVSplit, HighlightVBT, ListVBT,
       MenuSwitchVBT, NumericVBT, PackSplit, PaintOp, PixmapVBT,
       ProperSplit, ReactivityVBT, ScaleFilter, ScrollerVBT, Shadow,
       ShadowedVBT, ShadowedBarVBT, SourceVBT, 
       SplitterVBT, StableVBT, SwitchVBT, TSplit, TextEditVBT,
       TextPort, TextureVBT, TextVBT, TrillSwitchVBT, TypeinVBT,
       TypescriptVBT, VBT, VideoVBT, ViewportVBT, ZChassisVBT,
       ZGrowVBT, ZMoveVBT, ZChildVBT, ZTilps;
IMPORT StubImageVBT AS ImageVBT;

TYPE
  FVAny = VBT.Leaf;             (* just an alias *)
  FVAnyFilter = Filter.T;       (* just an alias *)
  FVAnySplit = ProperSplit.T;   (* just an alias *)

  FVAudio = AudioVBT.T BRANDED OBJECT END;
  FVBar = FlexVBT.T BRANDED OBJECT END;
  FVBoolean <: BooleanVBT.T;
  FVBorder = BorderedVBT.T BRANDED OBJECT END;
  FVBrowser =
    ListVBT.T BRANDED OBJECT END; (* requires a UniSelector *)
  FVButton <: SwitchVBT.T;
  FVChisel = ShadowedBarVBT.T BRANDED OBJECT END;
  FVChoice <: ChoiceVBT.T;
  FVCloseButton <: PublicCloseButton;
  FVDirMenu = FileBrowserVBT.DirMenu BRANDED OBJECT END;
  FVFileBrowser <: FileBrowserVBT.T;
  FVFill = FlexVBT.T BRANDED OBJECT END;
  FVFilter = ReactivityVBT.T BRANDED OBJECT END;
  FVFrame = ShadowedVBT.T BRANDED OBJECT END;
  FVGeneric = FlexVBT.T BRANDED OBJECT END;
  FVGlue = FlexVBT.T BRANDED OBJECT END;
  FVGuard <: GuardedBtnVBT.T;
  FVHBox <: HVSplit.T;
  FVHPackSplit = PackSplit.T;
  FVHTile <: SplitterVBT.T;
  FVHelp <: AnchorHelpSplit.T;
  FVHelper = FileBrowserVBT.Helper BRANDED OBJECT END;
  FVImage <: ImageVBT.T;
  FVIntApply <: IntApplyPublic;
  FVLinkButton <: SwitchVBT.T;
  FVLinkMButton <: MenuSwitchVBT.T;
  FVMButton <: MenuSwitchVBT.T;
  FVMenu <: AnchorSplit.T;
  FVMultiBrowser =
    ListVBT.T BRANDED OBJECT END; (* requires a MultiSelector *)
  FVNumeric <: NumericVBT.T;
  FVPageButton <: PublicPageButton;
  FVPageMButton <: PublicPageMButton;
  FVPixmap = PixmapVBT.T BRANDED OBJECT END;
  FVPopButton <: SwitchVBT.T;
  FVPopMButton <: MenuSwitchVBT.T;
  FVRadio = PublicRadio;
  FVRidge = ShadowedBarVBT.T BRANDED OBJECT END;
  FVRim = BorderedVBT.T BRANDED OBJECT END;
  FVScale = ScaleFilter.T BRANDED OBJECT END;
  FVScroller <: ScrollerVBT.T;
  FVShape = FlexVBT.T BRANDED OBJECT END;
  FVSource <: SourceVBT.T;
  FVStable = StableVBT.T BRANDED OBJECT END;
  FVTSplit = PublicTSplit;
  FVTarget = Filter.T BRANDED OBJECT END;
  FVText = TextVBT.T BRANDED OBJECT END;
  FVTextEdit =
    TextEditVBT.T BRANDED OBJECT END; (* requires a Port *)
  FVTexture = TextureVBT.T BRANDED OBJECT END;
  FVTrillButton <: TrillSwitchVBT.T;
  FVTypeIn <: TypeinVBT.T;
  FVTypescript = TypescriptVBT.T BRANDED OBJECT END;
  FVVBox <: HVSplit.T;
  FVVTile <: SplitterVBT.T;
  FVVideo = VideoVBT.T BRANDED OBJECT END;
  FVViewport = ViewportVBT.T BRANDED OBJECT END;
  FVZBackground = HighlightVBT.T BRANDED OBJECT END;
  FVZChassis <: ZChassisVBT.T;
  FVZChild = ZChildVBT.T BRANDED OBJECT END;
  FVZGrow = ZGrowVBT.T BRANDED OBJECT END;
  FVZMove = ZMoveVBT.T BRANDED OBJECT END;
  FVZSplit = ZTilps.T BRANDED OBJECT END;

TYPE UniSelector <: ListVBT.UniSelector;
(* If you create a subtype of "FVBrowser", its ".selector" field
   must be "NIL" or a subtype of "FVTypes.UniSelector".*)

TYPE MultiSelector <: ListVBT.MultiSelector;
(* If you create a subtype of "FVBrowser", its ".selector" field
   must be "NIL" or a subtype of "FVTypes.MultiSelector". *)

TYPE
  Port <: PublicPort;
  PublicPort =
    TextPort.T OBJECT
    METHODS
      init (textedit      : FVTextEdit;
            reportKeys    : BOOLEAN;
            font          : Font.T;
            colorScheme   : PaintOp.ColorScheme;
            wrap, readOnly: BOOLEAN;
            turnMargin    : REAL                 ): Port;
    END;
(* If you create a subtype of "FVTextEdit", its ".tp" field must
   be "NIL" or a subtype of "FVTypes.Port". *)

TYPE
  PublicCloseButton =
    SwitchVBT.T OBJECT
    METHODS
      init (ch: VBT.T; shadow: Shadow.T): FVCloseButton
    END;

  PublicPageButton = SwitchVBT.T OBJECT
                     METHODS
                       init (ch       : VBT.T;
                             shadow   : Shadow.T;
                             backwards: BOOLEAN;
                             tsplit   : FVTSplit  ): FVPageButton
                     END;

  PublicPageMButton =
    MenuSwitchVBT.T OBJECT
    METHODS
      init (ch       : VBT.T;
            shadow   : Shadow.T;
            backwards: BOOLEAN;
            tsplit   : FVTSplit  ): FVPageMButton
    END;

  PublicRadio = Filter.T OBJECT radio: ChoiceVBT.Group END;

  PublicTSplit = TSplit.T OBJECT circular := FALSE END;

  IntApplyPublic =
    Filter.T OBJECT
    METHODS
      init (fv      : VBT.T;
            ch      : VBT.T;
            name    : TEXT;
            property: TEXT    := NIL): FVIntApply
            RAISES {FormsVBT.Error};
      (* raises an error if NOT ISTYPE(fv, FormsVBT.T) OR
         NOT(ISTYPE(ch, FVNumeric) OR ISTYPE(ch, FVScroller)) *)
    END;

END FVTypes.
