%module QtNamespace

%include "m3qt.i"
%include "common.i"

%{
#include "qnamespace.h"
%}

%include "namespace_const.i"

%insert(m3wrapimpl) %{
IMPORT WeakRef;
IMPORT Ctypes AS C;
%}

%insert(m3wrapintf) %{

TYPE

  GlobalColor = {
        color0,
        color1,
        black,
        white,
        darkGray,
        gray,
        lightGray,
        red,
        green,
        blue,
        cyan,
        magenta,
        yellow,
        darkRed,
        darkGreen,
        darkBlue,
        darkCyan,
        darkMagenta,
        darkYellow,
        transparent
    };

    SortOrder = {
        AscendingOrder,
        DescendingOrder
    };

    TileRule = {
        StretchTile,
        RepeatTile,
        RoundTile
    };

    TextElideMode = {
        ElideLeft,
        ElideRight,
        ElideMiddle,
        ElideNone
    };

    BGMode = {
        TransparentMode,
        OpaqueMode
    };

    ArrowType = {
        NoArrow,
        UpArrow,
        DownArrow,
        LeftArrow,
        RightArrow
    };

    PenStyle = {
        NoPen,
        SolidLine,
        DashLine,
        DotLine,
        DashDotLine,
        DashDotDotLine,
        CustomDashLine
    };

    SizeMode = {
        AbsoluteSize,
        RelativeSize
    };

    UIEffect = {
        UI_General,
        UI_AnimateMenu,
        UI_FadeMenu,
        UI_AnimateCombo,
        UI_AnimateTooltip,
        UI_FadeTooltip,
        UI_AnimateToolBox
    };

    TextFormat = {
        PlainText,
        RichText,
        AutoText,
        LogText
    };

    AspectRatioMode = {
        IgnoreAspectRatio,
        KeepAspectRatio,
        KeepAspectRatioByExpanding
    };

    TimeSpec = {
        LocalTime,
        UTC,
        OffsetFromUTC
    };

    ScrollBarPolicy = {
        ScrollBarAsNeeded,
        ScrollBarAlwaysOff,
        ScrollBarAlwaysOn
    };

    CaseSensitivity = {
        CaseInsensitive,
        CaseSensitive
    };

    ShortcutContext = {
        WidgetShortcut,
        WindowShortcut,
        ApplicationShortcut,
        WidgetWithChildrenShortcut
    };

    FillRule = {
        OddEvenFill,
        WindingFill
    };

    MaskMode = {
        MaskInColor,
        MaskOutColor
    };

    ClipOperation = {
        NoClip,
        ReplaceClip,
        IntersectClip,
        UniteClip
    };

    TransformationMode = {
        FastTransformation,
        SmoothTransformation
    };

    Axis = {
        XAxis,
        YAxis,
        ZAxis
    };

    FocusReason = {
        MouseFocusReason,
        TabFocusReason,
        BacktabFocusReason,
        ActiveWindowFocusReason,
        PopupFocusReason,
        ShortcutFocusReason,
        MenuBarFocusReason,
        OtherFocusReason,
        NoFocusReason
    };

    ContextMenuPolicy = {
        NoContextMenu,
        DefaultContextMenu,
        ActionsContextMenu,
        CustomContextMenu,
        PreventContextMenu
    };

    InputMethodQuery = {
        ImMicroFocus,
        ImFont,
        ImCursorPosition,
        ImSurroundingText,
        ImCurrentSelection,
        ImMaximumTextLength,
        ImAnchorPosition
    };

    ToolButtonStyle = {
        ToolButtonIconOnly,
        ToolButtonTextOnly,
        ToolButtonTextBesideIcon,
        ToolButtonTextUnderIcon,
        ToolButtonFollowStyle
    };

    LayoutDirection = {
        LeftToRight,
        RightToLeft
    };

    AnchorPoint = {
        AnchorLeft,
        AnchorHorizontalCenter,
        AnchorRight,
        AnchorTop,
        AnchorVerticalCenter,
        AnchorBottom
    };

    CheckState = {
        Unchecked,
        PartiallyChecked,
        Checked
    };

    WindowModality = {
        NonModal,
        WindowModal,
        ApplicationModal
    };

    SizeHint = {
        MinimumSize,
        PreferredSize,
        MaximumSize,
        MinimumDescent,
        NSizeHints
    };

    WindowFrameSection = {
        NoSection,
        LeftSection,
        TopLeftSection,
        TopSection,
        TopRightSection,
        RightSection,
        BottomRightSection,
        BottomSection,
        BottomLeftSection,
        TitleBarArea
    };

    Initialization = {
        Uninitialized
    };

    CoordinateSystem = {
        DeviceCoordinates,
        LogicalCoordinates
    };

    NavigationMode = {
        NavigationModeNone,
        NavigationModeKeypadTabOrder,
        NavigationModeKeypadDirectional,
        NavigationModeCursorAuto,
        NavigationModeCursorForceVisible
    };

  Corner = {
     TopLeftCorner,
     TopRightCorner,
     BottomLeftCorner,
     BottomRightCorner
  };

(* end of propert enumerations *)

CONST

  ConnectionValid = ConnectionSet {
        AutoConnection,
        DirectConnection,
        QueuedConnection,
        AutoCompatConnection,
        BlockingQueuedConnection,
        UniqueConnection};

  MouseButtonValid = MouseButtonSet {
        NoButton,
        LeftButton,
        RightButton,
        MidButton,
        XButton1,
        XButton2,
        MouseButtonMask
    };

  AlignmentValid = AlignmentSet {
        AlignLeft,
        AlignLeading,
        AlignRight,
        AlignTrailing,
        AlignCenter,
        AlignJustify,
        AlignAbsolute,
        AlignHorizontal_Mask,
        AlignTop,
        AlignBottom,
        AlignCenter,
        AlignVertical_Mask,
        AlignCenter
  };


TYPE

  ConnectionType = [AutoConnection..UniqueConnection];
  ConnectionSet = SET OF ConnectionType;

  KeyboardModifierType = [NoModifier..GroupSwitchModifier]; (* cannot use set vals too large *)
  ModifierType = [SHIFT..META]; (* cannot use set vals too large *)

  MouseButtonType = [NoButton..MouseButtonMask];
  MouseButtonSet = SET OF MouseButtonType;

  Alignment = [AlignLeft..AlignVertical_Mask];
  AlignmentSet = SET OF Alignment;

  TextInteractionFlags = [NoTextInteraction..TextEditorInteraction];
  TextInteractionSet = SET OF TextInteractionFlags;

(*fixme another flag greater but negative *)
  WindowFlags = [Widget..WindowSoftkeysVisibleHint ];
  WindowFlagsSet = SET OF WindowFlags;

(*fixme another flag greater but negative *)
  InputMethodHints = [ImhNone..ImhUrlCharactersOnly];
  InputMethodHintSet = SET OF InputMethodHints;

  Orientation = [Horizontal..Vertical];
  OrientationSet = SET OF Orientation;

  DropAction = [IgnoreAction..TargetMoveAction];
  DropActionSet = SET OF DropAction;

  ItemFlag = [NoItemFlags..ItemIsTristate];
  ItemFlagSet = SET OF ItemFlag;
  ItemFlags = ItemFlagSet;

  ToolBarArea = [NoDockWidgetArea..DockWidgetArea_Mask];
  ToolBarAreaSet = SET OF ToolBarArea;
  ToolBarAreas = CARDINAL; (*ToolBarAreaSet;*)

  DockWidgetArea = [NoToolBarArea..ToolBarArea_Mask];
  DockWidgetAreaSet = SET OF DockWidgetArea;
  DockWidgetAreas = CARDINAL; (*DockWidgetAreaSet;*)

  WindowState = [WindowNoState..WindowActive ];
  WindowStateSet = SET OF WindowState;
  WindowStates = WindowStateSet;

%}

%ignore QInternal;

//ignore the functions
%ignore registerCallback;
%ignore unregisterCallback;
%ignore activateCallbacks;
%ignore callFunction;

//selectively ignore some enums until we need them


%ignore GlobalColor;
//%ignore KeyboardModifier;
//%ignore Modifier;
//%ignore MouseButton;
//%ignore Orientation;
%ignore FocusPolicy;
%ignore SortOrder;
%ignore TileRule;
//%ignore AlignmentFlag;
%ignore TextFlag;
%ignore TextElideMode;
//%ignore WindowType;
//%ignore WindowState;
%ignore WidgetAttribute;
%ignore ApplicationAttribute;
%ignore ImageConversionFlag;
%ignore BGMode;
%ignore Key;;
%ignore ArrowType;
%ignore PenStyle;
%ignore PenCapStyle;
%ignore PenJoinStyle;
%ignore BrushStyle;
%ignore SizeMode;
%ignore UIEffect;
%ignore CursorShape;
%ignore TextFormat;
%ignore AspectRatioMode;
%ignore AnchorAttribute;
//%ignore DockWidgetArea;
%ignore DockWidgetAreaSizes;
//%ignore ToolBarArea;
%ignore ToolBarAreaSizes;
%ignore DateFormat;
%ignore TimeSpec;
%ignore DayOfWeek;
%ignore ScrollBarPolicy;
%ignore CaseSensitivity;
//%ignore Corner;
//%ignore ConnectionType;
%ignore ShortcutContext;
%ignore FillRule;
%ignore MaskMode;
%ignore ClipOperation;
%ignore ItemSelectionMode;
%ignore TransformationMode;
%ignore Axis;
%ignore FocusReason;
%ignore ContextMenuPolicy;
%ignore InputMethodQuery;
//%ignore InputMethodHint;
%ignore ToolButtonStyle;
%ignore LayoutDirection;
%ignore AnchorPoint;
//%ignore DropAction;
%ignore CheckState;
%ignore ItemDataRole;
//%ignore ItemFlag;
%ignore MatchFlag;
%ignore WindowModality;
//%ignore TextInteractionFlag;
%ignore EventPriority;
%ignore SizeHint;
%ignore WindowFrameSection;
%ignore Initialization;
%ignore CoordinateSystem;
%ignore TouchPointState;
%ignore GestureState;
%ignore GestureType;
%ignore GestureFlag;
%ignore NavigationMode;
%ignore PaintDeviceFlags;
%ignore RelayoutType;
%ignore Callback;
%ignore InternalFunction;
%ignore DockPosition;


%pragma(modula3) constint="prefix=Qt::,enum=Qt::Qt::ConnectionType;int;srcstyle=orig,prefix=;";
%pragma(modula3) constint="prefix=Qt::,enum=Qt::Qt::KeyboardModifier;int;srcstyle=orig,prefix=;";
%pragma(modula3) constint="prefix=Qt::,enum=Qt::Qt::Modifier;int;srcstyle=orig,prefix=;";
%pragma(modula3) constint="prefix=Qt::,enum=Qt::Qt::MouseButton;int;srcstyle=orig,prefix=;";

%pragma(modula3) constint="prefix=Qt::,enum=Qt::Qt::ImageConversionFlag;int;srcstyle=underscore,prefix=;";

%pragma(modula3) constint="prefix=Qt::,enum=Qt::Qt::ApplicationAttribute;int;srcstyle=underscore,prefix=;";

%pragma(modula3) constint="prefix=Qt::,enum=Qt::Qt::WidgetAttribute;int;srcstyle=underscore,prefix=;";

%pragma(modula3) constint="prefix=Qt::,enum=Qt::Qt::AlignmentFlag;int;srcstyle=orig,prefix=;";

%pragma(modula3) constint="prefix=Qt::,enum=Qt::Qt::TextInteractionFlag;int;srcstyle=orig,prefix=;";

%pragma(modula3) constint="prefix=Qt::,enum=Qt::Qt::WindowType;int;srcstyle=orig,prefix=;";

%pragma(modula3) constint="prefix=Qt::,enum=Qt::Qt::Corner;int;srcstyle=orig,prefix=;";

%pragma(modula3) constint="prefix=Qt::,enum=Qt::Qt::InputMethodHint;int;srcstyle=orig,prefix=;";

%pragma(modula3) constint="prefix=Qt::,enum=Qt::Qt::Orientation;int;srcstyle=orig,prefix=;";

%pragma(modula3) constint="prefix=Qt::,enum=Qt::Qt::DropAction;int;srcstyle=orig,prefix=;";

%pragma(modula3) constint="prefix=Qt::,enum=Qt::Qt::ItemFlag;int;srcstyle=orig,prefix=;";

%pragma(modula3) constint="prefix=Qt::,enum=Qt::Qt::ToolBarArea;int;srcstyle=orig,prefix=;";

%pragma(modula3) constint="prefix=Qt::,enum=Qt::Qt::DockWidgetArea;int;srcstyle=orig,prefix=;";

%pragma(modula3) constint="prefix=Qt::,enum=Qt::Qt::WindowState;int;srcstyle=orig,prefix=;";


//%pragma(modula3) //constint="prefix=Qt::,enum=Qt::Qt::WindowType;int;srcstyle=underscore,prefix=;";

//%pragma(modula3) constint="prefix=Qt::,enum=Qt::Qt::Key;int;srcstyle=underscore,prefix=;";

//raw for QPoint
%typemap("m3rawrettype")   QPoint     %{ADDRESS%}
%typemap("m3rawrettype")   QPointF    %{ADDRESS%}


%include "qnamespace.h"

/*
INTERFACE QtNamespace;

CONST
  AutoConnection = 0;
  DirectConnection = 1;
  QueuedConnection = 2;
  AutoCompatConnection = 3;
  BlockingQueuedConnection = 4;
  UniqueConnection = 16_80;

  ConnectionValid = ConnectionSet{AutoConnection,
        DirectConnection,
        QueuedConnection,
        AutoCompatConnection,
        BlockingQueuedConnection,
        UniqueConnection};

TYPE

  AspectRatioMode = {IgnoreAspectRatio,
                     KeepAspectRatio,
                     KeepAspectRatioByExpanding};

  (* needs constants so cant use this type*)
  ConnectionOrigType = {
        AutoConnection,
        DirectConnection,
        QueuedConnection,
        AutoCompatConnection,
        BlockingQueuedConnection,
        UniqueConnection(* =  0x80*)};

  Initialization = {Uninitialized};

CONST
  AA = -2;
  BB = 3;
  CC = 8;
  clEnum = TestEnumSet{AA,BB,CC};

TYPE

  TestEnum = [AA..CC];
  TestEnumSet = SET OF TestEnum;
(*
In a wrapped proc have a typemap that does this

  IF parm IN clEnum THEN (* OK *) ELSE RAISE someexception END;
  or
  VAR tmp : TestEnumSet{parm}; which will raise checked subrange error if parm is not in set

*)

  ConnectionType = [AutoConnection..UniqueConnection];
  ConnectionSet = SET OF ConnectionType;

END QtNamespace.



building this

  466  ../../../../src/swig-2.0.2/swig -I../../../../src/swig-2.0.2/Lib -I../../../../src/swig-2.0.2/Lib/modula3 -generateconst temp.const -c++ -modula3 Qtnamespace.i
  467  ../../../../src/swig-2.0.2/swig -I../../../../src/swig-2.0.2/Lib -I../../../../src/swig-2.0.2/Lib/modula3 -generateconst temp.const -c++ -modula3 QtNamespace.i
  468  vi temp.const.c
  469  g++ -Wall temp.const.c -o temp.const
  470  vi temp.const.c
  471  g++ -Wall temp.const.c -o temp.const
  472  ../../../../src/swig-2.0.2/swig -I../../../../src/swig-2.0.2/Lib -I../../../../src/swig-2.0.2/Lib/modula3 -generateconst temp.const -c++ -modula3 QtNamespace.i
  473  vi temp.const.c
  474  g++ -Wall -I/usr/include/qt4 -I/usr/include/qt4/QtCore temp.const.c -o temp.const
  475  vi temp.const.c
  476  g++ -Wall -I/usr/include/qt4 -I/usr/include/qt4/QtCore temp.const.c -o temp.const
  477  ./temp.const
  478  ./temp.const > temp_const.i
  479  ./mi QtNamespace.i
  480  mv temp_const.i namespace_const.i
  481  ./mi QtNamespace.i
  482  grep enum qnamespace.h
  483  ./mi QtNamespace.i
*/
