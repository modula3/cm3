MODULE LongRealPLPlotStream;

IMPORT LongRealPLPlot AS PL, LongRealPLPlotFrame AS Frame, Pathname;

REVEAL
  T = Public BRANDED OBJECT
        id    : CARDINAL;
        orient: [0 .. 3];
      OVERRIDES
        put                   := Put;
        setPenWidth           := SetPenWidth;
        setCharacterRelHeight := SetCharacterRelHeight;
        setOrientation        := SetOrientation;
        exit                  := Exit;
      END;

  Generic =
    GenericPublic BRANDED OBJECT OVERRIDES init := GenericInit; END;

  XWindow =
    XWindowPublic BRANDED OBJECT OVERRIDES init := XWindowInit; END;

  PostScript =
    PostScriptPublic BRANDED OBJECT OVERRIDES init := PostScriptInit; END;



PROCEDURE Put (SELF: T; frame: Frame.T; ) =
  BEGIN
    PL.SetStream(SELF.id);
    PL.SetOrientation(SELF.orient);
    PL.AdvanceSubPage();
    (* PL.StartPage(); *)
    frame.draw(0.0D0, 0.0D0);
    (* PL.StopPage(); *)
  END Put;

PROCEDURE SetPenWidth (SELF: T; width: CARDINAL; ) =
  BEGIN
    PL.SetStream(SELF.id);
    PL.SetPenWidth(width);
  END SetPenWidth;

PROCEDURE SetCharacterRelHeight (SELF: T; relHeight: LONGREAL; ) =
  BEGIN
    PL.SetStream(SELF.id);
    PL.SetCharacterHeight(0.0D0, relHeight);
  END SetCharacterRelHeight;

PROCEDURE SetOrientation (SELF: T; orient: [0 .. 3]; ) =
  BEGIN
    SELF.orient := orient;
  END SetOrientation;


PROCEDURE Exit (SELF: T; ) =
  BEGIN
    PL.SetStream(SELF.id);
    PL.Exit();
  END Exit;

PROCEDURE Init (SELF: T; ): T =
  BEGIN
    SELF.orient := 0;
    PL.Init();
    RETURN SELF;
  END Init;

PROCEDURE GenericInit (SELF: Generic; ): Generic =
  BEGIN
    SELF.id := PL.CreateStream();
    RETURN Init(SELF);
  END GenericInit;

PROCEDURE XWindowInit (SELF: XWindow; ): XWindow =
  BEGIN
    SELF.id := PL.CreateStream();
    PL.SetDevice("xwin");
    RETURN Init(SELF);
  END XWindowInit;

PROCEDURE PostScriptInit
  (SELF: PostScript; filename: Pathname.T; colored, portrait: BOOLEAN; ):
  PostScript =
  BEGIN
    SELF.id := PL.CreateStream();
    IF colored THEN PL.SetDevice("psc"); ELSE PL.SetDevice("ps"); END;
    PL.SetFileName(filename);
    IF portrait THEN EVAL PL.SetOption("-portrait", ""); END;
    RETURN Init(SELF);
  END PostScriptInit;


BEGIN
END LongRealPLPlotStream.
