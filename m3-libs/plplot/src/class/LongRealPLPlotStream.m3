MODULE LongRealPLPlotStream;

IMPORT LongRealPLPlot AS PL, LongRealPLPlotFrame AS Frame, Pathname;

REVEAL
  T = Public BRANDED OBJECT
        id    : CARDINAL;
        orient: LONGREAL;
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
    PL.StartPage();
    frame.draw();
    PL.StopPage();
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

PROCEDURE SetOrientation (SELF: T; orient: LONGREAL; ) =
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
    SELF.orient := 0.0D0;
    PL.Init();
    RETURN SELF;
  END Init;

PROCEDURE GenericInit (SELF: Generic; ): T =
  BEGIN
    SELF.id := PL.CreateStream();
    RETURN Init(SELF);
  END GenericInit;

PROCEDURE XWindowInit (SELF: XWindow; ): T =
  BEGIN
    SELF.id := PL.CreateStream();
    PL.SetDevice("xwin");
    RETURN Init(SELF);
  END XWindowInit;

PROCEDURE PostScriptInit
  (SELF: PostScript; filename: Pathname.T; colored: BOOLEAN; ): T =
  BEGIN
    SELF.id := PL.CreateStream();
    IF colored THEN PL.SetDevice("psc"); ELSE PL.SetDevice("ps"); END;
    PL.SetFileName(filename);
    RETURN Init(SELF);
  END PostScriptInit;


BEGIN
END LongRealPLPlotStream.
