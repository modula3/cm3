MODULE Numeric EXPORTS Main;
IMPORT Trestle, VBT, TextVBT, NumericVBT, Font, Shadow, AnyEvent;
IMPORT HVSplit, HVBar, Axis, ButtonVBT, TextPort;
FROM Colors IMPORT royal, white, lgrey, dgrey;

PROCEDURE Callback (v: NumericVBT.T; event: AnyEvent.T) =
  BEGIN
    TYPECASE event OF
    | AnyEvent.Mouse => TextVBT.Put(display, "Mouse clicked");
    | AnyEvent.Key => TextVBT.Put(display, "Return pressed");
    END;
  END Callback;

PROCEDURE SetEmpty (v: ButtonVBT.T; READONLY cd: VBT.MouseRec) =
  BEGIN
    IF NOT NumericVBT.IsEmpty(numeric) THEN
      NumericVBT.SetEmpty(numeric);
    END;
  END SetEmpty;

PROCEDURE SetMax (v: ButtonVBT.T; READONLY cd: VBT.MouseRec) =
  BEGIN
    IF NOT NumericVBT.IsEmpty(numeric) THEN
      NumericVBT.PutMax(numeric, NumericVBT.Get(numeric));
    END;
  END SetMax;

PROCEDURE SetMin (v: ButtonVBT.T; READONLY cd: VBT.MouseRec) =
  BEGIN
    IF NOT NumericVBT.IsEmpty(numeric) THEN
      NumericVBT.PutMin(numeric, NumericVBT.Get(numeric));
    END;
  END SetMin;

CONST times14 = "-*-times-*-r-*-*-14-*-*-*-*-*-*-*";
VAR
  font := Font.FromName(ARRAY OF TEXT{times14});
  sh   := Shadow.New(3.0, royal, white, lgrey, dgrey);
  numeric := NEW(NumericVBT.T, callback := Callback).init(
               min := FIRST(INTEGER), max := LAST(INTEGER),
               allowEmpty := TRUE, naked := FALSE, font := font,
               shadow := sh);
  display  := TextVBT.New("");
  vsplit   := HVSplit.Cons(Axis.T.Ver, numeric, HVBar.New(), display);
  setempty := ButtonVBT.New(TextVBT.New("Set Empty"), SetEmpty);
  setmax   := ButtonVBT.New(TextVBT.New("Set Max"), SetMax);
  setmin   := ButtonVBT.New(TextVBT.New("Set Min"), SetMin);
  buttons := HVSplit.Cons(Axis.T.Ver, setmax, HVBar.New(), setmin,
                          HVBar.New(), setempty);
  main := HVSplit.Cons(Axis.T.Hor, vsplit, HVBar.New(), buttons);
BEGIN
  Trestle.Install(main);
  Trestle.AwaitDelete(main)
END Numeric.
