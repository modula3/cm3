MODULE Source EXPORTS Main;
IMPORT Trestle, VBT, TextVBT, ShadowedFeedbackVBT, RigidVBT, SourceVBT;
IMPORT HVSplit, Shadow, FeedbackVBT, Axis, Random, Fmt;
FROM Colors IMPORT lblue, dblue, lred, dred;
TYPE
  Source = SourceVBT.T OBJECT
           METHODS
             init (f: FeedbackVBT.T; t1, t2: VBT.T): Source := Init
           OVERRIDES
             hit      := SourceVBT.AlwaysHit;
             callback := Callback;
           END;
PROCEDURE Init (v: Source; f: FeedBackVBT.T; t1, t2: VBT.T): Source =
  BEGIN
    SourceVBT.BeTarget(t1, SourceVBT.NewTarget());
    SourceVBT.BeTarget(t2, SourceVBT.NewTarget());
    EVAL SourceVBT.T.init(v, f);
    RETURN v;
  END Init;
PROCEDURE Callback (v: Source; READONLY cd: VBT.MouseRec) =
  VAR t := SourceVBT.GetTarget(v);
  BEGIN
    IF (t # NIL) THEN
      IF t = target1 THEN
        TextVBT.Put(slot1, TextVBT.Get(number));
      ELSE                       (* t = target2 *)
        TextVBT.Put(slot2, TextVBT.Get(number));
      END;
      TextVBT.Put(number, Fmt.Int(Random.Subrange(seed, 0, 100)));
    END;
  END Callback;
PROCEDURE New (t: TextVBT.T; s: Shadow.T): ShadowedFeedbackVBT.T =
  VAR r := RigidVBT.FromHV(t, hMin := 15.0, vMin := 10.0);
  BEGIN
    RETURN (NEW(ShadowedFeedbackVBT.T).init(r, s));
  END New;
VAR
  seed    := Random.New(-1);
  number  := TextVBT.New(Fmt.Int(Random.Subrange(seed, 0, 100)));
  slot1   := TextVBT.New("");
  slot2   := TextVBT.New("");
  red_sh  := Shadow.New(5.0, light := lred, dark := dred);
  blue_sh := Shadow.New(5.0, light := lblue, dark := dblue);
  target1 := New(slot1, blue_sh);
  target2 := New(slot2, blue_sh);
  source  := NEW(Source).init(New(number, red_sh), target1, target2);
  vbt     := RigidVBT.FromHV(TextVBT.New("Drag and Drop"), 25.0, 15.0);
  hsplit  := HVSplit.Cons(Axis.T.Hor, target1, target2);
  main    := HVSplit.Cons(Axis.T.Ver, source, vbt, hsplit);
BEGIN
  Trestle.Install(main);
  Trestle.AwaitDelete(main);
END Source.
