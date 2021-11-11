MODULE Key EXPORTS Main;
IMPORT Trestle, VBT, TextVBT, BorderedVBT, RigidVBT, Font, Rect;
IMPORT Fmt, Wr, Stdio, Latin1Key;

TYPE
  KeyVBT = TextVBT.T OBJECT
           METHODS
             init (t: TEXT; f := Font.BuiltIn): KeyVBT := Init
           OVERRIDES
             position := Position;
             key      := Key;
           END;

PROCEDURE Init (v: KeyVBT; t: TEXT; f := Font.BuiltIn): KeyVBT =
  BEGIN
    EVAL TextVBT.T.init(v, t, fnt := f);
    RETURN v;                    (* Return KeyVBT, not TextVBT.T *)
  END Init;

PROCEDURE Position (v: KeyVBT; READONLY cd: VBT.PositionRec) =
  BEGIN
    (* Notify us whenever cursor enters the window. *)
    VBT.SetCage(v, VBT.GoneCage);
    (* Take keyboard focus if mouse moves in our window. *)
    VBT.Acquire(v, VBT.KBFocus, cd.time)
  END Position;

PROCEDURE Key (v: KeyVBT; READONLY cd: VBT.KeyRec) =
  CONST
    (* in [sp ..  td], the range of ascii printable characters, the
       KeyboardKey.KeySym's coincide *)
    sp = Latin1Key.space;
    td = Latin1Key.asciitilde;
  VAR
    wc       := cd.whatChanged;
    c : CHAR;
  BEGIN
    IF NOT cd.wentDown OR Rect.IsEmpty(VBT.Domain(v)) THEN RETURN END;
    IF sp <= wc AND wc <= td THEN
      c := VAL(ORD(wc), CHAR);
      TextVBT.Put(v, "character \'" & Fmt.Char(c) & "\'");
    END;
  END Key;

VAR
  times14 := "-*-times-*-r-*-*-14-*-*-*-*-*-*-*";
  font    := Font.FromName(ARRAY OF TEXT{times14});
  key     := NEW(KeyVBT).init("[uninitialized]", font);
  main    := BorderedVBT.New(RigidVBT.FromHV(key, 30.0, 10.0));
BEGIN
  Trestle.Install(main);
  Trestle.AwaitDelete(main);
END Key.
